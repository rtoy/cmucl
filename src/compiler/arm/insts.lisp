;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/insts.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the ARM architecture,
;;;
;;; Written by Raymond Toy
;;;
;;; Reference: ARM Architecture Reference Manual, ARMv7-A and ARMv7-R,
;;; http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0406c/index.html
;;;
(in-package "ARM")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p t
  :max-locations 101)			; TODO: How many locations?


;;;; Constants, types, conversion functions, some disassembler stuff.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
	 (tn-offset tn)
	 (error (intl:gettext "~S isn't a register.") tn)))))

;; The encoding of a register number is different between a single-reg
;; and a double-reg.
(defun fp-reg-tn-encoding (tn doublep)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error (intl:gettext "~S isn't a floating-point register.") tn))
  ;; The double regs have even values 0 to 62, but ARM numbers them
  ;; from 0 to 31.  Map them the the ARM numbering scheme.
  (let ((regnum (if doublep
		    (ash (tn-offset tn) -1)
		    (tn-offset tn))))
    ;; The instruction encodings want to split that into a 1 bit chunk
    ;; and a 4 bit chunk.  But which chunk is which depends on whether
    ;; it's a single or double reg.  See, for example the instruction
    ;; description for VADD.
    (if doublep
	(values (ldb (byte 1 4) regnum)
		(ldb (byte 4 0) regnum))
	(values (ldb (byte 1 0) regnum)
		(ldb (byte 4 1) regnum)))))

(disassem:set-disassem-params :instruction-alignment 32
			      :opcode-column-width 11)


(def-vm-support-routine location-number (loc)
  (etypecase loc
    (null)
    (number)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (registers
	(unless (zerop (tn-offset loc))
	  (tn-offset loc)))
       (float-registers
	(sc-case loc
	  (single-reg
	   (+ (tn-offset loc) 32))
	  (double-reg
	   (let ((offset (tn-offset loc)))
	     (assert (zerop (mod offset 2)))
	     (values (+ offset 32) 2)))))
       (control-registers
	96)
       (immediate-constant
	nil)))
    (symbol
     (ecase loc
       (:memory 0)
       (:apsr 97)
       (:fpscr 98)))))

;;; symbols used for disassembly printing
;;;
(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol name))))
       *register-names*)
  "The Lisp names for the ARM integer registers")

(defun get-reg-name (index)
  (aref reg-symbols index))

(defun maybe-add-notes (value dstate)
  (declare (ignore value dstate))
  )

(eval-when (compile load eval)
(defun reg-arg-printer (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (get-reg-name value)))
    (princ regname stream)
    (disassem:maybe-note-associated-storage-ref value
						'registers
						regname
						dstate)
    (maybe-add-notes value dstate)))
) ; eval-when
      
(disassem:define-argument-type reg
  :printer #'reg-arg-printer)

;; Do we need a separate set for single-float and double-float regs? 
;; The ARM names are s<n> and d<n>.
(defparameter float-reg-symbols
  (coerce 
   (loop for n from 0 below (* 2 double-float-registers)
	 collect (make-symbol (format nil "F~d" n)))
   'vector))

;; We need separate types for single and double float because ARM uses
;; the same numbering scheme for both.
(disassem:define-argument-type fp-single-reg
  :printer #'(lambda (vlist stream dstate)
	       (declare (stream stream))
	       ;; The fp-reg fields are always split into two parts,
	       ;; so we get a list of values from the two parts.
	       (let* ((value (logior (ash (second vlist) 1)
				     (first vlist)))
		      (regname (format nil "S~D" value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  regname
		  dstate))))

(disassem:define-argument-type fp-double-reg
  :printer #'(lambda (vlist stream dstate)
	       (declare (stream stream))
	       ;; The fp-reg fields are always split into two parts,
	       ;; so we get a list of values from the two parts.  We
	       ;; use the ARM syntax and numbering for the register.
	       (let* ((value (logior (ash (first vlist) 4)
					  (second vlist)))
		      (regname (format nil "D~D" value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  regname
		  dstate))))

;;
;; Table A8-1
(defconstant condition-codes
  '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al))

;; Hmm.  The only way to distinguish :ror from :rrx is the value of
;; the shift.  If the shift is 0, then it's :rrx.  Otherwise it's
;; :ror.  What should we do here?
(defconstant shift-types
  '(:lsl :lsr :asr :ror :rrx))

(deftype condition-code ()
  `(member ,@condition-codes))

(defconstant condition-code-name-vec
  (coerce condition-codes 'vector))

(defconstant condition-true
  #b1110)

(disassem:define-argument-type condition-code
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate))
	       (unless (= value condition-true)
		 (princ (aref condition-code-name-vec value) stream))))

(deftype shift-type ()
  `(member ,@shift-types))

(disassem:define-argument-type shift-type
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate))
	       (princ (elt shift-types value))))

;; Look through OPTIONS-LIST and find a condition code and return the
;; corresponding value for the COND field of an instruction.
(defun inst-condition-code (options-list)
  (let ((c (remove :s options-list)))
    (unless (cdr c)
      (error "invalid condition code: ~S" c))
    (let ((position (position (car c) condition-codes)))
      (or position
	  #b1111))))

;; Look through OPTIONS-LIST to find :S, which means the instruction
;; should set the flags.
(defun inst-set-flags (options-list)
  (if (member :s options-list)
      1
      0))

;; Convert SHIFT-TYPE to the value for an instruction field.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun shift-encoding (shift-type)
  (if (eq shift-type :rrx)
      3
      (or (position shift-type shift-types)
	  (error "Unknown shift type: ~S" shift-type)))))

(defun rotate-right2 (value amount)
  "Rotate VALUE right by 2*AMOUNT bits in a register of length 32 bits."
  (declare (type (unsigned-byte 32) value)
	   (type (integer 0 15) amount))
  (let* ((shift (* 2 amount))
	 (out (ldb (byte shift 0) value)))
    (logior (ash out (- 32 shift))
	    (ash value (- shift)))))

(defun rotate-left (value amount)
  "Rotate VALUE left by AMOUNT bits in a register of length 32 bits."
  (declare (type (unsigned-byte 32) value)
	   (type (integer 0 15) amount))
  (let* ((out (ldb (byte amount (- 32 amount)) value)))
    (logior out
	    (ash (ldb (byte (- 32 amount) 0) value) amount))))

(defun find-encoding (value)
  "Find a V and N such that VALUE = rotate_right(V, 2*N)
  The values N and V are returned as multiple values, in that order.
  If no such values of N and V exist, then NIL is returned."
  (cond ((< value 256)
	 (values 0 value))
	((zerop (ldb (byte 8 24) value))
	 ;; Easy case where the rotation didn't rotate anything into
	 ;; the top byte.
	 (let* ((trailing-zeros (floor (logcount (logand (lognot value)
							      (1- value)))
					    2))
		(new (ash value (- (* 2 trailing-zeros)))))
	   (when (< new 256)
	     (values (- 16 trailing-zeros) new))))
	(t
	 ;; Rotate the value left 8 bits.  This guarantees that only
	 ;; shifts and not rotates are needed to figure out the
	 ;; answer, if the top bits 8 bits are zero.
	 (let ((new (rotate-left value 8)))
	   (when (zerop (ldb (byte 8 24) new))
	     (multiple-value-bind (rot val)
		 (find-encoding new)
	       (when rot
		 (values (mod (+ 4 rot) 16) val))))))))


;;; Define instruction formats. See DDI0406C_b, section A5 for details.

;; Section A5.1 describes the basic forma of an instruction.
;; tHowever, the manual makes a mess of it when describing
;; instructions; the descriptions don't follow this basic format and
;; arbitrarily combines or splits fields.
;;
;; Section A5.1.  This is the basic encoding form, not including the
;; op field (byte 1 4).  That is sometimes used for other things.
(disassem:define-instruction-format
    (format-base 32)
  (cond  :field (byte 4 28) :type 'condition-code)
  (opb0  :field (byte 3 25)))

(defconstant format-1-immed-printer
  `(:name (:unless (s :constant 0) 's)
	  (:unless (:constant ,condition-true) cond)
	  :tab
	  dst
	  ", "
	  src1
	  ", "
	  immed))

(defconstant format-1-immed-set-printer
  `(:name (:unless (:constant ,condition-true) cond)
	  :tab
	  src1
	  ", "
	  immed))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun modified-immed-printer (value stream dstate)
  (declare (ignore dstate))
  (let ((rot (ldb (byte 4 8) value))
	(v (ldb (byte 8 0) value)))
    (format stream "#~D" (rotate-right2 v rot))))
)

(define-emitter emit-format-1-immed 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 12 0))

(disassem:define-instruction-format
    (format-1-immed 32 :include 'format-base
		       :default-printer format-1-immed-printer)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (src1  :field (byte 4 16))
  (dst   :field (byte 4 12))
  (immed :field (byte 12 0) :printer #'modified-immed-printer))

(defconstant format-0-reg-printer
  `(:name (:unless (s :constant 0) 's)
	  cond
	  :tab
	  dst
	  ", "
	  src1
	  ", "
	  src2
	  (:unless (shift :constant 0)
	    (:cond ((type :constant #b11) ; ror or rrx
		    (:cond ((shift :constant 0) " " 'rrx)
			   (t
			    " "
			    'ror
			    " "
			    shift)))
		   (t
		    " "
		    type
		    " #"
		    shift)))))

(defconstant format-0-reg-set-printer
  `(:name cond
	  :tab
	  src1
	  ", "
	  src2
	  (:unless (shift :constant 0)
	    (:cond ((type :constant #b11) ; ror or rrx
		    (:cond ((shift :constant 0) " " 'rrx)
			   (t
			    " "
			    'ror
			    " "
			    shift)))
		   (t
		    " "
		    type
		    " #"
		    shift)))))

(define-emitter emit-format-0-reg 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 5 7)
  (byte 2 5) (byte 1 4) (byte 4 0))
  
(disassem:define-instruction-format
    (format-0-reg 32 :include 'format-base :default-printer format-0-reg-printer)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (src1  :field (byte 4 16))
  (dst   :field (byte 4 12))
  (shift :field (byte 5 7))
  (type  :field (byte 2 5) :type 'shift-type)
  (rs    :field (byte 1 4) :value 0)
  (src2  :field (byte 4 0) :type 'reg))

(defconstant format-0-reg-shifted-printer
  `(:name (:unless (s :constant 0) 's)
	  cond
	  :tab
	  dst
	  ", "
	  src1
	  ", "
	  src2
	  " "
	  type
	  " "
	  sreg))

(defconstant format-0-reg-shifted-set-printer
  `(:name cond
	  :tab
	  src1
	  ", "
	  src2
	  " "
	  type
	  " "
	  sreg))

(define-emitter emit-format-0-reg-shifted 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 4 8)
  (byte 1 7) (byte 2 5) (byte 1 4) (byte 4 0))
  
(disassem:define-instruction-format
    (format-0-reg-shifted 32 :include 'format-base
			     :default-printer format-0-reg-shifted-printer)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (src1  :field (byte 4 16))
  (dst   :field (byte 4 12) :type 'reg)
  (sreg  :field (byte 4 8) :type 'reg)
  (z     :field (byte 1 7) :value 0)
  (type  :field (byte 2 5) :type 'shift-type)
  (rs    :field (byte 1 4) :value 1)
  (src2  :field (byte 4 0) :type 'reg))


;; Structure for hold the flexible operand used in data processing
;; instructions.
(defstruct flex-operand
  ;; Indicates if the shift is an immediate or register value
  (type (required-argument)
   :type (member :reg-shift-imm :reg-shift-reg))

  ;; Register to be shifted
  reg

  ;; Shift type
  shift-type

  ;; Amount of the shift, either an immediate or a register
  shift-reg-or-imm)

;; See Table A5-6 for encoding of modified immediates in ARM
;; processing instructions.

;; src2 = reg SFT amount, where SFT is LSL, LSR, ASR, ROR, RRX and
;; amount is either a number or a register.
(defun make-shift (reg shift-type &optional amount)
  (declare (type shift-type shift-type))
  (typecase amount
    ((unsigned-byte 5)
     (when (and (eq shift-type :rrx)
		amount
		(not (zerop amount)))
       (error "Cannot specify non-zero shift amount with :rrx"))
     (make-flex-operand :type :reg-shift-imm
			:reg reg
			:shift-reg-or-imm amount
			:shift-type shift-type))
    (otherwise
     (when (eq shift-type :rrx)
       (error "Cannot specify :rrx type with register shift"))
     (make-flex-operand :type :reg-shift-reg
			:reg reg
			:shift-reg-or-imm amount
			:shift-type shift-type))))


(defmacro define-data-proc (name opcode &key (src1-type 'reg) force-set-p)
  ;; For compare-type instructions, force-set-p should be true.  This
  ;; means the S bit is set in the instruction and that there is no
  ;; dst register.
  `(define-instruction ,name (segment dst src1 src2 &rest opts)
     (:declare (type tn dst)
	       (type tn src1)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg
			 flex-operand)
		     src2))
     (:printer format-1-immed
	       ((opb0 #b001) (op ,opcode)
		,(if force-set-p
		     '(s 1))
		,@(if src1-type
		      `((src1 nil :type ',src1-type))
		      '((src1 0)))
		,@(if force-set-p
		      '((dst 0))
		      '((dst nil :type 'reg))))
	       ,@(when force-set-p
		    `(format-1-immed-set-printer)))
     (:printer format-0-reg
	       ((opb0 #b000) (op ,opcode) (rs 0)
		,(if force-set-p
		     '(s 1))
		,@(if src1-type
		      `((src1 nil :type ',src1-type))
		      '((src1 0)))
		,@(if force-set-p
		      '((dst 0))
		      '((dst nil :type 'reg))))
	       ,@(when force-set-p
		   `(format-0-reg-set-printer)))
     (:printer format-0-reg-shifted
	       ((opb0 #b000) (op ,opcode) (rs 1)
		,(if force-set-p
		     '(s 1))
		,@(if src1-type
		      `((src1 nil :type ',src1-type))
		      '((src1 0)))
		,@(if force-set-p
		      '((dst 0))
		      '((dst nil :type 'reg))))
	       ,@(when force-set-p
		  `(format-0-reg-shifted-set-printer)))
     (:dependencies
      (reads src1)
      (writes dst))
     (:delay 0)
     (:emitter
      (etypecase src2
	(integer
	 (multiple-value-bind (rot val)
	     (find-encoding src2)
	   (unless rot
	     (error "Cannot encode the immediate value ~S~%" src2))
	   (emit-format-1-immed segment
				(inst-condition-code opts)
				#b001
				,opcode
				,(if force-set-p
				     1
				     `(inst-set-flags opts))
				(reg-tn-encoding src1)
				(reg-tn-encoding dst)
				(logior (ash rot 8) val))))
	(reg
	 (emit-format-0-reg segment
			    (inst-condition-code opts)
			    #b000
			    ,opcode
			    ,(if force-set-p
				 1
				 `(inst-set-flags opts))
			    (reg-tn-encoding src1)
			    (reg-tn-encoding dst)
			    0
			    (shift-encoding :lsl)
			    #b0
			    (reg-tn-encoding src2)))
	(flex-operand
	 (ecase (flex-operand-type src2)
	   (:reg-shift-imm
	    (emit-format-0-reg segment
			       (inst-condition-code opts)
			       #b000
			       ,opcode
			       ,(if force-set-p
				    1
				    `(inst-set-flags opts))
			       (reg-tn-encoding src1)
			       (reg-tn-encoding dst)
			       (flex-operand-shift-reg-or-imm src2)
			       (shift-encoding (flex-operand-shift-type src2))
			       #b0
			       (reg-tn-encoding (flex-operand-reg src2))))
	   (:reg-shift-reg
	    (emit-format-0-reg-shift segment
				     (inst-condition-code opts)
				     #b000
				     ,opcode
				     ,(if force-set-p
					  1
					  `(inst-set-flags opts))
				     (reg-tn-encoding src1)
				     (reg-tn-encoding dst)
				     (reg-tn-encoding (flex-operand-shift-reg-or-imm src2))
				     #b0
				     (shift-encoding (flex-operand-shift-type src2))
				     #b1
				     (reg-tn-encoding (flex-operand-reg src2))))))))))


(define-data-proc and #b0000)
(define-data-proc eor #b0001)
;; #b0010 is sub and adr (immediate operand)
(define-data-proc sub #b0010)
(define-data-proc rsb #b0011)
;; #b0100 is add and adr (immediate operand)
(define-data-proc add #b0100)
(define-data-proc adc #b0101)
(define-data-proc sbc #b0110)
(define-data-proc rsc #b0111)
;; #b10xx is data processing and miscellaneous instructions
(define-data-proc tst #b1000 :force-set-p t)
(define-data-proc teq #b1001 :force-set-p t)
(define-data-proc cmp #b1010 :force-set-p t)
(define-data-proc cmn #b1011 :force-set-p t)
(define-data-proc orr #b1100)
(define-data-proc bic #b1110)

;; MVN Aka bitnot
(define-instruction mvn (segment dst src2 &rest opts)
  (:declare (type tn dst)
	    (type (or (signed-byte 32)
		      (unsigned-byte 32)
		      reg
		      flex-operand)
		  src2))
  (:printer format-1-immed
	    ((opb0 #b001) (op #b1111)
			  (src1 0)
			  (dst nil :type 'reg))
	    `(:name (:unless (s :constant 0) 's)
		    cond
		    :tab
		    dst ", " immed))
  (:printer format-0-reg
	    ((opb0 #b000) (op #b1111) (rs 0)
			  (src1 0)
			  (dst nil :type 'reg))
	    `(:name (:unless (s :constant 0) 's)
		    cond
		    :tab
		    dst
		    ", "
		    src2
		    (:unless (shift :constant 0)
		      (:cond ((type :constant #b11) ; ror or rrx
			      (:cond ((shift :constant 0) " " 'rrx)
				     (t
				      " " 'ror " " shift)))
			     (t
			      " " type " #" shift)))))
  (:printer format-0-reg-shifted
	    ((opb0 #b000) (op #b1111) (rs 1)
			  (src1 0)
			  (dst nil :type 'reg))
	    `(:name (:unless (s :constant 0) 's)
		    cond
		    :tab
		    dst ", " src2 " " type " " sreg))
  (:dependencies
   (reads src1)
   (writes dst))
  (:delay 0)
  (:emitter
   (etypecase src2
     (integer
      (multiple-value-bind (rot val)
	  (find-encoding src2)
	(unless rot
	  (error "Cannot encode the immediate value ~S~%" src2))
	(emit-format-1-immed segment
			     (inst-condition-code opts)
			     #b001
			     #b1111
			     (inst-set-flags opts)
			     (reg-tn-encoding src1)
			     (reg-tn-encoding dst)
			     (logior (ash rot 8) val))))
     (reg
      (emit-format-0-reg segment
			 (inst-condition-code opts)
			 #b000
			 #b1111
			 (inst-set-flags opts)
			 (reg-tn-encoding src1)
			 (reg-tn-encoding dst)
			 0
			 (shift-encoding :lsl)
			 #b0
			 (reg-tn-encoding src2)))
     (flex-operand
      (ecase (flex-operand-type src2)
	(:reg-shift-imm
	 (emit-format-0-reg segment
			    (inst-condition-code opts)
			    #b000
			    #b1111
			    (inst-set-flags opts)
			    (reg-tn-encoding src1)
			    (reg-tn-encoding dst)
			    (flex-operand-shift-reg-or-imm src2)
			    (shift-encoding (flex-operand-shift-type src2))
			    #b0
			    (reg-tn-encoding (flex-operand-reg src2))))
	(:reg-shift-reg
	 (emit-format-0-reg-shift segment
				  (inst-condition-code opts)
				  #b000
				  #b1111
				  (inst-set-flags opts)
				  (reg-tn-encoding src1)
				  (reg-tn-encoding dst)
				  (reg-tn-encoding (flex-operand-shift-reg-or-imm src2))
				  #b0
				  (shift-encoding (flex-operand-shift-type src2))
				  #b1
				  (reg-tn-encoding (flex-operand-reg src2)))))))))

;; See A8.8.105.  It's easy to define this general MOV instruction.
;; Is that good enough for users?  They'll have to remember that a
;; shift is a move inst with a hairy flex operand 2.
(define-instruction mov (segment dst src2 &rest opts)
  (:declare (type tn dst)
	    (type (or (signed-byte 32)
		      (unsigned-byte 32)
		      reg
		      flex-operand)
		  src2))
  (:printer format-1-immed
	    ((opb0 #b001)
	     (op #b1101)
	     (src1 0)
	     (dst nil :type 'reg))
	    '(:name (:unless (s :constant 0) 's)
		    cond
		    :tab
		    dst ", " immed))
  (:printer format-0-reg
	    ((opb0 #b000)
	     (op #b1101)
	     (rs 0)
	     (src1 0)
	     (dst nil :type 'reg)
	     (shift 0)
	     (type 0)
	     (rs 0))
	    '(:name (:unless (s :constant 0) 's)
		    cond
		    :tab
		    dst
		    ", "
		    src2
		    (:unless (shift :constant 0)
		      (:cond ((type :constant #b11) ; ror or rrx
			      (:cond ((shift :constant 0) " " 'rrx)
				     (t
				      " " 'ror " " shift)))
			     (t
			      " " type " #" shift)))))
  (:dependencies
   (reads src1)
   (writes dst))
  (:delay 0)
  (:emitter
   (etypecase src2
     (integer
      (multiple-value-bind (rot val)
	  (find-encoding src2)
	(unless rot
	  (error "Cannot encode the immediate value ~S~%" src2))
	(emit-format-1-immed segment
			     (inst-condition-code opts)
			     #b001
			     #b1101
			     (inst-set-flags opts)
			     0
			     (reg-tn-encoding dst)
			     (logior (ash rot 8) val))))
     (reg
      (emit-format-0-reg segment
			 (inst-condition-code opts)
			 #b000
			 #b1101
			 (inst-set-flags opts)
			 (reg-tn-encoding src1)
			 (reg-tn-encoding dst)
			 0
			 (shift-encoding :lsl)
			 #b0
			 (reg-tn-encoding src2)))
     (flex-operand
      (ecase (flex-operand-type src2)
	(:reg-shift-imm
	 (emit-format-0-reg segment
			    (inst-condition-code opts)
			    #b000
			    #b1101
			    (inst-set-flags opts)
			    0
			    (reg-tn-encoding dst)
			    (flex-operand-shift-reg-or-imm src2)
			    (shift-encoding (flex-operand-shift-type src2))
			    #b0
			    (reg-tn-encoding (flex-operand-reg src2))))
	(:reg-shift-reg
	 (error "Use the shift instructions instead of MOV with a shifted register")))))))

(defmacro define-shift (name)
  `(define-instruction ,(symbolicate (string name)) (segment dst src2 shift &rest opts)
     (:declare (type tn dst src2)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg)
		     shift))
     (:printer format-0-reg
	       ((opb0 #b000)
		(op #b1101)
		(rs 0)
		(src1 0)
		(dst nil :type 'reg)
		(type ,(shift-encoding name))
		(rs 0))
	       '(:name (:unless (s :constant 0) 's)
		 cond
		 :tab
		 dst ", " src2 ", #" shift))
     (:printer format-0-reg-shifted
	       ((opb0 #b000)
		(op #b1101)
		(rs 1)
		(src1 0)
		(dst nil :type 'reg)
		(type ,(shift-encoding name)))
	       '(:name (:unless (s :constant 0) 's)
		 cond
		 :tab
		 dst ", " src2 ", " sreg))
     (:dependencies
      (reads src1)
      (writes dst))
     (:delay 0)
     (:emitter
      (etypecase shift
	(integer
	 (emit-format-0-reg segment
			    (inst-condition-code opts)
			    #b000
			    #b1101
			    (inst-set-flags opts)
			    #b0000
			    (reg-tn-encoding dst)
			    shift
			    (shift-encoding ,name)
			    #b0
			    (reg-tn-encoding src2)))
	(reg
	 (emit-format-0-reg-shift segment
				  (inst-condition-code opts)
				  #b000
				  #b1101
				  (inst-set-flags opts)
				  #b0000
				  (reg-tn-encoding dst)
				  (reg-tn-encoding shift)
				  #b0
				  (shift-encoding ,name)
				  #b1
				  (reg-tn-encoding src2)))))))

(define-shift :asr)
(define-shift :lsl)
(define-shift :lsr)
(define-shift :ror)

(define-instruction rrx (segment dst src2 &rest opts)
  (:declare (type tn dst src2)
	    (type (or (signed-byte 32)
		      (unsigned-byte 32)
		      reg)
		  shift))
  (:printer format-0-reg
	    ((opb0 #b000)
	     (op #b1101)
	     (rs 0)
	     (src1 0)
	     (dst nil :type 'reg)
	     (type (shift-encoding :ror))
	     (shift 0))
	    '(:name (:unless (s :constant 0) 's)
	      cond
	      :tab
	      dst ", " src2))
  (:dependencies
   (reads src2)
   (writes dst))
  (:delay 0)
  (:emitter
   (emit-format-0-reg segment
		      (inst-condition-code opts)
		      #b000
		      #b1101
		      (inst-set-flags opts)
		      0
		      (reg-tn-encoding dst)
		      0
		      (shift-encoding :ror)
		      #b0
		      (reg-tn-encoding src2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun split-imm16-printer (value stream dstate)
  (declare (ignore dstate))
  (format stream "#~D" (logior (ash (first value) 8)
			       (second value)))))

(define-emitter emit-format-mov16 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16) (byte 4 12) (byte 12 0))

(disassem:define-instruction-format
    (format-mov16 32 :include 'format-base
		     :default-printer `(:name cond :tab dst ", " imm16))
  (op    :field (byte 5 20))
  (imm16 :fields (list (byte 4 16) (byte 12 0)) :printer #'split-imm16-printer)
  (dst   :field (byte 4 12) :type 'reg))

;; FIXME: Probably need to make movt and movw support fixups!
(define-instruction movt (segment dst imm16 &optional (cc :al))
  (:declare (type tn dst)
	    (type (unsigned-byte 16) imm16)
	    (type condition-code cc))
  (:printer format-mov16
	    ((opb0 #b001)
	     (op #b10100)))
  (:dependencies
   (writes dst))
  (:emitter
   (emit-format-mov16 segment
		      (inst-condition-code (list cc))
		      #b001
		      #b10100
		      (ldb (byte 4 12) imm16)
		      (reg-tn-encoding dst)
		      (ldb (byte 12 0) imm16))))

(define-instruction movw (segment dst imm16 &optional (cc :al))
  (:declare (type tn dst)
	    (type (unsigned-byte 16) imm16)
	    (type condition-code cc))
  (:printer format-mov16
	    ((opb0 #b001)
	     (op #b10000)))
  (:dependencies
   (writes dst))
  (:emitter
   (emit-format-mov16 segment
		      (inst-condition-code (list cc))
		      #b001
		      #b10000
		      (ldb (byte 4 12) imm16)
		      (reg-tn-encoding dst)
		      (ldb (byte 12 0) imm16))))

;; A5.2.5 Multiply and Accumulate

(define-emitter emit-format-mul 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))
  
(disassem:define-instruction-format
    (format-0-mul 32 :include 'format-base)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (dst   :field (byte 4 16) :type 'reg)
  (src3  :field (byte 4 12))
  (src2  :field (byte 4 8) :type 'reg)
  (op1   :field (byte 4 4))
  (src1  :field (byte 4 0) :type 'reg))

(define-instruction mul (segment dst src1 src2 &rest opts)
  (:declare (type tn dst src1 src2)
	    (type condition-code cond))
  (:printer format-0-mul
	    ((opb0 #b000)
	     (op #b0000)
	     (op1 #b1001)
	     (src3 0))
	    `(:name (:unless (s :constant 0) 's)
		    cond
		    :tab
		    dst ", " src1 ", " src2))
  (:dependencies
   (reads src1)
   (reads src2)
   (writes dst))
  (:emitter
   (emit-format-0-mul segment
		      (inst-condition-code cond)
		      #b000
		      #b0000
		      (inst-set-flags opts)
		      (reg-tn-encoding dst)
		      #b0000
		      (reg-tn-encoding src2)
		      #b1001
		      (reg-tn-encoding src1))))

(defmacro define-4-arg-mul (name op &key two-outputs setflags0)
  `(define-instruction ,name (segment dst dst2-or-src src2 src3 &rest opts)
     (:declare (type tn dst dst2-or-src src2))
     (:printer format-0-mul
	       ((opb0 #b000) (op ,op) (op1 #b1001)
		(src3 nil :type 'reg)
		,(if setflags0 '(s 0)))
	       ',(if two-outputs
		     `(:name (:unless (s :constant 0) 's)
			     cond
			     :tab
			     src3 ", " dst ", " src2 ", " src3)
		     `(:name (:unless (s :constant 0) 's)
			     cond
			     :tab
			     dst ", " src1 ", " src2 ", " src3)))
     (:dependencies
      (reads src2)
      (reads src3)
      ,(if two-outputs
	   `(writes dst2-or-src)
	   `(reads dst2-or-src))
      (writes dst))
     (:emitter
      (emit-format-0-mul segment
			 (inst-condition-code opts)
			 #b000
			 ,op
			 ,(if setflags0
			      0
			      `(inst-set-flags opts))
			 ,@(if two-outputs
			      `((reg-tn-encoding dst2-or-src)
				(reg-tn-encoding dst)
				(reg-tn-encoding src3)
				#b1001
				(reg-tn-encoding src3))
			      `((reg-tn-encoding dst)
				(reg-tn-encoding src3)
				(reg-tn-encoding src1)
				#b1001
				(reg-tn-encoding dst2-or-src)))))))

(define-4-arg-mul mla   #b0001)
(define-4-arg-mul umaal #b0010 :two-outputs t :setflags0 t)
(define-4-arg-mul mls   #b0011 :setflags0 t)
(define-4-arg-mul umull #b0100 :two-outputs t)
(define-4-arg-mul umlal #b0101 :two-outputs t)
(define-4-arg-mul smull #b0110 :two-outputs t)
(define-4-arg-mul smlal #b0111 :two-outputs t)

;; Divide and friends
;; A5.4.4

(define-emitter emit-format-div 32
  (byte 4 28) (byte 3 25) (byte 2 23) (byte 3 20) (byte 4 16) (byte 4 12) (byte 4 8)
  (byte 3 5) (byte 1 4) (byte 4 0))

(defconstant format-div-printer
  `(:name cond
          :tab
          dst ", " src1 ", " src2))    

(disassem:define-instruction-format
    (format-div 32 :include 'format-base :default-printer format-div-printer)
  (op0   :field (byte 2 23) :value #b10)
  (op1   :field (byte 3 20))
  (dst   :field (byte 4 16) :type 'reg)
  (a     :field (byte 4 12) :value #b1111)
  (src2  :field (byte 4 8) :type 'reg)
  (op2   :field (byte 3 5))
  (one   :field (byte 1 4) :value #b1)
  (src1  :field (byte 4 0) :type 'reg))

(defmacro define-div (name op1 op2)
  `(define-instruction ,name (segment dst src1 src2 &optional (cond :al))
     (:declare (type tn dst src1 src2)
	       (type condition-code cond))
     (:printer format-div
	       ((opb0 #b011) (op1 ,op1) (a #b1111) (op2 ,op2) (one #b1)))
     (:dependencies
      (reads src1)
      (reads src2)
      (writes dst))
     (:emitter
      (emit-format-div segment
		       (inst-condition-code (list cond))
		       #b011
		       #b10
		       ,op1
		       (reg-tn-encoding dst)
		       #b1111
		       (reg-tn-encoding src2)
		       ,op2
		       #b1
		       (reg-tn-encoding src1)))))

(define-div sdiv #b001 #b000)
(define-div udiv #b011 #b000)

;; Misc instructions
;; A5.2.12

(define-emitter emit-format-0-bkpt 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 12 8) (byte 4 4) (byte 4 0))

(disassem:define-instruction-format
    (format-0-bkpt 32
		 :include 'format-base
		 :default-printer `(:name cond :tab imm16))
  (op0   :field (byte 5 20) :value #b10010)
  (imm16 :fields (list (byte 12 8) (byte 4 0)) :printer #'split-imm16-printer)
  (op1   :field (byte 4 4) :value #b0111))

(define-instruction bkpt (segment value &optional (cond :al))
  (:declare (type (unsigned-byte 16) value))
  (:printer format-0-bkpt
	    ((opb0 #b000) (op0 #b10010) (op1 #b0111)))
  (:emitter
   (emit-format-0-bkpt segment
		       (inst-condition-code (list cond))
		       #b000
		       #b10010
		       (ldb (byte 12 4) value)
		       #b0111
		       (ldb (byte 4 0) value))))
;; See A8.8.63
;; LDR/STR (immediate)
;;
;; ldr<c> dst, [src1, #+/-<imm>]
;; ldr<c> dst, [src1, #+/-<imm>]!
;; ldr<c> dst, [src1], #+/-<imm>
(defconstant format-2-immed-printer
  `(:name cond
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 1)
		  ", #"
		  (:unless (u :constant 0) "-")
		  immed
		  "]"
		  (:unless (w :constant 1) "!"))
		 (t
		  "], #"
		  (:unless (u :constant 0) "-")
		  immed))))
		  

(define-emitter emit-format-2-immed 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 1 20) (byte 4 16) (byte 4 12) (byte 12 0))

  
(disassem:define-instruction-format
    (format-2-immed 32
		    :include 'format-base
		    :default-printer format-2-immed-printer)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (byte  :field (byte 1 22))		; byte (1) or word (0)
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))		; ldr (1) or str (0)
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (immed :field (byte 12 0)))

;; See A8.8.66
;; LDR/STR (register)
;;
;; ldr<c> dst, [src1, +/-src2, shift]<!>
;; ldr<c> dst, [src1], +/-src2, shift
(defconstant format-3-reg-printer
  `(:name cond
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 1)
		  ", "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  rs ", " type " #" imm5 "]"
		  (:unless (w :constant 1) "!"))
		 (t
		  "], "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  rs ", " type " #" imm5))))
  
(define-emitter emit-format-3-reg 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 1 20) (byte 4 16) (byte 4 12) (byte 5 7) (byte 2 5) (byte 1 4) (byte 4 0))

(disassem:define-instruction-format
    (format-3-reg 32
		  :include 'format-base
		  :default-printer format-3-reg-printer)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (byte  :field (byte 1 22))
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))		; ldr (1) or str (0)
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (imm5  :field (byte 5 7))
  (type  :field (byte 2 5) :type 'shift-type)
  (z     :field (byte 1 4) :value 0)
  (rs    :field (byte 4 0) :type 'reg))

;; LDRH/STRH (register)
(defconstant format-0-halfword-reg-printer
  `(:name cond
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 0)
		  "], "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  src2)
		 (t
		  ", "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  src2
		  "]"
		  (:unless (w :constant 0) "!")))))

(define-emitter emit-format-0-halfword-reg 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 1 20) (byte 4 16) (byte 4 12) (byte 4 8) (byte 1 7) (byte 1 6) (byte 2 4)
  (byte 4 0))
  
(disassem:define-instruction-format
    (format-0-halfword-reg 32
			   :include 'format-base
			   :default-printer format-0-halfword-reg-printer)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (imm   :field (byte 1 22) :value 0)
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (z     :field (byte 4 8) :value 0)
  (one    :field (byte 1 7) :value 1)
  (signed :field (byte 1 6))
  (op2    :field (byte 2 4))
  (src2  :field (byte 4 0) :type 'reg))

;; LDRH/STRH (register)

(defconstant format-0-halfword-imm-printer
  `(:name cond
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 0)
		  "], #"
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  imm8)
		 (t
		  ", #"
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  imm8
		  "]"
		  (:unless (w :constant 0) "!")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun split-imm8-printer (value stream dstate)
  (declare (ignore dstate))
  (format stream "~D" (logior (ash (first value) 4) (second value)))))

(define-emitter emit-format-0-halfword-imm 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 1 20) (byte 4 16) (byte 4 12) (byte 4 8) (byte 1 7) (byte 1 6) (byte 2 4)
  (byte 4 0))
  
(disassem:define-instruction-format
    (format-0-halfword-imm 32
			   :include 'format-base
			   :default-printer format-0-halfword-imm-printer)
  (p      :field (byte 1 24))
  (u      :field (byte 1 23))
  (imm    :field (byte 1 22) :value 1)
  (w      :field (byte 1 21))
  (ld     :field (byte 1 20))
  (src1   :field (byte 4 16) :type 'reg)
  (dst    :field (byte 4 12) :type 'reg)
  (imm8   :fields (list (byte 4 8) (byte 4 0)) :printer #'split-imm8-printer)
  (one    :field (byte 1 7) :value 1)
  (signed :field (byte 1 6))
  (op2    :field (byte 2 4)))

(defstruct load-store-index
  (type (required-argument) :type '(member :reg :immediate))
  base-reg
  offset
  shift-type
  shift-amount
  add
  update
  post-indexed)

(defun make-ea (base-reg &key (offset 0) update
			   (add t addp) 
			   (shift-type :lsl shift-type-p)
			   (amount 0 amountp)
			   post-indexed)
  (assert (typep amount '(unsigned-byte 5)))
  (assert (typep shift-type shift-types))
  (etypecase offset
    (reg
     (make-load-store-index :type :reg
			    :base-reg base-reg
			    :offset offset
			    :add add
			    :shift-type shift-type
			    :shift-amount amount
			    :update update
			    :post-indexed post-indexed))
    ((or (unsigned-byte 12) (signed-byte 12))
     (let ((mag (abs imm))
	   (add (not (minusp imm))))
       ;; The :add :shift-type and :amount keywords aren't allowed in
       ;; this case.
       (assert (not addp))
       (assert (not shift-type-p))
       (assert (not amountp))
       (make-load-store-index :type :immediate
			      :base-reg base-reg
			      :offset mag
			      :add add
			      :update update
			      :post-indexed post-indexed)))))

(defun decode-load-store-index (index)
  "Determine the P, U, and W bits from the load-store-index"
  (values (if (load-store-index-post-indexed index) 0 1)
	  (if (load-store-index-add index) 1 0)
	  (if (load-store-index-update index) 1 0)))

(defmacro define-load/store (name loadp &optional bytep)
  `(define-instruction ,name (segment reg address &optional (cond :al))
     (:declare (type tn reg)
	       (type load-store-index address))
     (:dependencies
      (reads address)
      (writes reg))
     (:printer format-2-immed
	       ((opb0 #b010)
		(byte ,(if bytep 1 0))
		(ld ,(if loadp 1 0))))
     (:printer format-3-reg
	       ((opb0 #b011)
		(byte ,(if bytep 1 0))
		(ld ,(if loadp 1 0))))
     (:emitter
      (ecase (load-store-index-type address)
	(:reg
	 (multiple-value-bind (p u w)
	     (decode-load-store-index address)
	   (emit-format-3-reg segment
			      (inst-condition-code opts)
			      #b011
			      p
			      u
			      ,(if bytep 1 0)
			      w
			      ,(if loadp 1 0)
			      (reg-tn-encoding (load-store-index-base-reg address))
			      (reg-tn-encoding reg)
			      (load-store-index-shift-amount address)
			      (load-store-index-shift-type address)
			      0
			      (reg-tn-encoding (load-store-index-offset address)))))
	(:immediate
	 (multiple-value-bind (p u w)
	     (decode-load-store-index address)
	   (emit-format-2-immed segment
				(inst-condition-code opts)
				#b010
				p
				u
				,(if bytep 1 0)
				w
				,(if loadp 1 0)
				(reg-tn-encoding (load-store-index-base-reg address))
				(reg-tn-encoding reg)
				(load-store-index-offset address))))))))

(define-load/store ldr t)
(define-load/store ldrb t t)
(define-load/store str nil)
(define-load/store strb nil t)

(defmacro define-load/store-extra (name &optional loadp bytep signedp)
  `(define-instruction ,name (segment reg address &optional (cond :al))
     (:declare (type tn reg)
	       (type load-store-index address))
     (:dependencies
      ,(if loadp
	   `(writes reg)
	   `(reads reg)))
     (:printer format-0-halfword-imm
	       ((opb0  #b000)
		(ld ,(if loadp 1 0))
		(one 1)
		(signed ,(if (or signedp bytep) 1 0))
		(op2 ,(if bytep #b01 #b11))
		(imm 1)))
     (:printer format-0-halfword-reg
	       ((opb0  #b000)
		(ld ,(if loadp 1 0))
		(one 1)
		(signed ,(if (or signedp bytep) 1 0))
		(op2 ,(if bytep #b01 #b11))
		(z 0)
		(imm 0)))
     (:emitter
      (multiple-value-bind (sign op2)
		 ;; bytep implies signed.  The unsigned byte
		 ;; instruction is handled elsewhere.
	  (values (if ,bytep #b1 #b0)
		  (if (or ,signedp ,bytep)
		      #b01
		      #b11))
	(ecase (load-store-index-type address)
	  (:reg
	   (multiple-value-bind (p u w)
	       (decode-load-store-index address)
	     (emit-format-0-halfword-reg segment
					 (inst-condition-code opts)
					 #b000
					 p
					 u
					 0
					 w
					 ,(if loadp 1 0)
					 (reg-tn-encoding (load-store-index-base-reg address))
					 (reg-tn-encoding reg)
					 1
					 0
					 #b11
					 (reg-tn-encoding (load-store-index-offset address)))))
	  (:immediate
	   (multiple-value-bind (p u w)
	       (decode-load-store-index src2)
	     (emit-format-0-halfword-imm segment
					 (inst-condition-code opts)
					 #b000
					 p
					 u
					 1
					 w
					 ,(if loadp 1 0)
					 (reg-tn-encoding (load-store-index-base-reg address))
					 (reg-tn-encoding reg)
					 (ldb (byte 4 4) (load-store-index-offset address))
					 1
					 sign
					 op2
					 (ldb (byte 4 0) (load-store-index-offset address))))))))))

(define-load/store-extra ldrh t)
(define-load/store-extra strh nil)
(define-load/store-extra ldrsh t nil t)
(define-load/store-extra ldrsb t t)



(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
		 (declare (type (signed-byte 24) value)
			  (type disassem:disassem-state dstate))
		 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

(defconstant branch-imm-printer
  `(:name cond
	  :tab
	  imm24))

(define-emitter emit-branch-imm 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 24 0))

(disassem:define-instruction-format
    (branch-imm 32 :include 'format-base
		   :default-printer branch-imm-printer)
  (op :field (byte 1 24) :value 1)
  (imm24 :field (byte 24 0)))

(defconstant branch-reg-printer
  `(:name cond
	  :tab
	  src1))

(define-emitter emit-branch-reg 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 12 8) (byte 4 4) (byte 4 0))
  
(disassem:define-instruction-format
    (branch-reg 32 :include 'format-base
		   :default-printer branch-reg-printer)
  (op    :field (byte 5 20) :value #b10010)
  (op0   :field (byte 12 8) :value #b111111111111)
  (op1   :field (byte 4 4) :value #b0011)
  (src1  :field (byte 4 0) :type 'reg))

(defun emit-relative-branch (segment opb0 op cond target)
  (emit-back-patch segment 4
     #'(lambda (segment posn)
	 (emit-branch-imm segment
			  (inst-condition-code cond)
			  opb0
			  op
			  (ash (- (label-position target) posn) -2)))))

;; For these branch instructions, should we still keep the condition
;; at the end, like for other instructions?  Or can we have it
;; (optionally) first, like on sparc and x86?  This latter option
;; appeals to me (rtoy).

(define-instruction b (segment target &optional (cond :al))
  (:declare (type label target)
	    (type condition-code cond))
  (:printer branch-imm
	    ((opb0 #b101)
	     (op #b0)
	     (imm24 nil :type 'relative-label)))
  (:attributes branch)
  (:emitter
   (emit-relative-branch segment #b101 #b0 cond target)))

(define-instruction bl (segment target &optional (cond :al))
  (:declare (type label target)
	    (type condition-code cond))
  (:printer branch-imm
	    ((opb0 #b101)
	     (op #b1)
	     (imm24 nil :type 'relative-label)))
  (:attributes branch)
  (:emitter
   (emit-relative-branch segment #b101 #b1 cond target)))

(define-instruction blx (segment target)
  (:declare (type (or label reg) target))
  (:printer branch-imm
	    ((cond #b1111)
	     (opb0 #b101)
	     (op #b0)
	     (imm24 nil :type 'relative-label))
	    '(:name :tab imm24))
  (:printer branch-reg
	    ((opb0 #b000)
	     (op #b10010)
	     (op0 #b111111111111)
	     (op1 #b0011)))
  (:attributes branch)
  (:emitter
   (etypecase target
     (label
      (emit-relative-branch segment #b101 #b0 :al target))
     (reg
      (emit-branch-reg segment
		       (inst-condition-code cond)
		       #b000
		       #b10010
		       #b111111111111
		       #b0011
		       (reg-tn-encooding target))))))


;; Miscellaneous instructions

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun udf-imm-printer (value stream dstate)
  (declare (ignore dstate))
  (format stream "#~D" (logior (ash (first value) 4)
			       (second value)))))

(define-emitter emit-format-udf 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 12 8) (byte 4 4) (byte 4 0))

(disassem:define-instruction-format
    (format-udf 32 :include 'format-base
		:default-printer '(:name :tab imm))
  (op0 :field (byte 5 20) :value #b11111)
  (imm :fields (list (byte 12 8) (byte 4 0)) :printer #'udf-imm-printer)
  (op1 :field (byte 4 4) :value #b1111))

(define-instruction udf (segment imm)
  (:declare (type (unsigned-byte 16) imm))
  (:printer format-udf
	    ((cond #b1110)
	     (opb0 #b011)
	     (op0 #b11111)
	     (op1 #b1111)))
  (:emitter
   (emit-format-udf eegment
		    #b1110
		    #b011
		    #b11111
		    (ldb (byte 12 4) imm)
		    #b1111
		    (ldb (byte 4 0) imm))))

(define-instruction svc (segment imm24 &optional (cond :al))
  (:declare (type (unsigned-byte 24)))
  (:printer branch-imm
	    ((opb0 #b111)
	     (op #b1)))
  (:emitter
   (emit-branch segment
		(inst-condition-codes (list cond))
		#b111
		#b1
		imm24)))

(define-instruction nop (segment &optional (cc :al))
  (:declare)
  (:printer format-1-immed
	    ((opb0 #b001)
	     (op #b1001)
	     (s 0)
	     (src1 0)
	     (dst #b1111)
	     (immed 0))
	    '(:name))
  (:emitter
   (emit-format-1-immed segment
			(inst-condition-codes (list cc))
			#b001
			#b1001
			#b0
			#b0000
			#b1111
			0)))

;; MRS
(define-emitter emit-format-0-mrs 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16) (byte 4 12) (byte 12 0))

(disassem:define-instruction-format
    (format-0-mrs 32 :include 'format-base)
  (op0  :field (byte 5 20) :value #b10000)
  (op1  :field (byte 4 16) :value #b1111)
  (dst  :field (byte 4 12) :type 'reg)
  (op2  :field (byte 12 0) :value 0))

(define-instruction mrs (segment dst spec-reg &optional (cc :al))
  (:declare (type tn dst)
	    (type (member 'apsr) spec-reg)
	    (type condition-codes cc))
  (:printer format-0-mrs
	    ((opb0 #b000)
	     (op0 #b10000)
	     (op1 #b1111)
	     (op2 0))
	    '(:name cond :tab dst ", " 'apsr))
  (:emitter
   (emit-format-0-mrs segment
		      (inst-condition-codes (list cc))
		      #b000
		      #b10000
		      #b1111
		      (reg-tn-encoding dst)
		      0)))

  

;; Floating-point instructions
;; A7.5

(define-emitter format-vfp-3-arg 32
  (byte 4 28) (byte 3 25) (byte 2 23) (byte 1 22) (byte 2 20) (byte 4 16) (byte 4 12)
  (byte 3 9) (byte 1 8) (byte 1 7) (byte 1 6) (byte 1 5) (byte 1 4) (byte 4 0))

(defconstant format-vfp-3-arg-printer
  `(:name cond
    (:cond ((sz :constant 0) '|.F32|)
	   (t '|.F64|))
    :tab
    dst ", "
    src1 ", "
    src2))

(disassem:define-instruction-format
    (format-vfp-3 32 :include 'format-base
		     :default-printer format-vfp-3-arg-printer)
  (op0   :field (byte 2 23))
  (src1  :fields (list (byte 1 7) (byte 4 16)) :type 'fp-single-reg)
  (dst   :fields (list (byte 1 22) (byte 4 12)) :type 'fp-single-reg)
  (src2  :fields (list (byte 1 5) (byte 4 0)) :type 'fp-single-reg)
  (op1   :field (byte 2 20) :value #b11)
  (op2   :field (byte 3 9) :value #b101)
  (sz    :field (byte 1 8))
  (opa0  :field (byte 1 6))
  (opa1  :field (byte 1 4)))

(defmacro define-vfp-3 (name op0 op1 opa0 opa1 &optional doublep)
  (let ((full-name (symbolicate name (if doublep ".F64" ".F32"))))
    `(define-instruction ,full-name (segment dst src1 src2 &optional (cond :al))
       (:declare)
       (:printer format-vfp-3
		 ((opb0 #b111)
		  (op0 ,op0) (op1 ,op1) (op2 #b101)
		  (opa0 ,opa0) (opa1 ,opa1)
		  (sz ,(if doublep 1 0))
		  ,@(if doublep `((dst nil :type 'fp-double-reg)
				  (src1 nil :type 'fp-double-reg)
				  (src2 nil :type 'fp-double-reg))))
		 :default
		 :print-name ',name)
       (:emitter
	(multiple-value-bind (d vd)
	    (fp-reg-tn-encoding dst doublep)
	  (multiple-value-bind (n vn)
	      (fp-reg-tn-encoding src1 doublep)
	    (multiple-value-bind (m vm)
		(fp-reg-tn-encoding src2 doublep)
	      (emit-format-vfp-3-arg segment
				     (inst-condition-code (list cond))
				     #b111
				     ,op0
				     d
				     ,op1
				     vn
				     vd
				     #b101
				     ,(if doublep 1 0)
				     n
				     ,opa0
				     m
				     ,opa1
				     vm))))))))

(define-vfp-3 vadd #b00 #b11 0 0)
(define-vfp-3 vadd #b00 #b11 0 0 t)
(define-vfp-3 vsub #b00 #b11 1 0)
(define-vfp-3 vsub #b00 #b11 1 0 t)
(define-vfp-3 vmul #b00 #b10 0 0)
(define-vfp-3 vmul #b00 #b10 0 0 t)
(define-vfp-3 vdiv #b01 #b00 0 0)
(define-vfp-3 vdiv #b01 #b00 0 0 t)

(define-emitter emit-format-vfp-2-arg 32
  (byte 4 28) (byte 3 25) (byte 2 23) (byte 1 22) (byte 2 20) (byte 4 16) (byte 4 12)
  (byte 3 9) (byte 1 8) (byte 2 6) (byte 1 5) (byte 1 4) (byte 4 0))

(defconstant format-vfp-2-arg-printer
  `(:name cond
    (:cond ((sz :constant 0) '|.F32|)
	   (t '|.F64|))
    :tab
    dst ", "
    src))

(disassem:define-instruction-format
    (format-vfp-2-arg 32 :include 'format-base
			 :default-printer format-vfp-2-arg-printer)
  (op0   :field (byte 2 23) :value #b01)
  (op    :field (byte 2 20))
  (op1   :field (byte 4 16))
  (dst   :fields (list (byte 1 22) (byte 4 12)) :type 'fp-single-reg)
  (src   :fields (list (byte 1 5) (byte 4 0)))
  (op2   :field (byte 3 9) :value #b101)
  (sz    :field (byte 1 8))
  (ops   :field (byte 1 7))
  (opc3  :field (byte 1 6))
  (opc4  :field (byte 1 4)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fp-reg-as-reg-printer (value stream dstate)
    (reg-arg-printer (second value) stream dstate)))

(defmacro define-vfp-2 (name op op1 ops opc3 opc4 &key doublep ext
						    (dst-type 'fp-single-reg)
						    (src-type 'fp-single-reg)
						    printer)
  (let ((full-name (symbolicate name "." (if doublep
					     (or ext "F64")
					     (or ext "F32")))))
    `(define-instruction ,full-name (segment dst src &optional (cond :al))
       (:declare (type tn dst src))
       (:printer format-vfp-2-arg
		 ((opb0 #b111)
		  (op0 #b01) (op ,op) (op1 ,op1) (op2 #b101)
		  (ops ,ops) (opc3 ,opc3) (opc4 ,opc4)
		  (sz ,(if doublep 1 0))
		  (dst nil :type ',dst-type)
		  (src nil :type ',src-type
		       ,@(when (eq src-type 'reg)
			     `(:printer #'fp-reg-as-reg-printer))))
		 ,(or printer :default)
		 :print-name ',name)
       (:emitter
	(multiple-value-bind (d vd)
	    (fp-reg-tn-encoding dst doublep)
	  (multiple-value-bind (m vm)
	      (fp-reg-tn-encoding src doublep)
	    (emit-format-vfp-2-arg segment
				   (inst-condition-code (list cond))
				   #b111
				   #b01
				   d
				   ,op
				   ,op1
				   vd
				   #b101
				   ,(if doublep 1 0)
				   ,opc3
				   m
				   ,opc4
				   vm)))))))

(define-vfp-2 vabs  #b11 #b0000 #b1 #b1 0)
(define-vfp-2 vabs  #b11 #b0000 #b1 #b1 0 :doublep t
  :dst-type fp-double-reg :src-type fp-double-reg)
(define-vfp-2 vneg  #b11 #b0001 #b0 #b1 0)
(define-vfp-2 vneg  #b11 #b0001 #b0 #b1 0 :doublep t
  :dst-type fp-double-reg :src-type fp-double-reg)
(define-vfp-2 vsqrt #b11 #b0001 #b1 #b1 0)
(define-vfp-2 vsqrt #b11 #b0001 #b1 #b1 0 :doublep t
  :dst-type fp-double-reg :src-type fp-double-reg)

;; Conversions

(defconstant vcvt-printer
  '(:name cond
    (:cond ((op1 :constant #b1101)
	    (:cond ((sz :constant 1) '|.S32.F64|)
		   (t '|.S32.F32|)))
	   ((op1 :constant #b1100)
	    (:cond ((sz :constant 1) '|.U32.F64|)
		   (t '|.U32.F32|)))
	   ((op1 :constant #b1000)
	    (:cond ((sz :constant 1) '|.F64|
		    (:cond ((op :constant 1) '|.S32|)
			   (t '|.U32|)))
		   (t '|.F32|
		      (:cond ((op :constant 1) '|.S32|)
			     (t '|.U32|))))))
    :tab
    dst ", " src))
	   
;; Convert between double and single
(define-vfp-2 vcvt  #b11 #b0111 #b1 #b1 0 :ext "F64.F32"
  :dst-type fp-double-reg)
(define-vfp-2 vcvt  #b11 #b0111 #b1 #b1 0 :doublep t :ext "F32.F64"
  :src-type fp-double-reg)

;; Convert between float and integer
(define-vfp-2 vcvt  #b11 #b1101 #b1 #b1 0 :ext "S32.F32"
					  :dst-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvt  #b11 #b1101 #b1 #b1 0 :doublep t :ext "S32.F64"
					  :dst-type reg :src-type fp-double-reg
  :printer vcvt-printer)
(define-vfp-2 vcvt  #b11 #b1100 #b1 #b1 0 :ext "U32.F32"
					  :dst-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvt  #b11 #b1100 #b1 #b1 0 :doublep t :ext "U32.F64"
					  :dst-type reg :src-type fp-double-reg
  :printer vcvt-printer)

(define-vfp-2 vcvtr #b11 #b1101 #b0 #b1 0 :ext "S32.F32"
					  :dst-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvtr #b11 #b1101 #b0 #b1 0 :doublep t :ext "S32.F64"
  :dst-type reg :src-type fp-double-reg)
(define-vfp-2 vcvtr #b11 #b1100 #b0 #b1 0 :ext "U32.F32"
					  :dst-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvtr #b11 #b1100 #b0 #b1 0 :doublep t :ext "U32.F64"
					  :dst-type reg :src-type fp-double-reg
  :printer vcvt-printer)

(define-vfp-2 vcvt  #b11 #b1000 #b0 #b1 0 :ext "F32.U32"
					  :src-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvt  #b11 #b1000 #b0 #b1 0 :doublep t :ext "F64.U32"
					  :dst-type fp-double-reg :src-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvt  #b11 #b1000 #b1 #b1 0 :ext "F32.S32"
					  :src-type reg
  :printer vcvt-printer)
(define-vfp-2 vcvt  #b11 #b1000 #b1 #b1 0 :doublep t :ext "F64.S32"
					  :dst-type fp-double-reg :src-type reg
  :printer vcvt-printer)

(defmacro define-vfp-cmp (name ops opc3 &key doublep)
  (let ((full-name (symbolicate name (if doublep ".F64" ".F32")))
	(rtype (if doublep 'fp-double-reg 'fp-single-reg)))
    `(define-instruction ,full-name (segment dst src &optional (cc :al))
       (:declare (type dst tn)
		 (type (or tn (float 0.0 0.0)) src))
       (:printer format-vfp-2-arg
		 ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0100) (op2 #b101)
		  (ops ,ops) (opc3 ,opc3) (opc4 0)
		  (sz ,(if doublep 1 0))
		  (dst nil :type ',rtype)
		  (src nil :type ',rtype))
		 :default
	         :print-name ',name)
       (:printer format-vfp-2-arg
		 ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0101) (op2 #b101)
		  (ops ,ops) (opc3 ,opc3) (opc4 0)
		  (sz ,(if doublep 1 0))
		  (dst nil :type ',rtype)
		  (src (list 0 0)))
		 '(:name cond
		   (:cond ((sz :constant 0) '|.F32|)
			  (t '|.F64|))
		   :tab
		   dst ", #0.0")
	         :print-name ',name)
       (:emitter
	(etypecase src
	  (tn
	   (multiple-value-bind (d vd)
	       (fp-reg-tn-encoding dst doublep)
	     (multiple-value-bind (m vm)
		 (fp-reg-tn-encoding src doublep)
	       (emit-format-vfp-2-reg segment
				      (inst-condition-code (list cc))
				      #b111
				      #b01
				      d
				      #b11
				      #b0100
				      vd
				      #b101
				      ,(if doublep 1 0)
				      ,ops
				      ,opc3
				      m
				      0
				      vm))))
	  (float
	   (assert (zerop src))
	   (multiple-value-bind (d vd)
	       (fp-reg-tn-encoding dst doublep)
	     (emit-format-vfp-2-arg segment
				    (inst-condition-code (list cond))
				    #b111
				    #b01
				    d
				    #b11
				    #b0101
				    vd
				    #b101
				    ,(if doublep 1 0)
				    ,ops
				    ,opc3
				    0
				    0
				    0))))))))

(define-vfp-cmp vcmp  #b0 #b1)
(define-vfp-cmp vcmpe #b1 #b1)
(define-vfp-cmp vcmp  #b0 #b1 :doublep t)
(define-vfp-cmp vcmpe #b1 #b1 :doublep t)


;; Convert a float to the floating-point modified immediate constant.
;; See Table A7-18
(defun fp-immed-or-lose (f)
  (declare (type float f))
  ;; Convert to double which is exact.
  (multiple-value-bind (bits lo)
      (kernel:double-float-bits (coerce f 'double-float))
    (let ((sign-bit (ldb (byte 1 31) bits))
	  (exp (ldb (byte 3 20) bits))
	  (e (ldb (byte 8 23) bits))
	  (frac (ldb (byte 4 16) bits)))
      (if (and (or (= e #b01111111) (= e #b10000000))
	       (zerop (ldb (byte 16 0) bits))
	       (zerop lo))
	  (logior (ash sign-bit 7)
		  (ash exp 4)
		  frac)
	  (error "Invalid floating-point immediate: ~S" f)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
;; Print the encoded floating-point immediate value.  We only return a
;; single-float since the bits have way less than single-float
;; accuracy and range anyway.
(defun packed-float-immed-printer (value stream dstate)
  (declare (ignore dstate))
  (let ((sign (ldb (byte 1 3) (first value)))
	(exp (ldb (byte 3 0) (first value)))
	(frac (second value))
	(word 0))
    (setf word (dpb frac (byte 4 19) word))
    (setf word (dpb exp (byte 3 23) word))
    (setf word
	  (dpb (if (zerop (ldb (byte 1 2) exp))
		   #b10000
		   #b01111)
	       (byte 5 26)
	       word))
    (format stream "~S" (if (zerop sign)
			    (kernel:make-single-float word)
			    (- (kernel:make-single-float word)))))))

;; A8.8.339 VMOV: Move float immediate to a float register
;; A8.8.340 VMOV: Move float reg to another float register
(define-emitter emit-format-vfp-vmov-immed 32
  (byte 4 28) (byte 3 25) (byte 2 23) (byte 1 22) (byte 2 20) (byte 4 16) (byte 4 12)
  (byte 3 9) (byte 1 8) (byte 4 4) (byte 4 0))

(disassem:define-instruction-format
    (format-vfp-vmov-immed 32 :include 'format-base)
  (op0   :field (byte 2 23) :value #b01)
  (op    :field (byte 2 20) :value #b11)
  (imm8  :fields (list (byte 4 16) (byte 4 0)) :printer #'packed-float-immed-printer)
  (dst   :fields (list (byte 1 22) (byte 4 12)) :type 'fp-single-reg)
  (op2   :field (byte 3 9) :value #b101)
  (sz    :field (byte 1 8))
  (z     :field (byte 4 4) :value 0))

(define-emitter emit-format-vfp-vmov-reg 32
  (byte 4 28) (byte 3 25) (byte 2 23) (byte 1 22) (byte 2 20) (byte 4 16) (byte 4 12)
  (byte 3 9) (byte 1 8) (byte 2 6) (byte 1 5) (byte 1 4) (byte 4 0))

(defconstant format-vfp-vmov-reg-printer
  `(:name  cond
    :tab
    (:cond ((sz :constant 0) '|.F32|)
	   (t '|.F64|))
	   dst ", " src))

(disassem:define-instruction-format
    (format-vfp-vmov-reg 32 :include 'format-base)
  (op0   :field (byte 2 23) :value #b01)
  (op    :field (byte 2 20) :value #b11)
  (z     :field (byte 4 16) :value 0)
  (dst   :fields (list (byte 1 22) (byte 4 12)) :type 'fp-single-reg)
  (src   :fields (list (byte 1 5) (byte 4 0)) :type 'fp-single-reg)
  (op2   :field (byte 3 9) :value #b101)
  (sz    :field (byte 1 8))
  (op3   :field (byte 2 6) :value #b01)
  (z2    :field (byte 1 4) :value 0))

(defmacro define-vmov (doublep)
  (let ((full-name (symbolicate 'vmov (if doublep ".F64" ".F32"))))
    `(define-instruction ,full-name (segment dst src &optional (cond :al))
       (:declare (type tn dst)
		 (type (or float tn) src))
       (:printer format-vfp-vmov-immed
		 ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (z 0)
		  (sz ,(if doublep 1 0)))
		 :default
		 :print-name 'vmov)
       (:printer format-vfp-vmov-reg
		 ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (op3 #b01)
		  (z 0) (z2 0)
		  (sz ,(if doublep 1 0)))
		 :default
		 :print-name 'vmov)
       (:emitter
	(etypecase src
	  (tn
	   (multiple-value-bind (d vd)
	       (fp-single-reg-tn-encoding dst)
	     (multiple-value-bind (m vm)
		 (fp-single-reg-tn-encoding src)
	       (emit-format-vfp-vmov-reg segment
					 (inst-condition-code (list cond))
					 #b111
					 #b01
					 d
					 #b11
					 #b0000
					 vd
					 #b101
					 ,(if doublep 1 0)
					 #b01
					 m
					 0
					 vm))))
	  (float
	   (multiple-value-bind (d vd)
	       (fp-single-reg-tn-encoding dst)
	     (let ((value (fp-immed-or-lose src)))
	       (emit-format-vfp-vmov-immed segment
					   (inst-condition-code (list cond))
					   #b111
					   #b01
					   d
					   #b11
					   (ldb (byte 4 4) value)
					   vd
					   #b101
					   ,(if doublep 1 0)
					   #b0000
					   (ldb (byte 4 0) value))))))))))

(define-vmov nil)
(define-vmov t)

;; A8.8.343 VMOV between ARM reg to single-precision register

(define-emitter emit-format-7-vfp-vmov-core 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 1 7) (byte 7 0))

(disassem::define-instruction-format
    (format-7-vfp-vmov-core 32 :include 'format-base)
  (op0   :field (byte 4 21) :value #b0000)
  (op    :field (byte 1 20))
  (vn    :fields (list (byte 1 7) (byte 4 16)) :type 'fp-single-reg)
  (reg   :field (byte 4 12) :type 'reg)
  (op1   :field (byte 4 8) :value #b1010)
  (op2   :field (byte 7 0) :value #b0010000))

(define-instruction vmov (segment dst src &optional (cc :al))
  (:declare (type tn dst src))
  (:printer format-7-vfp-vmov-core
	    ((opb0 #b111)
	     (op0 #b0000)
	     (op1 #b1010)
	     (op2 #b0010000)
	     (op 1))
	    '(:name cond :tab reg ", " vn))
  (:printer format-7-vfp-vmov-core
	    ((opb0 #b111)
	     (op0 #b0000)
	     (op1 #b1010)
	     (op2 #b0010000)
	     (op 0))
	    '(:name cond :tab vn ", " reg))
  (:emitter
   (multiple-value-bind (op r fp)
       (cond ((and (typep (sc-case dst) 'reg)
		   (typep (sc-case src) 'single-reg))
	      ;; Move to arm reg
	      (values 1 dst src))
	     ((and (typep (sc-case src) 'reg)
		   (typep (sc-case dst) 'single-reg))
	      ;; Move to float reg
	      (values 0 src dst))
	     (t
	      (error "VMOV requires one ARM reg and one single-float reg")))
     (multiple-value-bind (n vn)
	 (fp-reg-tn-encoding fp nil)
       (emit-format-7-vfp-vmov-core segment
				    #b111
				    #b0000
				    op
				    vn
				    (reg-tn-encoding r)
				    #b1010
				    n
				    #b0010000)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun imm8-offset-printer (value stream dstate)
    (declare (ignore dstate))
    (format stream "~D" (ash value 2))))

(defconstant format-6-vfp-load/store-printer
  '(:name cond
    (:cond ((sz :constant 0) '|.32|)
	   (t '|.64|))
    :tab dst
    ", [" src
    (:cond ((imm8 :constant 0) "]")
	   ((u :constant 1)
	    "#+" imm8 "]")
	   (t
	    "#-" imm8 "]"))))

(define-emitter emit-format-6-vfp-load/store 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 2 20)
  (byte 4 16) (byte 4 12) (byte 3 9) (byte 1 8) (byte 8 0))

(disassem:define-instruction-format
    (format-6-vfp-load/store 32
			     :include 'format-base
			     :default-printer format-6-vfp-load/store-printer)
  (one   :field (byte 1 24) :value 1)
  (u     :field (byte 1 23))
  (dst   :fields (list (byte 1 22) (byte 4 12)))
  (op0   :field (byte 2 20))
  (src   :field (byte 4 16) :type 'reg)
  (op1   :field (byte 3 9) :value #b101)
  (sz    :field (byte 1 8))
  (imm8  :field (byte 8 0)))

(defmacro define-fp-load/store (name op0 &optional doublep)
  (let ((full-name (symbolicate name (if doublep ".64" ".32"))))
    `(define-instruction ,full-name (segment dst src &optional (cond :al))
       (:declare (type tn dst)
		 (type (or tn load-store-index) dst))
       (:printer format-6-vfp-load/store
		 ((opb0 #b110)
		  (one 1)
		  (op0 ,op0)
		  (op1 #b101)
		  (sz ,(if doublep 1 0))
		  (dst nil :type ',(if doublep
				       'fp-double-reg
				       'fp-single-reg)))
		 :default
		 :print-name ',name)
       (:emitter
	(etypecase src
	  (tn
	   (emit-format-6-vfp-load/store segment
					 (inst-condition-code (list cond))
					 #b110
					 #b1
					 1
					 d
					 ,op0
					 (reg-tn-encoding base)
					 vd
					 #b101
					 ,(if doublep 1 0)
					 0))
	  (load-store-index
	   ;; Only certain forms of indexing are allowed here!  Verify
	   ;; them.
	   (assert (eq :immediate (load-store-index-type src)))
	   (assert (and (null shift-type)
			(null shift-amount)
			(null update)
			(null post-indexed)))
	   (let ((offset (load-store-index-offset src))
		 (add (load-store-index-add src)))
	     (assert (zerop (ldb (byte 2 0) offset)))
	     (emit-format-6-vfp-load/store segment
					   (inst-condition-code (list cond))
					   #b110
					   #b1
					   (if add 1 0)
					   d
					   ,op0
					   (reg-tn-encoding base)
					   vd
					   #b101
					   ,(if doublep 1 0)
					   offset))))))))

(define-fp-load/store vldr #b01)
(define-fp-load/store vldr #b01 t)
(define-fp-load/store vstr #b00)
(define-fp-load/store vstr #b00 t)

;; A7.8
(disassem:define-instruction-format
    (format-vfp-xfer-base 32 :include 'format-base)
  (k0   :field (byte 1 24))
  (a    :field (byte 3 21) :value 0)
  (el   :field (byte 1 20))
  (k1   :field (byte 3 9) :value #b101)
  (c    :field (byte 1 8))
  (b    :field (byte 2 5))
  (k2   :field (byte 1 4) :value 0))

(define-emitter emit-format-vfp-fpscr 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 3 21) (byte 1 20) (byte 4 16)
  (byte 4 12) (byte 3 9) (byte 1 8) (byte 1 7) (byte 2 5) (byte 1 4) (byte 4 0))

(disassem:define-instruction-format
    (format-vfp-fpscr 32 :include 'format-base)
  (k0   :field (byte 1 24))
  (a    :field (byte 3 21) :value 0)
  (el   :field (byte 1 20))
  (src1 :field (byte 4 16) :value #b0001)
  (reg  :field (byte 4 12) :type 'reg)
  (k1   :field (byte 3 9) :value #b101)
  (c    :field (byte 1 8))
  (k2   :field (byte 1 7) :value 0)
  (b    :field (byte 2 5))
  (k3   :field (byte 1 4) :value 0)
  (src2 :field (byte 4 0) :value 0))

(define-instruction vmrs (segment fpscr reg &optional (cc :al))
  (:declare (type tn reg)
	    (type (member 'fpscr) fpscr))
  (:printer format-vfp-fpscr
	    ((opb0 #b111)
	     (k0 0)
	     (a #b111)
	     (el #b1)
	     (src1 #b0001)
	     (k1 #b101)
	     (c 0)
	     (k2 0)
	     (b #b00)
	     (k3 1)
	     (src 0))
	    '(:name :tab 'fpscr ", " reg))
  (:emitter
   (emit-format-vfp-fpscr segment
		      (inst-condition-code (list cc))
		      #b111
		      #b0
		      #b111
		      #b1
		      #b0001
		      (reg-tn-encoding reg)
		      #b101
		      #b0
		      #b00
		      #b1
		      #b0000)))

(define-instruction vmsr (segment fpscr reg &optional (cc :al))
  (:declare (type tn reg)
	    (type (member 'fpscr) fpscr))
  (:printer format-vfp-fpscr
	    ((opb0 #b111)
	     (k0 0)
	     (a #b111)
	     (el #b0)
	     (src1 #b0001)
	     (k1 #b101)
	     (c 0)
	     (k2 0)
	     (b #b00)
	     (k3 1)
	     (src 0))
	    '(:name :tab 'fpscr ", " reg))
  (:emitter
   (emit-format-vfp-fpscr segment
		      (inst-condition-code (list cc))
		      #b111
		      #b0
		      #b111
		      #b0
		      #b0001
		      (reg-tn-encoding reg)
		      #b101
		      #b0
		      #b00
		      #b1
			  #b0000)))
