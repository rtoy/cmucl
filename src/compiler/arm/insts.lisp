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

(def-assembler-params
  :scheduler-p nil)


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
    ;; See A7.3
    ;;
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
                              :opcode-column-width 9)


;;; symbols used for disassembly printing
;;;
(defparameter reg-symbols
  (map 'vector #'make-symbol *register-names*)
  "The Lisp names for the ARM integer registers")

(defparameter arm-reg-symbols
  #(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)
  "The ARM names for the integer registers.")

(defvar *use-lisp-register-names* t)
  
(defun get-reg-name (index)
  (aref (if *use-lisp-register-names*
	    reg-symbols
	    arm-reg-symbols)
	index))

(defun maybe-add-notes (value dstate)
  (declare (ignore value dstate))
  (error "Not yet implemented"))

(eval-when (compile load eval)
(defun reg-arg-printer (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (get-reg-name value)))
    (princ regname stream)
    (disassem:maybe-note-associated-storage-ref value
						'registers
						regname
						dstate)))
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
	       (let ((value (logior (ash (second vlist) 1)
				    (first vlist))))
		 (princ 'S stream)
		 (princ value stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  (symbolicate "S" (format nil "~D" value))
		  dstate))))

(disassem:define-argument-type fp-double-reg
  :printer #'(lambda (vlist stream dstate)
	       (declare (stream stream))
	       ;; The fp-reg fields are always split into two parts,
	       ;; so we get a list of values from the two parts.  We
	       ;; use the ARM syntax and numbering for the register.
	       (let ((value (logior (ash (first vlist) 4)
				    (second vlist))))
		 (princ 'D stream)
		 (princ value stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  (symbolicate "D" (format nil "~D" value))
		  dstate))))

;; Table A8-1
(defconstant condition-codes
  '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al))

;; The possible shift types.  We don't include :rrx; that needs to be
;; handled specially since it is encoded as :ror with a shift of 0.
(defconstant shift-types
  '(:lsl :lsr :asr :ror))

(deftype condition-code ()
  `(member ,@condition-codes))

(defconstant condition-code-name-vec
  (coerce condition-codes 'vector))

(defconstant condition-always
  #b1110)

(disassem:define-argument-type condition-code
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate))
	       (unless (= value condition-always)
		 (princ (aref condition-code-name-vec value) stream))))

(deftype shift-type ()
  `(member ,@shift-types))

(disassem:define-argument-type shift-type
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate))
	       (princ (elt shift-types value) stream)))

;; Convert the specified condition code to corresponding instruction
;; encoding.  Signal an error for unknown conditions.
(defun condition-code-encoding (c)
  (let ((position (position  c condition-codes)))
    (or position
	(error "Unknown condition code ~S" c))))

;; Convert SHIFT-TYPE to the value for an instruction field.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun shift-type-encoding (shift-type)
  (if (eq shift-type :rrx)
      3
      (or (position shift-type shift-types)
	  (error "Unknown shift type: ~S" shift-type))))
) ; eval-when

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

(defun encode-immediate (value)
  "Find a V and N such that VALUE = rotate_right(V, 2*N)
  The values N and V are returned as multiple values, in that order.
  If no such values of N and V exist, then NIL is returned."
  (cond ((minusp value)
	 (encode-immediate (ldb (byte 32 0) value)))
	((< value 256)
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
		 (encode-immediate new)
	       (when rot
		 (values (mod (+ 4 rot) 16) val))))))))


;;; Define instruction formats. See DDI0406C_b, section A5 for details.

;; Section A5.1 describes the basic format of an instruction.
;; However, the manual makes a mess of it when describing
;; instructions; the descriptions don't follow this basic format and
;; arbitrarily combines or splits fields.
;;
;; Section A5.1.  This is the basic encoding form, not including the
;; op field (byte 1 4).  That is sometimes used for other things.
;; Also, The names of all the formats below are numbered based on the
;; value of the 3-bit opb0 field.
(disassem:define-instruction-format
    (format-base 32)
  (cond  :field (byte 4 28) :type 'condition-code)
  (opb0  :field (byte 3 25)))

(defconstant format-1-immed-printer
  `(:name (:unless (:constant ,condition-always) cond)
	  :tab
	  dst ", " src1 ", " immed))

(defconstant format-1-immed-set-printer
  `(:name (:unless (:constant ,condition-always) cond)
	  :tab
	  src1 ", " immed))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun modified-immed-printer (value stream dstate)
  (declare (type (unsigned-byte 12) value) (stream stream)
	   (ignore dstate))
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
  `(:name cond
	  :tab
	  dst ", " src1 ", " src2
	  (:unless (shift :constant 0)
	    (:cond ((type :constant #b11) ; ror or rrx
		    (:cond ((shift :constant 0) " " 'rrx)
			   (t
			    " " 'ror " #" shift)))
		   (t
		    " " type " #" shift)))))

(defconstant format-0-reg-set-printer
  `(:name cond
	  :tab
	  src1 ", " src2
	  (:unless (shift :constant 0)
	    (:cond ((type :constant #b11) ; ror or rrx
		    (:cond ((shift :constant 0) " " 'rrx)
			   (t
			    " " 'ror " #" shift)))
		   (t
		    " " type " #" shift)))))

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
  `(:name cond
	  :tab
	  dst ", " src1 ", " src2 " " type " " sreg))

(defconstant format-0-reg-shifted-set-printer
  `(:name cond
	  :tab
	  src1 ", " src2 " " type " " sreg))

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
;; instructions.  Use MAKE-SHIFT to specify the flexible operand.
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
  "Specifies the shift that is to be applied.

  Reg is the register to be shifted.  Shift-type specifies the type of
  shift and should be one of :lsl, :lsr, :asr, :ror, or :rrx.  The
  Amound is optional and is the amount of the shift, which is a small
  positive integer or a register.  If the shift type is :rrx, the
  amount cannot be specified."
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

;; Handle emitting data processing instruction.
(defun emit-data-proc-format (segment dst src1 src2 cc
			      &key opcode set-flags-bit)
  (declare (type (or tn (member 0)) dst src1)
	   (type (or (signed-byte 32)
		     (unsigned-byte 32)
		     reg
		     flex-operand)
		 src2)
	   (type (or null (unsigned-byte 4)) opcode)
	   (type (or null bit) set-flags-bit))
  (flet ((reg-encoding (r)
	   (if (typep r 'tn)
	       (reg-tn-encoding r)
	       r)))
    (etypecase src2
      (integer
       (multiple-value-bind (rot val)
	   (encode-immediate src2)
	 (unless rot
	   (error "Cannot encode the immediate value ~S" src2))
	 (emit-format-1-immed segment
			      (condition-code-encoding cc)
			      #b001
			      opcode
			      set-flags-p
			      (reg-encoding src1)
			      (reg-encoding dst)
			      (logior (ash rot 8) val))))
      (reg
       (emit-format-0-reg segment
			  (condition-code-encoding cc)
			  #b000
			  opcode
			  set-flags-bit
			  (reg-encoding src1)
			  (reg-encoding dst)
			  0
			  (shift-type-encoding :lsl)
			  #b0
			  (reg-tn-encoding src2)))
      (flex-operand
       (ecase (flex-operand-type src2)
	 (:reg-shift-imm
	  (emit-format-0-reg segment
			     (condition-code-encoding cc)
			     #b000
			     opcode
			     set-flags-bit
			     (reg-encoding src1)
			     (reg-encoding dst)
			     (flex-operand-shift-reg-or-imm src2)
			     (shift-type-encoding (flex-operand-shift-type src2))
			     #b0
			     (reg-tn-encoding (flex-operand-reg src2))))
	 (:reg-shift-reg
	  (emit-format-0-reg-shift segment
				   (condition-code-encoding cc)
				   #b000
				   opcode
				   set-flags-bit
				   (reg-encoding src1)
				   (reg-encoding dst)
				   (reg-tn-encoding (flex-operand-shift-reg-or-imm src2))
				   #b0
				   (shift-type-encoding (flex-operand-shift-type src2))
				   #b1
				   (reg-tn-encoding (flex-operand-reg src2)))))))))

;; Define two instructions: one that does not set the condition codes
;; and one that does.  INST-DEF is the instruction definer to use and
;; the basic name of the instruction is NAME.  ARGS is for any
;; additional options needed by INST-DEF.
(defmacro define-set-flags-insts (inst-def name &rest args)
  `(progn
     (,inst-def ,name 0 ,@args)
     (,inst-def ,(symbolicate name "S") 1 ,@args)))


(defmacro define-basic-data-proc-inst (name set-flags-bit opcode)
  `(define-instruction ,name (segment dst src1 src2 &optional (cond :al))
     (:declare (type tn dst)
	       (type tn src1)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg
			 flex-operand)
		     src2)
	       (type condition-code cond))
     (:printer format-1-immed
	       ((opb0 #b001) (op ,opcode)
		(s ,set-flags-bit)
		(src1 nil :type 'reg)
		(dst nil :type 'reg)))
     (:printer format-0-reg
	       ((opb0 #b000) (op ,opcode) (rs 0)
		(s ,set-flags-bit)
		(src1 nil :type 'reg)
		(dst nil :type 'reg)))
     (:printer format-0-reg-shifted
	       ((opb0 #b000) (op ,opcode) (rs 1)
		(z 0)
		(s ,set-flags-bit)
		(src1 nil :type 'reg)
		(dst nil :type 'reg)))
     (:emitter
      (emit-data-proc-format segment dst src1 src2 cond
			     :opcode ,opcode
			     :set-flags-bit ,set-flags-bit))))

;; See A5.2 and Table A5-3.
;; Note: We do not implement the ADR instruction.
(macrolet
    ((frob (basename opcode)
       `(define-set-flags-insts define-basic-data-proc-inst ,basename ,opcode)))
  (frob and #b0000)
  (frob eor #b0001)
  (frob sub #b0010)
  (frob rsb #b0011)
  (frob add #b0100)
  (frob adc #b0101)
  (frob sbc #b0110)
  (frob rsc #b0111)
  (frob orr #b1100)
  (frob bic #b1110))

;; Compare type instructions need to be handled separately from the
;; above data processing instructions because the src1 isn't used.
(defmacro define-compare-inst (name opcode)
  `(define-instruction ,name (segment dst src1 src2 &optional (cond :al))
     (:declare (type tn dst)
	       (type tn src1)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg
			 flex-operand)
		     src2)
	       (type condition-code cond))
     (:printer format-1-immed
	       ((opb0 #b001) (op ,opcode)
		(s 1)
		(src1 nil :type 'reg)
		(dst 0))
	       format-1-immed-set-printer)
     (:printer format-0-reg
	       ((opb0 #b000) (op ,opcode) (rs 0)
		(s 1)
		(src1 nil :type 'reg)
		(dst 0))
	       format-0-reg-set-printer)
     (:printer format-0-reg-shifted
	       ((opb0 #b000) (op ,opcode) (rs 1)
		(z 0)
		(s 1)
		(src1 nil :type 'reg)
		(dst 0))
	       format-0-reg-shifted-set-printer)
     (:emitter
      (emit-data-proc-format segment 0 src1 src2 cond
			     :opcode ,opcode
			     :set-flags-bit 1))))

(define-compare-inst tst #b1000)
(define-compare-inst teq #b1001)
(define-compare-inst cmp #b1010)
(define-compare-inst cmn #b1011)


;; A8.8.115, A8.8.116, A8.8.117
;;
;; MVN Aka bitnot
(defmacro define-mvn-inst (name set-flags-bit)
  `(define-instruction ,name (segment dst src2 &optional (cond :al))
     (:declare (type tn dst)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg
			 flex-operand)
		     src2)
	       (type condition-code cond))
     (:printer format-1-immed
	       ((opb0 #b001) (op #b1111)
		(s ,set-flags-bit)
		(src1 0)
		(dst nil :type 'reg))
	       `(:name cond
		       :tab
		       dst ", " immed))
     (:printer format-0-reg
	       ((opb0 #b000) (op #b1111)
		(s ,set-flags-bit)
		(rs 0)
		(src1 0)
		(dst nil :type 'reg))
	       `(:name cond :tab
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
	       ((opb0 #b000) (op #b1111)
		(s ,set-flags-bit)
		(rs 1)
		(src1 0)
		(dst nil :type 'reg))
	       `(:name cond :tab
		       dst ", " src2 " " type " " sreg))
     (:emitter
      (emit-data-proc-format segment dst 0 src2 cond
			     :opcode #b1111
			     :set-flags-bit ,set-flags-bit))))

(define-set-flags-insts define-mvn-inst mvn)

;; See A8.8.105.
;;
;; We don't support the pseudo-op with a flexible operand 2 denoting
;; register shifted register, which are basically synonyms for the
;; shift instructions.
(defmacro define-mov-inst (name set-flags-bit)
  `(define-instruction ,name (segment dst src2 &optional (cond :al))
     (:declare (type tn dst)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg
			 flex-operand)
		     src2)
	       (type condition-code cond))
     (:printer format-1-immed
	       ((opb0 #b001)
		(op #b1101)
		(s ,set-flags-bit)
		(src1 0)
		(dst nil :type 'reg))
	       '(:name cond :tab
		 dst ", " immed))
     (:emitter
      (etypecase src2
	(integer
	 (multiple-value-bind (rot val)
	     (encode-immediate src2)
	   (unless rot
	     (error "Cannot encode the immediate value ~S~%" src2))
	   (emit-format-1-immed segment
				(condition-code-encoding cond)
				#b001
				#b1101
				,set-flags-bit
				0
				(reg-tn-encoding dst)
				(logior (ash rot 8) val))))
	(reg
	 (emit-format-0-reg segment
			    (condition-code-encoding cond)
			    #b000
			    #b1101
			    ,set-flags-bit
			    (reg-tn-encoding src1)
			    (reg-tn-encoding dst)
			    0
			    (shift-type-encoding :lsl)
			    #b0
			    (reg-tn-encoding src2)))
	(flex-operand
	 (ecase (flex-operand-type src2)
	   (:reg-shift-imm
	    (emit-format-0-reg segment
			       (condition-code-encoding cond)
			       #b000
			       #b1101
			       ,set-flags-bit
			       0
			       (reg-tn-encoding dst)
			       (flex-operand-shift-reg-or-imm src2)
			       (shift-type-encoding (flex-operand-shift-type src2))
			       #b0
			       (reg-tn-encoding (flex-operand-reg src2))))
	   (:reg-shift-reg
	    (error "Use the shift instructions instead of MOV with a shifted register"))))))))

(define-set-flags-insts define-mov-inst mov)

(defmacro define-basic-shift-inst (name set-flags-bit shift-type)
  `(define-instruction ,name
       (segment dst src2 shift &optional (cond :al))
     (:declare (type tn dst src2)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg)
		     shift)
	       (type condition-code cond))
     (:printer format-0-reg
	       ((opb0 #b000)
		(op #b1101)
		(rs 0)
		(src1 0)
		(dst nil :type 'reg)
		(s ,set-flags-bit)
		(type ,(shift-type-encoding shift-type)))
	       '(:name cond :tab
		 dst ", " src2 ", #" shift))
     (:printer format-0-reg-shifted
	       ((opb0 #b000)
		(op #b1101)
		(rs 1)
		(src1 0)
		(dst nil :type 'reg)
		(s ,set-flags-bit)
		(type ,(shift-type-encoding shift-type)))
	       '(:name cond :tab
		 dst ", " src2 ", " sreg))
     (:emitter
      (etypecase shift
	(integer
	 (emit-format-0-reg segment
			    (condition-code-encoding cond)
			    #b000
			    #b1101
			    ,set-flags-bit
			    #b0000
			    (reg-tn-encoding dst)
			    shift
			    (shift-type-encoding ,shift-type)
			    #b0
			    (reg-tn-encoding src2)))
	(reg
	 (emit-format-0-reg-shift segment
				  (condition-code-encoding opts)
				  #b000
				  #b1101
				  ,set-flags-bit
				  #b0000
				  (reg-tn-encoding dst)
				  (reg-tn-encoding shift)
				  #b0
				  (shift-type-encoding ,shift-type)
				  #b1
				  (reg-tn-encoding src2)))))))

;; Shift instructions
(macrolet
    ((frob (shift-type)
       `(define-set-flags-insts define-basic-shift-inst
	    ,(symbolicate (symbol-name shift-type)) ,shift-type)))
  (frob :asr)
  (frob :lsl)
  (frob :lsr)
  (frob :ror))

(defmacro define-rrx-inst (name set-flags-bit)
  `(define-instruction ,name (segment dst src2 &optional (cond :al))
     (:declare (type tn dst src2)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg)
		     shift)
	       (type condition-code cond))
     (:printer format-0-reg
	       ((opb0 #b000)
		(op #b1101)
		(rs 0)
		(src1 0)
		(dst nil :type 'reg)
		(type (shift-type-encoding :ror))
		(s ,set-flags-bit)
		(shift 0))
	       '(:name cond :tab
		 dst ", " src2))
     (:emitter
      (emit-format-0-reg segment
			 (condition-code-encoding cond)
			 #b000
			 #b1101
			 ,set-flags-bit
			 0
			 (reg-tn-encoding dst)
			 0
			 (shift-type-encoding :ror)
			 #b0
			 (reg-tn-encoding src2)))))

(define-set-flags-insts define-rrx-inst rrx)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun split-imm16-printer (value stream dstate)
  (declare (list value) (stream stream)
	   (ignore dstate))
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

;; A8.8.106
(define-instruction movt (segment dst src &optional (cond :al))
  (:declare (type tn dst)
	    (type (or (unsigned-byte 16) fixup) src)
	    (type condition-code cond))
  (:printer format-mov16
	    ((opb0 #b001)
	     (op #b10100)))
  (:emitter
   (etypecase src
     (integer
      (emit-format-mov16 segment
			 (condition-code-encoding cond)
			 #b001
			 #b10100
			 (ldb (byte 4 12) src)
			 (reg-tn-encoding dst)
			 (ldb (byte 12 0) src)))
     (fixup
      (note-fixup segment :movt src)
      (emit-format-mov16 segment
			 (condition-code-encoding cond)
			 #b001
			 #b10110
			 0
			 (reg-tn-encoding dst)
			 0)))))

;; A8.8.102
(define-instruction movw (segment dst src &optional (cond :al))
  (:declare (type tn dst)
	    (type (or (unsigned-byte 16) fixup) src)
	    (type condition-code cond))
  (:printer format-mov16
	    ((opb0 #b001)
	     (op #b10000)))
  (:emitter
   (etypecase src
     (integer
      (emit-format-mov16 segment
			 (condition-code-encoding cond)
			 #b001
			 #b10000
			 (ldb (byte 4 12) imm16)
			 (reg-tn-encoding dst)
			 (ldb (byte 12 0) imm16)))
     (fixup
      (note-fixup segment :mov3 src)
      (emit-format-mov16 segment
			 (condition-code-encoding cond)
			 #b001
			 #b10000
			 0
			 (reg-tn-encoding dst)
			 0)))))

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

;; A8.8.114
;;
;; Basic multiply, returning the bottom 32 bits of a 32x32 multiply.
(defmacro define-mul-inst (name set-flags-bit)
  `(define-instruction ,name
       (segment dst src1 src2 &optional (cond :al))
     (:declare (type tn dst src1 src2)
	       (type condition-code cond))
     (:printer format-0-mul
	       ((opb0 #b000)
		(op #b0000)
		(s ,set-flags-bit)
		(op1 #b1001)
		(src3 0))
	       `(:name cond :tab
		       dst ", " src1 ", " src2))
     (:emitter
      (emit-format-0-mul segment
			 (condition-code-encoding cond)
			 #b000
			 #b0000
			 ,set-flags-bit
			 (reg-tn-encoding dst)
			 #b0000
			 (reg-tn-encoding src2)
			 #b1001
			 (reg-tn-encoding src1)))))

(define-set-flags-insts define-mul-inst mul)

(defmacro define-basic-4-arg-mul-inst (name set-flags-bit op &key two-outputs)
  `(define-instruction ,name
	 (segment dst dst2-or-src src2 src3 &optional (cond :al))
       (:declare (type tn dst dst2-or-src src2)
		 (type condition-code cond))
       (:printer format-0-mul
		 ((opb0 #b000) (op ,op)
		  (s ,set-flags-bit)
		  (op1 #b1001)
		  (src3 nil :type 'reg))
		 ',(if two-outputs
		       `(:name cond :tab
			       src3 ", " dst ", " src1 ", " src2)
		       `(:name cond :tab
			       dst ", " src1 ", " src2 ", " src3)))
       (:emitter
	(emit-format-0-mul segment
			   (condition-code-encoding cond)
			   #b000
			   ,op
			   ,set-flags-bit
			   ,@(if two-outputs
				 `((reg-tn-encoding dst2-or-src)
				   (reg-tn-encoding dst)
				   (reg-tn-encoding src3)
				   #b1001
				   (reg-tn-encoding src2))
				 `((reg-tn-encoding dst)
				   (reg-tn-encoding src3)
				   (reg-tn-encoding src2)
				   #b1001
				   (reg-tn-encoding dst2-or-src)))))))

(macrolet
    ((frob (name opcode &key two-outputs)
       `(define-set-flags-insts define-basic-4-arg-mul-inst
	  ,name ,opcode :two-outputs two-outputs)))

  ;; A8.8.257; unsigned 32x32->64 multiply 
  (frob umull #b0100 :two-outputs t)
  ;; A8.8.189; signed 32x32->64 multiply
  (frob smull #b0110 :two-outputs t)
  ;; A8.8.100:  Multiply-add
  (frob mla   #b0001)
  ;; A8.8.256; unsiged 64-bit multiply-add
  (frob umlal #b0101 :two-outputs t)
  ;; A8.8.178; 64-bit signed multiply-add
  (frob smlal #b0111 :two-outputs t))

;; A8.8.101; Multiply-subtract
(define-basic-4-arg-mul-inst mls 0 #b0011)

;; A8.8.255; 64-bit unsigned multiply-add, adding 2 32-bit values
(define-basic-4-arg-mul-inst umaal 0 #b0010 :two-outputs t)

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

(defmacro define-div-inst (name op1 op2)
  `(define-instruction ,name (segment dst src1 src2 &optional (cond :al))
     (:declare (type tn dst src1 src2)
	       (type condition-code cond))
     (:printer format-div
	       ((opb0 #b011) (op1 ,op1) (a #b1111) (op2 ,op2) (one #b1)))
     (:emitter
      (emit-format-div segment
		       (condition-code-encoding cond)
		       #b011
		       #b10
		       ,op1
		       (reg-tn-encoding dst)
		       #b1111
		       (reg-tn-encoding src2)
		       ,op2
		       #b1
		       (reg-tn-encoding src1)))))

(define-div-inst sdiv #b001 #b000)
(define-div-inst udiv #b011 #b000)

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
  (:declare (type (unsigned-byte 16) value)
	    (type condition-code cond))
  (:printer format-0-bkpt
	    ((opb0 #b000)
	     (op0 #b10010)
	     (op1 #b0111)))
  (:emitter
   (unless (eq cond :al)
     (error "BPKT is undefined if the condition is not :AL (always)"))
   (emit-format-0-bkpt segment
		       (condition-code-encoding cond)
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
	  dst ", [" src1
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
	  dst ", [" src1
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
	  dst ", [" src1
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
  (p      :field (byte 1 24))
  (u      :field (byte 1 23))
  (imm    :field (byte 1 22) :value 0)
  (w      :field (byte 1 21))
  (ld     :field (byte 1 20))
  (src1   :field (byte 4 16) :type 'reg)
  (dst    :field (byte 4 12) :type 'reg)
  (z      :field (byte 4 8) :value 0)
  (one    :field (byte 1 7) :value 1)
  (signed :field (byte 1 6))
  (op2    :field (byte 2 4))
  (src2   :field (byte 4 0) :type 'reg))

;; LDRH/STRH (register)

(defconstant format-0-halfword-imm-printer
  `(:name cond
          :tab
	  dst ", [" src1
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
  (declare (list value) (stream stream)
	   (ignore dstate))
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
  indexed)

(defun make-ea (base-reg &key (offset 0) update
			   (add t addp) 
			   (shift-type :lsl shift-type-p)
			   (amount 0 amountp)
			   indexed)
  "Create effective address for load/store instructions.
  Base-reg specifies the base register with the following options:

  :Offset
     The offset from the base register.  This can be either a integer
     or a register
  :Update
     Non-nil means the base register is updated from the offset
  :Add
     Non-nil means the offset is added.  Default is to add.
  :Indexed
     Non-nil means the base register plus the offset is the address.
     Otherwise, the base-register is the address.
  :Shift-type
     The shift type to be applied to the offset register
  :Amount
     Amount of the shift

  Note that if the offset is an integer, the :shift-type, :add and
  :amount options are not allowed.

  If the offset is a register, all options are allowed.
"
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
			    :indexed indexed))
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
			      :indexed indexed)))))

(defun decode-load-store-index (index)
  "Determine the P, U, and W bits from the load-store-index"
  (values (if (load-store-index-post-indexed index) 0 1)
	  (if (load-store-index-add index) 1 0)
	  (if (load-store-index-update index) 1 0)))

;; A5.3 and table A5-15
;; Load/store for words and unsigned bytes
(defmacro define-load/store-inst (name loadp &optional bytep)
  `(define-instruction ,name (segment reg address &optional (cond :al))
     (:declare (type tn reg)
	       (type load-store-index address)
	       (type condition-code cond))
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
			      (condition-code-encoding cond)
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
				(condition-code-encoding cond)
				#b010
				p
				u
				,(if bytep 1 0)
				w
				,(if loadp 1 0)
				(reg-tn-encoding (load-store-index-base-reg address))
				(reg-tn-encoding reg)
				(load-store-index-offset address))))))))

(define-load/store-inst ldr t)
(define-load/store-inst ldrb t t)
(define-load/store-inst str nil)
(define-load/store-inst strb nil t)

;; A5.2.8; extra load/store instructions for halfwords (16-bit signed
;; and unsigned) and signed bytes.
(defmacro define-load/store-extra-inst (name &optional loadp bytep signedp)
  `(define-instruction ,name (segment reg address &optional (cond :al))
     (:declare (type tn reg)
	       (type load-store-index address)
	       (type condition-code cond))
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
					 (condition-code-encoding cond)
					 #b000
					 p
					 u
					 0
					 w
					 ,(if loadp 1 0)
					 (reg-tn-encoding
					  (load-store-index-base-reg address))
					 (reg-tn-encoding reg)
					 1
					 0
					 sign
					 op2
					 (reg-tn-encoding (load-store-index-offset address)))))
	  (:immediate
	   (multiple-value-bind (p u w)
	       (decode-load-store-index src2)
	     (emit-format-0-halfword-imm segment
					 (condition-code-encoding cond)
					 #b000
					 p
					 u
					 1
					 w
					 ,(if loadp 1 0)
					 (reg-tn-encoding
					  (load-store-index-base-reg address))
					 (reg-tn-encoding reg)
					 (ldb (byte 4 4)
					      (load-store-index-offset address))
					 1
					 sign
					 op2
					 (ldb (byte 4 0)
					      (load-store-index-offset address))))))))))

(define-load/store-extra-inst ldrh t)
(define-load/store-extra-inst strh nil)
(define-load/store-extra-inst ldrsh t nil t)
(define-load/store-extra-inst ldrsb t t)



;;; Branch instructions
(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
		 (declare (type (signed-byte 24) value)
			  (type disassem:disassem-state dstate))
		 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

(defconstant branch-imm-printer
  `(:name cond :tab imm24))

(define-emitter emit-branch-imm 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 24 0))

(disassem:define-instruction-format
    (branch-imm 32 :include 'format-base
		   :default-printer branch-imm-printer)
  (op :field (byte 1 24) :value 1)
  (imm24 :field (byte 24 0)))

(defconstant branch-reg-printer
  `(:name cond :tab src1))

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
			  (condition-code-encoding cond)
			  opb0
			  op
			  ;; The offset in the instruction is a word
			  ;; offset, not byte.
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
		       (condition-code-encoding :al)
		       #b000
		       #b10010
		       #b111111111111
		       #b0011
		       (reg-tn-encooding target))))))


;; Miscellaneous instructions

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun udf-imm-printer (value stream dstate)
  (declare (list value) (stream stream)
	   (ignore dstate))
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

;; A8.8.247
;; Undefined instruction
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

;; A8.8.228
(define-instruction svc (segment imm24 &optional (cond :al))
  (:declare (type (unsigned-byte 24))
	    (type condition-code cond))
  (:printer branch-imm
	    ((opb0 #b111)
	     (op #b1)))
  (:emitter
   (emit-branch segment
		(condition-code-encoding cond)
		#b111
		#b1
		imm24)))

;; A8.8.119.  Note this was introduced in ARMv6K and ARMv6T2. If you
;; want one that works eveyrwhere, use MOV R0, R0 (ARM) or MOV R8, R8
;; (Thumb).
;;
(define-instruction nop (segment &optional (cond :al))
  (:declare (type condition-code cond))
  (:printer format-1-immed
	    ((opb0 #b001)
	     (op #b1001)
	     (s 0)
	     (src1 0)
	     (dst #b1111)
	     (immed 0))
	    '(:name cond)
	    :print-name 'nop)
  (:emitter
   (emit-format-1-immed segment
			(condition-code-encoding cond)
			#b001
			#b1001
			#b0
			#b0000
			#b1111
			0)))

;; A8.8.33

(define-emitter emit-format-0-clz 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16) (byte 4 12)
  (byte 8 4) (byte 4 0))

(disassem::define-instruction-format
    (format-0-clz 32 :include 'format-base
		     :default-printer '(:name cond :tab dst ", " src2))
  (op0  :field (byte 5 20) :value #b10110)
  (src  :field (byte 4 16) :value #b1111)
  (dst  :field (byte 4 12) :type 'reg)
  (op1  :field (byte 8 4) :value #b11110000)
  (src2 :field (byte 4 0) :type 'reg))

(define-instruction clz (segment dst src &optional (cond :al))
  (:declare (type tn dst src)
	    (type condition-code cond))
  (:printer format-0-clz
	    ((opb0 #b000)
	     (op0 #b10110)
	     (src #b1111)
	     (op1 #b11110000)))
  (:emitter
   (emit-format-0-clz segment
		      (condition-code-encoding cond)
		      #b000
		      #b10110
		      #b1111
		      (reg-tn-encoding dst)
		      #b11110000
		      (reg-tn-encoding src))))

;; A8.8.109: MRS
(define-emitter emit-format-0-mrs 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16) (byte 4 12) (byte 12 0))

(disassem:define-instruction-format
    (format-0-mrs 32 :include 'format-base)
  (op0  :field (byte 5 20) :value #b10000)
  (op1  :field (byte 4 16) :value #b1111)
  (dst  :field (byte 4 12) :type 'reg)
  (op2  :field (byte 12 0) :value 0))

;; A8.8.109 says spec-reg can be either APSR or CPSR; we only support
;; APSR.
(define-instruction mrs (segment dst spec-reg &optional (cond :al))
  (:declare (type tn dst)
	    (type (member 'apsr) spec-reg)
	    (type condition-code cond))
  (:printer format-0-mrs
	    ((opb0 #b000)
	     (op0 #b10000)
	     (op1 #b1111)
	     (op2 0))
	    '(:name cond :tab dst ", " 'apsr))
  (:emitter
   (emit-format-0-mrs segment
		      (condition-code-encoding cond)
		      #b000
		      #b10000
		      #b1111
		      (reg-tn-encoding dst)
		      0)))

(define-emitter emit-formt-0-msr 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 2 18) (byte 14 4) (byte 4 0))

(disassem:define-instruction-format
    (format-0-msr 32 :include 'format-base)
  (op0  :field (byte 5 20) :value #b10010)
  (mask :field (byte 2 18))
  (op1  :field (byte 14 4) :value #b00111100000000)
  (src  :field (byte 4 0) :type 'reg))

(define-instruction msr (segment spec-reg reg &optional (cond :al))
  (:declare (type tn reg)
	    (type (member apsr-nzcvq apsr-g apsr-nzcvqg) spec-reg)
	    (type condition-code cond))
  (:printer format-0-msr
	    ((opb0 #b000)
	     (op0 #b10010)
	     (op1 #b00111100000000))
	    `(:name cond :tab
		    (:cond ((mask :constant #b10)
			    'apsr-nzcvq)
			   ((mask :constant #b01)
			    'apsr-g)
			   ((mask :constant #b11)
			    'apsr-nzcvqg))
		    ", " src))
  (:emitter
   (let ((mask (ecase spec-reg
		 (apsr-nzcvq  #b10)
		 (apsr-g      #b01)
		 (apsr-nzcvqg #b11))))
     (emit-format-0-msr segment
			(condition-code-encoding cond)
			#b000
			#b10010
			mask
			#b00111100000000
			(reg-tn-encoding reg)))))

(defun %li (reg value)
  (etypecase value
    ((or (signed-byte 32) (unsigned-byte 32))
     (cond ((encode-immediate value)
	    (inst mov reg value))
	   (t
	    (let ((hi (ldb (byte 16 16) value))
		  (lo (ldb (byte 16 0) value)))
	      (unless (zerop hi)
		(inst movt reg hi))
	      (unless (zerop lo)
		(inst movw reg lo))))))
    (fixup
     (inst movt reg value)
     (inst movw reg value))))

(define-instruction-macro li (reg value)
  `(%li ,reg, value))

  

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
	  dst ", " src1 ", " src2))

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

(defmacro define-vfp-3-inst (name inst-name op0 op1 opa0 opa1 &optional doublep)
  `(define-instruction ,inst-name (segment dst src1 src2 &optional (cond :al))
       (:declare (type tn dst src1 src2)
		 (type condition-code cond))
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
				     (condition-code-encoding cond)
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
				     vm)))))))

(macrolet
    ((frob (name op0 op1 opa0 opa1)
       `(progn
	  (define-vfp-3-inst ,name ,(symbolicate name ".F32")
	    ,op0 ,op1 ,opa0 ,opa1)
	  (define-vfp-3-inst ,name ,(symbolicate name ".F64")
	    ,op0 ,op1 ,opa0 ,opa1 t))))
  (frob vadd #b00 #b11 0 0)
  (frob vsub #b00 #b11 1 0)
  (frob vmul #b00 #b10 0 0)
  (frob vdiv #b01 #b00 0 0))


(define-emitter emit-format-vfp-2-arg 32
  (byte 4 28) (byte 3 25) (byte 2 23) (byte 1 22) (byte 2 20) (byte 4 16) (byte 4 12)
  (byte 3 9) (byte 1 8) (byte 1 7) (byte 1 6) (byte 1 5) (byte 1 4) (byte 4 0))

(defconstant format-vfp-2-arg-printer
  `(:name cond
    (:cond ((sz :constant 0) '|.F32|)
	   (t '|.F64|))
    :tab
    dst ", " src))

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

(defmacro define-vfp-2-inst (name inst-name op op1 ops opc3 opc4
			     &key (size-bit 0)
			       (dst-type 'fp-single-reg)
			       (src-type 'fp-single-reg)
			       printer)
  (check-type size-bit bit)
  (check-type dst-type (member fp-single-reg fp-double-reg))
  (check-type src-type (member fp-single-reg fp-double-reg))
  `(define-instruction ,inst-name (segment dst src &optional (cond :al))
     (:declare (type tn dst src)
	       (type condition-code cond))
     (:printer format-vfp-2-arg
	       ((opb0 #b111)
		(op0 #b01) (op ,op) (op1 ,op1) (op2 #b101)
		(ops ,ops) (opc3 ,opc3) (opc4 ,opc4)
		(sz ,size-bit)
		(dst nil :type ',dst-type)
		(src nil :type ',src-type))
	       ,(or printer :default)
	       :print-name ',name)
     (:emitter
      (multiple-value-bind (d vd)
	  (fp-reg-tn-encoding dst (eq dst-type 'fp-double-reg))
	(multiple-value-bind (m vm)
	    (fp-reg-tn-encoding src (eq src-type 'fp-double-reg)))
	(emit-format-vfp-2-arg segment
			       (condition-code-encoding cond)
			       #b111
			       #b01
			       d
			       ,op
			       ,op1
			       vd
			       #b101
			       ,size-bit
			       #b1
			       ,opc3
			       m
			       ,opc4
			       vm)))))

(macrolet
    ((double-inst (name op op1 ops opc3)
       `(define-vfp-2-inst ,name ,(symbolicate name ".F64")
	  ,op ,op1 ,ops, opc3 0
	  :size-bit 1
	  :dst-type fp-double-reg
	  :src-type fp-double-reg))
     (single-inst (name op op1 ops opc3)
       `(define-vfp-2-inst ,name ,(symbolicate name ".F32")
	  ,op ,op1 ,ops, opc3 0))
     (frob (name op1 ops)
       `(progn
	  (double-inst ,name #b11, op1, ops #b1)
	  (single-inst ,name #b11, op1, ops #b1))))
  (frob vabs  #b0000 #b1)
  (frob vneg  #b0001 #b0)
  (frob vsqrt #b0001 #b1))

;; Conversions

(defconstant vcvt-printer
  `(:name cond
	  (:cond ((op1 :constant #b1101)
		  (:cond ((sz :constant 1) '|.S32.F64|)
			 (t '|.S32.F32|)))
		 ((op1 :constant #b1100)
		  (:cond ((sz :constant 1) '|.U32.F64|)
			 (t '|.U32.F32|)))
		 ((op1 :constant #b1000)
		  (:cond ((sz :constant 1)
			  '|.F64|
			  (:cond ((op :constant 1) '|.S32|)
				 (t '|.U32|)))
			 (t
			  '|.F32|
			    (:cond ((op :constant 1) '|.S32|)
				   (t '|.U32|)))))
		 ((op1 :constant #b0111)
		  (:cond ((sz :constant 1)
			  '|.F32.F64|)
			 (t
			  '|.F64.F32|))))
	  :tab
	  dst ", " src))

(macrolet
    ((frob (ext op1 ops opc3 &key
			       (name 'vcvt)
			       (size-bit 0) 
			       (dst-type 'fp-single-reg)
			       (src-type 'fp-single-reg))
       `(define-vfp-2-inst ,name ,(symbolicate name "." ext)  #b11 ,op1 ,ops ,opc3 0
	  :size-bit ,size-bit
	  :dst-type ,dst-type
	  :src-type ,src-type
	  :printer vcvt-printer)))
  ;; Convert between double and single
  (frob "F64.F32" #b0111 #b1 #b1
	:dst-type fp-double-reg)
  (frob "F32.F64" #b0111 #b1 #b1
	:size-bit 1
	:src-type fp-double-reg)
  ;; A8.8.306
  ;; Convert between float and integer (truncating)
  (frob "S32.F32" #b1101 #b1 #b1)
  (frob "S32.F64" #b1101 #b1 #b1
	:src-type fp-double-reg
	:size-bit 1)
  (frob "U32.F32" #b1100 #b1 #b1)
  (frob ".U32.F64" #b1100 #b1 #b1
	:src-type fp-double-reg
	:size-bit 1)
  ;; Convert between float and integer (rounding)
  (frob "S32.F32" #b1101 #b0 #b1
	:name vcvtr)
  (frob "S32.F64" #b1101 #b0 #b1
	:name vcvtr
	:src-type fp-double-reg
	:size-bit 1)
  (frob "U32.F32" #b1100 #b0 #b1
	:name vcvtr)
  (frob "U32.F64" #b1100 #b0 #b1
	:name vcvtr
	:src-type fp-double-reg
	:size-bit 1)
  ;; Convert int to float
  (frob "F32.S32" #b1000 #b1 #b1)
  (frob "F32.U32" #b1000 #b0 #b1)
  (frob "F64.S32" #b1000 #b1 #b1
	:dst-type fp-double-reg
	:size-bit 1)
  (frob "F64.U32" #b1000 #b0 #b1
	:dst-type fp-double-reg
	:size-bit 1))

(defmacro define-vfp-cmp-inst (name inst-name ops opc3 &key doublep)
  (let ((rtype (if doublep 'fp-double-reg 'fp-single-reg))
	(size-bit (if doublep 1 0)))
    `(define-instruction ,inst-name (segment dst src &optional (cond :al))
       (:declare (type dst tn)
		 (type (or tn (float 0.0 0.0)) src)
		 (type condition-code cond))
       (:printer format-vfp-2-arg
		 ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0100) (op2 #b101)
		  (ops ,ops) (opc3 ,opc3) (opc4 0)
		  (sz ,size-bit)
		  (dst nil :type ',rtype)
		  (src nil :type ',rtype))
		 :default
	         :print-name ',name)
       (:printer format-vfp-2-arg
		 ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0101) (op2 #b101)
		  (ops ,ops) (opc3 ,opc3) (opc4 0)
		  (sz ,size-bit)
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
				      (condition-code-encoding cond)
				      #b111
				      #b01
				      d
				      #b11
				      #b0100
				      vd
				      #b101
				      ,size-bit
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
				    (condition-code-encoding cond)
				    #b111
				    #b01
				    d
				    #b11
				    #b0101
				    vd
				    #b101
				    ,size-bit
				    ,ops
				    ,opc3
				    0
				    0
				    0))))))))

(macrolet
    ((frob (name op)
       `(progn
	  (define-vfp-cmp-inst ,name ,(symbolicate name ".F32") ,op #b1)
	  (define-vfp-cmp-inst ,name ,(symbolicate name ".F64") ,op #b1 :doublep t))))
  (frob vcmp  #b0)
  (frob vcmpe #b1))


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
  (declare (list value) (stream stream)
	   (ignore dstate))
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

(defconstant format-vfp-vmov-immed-printer
  `(:name  cond
	   (:cond ((sz :constant 0) '|.F32|)
		  (t '|.F64|))
	   :tab
	   dst ", " imm8))


(disassem:define-instruction-format
    (format-vfp-vmov-immed 32
			   :include 'format-base
			   :default-printer format-vfp-vmov-immed-printer)
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
	   (:cond ((sz :constant 0) '|.F32|)
		  (t '|.F64|))
	   :tab
	   dst ", " src))

(disassem:define-instruction-format
    (format-vfp-vmov-reg 32
			 :include 'format-base
			 :default-printer format-vfp-vmov-reg-printer)
  (op0   :field (byte 2 23) :value #b01)
  (op    :field (byte 2 20) :value #b11)
  (z     :field (byte 4 16) :value 0)
  (dst   :fields (list (byte 1 22) (byte 4 12)) :type 'fp-single-reg)
  (src   :fields (list (byte 1 5) (byte 4 0)) :type 'fp-single-reg)
  (op2   :field (byte 3 9) :value #b101)
  (sz    :field (byte 1 8))
  (op3   :field (byte 2 6) :value #b01)
  (z2    :field (byte 1 4) :value 0))

(defmacro define-vmov-inst (inst-name doublep)
  (let ((reg-type (if doublep 'fp-double-reg 'fp-single-reg)))
    `(define-instruction ,inst-name (segment dst src &optional (cond :al))
       (:declare (type tn dst)
		 (type (or float tn) src)
		 (type condition-code cond))
       (:printer format-vfp-vmov-immed
		 ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (z 0)
		  (sz ,(if doublep 1 0))
		  (dst nil :type ',reg-type))
		 :default
		 :print-name 'vmov)
       (:printer format-vfp-vmov-reg
		 ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (op3 #b01)
		  (z 0) (z2 0)
		  (sz ,(if doublep 1 0))
		  (dst nil :type ',reg-type)
		  (src nil :type ',reg-type))
		 :default
		 :print-name 'vmov)
       (:emitter
	(etypecase src
	  (tn
	   (multiple-value-bind (d vd)
	       (fp-reg-tn-encoding dst doublep)
	     (multiple-value-bind (m vm)
		 (fp-reg-tn-encoding src doublep)
	       (emit-format-vfp-vmov-reg segment
					 (condition-code-encoding cond)
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
	       (fp-reg-tn-encoding dst doublep)
	     (let ((value (fp-immed-or-lose src)))
	       (emit-format-vfp-vmov-immed segment
					   (condition-code-encoding cond)
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

(define-vmov-inst vmov.f32 nil)
(define-vmov-inst vmov.f64 t)

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

(define-instruction vmov (segment dst src &optional (cond :al))
  (:declare (type tn dst src)
	    (type condition-code cond))
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
       (cond ((and (sc-is dst unsigned-reg signed-reg)
		   (sc-is src single-reg))
	      ;; Move to arm reg
	      (values 1 dst src))
	     ((and (sc-is src unsigned-reg signed-reg)
		   (sc-is dst single-reg))
	      ;; Move to float reg
	      (values 0 src dst))
	     (t
	      (error "VMOV requires one ARM reg and one single-float reg")))
     (multiple-value-bind (n vn)
	 (fp-reg-tn-encoding fp nil)
       (emit-format-7-vfp-vmov-core segment
				    (condition-code-encoding cond)
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
    (declare (type (unsigned-byte 8) value) (stream stream)
	     (ignore dstate))
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

(defmacro define-fp-load/store-inst (name inst-name op0 &optional doublep)
  `(define-instruction ,inst-name (segment dst src &optional (cond :al))
       (:declare (type tn dst)
		 (type (or tn load-store-index) dst)
		 (type condition-code cond))
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
					 (condition-code-encoding cond)
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
					   (condition-code-encoding cond)
					   #b110
					   #b1
					   (if add 1 0)
					   d
					   ,op0
					   (reg-tn-encoding base)
					   vd
					   #b101
					   ,(if doublep 1 0)
					   offset)))))))

(macrolet
    ((frob (name op)
       `(progn
	  (define-fp-load/store-inst ,name ,(symbolicate name ".32") ,op)
	  (define-fp-load/store-inst ,name ,(symbolicate name ".64") ,op t))))
  (frob vldr #b01)
  (frob vstr #b00))

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

(define-instruction vmrs (segment fpscr reg &optional (cond :al))
  (:declare (type (or tn (member 'apsr)) reg)
	    (type (member 'fpscr) fpscr)
	    (type condition-code cond))
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
	    '(:name cond :tab (:cond ((reg :constant 15) 'apsr)
				     (t reg))
	      ", " 'fpscr))
  (:emitter
   (let ((rt (if (typep reg 'tn)
		 (reg-tn-encoding reg)
		 15)))
     (emit-format-vfp-fpscr segment
			    (condition-code-encoding cond)
			    #b111
			    #b0
			    #b111
			    #b1
			    #b0001
			    rt
			    #b101
			    #b0
			    #b0
			    #b00
			    #b1
			    #b0000))))

(define-instruction vmsr (segment fpscr reg &optional (cond :al))
  (:declare (type tn reg)
	    (type (member 'fpscr) fpscr)
	    (type condition-code cond))
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
	    '(:name cond :tab 'fpscr ", " reg))
  (:emitter
   (emit-format-vfp-fpscr segment
			  (condition-code-encoding cond)
			  #b111
			  #b0
			  #b111
			  #b0
			  #b0001
			  (reg-tn-encoding reg)
			  #b101
			  #b0
			  #b0
			  #b00
			  #b1
			  #b0000)))