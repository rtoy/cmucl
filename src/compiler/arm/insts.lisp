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


;; See section A2.3 in the ARMv7-A architecture manual which says
;;
;;   When executing an ARM instruction, PC reads as the address of the
;;   current instruction plus 8.
(defconstant pc-read-offset 8)

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

;; ARM signed offsets in instructions. Offsets in instructions usually
;; have a magnitude value with an additional sign bit.  Basically,
;; signed magnitude.
(deftype arm-signed-offset (magnitude)
  `(integer ,(- magnitude) ,magnitude))

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

(defun print-immed (value stream)
  ;; Print immediate values in ARM syntax, honoring the output radix.
  (format stream "#~S" value))


;;;; Primitive emitters.

(define-emitter emit-word 32
  (byte 32 0))

(define-emitter emit-short 16
  (byte 16 0))


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
    (print-immed (rotate-right2 v rot) stream)))
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
  (declare (type (or shift-type (member :rrx)) shift-type))
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
		     tn
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
			      set-flags-bit
			      (reg-encoding src1)
			      (reg-encoding dst)
			      (logior (ash rot 8) val))))
      (tn
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
	  (emit-format-0-reg-shifted segment
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
			 tn
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
;; above data processing instructions because there is no dst
;; register.
(defmacro define-compare-inst (name opcode)
  `(define-instruction ,name (segment src1 src2 &optional (cond :al))
     (:declare (type tn src1)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 tn
			 flex-operand)
		     src2)
	       (type condition-code cond))
     (:printer format-1-immed
	       ((opb0 #b001) (op ,opcode)
		(s 1)
		(src1 nil :type 'reg)
		(dst nil))
	       format-1-immed-set-printer)
     (:printer format-0-reg
	       ((opb0 #b000) (op ,opcode) (rs 0)
		(s 1)
		(src1 nil :type 'reg)
		(dst nil))
	       format-0-reg-set-printer)
     (:printer format-0-reg-shifted
	       ((opb0 #b000) (op ,opcode) (rs 1)
		(z 0)
		(s 1)
		(src1 nil :type 'reg)
		(dst nil))
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
			 tn
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
			 tn
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
     (:printer format-0-reg
	       ((opb0 #b000)
		(op #b1101)
		(s ,set-flags-bit)
		(src1 0)
		(dst nil :type 'reg))
	       `(:name cond :tab
		       dst ", " src2))
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
	(tn
	 (emit-format-0-reg segment
			    (condition-code-encoding cond)
			    #b000
			    #b1101
			    ,set-flags-bit
			    0
			    (reg-tn-encoding dst)
			    0
			    (shift-type-encoding :lsl)
			    #b0
			    (reg-tn-encoding src2)))
	#+nil
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
			 tn)
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
     (:printer format-0-reg
	       ;; This is really lsl rd, rs, #0, but we want to print
	       ;; it as mov rd, rs, which is much easier to
	       ;; understand.
	       ((opb0 #b000)
		(op #b1101)
		(rs 0)
		(src1 0)
		(dst nil :type 'reg)
		(s ,set-flags-bit)
		(shift 0)
		(type 0))
	       '(:name cond :tab dst ", " src2)
	       :print-name 'mov)
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
	(tn
	 (emit-format-0-reg-shifted segment
				    (condition-code-encoding cond)
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
  (destructuring-bind (hi lo)
      value
    (print-immed (logior (ash hi 12) lo) stream))))

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
			 #b10100
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
			 (ldb (byte 4 12) src)
			 (reg-tn-encoding dst)
			 (ldb (byte 12 0) src)))
     (fixup
      (note-fixup segment :movw src)
      (emit-format-mov16 segment
			 (condition-code-encoding cond)
			 #b001
			 #b10000
			 0
			 (reg-tn-encoding dst)
			 0)))))

;; A5.2.5 Multiply and Accumulate

(define-emitter emit-format-0-mul 32
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
  `(:name cond :tab
	  dst ", [" src1
	  (:cond ((p :constant 1)
		  ;; P = 1 => pre-indexed
		  ", #"
		  (:unless (u :constant 1) "-")	;; U = 1 means add
		  immed
		  "]"
		  (:unless (w :constant 0) "!")) ;; W = 1 means write-back
		 (t
		  ;; P = 0 => post-indexed
		  "], #"
		  (:unless (u :constant 1) "-")
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
  (macrolet ((frob (printer)
	       `(let ((add/sub-reg
			;; Print "-" only if the register is to be
			;; subtracted.  If adding, nothing is needed,
			;; and we choose not to print the optional "+".
			'(:unless (u :constant 1) "-"))
		      (shift-reg-amount
			'(:unless (:and (type :constant 0) (imm5 :constant 0))
			  ;; Shift type LSL with 0 shift is the
			  ;; default identity operation so don't print
			  ;; anything. All other shift types and
			  ;; amounts are displayed, even if they would
			  ;; be an identity like ROR #0.
			  ", " type " #" imm5)))
		  ,printer)))
    (frob `(:name cond
	    :tab
	    dst
	    ", [" src1
	    (:cond ((p :constant 1)
		    ", "
		    ,add/sub-reg
		    rs
		    ,shift-reg-amount
		    "]"
		    (:unless (w :constant 0) "!"))
		   (t
		    "], "
		    ,add/sub-reg
		    rs
		    ,shift-reg-amount))))))

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
  (print-immed (logior (ash (first value) 4) (second value)) stream)))

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

;; Indexing mode for load/store instructions.  See the comments for
;; load/store-inst for usage.
(defstruct indexing-mode
  ;; The base register
  reg
  ;; Indicates pre or post indexing if Nil or non-Nil, respectively.
  post-index)

(defun pre-index (reg)
  (make-indexing-mode :reg reg :post-index nil))

(defun post-index (reg)
  (make-indexing-mode :reg reg :post-index t))

;; Structure to hold the hairy indexing information for load/store-inst.  
(defstruct load-store-index
  ;; The register that is to be shifted.
  offset
  ;; The shift type (:lsr, :lsl, etc.)
  shift-type
  ;; The shift amount
  shift-amount
  ;; Boolean to indicate whether to add or subtract the register.
  ;; Default is non-Nil meaning to add
  add)

;; Encode the information for the hairy modes of the load/store-inst.
;; This is used to handle the case where an register is applied to the
;; base register.  The register can be either subtracted or added and
;; can optionally be shifted by some amount.
(defun make-op2 (reg &key (add t) (shift-type :lsl) (count 0))
  (declare (type tn reg)
	   (type boolean add)
	   (type (unsigned-byte 5) count)
	   (type shift-type shift-type))
  (make-load-store-index :offset reg
			 :add add
			 :shift-type shift-type
			 :shift-amount count))

;; Determine the P, U, and W bits from an immediate load/store
;; instruction. src2 must be an integer type.
(defun decode-immediate-indexing-mode (src1 src2)
  ;; The simple load/store immediate case:
  ;; (inst ldr rd r1 100) ->
  ;;   ldr rd, [r1, #100]
  ;; (inst ldr rd (pre-index r1) 100) ->
  ;;   ldr rd, [r1, 100]!
  ;; (inst ldr rd (post-index r1) -100) ->
  ;;   ldr rd, [r1], -100
  ;;
  ;; If src1 is an indexed obect, then we always write back (W=1).
  ;; The P bit is set for pre-indexing, which is always true except
  ;; for the post-indexing mode. U is set if src2 is non-negative. W
  ;; (write-back) is set if src1 is an index, that is src1 is not a
  ;; TN.
  (declare (integer src2))
  (values (if (and (indexing-mode-p src1)
		   (indexing-mode-post-index src1))
	      0 1)
	  (if (minusp src2)
	      0 1)
	  (if (tn-p src1)
	      0 1)))

(defun decode-indexing-and-options (src1 op2)
  "Determine the P, U, and W bits from the load-store-index"
  ;; This handles the hairy register load/store indexing where op2
  ;; indicates index register and whether it is added or subtracted
  ;; with an optional shift amount and whether the base register is
  ;; subsequently updated.
  ;;
  ;; Examples:
  ;; ldr rd, [r1, r2, lsr 2] ->
  ;;   (inst ldr rd r1 (make-op2 r2 :shift :lsr :count 2))
  ;; ldr rd, [r1, -r2] ->
  ;;   (inst ldr rd r1 (make-op2 r2 :add nil))
  ;; ldr rd, [r1, -r2, lsl 3] ->
  ;;   (inst ldr rd r1 (make-op2 r2 :add nil :shift :lsl :count 3))
  ;; ldr rd, [r1, 100]! ->
  ;;   (inst ldr rd (pre-index r1) 100)
  ;; ldr rd, [r1, -r2, lsl 3]! ->
  ;;   (inst ldr rd (pre-index r1) (make-op2 r2 :add nil :shift :lsl :count 3))
  ;; ldr rd, [r1], -100 ->
  ;;   (inst ldr rd (post-index r1) -100)
  ;; ldr rd, [r1], r2 ->
  ;;   (inst ldr rd (post-index r1) r2)
  ;; ldr rd, [r1], -r2, lsl 2 ->
  ;;   (inst ldr rd (post-index r1) (make-op2 r2 :add nil :shift :lsl :count 2))

  (values (if (and (indexing-mode-p src1)
		   (indexing-mode-post-index src1))
	      0 1)
	  (if (load-store-index-add op2)
	      1 0)
	  (if (indexing-mode-p src1)
	      1 0)))

;; A5.3 and table A5-15
;; Load/store for words and unsigned bytes
;;
;; Here is the basic syntax for the load/store instructions:
;;
;; (inst ldr dst src1 src2 &optional cond)
;;
;; src1 is either a tn or an index-type which is used to indicate
;; whether we are using pre- or post-indexing address modes, indicated
;; by (pre-index tn) or (post-index tn), respectively.
;;
;; src2 can be a tn, an immediate, or an op2.  An op2 is a structure
;; that encodes the hairy options like whether src2 is added or
;; subtracted, and whether shift is applied.
;;
;; Some examples:
;;
;; ldr rd, [r1, 100] ->
;;   (inst ldr rd r1 100)
;; ldr rd, [r1, -100] ->
;;   (inst ldr rd r1 -100)
;; ldr rd, [r1, r2] ->
;;   (inst ldr rd r1 r2)
;; ldr rd, [r1, r2, lsr 2] ->
;;   (inst ldr rd r1 (make-op2 r2 :shift :lsr :count 2))
;; ldr rd, [r1, -r2] ->
;;   (inst ldr rd r1 (make-op2 r2 :add nil))
;; ldr rd, [r1, -r2, lsl 3] ->
;;   (inst ldr rd r1 (make-op2 r2 :add nil :shift :lsl :count 3))
;; ldr rd, [r1, 100]! ->
;;   (inst ldr rd (pre-index r1) 100)
;; ldr rd, [r1, -r2, lsl 3]! ->
;;   (inst ldr rd (pre-index r1) (make-op2 r2 :add nil :shift :lsl :count 3))
;; ldr rd, [r1], -100 ->
;;   (inst ldr rd (post-index r1) -100)
;; ldr rd, [r1], r2 ->
;;   (inst ldr rd (post-index r1) r2)
;; ldr rd, [r1], -r2, lsl 2 ->
;;   (inst ldr rd (post-index r1) (make-op2 r2 :add nil :shift :lsl :count 2))
;;
;; ldrge rd, [r1], -r2 ->
;;   (inst ldr rd (post-index r1) (make-op2 r2 :add nil) :ge)
(defmacro define-load/store-inst (name loadp &optional bytep)
  `(define-instruction ,name (segment reg src1 src2 &optional (cond :al))
     (:declare (type tn reg)
	       (type (or tn indexing-mode) src1)
	       (type (or tn (arm-signed-offset 4095) load-store-index) src2)
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
      (etypecase src2
	(integer
	 (multiple-value-bind (p u w)
	     (decode-immediate-indexing-mode src1 src2)
	   (assert (typep src2 '(arm-signed-offset 4095)))
	   (emit-format-2-immed segment
				(condition-code-encoding cond)
				#b010
				p
				u
				0
				w
				,(if loadp 1 0)
				(reg-tn-encoding (if (indexing-mode-p src1)
						     (indexing-mode-reg src1)
						     src1))
				(reg-tn-encoding reg)
				(abs src2))))
	(tn
	 ;; Simple case where we have just an offset register
	 (emit-format-3-reg segment
			    (condition-code-encoding cond)
			    #b011
			    1		; P: indexed
			    1		; U: add
			    ,(if bytep 1 0)
			    0		; W: no writeback (update)
			    ,(if loadp 1 0)
			    (reg-tn-encoding src1)
			    (reg-tn-encoding reg)
			    0
			    0
			    0
			    (reg-tn-encoding src2)))
	(load-store-index
	 ;; Handle the complicated cases with an offset register, with
	 ;; a possible hairy shift operation.
	 (multiple-value-bind (p u w)
	     (decode-indexing-and-options src1 src2)
	   (emit-format-3-reg segment
			      (condition-code-encoding cond)
			      #b011
			      p
			      u
			      ,(if bytep 1 0)
			      w
			      ,(if loadp 1 0)
			      (reg-tn-encoding (if (indexing-mode-p src1)
						   (indexing-mode-reg src1)
						   src1))
			      (reg-tn-encoding reg)
			      (load-store-index-shift-amount src2)
			      (load-store-index-shift-type src2)
			      0
			      (reg-tn-encoding (load-store-index-offset src2)))))))))

(define-load/store-inst ldr t)
(define-load/store-inst ldrb t t)
(define-load/store-inst str nil)
(define-load/store-inst strb nil t)

;; A5.2.8; extra load/store instructions for halfwords (16-bit signed
;; and unsigned) and signed bytes.
;;
;; ldrh rt, [rn, 0]      -> (inst ldrh rt rn 0)
;; ldrh rt, [rn, #off]!  -> (inst ldrh rt (pre-index rn) off)
;; ldrh rt, [rn, rm]     -> (inst ldrh rt rn rm)
;; ldrh rt, [rn, -rm]    -> (inst ldrh rt rn (make-op2 rm :add nil))
;; ldrh rt, [rn, rm]!    -> (inst ldrh rt (pre-index rn) rm)
;; ldrh rt, [rn, -rm]!   -> (inst ldrh rt (pre-index rn) (make-op2 rm :add nil))
;; ldrh rt, [rn], +rm    -> (inst ldrh rt (post-index rn) (make-op2 rm :add t))
;; ldrh rt, [rn], -rm    -> (inst ldrh rt (post-index rn) (make-op2 rm :add nil))
;;
;; There are other possible forms for ldrh, but we only support ARM A1
;; encoding, so the ones listed above are all that are supported.
(defmacro define-load/store-extra-inst (name &optional loadp bytep signedp)
  `(define-instruction ,name (segment reg src1 src2 &optional (cond :al))
     (:declare (type tn reg)
	       (type (or tn indexing-mode) src1)
	       (type (or tn (arm-signed-offset 255) load-store-index) src2)
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
	  (etypecase src2
	    (load-store-index
	     (multiple-value-bind (p u w)
		 (decode-indexing-and-options src1 src2)
	       ;; If src2 is an indexing-mode, ensure the default
	       ;; shift type (LSL) and amount (0) are given.  Anything
	       ;; else is invalid. (And strictly speaking so is LSL,
	       ;; but make-op2 defaults to LSL and does not support
	       ;; specifying NIL for :shift.  Fix this?
	       (when (load-store-index-shift-type src2)
		 (assert (eq (load-store-index-shift-type src2) :lsl))
		 (assert (zerop (load-store-index-shift-amount src2))))
	       (emit-format-0-halfword-reg segment
					   (condition-code-encoding cond)
					   #b000
					   p
					   u
					   0
					   w
					   ,(if loadp 1 0)
					   (reg-tn-encoding (if (indexing-mode-p src1)
								(indexing-mode-reg src1)
								src1))
					   (reg-tn-encoding reg)
					   0
					   1
					   sign
					   op2
					   (reg-tn-encoding (load-store-index-offset src2)))))
	    (tn
	     (multiple-value-bind (p u w)
		 (decode-indexing-and-options src1 src2)
	       (declare (ignore u))
	       (emit-format-0-halfword-reg segment
					   (condition-code-encoding cond)
					   #b000
					   p
					   1
					   0
					   w
					   ,(if loadp 1 0)
					   (reg-tn-encoding (if (indexing-mode-p src1)
								(indexing-mode-reg src1)
								src1))
					   (reg-tn-encoding reg)
					   0
					   1
					   sign
					   op2
					   (reg-tn-encoding src2))))
	    (integer
	     (multiple-value-bind (p u w)
		 (decode-immediate-indexing-mode src1 src2)
	       (let ((imm8 (abs src2)))
		 (emit-format-0-halfword-imm segment
					     (condition-code-encoding cond)
					     #b000
					     p
					     u
					     1
					     w
					     ,(if loadp 1 0)
					     (reg-tn-encoding (if (indexing-mode-p src1)
								  (indexing-mode-reg src1)
								  src1))
					     (reg-tn-encoding reg)
					     (ldb (byte 4 4) imm8)
					     1
					     sign
					     op2
					     (ldb (byte 4 0) imm8))))))))))

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
		 (+ (ash value 2) (disassem:dstate-cur-addr dstate) pc-read-offset)))

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
			  (ash (- (label-position target) posn pc-read-offset) -2)))))

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
  (:declare (type (or label tn) target))
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
     (tn
      (emit-branch-reg segment
		       (condition-code-encoding :al)
		       #b000
		       #b10010
		       #b111111111111
		       #b0011
		       (reg-tn-encoding target))))))

(define-instruction bx (segment target)
  (:declare (type tn target))
  (:printer branch-reg
	    ((opb0 #b000)
	     (op #b10010)
	     (op0 nil)
	     (op1 #b0001)))
  (:attributes branch)
  (:emitter
   (emit-branch-reg segment
		    (condition-code-encoding :al)
		    #b000
		    #b10010
		    #b111111111111
		    #b0001
		    (reg-tn-encoding target))))


;; Miscellaneous instructions

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun udf-imm-printer (value stream dstate)
  (declare (list value) (stream stream)
	   (ignore dstate))
  (destructuring-bind (hi lo)
      value
    (print-immed (logior (ash hi 4) lo)
		 stream))))

(define-emitter emit-format-udf 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 12 8) (byte 4 4) (byte 4 0))

(disassem:define-instruction-format
    (format-udf 32 :include 'format-base
		:default-printer '(:name :tab imm))
  (op0 :field (byte 5 20) :value #b11111)
  (imm :fields (list (byte 12 8) (byte 4 0)) :printer #'udf-imm-printer)
  (op1 :field (byte 4 4) :value #b1111))

;; Try to extract the name from the bytes following the UDF
;; instruction which contain the name of the unimplemented item.
;;
;; It would be really nice if we could print out those bytes as just
;; words instead of having the disassembler try to disassemble random
;; words.
(defun snarf-not-implemented-name (stream dstate)
  (let* ((sap (disassem:dstate-segment-sap dstate))
	 (offset (disassem:dstate-next-offs dstate))
	 (branch-inst (sys:sap-ref-32 sap offset)))
    ;; sap + offset should point to the branch instruction after the
    ;; udf instruction.  Make sure it's an unconditional branch
    ;; instrution.
    (unless (= (ldb (byte 8 24) branch-inst) #xea)
      (return-from snarf-not-implemented-name ""))
    ;; From the offset in the branch instruction, compute the max
    ;; length of the string that was encoded.
    (let ((max-length (+ (ash (ldb (byte 24 0) branch-inst) 2) 4)))
      ;; Skip the branch instruction
      (incf offset 4)
      ;; Print each following byte until max-length is reached or we
      ;; get a 0 byte.
      (with-output-to-string (s)
	(do* ((k 0 (+ k 1))
	      (octet (sys:sap-ref-8 sap (+ offset k))
		     (sys:sap-ref-8 sap (+ offset k))))
	     ((or (>= k max-length)
		  (zerop octet)))
	  (write-char (code-char octet) s))))))

(defun udf-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x)
	   (when stream
	     (disassem:note x dstate))))
    (destructuring-bind (hi lo)
	(format-udf-imm chunk dstate)
      (let ((udf-value (logior (ash hi 4) lo)))
	(case udf-value
	  (#.vm::not-implemented-trap
	   (nt (concatenate 'string
			    "Not-implemented trap: "
			    (snarf-not-implemented-name stream dstate)))))))))

;; A8.8.247
;; Undefined instruction
(define-instruction udf (segment imm)
  (:declare (type (unsigned-byte 16) imm))
  (:printer format-udf
	    ((cond #b1110)
	     (opb0 #b011)
	     (op0 #b11111)
	     (op1 #b1111))
	    :default
	    :control #'udf-control)
  (:emitter
   (emit-format-udf segment
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
   (emit-branch-imm segment
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

(define-emitter emit-format-0-msr 32
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
	      (inst movw reg lo)
	      (unless (zerop hi)
		(inst movt reg hi))))))
    (fixup
     (inst movw reg value)
     (inst movt reg value))))

(define-instruction-macro li (reg value)
  `(%li ,reg, value))

(define-instruction-macro neg (dst src)
  `(inst rsb ,dst ,src 0))
  

;; Floating-point instructions
;; A7.5

(define-emitter emit-format-vfp-3-arg 32
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

(defmacro define-vfp-3-inst (name op0 op1 opa0)
  `(define-instruction ,name (segment dst src1 src2 &optional (cond :al))
     (:declare (type tn dst src1 src2)
	       (type condition-code cond))
     (:printer format-vfp-3
	       ((opb0 #b111)
		(op0 ,op0) (op1 ,op1) (op2 #b101)
		(opa0 ,opa0) (opa1 0)
		(sz 0)))
     (:printer format-vfp-3
	       ((opb0 #b111)
		(op0 ,op0) (op1 ,op1) (op2 #b101)
		(opa0 ,opa0) (opa1 0)
		(sz 1)
		(dst nil :type 'fp-double-reg)
		(src1 nil :type 'fp-double-reg)
		(src2 nil :type 'fp-double-reg)))
     (:emitter
      ;; All three register types must be the same type---either
      ;; single-reg or double-reg.
      (assert (or (and (sc-is dst single-reg)
		       (sc-is src1 single-reg)
		       (sc-is src2 single-reg))
		  (and (sc-is dst double-reg)
		       (sc-is src1 double-reg)
		       (sc-is src2 double-reg))))
      (let ((doublep (sc-is dst double-reg)))
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
				     (if doublep 1 0)
				     n
				     ,opa0
				     m
				     0
				     vm))))))))

(define-vfp-3-inst vadd #b00 #b11 0)
(define-vfp-3-inst vsub #b00 #b11 1)
(define-vfp-3-inst vmul #b00 #b10 0)
(define-vfp-3-inst vdiv #b01 #b00 0)

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

(defmacro define-vfp-2-inst (name op op1 ops opc3)
  `(define-instruction ,name (segment dst src &optional (cond :al))
     (:declare (type tn dst src)
	       (type condition-code cond))
     (:printer format-vfp-2-arg
	       ((opb0 #b111)
		(op0 #b01) (op ,op) (op1 ,op1) (op2 #b101)
		(ops ,ops) (opc3 ,opc3) (opc4 0)
		(sz 0)
		(src nil :type 'fp-single-reg)))
     (:printer format-vfp-2-arg
	       ((opb0 #b111)
		(op0 #b01) (op ,op) (op1 ,op1) (op2 #b101)
		(ops ,ops) (opc3 ,opc3) (opc4 0)
		(sz 1)
		(dst nil :type 'fp-double-reg)
		(src nil :type 'fp-double-reg)))
     (:emitter
      ;; dst and src regs must be the same type of float register.
      (assert (or (and (sc-is dst single-reg)
		       (sc-is src single-reg))
		  (and (sc-is dst double-reg)
		       (sc-is src double-reg))))
      (let ((doublep (sc-is dst double-reg)))
	(multiple-value-bind (d vd)
	    (fp-reg-tn-encoding dst doublep)
	  (multiple-value-bind (m vm)
	      (fp-reg-tn-encoding src doublep)
	    (emit-format-vfp-2-arg segment
				   (condition-code-encoding cond)
				   #b111
				   #b01
				   d
				   ,op
				   ,op1
				   vd
				   #b101
				   (if doublep 1 0)
				   #b1
				   ,opc3
				   m
				   0
				   vm)))))))

(define-vfp-2-inst vabs  #b11 #b0000 #b1 #b1)
(define-vfp-2-inst vneg  #b11 #b0001 #b0 #b1)
(define-vfp-2-inst vsqrt #b11 #b0001 #b1 #b1)

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

(defmacro define-vcvt-inst (name inst-name op op1 ops opc3 opc4
			    &key (size-bit 0)
			      (dst-type 'fp-single-reg)
			      (src-type 'fp-single-reg))
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
	       vcvt-printer
	       :print-name ',name)
     (:emitter
      (multiple-value-bind (d vd)
	  (fp-reg-tn-encoding dst (eq ',dst-type 'fp-double-reg))
	(multiple-value-bind (m vm)
	    (fp-reg-tn-encoding src (eq ',src-type 'fp-double-reg))
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
				 vm))))))

(macrolet
    ((frob (ext op1 ops opc3 &key
			       (name 'vcvt)
			       (size-bit 0) 
			       (dst-type 'fp-single-reg)
			       (src-type 'fp-single-reg))
       `(define-vcvt-inst ,name ,(symbolicate name "." ext)  #b11 ,op1 ,ops ,opc3 0
	  :size-bit ,size-bit
	  :dst-type ,dst-type
	  :src-type ,src-type)))
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

(defmacro define-vfp-cmp-inst (name ops)
  `(define-instruction ,name (segment dst src &optional (cond :al))
     (:declare (type tn dst)
	       (type (or tn (member 0f0 0d0)) src)
	       (type condition-code cond))
     ;; Compare two single-regs
     (:printer format-vfp-2-arg
	       ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0100) (op2 #b101)
		(ops ,ops) (opc3 1) (opc4 0)
		(sz 0)))
     ;; Compare two double-regs
     (:printer format-vfp-2-arg
	       ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0100) (op2 #b101)
		(ops ,ops) (opc3 1) (opc4 0)
		(sz 1)
		(dst nil :type 'fp-double-reg)
		(src nil :type 'fp-double-reg)))
     ;; Compare single-reg with 0
     (:printer format-vfp-2-arg
	       ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0101) (op2 #b101)
		(ops ,ops) (opc3 1) (opc4 0)
		(sz 0)
		(src (list 0 0)))
	       '(:name cond '|.F32|
		 :tab
		 dst ", #0.0"))
     ;; Compare double-reg with 0
     (:printer format-vfp-2-arg
	       ((opb0 #b111) (op0 #b01) (op #b11) (op1 #b0101) (op2 #b101)
		(ops ,ops) (opc3 1) (opc4 0)
		(sz 1)
		(dst nil :type 'fp-double-reg)
		(src (list 0 0)))
	       '(:name cond '|.F64|
		 :tab
		 dst ", #0.0"))
     (:emitter
      (let* ((doublep (sc-is dst double-reg))
	     (size-bit (if doublep 1 0)))
	(etypecase src
	  (tn
	   (assert (or (and (sc-is dst double-reg)
			    (sc-is src double-reg))
		       (and (sc-is dst single-reg)
			    (sc-is src single-reg))))
	   (multiple-value-bind (d vd)
	       (fp-reg-tn-encoding dst doublep)
	     (multiple-value-bind (m vm)
		 (fp-reg-tn-encoding src doublep)
	       (emit-format-vfp-2-arg segment
				      (condition-code-encoding cond)
				      #b111
				      #b01
				      d
				      #b11
				      #b0100
				      vd
				      #b101
				      size-bit
				      ,ops
				      1
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
				    size-bit
				    ,ops
				    1
				    0
				    0
				    0))))))))

(define-vfp-cmp-inst vcmp  #b0)
(define-vfp-cmp-inst vcmpe #b1)

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
  `(:name cond
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

;; A8.8.345 VMOV between 2 ARM regs and a double-precision register

(define-emitter emit-format-6-vfp-vmov-core-double 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 4 8)
  (byte 2 6) (byte 1 5) (byte 1 4) (byte 4 0))

(disassem:define-instruction-format
    (format-6-vfp-vmov-core-double 32 :include 'format-base)
  (op1  :field (byte 4 21) :value #b0010)
  (op   :field (byte 1 20))
  (rt2  :field (byte 4 16) :type 'reg)
  (rt   :field (byte 4 12) :type 'reg)
  (op2  :field (byte 4 8) :value #b1011)
  (op3  :field (byte 2 6) :value 0)
  (vm   :fields (list (byte 1 5) (byte 4 0)) :type 'fp-double-reg)
  (op4  :field (byte 1 4) :value 0))

;; Handle various types of vmov instructions.  However, we don't
;; currently support moving 2 ARM registers to or from two
;; single-precision registers.
(define-instruction vmov (segment dst dst-or-src &optional src-or-cond cond)
  (:declare (type tn dst)
	    (type (or real tn) dst-or-src)
	    (type (or null tn condition-code) src-or-cond)
	    (type (or null condition-code) cond))
  ;; vmov float, immed
  (:printer format-vfp-vmov-immed
	    ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (z 0)
	     (sz 0)
	     (dst nil :type 'fp-single-reg)))
  (:printer format-vfp-vmov-immed
	    ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (z 0)
	     (sz 1)
	     (dst nil :type 'fp-double-reg)))
  ;; vmov float, float
  (:printer format-vfp-vmov-reg
	    ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (op3 #b01)
	     (z 0) (z2 0)
	     (sz 0)
	     (dst nil :type 'fp-single-reg)
	     (src nil :type 'fp-single-reg)))
  (:printer format-vfp-vmov-reg
	    ((opb0 #b111) (op0 #b01) (op #b11) (op2 #b101) (op3 #b01)
	     (z 0) (z2 0)
	     (sz 1)
	     (dst nil :type 'fp-double-reg)
	     (src nil :type 'fp-double-reg)))
  ;; vmov reg, single
  (:printer format-7-vfp-vmov-core
	    ((opb0 #b111)
	     (op0 #b0000)
	     (op1 #b1010)
	     (op2 #b0010000)
	     (op 1))
	    '(:name cond :tab reg ", " vn))
  ;; vmov single, reg
  (:printer format-7-vfp-vmov-core
	    ((opb0 #b111)
	     (op0 #b0000)
	     (op1 #b1010)
	     (op2 #b0010000)
	     (op 0))
	    '(:name cond :tab vn ", " reg))
  ;; vmov d, rt, rt2
  (:printer format-6-vfp-vmov-core-double
	    ((opb0 #b110)
	     (op1 #b0010)
	     (op 0)
	     (op2 #b1011)
	     (op3 0)
	     (op4 0))
	    '(:name cond :tab vm ", " rt ", " rt2))
  ;; vmov rt, rt2, d
  (:printer format-6-vfp-vmov-core-double
	    ((opb0 #b110)
	     (op1 #b0010)
	     (op 1)
	     (op2 #b1011)
	     (op3 0)
	     (op4 0))
	    '(:name cond :tab rt ", " rt2 ", " vm))
  (:emitter
   ;; Look through the arg types to figure out what kind of vmov
   ;; instruction we have.
   (typecase src-or-cond
     (tn
      ;; If SRC-OR-COND is a TN, then at least 3 args were given so
      ;; this must be a move of 2 ARM registers to/from a double reg.
      (let ((cc (or cond :al)))
	(sc-case dst
	  (double-reg
	   ;; Moving 2 ARM regs to a double.  Make sure the two src
	   ;; regs are ARM registers
	   (assert (and (sc-is dst-or-src signed-reg unsigned-reg)
			(sc-is src-or-cond  signed-reg unsigned-reg)))
	   (multiple-value-bind (m vm)
	       (fp-reg-tn-encoding dst t)
	     (emit-format-6-vfp-vmov-core-double segment
						 (condition-code-encoding cc)
						 #b110
						 #b0010
						 0
						 (reg-tn-encoding dst-or-src)
						 (reg-tn-encoding src-or-cond)
						 #b1011
						 0
						 m
						 0
						 vm)))
	  ((signed-reg unsigned-reg)
	   ;; Moving double to 2 ARM regs.  Make sure the other args
	   ;; are valid.
	   (assert (and (sc-is dst-or-src signed-reg unsigned-reg)
			(sc-is src-or-cond double-reg)))
	   ;; The two destination regs must be different.
	   (assert (/= (tn-offset dst) (tn-offset dst-or-src)))
	   (multiple-value-bind (m vm)
	       (fp-reg-tn-encoding src-or-cond t)
	     (emit-format-6-vfp-vmov-core-double segment
						 (condition-code-encoding cc)
						 #b110
						 #b0010
						 1
						 (reg-tn-encoding dst-or-src)
						 (reg-tn-encoding dst)
						 #b1011
						 0
						 m
						 0
						 vm))))))
     (otherwise
      ;; SRC-OR-COND is not a TN, so we have the two arg case.
      (let ((cc (or src-or-cond :al)))
	(cond ((and (sc-is dst signed-reg unsigned-reg)
		    (sc-is dst-or-src single-reg))
	       ;; Move to ARM reg from single float reg
	       (multiple-value-bind (n vn)
		   (fp-reg-tn-encoding dst-or-src nil)
		 (emit-format-7-vfp-vmov-core segment
					      (condition-code-encoding cc)
					      #b111
					      #b0000
					      1
					      vn
					      (reg-tn-encoding dst)
					      #b1010
					      n
					      #b0010000)))
	      ((and (sc-is dst single-reg)
		    (sc-is dst-or-src signed-reg unsigned-reg))
	       ;; Move to float reg from ARM reg
	       (multiple-value-bind (n vn)
		   (fp-reg-tn-encoding dst nil)
		 (emit-format-7-vfp-vmov-core segment
					      (condition-code-encoding cc)
					      #b111
					      #b0000
					      0
					      vn
					      (reg-tn-encoding dst-or-src)
					      #b1010
					      n
					      #b0010000)))
	      ((and (sc-is dst single-reg double-reg)
		    (sc-is dst-or-src single-reg double-reg))
	       ;; Moving between float regs.  They have to be the same type.
	       (assert (or (and (sc-is dst single-reg)
				(sc-is dst-or-src single-reg))
			   (and (sc-is dst double-reg)
				(sc-is dst-or-src double-reg))))
	       (let ((doublep (sc-is dst double-reg)))
		 (multiple-value-bind (d vd)
		     (fp-reg-tn-encoding dst doublep)
		   (multiple-value-bind (m vm)
		       (fp-reg-tn-encoding dst-or-src doublep)
		     (emit-format-vfp-vmov-reg segment
					       (condition-code-encoding cc)
					       #b111
					       #b01
					       d
					       #b11
					       #b0000
					       vd
					       #b101
					       (if doublep 1 0)
					       #b01
					       m
					       0
					       vm)))))
	      ((and (sc-is dst single-reg double-reg)
		    (realp dst-or-src))
	       ;; Move immediate value to float.
	       (let ((doublep (sc-is dst double-reg)))
		 (multiple-value-bind (d vd)
		     (fp-reg-tn-encoding dst doublep)
		   (let ((f (fp-immed-or-lose dst-or-src)))
		     (emit-format-vfp-vmov-immed segment
						 (condition-code-encoding cond)
						 #b111
						 #b01
						 d
						 #b11
						 (ldb (byte 4 4) f)
						 vd
						 #b101
						 (if doublep 1 0)
						 0
						 (ldb (byte 4 0) f))))))
	      (t
	       (error "Unknown or unsupported argument types for VMOV"))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun imm8-offset-printer (value stream dstate)
    (declare (type (unsigned-byte 8) value) (stream stream)
	     (ignore dstate))
    (print-immed (ash value 2) stream)))

(defconstant format-6-vfp-load/store-printer
  `(:name cond
	  (:cond ((sz :constant 0) '|.32|)
		 (t '|.64|))
	  :tab dst
	  ", [" src
	  (:cond ((imm8 :constant 0) "]")
		 ((u :constant 1)
		  ", #+" imm8 "]")
		 (t
		  ", #-" imm8 "]"))))

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

;; vldr rd, [rn] -> (inst vldr rd rn 0)
;; vldrge rd, [rn, -8] -> (inst vldr rd rn -8 :ge)
(defmacro define-fp-load/store-inst (name op0)
  `(define-instruction ,name (segment dst src offset &optional (cond :al))
     (:declare (type tn dst)
	       (type tn src)
	       ;; This is small lie.  The range is correct, but the
	       ;; offset must be a multiple of 4.
	       (type (arm-signed-offset 1020) offset)
	       (type condition-code cond))
     (:printer format-6-vfp-load/store
	       ((opb0 #b110)
		(one 1)
		(op0 ,op0)
		(op1 #b101)
		(sz 0)
		(dst nil :type 'fp-single-reg)))
     (:printer format-6-vfp-load/store
	       ((opb0 #b110)
		(one 1)
		(op0 ,op0)
		(op1 #b101)
		(sz 1)
		(dst nil :type 'fp-double-reg)))
     (:emitter
      (assert (sc-is dst single-reg double-reg))
      (let ((doublep (sc-is dst double-reg)))
	(multiple-value-bind (d vd)
	    (fp-reg-tn-encoding dst doublep)
	  ;; The offset must be divisible by 4!
	  (assert (zerop (ldb (byte 2 0) (abs offset))))
	  (emit-format-6-vfp-load/store segment
					(condition-code-encoding cond)
					#b110
					#b1
					(if (minusp offset) 0 1)
					d
					,op0
					(reg-tn-encoding src)
					vd
					#b101
					(if doublep 1 0)
					(ash (abs offset) -2)))))))

(define-fp-load/store-inst vldr #b01)
(define-fp-load/store-inst vstr #b00)

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

(defmacro not-implemented (&optional name)
  (let ((string (string name)))
    `(let ((length-label (gen-label)))
       (inst udf not-implemented-trap)
       ;; NOTE: The branch offset helps estimate the length of the
       ;; string.  The actual length of the string may be equal to the
       ;; displacement or it may be up to three bytes shorter at the
       ;; first trailing NUL byte.  The string may or may not be
       ;; 0-terminated.
       (inst b length-label)
       ,@(map 'list #'(lambda (c)
			`(inst byte ,(char-code c)))
	      string)
       ;; Append enough zeros to end on a word boundary.
       ,@(make-list (mod (- (length string)) 4)
		    :initial-element '(inst byte 0))
       (emit-label length-label))))

;;;; Instructions for dumping data and header objects.

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:delay 0)
  (:emitter
   (emit-word segment word)))

(define-instruction short (segment short)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) short))
  :pinned
  (:delay 0)
  (:emitter
   (emit-short segment short)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))

(define-emitter emit-header-object 32
  (byte 24 8) (byte 8 0))
  
(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
		  (logior type
			  (ash (+ posn (component-header-length))
			       (- type-bits word-shift)))))))

(define-instruction function-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment function-header-type)))

(define-instruction lra-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-header-type)))


;;;; Instructions for converting between code objects, functions, and lras.
(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We (currently) emit 20 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (declare (ignore segment posn delta-if-after))
       ;; Just use the worse case for now.
       nil)
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
	 (assemble (segment vop)
	   (inst movw temp (ldb (byte 16 0) delta))
	   (inst movt temp (ldb (byte 16 16) delta))
	   (inst add dst src temp))))))

;; code = fn - fn-ptr-type - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     function-pointer-type
			     (label-position label posn delta-if-after)
			     (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length)))))))


;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (+ (label-position label posn delta-if-after)
			     (component-header-length))))))

;; reg-list is the register list immediate value in the LDM/STM
;; instructions.  The disassembler will disassemble them as a list of
;; registers.  We don't don't support coalescing a consecutive
;; sequence to the form r<first>-r<lst>. Instead each register is
;; printed.
(disassem:define-argument-type reg-list
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate))
	       (princ '{ stream)
	       (dotimes (k 16)
		 (unless (zerop (ldb (byte 1 k) value))
		   (reg-arg-printer k stream dstate)
		   (setf (ldb (byte 1 k) value) 0)
		   (unless (zerop value)
		     (princ ", " stream))))
	       (princ '} stream)))

(defconstant format-4-printer
  `(:name cond
	  :tab
	  dst
	  (:unless (w :constant 0)
		    '!)
	  ", "
	  rlist))
	  
(define-emitter emit-format-4 32
  (byte 4 28) (byte 3 25)
  (byte 3 22) (byte 1 21) (byte 1 20) (byte 4 16) (byte 16 0))

(disassem:define-instruction-format
    (format-4 32 :include 'format-base
		 :default-printer format-4-printer)
  (op4 :field (byte 3 22))
  (w :field (byte 1 21))
  (ld :field (byte 1 20))
  (dst :field (byte 4 16) :type 'reg)
  (rlist :field (byte 16 0) :type 'reg-list))

;; LDM/STM
;;
;; ldm sp, {r0, r1} -> (inst ldm sp (list r0 r1))
;; ldmeq sp, {r1, r7} -> (inst ldm sp (list r1 r7) :eq)
;; ldm sp!, {r0, r1} -> (inst ldm (pre-index sp) (list r0 r1))
;; ldmeq sp, {r1, r7} -> (inst ldm (pre-index sp) (list r1 r7) :eq)
;;
;; Same for STM.  For indexing, you can specify either pre-index or
;; post-index to have the register updated. Some combinations of the
;; dst register and the register list are invalid, but no check is
;; made for that here.
(macrolet
    ((frob (name code)
       `(define-instruction ,name (segment dst reg-list &optional (cond :al))
	  (:declare (type (or tn indexing-mode) dst)
		    (type list reg-list)
		    (type condition-code cond))
	  (:printer format-4
		    ((opb0 #b100)
		     (op4 ,code)
		     (ld 1)))
	  (:emitter
	   (let ((regbits 0))
	     (dolist (r reg-list)
	       (setf (logbitp (reg-tn-encoding r) regbits) 1))
	     (multiple-value-bind (d write-back)
		 (if (tn-p dst)
		     (values dst 0)
		     (values (indexing-mode-reg dst) 1))
	       (emit-format-4 segment
			      (condition-code-encoding cond)
			      #b100
			      ,code
			      write-back
			      1
			      (reg-tn-encoding d)
			      regbits)))))))
  (frob ldm #b010)
  (frob ldmda #b000)
  (frob ldmdb #b100)
  (frob ldmib #b110))

(macrolet
    ((frob (name code)
       `(define-instruction stmdb (segment dst reg-list &optional (cond :al))
	  (:declare (type (or tn indexing-mode) dst)
		    (type list reg-list)
		    (type condition-code cond))
	  (:printer format-4
		    ((opb0 #b100)
		     (op4 #b100)
		     (ld 0)))
	  (:emitter
	   (let ((regbits 0))
	     (dolist (r reg-list)
	       (setf (logbitp (reg-tn-encoding r) regbits) 1))
     
	     (multiple-value-bind (d write-back)
		 (if (tn-p dst)
		     (values dst 0)
		     (values (indexing-mode-reg dst) 1))
	       (emit-format-4 segment
			      (condition-code-encoding cond)
			      #b100
			      #b100
			      write-back
			      0
			      (reg-tn-encoding d)
			      regbits)))))))
  (frob stmdb #b100)
  (frob stm #b010)
  (frob stmda #b000)
  (frob stmib #b110))
