;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; Description of the IBM RT instruction set.
;;;
;;; Written by William Lott and Bill Chiles.
;;;

(in-package "RT")

(use-package "ASSEM")
(use-package "EXT")



;;;; Formats.

(define-format (j1 16)
  (op (byte 4 12))
  (sub-op (byte 1 11))
  (n (byte 3 8))
  (j1 (byte 8 0)))

(define-format (x 16)
  (op (byte 4 12))
  (r1 (byte 4 8))
  (r2 (byte 4 4))
  (r3 (byte 4 0)))

(define-format (d-short 16)
  (op (byte 4 12))
  (i (byte 4 8))
  (r2 (byte 4 4))
  (r3 (byte 4 0)))

(define-format (r 16)
  (op (byte 8 8))
  (r2 (byte 4 4))
  (r3 (byte 4 0)))

(define-format (bi 32)
  (op (byte 8 24))
  (r2 (byte 4 20))
  (bi (byte 20 0)))

(define-format (ba 32)
  (op (byte 8 24))
  (ba (byte 24 0)))

(define-format (d 32)
  (op (byte 8 24))
  (r2 (byte 4 20))
  (r3 (byte 4 16))
  (i (byte 16 0)))



;;;; Special argument types and fixups.

(define-argument-type register
  :type '(satisfies (lambda (object)
		      (and (tn-p object)
			   (or (eq (sc-name (tn-sc object)) 'null)
			       (eq (sb-name (sc-sb (tn-sc object)))
				   'registers)))))
  :function (lambda (tn)
	      (case (sc-name (tn-sc tn))
		(null null-offset)
		(t (tn-offset tn)))))

(defun address-register-p (object)
  (and (tn-p object)
       (not (zerop (tn-offset object)))
       (eq (sb-name (sc-sb (tn-sc object))) 'registers)))

(define-argument-type address-register
  :type '(satisfies address-register-p)
  :function tn-offset)


;;; LABEL-OFFSET -- Internal.
;;;
;;; This uses assem:*current-position* and the label's position to compute
;;; the bits that an instructions immediate field wants.
;;;
(defun label-offset (label)
  (ash (- (label-position label) *current-position*) -1))

(define-argument-type relative-label
  :type 'label
  :function label-offset)


(defun jump-condition-value (cond)
  (ecase cond
    (:pz #b000)
    (:lt #b001)
    (:eq #b010)
    (:gt #b011)
    (:c0 #b100)
    (:ov #b110)
    (:tb #b111)))

(define-argument-type jump-condition
  :type '(member :pz :lt :eq :gt :c0 :ov :tb)
  :function jump-condition-value)

(defun branch-condition-value (cond)
  (ecase cond
    (:pz #b1000)
    (:lt #b1001)
    (:eq #b1010)
    (:gt #b1011)
    (:c0 #b1100)
    (:ov #b1110)
    (:tb #b1111)))

(define-argument-type branch-condition
  :type '(member :pz :lt :eq :gt :c0 :ov :tb)
  :function branch-condition-value)


(define-fixup-type :ba) ;branch-absolute.
(define-fixup-type :cau)
(define-fixup-type :cal)



;;;; Loading and storing.

;;; load-character.
;;;
(define-instruction (lc)
  (d-short (op :constant 4)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :argument (unsigned-byte 4)))
  (d-short (op :constant 4)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xCE)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; store-character.
;;;
(define-instruction (stc)
  (d-short (op :constant 1)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :argument (unsigned-byte 4)))
  (d-short (op :constant 1)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xDE)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; load-half.
;;;
(define-instruction (lh)
  (d (op :constant #xDA)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16)))
  (r (op :constant #xEB)
     (r2 :argument register)
     (r3 :argument register)))

;;; load-half-algebraic.
;;;
(define-instruction (lha)
  (d (op :constant #xCA)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16)))
  (d-short (op :constant 5)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a half-word index.
	   (i :argument (member 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
	      :function (lambda (x) (ash x -1)))))

;;; store-half.
;;;
(define-instruction (sth)
  (d-short (op :constant 2)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a half-word index.
	   (i :argument (member 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
	      :function (lambda (x) (ash x -1))))
  (d (op :constant #xDC)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; load-word.
;;;
(define-instruction (l)
  (d-short (op :constant 7)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a word index.
	   (i :argument (member 0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
	      :function (lambda (x) (ash x -2))))
  (d-short (op :constant 7)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xCD)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; store-word.
;;;
(define-instruction (st)
  (d-short (op :constant 3)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a word index.
	   (i :argument (member 0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
	      :function (lambda (x) (ash x -2))))
  (d-short (op :constant 3)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xDD)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))



;;;; Address computation.

;;; compute-address-lower-half.
;;;
;;; The second format can be used for load-immediate (signed-byte 16).
;;;
(define-instruction (cal)
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16)))
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument (signed-byte 16)))
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument cal-fixup)))

;;; compute-address-lower-half-16bit.
;;;
;;; The second format can be used for load-immediate (unsigned-byte 16).
;;;
(define-instruction (cal16)
  (d (op :constant #xC2)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xC2)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument (unsigned-byte 16))))

;;; compute-address-upper-half.
;;;
(define-instruction (cau)
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument cau-fixup)))

;;; compute-address-short.
;;;
;;; Use the second flavor to copy registers.
;;;
(define-instruction (cas)
  (x (op :constant 6)
     (r1 :argument register)
     (r2 :argument register)
     (r3 :argument address-register))
  (x (op :constant 6)
     (r1 :argument register)
     (r2 :argument register)
     (r3 :constant 0)))

;;; increment.
;;;
(define-instruction (inc)
  (r (op :constant 91)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4))))

;;; decrement
;;;
(define-instruction (dec)
  (r (op :constant 93)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4))))


;;; load-immediate-short.
;;;
(define-instruction (lis)
  (r (op :constant #xA4)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4))))



;;;; Arithmetics.

(macrolet ((define-math-inst (name &key two-regs short-immediate immediate
				   function (signed t))
	     `(define-instruction (,name)
		,@(when two-regs
		    `((r (op :constant ,two-regs)
			 (r2 :argument register)
			 (r3 :argument register))
		      (r (op :constant ,two-regs)
			 (r2 :argument register)
			 (r3 :same-as r2))))
		,@(when short-immediate
		    `((r (op :constant ,short-immediate)
			 (r2 :argument register)
			 (r3 :argument (unsigned-byte 4)))))
		,@(when immediate
		    `((d (op :constant ,immediate)
			 (r2 :argument register)
			 (r3 :argument register)
			 (i :argument
			    (,(if signed 'signed-byte 'unsigned-byte) 16)
			    ,@(if function `(:function ,function))))
		      (d (op :constant ,immediate)
			 (r2 :argument register)
			 (r3 :same-as r2)
			 (i :argument
			    (,(if signed 'signed-byte 'unsigned-byte) 16)
			    ,@(if function `(:function ,function)))))))))

  (define-math-inst a :two-regs #xE1 :short-immediate #x90 :immediate #xC1)
  (define-math-inst ae :two-regs #xF1 :immediate #xD1)
  (define-math-inst abs :two-regs #xE0)
  (define-math-inst neg :two-regs #xE4) ;Arithmetic negation (two's complement).
  (define-math-inst s :two-regs #xE2 :short-immediate #x92
    :immediate #xC1 :function -)
  (define-math-inst sf :two-regs #xB2 :immediate #xD2)
  (define-math-inst se :two-regs #xF2)
  (define-math-inst d :two-regs #xB6)
  (define-math-inst m :two-regs #xE6)
  
  (define-math-inst exts :two-regs #xB1)
  
  (define-math-inst clrbl :short-immediate #x99)
  (define-math-inst clrbu :short-immediate #x98)
  (define-math-inst setbl :short-immediate #x9B)
  (define-math-inst setbu :short-immediate #x9A)
  
  (define-math-inst not :two-regs #xF4) ;Logical not.
  (define-math-inst n :two-regs #xE5)
  (define-math-inst nilz :immediate #xC5 :signed nil)
  (define-math-inst nilo :immediate #xC6 :signed nil)
  (define-math-inst niuz :immediate #xD5 :signed nil)
  (define-math-inst niuo :immediate #xD6 :signed nil)
  (define-math-inst o :two-regs #xE3)
  (define-math-inst oil :immediate #xC4 :signed nil)
  (define-math-inst oiu :immediate #xC3 :signed nil)
  (define-math-inst x :two-regs #xE7)
  (define-math-inst xil :immediate #xC7 :signed nil)
  (define-math-inst xiu :immediate #xD7 :signed nil)

  (define-math-inst clz :two-regs #xF5)

) ;macrolet


;;; compare.
;;; compare-immediate-short.
;;; compare-immediate.
;;;
(define-instruction (c)
  (r (op :constant #xB4)
     (r2 :argument register)
     (r3 :argument register))
  (r (op :constant #x94)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4)))
  (d (op :constant #xD4)
     (r2 :constant 0)
     (r3 :argument register)
     (i :argument (signed-byte 16))))


;;; compare-logical.
;;; compare-logical-immediate.
;;;
(define-instruction (cl)
  (r (op :constant #xB3)
     (r2 :argument register)
     (r3 :argument register))
  (d (op :constant #xD3)
     (r2 :constant 0)
     (r3 :argument register)
     (i :argument (signed-byte 16))))



;;;; Shifting.

(define-instruction (sr)
  (r (op :constant #xB8)
     (r2 :argument register)
     (r3 :argument register))
  (r (op :constant #xA8)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4)))
  (r (op :constant #xA9)
     (r2 :argument register)
     (r3 :argument (integer 16 31)
	 :function (lambda (x) (- x 16)))))

(define-instruction (sl)
  (r (op :constant #xBA)
     (r2 :argument register)
     (r3 :argument register))
  (r (op :constant #xAA)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4)))
  (r (op :constant #xAB)
     (r2 :argument register)
     (r3 :argument (integer 16 31)
	 :function (lambda (x) (- x 16)))))

(define-instruction (sar)
  (r (op :constant #xB0)
     (r2 :argument register)
     (r3 :argument register))
  (r (op :constant #xA0)
     (r2 :argument register)
     (r3 :argument (unsigned-byte 4)))
  (r (op :constant #xA1)
     (r2 :argument register)
     (r3 :argument (integer 16 31)
	 :function (lambda (x) (- x 16)))))



;;;; Branch instructions.

;;; There are some pseudo-instructions defined after this page that use these
;;; definitions.
;;;

(define-instruction (jb)
  (j1 (op :constant 0)
      (sub-op :constant 1)
      (n :argument jump-condition)
      (j1 :argument relative-label)))

(define-instruction (jnb)
  (j1 (op :constant 0)
      (sub-op :constant 0)
      (n :argument jump-condition)
      (j1 :argument relative-label)))

(macrolet ((define-branch-inst (name immediate-op register-op)
	     `(define-instruction (,name)
		(bi (op :constant ,immediate-op)
		    (r2 :argument branch-condition)
		    (bi :argument relative-label))
		(r (op :constant ,register-op)
		   (r2 :argument branch-condition)
		   (r3 :argument register)))))

  ;; branch-on-condition-bit-immediate.
  ;; branch-on-condition-bit-register.
  ;;
  (define-branch-inst bb #x8E #xEE)
  
  ;; branch-on-condition-bit-immediate-with-execute.
  ;; branch-on-condition-bit-register-with-execute.
  ;;
  (define-branch-inst bbx #x8F #xEF)

  ;; branch-on-not-condition-bit-immediate.
  ;; branch-on-not-condition-bit-register.
  ;;
  (define-branch-inst bnb #x88 #xE8)

  ;; branch-on-not-condition-bit-immediate-with-execute.
  ;; branch-on-not-condition-bit-register-with-execute.
  ;;
  (define-branch-inst bnbx #x89 #xE9)

) ;MACROLET

(define-instruction (bala)
  (ba (op :constant #x8A)
      (ba :argument ba-fixup)))

(define-instruction (balax)
  (ba (op :constant #x8B)
      (ba :argument ba-fixup)))



;;;; Pseudo-instructions

;;; move.
;;;
;;; This body is the second format of compute-address-short.
;;;
(define-instruction (move)
  (x (op :constant 6)
     (r1 :argument register)
     (r2 :argument register)
     (r3 :constant 0)))

;;;
;;; A couple load-immediate pseudo-instructions.
;;;

;;; load-immediate.
;;;
;;; This might affect the condition codes, but it allows for loading 32-bit
;;; quantities into R0.
;;;
(define-pseudo-instruction li 64 (reg value)
  (etypecase value
    ((unsigned-byte 4)
     (inst lis reg value))
    ((signed-byte 16)
     (inst cal reg value))
    ((unsigned-byte 16)
     (inst cal16 reg value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst cau reg (ldb (byte 16 16) value))
     (let ((low (ldb (byte 16 0) value)))
       (unless (zerop low)
	 (inst oil reg low))))))

;;; compute-address-immediate.
;;;
;;; This basically exists to load 32-bit constants into address-registers, and
;;; it does not affect condition codes.  Since fixups are always addresses, we
;;; have the fixup branch here instead of in load-immediate.  We may use the
;;; other branches since it already exists.
;;;
(define-pseudo-instruction cai 64 (reg value)
  (etypecase value
    ((unsigned-byte 4)
     (inst lis reg value))
    ((signed-byte 16)
     (inst cal reg value))
    ((unsigned-byte 16)
     (inst cal16 reg value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst cau reg (ldb (byte 16 16) value))
     (let ((low (ldb (byte 16 0) value)))
       (unless (zerop low)
	 (inst cal16 reg reg low))))
    (fixup
     (inst cau reg value)
     (inst cal reg reg value))))


;;; branch-unconditional.
;;;
(define-pseudo-instruction b 32 (target)
  (if (and (assem::label-p target)
	   (<= -128 (label-offset target) 127))
      (inst jnb :pz target)
      (inst bnb :pz target)))

;;; branch-unconditional-with-execute.
;;;
(define-pseudo-instruction bx 32 (target)
  (inst bnbx :pz target))

;;; branch-condition.
;;;
(define-pseudo-instruction bc 32 (condition target)
  (if (and (assem::label-p target)
	   (<= -128 (label-offset target) 127))
      (inst jb condition target)
      (inst bb condition target)))

;;; branch-not-condition.
;;;
(define-pseudo-instruction bnc 32 (condition target)
  (if (and (assem::label-p target)
	   (<= -128 (label-offset target) 127))
      (inst jnb condition target)
      (inst bnb condition target)))

;;; branch-condition-with-execute.
;;; branch-not-condition-with-execute.
;;;
;;; We define these, so VOP readers see a consistent naming scheme in branch
;;; instructions.
;;;
(define-pseudo-instruction bcx 32 (condition target)
  (inst bbx condition target))
;;;
(define-pseudo-instruction bncx 32 (condition target)
  (inst bnbx condition target))

;;; no-op.
;;;
;;; This is compute-address-short, adding zero to R0 putting the result in R0.
;;;
(define-instruction (no-op)
  (x (op :constant 6)
     (r1 :constant 0)
     (r2 :constant 0)
     (r3 :constant 0)))

(define-format (word-format 32)
  (data (byte 32 0)))
(define-instruction (word)
  (word-format (data :argument (or (unsigned-byte 32) (signed-byte 32)))))

(define-format (short-format 16)
  (data (byte 16 0)))
(define-instruction (short)
  (short-format (data :argument (or (unsigned-byte 16) (signed-byte 16)))))

(define-format (byte-format 8)
  (data (byte 8 0)))
(define-instruction (byte)
  (byte-format (data :argument (or (unsigned-byte 8) (signed-byte 8)))))



;;;; Breaking.

;;; break.
;;;
;;; This is trap-on-condition-immediate.  We use the immediate field to
;;; stick in a constant value indicating why we're breaking.
;;;
(define-instruction (break)
  (d (op :constant #xCC)
     (r2 :constant #b0111)
     (r3 :constant 0)
     (i :argument (signed-byte 16))))



;;;; System control.

;;; move-to-multiplier-quotient-system-control-register
;;;
(define-instruction (mtmqscr)
  (r (op :constant #xB5)
     (r2 :constant 10)
     (r3 :argument register)))

;;; move-from-multiplier-quotient-system-control-register
;;;
(define-instruction (mfmqscr)
  (r (op :constant #x96)
     (r2 :constant 10)
     (r3 :argument register)))



;;;; Function and LRA Headers emitters and calculation stuff.

(defun header-data (ignore)
  (declare (ignore ignore))
  (ash (+ *current-position* (component-header-length)) (- vm:word-shift)))

(define-format (header-object 32)
  (type (byte 8 0))
  (data (byte 24 8) :default 0 :function header-data))

(define-instruction (function-header-word)
  (header-object (type :constant vm:function-header-type)))

(define-instruction (lra-header-word)
  (header-object (type :constant vm:return-pc-header-type)))


;;; DEFINE-COMPUTE-INSTRUCTION -- Internal.
;;;
;;; This defines a pseudo-instruction, name, which requires the other specific
;;; instructions.  When the pseudo-instruction expands, it looks at the current
;;; value of the calculation to choose between two instruction sequences.
;;; Later, when the assembler emits the instructions, the :function's run to
;;; really compute the calculation's value.  This is guaranteed to be lesser in
;;; magnitude than the original test in the pseudo-instruction since all these
;;; calculation are relative to the component start, so we can only remove
;;; instructions in that range, not add them.
;;;
(defmacro define-compute-instruction (name calculation)
  (let ((cal (symbolicate name "-CAL"))
	(cau (symbolicate name "-CAU")))
    `(progn
       ;; This is a special compute-address-lower that takes a label and
       ;; asserts the type of the immediate value.
       (defun ,name (label)
	 (let* ((whole ,calculation)
		(low (logand whole #xffff))
		(high (ash whole -16)))
	   (values (if (logbitp 15 low)
		       (1+ high)
		       high)
		   low)))
       (define-instruction (,cal)
	 (d (op :constant #xC8)
	    (r2 :argument register)
	    (r3 :argument address-register)
	    (i :argument label
	       :function (lambda (label)
			   (multiple-value-bind (high low) (,name label)
			     (declare (ignore high))
			     low)))))
       (define-instruction (,cau)
	 (d (op :constant #xD8)
	    (r2 :argument register)
	    (r3 :argument address-register)
	    (i :argument label
	       :function (lambda (label)
			   (multiple-value-bind (high low) (,name label)
			     (declare (ignore low))
			     high)))))
       (define-pseudo-instruction ,name 64 (dst src label)
	 (multiple-value-bind (high low) (,name label)
	   (declare (ignore low))
	   (cond ((zerop high)
		  (inst ,cal dst src label))
		 (t
		  (inst ,cal dst src label)
		  (inst ,cau dst src label))))))))


;; code = fn - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-fn
			    (- vm:other-pointer-type
			       (label-position label)
			       (component-header-length)))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-lra
			    (- (+ (label-position label)
				  (component-header-length))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-compute-instruction compute-lra-from-code
			    (+ (label-position label)
			       (component-header-length)))
