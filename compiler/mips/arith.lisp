;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the VM definition arithmetic VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "C")



;;;; Unary operations.

(define-vop (fixnum-unop)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types fixnum)
  (:result-types fixnum)
  (:policy :fast-safe))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst sub res zero-tn x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:temporary (:scs (any-reg) :type fixnum :to (result 0))
	      temp)
  (:translate lognot)
  (:generator 1
    (loadi temp (fixnum -1))
    (inst xor res x temp)))



;;;; Binary operations.

;;; Assume that any constant operand is the second arg...

(defmacro define-fixnum-binop (name inherits translate op
				    &key unsigned immed-op function)
  `(define-vop (,name ,inherits)
     (:translate ,translate)
     (:generator 1
       (sc-case y
	 ((any-reg descriptor-reg)
	  (inst ,op r x y))
	 (zero
	  (inst ,op r x zero-tn))
	 ,@(when immed-op
	     `(((immediate
		 ,(if unsigned 'unsigned-immediate 'negative-immediate))
		(inst ,immed-op r x
		      (fixnum ,(if function
				   `(,function (tn-value y))
				   '(tn-value y)))))))))))

;;;; Arithmetic:

(define-vop (fast-binop/fixnum)
  (:args (x :target r
	    :scs (any-reg descriptor-reg))
	 (y :target r
	    :scs (any-reg descriptor-reg negative-immediate immediate zero)))
  (:results (r :scs (any-reg descriptor-reg)))
  (:arg-types fixnum fixnum)
  (:result-types fixnum)
  (:effects)
  (:affected)
  (:note "inline fixnum arithmetic")
  (:policy :fast-safe))

(define-fixnum-binop fast-+/fixnum fast-binop/fixnum + add
  :immed-op addi)

(define-fixnum-binop fast--/fixnum fast-binop/fixnum - sub
  :immed-op addi :function -)


;;;; Logic operations:

;;; Like fast-binop/fixnum, except the immediate operand is unsigned, and
;;; a fixnum result assertion isn't needed.
;;;
(define-vop (fast-logic-binop/fixnum fast-binop/fixnum)
  (:args (x :target r
	    :scs (any-reg descriptor-reg))
	 (y :target r
	    :scs (any-reg descriptor-reg immediate unsigned-immediate zero)))
  (:result-types t))

(define-fixnum-binop fast-logior/fixnum fast-logic-binop/fixnum logior or
  :immed-op ori :unsigned t)

(define-fixnum-binop fast-logand/fixnum fast-logic-binop/fixnum logand and
  :immed-op andi :unsigned t)

(define-fixnum-binop fast-logxor/fixnum fast-logic-binop/fixnum logxor xor
  :immed-op xori :unsigned t)



;;;; Binary conditional VOPs:

(define-vop (fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg negative-immediate immediate zero)))
  (:arg-types fixnum fixnum)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe)
  (:note "inline fixnum comparison"))


(define-vop (fast-if-</fixnum fast-conditional/fixnum)
  (:temporary (:type fixnum :scs (any-reg descriptor-reg)
	       :from (:argument 0))
	      temp)
  (:translate <)
  (:generator 3
    (sc-case y
      (zero
       (if not-p
	   (inst bgez x target)
	   (inst bltz x target)))
      ((negative-immediate immediate)
       (inst slti temp x (fixnum (tn-value y)))
       (if not-p
	   (inst beq temp zero-tn target)
	   (inst bne temp zero-tn target)))
      ((any-reg descriptor-reg)
       (inst slt temp x y)
       (if not-p
	   (inst beq temp zero-tn target)
	   (inst bne temp zero-tn target))))
    (nop)))

(define-vop (fast-if->/fixnum fast-conditional/fixnum)
  (:temporary (:type fixnum :scs (any-reg descriptor-reg)
	       :from (:argument 0))
	      temp)
  (:translate >)
  (:generator 3
    (sc-case y
      (zero
       (if not-p
	   (inst blez x target)
	   (inst bgtz x target)))
      ((negative-immediate immediate)
       (inst slti temp x (fixnum (1+ (tn-value y))))
       (if not-p
	   (inst bne temp zero-tn target)
	   (inst beq temp zero-tn target)))
      ((any-reg descriptor-reg)
       (inst slt temp y x)
       (if not-p
	   (inst beq temp zero-tn target)
	   (inst bne temp zero-tn target))))
    (nop)))

(define-vop (fast-if-=/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg zero)))
  (:translate =)
  (:generator 2
    (let ((foo (sc-case y
		 (zero zero-tn)
		 ((any-reg descriptor-reg) y))))
      (if not-p
	  (inst bne x foo target)
	  (inst beq x foo target)))
    (nop)))
