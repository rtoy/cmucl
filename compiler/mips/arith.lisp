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
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0))
	      temp)
  (:translate lognot)
  (:generator 1
    (loadi temp (fixnum -1))
    (inst xor res x temp)))



;;;; Binary operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-binop)
  (:args (x :target r
	    :scs (any-reg descriptor-reg))
	 (y :target r
	    :scs (any-reg descriptor-reg)))
  (:arg-types fixnum fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:effects)
  (:affected)
  (:note "inline fixnum arithmetic")
  (:policy :fast-safe))

(defmacro define-fixnum-binop ((name translate cost result-type)
			       op
			       &key unsigned immed-op function)
  `(define-vop (,name fast-binop)
     (:args (x :target r
	       :scs (any-reg descriptor-reg))
	    (y :target r
	       :scs (any-reg descriptor-reg
			     ,@(when immed-op
				 (list (if unsigned
					   'unsigned-immediate
					   'negative-immediate)
				       'immediate
				       'zero)))))
     (:translate ,translate)
     (:result-types ,result-type)
     (:generator ,cost
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

;;; Plus and minus.

(define-fixnum-binop (fast-+/fixnum=>fixnum + 1 fixnum)
		     addu :immed-op addiu))
(define-fixnum-binop (fast-+/fixnum + 2 t)
		     add :immed-op addi)

(define-fixnum-binop (fast--/fixnum=>fixnum - 1 fixnum)
		     subu :immed-op addiu :function -)
(define-fixnum-binop (fast--/fixnum - 2 t)
		     sub :immed-op addi :function -)


;;; Logical operatons.

(define-fixnum-binop (fast-logior/fixnum logior 1 t)
		     or :immed-op ori :unsigned t)

(define-fixnum-binop (fast-logand/fixnum logand 1 t)
		     and :immed-op andi :unsigned t)

(define-fixnum-binop (fast-logxor/fixnum logxor 1 t)
		     xor :immed-op xori :unsigned t)


;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-binop)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:result-types fixnum)
  (:translate *)
  (:generator 4
    (inst sra temp y 2)
    (inst mult x temp)
    (inst mflo r)))

(define-vop (fast-*/fixnum fast-binop)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:translate *)
  (:generator 4
    (let ((fixnum (gen-label)))
      (inst sra temp y 2)
      (inst mult x temp)
      (inst mfhi temp)
      (inst beq temp zero-tn fixnum)
      (inst mflo r)
      ;; ### Need to make a bignum out of the high and low regs.

      (emit-label fixnum))))

(define-vop (fast-truncate/fixnum fast-binop)
  (:translate truncate)
  (:temporary (:scs (non-descriptor-reg) :type random) t1 t2)
  (:results (q :scs (any-reg descriptor-reg))
	    (r :scs (any-reg descriptor-reg)))
  (:node-var node)
  (:generator 5
    (let ((zero (generate-error-code node di:division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst sra t1 x 2)
    (inst sra t2 y 2)
    (inst div t1 t2)
    (inst mflo t1)
    (inst sll q t1 2)
    (inst mfhi t1)
    (inst sll r t1 2)))

(define-vop (fast-rem/fixnum fast-binop)
  (:temporary (:scs (non-descriptor-reg) :type random) t1 t2)
  (:translate rem)
  (:generator 4
    (let ((zero (generate-error-code di:division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst sra t1 x 2)
    (inst sra t2 y 2)
    (inst div t1 t2)
    (inst mfhi t1)
    (inst sll r t1 2)))




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


(define-static-function two-arg-+ (x y) :translate +)
(define-static-function two-arg-- (x y) :translate -)
(define-static-function two-arg-* (x y) :translate *)
(define-static-function two-arg-/ (x y) :translate /)

(define-static-function negate (x) :translate -)
