;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/move.lisp,v 1.17 1990/06/16 15:35:17 wlott Exp $
;;;
;;;    This file contains the MIPS VM definition of operand loading/saving and
;;; the Move VOP.
;;;
;;; Written by Rob MacLachlan.
;;; MIPS conversion by William Lott.
;;;
(in-package "C")


(define-move-function (load-immediate 1) (vop x y)
  ((null unsigned-immediate immediate zero negative-immediate
	 random-immediate immediate-base-character immediate-sap)
   (any-reg descriptor-reg base-character-reg sap-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst li y (fixnum val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst li y (logior (ash (char-code val) type-bits)
			  base-character-type))))))

(define-move-function (load-number 1) (vop x y)
  ((null unsigned-immediate immediate zero negative-immediate random-immediate)
   (signed-reg unsigned-reg))
  (inst li y (tn-value x)))

(define-move-function (load-base-character 1) (vop x y)
  ((immediate-base-character) (base-character-reg))
  (inst li y (char-code (tn-value x))))

(define-move-function (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (loadw y code-tn (tn-offset x) other-pointer-type))

(define-move-function (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-function (load-number-stack 5) (vop x y)
  ((base-character-stack) (base-character-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (let ((nfp (current-nfp-tn vop)))
    (loadw y nfp (tn-offset x))))

(define-move-function (load-single 5) (vop x y)
  ((single-stack) (single-reg))
  (cerror "Do nothing." "Not yet." x y))

(define-move-function (load-double 7) (vop x y)
  ((double-stack) (double-reg))
  (cerror "Do nothing." "Not yet." x y))

(define-move-function (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack))
  (store-stack-tn y x))

(define-move-function (store-number-stack 5) (vop x y)
  ((base-character-reg) (base-character-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (let ((nfp (current-nfp-tn vop)))
    (storew x nfp (tn-offset y))))

(define-move-function (store-single 5) (vop x y)
  ((single-reg) (single-stack))
  (cerror "Do nothing." "Not yet." x y))

(define-move-function (store-double 7) (vop x y)
  ((double-reg) (double-stack))
  (cerror "Do nothing." "Not yet." x y))



;;;; The Move VOP:
;;;
(define-vop (move)
  (:args (x :target y
	    :scs (any-reg descriptor-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; Make Move the check VOP for T so that type check generation doesn't think
;;; it is a hairy type.  This also allows checking of a few of the values in a
;;; continuation to fall out.
;;;
(primitive-type-vop move (:check) t)

;;;    The Move-Argument VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
;;;
(define-vop (move-argument)
  (:args (x :target y
	    :scs (any-reg descriptor-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move y x))
      (control-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-argument :move-argument
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))



;;;; ILLEGAL-MOVE

;;; This VOP exists just to begin the lifetime of a TN that couldn't be written
;;; legally due to a type error.  An error is signalled before this VOP is
;;; so we don't need to do anything (not that there would be anything sensible
;;; to do anyway.)
;;;
(define-vop (illegal-move)
  (:args (x) (type))
  (:results (y))
  (:ignore y)
  (:generator 666
    (error-call object-not-type-error x type)))



;;;; Moves and coercions:

;;; Move a tagged number to an untagged representation.
;;;
(define-vop (move-to-signed/unsigned)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (sc-case x
      (any-reg
       (inst sra y x 2))
      (descriptor-reg
       (let ((done (gen-label)))
	 (inst and temp x 3)
	 (inst beq temp done)
	 (sc-case y
	   (signed-reg
	    (inst sra y x 2))
	   (unsigned-reg
	    (inst srl y x 2)))

	 (loadw y x vm:bignum-digits-offset vm:other-pointer-type)

	 (emit-label done))))))

;;;
(define-move-vop move-to-signed/unsigned :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))


;;; Move an untagged number to a tagged representation.
;;;
(define-vop (move-from-signed/unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:generator 20
    (sc-case y
      (any-reg
       ;; The results must be a fixnum, so we can just do the shift.
       (inst sll y arg 2))
      (descriptor-reg
       ;; The results might be a bignum, so we have to make sure.
       (move x arg)
       (sc-case arg
	 (signed-reg
	  (let ((fixnum (gen-label))
		(done (gen-label)))
	    (inst sra temp x 29)
	    (inst beq temp fixnum)
	    (inst nor temp zero-tn)
	    (inst beq temp done)
	    (inst sll y x 2)

	    (pseudo-atomic (temp)
	      (inst addu y alloc-tn vm:other-pointer-type)
	      (inst addu alloc-tn
		    (vm:pad-data-block (1+ vm:bignum-digits-offset)))
	      (inst li temp (logior (ash 1 vm:type-bits) vm:bignum-type))
	      (storew temp y 0 vm:other-pointer-type)
	      (storew x y vm:bignum-digits-offset vm:other-pointer-type))
	    (inst b done)
	    (inst nop)

	    (emit-label fixnum)
	    (inst sll y x 2)
	    (emit-label done)))
	 (unsigned-reg
	  (let ((done (gen-label))
		(one-word (gen-label)))
	    (inst sra temp x 29)
	    (inst beq temp done)
	    (inst sll y x 2)

	    (pseudo-atomic (temp)
	      (inst addu y alloc-tn vm:other-pointer-type)
	      (inst addu alloc-tn
		    (vm:pad-data-block (1+ vm:bignum-digits-offset)))
	      (inst bgez x one-word)
	      (inst li temp (logior (ash 1 vm:type-bits) vm:bignum-type))
	      (inst addu alloc-tn (vm:pad-data-block 1))
	      (inst li temp (logior (ash 2 vm:type-bits) vm:bignum-type))
	      (emit-label one-word)
	      (storew temp y 0 vm:other-pointer-type)
	      (storew x y vm:bignum-digits-offset vm:other-pointer-type))
	    (emit-label done))))))))
      
;;;
(define-move-vop move-from-signed/unsigned :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))


;;; Move untagged numbers.
;;;
(define-vop (signed/unsigned-move)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 1
    (move y x)))
;;;
(define-move-vop signed/unsigned-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))


;;; Move untagged number arguments/return-values.
;;;
(define-vop (move-signed/unsigned-argument)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move y x))
      ((signed-stack unsigned-stack)
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-signed/unsigned-argument :move-argument
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged number to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

