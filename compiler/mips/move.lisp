;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/move.lisp,v 1.14 1990/05/09 06:39:33 wlott Exp $
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
	 (fp :scs (descriptor-reg)
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
    (error-call di:object-not-type-error x type)))


