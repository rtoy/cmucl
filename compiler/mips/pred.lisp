;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/pred.lisp,v 1.3 1990/03/06 19:39:09 wlott Exp $
;;;
;;;    This file contains the VM definition of predicate VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "C")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (b dest)
    (nop)))


;;;; Conditional VOPs:

;if-true (???), if-eql, ...

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
	 (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (let ((x-prime (sc-case x
		     ((any-reg descriptor-reg) x)
		     (zero zero-tn)
		     (null null-tn)))
	  (y-prime (sc-case y
		     ((any-reg descriptor-reg) y)
		     (zero zero-tn)
		     (null null-tn))))
      (if not-p
	  (inst bne x-prime y-prime target)
	  (inst beq x-prime y-prime target)))
    (nop)))


