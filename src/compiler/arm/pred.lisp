;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/pred.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of predicate VOPs for the ARM.
;;;

(in-package "ARM")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (emit-not-implemented)))


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg null))
	 (y :scs (any-reg descriptor-reg null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (emit-not-implemented)
    (inst cmp x y)
    (inst b target (if not-p :ne :eq))))
