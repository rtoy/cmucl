;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/hppa/pred.lisp,v 1.1 1992/07/13 03:48:30 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of predicate VOPs for the HPPA
;;;
;;; Written by William Lott.
;;; 

(in-package "HPPA")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest :nullify t)))


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
	 (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst bc := not-p x y target)))
