;;; -*- Mode: Lisp; Package: EXTENSIONS; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (Scott.Fahlman@CS.CMU.EDU). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/weak.lisp,v 1.1 1990/06/04 18:51:21 ch Exp $
;;;
;;; Weak Pointer Support.
;;;
;;; Written by Christopher Hoover.
;;; 

(in-package "EXTENSIONS")

(export '(weak-pointer weak-pointer-p make-weak-pointer weak-pointer-value))

(defun make-weak-pointer (object)
  "Allocates and returns a weak pointer which points to OBJECT."
  (c::%make-weak-pointer object nil))

(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns the value of WEAK-POINTER and T.
  If the referent of WEAK-POINTER has been garbage collected, returns
  the values NIL and NIL.  The value may be set with SETF."
  (declare (type weak-pointer weak-pointer))
  (without-gcing
    (let ((value (c::%weak-pointer-value weak-pointer))
	  (broken (c::%weak-pointer-broken weak-pointer)))
      (values value (not broken)))))

(defun set-weak-pointer-value (weak-pointer new-value)
  (declare (type weak-pointer weak-pointer))
  (without-gcing
    (setf (c::%weak-pointer-value weak-pointer) new-value)
    (setf (c::%weak-pointer-broken weak-pointer) nil)
    new-value))

(defsetf weak-pointer-value set-weak-pointer-value)
