;;; -*- Mode: Lisp; Package: EXTENSIONS; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/weak.lisp,v 1.4 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/weak.lisp,v 1.4 1994/10/31 04:11:27 ram Exp $
;;;
;;; Weak Pointer Support.
;;;
;;; Written by Christopher Hoover.
;;; 

(in-package "EXTENSIONS")

(export '(weak-pointer weak-pointer-p make-weak-pointer weak-pointer-value))

(defun make-weak-pointer (object)
  "Allocates and returns a weak pointer which points to OBJECT."
  (declare (values weak-pointer))
  (make-weak-pointer object))

(declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns the value of WEAK-POINTER and T.
   If the referent of WEAK-POINTER has been garbage collected, returns
   the values NIL and NIL."
  (declare (type weak-pointer weak-pointer)
	   (values t (member t nil)))
  ;; We don't need to wrap this with a without-gcing, because once we have
  ;; extracted the value, our reference to it will keep the weak pointer
  ;; from becoming broken.  We just have to make sure the compiler won't
  ;; reorder these primitives.
  (let ((value (c::%weak-pointer-value weak-pointer))
	(broken (c::%weak-pointer-broken weak-pointer)))
    (values value (not broken))))
