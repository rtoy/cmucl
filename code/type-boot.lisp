;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Some initialization hacks that we need to get the type system started up
;;; enough so that we can define the types used to define types.
;;;
(in-package "C")

;;; Make these types be sort-of-defined to allow bootstrapping.
(setf (info type defined-structure-info 'defstruct-description)
      (make-defstruct-description))

(setf (info type defined-structure-info 'defstruct-slot-description)
      (make-defstruct-description))


;;; Define this now so that EQUAL works:
;;;
(defun pathnamep (x)
  (and (structurep x)
       (eq (%primitive header-ref x %g-vector-structure-name-slot)
	   'pathname)))

;;; Define so that we can test for VOLATILE-INFO-ENVs from the beginning of
;;; initialization.
;;;
(defun volatile-info-env-p (x)
  (and (structurep x)
       (eq (%primitive header-ref x %g-vector-structure-name-slot)
	   'volatile-info-env)))


(deftype inlinep ()
  '(member :inline :maybe-inline :notinline nil))

(deftype boolean ()
  '(member t nil))
