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
       (eq (%primitive structure-ref x 0)
	   'pathname)))

;;; Define so that we can test for VOLATILE-INFO-ENVs from the beginning of
;;; initialization.
;;;
(defun volatile-info-env-p (x)
  (and (structurep x)
       (eq (%primitive structure-ref x 0)
	   'volatile-info-env)))


(deftype inlinep ()
  '(member :inline :maybe-inline :notinline nil))

(deftype boolean ()
  '(member t nil))

;;; Define this so that we can define the type system.
(in-package "KERNEL")
(defun ctype-p (thing)
  (and (structurep thing)
       (member (%primitive structure-ref thing 0)
	       '(ctype hairy-type named-type numeric-type array-type
		       member-type structure-type union-type args-type
		       values-type function-type))))

(defun values-type-p (thing)
  (and (structurep thing)
       (eq (%primitive structure-ref thing 0) 'values-type)))

;;; Define this so that we can copy type-class structures before the defstruct
;;; for type-class runs.  ### It relies on structures being identical to
;;; simple-vectors so that the length can be extracted from the -1'st slot.
;;;
(defun copy-type-class (tc)
  (let ((new (make-type-class)))
    (dotimes (i (truly-the index (%primitive structure-ref tc -1)))
      (declare (type index i))
      (%primitive structure-index-set new i
		  (%primitive structure-index-ref tc i)))
    new))
