;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/type-boot.lisp,v 1.7 1993/02/26 08:26:22 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Some initialization hacks that we need to get the type system started up
;;; enough so that we can define the types used to define types.
;;;
(in-package "C")

(deftype inlinep ()
  '(member :inline :maybe-inline :notinline nil))

(deftype boolean ()
  '(member t nil))

(in-package "KERNEL")

;;; Define this so that we can copy type-class structures before the defstruct
;;; for type-class runs.
;;;
(defun copy-type-class (tc)
  (let ((new (make-type-class)))
    (dotimes (i (%instance-length tc))
      (declare (type index i))
      (setf (%instance-ref new i)
	    (%instance-ref tc i)))
    new))

#-ns-boot
;;; Define the STRUCTURE-OBJECT class as a subclass of INSTANCE.  This must be
;;; the first DEFSTRUCT executed.
;;;
(defstruct (structure-object (:alternate-metaclass instance)))
