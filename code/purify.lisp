;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/purify.lisp,v 1.11 1991/02/08 13:35:00 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Storage purifier for Spice Lisp.
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
;;; Rewritten in C by William Lott.
;;;
(in-package 'lisp)

(def-c-routine ("purify" %purify) (void)
  (static-roots unsigned-long)
  (read-only-roots unsigned-long))

(defun purify (&key root-structures constants)
  (write-string "[Doing purification: ")
  (force-output)
  (without-gcing
   (clear-auto-gc-trigger)
   (%purify (di::get-lisp-obj-address root-structures)
	    (di::get-lisp-obj-address constants))
   (when *gc-trigger*
     (setf *gc-trigger* *bytes-consed-between-gcs*)
     (set-auto-gc-trigger *gc-trigger*)))
  (write-line "Done.]")
  (force-output)
  nil)

