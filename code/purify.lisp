;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/purify.lisp,v 1.12 1992/02/14 23:45:24 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Storage purifier for Spice Lisp.
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
;;; Rewritten in C by William Lott.
;;;
(in-package 'lisp)

(alien:def-alien-routine ("purify" %purify) c-call:void
  (static-roots c-call:unsigned-long)
  (read-only-roots c-call:unsigned-long))

(defun purify (&key root-structures constants)
  (write-string "[Doing purification: ")
  (force-output)
  (without-gcing
   (clear-auto-gc-trigger)
   (%purify (get-lisp-obj-address root-structures)
	    (get-lisp-obj-address constants))
   (when *gc-trigger*
     (setf *gc-trigger* *bytes-consed-between-gcs*)
     (set-auto-gc-trigger *gc-trigger*)))
  (write-line "Done.]")
  (force-output)
  nil)

