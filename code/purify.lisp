;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/purify.lisp,v 1.13 1992/03/26 03:18:51 wlott Exp $")
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
  (let ((*gc-notify-before*
	 #'(lambda (bytes-in-use)
	     (declare (ignore bytes-in-use))
	     (write-string "[Doing purification: ")
	     (force-output)))
	(*internal-gc*
	 #'(lambda ()
	     (%purify (get-lisp-obj-address root-structures)
		      (get-lisp-obj-address constants))))
	(*gc-notify-after*
	 #'(lambda (&rest ignore)
	     (declare (ignore ignore))
	     (write-line "Done.]"))))
    (gc t))
  nil)

