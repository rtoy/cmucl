;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/purify.lisp,v 1.16 1994/02/14 13:48:24 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Storage purifier for Spice Lisp.
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
;;; Rewritten in C by William Lott.
;;;
(in-package "LISP")
(export 'ext::purify "EXT")

(alien:def-alien-routine ("purify" %purify) c-call:void
  (static-roots c-call:unsigned-long)
  (read-only-roots c-call:unsigned-long))

(defun purify (&key root-structures (environment-name "Auxiliary"))
  "This function optimizes garbage collection by moving all currently live
   objects into non-collected storage.  ROOT-STRUCTURES is an optional list of
   objects which should be copied first to maximize locality.

   DEFSTRUCT structures defined with the (:PURE T) option are moved into
   read-only storage, further reducing GC cost.  List and vector slots of pure
   structures are also moved into read-only storage.

   ENVIRONMENT-NAME is gratuitous documentation for compacted version of the
   current global environment (as seen in C::*INFO-ENVIRONMENT*.)  If NIL is
   supplied, then environment compaction is inhibited."
  (when environment-name
    (let ((old-ie (car *info-environment*)))
      (setq *info-environment*
	    (list* (make-info-environment :name "Working")
		   (compact-info-environment (first *info-environment*)
					     :name environment-name)
		   (rest *info-environment*)))
      ;; next 2 lines for GC.
      (shrink-vector (c::volatile-info-env-table old-ie) 0)
      (setq old-ie nil)))

  (let ((*gc-notify-before*
	 #'(lambda (bytes-in-use)
	     (declare (ignore bytes-in-use))
	     (write-string "[Doing purification: ")
	     (force-output)))
	(*internal-gc*
	 #'(lambda ()
	     (%purify (get-lisp-obj-address root-structures)
		      (get-lisp-obj-address nil))))
	(*gc-notify-after*
	 #'(lambda (&rest ignore)
	     (declare (ignore ignore))
	     (write-line "Done.]"))))
    (gc t))
  nil)
