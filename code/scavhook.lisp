;;; -*- Package: EXT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/scavhook.lisp,v 1.1 1991/07/30 00:40:04 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file implements the ``Scavenger Hook'' extension.
;;;
;;; Written by William Lott
;;;

(in-package "EXT")

(export '(scavenger-hook scavenger-hook-p make-scavenger-hook
	  scavenger-hook-value scavenger-hook-function))

(defun scavenger-hook-p (object)
  "Returns T if OBJECT is a scavenger-hook, and NIL if not."
  (scavenger-hook-p object))

(defun make-scavenger-hook (&key value (function (required-argument)))
  "Create a new scavenger-hook with the specified VALUE and FUNCTION.  For
   as long as the scavenger-hook is alive, the scavenger in the garbage
   collector will note whenever VALUE is moved, and arrange for FUNCTION
   to be funcalled."
  (declare (type function function))
  (c::%make-scavenger-hook value function))

(defun scavenger-hook-value (scavhook)
  "Returns the VALUE being monitored by SCAVHOOK.  Can be setf."
  (declare (type scavenger-hook scavhook))
  (scavenger-hook-value scavhook))

(defun (setf scavenger-hook-value) (value scavhook)
  (declare (type scavenger-hook scavhook))
  (setf (scavenger-hook-value scavhook) value))

(defun scavenger-hook-function (scavhook)
  "Returns the FUNCTION invoked when the monitored value is moved.  Can be
   setf."
  (declare (type scavenger-hook scavhook))
  (scavenger-hook-function scavhook))

(defun (setf scavenger-hook-function) (function scavhook)
  (declare (type function function)
	   (type scavenger-hook scavhook))
  (setf (scavenger-hook-function scavhook) function))

