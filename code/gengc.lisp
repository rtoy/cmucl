;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/gengc.lisp,v 1.1 1993/05/20 13:45:42 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Lisp level interface to the Generational Garbage Collector.
;;;
;;; Written by William Lott.
;;; 

(in-package "LISP")

(defun do-before-gc-stuff ()
  nil)

(defun do-after-gc-stuff ()
  nil)

