;;; -*- Package: X86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: src/assembly/x86/alloc.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle allocating simple objects.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski -- Spring 1995.
;;;

(in-package "X86")

;;; But we do everything inline now that we have a better pseudo-atomic.
