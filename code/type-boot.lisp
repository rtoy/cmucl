;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/type-boot.lisp,v 1.6.1.1 1993/01/23 14:30:29 ram Exp $")
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
