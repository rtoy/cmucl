;;; **********************************************************************
;;; -*- Package: C -*-
;;;
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/globals.lisp,v 1.6.54.1 2010/02/08 17:15:50 rtoy Exp $")

(in-package "C")
(intl:textdomain "cmucl")

(declaim (special
	  *defprint-pretty* *event-info* *event-note-threshold*
	  *compiler-error-context*
	  *converting-for-interpreter*
	  *undefined-warnings*
	  *code-segment* *elsewhere*
	  *collect-dynamic-statistics* *count-vop-usages* *dynamic-counts-tn*
	  *source-info*))
