;;; **********************************************************************
;;; -*- Package: C -*-
;;;
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/globals.lisp,v 1.5.2.1 2002/03/23 18:50:21 pw Exp $")

(in-package "C")
(declaim (special
	  *defprint-pretty* *event-info* *event-note-threshold*
	  *compiler-error-context*
	  *converting-for-interpreter*
	  *undefined-warnings*
	  *code-segment* *elsewhere*
	  *collect-dynamic-statistics* *count-vop-usages* *dynamic-counts-tn*
	  *source-info*))
