;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This has been placed in the public domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream-comp.lisp,v 1.1 2010/07/20 21:34:29 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; Precompile builtin external-formats.

(in-package "LISP")

(intl:textdomain "cmucl")

;; The external format :iso8859-1 is builtin so we want all of the
;; basic methods to be compiled so that they don't have to be compiled
;; at runtime.  There are issues if we don't do this.
;;
;; These are needed for both unicode and non-unicode Lisps.
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-cin+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-cout+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-sout+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-os+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-so+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-en+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-de+)

