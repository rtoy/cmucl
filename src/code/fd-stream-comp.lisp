;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This has been placed in the public domain.
;;; 
(ext:file-comment
 "$Header: src/code/fd-stream-comp.lisp $")
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
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-osc+)

;; :utf-8 is builtin.  Important since it's the default now.
(stream::precompile-ef-slot :utf-8 #.stream::+ef-cin+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-cout+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-sout+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-os+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-so+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-en+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-de+)
(stream::precompile-ef-slot :utf-8 #.stream::+ef-osc+)

;; :ascii is builtin.
(stream::precompile-ef-slot :ascii #.stream::+ef-cin+)
(stream::precompile-ef-slot :ascii #.stream::+ef-cout+)
(stream::precompile-ef-slot :ascii #.stream::+ef-sout+)
(stream::precompile-ef-slot :ascii #.stream::+ef-os+)
(stream::precompile-ef-slot :ascii #.stream::+ef-so+)
(stream::precompile-ef-slot :ascii #.stream::+ef-en+)
(stream::precompile-ef-slot :ascii #.stream::+ef-de+)
(stream::precompile-ef-slot :ascii #.stream::+ef-osc+)
