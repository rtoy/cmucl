;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm64/alloc.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle allocating simple objects.
;;;
;;; Ported to ARM64 from the SPARC backend.
;;;

(in-package "ARM64")

;;; But we do everything inline now that we have a better pseudo-atomic.
