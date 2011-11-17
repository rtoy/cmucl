;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: INTL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment "$Header: src/code/intl-tramp.lisp $")

;;;
;;; **********************************************************************
;;;
;;; This is a stub for building CMUCL. We need FIND-DOMAIN to be
;;; defined during worldbuild.  The real version will get loaded in
;;; intl.lisp during worldload.

(in-package "INTL")

(defun find-domain (domain locale &optional (locale-dir *locale-directories*))
  (declare (ignore domain locale locale-dir))
  nil)
