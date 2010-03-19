;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: INTL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/intl-tramp.lisp,v 1.2 2010/03/19 15:18:59 rtoy Rel $")

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
