;;; -*- Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/exports-errno.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Defines the ERRNO package and all the exported symbols.
;;;
;;; This file is auto-generated via bin/create-errno.sh.
;;;
;;; DO NOT EDIT!
;;;

(in-package "LISP")

(intl:textdomain "cmucl")

(if (find-package "ERRNO")
    (rename-package "ERRNO" "ERRNO" 'nil)
    (make-package "ERRNO" :nicknames 'nil :use nil))

(use-package '("LISP") "ERRNO")

(defpackage "ERRNO"
  (:export
   "ESUCCESS"
