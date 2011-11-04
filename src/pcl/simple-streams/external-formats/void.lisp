;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/void.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; This is actually implemented in the external-formats code
;; It appears here only for reference, and will never get loaded

(define-external-format :void (:size 0 :documentation
"Void external format that signals an error on any input or output.")
  ()
  (octets-to-code (state input unput error)
    `(error 'void-external-format))
  (code-to-octets (code state output error)
    `(error 'void-external-format)))
