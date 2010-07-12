;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/void.lisp,v 1.5 2010/07/12 13:58:42 rtoy Exp $")

;; This is actually implemented in the external-formats code
;; It appears here only for reference, and will never get loaded

(define-external-format :void (:size 0 :documentation
"Void external format that signals an error on any input or output.")
  ()
  (octets-to-code (state input unput error)
    `(error 'void-external-format))
  (code-to-octets (code state output error)
    `(error 'void-external-format)))
