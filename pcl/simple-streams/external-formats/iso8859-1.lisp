;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/iso8859-1.lisp,v 1.4 2010/06/30 04:02:53 rtoy Exp $")

(in-package "STREAM")

;; This is actually implemented in the external-formats code
;; It appears here only for reference, and will never get loaded

(define-external-format :iso8859-1 (:size 1)
  ()

  (octets-to-code (state input unput error)
    (values ,input 1))
  (code-to-octets (code state output error)
    (,output (if (> ,code 255) #x3F ,code))))
