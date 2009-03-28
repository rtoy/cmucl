;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/iso8859-1.lisp,v 1.1.4.1.2.2 2009/03/28 13:40:41 rtoy Exp $")

(in-package "STREAM")

;; This is actually implemented in the external-formats code
;; It appears here only for reference, and will never get loaded

(define-external-format :iso8859-1 (:size 1)
  ()

  (octets-to-code (state input unput)
    (values ,input 1))
  (code-to-octets (code state output)
    (,output (if (> ,code 255) #x3F ,code))))
