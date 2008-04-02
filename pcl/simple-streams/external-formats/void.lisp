;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/void.lisp,v 1.1 2007/10/25 15:17:07 rtoy Exp $")
;;;
;;; **********************************************************************

;; This is actually implemented internally.  It appears here only for
;; reference, and will never get loaded.

(define-external-format :void
  (octets-to-code (state input unput)
    (declare (ignore input))
    (error 'void-external-format))
  (code-to-octets (code state output)
    (declare (ignore code))
    (error 'void-external-format)))
