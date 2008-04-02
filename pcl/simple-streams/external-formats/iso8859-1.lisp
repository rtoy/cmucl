;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/iso8859-1.lisp,v 1.1 2007/10/25 15:17:07 rtoy Exp $")
;;;
;;; **********************************************************************

;; This is actually implemented internally.  It appears here only for
;; reference, and will never get loaded.

(define-external-format :iso8859-1
  (octets-to-code (state input unput)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (values (funcall input) 1 nil))
  (code-to-octets (code state output)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (funcall output (if (> code 255) #x3F code))
    nil))
