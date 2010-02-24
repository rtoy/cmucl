;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/ascii.lisp,v 1.1 2009/11/10 15:31:43 rtoy Exp $")

(in-package "STREAM")

(define-external-format :ascii (:size 1)
  ()
  (octets-to-code (state input unput c)
    `(let ((,c ,input))		  
       (values (if (< ,c #x80) ,c +replacement-character-code+)
	       1))
  (code-to-octets (code state output)
    `(,output (if (> ,code #x7F) #x3F ,code))))

