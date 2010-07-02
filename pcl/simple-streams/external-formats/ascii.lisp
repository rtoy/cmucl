;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/ascii.lisp,v 1.2 2010/07/02 23:13:11 rtoy Exp $")

(in-package "STREAM")

(define-external-format :ascii (:size 1)
  ()
  (octets-to-code (state input unput error c)
    `(let ((,c ,input))		  
       (values (if (< ,c #x80)
		   ,c
		   (if ,error
		       (funcall ,error "Invalid octet #x~4,0X for ASCII" ,c 1)
		       +replacement-character-code+))
	       1))
  (code-to-octets (code state output error)
    `(,output (if (> ,code #x7F)
		  (if ,error
		      (funcall ,error "Cannot output codepoint #x~X to ASCII stream" ,code)
		      #x3F)
		  ,code)))))

