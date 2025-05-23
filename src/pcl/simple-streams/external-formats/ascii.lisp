;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/ascii.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(define-external-format :ascii (:size 1 :documentation 
"US ASCII 7-bit encoding.  Illegal input sequences are replaced with
the Unicode replacment character.  Illegal output characters are
replaced with a question mark.")
  ()
  (octets-to-code (state input unput error c)
    `(let ((,c ,input))		  
       (values (if (< ,c #x80)
		   ,c
		   (if ,error
		       (locally
			   ;; No warnings about fdefinition
			   (declare (optimize (ext:inhibit-warnings 3)))
			 (funcall ,error "Invalid octet #x~4,'0X for ASCII" ,c 1))
		       +replacement-character-code+))
	       1)))
  (code-to-octets (code state output error)
    `(,output (if (> ,code #x7F)
		  (if ,error
		      (locally
			  ;; No warnings about fdefinition
			  (declare (optimize (ext:inhibit-warnings 3)))
			(funcall ,error "Cannot output codepoint #x~X to ASCII stream" ,code))
		      #x3F)
		  ,code)))
  ()
  ()
  (octet-count (code state error)
    `(if (> ,code #x7f)
	 (if ,error
	     (locally
		 (declare (optimize (ext:inhibit-warnings 3)))
	       (funcall ,error "Cannot output codepoint #x~X to ASCII stream" ,code))
	     1)
	 1)))

