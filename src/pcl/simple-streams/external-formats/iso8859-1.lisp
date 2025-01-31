;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-1.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; This is actually implemented in the external-formats code
;; It appears here only for reference, and will never get loaded

(define-external-format :iso8859-1 (:size 1 :documentation
"ISO8859-1 is an 8-bit character encoding generally intended for
Western European languages including English, German, Italian,
Norwegian, Portuguese, Spanish, Swedish and many others.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ()
  (octets-to-code (state input unput error)
    `(values ,input 1))
  (code-to-octets (code state output error)
    `(,output (if (> ,code 255)
		  (if ,error
		   (locally
		       ;; No warnings about fdefinition
		       (declare (optimize (ext:inhibit-warnings 3)))
		     (funcall ,error "Cannot output codepoint #x~X to ISO8859-1 stream"
			      ,code 1))
		      #x3F)
		  ,code)))
  ()
  ()
  (octet-count (code state error)
    `(if (> ,code 255)
	 (if ,error
	     (locally
		 ;; No warnings about fdefinition
		 (declare (optimize (ext:inhibit-warnings 3)))
	       (funcall ,error
			(intl:gettext "Cannot output codepoint #x~X to ISO8859-1 stream")
			,code 1))
	     1)
	 1)))
