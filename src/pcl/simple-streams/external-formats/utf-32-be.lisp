;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/utf-32-be.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(define-external-format :utf-32-be (:size 4 :documentation
"UTF-32-BE is a fixed-length character encoding of 4 octets for
Unicode.  For both input and output, the data is assumed to be in
big-endian order.  No byte-order mark is allowed on input, and no
byte-order mark is produced on output.  (This is specified by the
Unicode standard.)

By default, illegal inputs and illegal outputs are replaced by the
Unicode replacement character.")
  ()

  (octets-to-code (state input unput error c c1 c2 c3 c4)
    `(let* ((,c1 ,input)
	    (,c2 ,input)
	    (,c3 ,input)
	    (,c4 ,input)
	    (,c (+ (ash ,c1 24)
		   (ash ,c2 16)
		   (ash ,c3  8)
		   ,c4)))
       (declare (type (unsigned-byte 8) ,c1 ,c2 ,c3 ,c4)
		(optimize (speed 3)))
       (cond ((or (>= ,c lisp:codepoint-limit)
		  (lisp::surrogatep ,c))
	      ;; Surrogates are illegal.  Use replacement character.
	      (values (if ,error
			  (locally
			      ;; No warnings about fdefinition
			      (declare (optimize (ext:inhibit-warnings 3)))
			    (if (>= ,c lisp:codepoint-limit)
				(funcall ,error "Illegal codepoint #x~4,'0X" ,c 4)
				(funcall ,error "Surrogate #x~4,'0X not allowed in UTF32" ,c 4)))
			  +replacement-character-code+)
		      4))
	     (t
	      (values ,c 4)))))

  (code-to-octets (code state output error c i)
    `(flet ((out (,c)
	      (declare (type (unsigned-byte 32) ,c))
	      ;; Big-endian output
	      (dotimes (,i 4)
		(,output (ldb (byte 8 (* 8 (- 3 ,i))) ,c)))))
       (cond ((lisp::surrogatep ,code)
	      (out (if ,error
		       (locally
			   ;; No warnings about fdefinition
			   (declare (optimize (ext:inhibit-warnings 3)))
			 (funcall ,error "Surrogate code #x~4,'0X is illegal for UTF32 output"
				  ,code))
		       +replacement-character-code+)))
	     (t
	      (out ,code)))))
  ()
  ()
  (code-to-octets (code state error)
    `(cond ((lisp::surrogatep ,code)
	    (if ,error
		(locally
		    ;; No warnings about fdefinition
		    (declare (optimize (ext:inhibit-warnings 3)))
		  (funcall ,error "Surrogate code #x~4,'0X is illegal for UTF32 output"
			   ,code))
		;; Replacement character is 2 octets
		2))
	   (t
	    4)))))
