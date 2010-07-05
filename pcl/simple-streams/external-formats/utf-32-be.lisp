;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-32-be.lisp,v 1.7 2010/07/05 04:12:47 rtoy Exp $")

(in-package "STREAM")

(define-external-format :utf-32-be (:size 4)
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
			  (if (>= ,code lisp:codepoint-limit)
			      (funcall ,error "Illegal codepoint #x~4,'0X" ,code 4)
			      (funcall ,error "Surrogate #x~4,'0X not allowed in UTF32" ,code 4))
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
       ;; Write BOM
       (unless ,state
	 (out #xFEFF)
	 (setf ,state t))
       (cond ((lisp::surrogatep ,code)
	      (out (if ,error
		       (funcall ,error "Surrogate code #x~4,'0X is illegal for UTF32 output"
				,code)
		       +replacement-character-code+)))
	     (t
	      (out ,code))))))
