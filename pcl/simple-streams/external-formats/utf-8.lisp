;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-8.lisp,v 1.2.4.1 2008/06/19 03:30:45 rtoy Exp $")

(define-external-format :utf-8
  (octets-to-code (state input unput c i j n)
    `(flet ((utf8 (,c ,i)
	      (let ((,n (ash (ldb (byte (- 6 ,i) 0) ,c) (* 6 ,i))))
		(dotimes (,j ,i (values ,n (1+ ,i)))
		  (setf (ldb (byte 6 (* 6 (- ,i ,j 1))) ,n)
		      (ldb (byte 6 0) ,input))))))
       (let ((,c ,input))
	 (cond ((null ,c) (values nil 0))
	       ((< ,c #b10000000) (values ,c 1))
	       ((< ,c #b11000000) (error "UTF-8 desync"))
	       ((< ,c #b11100000) (utf8 ,c 1))
	       ((< ,c #b11110000) (utf8 ,c 2))
	       ((< ,c #b11111000) (utf8 ,c 3))
	       ((< ,c #b11111100) (utf8 ,c 4))
	       ((< ,c #b11111110) (utf8 ,c 5))
	       (t (error "Invalid UTF-8 character"))))))
  (code-to-octets (code state output i j n p init)
    `(flet ((utf8 (,n ,i)
	      (let* ((,j (- 6 ,i))
		     (,p (* 6 ,i))
		     (,init (logand #xFF (ash #b01111110 ,j))))
		(,output (logior ,init (ldb (byte ,j ,p) ,n)))
		(dotimes (,i ,i)
		  (decf ,p 6)
		  (,output (logior 128 (ldb (byte 6 ,p) ,n)))))))
       (cond ((< ,code #x80) (,output ,code))
	     ((< ,code #x800) (utf8 ,code 1))
	     ((< ,code #x10000) (utf8 ,code 2))
	     ((< ,code #x200000) (utf8 ,code 3))
	     ((< ,code #x4000000) (utf8 ,code 4))
	     (t (utf8 ,code 5))))))
