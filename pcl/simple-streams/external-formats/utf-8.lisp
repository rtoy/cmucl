;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-8.lisp,v 1.2 2007/10/25 15:17:07 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; A UTF-8 external format for simple-streams

(in-package "STREAM")
(define-external-format :utf-8
  (octets-to-code (state input unput)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (flet ((utf8 (c i)
	     (let ((n (ash (ldb (byte (- 6 i) 0) c) (* 6 i))))
	       (dotimes (j i (values n (1+ i) nil))
		 (setf (ldb (byte 6 (* 6 (- i j 1))) n)
		     (ldb (byte 6 0) (funcall input)))))))
      (let ((c (funcall input)))
	(cond ((< c #b10000000) (values c 1 nil))
	      ((< c #b11000000) (error "UTF-8 desync"))
	      ((< c #b11100000) (utf8 c 1))
	      ((< c #b11110000) (utf8 c 2))
	      ((< c #b11111000) (utf8 c 3))
	      ((< c #b11111100) (utf8 c 4))
	      ((< c #b11111110) (utf8 c 5))
	      (t (error "Invalid UTF-8 character"))))))
  (code-to-octets (code state output)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (flet ((utf8 (n i)
	     (let* ((j (- 6 i))
		    (p (* 6 i))
		    (init (logand #xFF (ash #b01111110 j))))
	       (funcall output (logior init (ldb (byte j p) n)))
	       (dotimes (i i)
		 (decf p 6)
		 (funcall output (logior 128 (ldb (byte 6 p) n)))))))
      (cond ((< code #x80) (funcall output code))
	    ((< code #x800) (utf8 code 1))
	    ((< code #x10000) (utf8 code 2))
	    ((< code #x200000) (utf8 code 3))
	    ((< code #x4000000) (utf8 code 4))
	    (t (utf8 code 5))))
    nil))
