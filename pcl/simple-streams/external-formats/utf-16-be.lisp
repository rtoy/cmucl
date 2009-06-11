;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-16-be.lisp,v 1.2 2009/06/11 16:04:02 rtoy Rel $")

(in-package "STREAM")

;; UTF-16BE.  BOM is not recognized and is never output.

(define-external-format :utf-16-be (:size 2)
  ()

  (octets-to-code (state input unput c1 c2 code next)
    `(let* ((,c1 ,input)
	    (,c2 ,input)
	    (,code (+ (* 256 ,c1) ,c2)))
       (declare (type (integer 0 #xffff) ,code))
       (cond ((lisp::surrogatep ,code :low)
	      (setf ,code +replacement-character-code+))
	     ((lisp::surrogatep ,code :high)
	      (let* ((,c1 ,input)
		     (,c2 ,input)
		     (,next (+ (* 256 ,c1) ,c2)))
		;; If we don't have a high and low surrogate,
		;; replace with REPLACEMENT CHARACTER.  Possibly
		;; unput 2 so it'll be read as another character
		;; next time around?
		(if (lisp::surrogatep ,next :low)
		    (setq ,code (+ (ash (- ,code #xD800) 10) ,next #x2400))
		    (setf ,code +replacement-character-code+))))
	     ((= ,code #xFFFE)
	      ;; Replace with REPLACEMENT CHARACTER.  
	      (setf ,code +replacement-character-code+)))
       (values ,code 2)))
  (code-to-octets (code state output c c1 c2)
    `(flet ((output (code)
	      (,output (ldb (byte 8 8) code))
	      (,output (ldb (byte 8 0) code))))
       (cond ((< ,code #x10000)
	      (output ,code))
	     ((< ,code #x110000)
	      (let* ((,c (- ,code #x10000))
		     (,c1 (ldb (byte 10 10) ,c))
		     (,c2 (ldb (byte 10 0) ,c)))
		(output (logior ,c1 #xD800))
		(output (logior ,c2 #xDC00))))
	     (t
	      (output +replacement-character-code+))))))
