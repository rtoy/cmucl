;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-8.lisp,v 1.6 2010/06/30 04:02:53 rtoy Exp $")

(in-package "STREAM")

;; A safe UTF-8 external format.  Any illegal UTF-8 sequences on input
;; are replaced with the Unicode REPLACEMENT CHARACTER (U+FFFD).
;;
;; See Table 3-7, Ch 3.9 in the Unicode book.

(define-external-format :utf-8 (:min 1 :max 4)
  ()
  (octets-to-code (state input unput error c i j n)
    `(labels ((utf8 (,c ,i)
	       (declare (type (unsigned-byte 8) ,c)
			(type (integer 1 5) ,i))
	       (let ((,n (ash (ldb (byte (- 6 ,i) 0) ,c)
			      (* 6 ,i))))
		 (declare (type (unsigned-byte 31) ,n))
		 (dotimes (,j ,i (check ,n ,i))
		   (let ((,c ,input))
		     ;; Following bytes must all have the form
		     ;; #b10xxxxxx.  If not, put back the octet we
		     ;; just read and return the replacement character
		     ;; for the bad sequence.
		     (if (< (logxor ,c #x80) #x40)
			 (setf (ldb (byte 6 (* 6 (- ,i ,j 1))) ,n)
			       (ldb (byte 6 0) ,c))
			 (progn
			   (,unput 1)
			   (return (values +replacement-character-code+ (1+ ,j)))))))))
	      (check (,n ,i)
	       (declare (type (unsigned-byte 31) ,n)
			(type (integer 1 5) ,i))
	       ;; We check for overlong sequences (sequences that
	       ;; encode to codepoints that don't need that long of a
	       ;; sequence) and any surrogate values and any code
	       ;; outside the 21-bit Unicode range.
	       (if (or (> ,n #x10ffff)
		       (<= ,n (the (member 127 1023 32767)
				(svref #(127 1023 32767) (1- ,i)))) ; overlong
		       (lisp::surrogatep ,n)) ; surrogate
		   (progn
		     (,unput ,i)
		     (values +replacement-character-code+ 1))
		   (values ,n (1+ ,i)))))
      (let ((,c ,input))
	(declare (optimize (ext:inhibit-warnings 3)))
	(cond ((null ,c) (values nil 0))
	      ((< ,c #b10000000) (values ,c 1))
	      ((< ,c #b11000010) (values +replacement-character-code+ 1))
	      ((< ,c #b11100000) (utf8 ,c 1))
	      ((< ,c #b11110000) (utf8 ,c 2))
	      ((< ,c #b11111000) (utf8 ,c 3))
	      (t (values +replacement-character-code+ 1))))))
  (code-to-octets (code state output error i j n p init)
    `(flet ((utf8 (,n ,i)
          (let* ((,j (- 6 ,i))
             (,p (* 6 ,i))
             (,init (logand #xFF (ash #b01111110 ,j))))
        (,output (logior ,init (ldb (byte ,j ,p) ,n)))
        (dotimes (,i ,i)
          (decf ,p 6)
          (,output (logior 128 (ldb (byte 6 ,p) ,n)))))))
       (declare (optimize (ext:inhibit-warnings 3)))
       (cond ((< ,code #x80) (,output ,code))
         ((< ,code #x800) (utf8 ,code 1))
         ((< ,code #x10000) (utf8 ,code 2))
         ((< ,code #x110000) (utf8 ,code 3))
         (t (error "How did this happen?  Codepoint U+~X is illegal" ,code))))))
