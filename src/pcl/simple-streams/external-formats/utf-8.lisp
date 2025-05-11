;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/utf-8.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; This is actually implemented in the external-formats code
;; It appears here only for reference, and will never get loaded


;; A safe UTF-8 external format.  Any illegal UTF-8 sequences on input
;; are replaced with the Unicode REPLACEMENT CHARACTER (U+FFFD), or
;; signals an error as appropriate.
;;
;; See Table 3-7, Ch 3.9 in the Unicode book.

(define-external-format :utf-8 (:min 1 :max 4 :documentation 
"UTF-8 is a variable-length character encoding for Unicode.  By
default, illegal input sequences are replaced by the Unicode
replacement character.")

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
			    (return
			      (values
			       (locally
				   ;; No warnings about fdefinition
				   (declare (optimize (ext:inhibit-warnings 3)))
				 (if ,error
				     (funcall ,error "Invalid utf8 octet #x~X at offset ~D"
					      ,c (1+ ,j))
				     +replacement-character-code+))
			       (1+ ,j)))))))))
	      (check (,n ,i)
		(declare (type (unsigned-byte 31) ,n)
			 (type (integer 1 5) ,i))
		;; We check for overlong sequences (sequences that
		;; encode to codepoints that don't need that long of a
		;; sequence) and any surrogate values and any code
		;; outside the 21-bit Unicode range.
		(if (or (>= ,n lisp:codepoint-limit)
			(<= ,n (the (member 127 2047 65535)
				 (svref #(127 2047 65535) (1- ,i)))) ; overlong
			(lisp::surrogatep ,n)) ; surrogate
		    (progn
		      ;; Replace the entire sequence with the
		      ;; replacment character
		      (values (if ,error
				  (cond
				    ((>= ,n lisp:codepoint-limit)
				     (locally
					 ;; No warnings about fdefinition
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error "Invalid codepoint #x~X of ~D octets"
						,n (1+ ,i))))
				    ((lisp::surrogatep ,n)
				     (locally
					 ;; No warnings about fdefinition
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error "Invalid surrogate code #x~X" ,n (1+ ,i))))
				    (t
				     (locally
					 ;; No warnings about fdefinition
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error "Overlong utf8 sequence of ~*~D octets" nil (1+ ,i)))))
				  +replacement-character-code+)
			      (1+ ,i)))
		    (values ,n (1+ ,i)))))
      (let ((,c ,input))
	(declare (optimize (ext:inhibit-warnings 3)))
	(cond ((null ,c) (values nil 0))
	      ((< ,c #b10000000) (values ,c 1))
	      ((< ,c #b11000010)
	       (values
		(locally
		    ;; No warnings about fdefinition
		    (declare (optimize (ext:inhibit-warnings 3)))
		  (if ,error
		      (funcall ,error "Invalid initial utf8 octet: #x~X" ,c 1)
		      +replacement-character-code+))
		       1))
	      ((< ,c #b11100000) (utf8 ,c 1))
	      ((< ,c #b11110000) (utf8 ,c 2))
	      ((< ,c #b11111000) (utf8 ,c 3))
	      (t
	       (values
		(locally
		    ;; No warnings about fdefinition
		    (declare (optimize (ext:inhibit-warnings 3)))
		  (if ,error
		      (funcall ,error "Invalid initial utf8 octet: #x~X" ,c 1)
		      +replacement-character-code+))
		1))))))
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
         (t (error "How did this happen?  Codepoint U+~X is illegal" ,code)))))
  ()
  ()
  (octet-count (code state error)
    `(locally
	 (declare (optimize (ext:inhibit-warnings 3)))
       (cond ((< ,code #x80) 1)
             ((< ,code #x800) 2)
             ((< ,code #x10000) 3)
             ((< ,code #x110000) 4)
             (t (error "How did this happen?  Codepoint U+~X is illegal" ,code))))))
