;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/utf-32.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; UTF-32, with unspecified endianness.  We default to big-endian (as
;; specified), but we do recognize the BOM to specify endianness.  We
;; also output the BOM (for big-endian output), although we don't have
;; to.  This allows the user to have a way of outputting the BOM,
;; since UTF-32LE and UTF-32BE are specified not to output the BOM.
;;
;; This is modeled after the utf-16 format.

;; The state is a cons.  The car is an integer:
;;   0 = initial state, nothing has been read yet
;;  -4 = BOM has been read, little-endian
;;   4 = BOM has been read, big-endian, or non-BOM char has been read
;;
;; (minusp state) = little-endian
;; (plusp state) = big-endian
;; (zerop state) = #xFEFF/#xFFFE is BOM (to be skipped)
;;
;; The absolute value of the car specifies the size of the BOM in
;; octets.  This is used in stream.lisp to account for the BOM.
;;
(define-external-format :utf-32 (:size 4 :documentation
"UTF-32 is a fixed-length character encoding of 4 octets for Unicode.
On input, a byte-order mark is recognized.  If no byte-order mark is
given on input, then the encoding is assumed to be big-endian.  For
output, the byte-order mark is not written, and the output is
big-endian.  (This is specified by the Unicode standard.)

By default, illegal inputs and illegal outputs are replaced by the
Unicode replacement character.")
  ()

  (octets-to-code (state input unput error code c1 c2 c3 c4 st wd)
    `(block nil
       (when (null ,state) (setf ,state (cons 0 nil)))
       (tagbody
	:again
	  (let* ((,st (car ,state))
		 (,c1 ,input)
		 (,c2 ,input)
		 (,c3 ,input)
		 (,c4 ,input)
		 (,code (if (minusp ,st)
			    ;; Little-endian
			    (+ ,c1
			       (ash ,c2 8)
			       (ash ,c3 16)
			       (ash ,c4 24))
			    ;; Big-endian (including BOM, if any)
			    (+ (ash ,c1 24)
			       (ash ,c2 16)
			       (ash ,c3  8)
			       ,c4)))
		 (,wd 4))
	    (declare (type (member 0 -4 4) ,st)
		     (type (unsigned-byte 8) ,c1 ,c2 ,c3 ,c4)
		     (type (unsigned-byte 32) ,code)
		     (optimize (speed 3)))
	    (cond ((or (>= ,code lisp:codepoint-limit)
		       (lisp::surrogatep ,code))
		   ;; Surrogates are illegal and codepoints outside
		   ;; the Unicode range are illegal.  Use replacement
		   ;; character.
		   (setf ,code
			 (if ,error
			     (locally
				 ;; No warnings about fdefinition
				 (declare (optimize (ext:inhibit-warnings 3)))
			       (if (>= ,code lisp:codepoint-limit)
				   (funcall ,error "Illegal codepoint #x~4,'0X" ,code 4)
				   (funcall ,error "Surrogate #x~4,'0X not allowed in UTF32"
					    ,code 4)))
			     +replacement-character-code+)))
		  ((and  (zerop ,st) (= ,code #xFFFE0000))
		   ;; BOM for little-endian
		   (setf (car ,state) -4)
		   (go :again))
		  ((and (zerop ,st) (= ,code #xFEFF))
		   ;; BOM for big-endian
		   (setf (car ,state) 4)
		   (go :again)))
	    (return (values ,code ,wd))))))

  (code-to-octets (code state output error i c)
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
		       (locally
			   ;; No warnings about fdefinition
			   (declare (optimize (ext:inhibit-warnings 3)))
			 (funcall ,error "Surrogate code #x~4,'0X is illegal for UTF32 output"
				  ,code))
		       +replacement-character-code+)))
	     (t
	      (out ,code)))))
  nil
  (copy-state (state)
    ;; The state is either NIL or T, so we can just return that.
    `(progn ,state))
  (octet-count (code state error)
    `(let ((bom-count 0))
       (unless ,state
	 (setf bom-count 4)
	 (setf ,state t))
       (cond ((lisp::surrogatep ,code)
	      (if ,error
		  (locally
		      ;; No warnings about fdefinition
		      (declare (optimize (ext:inhibit-warnings 3)))
		    (funcall ,error "Surrogate code #x~4,'0X is illegal for UTF32 output"
			     ,code))
		  ;; Replacement character is 2 octets
		  (+ 2 bom-count)))
	     (t
	      (+ 4 bom-count))))))
