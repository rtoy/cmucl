;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/utf-16-be.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; UTF-16BE.  BOM is not recognized and is never output.

(define-external-format :utf-16-be (:size 2 :documentation
"UTF-16-BE is a variable length character encoding for Unicode.  For
both input and output, the data is assumed to be in big-endian order.
No byte-order mark is allowed on input, and no byte-order mark is
produced on output.  (This is specified by the Unicode standard.)

By default, illegal inputs and illegal outputs are replaced by the
Unicode replacement character.")
  ()

  (octets-to-code (state input unput error c1 c2 code wd next)
    `(let* ((,c1 ,input)
	    (,c2 ,input)
	    (,code (+ (* 256 ,c1) ,c2))
	    (, wd 2))
       (declare (type lisp:codepoint ,code))
       (cond ((lisp::surrogatep ,code :low)
	      ;; Got low surrogate.  Combine with the state (high
	      ;; surrogate), if we have it.  Otherwise, use the
	      ;; replacement code.
	      (if ,state
		  (setf ,code (+ (ash (- (the (integer #xd800 #xdbff) ,state) #xD800) 10)
				 ,code #x2400)
			,state nil)
		  (setf ,code
			(if ,error
			    (locally
				;; No warnings about fdefinition
				(declare (optimize (ext:inhibit-warnings 3)))
			      (funcall ,error "Bare low surrogate #x~4,'0X" ,code 2))
			    +replacement-character-code+))))
	     ((lisp::surrogatep ,code :high)
	      ;; Remember the high surrogate in case we bail out
	      ;; reading the low surrogate (for octets-to-string.)
	      (setf ,state ,code)
	      (let* ((,c1 ,input)
		     (,c2 ,input)
		     (,next (+ (* 256 ,c1) ,c2)))
		;; We read the trailing code, so clear the state.
		(setf ,state nil)
		;; If we don't have a high and low surrogate,
		;; replace with REPLACEMENT CHARACTER.  Possibly
		;; unput 2 so it'll be read as another character
		;; next time around?
		(if (lisp::surrogatep ,next :low)
		    (setq ,code (+ (ash (- ,code #xD800) 10) ,next #x2400)
			  ,wd 4)
		    (setf ,code
			  (if ,error
			      (locally
				  ;; No warnings about fdefinition
				  (declare (optimize (ext:inhibit-warnings 3)))
				(funcall ,error "High surrogate followed by #x~4,'0X ~
                                                 instead of low surrogate" ,next 2))
			      +replacement-character-code+)))))
	     ((= ,code #xFFFE)
	      ;; Replace with REPLACEMENT CHARACTER.  
	      (setf ,code
		    (if ,error
			(locally
			    ;; No warnings about fdefinition
			    (declare (optimize (ext:inhibit-warnings 3)))
			  (funcall ,error "BOM is not valid within a UTF-16 stream" ,code 2))
			+replacement-character-code+)))
	     (t (setf ,state nil)))
       (values ,code ,wd)))
  (code-to-octets (code state output error c c1 c2)
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
	      (output +replacement-character-code+)))))
  (flush-state (state output error c)
    `(flet ((out (code)
	      (,output (ldb (byte 8 8) code))
	      (,output (ldb (byte 8 0) code))))
       (let ((,c (car ,state)))
	 (when ,c
	   (out (if (lisp::surrogatep ,c)
		    (if ,error
			(locally
			    ;; No warnings about fdefinition
			    (declare (optimize (ext:inhibit-warnings 3)))
			  (funcall ,error
				   "Flushing bare surrogate #x~4,'0X is illegal for UTF-16"
				   (char-code ,c)))
			+replacement-character-code+)
		    ,c))))))
  (copy-state (state)
    ;; The state is either NIL or a codepoint, so nothing really
    ;; special is needed to copy it.
    `(progn ,state))
  (octet-count (code state error)
    `(cond ((< ,code #x10000)
	    2)
	   ((< ,code #x110000)
	    4)
	   (t
	    ;; Replacement character is 2 octets
	    2))))
