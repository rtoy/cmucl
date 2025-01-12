;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/utf-16.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; From the Unicode BOM FAQ
;; (http://www.unicode.org/faq/utf_bom.html#BOM), UTF-16BE and
;; UTF-16LE should never output a BOM.  On input, since we selected
;; UTF-16BE, the input stream shouldn't have a BOM.  But we allow for
;; one here, anyway.  This should be compatible with the Unicode spec.

;; The state is a cons.  The car is an integer:
;;   0 = initial state, nothing has been read yet
;;  -2 = BOM has been read, little-endian
;;   2 = BOM has been read, big-endian, or non-BOM char has been read
;;
;; The absolute value of car specifies the size of the BOM in octets.
;; This is used in stream.lisp to account for the BOM.
;;
;; The cdr is either NIL or a codepoint which is used for converting
;; surrogate pairs into codepoints.  If the cdr is non-NIL, then it is
;; the leading (high) surrogate of a surrogate pair.
;;
;;
;; When writing, always output a BOM.

(define-external-format :utf-16 (:size 2 :documentation
"UTF-16 is a variable length character encoding for Unicode.  On
input, a byte-order mark is recognized.  If no byte-order mark is
given on input, then the encoding is assumed to be big-endian.  For
output, the byte-order mark is not written, and the output is
big-endian.  (This is specified by the Unicode standard.)

By default, illegal inputs and illegal outputs are replaced by the
Unicode replacement character.")
  ()

  (octets-to-code (state input unput error c1 c2 code wd next st)
    `(block nil
       (when (null ,state) (setf ,state (cons 0 nil)))
       (tagbody
	:again
	  (let* ((,st (car ,state))
		 (,c1 ,input)
		 (,c2 ,input)
		 (,code (if (minusp ,st)
			    ;; Little endian
			    (+ (* 256 ,c2) ,c1)
			    ;; Big endian (including BOM, if any)
			    (+ (* 256 ,c1) ,c2)))
		 (,wd 2))
	    (declare (type (member 0 -2 2) ,st)
		     (type lisp:codepoint ,code))
	    ;; Note that if BOM is read, WD will be 2 but 4 octets have
	    ;; actually been read: this is intentional - the returned
	    ;; width tells how much to back up to unread a character, and
	    ;; we don't want to back up past the BOM since the state now
	    ;; indicates that BOM has been seen, so that would result in
	    ;; the BOM being reread as a character
	    (cond ((lisp::surrogatep ,code :low)
		   ;; If possible combine this low surrogate with the
		   ;; high surrogate in the state.  Otherwise, we have
		   ;; a bare low surrogate so return the replacement
		   ;; character.
		   (if (cdr ,state)
		       (setf ,code (+ (ash (- (the (integer #xd800 #xdbff) (cdr ,state)) #xD800)
					   10)
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
		   ;; Save the high (leading) code in the state, in
		   ;; case we fail to read the low (trailing)
		   ;; surrogate.  (This should only happen when we're
		   ;; doing octets-to-string.)
		   (setf (cdr ,state) ,code)
		   (let* ((,c1 ,input)
			  (,c2 ,input)
			  (,next (if (plusp ,st)
				     (+ (* 256 ,c1) ,c2)
				     (+ (* 256 ,c2) ,c1))))
		     ;; We read the trailing surrogate, so clear the state.
		     (setf (cdr ,state) nil)
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
				     (funcall ,error "High surrogate followed by #x~4,'0X instead of low surrogate" ,next ,wd))
				   +replacement-character-code+)))))
		  ((and (= ,code #xFFFE) (zerop ,st))
		   ;; BOM for little-endian order
		   (setf (car ,state) -2) (go :again))
		  ((and (= ,code #xFEFF) (zerop ,st))
		   ;; BOM for big-endian order
		   (setf (car ,state) 2) (go :again))
		  ((= ,code #xFFFE)
		   ;; Replace with REPLACEMENT CHARACTER.  
		   (setf ,code
			 (if ,error
			     (locally
				 ;; No warnings about fdefinition
				 (declare (optimize (ext:inhibit-warnings 3)))
			       (funcall ,error "BOM is not valid within a UTF-16 stream" ,code ,wd))
			     +replacement-character-code+))))
	    (return (values ,code ,wd))))))
  (code-to-octets (code state output error c c1 c2)
    `(flet ((output (code)
	      (,output (ldb (byte 8 8) code))
	      (,output (ldb (byte 8 0) code))))
       (unless ,state
	 ;; Output BOM
	 (output #xFEFF)
	 (setf ,state t))
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
    ;; The state is list. Copy it
	      `(copy-list ,state))
  (code-to-octets (code state error)
    `(progn
       #+nil
       (unless ,state
	 ;; Output BOM
	 (output #xFEFF)
	 (setf ,state t))
       (cond ((< ,code #x10000)
	      2)
	     ((< ,code #x110000)
	      4)
	     (t
	      ;; Replacement character is 2 octets
	      2)))))
