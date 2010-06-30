;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-16-le.lisp,v 1.5 2010/06/30 04:02:53 rtoy Exp $")

(in-package "STREAM")

;; UTF-16LE.  BOM is not recognized, and is never output.
;;
;; The state is either NIL or a codepoint.
(define-external-format :utf-16-le (:size 2)
  ()

  (octets-to-code (state input unput error c1 c2 code next)
    `(let* ((,c1 ,input)
	    (,c2 ,input)
	    (,code (+ (* 256 ,c2) ,c1)))
       (declare (type lisp:codepoint ,code))
       (cond ((lisp::surrogatep ,code :low)
	      ;; If possible combine this low surrogate with the
	      ;; high surrogate in the state.  Otherwise, we have
	      ;; a bare low surrogate so return the replacement
	      ;; character.
	      (if ,state
		  (setf ,code (+ (ash (- (the (integer #xd800 #xdbff) ,state) #xD800) 10)
				 ,code #x2400)
			,state nil)
		  (setf ,code +replacement-character-code+)))
	     ((lisp::surrogatep ,code :high)
	      ;; Remember the high surrogate in case we bail out
	      ;; reading the low surrogate (for octets-to-string.)
	      (setf ,state ,code)
	      (let* ((,c1 ,input)
		     (,c2 ,input)
		     (,next (+ (* 256 ,c2) ,c1)))
		;; We read the trailing code, so clear the state.
		(setf ,state nil)
		;; Replace with REPLACEMENT CHARACTER.  Possibly
		;; unput 2 so it'll be read as another character
		;; next time around?
		(if (lisp::surrogatep ,next :low)
		    (setq ,code (+ (ash (- ,code #xD800) 10) ,next #x2400))
		    (setq ,code +replacement-character-code+))))
	     ((= ,code #xFFFE)
	      ;; replace with REPLACEMENT CHARACTER ?
	      (error "Illegal character U+FFFE in UTF-16 sequence."))
	     (t (setf ,state nil)))
      (values ,code 2)))
  (code-to-octets (code state output error c c1 c2)
    `(flet ((output (code)
	      (,output (ldb (byte 8 0) code))
	     (,output (ldb (byte 8 8) code))))
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
  nil
  (copy-state (state)
    ;; The state is either NIL or a codepoint, so nothing really
    ;; special is needed.
    `(progn ,state)))
