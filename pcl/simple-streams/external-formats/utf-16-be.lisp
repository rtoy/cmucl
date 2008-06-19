;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-16-be.lisp,v 1.1 2008/06/19 01:41:34 rtoy Exp $")

;; make state an integer:
;;  or (or state 0) to cope with NIL case
;;  0 = initial state, nothing has been read yet
;;  1 = BOM has been read, little-endian
;;  2 = BOM has been read, big-endian, or non-BOM char has been read
;;
;; (oddp state) = little-endian
;; (evenp state) = big-endian
;; (zerop state) = #xFEFF/#xFFFE is BOM (to be skipped)
;;
;; when writing: output BOM if (zerop state), if BOM-output selected
;; but state doesn't choose between big-endian and little-endian output

(define-external-format :utf-16-be
  (octets-to-code (state input unput c1 c2 code wd next)
    `(block nil
       (when (null ,state) (setf ,state 0))
       (tagbody
	:again
	  (let* ((,c1 ,input)
		 (,c2 ,input)
		 (,code (if (oddp ,state)
			    (+ (* 256 ,c2) ,c1)
			    (+ (* 256 ,c1) ,c2)))
		 (,wd 2))
	    ;; Note that if BOM is read, WD will be 2 but 4 octets have
	    ;; actually been read: this is intentional - the returned
	    ;; width tells how much to back up to unread a character, and
	    ;; we don't want to back up past the BOM since the state now
	    ;; indicates that BOM has been seen, so that would result in
	    ;; the BOM being reread as a character
	    (cond ((<= #xDC00 ,code #xDFFF)
		   ;; replace with REPLACEMENT CHARACTER ?
		   (error "Illegal character U+~4,'0X in UTF-16 sequence."
			  ,code))
		  ((<= #xD800 ,code #xDBFF)
		   (let* ((,c1 ,input)
			  (,c2 ,input)
			  (,next (if (oddp ,state)
				     (+ (* 256 ,c2) ,c1)
				     (+ (* 256 ,c1) ,c2))))
		     (unless (<= #xDC00 ,next #xDFFF)
		       ;; replace with REPLACEMENT CHARACTER ?
		       ;; possibly unput 2 so it'll be read as
		       ;; another character next time around?
		       (error "Illegal surrogate pair U+~4,'0X U+~4,'0X ~
			       in UTF-16 sequence." ,code ,next))
		     (setq ,code (+ (ash (- ,code #xD800) 10) ,next #x2400)
			   ,wd 4)))
		  ((and (= ,code #xFFFE) (zerop ,state))
		   (setf ,state 1) (go :again))
		  ((and (= ,code #xFEFF) (zerop ,state))
		   (setf ,state 2) (go :again))
		  ((= ,code #xFFFE)
		   ;; replace with REPLACEMENT CHARACTER ?
		   (error "Illegal character U+FFFE in UTF-16 sequence.")))
	    (return (values ,code ,wd))))))
  (code-to-octets (code state output c c1 c2)
    `(flet ((output (code)
	      (,output (ldb (byte 8 8) code))
	      (,output (ldb (byte 8 0) code))))
       (when (and (null ,state) #|BOM wanted|#t)
	 (output #xFEFF) (setf ,state 2))
       (cond ((< ,code #x10000)
	      (output ,code))
	     ((< ,code #x110000)
	      (let* ((,c (- ,code #x10000))
		     (,c1 (ldb (byte 10 10) ,c))
		     (,c2 (ldb (byte 10 0) ,c)))
		(output (logior ,c1 #xD800))
		(output (logior ,c2 #xDC00))))
	     (t
	      (output #xFFFD))))))
