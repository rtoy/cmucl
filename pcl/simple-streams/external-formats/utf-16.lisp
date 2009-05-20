;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-16.lisp,v 1.1.2.3 2009/05/20 21:47:37 rtoy Exp $")

(in-package "STREAM")

;; From the Unicode BOM FAQ
;; (http://www.unicode.org/faq/utf_bom.html#BOM), UTF-16BE and
;; UTF-16LE should never output a BOM.  On input, since we selected
;; UTF-16BE, the input stream shouldn't have a BOM.  But we allow for
;; one here, anyway.  This should be compatible with the Unicode spec.

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
;; When writing, never output a BOM.

(define-external-format :utf-16 (:size 2)
  ()

  (octets-to-code (state input unput c1 c2 code wd next st)
    `(block nil
       (when (null ,state) (setf ,state 0))
       (tagbody
	:again
	  (let* ((,st ,state)
		 (,c1 ,input)
		 (,c2 ,input)
		 (,code (if (oddp ,st)
			    (+ (* 256 ,c2) ,c1)
			    (+ (* 256 ,c1) ,c2)))
		 (,wd 2))
	    (declare (type (integer 0 2) ,st)
		     (type (integer 0 #xffff) ,code))
	    ;; Note that if BOM is read, WD will be 2 but 4 octets have
	    ;; actually been read: this is intentional - the returned
	    ;; width tells how much to back up to unread a character, and
	    ;; we don't want to back up past the BOM since the state now
	    ;; indicates that BOM has been seen, so that would result in
	    ;; the BOM being reread as a character
	    (cond ((lisp::surrogatep ,code :low)
		   ;; replace with REPLACEMENT CHARACTER ?
		   (setf ,code #xFFFD))
		  ((lisp::surrogatep ,code :high)
		   (let* ((,c1 ,input)
			  (,c2 ,input)
			  (,next (if (oddp ,st)
				     (+ (* 256 ,c2) ,c1)
				     (+ (* 256 ,c1) ,c2))))
		     ;; If we don't have a high and low surrogate,
		     ;; replace with REPLACEMENT CHARACTER.  Possibly
		     ;; unput 2 so it'll be read as another character
		     ;; next time around?
		     (if (lisp::surrogatep ,next :low)
			 (setq ,code (+ (ash (- ,code #xD800) 10) ,next #x2400)
			       ,wd 4)
			 (setf ,code #xFFFD))))
		  ((and (= ,code #xFFFE) (zerop ,st))
		   (setf ,state 1) (go :again))
		  ((and (= ,code #xFEFF) (zerop ,st))
		   (setf ,state 2) (go :again))
		  ((= ,code #xFFFE)
		   ;; Replace with REPLACEMENT CHARACTER.  
		   (setf ,code #xFFFD)))
	    (return (values ,code ,wd))))))
  (code-to-octets (code state output c c1 c2)
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
	      (output #xFFFD))))))
