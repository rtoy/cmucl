;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-32.lisp,v 1.9 2010/07/05 04:12:47 rtoy Exp $")

(in-package "STREAM")

;; UTF-32, with unspecified endianness.  We default to big-endian (as
;; specified), but we do recognize the BOM to specify endianness.  We
;; also output the BOM (for big-endian output), although we don't have
;; to.  This allows the user to have a way of outputting the BOM,
;; since UTF-32LE and UTF-32BE are specified not to output the BOM.
;;
;; This is modeled after the utf-16 format.

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
(define-external-format :utf-32 (:size 4)
  ()

  (octets-to-code (state input unput code error c1 c2 c3 c4 st wd)
    `(block nil
       (when (null ,state) (setf ,state 0))
       (tagbody
	:again
	  (let* ((,st ,state)
		 (,c1 ,input)
		 (,c2 ,input)
		 (,c3 ,input)
		 (,c4 ,input)
		 (,code (if (oddp ,st)
			    ;; Little-endian
			    (+ ,c1
			       (ash ,c2 8)
			       (ash ,c3 16)
			       (ash ,c4 24))
			    ;; Big-endian
			    (+ (ash ,c1 24)
			       (ash ,c2 16)
			       (ash ,c3  8)
			       ,c4)))
		 (,wd 4))
	    (declare (type (integer 0 2) ,st)
		     (type (unsigned-byte 8) ,c1 ,c2 ,c3 ,c4)
		     (optimize (speed 3)))
	    (cond ((or (>= ,code lisp:codepoint-limit)
		       (lisp::surrogatep ,code))
		   ;; Surrogates are illegal and codepoints outside
		   ;; the Unicode range are illegal.  Use replacement
		   ;; character.
		   (setf ,code
			 (if ,error
			     (if (>= ,code lisp:codepoint-limit)
				 (funcall ,error "Illegal codepoint #x~4,'0X" ,code 4)
				 (funcall ,error "Surrogate #x~4,'0X not allowed in UTF32"
					  ,code 4))
			     +replacement-character-code+)))
		  ((and  (zerop ,st) (= ,code #xFFFE0000))
		   ;; BOM for little-endian
		   (setf ,state 1)
		   (go :again))
		  ((and (zerop ,st) (= ,code #xFEFF))
		   ;; BOM for big-endian
		   (setf ,state 2)
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
		       (funcall ,error "Surrogate code #x~4,'0X is illegal for UTF32 output"
				,code)
		       +replacement-character-code+)))
	     (t
	      (out ,code)))))
  nil
  (copy-state (state)
    ;; The state is either NIL or T, so we can just return that.
    `(progn ,state)))
