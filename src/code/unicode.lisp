;;; -*- Log: code.log; Package: Unicode -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/unicode.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to process Unicode strings for CMU Common Lisp
;;; Written by Paul Foley and Raymond Toy.
;;;
;;; ****************************************************************
;;;
(in-package "UNICODE")
(intl:textdomain "cmucl")

(defun string-upcase-simple (string &key (start 0) end)
  _N"Given a string, returns a new string that is a copy of it with all
  lower case alphabetic characters converted to uppercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (lisp::with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index)))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (multiple-value-bind (code wide) (codepoint string index)
	    (when wide (incf index))
            ;; Use char-upcase if it's not a surrogate pair so that
            ;; we're always consist.
            (if wide
                (setq code (unicode-upper code))
                (setf code (char-code (char-upcase (code-char code)))))
	    ;;@@ WARNING: this may, in theory, need to extend newstring
	    ;;  but that never actually occurs as of Unicode 5.1.0,
	    ;;  so I'm just going to ignore it for now...
	    (multiple-value-bind (hi lo) (surrogates code)
	      (setf (schar newstring new-index) hi)
	      (when lo
		(setf (schar newstring (incf new-index)) lo)))))
	;;@@ WARNING: see above
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))


;; An example where this differs from cl:string-upcase differ:
;; #\Latin_Small_Letter_Sharp_S
(defun string-upcase-full (string &key (start 0) end)
  _N"Given a string, returns a new string that is a copy of it with
  all lower case alphabetic characters converted to uppercase using
  full case conversion."
  (declare (fixnum start)) (let* ((string (if
  (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-output-to-string (s)
      (lisp::with-one-string string start end offset
        (let ((offset-slen (+ slen offset)))
	  (declare (fixnum offset-slen))
	  (write-string string s :start offset :end start)
	  (do ((index start (1+ index)))
	      ((= index (the fixnum end)))
	    (declare (fixnum index))
	    (multiple-value-bind (code wide)
		(codepoint string index)
	      (when wide (incf index))
	      ;; Handle ASCII specially because this is called early in
	      ;; initialization, before unidata is available.
	      (cond ((< 96 code 123)
		     (write-char (code-char (decf code 32)) s))
		    ((> code 127)
		     (write-string (unicode-full-case-upper code) s))
		    (t
		     (multiple-value-bind (hi lo)
			 (surrogates code)
		       (write-char hi s)
		       (when lo
			 (write-char lo s)))))))
	  (write-string string s :start end :end offset-slen))))))

(defun string-upcase (string &key (start 0) end (casing :full))
  _N"Given a string, returns a new string that is a copy of it with
  all lower case alphabetic characters converted to uppercase.  Casing
  is :simple or :full for simple or full case conversion,
  respectively."
  (declare (fixnum start))
  (if (eq casing :simple)
      (string-upcase-simple string :start start :end end)
      (string-upcase-full string :start start :end end)))

(defun string-downcase-simpl (string &key (start 0) end)
  _N"Given a string, returns a new string that is a copy of it with all
  upper case alphabetic characters converted to lowercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (lisp::with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index)))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (multiple-value-bind (code wide) (codepoint string index)
	    (when wide (incf index))
            ;; Use char-downcase if it's not a surrogate pair so that
            ;; we're always consist.
            (if wide
                (setq code (unicode-lower code))
                (setq code (char-code (char-downcase (code-char code)))))
	    ;;@@ WARNING: this may, in theory, need to extend newstring
	    ;;  but that never actually occurs as of Unicode 5.1.0,
	    ;;  so I'm just going to ignore it for now...
	    (multiple-value-bind (hi lo) (surrogates code)
	      (setf (schar newstring new-index) hi)
	      (when lo
		(setf (schar newstring (incf new-index)) lo)))))
	;;@@ WARNING: see above
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

;; An example this differs from cl:string-downcase:
;; #\Latin_Capital_Letter_I_With_Dot_Above.
(defun string-downcase-full (string &key (start 0) end)
  _N"Given a string, returns a new string that is a copy of it with
  all uppercase alphabetic characters converted to lowercase using
  full case conversion.."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-output-to-string (s)
      (lisp::with-one-string string start end offset
        (let ((offset-slen (+ slen offset)))
	  (declare (fixnum offset-slen))
	  (write-string string s :start offset :end start)
	  (do ((index start (1+ index)))
	      ((= index (the fixnum end)))
	    (declare (fixnum index))
	    (multiple-value-bind (code wide)
		(codepoint string index)
	      (when wide (incf index))
	      ;; Handle ASCII specially because this is called early in
	      ;; initialization, before unidata is available.
	      (cond ((< 64 code 91)
		     (write-char (code-char (incf code 32)) s))
		    ((> code 127)
		     (write-string (unicode-full-case-lower code) s))
		    (t
		     ;; Handle codes below 64
		     (multiple-value-bind (hi lo)
			 (surrogates code)
		       (write-char hi s)
		       (when lo
			 (write-char lo s)))))))
	  (write-string string s :start end :end offset-slen))))))

(defun string-downcase (string &key (start 0) end (casing :full))
  _N"Given a string, returns a new string that is a copy of it with all
  uppercase alphabetic characters converted to lowercase.  Casing is
  :simple or :full for simple or full case conversion, respectively."

  (declare (fixnum start))
  (if (eq casing :simple)
      (string-downcase-simple string :start start :end end)
      (string-downcase-full string :start start :end end)))


;;;
;;; This is a Lisp translation of the Scheme code from William
;;; D. Clinger that implements the word-breaking algorithm.  This is
;;; used with permission.
;;;
;;; This version is modified from the original at
;;; http://www.ccs.neu.edu/home/will/R6RS/ to conform to CMUCL's
;;; implementation of the word break properties.
;;;
;;;
;;; Copyright statement and original comments:
;;;
;;;--------------------------------------------------------------------------------

;; Copyright 2006 William D Clinger.
;;
;; Permission to copy this software, in whole or in part, to use this
;; software for any lawful purpose, and to redistribute this software
;; is granted subject to the restriction that all copies made of this
;; software must include this copyright and permission notice in full.
;;
;; I also request that you send me a copy of any improvements that you
;; make to this software so that they may be incorporated within it to
;; the benefit of the Scheme community.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Word-breaking as defined by Unicode Standard Annex #29.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implementation notes.
;;
;; The string-foldcase, string-downcase, and string-titlecase
;; procedures rely on the notion of a word, which is defined
;; by Unicode Standard Annex 29.
;;
;; The string-foldcase and string-downcase procedures rely on
;; word boundaries only when they encounter a Greek sigma, so
;; their performance should not be greatly affected by the
;; performance of the word-breaking algorithm.
;;
;; The string-titlecase procedure must find all word boundaries,
;; but it is typically used on short strings (titles).
;;
;; Hence the performance of the word-breaking algorithm should
;; not matter too much for this reference implementation.
;; Word-breaking is more generally useful, however, so I tried
;; to make this implementation reasonably efficient.
;;
;; Word boundaries are defined by 14 different rules in
;; Unicode Standard Annex #29, and by GraphemeBreakProperty.txt
;; and WordBreakProperty.txt.  See also WordBreakTest.html.
;;
;; My original implementation of those specifications failed
;; 6 of the 494 tests in auxiliary/WordBreakTest.txt, but it
;; appeared to me that those tests were inconsistent with the
;; word-breaking rules in UAX #29.  John Cowan forwarded my
;; bug report to the Unicode experts, and Mark Davis responded
;; on 29 May 2007:
;;
;;   Thanks for following up on this. I think you have found a problem in the
;;   formulation of word break, not the test. The intention was to break after a
;;   Sep character, as is done in Sentence break. So my previous suggestion was
;;   incorrect; instead, what we need is a new rule:
;; 
;;   *Break after paragraph separators.*
;;    WB3a. Sep
;;   I'll make a propose to the UTC for this.
;;
;; Here is Will's translation of those rules (including WB3a)
;; into a finite state machine that searches forward within a
;; string, looking for the next position at which a word break
;; is allowed.  The current state consists of an index i into
;; the string and a summary of the left context whose rightmost
;; character is at index i.  The left context is usually
;; determined by the character at index i, but there are three
;; complications:
;;
;;     Extend and Format characters are ignored unless they
;;         follow a separator or the beginning of the text.
;;     ALetter followed by MidLetter is treated specially.
;;     Numeric followed by MidNum is treated specially.
;;
;; In the implementation below, the left context ending at i
;; is encoded by the following symbols:
;;
;;     CR
;;     Sep (excluding CR)
;;     ALetter
;;     MidLetter
;;     ALetterMidLetter (ALetter followed by MidLetter)
;;     Numeric
;;     MidNum
;;     NumericMidNum (Numeric followed by MidNum)
;;     Katakana
;;     ExtendNumLet
;;     other (none of the above)
;;
;; Given a string s and an exact integer i (which need not be
;; a valid index into s), returns the index of the next character
;; that is not part of the word containing the character at i,
;; or the length of s if the word containing the character at i
;; extends through the end of s.  If i is negative or a valid
;; index into s, then the returned value will be greater than i.
;;
;;;--------------------------------------------------------------------------------

(defun string-next-word-break (s i)
  _N"Given a string, S, and a starting index, return the index of the
  next character that is not part of the word containing the character
  at the index, or the length of S if the word containing the
  character extends to the end of S.  If the index is negative or
  valid index into S, the returned value will be strictly greater than
  the index."
  (let ((n (length s)))
    (labels
	((char-word-break-category (c)
	   ;; Map our unicode word break property into what this
	   ;; algorithm wants.
	   (let ((cat (unicode-word-break c)))
	     (case cat
	       ((:lf :cr :newline)
		:sep)
	       ((:extend :format)
		:extend-or-format)
	       (otherwise cat))))
	 (left-context (i)
	   ;; Given a valid index i into s, returns the left context
	   ;; at i.
	   (multiple-value-bind (c widep)
	       (codepoint s i n)
	     (let* ((back
		     ;; If we're at a regular character or a leading
		     ;; surrogate, decrementing by 1 gets us the to
		     ;; previous character.  But for a trailing
		     ;; surrogate, we need to decrement by 2!
		     (if (eql widep -1)
			 2
			 1))
		    (cat (char-word-break-category c)))
	       (case cat
		 ((:sep)
		  (if (= c (char-code #\return)) :cr cat))
		 ((:midletter :midnumlet)
		  (let ((i-1 (- i back)))
		    (if (and (<= 0 i-1)
			     (eq (left-context i-1) :aletter))
			:aletter-midletter
			cat)))
		 ((:midnum :midnumlet)
		  (let ((i-1 (- i back)))
		    (if (and (<= 0 i-1)
			     (eq (left-context i-1) :numeric))
			:numeric-midnum
			cat)))
		 ((:extendorformat)
		  (if (< 0 i)
		      (left-context (- i back))
		      :other))
		 (otherwise cat)))))

	 (index-of-previous-non-ignored (j)
	   ;; Returns the index of the last non-Extend, non-Format
	   ;; character within (substring s 0 j).  Should not be
	   ;; called unless such a character exists.

	   (let* ((j1 (- j 1)))
	     (multiple-value-bind (c widep)
		 (codepoint s j1)
	       (when (eql widep -1)
		 ;; Back up one more if we're at the trailing
		 ;; surrogate.
		 (decf j1))
	       (let ((cat (char-word-break-category c)))
		 (case cat
		   ((:extend-or-format)
		    (index-of-previous-non-ignored j1))
		   (otherwise j1))))))

	 (lookup (j context)
	   ;; Given j and the context to the left of (not including) j,
	   ;; returns the index at the start of the next word
	   ;; (or before which a word break is permitted).

	   (if (>= j n)
	       (case context
		 ((:aletter-midletter :numeric-midnum)
		  (let ((j (index-of-previous-non-ignored n)))
		    (if (< i j) j n)))
		 (otherwise n))
	       (multiple-value-bind (c widep)
		   (codepoint s j)
		 (let* ((next-j
			 ;; The next character is either 1 or 2 code
			 ;; units away.  For a leading surrogate, it's
			 ;; 2; Otherwise just 1.
			 (if (eql widep 1)
			     2
			     1))
			(cat (char-word-break-category c)))
		   (case cat
		     ((:extend-or-format)
		      (case context
			((:cr :sep) j)
			(otherwise (lookup (+ j next-j) context))))
		     (otherwise
		      (case context
			((:cr)
			 (if (= c (char-code #\linefeed))
			     ;; Rule WB3:  Don't break CRLF, continue looking
			     (lookup (+ j next-j) cat)
			     j))
			((:aletter)
			 (case cat
			   ((:aletter :numeric :extendnumlet)
			    ;; Rules WB5, WB9, ?
			    (lookup (+ j next-j) cat))
			   ((:midletter :midnumlet)
			    ;; Rule WB6, need to keep looking
			    (lookup (+ j next-j) :aletter-midletter))
			   (otherwise j)))
			((:aletter-midletter)
			 (case cat
			   ((:aletter)
			    ;; Rule WB7
			    (lookup (+ j next-j) cat))
			   (otherwise
			    ;; Rule WB6 and WB7 were extended, but the
			    ;; region didn't end with :aletter.  So
			    ;; backup and break at that point.
			    (let ((j2 (index-of-previous-non-ignored j)))
			      (if (< i j2) j2 j)))))
			((:numeric)
			 (case cat
			   ((:numeric :aletter :extendnumlet)
			    ;; Rules WB8, WB10, ?
			    (lookup (+ j next-j) cat))
			   ((:midnum :midnumlet)
			    ;; Rules WB11, need to keep looking
			    (lookup (+ j next-j) :numeric-midnum))
			   (otherwise j)))
			((:numeric-midnum)
			 (case cat
			   ((:numeric)
			    ;; Rule WB11, keep looking
			    (lookup (+ j next-j) cat))
			   (otherwise
			    ;; Rule WB11, WB12 were extended, but the
			    ;; region didn't end with :numeric, so
			    ;; backup and break at that point.
			    (let ((j2 (index-of-previous-non-ignored j)))
			      (if (< i j2) j2 j)))))
			((:midletter :midnum :midnumlet)
			 ;; Rule WB14
			 j)
			((:katakana)
			 (case cat
			   ((:katakana :extendnumlet)
			    ;; Rule WB13, WB13a
			    (lookup (+ j next-j) cat))
			   (otherwise j)))
			((:extendnumlet)
			 (case cat
			   ((:extendnumlet :aletter :numeric :katakana)
			    ;; Rule WB13a, WB13b
			    (lookup (+ j next-j) cat))
			   (otherwise j)))
			((:regional_indicator)
			 (case cat
			   ((:regional_indicator)
			    ;; Rule WB13c
			    (lookup (+ j next-j) cat))
			   (otherwise j)))
			(otherwise j)))))))))
      (declare (notinline lookup left-context))
      (cond ((< i 0)
	     ;; Rule WB1
	     0)
	    ((<= n i)
	     ;; Rule WB2
	     n)
	    (t
	     (multiple-value-bind (c widep)
		 (codepoint s i)
	       (declare (ignore c))
	       (lookup (+ i (if (eql widep 1) 2 1)) (left-context i))))))))

(defun string-capitalize-unicode (string &key (start 0) end (casing :simple))
  "Capitalize String using the Unicode word-break algorithm to find
  the words in String.  The beginning is capitalized depending on the
  value of Casing"
  (declare (type (member :simple :full :title) casing))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-output-to-string (result)
      (lisp::with-one-string string start end offset
        (let ((offset-slen (+ slen offset)))
	  (declare (fixnum offset-slen))

	  (write-string string result :start 0 :end start)
	  (let ((upper (ecase casing
			 (:simple
			  #'(lambda (ch)
			      (multiple-value-bind (hi lo)
				  (surrogates (unicode-upper ch))
				(write-char hi result)
				(when lo (write-char lo result)))))
			 (:full
			  #'(lambda (ch)
			      (write-string (unicode-full-case-upper ch) result)))
			 (:title
			  #'(lambda (ch)
			      (write-string (unicode-full-case-title ch) result))))))
	    (do ((start start next)
		 (next (string-next-word-break string start)
		       (string-next-word-break string next)))
		((or (= start next)
		     (>= start end)))
	      ;; Convert the first character of the word to upper
	      ;; case, and then make the rest of the word lowercase.
	      (funcall upper (codepoint string start))
	      (write-string (string-downcase string :start (1+ start)
						    :end next
						    :casing casing)
			    result
			    :start (1+ start)
			    :end next)))
	  (write-string string result :start end :end offset-slen))))))

(defun string-capitalize-full (string &key (start 0) end (casing :full))
  "Capitalize String using the Common Lisp word-break algorithm to find
  the words in String.  The beginning is capitalized depending on the
  value of Casing"
  (declare (fixnum start)
	   (type (member :simple :full :title) casing))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-output-to-string (s)
      (lisp::with-one-string string start end offset
        (let ((offset-slen (+ slen offset)))
	  (declare (fixnum offset-slen))
	  (write-string string s :start offset :end start)
	  (flet ((alphanump (m)
		   (or (< 47 m 58) (< 64 m 91) (< 96 m 123)
		       #+(and unicode (not unicode-bootstrap))
		       (and (> m 127)
			    (<= +unicode-category-letter+
				(unicode-category m)
				(+ +unicode-category-letter+ #x0F)))))
		 (upper (ch)
		   (ecase casing
		     (:simple
		      #'(lambda (ch)
			  (multiple-value-bind (hi lo)
			      (surrogates (unicode-upper ch))
			    (write-char hi s)
			    (when lo (write-char lo s)))))
		     (:full
		      #'(lambda (ch)
			  (write-string (unicode-full-case-upper ch) s)))
		     (:title
		      #'(lambda (ch)
			  (write-string (unicode-full-case-title ch) s))))))
	    (do ((index start (1+ index))
		 (newword t))
		((= index (the fixnum end)))
	      (declare (fixnum index))
	      (multiple-value-bind (code wide)
		  (codepoint string index)
		(when wide (incf index))
		(cond ((not (alphanump code))
		       (multiple-value-bind (hi lo)
			   (surrogates code)
			 (write-char hi s)
			 (when lo (write-char lo s)))
		       (setq newword t))
		      (newword
		       ;; Char is first case-modifiable after non-case-modifiable
		       (funcall upper code)
		       (setq newword ()))
		      (t
		       ;; char is case-modifiable, but not first
		       (write-string (unicode-full-case-lower code) s))))))
	  (write-string string s :start end :end offset-slen))))))

(defun string-capitalize (string &key (start 0) end
				 (casing :title)
				 (unicode-word-break t))
  _N"Given a string, returns a copy of the string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. Casing is :simple, :full
  or :title for simple, full or title case conversion, respectively.  If
  Unicode-Word-Break is non-Nil, then the Unicode word-breaking
  algorithm is used to determine the word boundaries.  Otherwise, A
  ``word'' is defined to be a string of case-modifiable characters
  delimited by non-case-modifiable chars.  "

  (declare (fixnum start)
	   (type (member :simple :full :title) casing))
  (if unicode-word-break
      (string-capitalize-unicode string :start start :end end :casing casing)
      (if (eq casing :simple)
	  (cl:string-capitalize string :start start :end end)
	  (string-capitalize-full string :start start :end end :casing casing))))


(defun decompose-hangul-syllable (cp stream)
  "Decompose the Hangul syllable codepoint CP to an equivalent sequence
  of conjoining jamo and print the decomposed result to the stream
  STREAM."
  (let* ((s-base #xac00)
	 (l-base #x1100)
	 (v-base #x1161)
	 (t-base #x11a7)
	 (v-count 21)
	 (t-count 28)
	 (n-count (* v-count t-count)))
    ;; Step 1: Compute index of the syllable S
    (let ((s-index (- cp s-base)))
      ;; Step 2: If s is in the range 0 <= s <= s-count, the compute
      ;; the components.
      (let ((l (+ l-base (truncate s-index n-count)))
	    (v (+ v-base (truncate (mod s-index n-count) t-count)))
	    (tt (+ t-base (mod s-index t-count))))
	;; Step 3: If tt = t-base, then there is no trailing character
	;; so replace s by the sequence <l,v>.  Otherwise there is a
	;; trailing character, so replace s by the sequence <l,v,tt>.
	(princ (code-char l) stream)
	(princ (code-char v) stream)
	(unless (= tt t-base)
	  (princ (code-char tt) stream)))))
  (values))

(defun is-hangul-syllable (codepoint)
  "Test if CODEPOINT is a Hangul syllable"
  (let* ((s-base #xac00)
	 (l-count 19)
	 (v-count 21)
	 (t-count 28)
	 (n-count (* v-count t-count))
	 (number-of-syllables (* l-count n-count)))
    (<= 0 (- codepoint s-base) number-of-syllables)))

(defun decompose-hangul (string)
  "Decompose any Hangul syllables in STRING to an equivalent sequence of
  conjoining jamo characters."
  (with-output-to-string (s)
    (loop for cp being the codepoints of string
	  do
	     (if (is-hangul-syllable cp)
		 (decompose-hangul-syllable cp s)
		 (multiple-value-bind (high low)
		     (surrogates cp)
		   (princ high s)
		   (when low
		     (princ low s)))))))
