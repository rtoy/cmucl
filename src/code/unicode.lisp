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
  (let* ((string (string string))
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
  (declare (fixnum start))
  (let* ((string (string string))
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
  (declare (fixnum start)
           (type (member :simple :full) casing))
  (ecase casing
    (:simple
     (string-upcase-simple string :start start :end end))
    (:full
     (string-upcase-full string :start start :end end))))

(defun string-downcase-simple (string &key (start 0) end)
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

  (declare (fixnum start)
           (type (member :simple :full) casing))
  (ecase casing
    (:simple
     (string-downcase-simple string :start start :end end))
    (:full
     (string-downcase-full string :start start :end end))))


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
  ;; Implements the word-boundary rules of UAX #29 (Unicode 17.0).
  ;;
  ;; Decode S into codepoints (collapsing UTF-16 surrogate pairs),
  ;; classify each by its word-break property, then scan boundaries
  ;; left to right applying the WB rules, returning the first boundary
  ;; whose string index is strictly greater than I.
  ;;
  ;; WB4 makes Extend, Format and ZWJ "ignorable": they attach to the
  ;; preceding context, so most rules compare a candidate against the
  ;; nearest non-ignorable element on each side rather than the literal
  ;; neighbour.  ZWJ is also significant for WB3c, and Extend can sit
  ;; between paired Regional_Indicators, so those rules look past
  ;; ignorables explicitly.
  (declare (type simple-string s))
  (let ((n (length s)))
    (cond
      ((< i 0) 0)                       ; Rule WB1 (start of text)
      ((>= i n) n)                      ; Rule WB2 (end of text)
      (t
       (let ((cls (make-array 16 :fill-pointer 0 :adjustable t))
             (idx (make-array 16 :fill-pointer 0 :adjustable t)))
         ;; Decode codepoints and record the string index of each.
         (let ((k 0))
           (loop while (< k n) do
             (multiple-value-bind (cp widep) (codepoint s k)
               (vector-push-extend (unicode-word-break cp) cls)
               (vector-push-extend k idx)
               (incf k (if (eql widep 1) 2 1)))))
         (let ((m (fill-pointer cls)))
           (labels
               ((class (j) (aref cls j))
                (ah-letter-p (c) (or (eq c :aletter) (eq c :hebrew_letter)))
                (mid-letter-q-p (c)     ; (MidLetter | MidNumLetQ)
                  (or (eq c :midletter) (eq c :midnumlet) (eq c :single_quote)))
                (mid-num-q-p (c)        ; (MidNum | MidNumLetQ)
                  (or (eq c :midnum) (eq c :midnumlet) (eq c :single_quote)))
                (ignorable (c)          ; WB4: Extend | Format | ZWJ
                  (or (eq c :extend) (eq c :format) (eq c :zwj)))
                (ext-pict-at (j)
                  (unicode-extended-pictographic-p (codepoint s (aref idx j))))
                (prev-significant (j)   ; last non-ignorable element < J, or -1
                  (loop for p from (1- j) downto 0
                        unless (ignorable (class p)) return p
                        finally (return -1)))
                (next-significant (j)   ; first non-ignorable element > J, or -1
                  (loop for q from (1+ j) below m
                        unless (ignorable (class q)) return q
                        finally (return -1)))
                (ri-count-left (j)
                  ;; Number of significant Regional_Indicator elements
                  ;; immediately to the left of J (stopping at the first
                  ;; non-RI significant element; ignorables are skipped).
                  (let ((count 0))
                    (loop for p = (prev-significant j) then (prev-significant p)
                          while (and (>= p 0) (eq (class p) :regional_indicator))
                          do (incf count))
                    count))
                (break-before-p (j)
                  ;; Does the algorithm allow a break immediately before
                  ;; element J (1 <= J < M)?
                  (let* ((c (class j))
                         (lit (class (1- j)))   ; literal previous element
                         (pj (prev-significant j)))
                    (cond
                      ;; WB3: CR x LF
                      ((and (eq lit :cr) (eq c :lf)) nil)
                      ;; WB3a: (Newline | CR | LF) div
                      ((member lit '(:newline :cr :lf)) t)
                      ;; WB3b: div (Newline | CR | LF)
                      ((member c '(:newline :cr :lf)) t)
                      ;; WB3c: ZWJ x \p{Extended_Pictographic}
                      ((and (eq lit :zwj) (ext-pict-at j)) nil)
                      ;; WB3d: WSegSpace x WSegSpace
                      ((and (eq lit :wsegspace) (eq c :wsegspace)) nil)
                      ;; WB4: x (Extend | Format | ZWJ): never break before
                      ;; an ignorable (covers ignorables after sot or after
                      ;; another ignorable too).
                      ((ignorable c) nil)
                      ;; Nothing significant to the left: break.
                      ((< pj 0) t)
                      (t
                       (let ((p (class pj)))
                         (cond
                           ;; WB5: AHLetter x AHLetter
                           ((and (ah-letter-p p) (ah-letter-p c)) nil)
                           ;; WB6: AHLetter x (MidLetter|MidNumLetQ) AHLetter
                           ((and (ah-letter-p p) (mid-letter-q-p c)
                                 (let ((nx (next-significant j)))
                                   (and (>= nx 0) (ah-letter-p (class nx)))))
                            nil)
                           ;; WB7: AHLetter (MidLetter|MidNumLetQ) x AHLetter
                           ((and (ah-letter-p c) (mid-letter-q-p p)
                                 (let ((pp (prev-significant pj)))
                                   (and (>= pp 0) (ah-letter-p (class pp)))))
                            nil)
                           ;; WB7a: Hebrew_Letter x Single_Quote
                           ((and (eq p :hebrew_letter) (eq c :single_quote)) nil)
                           ;; WB7b: Hebrew_Letter x Double_Quote Hebrew_Letter
                           ((and (eq p :hebrew_letter) (eq c :double_quote)
                                 (let ((nx (next-significant j)))
                                   (and (>= nx 0) (eq (class nx) :hebrew_letter))))
                            nil)
                           ;; WB7c: Hebrew_Letter Double_Quote x Hebrew_Letter
                           ((and (eq c :hebrew_letter) (eq p :double_quote)
                                 (let ((pp (prev-significant pj)))
                                   (and (>= pp 0) (eq (class pp) :hebrew_letter))))
                            nil)
                           ;; WB8: Numeric x Numeric
                           ((and (eq p :numeric) (eq c :numeric)) nil)
                           ;; WB9: AHLetter x Numeric
                           ((and (ah-letter-p p) (eq c :numeric)) nil)
                           ;; WB10: Numeric x AHLetter
                           ((and (eq p :numeric) (ah-letter-p c)) nil)
                           ;; WB11: Numeric (MidNum|MidNumLetQ) x Numeric
                           ((and (eq c :numeric) (mid-num-q-p p)
                                 (let ((pp (prev-significant pj)))
                                   (and (>= pp 0) (eq (class pp) :numeric))))
                            nil)
                           ;; WB12: Numeric x (MidNum|MidNumLetQ) Numeric
                           ((and (eq p :numeric) (mid-num-q-p c)
                                 (let ((nx (next-significant j)))
                                   (and (>= nx 0) (eq (class nx) :numeric))))
                            nil)
                           ;; WB13: Katakana x Katakana
                           ((and (eq p :katakana) (eq c :katakana)) nil)
                           ;; WB13a: (AHLetter|Numeric|Katakana|ExtendNumLet) x ExtendNumLet
                           ((and (member p '(:aletter :hebrew_letter :numeric
                                             :katakana :extendnumlet))
                                 (eq c :extendnumlet))
                            nil)
                           ;; WB13b: ExtendNumLet x (AHLetter|Numeric|Katakana)
                           ((and (eq p :extendnumlet)
                                 (or (ah-letter-p c)
                                     (member c '(:numeric :katakana))))
                            nil)
                           ;; WB15/WB16: in a Regional_Indicator run, break
                           ;; only between pairs: break before J iff an even
                           ;; number of RIs precede it.
                           ((and (eq p :regional_indicator)
                                 (eq c :regional_indicator))
                            (evenp (ri-count-left j)))
                           ;; WB999: otherwise break.
                           (t t))))))))
             ;; Find the first allowed boundary whose string index is > I.
             (loop for j from 1 below m
                   when (and (> (aref idx j) i) (break-before-p j))
                     do (return-from string-next-word-break (aref idx j)))
             ;; WB2: otherwise the word extends to end of text.
             n)))))))

(defun char-titlecase (char)
  "Returns CHAR converted to title-case if that is possible."
  (declare (character char))
  (let ((m (char-code char)))
    (cond ((<= (char-code #\a) m (char-code #\z))
           (code-char (logxor m #x20)))
          #+(and unicode (not unicode-bootstrap))
	  ((> m lisp::+ascii-limit+)
           (code-char (lisp::unicode-title m)))
	  (t char))))

(defun title-case-p (char)
  "The argument must be a character object; title-case-p returns T if the
  argument is a title-case character, NIL otherwise."
  (declare (character char))
  (let ((m (char-code char)))
    (or (<= (char-code #\A) m (char-code #\Z))
	#+(and unicode (not unicode-bootstrap))
	(and (> m lisp::+ascii-limit+)
	     (= (unicode-category m) +unicode-category-title+)))))

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

(defun string-capitalize-simple (string &key (start 0) end)
  _N"Given a string, returns a copy of the string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
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
	     (new-index (- start offset) (1+ new-index))
	     (newword t)
	     (char ()))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setq char (schar string index))
	  (cond ((not (alphanumericp char))
		 (setq newword t))
		(newword
		 ;;char is first case-modifiable after non-case-modifiable
		 (setq char (char-titlecase char))
		 (setq newword ()))
		;;char is case-modifiable, but not first
		(t (setq char (char-downcase char))))
	  (setf (schar newstring new-index) char))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

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
	   (type (member :simple-title :simple :full :title) casing))
  (if unicode-word-break
      (string-capitalize-unicode string :start start :end end :casing casing)
      (ecase casing
        (:simple-title
	 (string-capitalize-simple string :start start :end end))
        ((:simple :full :title)
	 (string-capitalize-full string :start start :end end :casing casing)))))


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
