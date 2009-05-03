;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/string.lisp,v 1.12.30.10 2009/05/03 12:37:02 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to implement strings for CMU Common Lisp
;;; Written by David Dill
;;; Rewritten by Skef Wholey, Bill Chiles and Rob MacLachlan.
;;;
;;; ****************************************************************
;;;
(in-package "LISP")
(export '(char schar glyph sglyph string
	  string= string-equal string< string> string<= string>= string/=
	  string-lessp string-greaterp string-not-lessp string-not-greaterp
	  string-not-equal
	  string-to-nfd string-to-nfkd string-to-nfc string-to-nfkc
	  make-string
	  string-trim string-left-trim string-right-trim
	  string-upcase
	  string-downcase string-capitalize nstring-upcase nstring-downcase
	  nstring-capitalize))


(declaim (inline surrogates-to-codepoint codepoint surrogates))

(defun surrogates-to-codepoint (hi lo)
  "Convert the given Hi and Lo surrogate characters to the
  corresponding codepoint value"
  (declare (type character hi lo))
  (+ (ash (- (the (integer #xD800 #xDBFF) (char-code hi)) #xD800) 10)
     (the (integer #xDC00 #xDFFF) (char-code lo)) #x2400))

(defun codepoint (string i &optional (end (length string)))
  "Return the codepoint value from String at position I.  If that
  position is a surrogate, it is combined with either the previous or
  following character (when possible) to compute the codepoint.  The
  second return value is NIL if the position is not a surrogate pair.
  Otherwise +1 or -1 is returned if the position is the high or low
  surrogate value, respectively."
  (declare (type simple-string string) (type kernel:index i end))
  (let ((code (char-code (schar string i))))
    (cond ((and (<= #xD800 code #xDBFF) (< (1+ i) end))
	   (let ((tmp (char-code (schar string (1+ i)))))
	     (if (<= #xDC00 tmp #xDFFF)
		 (values (+ (ash (- code #xD800) 10) tmp #x2400) +1)
		 (values code nil))))
	  ((and (<= #xDC00 code #xDFFF) (> i 0))
	   (let ((tmp (char-code (schar string (1- i)))))
	     (if (<= #xD800 tmp #xDBFF)
		 (values (+ (ash (- tmp #xD800) 10) code #x2400) -1)
		 (values code nil))))
	  (t (values code nil)))))

(defun surrogates (codepoint)
  "Return the high and low surrogate characters for Codepoint.  If
  Codepoint is in the BMP, the first return value is the corresponding
  character and the second is NIL."
  (declare (type (integer 0 #x10FFFF) codepoint))
  (if (< codepoint #x10000)
      (values (code-char codepoint) nil)
      (let* ((tmp (- codepoint #x10000))
	     (hi (logior (ldb (byte 10 10) tmp) #xD800))
	     (lo (logior (ldb (byte 10 0) tmp) #xDC00)))
	(values (code-char hi) (code-char lo)))))


(defun string (X)
  "Coerces X into a string.  If X is a string, X is returned.  If X is a
   symbol, X's pname is returned.  If X is a character then a one element
   string containing that character is returned.  If X cannot be coerced
   into a string, an error occurs."
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	((characterp x)
	 (let ((res (make-string 1)))
	   (setf (schar res 0) x) res))
	(t
	 (error 'simple-type-error
		:datum x
		:expected-type '(or string symbol character)
		:format-control "~S cannot be coerced to a string."
		:format-arguments (list x)))))

;;; With-One-String is used to set up some string hacking things.  The keywords
;;; are parsed, and the string is hacked into a simple-string.

(eval-when (compile)

(defmacro with-one-string (string start end cum-offset &rest forms)
  `(let ((,string (if (stringp ,string) ,string (string ,string))))
     ;; Optimizer may prove STRING is one.
     (declare (optimize (ext:inhibit-warnings 3)))
     (with-array-data ((,string ,string :offset-var ,cum-offset)
		       (,start ,start)
		       (,end (or ,end (length (the vector ,string)))))
       ,@forms)))

)

;;; With-String is like With-One-String, but doesn't parse keywords.

(eval-when (compile)

(defmacro with-string (string &rest forms)
  `(let ((,string (if (stringp ,string) ,string (string ,string))))
     (with-array-data ((,string ,string)
		       (start)
		       (end (length (the vector ,string))))
       ,@forms)))

)

;;; With-Two-Strings is used to set up string comparison operations.  The
;;; keywords are parsed, and the strings are hacked into simple-strings.

(eval-when (compile)

(defmacro with-two-strings (string1 string2 start1 end1 cum-offset-1
				    start2 end2 &rest forms)
  `(let ((,string1 (if (stringp ,string1) ,string1 (string ,string1)))
	 (,string2 (if (stringp ,string2) ,string2 (string ,string2))))
     (with-array-data ((,string1 ,string1 :offset-var ,cum-offset-1)
		       (,start1 ,start1)
		       (,end1 (or ,end1 (length (the vector ,string1)))))
       (with-array-data ((,string2 ,string2)
			 (,start2 ,start2)
			 (,end2 (or ,end2 (length (the vector ,string2)))))
	 ,@forms))))

)


(defun char (string index)
  "Given a string and a non-negative integer index less than the length of
  the string, returns the character object representing the character at
  that position in the string."
  (declare (optimize (safety 1)))
  (char string index))

(defun %charset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (char string index) new-el))

(defun schar (string index)
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (declare (optimize (safety 1)))
  (schar string index))

(defun %scharset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (schar string index) new-el))

(defun string=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (not (%sp-string-compare string1 start1 end1 string2 start2 end2))))


(defun string/=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((comparison (%sp-string-compare string1 start1 end1
					  string2 start2 end2)))
      (if comparison (- (the fixnum comparison) offset1)))))

(eval-when (compile eval)

;;; Lessp is true if the desired expansion is for string<* or string<=*.
;;; Equalp is true if the desired expansion is for string<=* or string>=*.
(defmacro string<>=*-body (lessp equalp)
  (let ((offset1 (gensym)))
    `(with-two-strings string1 string2 start1 end1 ,offset1 start2 end2
       (let ((index (%sp-string-compare string1 start1 end1
					string2 start2 end2)))
	 (if index
	     (cond ((= (the fixnum index) (the fixnum end1))
		    ,(if lessp
			 `(- (the fixnum index) ,offset1)
		       `nil))
		   ((= (+ (the fixnum index) (- start2 start1))
		       (the fixnum end2))
		    ,(if lessp
			 `nil
			 `(- (the fixnum index) ,offset1)))
		   #-unicode
		   ((,(if lessp 'char< 'char>)
		     (schar string1 index)
		     (schar string2 (+ (the fixnum index) (- start2 start1))))
		    (- (the fixnum index) ,offset1))
		   #-unicode
		   (t nil)
		   #+unicode
		   (t
		    ;; Compare in code point order.  See
		    ;; http://icu-project.org/docs/papers/utf16_code_point_order.html
		    (flet ((fixup (code)
			     (if (>= code #xe000)
				 (- code #x800)
				 (+ code #x2000))))
		      (declare (inline fixup))
		      (let* ((c1 (char-code (schar string1 index)))
			     (c2 (char-code (schar string2
						   (+ (the fixnum index)
						      (- start2 start1))))))
			(cond ((and (>= c1 #xd800)
				    (>= c2 #xd800))
			       (let ((fix-c1 (fixup c1))
				     (fix-c2 (fixup c2)))
				 (if (,(if lessp '< '>) fix-c1 fix-c2)
				     (- (the fixnum index) ,offset1)
				     nil)))
			      (t
			       (if (,(if lessp '< '>) c1 c2)
				   (- (the fixnum index) ,offset1)
				   nil)))))))
	     ,(if equalp `(- (the fixnum end1) ,offset1) 'nil))))))
) ; eval-when

(defun string<* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body t nil))

(defun string>* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body nil nil))

(defun string<=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body t t))

(defun string>=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body nil t))



(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string<* string1 string2 start1 end1 start2 end2))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string>* string1 string2 start1 end1 start2 end2))


(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string<=* string1 string2 start1 end1 start2 end2))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string>=* string1 string2 start1 end1 start2 end2))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char=)."
  (string=* string1 string2 start1 end1 start2 end2))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string/=* string1 string2 start1 end1 start2 end2))


(eval-when (compile eval)

;;; STRING-NOT-EQUAL-LOOP is used to generate character comparison loops for
;;; STRING-EQUAL and STRING-NOT-EQUAL.
(defmacro string-not-equal-loop (end end-value
				     &optional (abort-value nil abortp))
  (declare (fixnum end))
  (let ((end-test (if (= end 1)
		      `(= index1 (the fixnum end1))
		      `(= index2 (the fixnum end2)))))
    `(do ((index1 start1 (1+ index1))
	  (index2 start2 (1+ index2)))
	 (,(if abortp
	       end-test
	       `(or ,end-test
		    (not (char-equal (schar string1 index1)
				     (schar string2 index2)))))
	  ,end-value)
       (declare (fixnum index1 index2))
       ,@(if abortp
	     `((if (not (char-equal (schar string1 index1)
				    (schar string2 index2)))
		   (return ,abort-value)))))))

) ; eval-when

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char-equal)."
  (declare (fixnum start1 start2))
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((slen1 (- (the fixnum end1) start1))
	  (slen2 (- (the fixnum end2) start2)))
      (declare (fixnum slen1 slen2))
      (if (or (minusp slen1) (minusp slen2))
	  ;;prevent endless looping later.
	  (error "Improper bounds for string comparison."))
      (if (= slen1 slen2)
	  ;;return () immediately if lengths aren't equal.
	  (string-not-equal-loop 1 t nil))))) 

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((slen1 (- end1 start1))
	  (slen2 (- end2 start2)))
      (declare (fixnum slen1 slen2))
      (if (or (minusp slen1) (minusp slen2))
	  ;;prevent endless looping later.
	  (error "Improper bounds for string comparison."))
      (cond ((or (minusp slen1) (or (minusp slen2)))
	     (error "Improper substring for comparison."))
	    ((= slen1 slen2)
	     (string-not-equal-loop 1 nil (- index1 offset1)))
	    ((< slen1 slen2)
	     (string-not-equal-loop 1 (- index1 offset1)))
	    (t
	     (string-not-equal-loop 2 (- index1 offset1)))))))
 


(eval-when (compile eval)

;;; STRING-LESS-GREATER-EQUAL-TESTS returns a test on the lengths of string1
;;; and string2 and a test on the current characters from string1 and string2
;;; for the following macro.
(defun string-less-greater-equal-tests (lessp equalp)
  (if lessp
      (if equalp
	  ;; STRING-NOT-GREATERP
	  (values '<= `(not (char-greaterp char1 char2)))
	  ;; STRING-LESSP
	  (values '< `(char-lessp char1 char2)))
      (if equalp
	  ;; STRING-NOT-LESSP
	  (values '>= `(not (char-lessp char1 char2)))
	  ;; STRING-GREATERP
	  (values '> `(char-greaterp char1 char2)))))

(defmacro string-less-greater-equal (lessp equalp)
  (multiple-value-bind (length-test character-test)
		       (string-less-greater-equal-tests lessp equalp)
    `(with-two-strings string1 string2 start1 end1 offset1 start2 end2
       (let ((slen1 (- (the fixnum end1) start1))
	     (slen2 (- (the fixnum end2) start2)))
	 (declare (fixnum slen1 slen2))
	 (if (or (minusp slen1) (minusp slen2))
	     ;;prevent endless looping later.
	     (error "Improper bounds for string comparison."))
	 (do ((index1 start1 (1+ index1))
	      (index2 start2 (1+ index2))
	      (char1)
	      (char2))
	     ((or (= index1 (the fixnum end1)) (= index2 (the fixnum end2)))
	      (if (,length-test slen1 slen2) (- index1 offset1)))
	   (declare (fixnum index1 index2))
	   (setq char1 (schar string1 index1))
	   (setq char2 (schar string2 index2))
	   (if (not (char-equal char1 char2))
	       (if ,character-test
		   (return (- index1 offset1))
		   (return ()))))))))

) ; eval-when

(defun string-lessp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal t nil))

(defun string-greaterp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal nil nil))

(defun string-not-lessp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal nil t))

(defun string-not-greaterp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal t t))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-lessp* string1 string2 start1 end1 start2 end2))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-greaterp* string1 string2 start1 end1 start2 end2))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-lessp* string1 string2 start1 end1 start2 end2))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0)
				    end2)
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-greaterp* string1 string2 start1 end1 start2 end2))


(defun make-string (count &key element-type ((:initial-element fill-char)))
  "Given a character count and an optional fill character, makes and returns
   a new string Count long filled with the fill character."
  (declare (type fixnum count))
  (assert (subtypep element-type 'character))
  (if fill-char
      (do ((i 0 (1+ i))
	   (string (make-string count)))
	  ((= i count) string)
	(declare (fixnum i))
	(setf (schar string i) fill-char))
      (make-string count)))

(defun string-upcase (string &key (start 0) end)
  "Given a string, returns a new string that is a copy of it with
  all lower case alphabetic characters converted to uppercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
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
	    ;; Handle ASCII specially because this is called early in
	    ;; initialization, before unidata is available.
	    (cond ((< 96 code 123) (decf code 32))
		  ((> code 127) (setq code (unicode-upper code))))
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

(defun string-downcase (string &key (start 0) end)
  "Given a string, returns a new string that is a copy of it with
  all upper case alphabetic characters converted to lowercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
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
	    ;; Handle ASCII specially because this is called early in
	    ;; initialization, before unidata is available.
	    (cond ((< 64 code 91) (incf code 32))
		  ((> code 127) (setq code (unicode-lower code))))
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

(defun string-capitalize (string &key (start 0) end)
  "Given a string, returns a copy of the string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
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

(defun nstring-upcase (string &key (start 0) end)
  "Given a string, returns that string with all lower case alphabetic
  characters converted to uppercase."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(multiple-value-bind (code wide) (codepoint string index)
	  (declare (ignore wide))
	  ;; Handle ASCII specially because this is called early in
	  ;; initialization, before unidata is available.
	  (cond ((< 96 code 123) (decf code 32))
		((> code 127) (setq code (unicode-upper code))))
	  ;;@@ WARNING: this may, in theory, need to extend string
	  ;;      (which, obviously, we can't do here.  Unless
	  ;;       STRING is adjustable, maybe)
	  ;;  but that never actually occurs as of Unicode 5.1.0,
	  ;;  so I'm just going to ignore it for now...
	  (multiple-value-bind (hi lo) (surrogates code)
	    (setf (schar string index) hi)
	    (when lo
	      (setf (schar string (incf index)) lo))))))
    save-header))

(defun nstring-downcase (string &key (start 0) end)
  "Given a string, returns that string with all upper case alphabetic
  characters converted to lowercase."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(multiple-value-bind (code wide) (codepoint string index)
	  (declare (ignore wide))
	  (cond ((< 64 code 91) (incf code 32))
		((> code 127) (setq code (unicode-lower code))))
	  ;;@@ WARNING: this may, in theory, need to extend string
	  ;;      (which, obviously, we can't do here.  Unless
	  ;;       STRING is adjustable, maybe)
	  ;;  but that never actually occurs as of Unicode 5.1.0,
	  ;;  so I'm just going to ignore it for now...
	  (multiple-value-bind (hi lo) (surrogates code)
	    (setf (schar string index) hi)
	    (when lo
	      (setf (schar string (incf index)) lo))))))
    save-header))

(defun nstring-capitalize (string &key (start 0) end)
  "Given a string, returns that string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index))
	   (newword t)
	   (char ()))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setq char (schar string index))
	(cond ((not (alphanumericp char))
	       (setq newword t))
	      (newword
	       ;;char is first case-modifiable after non-case-modifiable
	       (setf (schar string index) (char-titlecase char))
	       (setq newword ()))
	      (t
	       (setf (schar string index) (char-downcase char))))))
    save-header))

(defun string-left-trim (char-bag string)
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  left end."
  (with-string string
    (do ((index start (1+ index)))
	((or (= index (the fixnum end))
	     (not (find (schar string index) char-bag)))
	 (subseq (the simple-string string) index end))
      (declare (fixnum index)))))

(defun string-right-trim (char-bag string)
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  right end."
  (with-string string
    (do ((index (1- (the fixnum end)) (1- index)))
	((or (< index start) (not (find (schar string index) char-bag)))
	 (subseq (the simple-string string) start (1+ index)))
      (declare (fixnum index)))))

(defun string-trim (char-bag string)
  "Given a set of characters (a list or string) and a string, returns a
  copy of the string with the characters in the set removed from both
  ends."
  (with-string string
    (let* ((left-end (do ((index start (1+ index)))
			 ((or (= index (the fixnum end))
			      (not (find (schar string index) char-bag)))
			  index)
		       (declare (fixnum index))))
	   (right-end (do ((index (1- (the fixnum end)) (1- index)))
			  ((or (< index left-end)
			       (not (find (schar string index) char-bag)))
			   (1+ index))
			(declare (fixnum index)))))
      (subseq (the simple-string string) left-end right-end))))

(declaim (inline %glyph-f %glyph-b))
(defun %glyph-f (string index)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0))
	   (type simple-string string) (type kernel:index index))
  (let* ((prev 0)
	 (l (length string))
	 (c (codepoint string index l))
	 (n (+ index (if (> c #xFFFF) 2 1))))
    (declare (type (integer 0 #x10FFFF) c) (type kernel:index l n))
    (loop while (< n l) do
      (let* ((c (codepoint string n l))
	     (d (the (unsigned-byte 8) (unicode-combining-class c))))
	(when (or (zerop d) (< d prev))
	  (return))
	(setq prev d)
	(incf n (if (> c #xFFFF) 2 1))))
    n))

(defun %glyph-b (string index)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0))
	   (type simple-string string) (type kernel:index index))
  (flet ((xchar (string index)
	   (let ((c (char-code (schar string index))))
	     (declare (type (integer 0 #x10FFFF) c))
	     (cond ((<= #xDC00 c #xDFFF)
		    (let ((c2 (char-code (schar string (1- index)))))
		      (if (<= #xD800 c2 #xDBFF)
			  (+ (ash (- c2 #xD800) 10) c #x2400)
			  (error "Naked low surrogate in string."))))
		   ((<= #xD800 c #xDBFF)
		    (error "Naked high surrogate in string."))
		   (t c)))))
    (let ((prev 255)
	  (n (1- index)))
      (declare (type kernel:index n))
      (loop while (> n 0) do
	(let* ((c (xchar string n))
	       (d (the (unsigned-byte 8) (unicode-combining-class c))))
	  (cond ((zerop d) (return))
		((> d prev) (incf n (if (> c #xFFFF) 2 1)) (return)))
	  (setq prev d)
	  (decf n (if (> c #xFFFF) 2 1))))
      n)))

(defun glyph (string index &key (from-end nil))
  "GLYPH returns the glyph at the indexed position in a string, and the
  position of the next glyph (or NIL) as a second value.  A glyph is
  a substring consisting of the character at INDEX followed by all
  subsequent combining characters."
  (declare (type simple-string string) (type kernel:index index))
  #-unicode
  (char string index)
  #+unicode
  (with-array-data ((string string) (start) (end))
    (declare (ignore start end))
    (let ((n (if from-end (%glyph-b string index) (%glyph-f string index))))
      (if from-end
	  (values (subseq string n index) (and (> n 0) n))
	  (values (subseq string index n) (and (< n (length string)) n))))))

(defun sglyph (string index &key (from-end nil))
  "SGLYPH returns the glyph at the indexed position, the same as GLYPH,
  except that the string must be a simple-string"
  (declare (type simple-string string) (type kernel:index index))
  #-unicode
  (schar string index)
  #+unicode
  (let ((n (if from-end (%glyph-b string index) (%glyph-f string index))))
    (if from-end
	(values (subseq string n index) (and (> n 0) n))
	(values (subseq string index n) (and (< n (length string)) n)))))

(defun decompose (string &optional (compatibility t))
  (declare (type string string))
  (let ((result (make-string (cond ((< (length string) 40)
				    (* 5 (length string)))
				   ((< (length string) 4096)
				    (* 2 (length string)))
				   (t (round (length string) 5/6)))))
	(fillptr 0))
    (declare (type kernel:index fillptr))
    (labels ((rec (string)
	       (declare (type simple-string string))
	       (do ((i 0 (1+ i)))
		   ((= i (length string)))
		 (declare (type kernel:index i))
		 (multiple-value-bind (code wide) (codepoint string i)
		   (when wide (incf i))
		   (let ((decomp (unicode-decomp code compatibility)))
		     (if decomp (rec decomp) (out code))))))
	     (out (code)
	       (multiple-value-bind (hi lo) (surrogates code)
		 (outch hi)
		 (when lo
		   (outch lo))
		 (let ((cc (unicode-combining-class code)))
		   (unless (zerop cc)
		     (order lo cc (- fillptr (if lo 2 1)))))))
	     (outch (char)
	       (when (= fillptr (length result))
		 (let ((tmp (make-string (round (length result) 5/6))))
		   (replace tmp result)
		   (setq result tmp)))
	       (setf (schar result fillptr) char)
	       (incf fillptr))
	     (order (wide1 cc last)
	       (loop until (minusp last) do
		 (multiple-value-bind (code2 wide2) (codepoint result last)
		   (let ((cc2 (unicode-combining-class code2)))
		     (cond ((zerop cc2) (return))
			   ((> cc2 cc)
			    (case (+ (if wide2 2 0) (if wide1 1 0))
			      (0 (rotatef (schar result last)
					  (schar result (1+ last))))
			      (1 (rotatef (schar result last)
					  (schar result (+ last 1))
					  (schar result (+ last 2))))
			      (2 (rotatef (schar result last)
					  (schar result (1- last))
					  (schar result (1+ last))))
			      (3 (rotatef (schar result last)
					  (schar result (+ last 2)))
				 (rotatef (schar result (1- last))
					  (schar result (1+ last)))))
			    (decf last (if wide2 2 1)))
			   (t (return))))))))
      (with-array-data ((string string) (start) (end))
	(declare (ignore start end))
	(rec string))
      (shrink-vector result fillptr))))

(defun string-to-nfd (string)
  (decompose string nil))

(defun string-to-nfkd (string)
  (decompose string t))

(defun string-to-nfc (string)
  ;;@@ Implement me
  string)

(defun string-to-nfkc (string)
  ;;@@ Implement me
  string)
