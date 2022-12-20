;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/string.lisp $")
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
(intl:textdomain "cmucl")

(export '(char schar glyph sglyph string
	  string= string-equal string< string> string<= string>= string/=
	  string-lessp string-greaterp string-not-lessp string-not-greaterp
	  string-not-equal
	  string-to-nfc
	  make-string
	  string-trim string-left-trim string-right-trim
	  string-upcase
	  string-downcase string-capitalize nstring-upcase nstring-downcase
	  nstring-capitalize))

#+unicode
(export '(string-to-nfd string-to-nfkd string-to-nfkc surrogates))

(declaim (inline surrogatep surrogates-to-codepoint codepoint surrogates))

(defun surrogatep (char-or-code &optional surrogate-type)
  "Test if C is a surrogate.  C may be either an integer or a
  character. Surrogate-type indicates what kind of surrogate to test
  for.  :High means to test for the high (leading) surrogate; :Low
  tests for the low (trailing surrogate).  A value of :Any or Nil
  tests for any surrogate value (high or low)."
  (declare (type (or character codepoint) char-or-code)
	   (type (or null (member :high :leading :low :trailing :any)) surrogate-type)
	   (optimize (inhibit-warnings 3)))
  (let ((code (if (characterp char-or-code)
		  (char-code char-or-code)
		  char-or-code)))
    (ecase surrogate-type
      ((:high :leading)
       ;; Test for high surrogate (#xD800 to #xDBFF)
       (= #b110110 (ash code -10)))
      ((:low :trailing)
       ;; Test for low surrogate (#xDC00 to #xDFFF)
       (= #b110111 (ash code -10)))
      ((:any nil)
       ;; Test for any surrogate (#xD800 to #xDFFF)
       (= #b11011 (ash code -11))))))

(defun surrogates-to-codepoint (hi-surrogate-char lo-surrogate-char)
  "Convert the given Hi and Lo surrogate characters to the
  corresponding codepoint value"
  (declare (type character hi-surrogate-char lo-surrogate-char))
  (+ (ash (- (the (integer #xD800 #xDBFF) (char-code hi-surrogate-char)) #xD800) 10)
     (the (integer #xDC00 #xDFFF) (char-code lo-surrogate-char)) #x2400))

(defun codepoint (string i &optional (end (length string)))
  "Return the codepoint value from String at position I.  If that
  position is a surrogate, it is combined with either the previous or
  following character (when possible) to compute the codepoint.  The
  second return value is NIL if the position is not a surrogate pair.
  Otherwise +1 or -1 is returned if the position is the high or low
  surrogate value, respectively."
  (declare (type simple-string string) (type kernel:index i end))
  (let ((code (char-code (schar string i))))
    (cond ((and (surrogatep code :high) (< (1+ i) end))
	   (let ((tmp (char-code (schar string (1+ i)))))
	     (if (surrogatep tmp :low)
		 (values (truly-the codepoint (+ (ash (- code #xD800) 10) tmp #x2400))
			 +1)
		 (values (truly-the codepoint code) nil))))
	  ((and (surrogatep code :low) (> i 0))
	   (let ((tmp (char-code (schar string (1- i)))))
	     (if (surrogatep tmp :high)
		 (values (truly-the codepoint (+ (ash (- tmp #xD800) 10) code #x2400))
			 -1)
		 (values (truly-the codepoint code) nil))))
	  (t (values (truly-the codepoint code) nil)))))

(defmacro with-string-codepoint-iterator ((next string) &body body)
  _N"WITH-STRING-CODEPOINT-ITERATOR ((next string) &body body)
  provides a method of looping through a string from the beginning to
  the end of the string prodcucing successive codepoints from the
  string.  NEXT is bound to a generator macro that, within the scope
  of the invocation, returns one or two values. The first value tells
  whether any objects remain in the string. When the first value is
  non-NIL, the second value is the codepoint of the next object."
  (let ((n-next (gensym "WITH-STRING-CODEPOINT-ITERATOR-")))
    `(let ((,n-next
	     (let* ((s ,string)
		    (len (length s))
		    (index 0))
	       (labels
		   ((,next ()
		      (when (< index len)
			(multiple-value-bind (cp surrogatep)
			    (codepoint s index)
			  (incf index (if surrogatep 2 1))
			  (values t cp)))))
		 #',next))))
       (macrolet ((,next () '(funcall ,n-next)))
	 ,@body))))

(defun surrogates (codepoint)
  "Return the high and low surrogate characters for Codepoint.  If
  Codepoint is in the BMP, the first return value is the corresponding
  character and the second is NIL."
  (declare (type codepoint codepoint))
  (if (< codepoint #x10000)
      (values (code-char codepoint) nil)
      (let* ((tmp (- codepoint #x10000))
	     (hi (logior (ldb (byte 10 10) tmp) #xD800))
	     (lo (logior (ldb (byte 10 0) tmp) #xDC00)))
	(values (code-char hi) (code-char lo)))))

(defun (setf codepoint) (codepoint string i)
  "Set the codepoint at string position I to the Codepoint.  If the
  codepoint requires a surrogate pair, the high (leading surrogate) is
  stored at position I and the low (trailing) surrogate is stored at
  I+1"
  (declare (type codepoint codepoint)
	   (type simple-string string))
  (let ((widep nil))
    (multiple-value-bind (hi lo)
	(surrogates codepoint)
      (setf (aref string i) hi)
      (when lo
	(setf (aref string (1+ i)) lo)
	(setf widep t)))
    (values codepoint widep)))

#+unicode
(defun utf16-string-p (string)
  _N"Check if String is a valid UTF-16 string.  If the string is valid,
  T is returned.  If the string is not valid, NIL is returned, and the
  second value is the index into the string of the invalid character.
  A string is also invalid if it contains any unassigned codepoints."
  (do ((len (length string))
       (index 0 (1+ index)))
      ((>= index len)
       t)
    (multiple-value-bind (codepoint wide)
	(codepoint string index)
      ;; We step through the string in order.  If there are any
      ;; surrogates pairs, we must reach the lead surrogate first,
      ;; which means WIDE is +1.  Otherwise, we have an invalid
      ;; surrogate pair.  If we get any codepoint that is in the
      ;; surrogate range, we also have an invalid string.  An
      ;; unassigned codepoint is also considered invalid.
      (when (or (eq wide -1)
		(surrogatep codepoint)
		(not (unicode-assigned-codepoint-p codepoint)))
	(return-from utf16-string-p (values nil index)))
      (when wide (incf index)))))

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
		:format-control (intl:gettext "~S cannot be coerced to a string.")
		:format-arguments (list x)))))

;;; With-One-String is used to set up some string hacking things.  The keywords
;;; are parsed, and the string is hacked into a simple-string.

(eval-when (compile load eval)

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

#+unicode
(defun string-case-fold (string &key (start 0) end (casing :simple))
  _N"Return a new string with the case folded according to Casing as follows:

  :SIMPLE  Unicode simple case folding (preserving length)
  :FULL    Unicode full case folding (possibly changing length)

  Default Casing is :SIMPLE."
  (ecase casing
    (:simple
     (with-output-to-string (s)
       (with-one-string string start end offset
	 (do ((index offset (1+ index)))
	     ((>= index end))
	   (multiple-value-bind (code widep)
	       (codepoint string index)
	     (when widep (incf index))
	     (multiple-value-bind (hi lo)
		 (surrogates (unicode-case-fold-simple code))
	       (write-char hi s)
	       (when lo (write-char lo s))))))))
    (:full
     (with-output-to-string (s)
       (with-one-string string start end offset
	 (do ((index offset (1+ index)))
	     ((>= index end))
	   (multiple-value-bind (code widep)
	       (codepoint string index)
	     (when widep (incf index))
	     (write-string (unicode-case-fold-full code) s))))))))

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
	  (error (intl:gettext "Improper bounds for string comparison.")))
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
	  (error (intl:gettext "Improper bounds for string comparison.")))
      (cond ((or (minusp slen1) (or (minusp slen2)))
	     (error (intl:gettext "Improper substring for comparison.")))
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
	  (values '<=
		  #-unicode `(not (char-greaterp char1 char2))
		  #+unicode `(<= char1 char2))
	  ;; STRING-LESSP
	  (values '<
		  #-unicode `(char-lessp char1 char2)
		  #+unicode `(< char1 char2)))
      (if equalp
	  ;; STRING-NOT-LESSP
	  (values '>=
		  #-unicode `(not (char-lessp char1 char2))
		  #+unicode `(>= char1 char2))
	  ;; STRING-GREATERP
	  (values '>
		  #-unicode `(char-greaterp char1 char2)
		  #+unicode `(> char1 char2)))))

#-unicode
(defmacro string-less-greater-equal (lessp equalp)
  (multiple-value-bind (length-test character-test)
		       (string-less-greater-equal-tests lessp equalp)
    `(with-two-strings string1 string2 start1 end1 offset1 start2 end2
       (let ((slen1 (- (the fixnum end1) start1))
	     (slen2 (- (the fixnum end2) start2)))
	 (declare (fixnum slen1 slen2))
	 (if (or (minusp slen1) (minusp slen2))
	     ;;prevent endless looping later.
	     (error (intl:gettext "Improper bounds for string comparison.")))
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

;; Convert to lowercase for case folding, to match what Unicode
;; CaseFolding.txt says.  An example where this matters: U+1E9E maps
;; to U+00DF.  But the uppercase version of U+00DF is U+00DF.
#+unicode
(defmacro equal-char-codepoint (codepoint)
  `(let ((ch ,codepoint))
     ;; Handle ASCII separately for bootstrapping and for unidata missing.
     (if (< 64 ch 91)
	 (+ ch 32)
	 #-(and unicode (not unicode-bootstrap))
	 ch
	 #+(and unicode (not unicode-bootstrap))
	 (if (> ch 127) (unicode-lower ch) ch))))

#+unicode
(defmacro string-less-greater-equal (lessp equalp)
  (multiple-value-bind (length-test character-test)
      (string-less-greater-equal-tests lessp equalp)
    `(with-two-strings string1 string2 start1 end1 offset1 start2 end2
       (let ((slen1 (- (the fixnum end1) start1))
	     (slen2 (- (the fixnum end2) start2)))
	 (declare (fixnum slen1 slen2))
	 (if (or (minusp slen1) (minusp slen2))
	     ;;prevent endless looping later.
	     (error (intl:gettext "Improper bounds for string comparison.")))
	 (do ((index1 start1 (1+ index1))
	      (index2 start2 (1+ index2)))
	     ((or (= index1 (the fixnum end1)) (= index2 (the fixnum end2)))
	      (if (,length-test slen1 slen2) (- index1 offset1)))
	   (declare (fixnum index1 index2))
	   (multiple-value-bind (char1 wide1)
	       (codepoint string1 index1)
	     (declare (type codepoint char1))
	     (multiple-value-bind (char2 wide2)
		 (codepoint string2 index2)
	       (declare (type codepoint char2))
	       (setf char1 (equal-char-codepoint char1))
	       (setf char2 (equal-char-codepoint char2))
	       (if (= char1 char2)
		   (progn
		     (when wide1 (incf index1))
		     (when wide2 (incf index2)))
		   (if ,character-test
		       (return (- index1 offset1))
		       (return ()))))))))))

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
  _N"Given a string, returns a new string that is a copy of it with all
  lower case alphabetic characters converted to uppercase."
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
		  #+unicode
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
  _N"Given a string, returns a new string that is a copy of it with all
  upper case alphabetic characters converted to lowercase."
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
  _N"Given a string, returns a copy of the string with the first
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
		#+unicode
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
		#+unicode
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


#+unicode
(progn
;; Like string-left-trim, but return the index 
(defun string-left-trim-index (char-bag string)
  (with-string string
    (if (stringp char-bag)
	;; When char-bag is a string, we try to do the right thing.
	;; Convert char-bag to a list of codepoints and compare the
	;; codepoints in the string with this.  
	(let ((code-bag (with-string char-bag
			  (do ((index start (1+ index))
			       (result nil))
			      ((= index end)
			       (nreverse result))
			    (multiple-value-bind (c widep)
				(codepoint char-bag index)
			      (push c result)
			      (when widep (incf index)))))))
	  (do ((index start (1+ index)))
	      ((= index (the fixnum end))
	       end)
	    (declare (fixnum index))
	    (multiple-value-bind (c widep)
		(codepoint string index)
	      (unless (find c code-bag)
		(return-from string-left-trim-index index))
	      (when widep (incf index)))))
	;; When char-bag is a list, we just look at each codepoint of
	;; STRING to see if it's in char-bag.  If char-bag contains a
	;; surrogate, we could accidentally trim off a surrogate,
	;; leaving an invalid UTF16 string.
	(do ((index start (1+ index)))
	    ((= index (the fixnum end))
	     end)
	  (declare (fixnum index))
	  (multiple-value-bind (c widep)
	      (codepoint string index)
	    (unless (find c char-bag :key #'char-code)
	      (return-from string-left-trim-index index))
	    (when widep (incf index)))))))

(defun string-left-trim (char-bag string)
  _N"Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  left end.  If the set of characters is a string, surrogates will be
  properly handled."
  (let ((begin (string-left-trim-index char-bag string)))
    (with-string string
      (declare (ignore start))
      (subseq string begin end))))

(defun string-right-trim-index (char-bag string)
  (with-string string
    (if (stringp char-bag)
	;; When char-bag is a string, we try to do the right thing
	;; with surrogates.  Convert char-bag to a list of codepoints
	;; and compare the codepoints in the string with this.
	(let ((code-bag (with-string char-bag
			  (do ((index start (1+ index))
			       (result nil))
			      ((= index end)
			       result)
			    (multiple-value-bind (c widep)
				(codepoint char-bag index)
			      (push c result)
			      (when widep (incf index)))))))
	  (do ((index (1- end) (1- index)))
	      ((< index start)
	       start)
	    (declare (fixnum index))
	    (multiple-value-bind (c widep)
		(codepoint string index)
	      (unless (find c code-bag)
		(return-from string-right-trim-index (1+ index)))
	      (when widep (decf index)))))
	;; When char-bag is a list, we just look at each codepoint of
	;; STRING to see if it's in char-bag.  If char-bag contains a
	;; surrogate, we could accidentally trim off a surrogate,
	;; leaving an invalid UTF16 string.
	(do ((index (1- end) (1- index)))
	    ((< index start)
	     start)
	  (declare (fixnum index))
	  (multiple-value-bind (c widep)
	      (codepoint string index)
	    (unless (find c char-bag :key #'char-code)
	      (return-from string-right-trim-index (1+ index)))
	    (when widep (decf index)))))))

(defun string-right-trim (char-bag string)
  _N"Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  right end.  If the set of characters is a string, surrogates will be
  properly handled."
  (let ((stop (string-right-trim-index char-bag string)))
    (with-string string
      (declare (ignore end))
      (subseq string start stop))))

(defun string-trim (char-bag string)
  _N"Given a set of characters (a list or string) and a string, returns a
  copy of the string with the characters in the set removed from both
  ends.  If the set of characters is a string, surrogates will be
  properly handled."
  (let ((left-end (string-left-trim-index char-bag string))
	(right-end (string-right-trim-index char-bag string)))
    (with-string string
      (declare (ignore start end))
      (subseq (the simple-string string) left-end right-end))))
) ; end unicode version

#-unicode
(progn
(defun string-left-trim (char-bag string)
  _N"Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  left end."
  (with-string string
    (do ((index start (1+ index)))
	((or (= index (the fixnum end))
	     (not (find (schar string index) char-bag)))
	 (subseq (the simple-string string) index end))
      (declare (fixnum index)))))

(defun string-right-trim (char-bag string)
  _N"Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  right end."
  (with-string string
    (do ((index (1- (the fixnum end)) (1- index)))
	((or (< index start) (not (find (schar string index) char-bag)))
	 (subseq (the simple-string string) start (1+ index)))
      (declare (fixnum index)))))

(defun string-trim (char-bag string)
  _N"Given a set of characters (a list or string) and a string, returns a
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
) ; non-unicode version

#+unicode
(progn
(declaim (inline %glyph-f %glyph-b))
(defun %glyph-f (string index)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0))
	   (type simple-string string) (type kernel:index index))
  (let* ((prev 0)
	 (l (length string))
	 (c (codepoint string index l))
	 (n (+ index (if (> c #xFFFF) 2 1))))
    (declare (type codepoint c) (type kernel:index l n))
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
  (let* ((prev 255)
	 (n (1- index)))
    (declare (type kernel:index n))
    (loop until (< n 0) do
      (let* ((c (codepoint string n 0))
	     (d (the (unsigned-byte 8) (unicode-combining-class c))))
	(cond ((zerop d) (return))
	      ((> d prev) (incf n (if (> c #xFFFF) 2 1)) (return)))
	(setq prev d)
	(decf n (if (> c #xFFFF) 2 1))))
    n))
) ; unicode

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

(defmacro with-string-glyph-iterator ((next string) &body body)
  _N"WITH-STRING-GLYPH-ITERATOR ((next string) &body body)
  provides a method of looping through a string from the beginning to
  the end of the string prodcucing successive glyphs from the string.
  NEXT is bound to a generator macro that, within the scope of the
  invocation, returns one or three values. The first value tells
  whether any objects remain in the string. When the first value is
  non-NIL, the second value is the index into the string of the glyph
  and the third value is index of the next glyph."
  (let ((n-next (gensym "WITH-STRING-GLYPH-ITERATOR-")))
    `(let ((,n-next
	     (let* ((s ,string)
		    (len (length s))
		    (index 0))
	       (labels
		   ((,next ()
		      (when (< index len)
			(let ((glyph-end (%glyph-f s index)))
			  (multiple-value-prog1
			      (values t index glyph-end)
			    (setf index glyph-end))))))
		 #',next))))
       (macrolet ((,next () '(funcall ,n-next)))
	 ,@body))))

#+unicode
(defun string-reverse* (sequence)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type string sequence))
  (with-string sequence
    (let* ((length (- end start))
	   (r (make-string length)))
      (do ((dst-index (1- length) (1- dst-index))
	   (src-index start (1+ src-index)))
	  ((minusp dst-index))
	(declare (fixnum src-index dst-index))
	(setf (schar r dst-index) (schar sequence src-index)))
      r)))

#+unicode
(defun string-nreverse* (sequence)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type string sequence))
  (with-string sequence
    (let ((length (- end start)))
      (declare (fixnum length))
      (do ((left-index 0 (1+ left-index))
	   (right-index (1- length) (1- right-index))
	   (half-length (truncate length 2)))
	  ((= left-index half-length) sequence)
	(declare (fixnum left-index right-index half-length))
	(rotatef (aref sequence left-index)
		 (aref sequence right-index)))))
  sequence)




#+unicode
(progn
(defun decompose (string &key (compatibility t) (start 0) end darwinp)
  "Convert STRING to NFD (or NFKD).  If :darwinp is non-NIL, then
  characters in the ranges U2000-U2FFF, UF900-UFA6A, and U2F800-U2FA1D
  are not decomposed, as specified for Darwin pathnames."
  (declare (type string string))
  (let ((result (make-string (cond ((< (length string) 40)
				    (* 5 (length string)))
				   ((< (length string) 4096)
				    (* 2 (length string)))
				   (t (round (length string) 5/6)))))
	(fillptr 0))
    (declare (type kernel:index fillptr))
    (labels ((rec (string start end)
	       (declare (type simple-string string))
	       (do ((i start (1+ i)))
		   ((= i end))
		 (declare (type kernel:index i))
		 (multiple-value-bind (code wide) (codepoint string i)
		   (when wide (incf i))
		   (if (and darwinp
			    (or (or (<= #x2000 code #x2fff)
				    (<= #xf900 code #xfa6a)
				    (<= #x2f800 code #x2fa1d))))
		       (out code)
		       (let ((decomp (unicode-decomp code compatibility)))
			 (if decomp (rec decomp 0 (length decomp)) (out code)))))))
	     (out (code)
	       (multiple-value-bind (hi lo) (surrogates code)
		 (outch hi)
		 (when lo
		   (outch lo))
		 (let ((cc (unicode-combining-class code)))
		   (unless (zerop cc)
		     (order lo cc (- fillptr (if lo 3 2)))))))
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
      (with-one-string string start end offset-var
	(rec string start end))
      (shrink-vector result fillptr))))

(declaim (inline normalized-codepoint-p))
(defun normalized-codepoint-p (cp form)
  (ecase form
    (:nfc (unicode-nfc-qc cp))
    (:nfkc (unicode-nfkc-qc cp))
    (:nfd (unicode-nfd-qc cp))
    (:nfkd (unicode-nfkd-qc cp))))

;; Perform check to see if string is already normalized.  The Unicode
;; example can return YES, NO, or MAYBE.  For our purposes, only YES
;; is important, for which we return T.   For NO or MAYBE, we return NIL.
(defun normalized-form-p (string &optional (form :nfc))
  (declare (type (member :nfc :nfkc :nfd :nfkd) form)
	   (optimize (speed 3)))
  (with-string string
    (let ((last-class 0))
      (declare (type (integer 0 256) last-class))
      (do ((k start (1+ k)))
	  ((>= k end))
	(declare (type kernel:index k))
	(multiple-value-bind (ch widep)
	    (codepoint string k end)
	  (when widep (incf k))
	  ;; Handle ASCII specially
	  (unless (< ch 128)
	    (let ((class (unicode-combining-class ch)))
	      (declare (type (unsigned-byte 8) class))
	      (when (and (> last-class class) (not (zerop class)))
		;; Definitely not normalized
		(return-from normalized-form-p nil))
	      (let ((check (normalized-codepoint-p ch form)))
		(unless (eq check :y)
		  (return-from normalized-form-p nil)))
	      (setf last-class class)))))
      t)))


;; Compose a string in place.  The string must already be in decomposed form.
(defun %compose (target)
  (declare (type string target)
	   (optimize (speed 3)))
  (let ((len (length target))
	(starter-pos 0))
    (declare (type kernel:index starter-pos))
    (multiple-value-bind (starter-ch wide)
	(codepoint target 0 len)
      (let ((comp-pos (if wide 2 1))
	    (last-class (unicode-combining-class starter-ch)))
	(declare (type (integer 0 256) last-class)
		 (type kernel:index comp-pos))
	(unless (zerop last-class)
	  ;; Fix for strings starting with a combining character
	  (setf last-class 256))
	;; Loop on decomposed characters, combining where possible
	(do ((decomp-pos comp-pos (1+ decomp-pos)))
	    ((>= decomp-pos len))
	  (declare (type kernel:index decomp-pos))
	  (multiple-value-bind (ch wide)
	      (codepoint target decomp-pos len)
	    (when wide (incf decomp-pos))
	    (let ((ch-class (unicode-combining-class ch))
		  (composite (unicode-pairwise-composition starter-ch ch)))
	      (declare (type (integer 0 256) ch-class))
	      (cond ((and composite
			  (or (< last-class ch-class) (zerop last-class)))
		     ;; Note: As far as I know, there is no pairwise
		     ;; composition such that the composite character
		     ;; is outside the BMP but the starter-ch is
		     ;; inside the BMP.  Hence, it is always safe to
		     ;; replace the possible surrogate at starter-pos
		     ;; with another.  We won't accidentally replace
		     ;; the next character with our trailing surrogate
		     ;; character.
		     (multiple-value-bind (hi lo)
			 (surrogates composite)
		       (setf (aref target starter-pos) hi)
		       (when lo
			 (setf (aref target (1+ starter-pos)) lo))
		     (setf starter-ch composite)))
		    (t
		     (when (zerop ch-class)
		       (setf starter-pos comp-pos)
		       (setf starter-ch ch))
		     (setf last-class ch-class)
		     (multiple-value-bind (hi lo)
			 (surrogates ch)
		       (setf (aref target comp-pos) hi)
		       (when lo
			 (incf comp-pos)
			 (setf (aref target comp-pos) lo))
		       (incf comp-pos)))))))
	(shrink-vector target comp-pos)))))

(defun string-to-nfd (string)
  _N"Convert String to Unicode Normalization Form D (NFD) using the
  canonical decomposition.  The NFD string is returned"
  (decompose string :compatibility nil))

(defun string-to-nfkd (string)
  _N"Convert String to Unicode Normalization Form KD (NFKD) uisng the
  compatible decomposition form.  The NFKD string is returned."
  (decompose string :compatibility t))

(defun string-to-nfc (string)
  _N"Convert String to Unicode Normalization Form C (NFC).  If the
  string a simple string and is already normalized, the original
  string is returned."
  (if (normalized-form-p string :nfc)
      (if (simple-string-p string) string (coerce string 'simple-string))
      (coerce (if (normalized-form-p string :nfd)
		  (%compose (copy-seq string))
		  (%compose (string-to-nfd string)))
	      'simple-string)))

(defun string-to-nfkc (string)
  _N"Convert String to Unicode Normalization Form KC (NFKC).  If the
  string is a simple string and is already normalized, the original
  string is returned."
  (if (normalized-form-p string :nfkc)
      (if (simple-string-p string) string (coerce string 'simple-string))
      (coerce (if (normalized-form-p string :nfkd)
		  (%compose (copy-seq string))
		  (%compose (string-to-nfkd string)))
	      'simple-string)))
) ; end unicode

#-unicode  ;; Needed by package.lisp
(defun string-to-nfc (string)
  (if (simple-string-p string) string (coerce string 'simple-string)))


;; Some utilities
(defun codepoints-string (seq)
  "Convert a sequence of codepoints to a string.  Codepoints outside
  the basic multilingual plane (BMP) are converted into the
  corresponding surrogate pairs."
  (with-output-to-string (s)
    (map nil #'(lambda (c)
		 (multiple-value-bind (hi lo)
		     (surrogates c)
		   (write-char hi s)
		   (when lo (write-char lo s))))
	 seq)))

(defun string-codepoints (s)
  "Convert a string to a list of corresponding code points.  Surrogate
  pairs in the string are converted into the correspoinding
  codepoint."
  (declare (type simple-string s))
  (let ((len (length s))
	cp)
    (do ((idx 0))
	((>= idx len))
      (multiple-value-bind (c widep)
	  (codepoint s idx)
	(if widep
	    (incf idx 2)
	    (incf idx))
	(push c cp)))
    (nreverse cp)))
