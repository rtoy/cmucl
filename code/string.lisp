;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Functions to implement strings for Spice Lisp
;;; Written by David Dill
;;; Rewritten and currently maintained by Skef Wholey
;;;
;;; Runs in the standard Spice Lisp environment.
;;;
;;; ****************************************************************
;;;
(in-package 'lisp)
(export '(char schar string
	  string= string-equal string< string> string<= string>= string/=
	  string-lessp string-greaterp string-not-lessp string-not-greaterp
	  string-not-equal
	  make-string
	  string-trim string-left-trim string-right-trim
	  string-upcase
	  string-downcase string-capitalize nstring-upcase nstring-downcase
	  nstring-capitalize))

(eval-when (compile)

;;; %String returns its arg if it is a string, otherwise calls String.
;;;
(defmacro %string (thing)
  `(if (stringp ,thing) ,thing (string ,thing)))
)

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
	 (error "~S cannot be coerced to a string." x))))


;;; With-One-String is used to set up some string hacking things.  The keywords
;;; are parsed, and the string is hacked into a simple-string.

(eval-when (compile)

(defmacro with-one-string (string start end cum-offset &rest forms)
  (let ((data (gensym))
	(data-start (gensym))
	(data-end (gensym))
	(offset (gensym)))
    `(progn
      (if (symbolp ,string) (setq ,string (symbol-name ,string)))
      (if (array-header-p ,string)
	  (with-array-data ((,data ,string :offset-var ,offset)
			    (,data-start ,start)
			    (,data-end (or ,end
					   (%primitive header-ref ,string
						       %array-fill-pointer-slot))))
			   (psetq ,string ,data
				  ,cum-offset ,offset
				  ,start ,data-start
				  ,end ,data-end))
	  (if (not ,end) (setq ,end (length (the simple-string ,string)))))
      ,@forms)))

)

;;; With-String is like With-One-String, but doesn't parse keywords.

(eval-when (compile)

(defmacro with-string (string &rest forms)
  `(let ((start 0)
	 (end ()))
     (if (symbolp ,string) (setq ,string (symbol-name ,string)))
     (if (array-header-p ,string)
	 (with-array-data ((data ,string)
			   (data-start start)
			   (data-end (%primitive header-ref ,string
						 %array-fill-pointer-slot)))
	   (psetq ,string data
		  start data-start
		  end data-end))
	 (setq end (length (the simple-string ,string))))
     ,@forms))

)

;;; With-Two-Strings is used to set up string comparison operations.  The
;;; keywords are parsed, and the strings are hacked into simple-strings.

(eval-when (compile)

(defmacro with-two-strings (string1 string2 start1 end1 cum-offset-1
				    start2 end2 &rest forms)
  (let ((data (gensym))
	(data-start (gensym))
	(data-end (gensym))
	(offset (gensym)))
    `(progn
      (if (symbolp ,string1) (setq ,string1 (symbol-name ,string1)))
      (if (symbolp ,string2) (setq ,string2 (symbol-name ,string2)))
      (if (array-header-p ,string1)
	  (with-array-data ((,data ,string1 :offset-var ,offset)
			    (,data-start ,start1)
			    (,data-end (or ,end1
					   (%primitive header-ref ,string1
						       %array-fill-pointer-slot))))
			   (psetq ,string1 ,data
				  ,cum-offset-1 ,offset
				  ,start1 ,data-start
				  ,end1 ,data-end))
	  (if (not ,end1) (setq ,end1 (length (the simple-string ,string1)))))
      (if (array-header-p ,string2)
	  (with-array-data ((,data ,string2)
			    (,data-start ,start2)
			    (,data-end (or ,end2
					   (%primitive header-ref ,string2
						       %array-fill-pointer-slot))))
			   (psetq ,string2 ,data
				  ,start2 ,data-start
				  ,end2 ,data-end))
	  (if (not ,end2) (setq ,end2 (length (the simple-string ,string2)))))
      ,@forms)))

)

(defun char (string index)
  "Given a string and a non-negative integer index less than the length of
  the string, returns the character object representing the character at
  that position in the string."
  (char string index))

(defun %charset (string index new-el)
  (setf (char string index) new-el))

(defun schar (string index)
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (schar string index))

(defun %scharset (string index new-el)
  (setf (schar string index) new-el))

(defun string=* (string1 string2 start1 end1 start2 end2)
  (let ((offset1 0))
    (with-two-strings string1 string2 start1 end1 offset1 start2 end2
      (not (%sp-string-compare string1 start1 end1 string2 start2 end2)))))


(defun string/=* (string1 string2 start1 end1 start2 end2)
  (let ((offset1 0))
    (with-two-strings string1 string2 start1 end1 offset1 start2 end2
      (let ((comparison (%sp-string-compare string1 start1 end1
					    string2 start2 end2)))
	(if comparison (- (the fixnum comparison) offset1))))))

(eval-when (compile eval)

;;; Lessp is true if the desired expansion is for string<* or string<=*.
;;; Equalp is true if the desired expansion is for string<=* or string>=*.
(defmacro string<>=*-body (lessp equalp)
  (let ((offset1 (gensym)))
    `(let ((,offset1 0))
       (declare (fixnum ,offset1))
       (with-two-strings string1 string2 start1 end1 ,offset1 start2 end2
	 (let ((index (%sp-string-compare string1 start1 end1
					  string2 start2 end2)))
	   (if index
	       (cond ((= (the fixnum index)
			 ,(if lessp `(the fixnum end1) `(the fixnum end2)))
		      (- (the fixnum index) ,offset1))
		     ((= (the fixnum index)
			 ,(if lessp `(the fixnum end2) `(the fixnum end1)))
		      nil)
		     ((,(if lessp 'char< 'char>)
		       (schar string1 index)
		       (schar string2 (+ (the fixnum index) (- start2 start1))))
		      (- (the fixnum index) ,offset1))
		     (t nil))
	       ,(if equalp `(- (the fixnum end1) ,offset1) 'nil)))))))
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
  (let ((offset1 0))
    (with-two-strings string1 string2 start1 end1 offset1 start2 end2
      (let ((slen1 (- (the fixnum end1) start1))
	    (slen2 (- (the fixnum end2) start2)))
	(declare (fixnum slen1 slen2))
	(if (or (minusp slen1) (minusp slen2))
	    ;;prevent endless looping later.
	    (error "Improper bounds for string comparison."))
	(if (= slen1 slen2)
	    ;;return () immediately if lengths aren't equal.
	    (string-not-equal-loop 1 t nil)))))) 

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (let ((offset1 0))
    (declare (fixnum offset1))
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
	       (string-not-equal-loop 2 (- index1 offset1))))))))
 


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
    `(let ((offset1 0))
       (declare (fixnum offset1))
       (with-two-strings string1 string2 start1 end1 offset1 start2 end2
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
		     (return ())))))))))

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


(defun make-string (count &key ((:initial-element fill-char)))
  "Given a character count and an optional fill character, makes and returns
   a new string Count long filled with the fill character."
  (declare (fixnum count))
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
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string))
	(offset 0))
    (declare (fixnum slen offset))
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
	  (setf (schar newstring new-index)
		(char-upcase (schar string index))))
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
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string))
	(offset 0))
    (declare (fixnum slen offset))
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
	  (setf (schar newstring new-index)
		(char-downcase (schar string index))))
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
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string))
	(offset 0))
    (declare (fixnum slen offset))
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
		 (setq char (char-upcase char))
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
  (let ((save-header string)
	offset)
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setf (schar string index) (char-upcase (schar string index)))))
    save-header))

(defun nstring-downcase (string &key (start 0) end)
  "Given a string, returns that string with all upper case alphabetic
  characters converted to lowercase."
  (declare (fixnum start))
  (let ((save-header string)
	offset)
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setf (schar string index) (char-downcase (schar string index)))))
    save-header)))

(defun nstring-capitalize (string &key (start 0) end)
  "Given a string, returns that string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let ((save-header string)
	offset)
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
	       (setf (schar string index) (char-upcase char))
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
