;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Character functions for Spice Lisp.  Part of the standard Spice Lisp
;;; environment.
;;;
;;; This file assumes the use of ASCII codes and the specific character formats
;;; used in Spice Lisp and Vax Common Lisp.  It is optimized for performance
;;; rather than for portability and elegance, and may have to be rewritten if
;;; the character representation is changed.
;;;
;;; Written by Guy Steele.
;;; Rewritten by David Dill.
;;; Hacked up for speed by Scott Fahlman.
;;; Font support flushed and type hackery rewritten by Rob MacLachlan.
;;;
(in-package 'lisp)

(export '(char-code-limit char-font-limit char-bits-limit standard-char-p
	  graphic-char-p string-char-p alpha-char-p upper-case-p lower-case-p
	  both-case-p digit-char-p alphanumericp char= char/= char< char>
	  char<= char>= char-equal char-not-equal char-lessp char-greaterp
	  char-not-greaterp char-not-lessp character char-code char-bits
	  char-font code-char make-char char-upcase char-downcase
	  digit-char char-int int-char char-name name-char char-control-bit
	  char-meta-bit char-hyper-bit char-super-bit char-bit set-char-bit))


;;; Compile some trivial character operations via inline expansion:
;;;
(proclaim '(inline standard-char-p
		   graphic-char-p string-char-p alpha-char-p upper-case-p
		   lower-case-p both-case-p alphanumericp char-bits
		   char-int))


(defconstant char-code-limit 256
  "The upper exclusive bound on values produced by CHAR-CODE.")
(defconstant char-font-limit 1
  "The upper exclusive bound on values produced by CHAR-FONT.")
(defconstant char-bits-limit 256
  "The upper exclusive bound on values produced by CHAR-BITS.")

(defconstant char-control-bit 1
  "This bit indicates a control character.")
(defconstant char-meta-bit 2
  "This bit indicates a meta character.")
(defconstant char-super-bit 4
  "This bit indicates a super character.")
(defconstant char-hyper-bit 8
  "This bit indicates a hyper character.")


(defparameter char-name-alist
	`(("NULL" . ,(code-char 0))
	  ("BELL" . ,(code-char 7))
	  ("BACKSPACE" . ,(code-char 8)) ("BS" . ,(code-char 8))
	  ("TAB" . ,(code-char 9))
	  ("LINEFEED" . ,(code-char 10)) ("LF" . ,(code-char 10))
	  ("VT" . ,(code-char 11))
	  ("PAGE" . ,(code-char 12)) ("FORM" . ,(code-char 12))
	  ("FORMFEED" . ,(code-char 12)) ("FF" . ,(code-char 12))
	  ("RETURN" . ,(code-char 13)) ("NL" . ,(code-char 10))
	  ("NEWLINE" . ,(code-char 10))  ("CR" . ,(code-char 13))
	  ("ALTMODE" . ,(code-char 27)) ("ALT" . ,(code-char 27))
	  ("ESCAPE" . ,(code-char 27)) ("ESC" . ,(code-char 27))
	  ("SPACE" . ,(code-char 32)) ("SP" . ,(code-char 32))
	  ("RUBOUT" . ,(code-char 127)) ("DELETE" . ,(code-char 127)))
  "This is the alist of (character-name . character) for characters
  with long names.  The first name in this list for a given character
  is used on typeout and is the preferred form for input.")


;;;; Accessor functions:

(defun char-code (char)
  "Given a character object argument, char-code returns the code attribute
   of that object as a non-negative integer."
  (ldb %character-code-byte (char-int char)))

(defun char-bits (char)
  "Given a character object argument, char-code returns the bits attribute
   of that object as a non-negative integer."
  (ldb %character-control-byte (char-int char)))

(defun char-font (char)
  "Given a character object argument, char-code returns the font attribute
   of that object as 0."
  (declare (ignore char))
  0)

(defun char-int (char)
  "The argument must be a character-object.  Returns the font, bits, and
  code fields as a single non-negative integer.  Implementation dependent.
  Used mostly for hashing."
  (declare (character char))
  (%primitive make-fixnum char))


(defun int-char (n)
  "Performs the inverse of char-int.  The argument must be a non-negative
  integer of the appropriate size.  It is turned into a character object."
  (declare (type unsigned-byte n))
  (cond ((or (not (fixnump n))
	     (not (<= 0 (the fixnum n) %character-int-mask)))
	 nil)
	((zerop (ldb %character-control-byte (the fixnum n)))
	 (%primitive make-immediate-type n %string-char-type))
	(t
	 (%primitive make-immediate-type n %bitsy-char-type))))


(defun code-char (code &optional (bits 0) (font 0))
  "All three arguments, must be non-negative integers; the last two are 
   optional with default values of 0 (for the bits and font attributes).
   Returns a character object with the specified code, bits, and font,
   or returns NIL if this is not possible."
  (cond ((not (and (< -1 code char-code-limit) (zerop font))) nil)
	((zerop bits) (code-char code))
	((< -1 bits char-bits-limit)
	 (%primitive make-immediate-type 
		     (dpb bits %character-control-byte code)
		     %bitsy-char-type))
	(t nil)))


(defun character (object)
  "Coerces its argument into a character object if possible.  Accepts
  characters, strings and symbols of length 1, and integers."
  (typecase object
    (character object)
    (integer (int-char object))
    (string (if (= 1 (the fixnum (length (the string object))))
		(char object 0)
		(error "String is not of length one: ~S" object)))
    (symbol (if (= 1 (the fixnum (length (symbol-name object))))
		(schar (symbol-name object) 0)
		(error "Symbol name is not of length one: ~S" object)))
    (t
     (error "~S cannot be coerced to a character."))))


(defun make-char (char &optional (bits 0) (font 0))
  "Replaces the bits and font attributes of the specified character with
  those supplied by the user as fixnums.  Bits and font both default to 0."
  (declare (character char))
  (and (< -1 bits char-bits-limit)
       (zerop font)
       (int-char (dpb bits %character-control-byte (char-code char)))))


(defun char-name (char)
  "Given a character object, char-name returns the name for that
  object (a symbol)."
  (car (rassoc char char-name-alist)))


(defun name-char (name)
  "Given an argument acceptable to string, name-char returns a character
  object whose name is that symbol, if one exists.  Otherwise, () is returned."
  (cdr (assoc (string name) char-name-alist :test #'string-equal)))


(defun char-bit (char name)
  "Returns T if the named bit is set in character object CHAR.  Else,
  returns NIL.  Legal names are :CONTROL, :META, :HYPER, and :SUPER."
  (logtest (case name
	     (:control char-control-bit)
	     (:meta char-meta-bit)
	     (:hyper char-hyper-bit)
	     (:super char-super-bit))
	   (char-bits char)))


(defun set-char-bit (char name newvalue)
  "Returns a character just like CHAR except that the named bit is
  set or cleared, according to whether NEWVALUE is non-null or NIL.
  Legal bit names are :CONTROL, :META, :HYPER, and :SUPER."
  (let ((bit (case name
	      (:control char-control-bit)
	      (:meta char-meta-bit)
	      (:hyper char-hyper-bit)
	      (:super char-super-bit)
	      (t 0))))
    (code-char (char-code char)
	       (if newvalue
		   (logior bit (char-bits char))
		   (logand (lognot bit) (char-bits char)))
	       (char-font char))))


;;;; Predicates:

(defun standard-char-p (char)
  "The argument must be a character object.  Standard-char-p returns T if the
   argument is a standard character -- one of the 95 ASCII printing characters
   or <return>."
  (declare (character char))
  (and (typep char 'string-char)
       (let ((n (char-code (the string-char char))))
	 (or (< 31 n 127)
	     (= n 13)
	     (= n 10)))))


(defun graphic-char-p (char)
  "The argument must be a character object.  Graphic-char-p returns T if the
  argument is a printing character (space through ~ in ASCII), otherwise
  returns ()."
  (declare (character char))
  (and (typep char 'string-char)
       (< 31
	  (char-code (the string-char char))
	  127)))


(defun string-char-p (char)
  "The argument must be a character object.  String-char-p returns T if the
   argument can be stored in a string."
  (declare (character char))
  (typep char 'string-char))


(defun alpha-char-p (char)
  "The argument must be a character object.  Alpha-char-p returns T if the
   argument is an alphabetic character, A-Z or a-z; otherwise ()."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))


(defun upper-case-p (char)
  "The argument must be a character object; upper-case-p returns T if the
   argument is an upper-case character, () otherwise."
  (declare (character char))
  (< 64
     (char-code char)
     91))


(defun lower-case-p (char)
  "The argument must be a character object; lower-case-p returns T if the 
   argument is a lower-case character, () otherwise."
  (declare (character char))
  (< 96
     (char-code char)
     123))


(defun both-case-p (char)
  "The argument must be a character object.  Both-case-p returns T if the
  argument is an alphabetic character and if the character exists in
  both upper and lower case.  For ASCII, this is the same as Alpha-char-p."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))


(defun digit-char-p (char &optional (radix 10.))
  "If char is a digit in the specified radix, returns the fixnum for
  which that digit stands, else returns NIL.  Radix defaults to 10
  (decimal)."
  (declare (character char))
  (let ((m (- (char-code char) 48)))
    (cond ((<= radix 10.)
	   ;; Special-case decimal and smaller radices.
	   (if (and (>= m 0) (< m radix))  m  nil))
	  ;; Cannot handle radix past Z.
	  ((> radix 36)
	   (error "~S too large to be an input radix."  radix))
	  ;; Digits 0 - 9 are used as is, since radix is larger.
	  ((and (>= m 0) (< m 10)) m)
	  ;; Check for upper case A - Z.
	  ((and (>= (setq m (- m 7)) 10) (< m radix)) m)
	  ;; Also check lower case a - z.
	  ((and (>= (setq m (- m 32)) 10) (< m radix)) m)
	  ;; Else, fail.
	  (t nil))))


(defun alphanumericp (char)
  "Given a character-object argument, alphanumericp returns T if the
   argument is either numeric or alphabetic."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 47 m 58) (< 64 m 91) (< 96 m 123))))


(defun char= (character &rest more-characters)
  "Returns T if all of its arguments are the same character."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (eq (car clist) character) (return nil))))


(defun char/= (character &rest more-characters)
  "Returns T if no two of its arguments are the same character."
  (do* ((head character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (do* ((l list (cdr l)))                  ;inner loop returns T 
		 ((atom l) T)			     ; iff head /= rest.
	      (if (eq head (car l)) (return nil)))
      (return nil))))


(defun char< (character &rest more-characters)
  "Returns T if its arguments are in strictly increasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (char-int c)
	       (char-int (car list)))
      (return nil))))


(defun char> (character &rest more-characters)
  "Returns T if its arguments are in strictly decreasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (char-int c)
	       (char-int (car list)))
      (return nil))))


(defun char<= (character &rest more-characters)
  "Returns T if its arguments are in strictly non-decreasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (char-int c)
		(char-int (car list)))
      (return nil))))


(defun char>= (character &rest more-characters)
  "Returns T if its arguments are in strictly non-increasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (char-int c)
		(char-int (car list)))
      (return nil))))



;;; Equal-Char-Code is used by the following functions as a version of char-int
;;;  which loses font, bits, and case info.

(defmacro equal-char-code (character)
  `(let ((ch (char-code ,character)))
     (if (< 96 ch 123) (- ch 32) ch)))



(defun char-equal (character &rest more-characters)
  "Returns T if all of its arguments are the same character.
  Font, bits, and case are ignored."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (= (equal-char-code (car clist))
	       (equal-char-code character))
      (return nil))))


(defun char-not-equal (character &rest more-characters)
  "Returns T if no two of its arguments are the same character.
   Font, bits, and case are ignored."
  (do* ((head character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (do* ((l list (cdr l)))
		 ((atom l) T)
	      (if (= (equal-char-code head)
		     (equal-char-code (car l)))
		  (return nil)))
      (return nil))))


(defun char-lessp (character &rest more-characters)
  "Returns T if its arguments are in strictly increasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))


(defun char-greaterp (character &rest more-characters)
  "Returns T if its arguments are in strictly decreasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))


(defun char-not-greaterp (character &rest more-characters)
  "Returns T if its arguments are in strictly non-decreasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))


(defun char-not-lessp (character &rest more-characters)
  "Returns T if its arguments are in strictly non-increasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))




;;;; Miscellaneous functions:

(defun char-upcase (char)
  "Returns a character with the same bits and font as the input character,
  converted to upper-case if that is possible."
  (declare (character char))
  (cond ((typep char 'string-char)
	 (char-upcase (the string-char char)))
	((lower-case-p char)
	 (int-char (- (char-int char) 32)))
	(t
	 char)))


(defun char-downcase (char)
  "Returns a character with the same bits and font as the input character,
  converted to lower-case if that is possible."
  (declare (character char))
  (cond ((typep char 'string-char)
	 (char-downcase (the string-char char)))
	((upper-case-p char)
	 (int-char (+ (char-int char) 32)))
	(t
	 char)))


(defun digit-char (weight &optional (radix 10) (font 0))
  "All arguments must be integers.  Returns a character object that
  represents a digit of the given weight in the specified radix.  Returns
  NIL if no such character exists.  The character will have the specified
  font attributes."
  (and (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight))
		  0 font)))
