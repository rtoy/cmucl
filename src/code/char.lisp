;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/char.lisp $")
;;;
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
(in-package "LISP")

(intl:textdomain "cmucl")

(export '(char-code-limit standard-char-p graphic-char-p 
	  alpha-char-p upper-case-p lower-case-p both-case-p digit-char-p
	  alphanumericp char= char/= char< char> char<= char>= char-equal
	  char-not-equal char-lessp char-greaterp char-not-greaterp
	  char-not-lessp character char-code code-char char-upcase
	  char-titlecase title-case-p
	  char-downcase digit-char char-int char-name name-char
	  codepoint-limit codepoint))


;;; Compile some trivial character operations via inline expansion:
;;;
(declaim (inline standard-char-p graphic-char-p alpha-char-p
		 upper-case-p lower-case-p both-case-p alphanumericp
		 char-int))

(declaim (maybe-inline digit-char-p digit-weight))

(defconstant char-code-limit
  #-unicode 256
  #+unicode 65536
  "The upper exclusive bound on values produced by CHAR-CODE.")

(deftype char-code ()
  `(integer 0 (,char-code-limit)))

(defconstant codepoint-limit
  #x110000
  "The upper exclusive bound on the value of a Unicode codepoint")

;;; The range of a Unicode code point
(deftype codepoint ()
  `(integer 0 (,codepoint-limit)))

;; This MUST be greater than or equal to 127!
(defconstant +unicode-lower-limit+
  191
  "A character code strictly larger than this is handled using Unicode rules.")


(macrolet ((frob (char-names-list)
	     (collect ((results))
	       (dolist (code char-names-list)
		 (destructuring-bind (ccode names)
		     code
		   (dolist (name names)
		     (results (cons name (code-char ccode))))))
	       `(defparameter char-name-alist ',(results)
  "This is the alist of (character-name . character) for characters with
  long names.  The first name in this list for a given character is used
  on typeout and is the preferred form for input."))))
  ;; Note: the char-name listed here should be what string-capitalize
  ;; would produce.  This is needed to match what format ~:C would
  ;; produce.
  (frob ((#x00 ("Null" "^@" "NUL"))
	 (#x01 ("^A" "SOH"))
	 (#x02 ("^B" "STX"))
	 (#x03 ("^C" "ETX"))
	 (#x04 ("^D" "EOT"))
	 (#x05 ("^E" "ENQ"))
	 (#x06 ("^F" "ACK"))
	 (#x07 ("Bell" "^g" "BEL"))
	 (#x08 ("Backspace" "^h" "BS"))
	 (#x09 ("Tab" "^i" "HT"))
	 (#x0A ("Newline" "Linefeed" "^j" "LF" "NL" ))
	 (#x0B ("Vt" "^k"))
	 (#x0C ("Page" "^l" "Form" "Formfeed" "FF" "NP"))
	 (#x0D ("Return" "^m" "RET" "CR"))
	 (#x0E ("^N" "SO"))
	 (#x0F ("^O" "SI"))
	 (#x10 ("^P" "DLE"))
	 (#x11 ("^Q" "DC1"))
	 (#x12 ("^R" "DC2"))
	 (#x13 ("^S" "DC3"))
	 (#x14 ("^T" "DC4"))
	 (#x15 ("^U" "NAK"))
	 (#x16 ("^V" "SYN"))
	 (#x17 ("^W" "ETB"))
	 (#x18 ("^X" "CAN"))
	 (#x19 ("^Y" "EM" "EOM"))
	 (#x1A ("^Z" "SUB"))
	 (#x1B ("Escape" "^[" "Altmode" "ESC" "Alt"))
	 (#x1C ("Is4" "FS" "^\\"))
	 (#x1D ("Is3" "GS" "^]"))
	 (#x1E ("Is2" "RS" "^^"))
	 (#x1F ("Is1" "US" "^_"))
	 (#x20 ("Space" "SP" "SPC"))
	 (#x7f ("Rubout" "Delete" "DEL")))))


;;;; Accessor functions:

(defun char-code (char)
  "Returns the integer code of CHAR."
  (etypecase char
    (base-char (char-code (truly-the base-char char)))))


(defun char-int (char)
  "Returns the integer code of CHAR.  This is the same as char-code, as
   CMU Common Lisp does not implement character bits or fonts."
  (char-code char))


(defun code-char (code)
  "Returns the character with the code CODE."
  (declare (type char-code code))
  (code-char code))


(defun character (object)
  "Coerces its argument into a character object if possible.  Accepts
  characters, strings and symbols of length 1."
  (flet ((do-error (control args)
	   (error 'simple-type-error
		  :datum object
		  ;;?? how to express "symbol with name of length 1"?
		  :expected-type '(or character (string 1))
		  :format-control control
		  :format-arguments args)))
    (typecase object
      (character object)
      (string (if (= 1 (length (the string object)))
		  (char object 0)
		  (do-error
		   (intl:gettext "String is not of length one: ~S") (list object))))
      (symbol (if (= 1 (length (symbol-name object)))
		  (schar (symbol-name object) 0)
		  (do-error
		   (intl:gettext "Symbol name is not of length one: ~S") (list object))))
      (t (do-error (intl:gettext "~S cannot be coerced to a character.") (list object))))))


(defun char-name (char)
  "Given a character object, char-name returns the name for that
  object (a symbol)."
  (let ((name (car (rassoc char char-name-alist))))
    (if name
	name
	#-unicode nil
	#+unicode
	;; Return the Unicode name of the character,
	;;   or U+xxxx if it doesn't have a name
	(let* ((code (char-code char))
	       (name (unicode-name code)))
	  (if name
	      (nstring-capitalize (nsubstitute #\_ #\Space name))
	      (format nil "U+~4,'0X" code))))))

(defun name-char (name)
  "Given an argument acceptable to string, name-char returns a character
  object whose name is that symbol, if one exists, otherwise NIL."
  (if (and (stringp name) (> (length name) 2) (string-equal name "U+" :end1 2))
      (code-char (parse-integer name :radix 16 :start 1))
      (or (cdr (assoc (string name) char-name-alist :test #'string-equal))
	  #-unicode nil
	  #+unicode
	  (let ((code (unicode-name-to-codepoint
		       (nsubstitute #\Space #\_ (string-upcase name)))))
	    (if code
		(code-char code)
		nil)))))


;;;; Predicates:

(defun standard-char-p (char)
  "The argument must be a character object.  Standard-char-p returns T if the
   argument is a standard character -- one of the 95 ASCII printing characters
   or <return>."
  (declare (character char))
  (and (typep char 'base-char)
       (let ((n (char-code (the base-char char))))
	 (or (< 31 n 127)
	     (= n 10)))))

(defun %standard-char-p (thing)
  "Return T if and only if THING is a standard-char.  Differs from
  standard-char-p in that THING doesn't have to be a character."
  (and (characterp thing) (standard-char-p thing)))

(defun graphic-char-p (char)
  "The argument must be a character object.  Graphic-char-p returns T if the
  argument is a printing character, otherwise returns NIL."
  (declare (character char))
  (and (typep char 'base-char)
       (let ((m (char-code (the base-char char))))
	 (or (< 31 m 127)
	     #+(and unicode (not unicode-bootstrap))
	     (and (> m +unicode-lower-limit+)
		  (>= (unicode-category m) +unicode-category-graphic+))))))


(defun alpha-char-p (char)
  "The argument must be a character object.  Alpha-char-p returns T if the
  argument is an alphabetic character; otherwise NIL."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123)
	#+(and unicode (not unicode-bootstrap))
	(and (> m +unicode-lower-limit+)
	     (<= +unicode-category-letter+ (unicode-category m)
		 (+ +unicode-category-letter+ #x0F))))))


(defun upper-case-p (char)
  "The argument must be a character object; upper-case-p returns T if the
  argument is an upper-case character, NIL otherwise."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91)
	#+(and unicode (not unicode-bootstrap))
	(and (> m +unicode-lower-limit+)
	     (= (unicode-category m) +unicode-category-upper+)))))


(defun lower-case-p (char)
  "The argument must be a character object; lower-case-p returns T if the 
  argument is a lower-case character, NIL otherwise."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 96 m 123)
	#+(and unicode (not unicode-bootstrap))
	(and (> m +unicode-lower-limit+)
             ;; We don't want 223 to be a lower-case letter because
             ;; CHAR-UPCASE returns the same character instead of the
             ;; upper-case version.
             (/= m 223)
	     (= (unicode-category m) +unicode-category-lower+)))))

(defun title-case-p (char)
  "The argument must be a character object; title-case-p returns T if the
  argument is a title-case character, NIL otherwise."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91)
	#+(and unicode (not unicode-bootstrap))
	(and (> m +unicode-lower-limit+)
	     (= (unicode-category m) +unicode-category-title+)))))


(defun both-case-p (char)
  "The argument must be a character object.  Both-case-p returns T if the
  argument is an alphabetic character and if the character exists in
  both upper and lower case.  For ASCII, this is the same as Alpha-char-p."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123)
	#+(and unicode (not unicode-bootstrap))
	(and (> m +unicode-lower-limit+)
             (/= m 223)
	     (<= +unicode-category-upper+
		 (unicode-category m)
		 +unicode-category-lower+)))))


(defun digit-char-p (char &optional (radix 10.))
  "If char is a digit in the specified radix, returns the fixnum for
  which that digit stands, else returns NIL.  Radix defaults to 10
  (decimal)."
  (declare (character char) (type (integer 2 36) radix))
  (let ((m (- (char-code char) 48)))
    (declare (fixnum m))
    (cond ((<= radix 10.)
	   ;; Special-case decimal and smaller radices.
	   (if (and (>= m 0) (< m radix))  m  nil))
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
    ;; Shortcut for ASCII digits and upper and lower case ASCII letters
    (or (< 47 m 58) (< 64 m 91) (< 96 m 123)
	#+(and unicode (not unicode-bootstrap))
	(and (> m +unicode-lower-limit+)
	     (<= +unicode-category-letter+ (unicode-category m)
		 (+ +unicode-category-letter+ #x0F))))))


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
;;; which loses case info.  We convert to lower case

(defmacro equal-char-code (character)
  `(let ((ch (char-code ,character)))
     ;; Handle ASCII separately for bootstrapping and for unidata missing.
     (if (< 64 ch 91)
	 (+ ch 32)
	 #-(and unicode (not unicode-bootstrap))
	 ch
	 #+(and unicode (not unicode-bootstrap))
	 (if (> ch +unicode-lower-limit+) (unicode-lower ch) ch))))


(defun char-equal (character &rest more-characters)
  "Returns T if all of its arguments are the same character.
   Case is ignored."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (= (equal-char-code (car clist))
	       (equal-char-code character))
      (return nil))))


(defun char-not-equal (character &rest more-characters)
  "Returns T if no two of its arguments are the same character.
   Case is ignored."
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
   Case is ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))


(defun char-greaterp (character &rest more-characters)
  "Returns T if its arguments are in strictly decreasing alphabetic order.
   Case is ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))


(defun char-not-greaterp (character &rest more-characters)
  "Returns T if its arguments are in strictly non-decreasing alphabetic order.
   Case is ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))


(defun char-not-lessp (character &rest more-characters)
  "Returns T if its arguments are in strictly non-increasing alphabetic order.
   Case is ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))




;;;; Miscellaneous functions:

(defun char-upcase (char)
  "Returns CHAR converted to upper-case if that is possible."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (if (lower-case-p char)
      (code-char (- (char-code char) 32))
      char)
  #+(and unicode (not unicode-bootstrap))
  (let ((m (char-code char)))
    (cond ((< 96 m 123) (code-char (- m 32)))
          ((> m +unicode-lower-limit+)
           (if (member (unicode-category m) '(92 32 75 109))
               char
               (code-char (unicode-upper m))))
	  (t char))))

(defun char-titlecase (char)
  "Returns CHAR converted to title-case if that is possible."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (if (lower-case-p char)
      (code-char (- (char-code char) 32))
      char)
  #+(and unicode (not unicode-bootstrap))
  (let ((m (char-code char)))
    (cond ((> m +unicode-lower-limit+) (code-char (unicode-title m)))
	  ((< 96 m 123) (code-char (- m 32)))
	  (t char))))

(defun char-downcase (char)
  "Returns CHAR converted to lower-case if that is possible."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (if (upper-case-p char)
      (code-char (+ (char-code char) 32))
      char)
  #+(and unicode (not unicode-bootstrap))
  (let ((m (char-code char)))
    (cond ((> m +unicode-lower-limit+)
           (if (member (unicode-category m) '(92 75 109))
               char
               (code-char (unicode-lower m))))
	  ((< 64 m 91) (code-char (+ m 32)))
	  (t char))))

(defun digit-char (weight &optional (radix 10))
  "All arguments must be integers.  Returns a character object that
  represents a digit of the given weight in the specified radix.  Returns
  NIL if no such character exists."
  (declare (type (integer 2 36) radix) (type unsigned-byte weight))
  (and (typep weight 'fixnum)
       (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))
