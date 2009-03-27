;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/char.lisp,v 1.15.18.3.2.3 2009/03/27 04:14:10 rtoy Exp $")
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

(export '(char-code-limit standard-char-p graphic-char-p 
	  alpha-char-p upper-case-p lower-case-p both-case-p digit-char-p
	  alphanumericp char= char/= char< char> char<= char>= char-equal
	  char-not-equal char-lessp char-greaterp char-not-greaterp
	  char-not-lessp character char-code code-char char-upcase
	  char-downcase digit-char char-int char-name name-char))


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

#+unicode
(defstruct (unicode (:type vector))
  character name category num1 num2 num3 upper lower title)

;; Note: *unicode-data* is initialized with itself.  So how does it
;; get initialized to begin with?  A bootstrap files needs to be run
;; and rebuild-unicode-data will fill *unicode-data* with appropriate
;; values.  Likewise for *assigned-codepoints-bitmap*.
#+unicode
(defvar *unicode-data* #.*unicode-data*)

#+unicode
(declaim (inline unicode-data))
#+unicode
(defun unicode-data (thing)
  (gethash thing *unicode-data*))

#+unicode
(defvar *assigned-codepoints-bitmap* #.*assigned-codepoints-bitmap*)

(defun codepoint-assigned-p (codepoint)
  #-unicode (declare (ignore codepoint))
  #-unicode t
  #+unicode
  (= 1 (aref (the (simple-bit-vector #.char-code-limit)
	       *assigned-codepoints-bitmap*)
	     codepoint)))


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
		   "String is not of length one: ~S" (list object))))
      (symbol (if (= 1 (length (symbol-name object)))
		  (schar (symbol-name object) 0)
		  (do-error
		   "Symbol name is not of length one: ~S" (list object))))
      (t (do-error "~S cannot be coerced to a character." (list object))))))


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
	(let ((data (unicode-data char)))
	  (if data
	      (nstring-capitalize (substitute #\_ #\Space (unicode-name data)))
	      (format nil "U+~4,'0X" (char-code char)))))))

(defun name-char (name)
  "Given an argument acceptable to string, name-char returns a character
  object whose name is that symbol, if one exists.  Otherwise, () is returned."
  (if (and (stringp name) (> (length name) 2) (string-equal name "U+" :end1 2))
      (code-char (parse-integer name :radix 16 :start 1))
      (or (cdr (assoc (string name) char-name-alist :test #'string-equal))
	  #-unicode nil
	  #+unicode
	  ;; See if it's a valid Unicode character name
	  (let ((data (unicode-data
		       (nsubstitute #\Space #\_ (string-upcase name)))))
	    (if data
		(unicode-character data)
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
  argument is a printing character (space through ~ in ASCII), otherwise
  returns ()."
  (declare (character char))
  (and (typep char 'base-char)
       #-(and unicode (not unicode-bootstrap))
       (< 31
	  (char-code (the base-char char))
	  127)
       #+(and unicode (not unicode-bootstrap))
       (let ((data (unicode-data char)))
	 (and data (> (unicode-category data) 16)))))


(defun alpha-char-p (char)
  "The argument must be a character object.  Alpha-char-p returns T if the
   argument is an alphabetic character, A-Z or a-z; otherwise ()."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123)))
  #+(and unicode (not unicode-bootstrap))
  (let ((data (unicode-data char)))
    (and data (= (ldb (byte 3 4) (unicode-category data)) 1))))


(defun upper-case-p (char)
  "The argument must be a character object; upper-case-p returns T if the
   argument is an upper-case character, () otherwise."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (< 64
     (char-code char)
     91)
  #+(and unicode (not unicode-bootstrap))
  (let ((data (unicode-data char)))
    (and data (= (unicode-category data) #x1d))))
  


(defun lower-case-p (char)
  "The argument must be a character object; lower-case-p returns T if the 
   argument is a lower-case character, () otherwise."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (< 96
     (char-code char)
     123)
  #+(and unicode (not unicode-bootstrap))
  (let ((data (unicode-data char)))
    (and data (= (unicode-category data) #x16))))


(defun both-case-p (char)
  "The argument must be a character object.  Both-case-p returns T if the
  argument is an alphabetic character and if the character exists in
  both upper and lower case.  For ASCII, this is the same as Alpha-char-p."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123)))
  #+(and unicode (not unicode-bootstrap))
  (let ((data (unicode-data char)))
    (and data (or (= (unicode-category data) #x16)
		  (= (unicode-category data) #x1d)))))
  


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
    ;; Shortcut for ASCII digits and upper and lower case letters
    (or (< 47 m 58) (< 64 m 91) (< 96 m 123)
	#+(and unicode (not unicode-bootstrap))
	(let ((data (unicode-data char)))
	  (and data (or (= (unicode-category data) #x16)
			(= (unicode-category data) #x1D)))))))


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
;;; which loses case info.

#-(and unicode (not unicode-bootstrap))
(defmacro equal-char-code (character)
  `(let ((ch (char-code ,character)))
     (if (< 96 ch 123) (- ch 32) ch)))

#+(and unicode (not unicode-bootstrap))
(defmacro equal-char-code (character)
  `(let* ((char ,character)
	  (data (unicode-data char)))
     (char-code (or (and data (unicode-upper data)) char))))


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
  (let ((data (unicode-data char)))
    (if data
	(or (unicode-upper data) char)
	char)))
  

(defun char-downcase (char)
  "Returns CHAR converted to lower-case if that is possible."
  (declare (character char))
  #-(and unicode (not unicode-bootstrap))
  (if (upper-case-p char)
      (code-char (+ (char-code char) 32))
      char)
  #+(and unicode (not unicode-bootstrap))
  (let ((data (unicode-data char)))
    (if data
	(or (unicode-lower data) char)
	char)))

(defun digit-char (weight &optional (radix 10))
  "All arguments must be integers.  Returns a character object that
  represents a digit of the given weight in the specified radix.  Returns
  NIL if no such character exists."
  (declare (type (integer 2 36) radix) (type unsigned-byte weight))
  (and (typep weight 'fixnum)
       (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))


;; Rebuild *unicode-data* and *assigned-codepoints-bitmap*
#+unicode
(defun rebuild-unicode-data (&optional (unicode-data-file "target:i18n/UnicodeData.txt"))
  (dolist (range '((#x0000 . #x001F) (#x007F . #x009F) (#x3400 . #x4DB5)
		   (#x4E00 . #x9FBB) (#xAC00 . #xD7A3) (#xE000 . #xF8FF)
		   (#xDB80 . #xDBFF)))
    (loop for i from (car range) to (cdr range) do
	 (setf (aref *assigned-codepoints-bitmap* i) 1)))
  ;; Make the sure the hash table is an eql hash table.  This helps
  ;; during build because we don't need to do equal on characters.
  (setf *unicode-data* (make-hash-table :test 'eql :size 30000))
  (with-open-file (s unicode-data-file)
    (flet ((cat (x) (dpb (position (char x 0) "CLMNPSZ") (byte 3 4)
			 (position (char x 1) "cdefiklmnopstu")))
	   (num (x) (if (string= x "") nil x))
	   (chr (x) (if (string= x "") nil
			(let ((n (parse-integer x :radix 16)))
			  (and (< n char-code-limit) (code-char n))))))
      (loop for line = (read-line s nil) while line do
	   (let* ((split (loop for i = 0 then (1+ j)
			    as j = (position #\; line :start i)
			    collect (subseq line i j) while j))
		  (code (parse-integer (first split) :radix 16)))
	     (unless (or (>= code char-code-limit)
			 (char= (char (second split) 0) #\<))
	       (setf (aref *assigned-codepoints-bitmap* code) 1)
	       (let ((x (vector (code-char code)
				(nth 1 split)
				(cat (nth 2 split))
				(num (nth 6 split))
				(num (nth 7 split))
				(num (nth 8 split))
				(chr (nth 12 split))
				(chr (nth 13 split))
				(chr (nth 14 split)))))
		 (setf (gethash (code-char code) *unicode-data*) x)
		 (setf (gethash (second split) *unicode-data*) x))))))))
