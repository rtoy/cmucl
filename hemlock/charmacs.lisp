;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Implementation specific character-hacking macros and constants.
;;;
(in-package 'hemlock-internals)
(export ' (syntax-char-code-limit command-char-bits-limit
	   command-char-code-limit search-char-code-limit
	   do-alpha-chars))

;;; This file contains various constants and macros which are
;;; implementation or ASCII dependant.  In particular it contains
;;; all the character implementation parameters such as
;;; Command-Char-Bits-Limit, and contains various versions
;;; of char-code which don't check types and omit the top bit
;;; so that various structures can be allocated 128 long instead
;;; of 256, and we don't get errors if a loser visits a binary file.

;;; There are so many different constants and macros that do the same
;;; thing because in principle the char-code-limit for the syntax
;;; functions is independant of that for the searching functions, etc.

;;; This file also contains code which adds any implementation specific
;;; character names to the char file's Char-Name-Alist so that there
;;; is a reasonable read-syntax and print-representation for all
;;; characters a user might run across.


;;;; Stuff for the Syntax table functions (syntax)

(defconstant syntax-char-code-limit 128
  "The highest char-code which a character argument to the syntax
  table functions may have.")
(defconstant syntax-char-code-mask #x+7f
  "Mask we AND with characters given to syntax table functions to blow away
  bits we don't want.")
(defmacro syntax-char-code (char)
  `(logand syntax-char-code-mask (char-code ,char)))

;;;; Stuff for the command interpreter (interp)
;;;
;;;    On the Perq we have bits for command bindings, on the VAX there 
;;; aren't.  The code to interpret them is conditionally compiled
;;; so that the VAX isnt't slowed down.
;;;
;;; Make command-char-code-limit 256 instead of 128 for X keyboard scan-codes.
(defconstant command-char-code-limit 256
  "The upper bound on character codes supported for key bindings.")
(defconstant command-char-bits-limit 16
  "The maximum value of character bits supported for key bindings.")
(defmacro key-char-bits (char)
  `(logand (char-bits ,char) #xF))
(defmacro key-char-code (char)
  `(char-code ,char))


;;;; Stuff used by the searching primitives (search)
;;;
(defconstant search-char-code-limit 128
  "The exclusive upper bound on significant char-codes for searching.")
(defmacro search-char-code (ch)
  `(logand (char-code ,ch) #x+7F))
;;;
;;;    search-hash-code must be a function with the following properties:
;;; given any character it returns a number between 0 and 
;;; search-char-code-limit, and the same hash code must be returned 
;;; for the upper and lower case forms of each character.
;;;    In ASCII this is can be done by ANDing out the 5'th bit.
;;;
(defmacro search-hash-code (ch)
  `(logand (char-code ,ch) #x+5F))

;;; Doesn't do anything special, but it should fast and not waste any time
;;; checking type and whatnot.
(defmacro search-char-upcase (ch)
  `(char-upcase (the base-character ,ch)))



;;;; DO-ALPHA-CHARS.

;;; ALPHA-CHARS-LOOP loops from start-char through end-char binding var
;;; to the alphabetic characters and executing body.  Note that the manual
;;; guarantees lower and upper case char codes to be separately in order,
;;; but other characters may be interspersed within that ordering.
(defmacro alpha-chars-loop (var start-char end-char result body)
  (let ((n (gensym))
	(end-char-code (gensym)))
    `(do ((,n (char-code ,start-char) (1+ ,n))
	  (,end-char-code (char-code ,end-char)))
	 ((> ,n ,end-char-code) ,result)
       (let ((,var (code-char ,n)))
	 (when (alpha-char-p ,var)
	   ,@body)))))

(defmacro do-alpha-chars ((var kind &optional result) &rest forms)
  "(do-alpha-chars (var kind [result]) . body).  Kind is one of
   :lower, :upper, or :both, and var is bound to each character in
   order as specified under character relations in the manual.  When
   :both is specified, lowercase letters are processed first."
  (case kind
    (:both
     `(progn (alpha-chars-loop ,var #\a #\z nil ,forms)
	     (alpha-chars-loop ,var #\A #\Z ,result ,forms)))
    (:lower
     `(alpha-chars-loop ,var #\a #\z ,result ,forms))
    (:upper
     `(alpha-chars-loop ,var #\A #\Z ,result ,forms))
    (t (error "Kind argument not one of :lower, :upper, or :both -- ~S."
	      kind))))
