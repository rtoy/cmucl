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


;;; All the meaningful bit names in this implementation.
;;; 
(defconstant all-bit-names '(:control :meta :super :hyper))


;;;; Stuff for the Syntax table functions (syntax)
;;;
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
  `(char-upcase (the string-char ,ch)))


;;; Specal RT and Sun keys:

(eval-when (compile load eval)

(push (cons "DELETE" #\delete) lisp::char-name-alist)
(push (cons "ESCAPE" #\escape) lisp::char-name-alist)
(push (cons "F1" (code-char 1)) lisp::char-name-alist)
(push (cons "F2" (code-char 2)) lisp::char-name-alist)
(push (cons "F3" (code-char 3)) lisp::char-name-alist)
(push (cons "F4" (code-char 4)) lisp::char-name-alist)
(push (cons "F5" (code-char 5)) lisp::char-name-alist)
(push (cons "F6" (code-char 6)) lisp::char-name-alist)
(push (cons "F7" (code-char 7)) lisp::char-name-alist)
(push (cons "F8" (code-char 11)) lisp::char-name-alist)
(push (cons "F9" (code-char 12)) lisp::char-name-alist)
(push (cons "F10" (code-char 14)) lisp::char-name-alist)
(push (cons "F11" (code-char 17)) lisp::char-name-alist)
(push (cons "F12" (code-char 18)) lisp::char-name-alist)
(push (cons "LEFTARROW" (code-char 19)) lisp::char-name-alist)
(push (cons "RIGHTARROW" (code-char 20)) lisp::char-name-alist)
(push (cons "UPARROW" (code-char 22)) lisp::char-name-alist)
(push (cons "DOWNARROW" (code-char 23)) lisp::char-name-alist)
(push (cons "LEFTDOWN" (code-char 24)) lisp::char-name-alist)
(push (cons "MIDDLEDOWN" (code-char 25)) lisp::char-name-alist)
(push (cons "RIGHTDOWN" (code-char 128)) lisp::char-name-alist)
(push (cons "LEFTUP" (code-char 129)) lisp::char-name-alist)
(push (cons "MIDDLEUP" (code-char 130)) lisp::char-name-alist)
(push (cons "RIGHTUP" (code-char 131)) lisp::char-name-alist)
(push (cons "INSERT" (code-char 132)) lisp::char-name-alist)
(push (cons "PRINTSCREEN" (code-char 133)) lisp::char-name-alist)
(push (cons "PAUSE" (code-char 134)) lisp::char-name-alist)
(push (cons "HOME" (code-char 135)) lisp::char-name-alist)
(push (cons "END" (code-char 136)) lisp::char-name-alist)
(push (cons "PAGEUP" (code-char 137)) lisp::char-name-alist)
(push (cons "PAGEDOWN" (code-char 138)) lisp::char-name-alist)
(push (cons "NUMLOCK" (code-char 139)) lisp::char-name-alist)
(push (cons "F13" (code-char 140)) lisp::char-name-alist)
(push (cons "F14" (code-char 141)) lisp::char-name-alist)
(push (cons "F15" (code-char 142)) lisp::char-name-alist)
(push (cons "F16" (code-char 143)) lisp::char-name-alist)
(push (cons "F17" (code-char 144)) lisp::char-name-alist)
(push (cons "F18" (code-char 145)) lisp::char-name-alist)
(push (cons "F19" (code-char 146)) lisp::char-name-alist)
(push (cons "F20" (code-char 147)) lisp::char-name-alist)
(push (cons "F21" (code-char 148)) lisp::char-name-alist)
(push (cons "F22" (code-char 149)) lisp::char-name-alist)
(push (cons "F23" (code-char 150)) lisp::char-name-alist)
(push (cons "F24" (code-char 151)) lisp::char-name-alist)
(push (cons "F25" (code-char 152)) lisp::char-name-alist)
(push (cons "F26" (code-char 153)) lisp::char-name-alist)
(push (cons "F27" (code-char 154)) lisp::char-name-alist)
(push (cons "F28" (code-char 155)) lisp::char-name-alist)
(push (cons "F29" (code-char 156)) lisp::char-name-alist)
(push (cons "F30" (code-char 157)) lisp::char-name-alist)
(push (cons "F31" (code-char 158)) lisp::char-name-alist)
(push (cons "F32" (code-char 159)) lisp::char-name-alist)
(push (cons "F33" (code-char 160)) lisp::char-name-alist)
(push (cons "F34" (code-char 161)) lisp::char-name-alist)
(push (cons "F35" (code-char 162)) lisp::char-name-alist)
;; ALTERNATE key on Sun keyboard.
(push (cons "BREAK" (code-char 163)) lisp::char-name-alist)

) ;eval-when (compile load eval)

;;; Stick them on the end so that they don't print this way.
;;; Use two separate EVAL-WHEN forms, so the #\f<13-35> characters can be
;;; read at this point.
;;;
(eval-when (compile load eval)
(setq lisp::char-name-alist
      (append lisp::char-name-alist
	      '(("ENTER" . #\return) ("ACTION" . #\linefeed)

		("L1" . #\F11) ("L2" . #\F12) ("L3" . #\F13) ("L4" . #\F14)
		("L5" . #\F15) ("L6" . #\F16) ("L7" . #\F17) ("L8" . #\F18)
		("L9" . #\F19) ("L10" . #\F20)

		("R1" . #\F21) ("R2" . #\F22) ("R3" . #\F23) ("R4" . #\F24)
		("R5" . #\F25) ("R6" . #\F26) ("R7" . #\F27) ("R8" . #\F28)
		("R9" . #\F29) ("R10" . #\F30) ("R11" . #\F31) ("R12" . #\F32)
		("R13" . #\F33) ("R14" . #\F34) ("R15" . #\F35))))
) ;eval-when


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
