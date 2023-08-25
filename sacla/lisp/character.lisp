;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: character.lisp,v 1.6 2004/02/20 07:23:42 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defun char/= (character &rest more-characters)
  "Return true if all characters are different; otherwise, return false."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (member c list :test #'char=)
      (return nil))
    (setq c (car list))))

(defun char> (character &rest more-characters)
  "Return true if the characters are monotonically decreasing."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (or (char= c (car list)) (char< c (car list)))
      (return nil))
    (setq c (car list))))

(defun char<= (character &rest more-characters)
  "Return true if the characters are monotonically nondecreasing;"
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (char> c (car list))
      (return nil))
    (setq c (car list))))

(defun char>= (character &rest more-characters)
  "Return true if the characters are monotonically nonincreasing."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (char< c (car list))
      (return nil))
    (setq c (car list))))

(defun char-equal (character &rest more-characters)
  "Return true if all characters are the same when ignoring the case."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (unless (char= (char-upcase c) (char-upcase (car list)))
      (return nil))
    (setq c (car list))))

(defun char-not-equal (character &rest more-characters)
  "Return true if all characters are different when ignoring the case."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (member c list :test #'char-equal)
      (return nil))
    (setq c (car list))))

(defun char-lessp (character &rest more-characters)
  "Return true if the chars are monotonically increasing when ignoring the case."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (unless (char< (char-upcase c) (char-upcase (car list)))
      (return nil))
    (setq c (car list))))

(defun char-greaterp (character &rest more-characters)
  "Return true if the chars are monotonically decreasing when ignoring the case."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (unless (char> (char-upcase c) (char-upcase (car list)))
      (return nil))
    (setq c (car list))))

(defun char-not-greaterp (character &rest more-characters)
  "Return true if the chars are monotonically nondecreasing when ignoring the case."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (char-greaterp c (car list))
      (return nil))
    (setq c (car list))))


(defun char-not-lessp (character &rest more-characters)
  "Return true if the characters are monotonically nonincreasing."
  (do ((c character)
       (list more-characters (cdr list)))
      ((atom list) t)
    (when (char-lessp c (car list))
      (return nil))
    (setq c (car list))))


(defun character (designator)
  "Return the character denoted by the character designator CHARACTER."
  (etypecase designator
    (character designator)
    ((string 1) (char designator 0))
    (character-designator-simbol (char (symbol-name designator) 0))))


(defun digit-char (weight &optional (radix 10))
  "Return a character which has WEIGHT when considered as a digit in RADIX."
  (check-type radix (integer 2 36))
  (if (>= weight radix)
      nil
    (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight)))

(defun digit-char-p (char &optional (radix 10))
  "Test whether CHAR is a digit in RADIX. If it is, return its weight."
  (check-type radix (integer 2 36))
  (position (char-upcase char)
	    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	    :end radix))

(defconstant standard-chars
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
"
  "Standard characters")

(defun standard-char-p (character)
  "Return true if CHARACTER is of type standard-char; otherwise, return false."
  (check-type character character)
  (find character standard-chars :test #'char=))

