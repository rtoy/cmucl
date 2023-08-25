;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: symbol.lisp,v 1.8 2004/02/20 07:23:42 yuji Exp $
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

(defun keywordp (object)
  "Return true if OBJECT is a keyword; otherwise, return false."
  (and (symbolp object)
       (symbol-package object)
       (eq (symbol-package object)
	   (find-package "KEYWORD"))))

(defun copy-symbol (symbol &optional copy-properties)
  "Returns a fresh, uninterned symbol whose name is string= to SYMBOL's."
  (let ((copy (make-symbol (symbol-name symbol))))
    (when copy-properties
      (when (boundp symbol)
	(setf (symbol-value copy) (symbol-value symbol)))
      (when (fboundp symbol)
	(setf (symbol-function copy) (symbol-function symbol)))
      (setf (symbol-plist copy) (copy-list (symbol-plist symbol))))
    copy))


(defun counter-to-str (x)
  (flet ((digit-to-str (digit)
           (string (char "0123456789" digit))))
    (do* ((x x (floor x 10))
	  (digits (list (digit-to-str (mod x 10)))
		  (cons (digit-to-str (mod x 10)) digits)))
	((zerop (floor x 10)) (apply #'concatenate
				       'string
				       digits)))))

;;(defvar *gensym-counter* 0
;;  "Counter for generating unique GENSYM symbols.")
;;
;;(defun gensym (&optional (x "G"))
;;  "Return a fresh, uninterned symbol whose name is determined using X."
;;  (multiple-value-bind (prefix index)
;;	(etypecase x
;;	  ((integer 0) (values "G" x))
;;	  ((string) (multiple-value-prog1
;;		     (values x *gensym-counter*)
;;		     (setq *gensym-counter* (1+ *gensym-counter*)))))
;;    (make-symbol (concatenate 'string prefix (counter-to-str index)))))

(defvar *gentemp-counter* 0)

(defun gentemp (&optional (prefix "T") (package *package*))
  "Return a fresh symbol, newly interned in PACKAGE."
  (flet ((make-name (prefix)
	   (prog1
	       (concatenate 'string
			    prefix
			    (counter-to-str *gentemp-counter*))
	     (setq *gentemp-counter* (1+ *gentemp-counter*)))))
    (do ((name (make-name prefix) (make-name prefix)))
	(nil)
      (multiple-value-bind (symbol status)
	  (find-symbol name package)
	(declare (ignore symbol))
	(when (eq status nil)
	  (return (intern name package)))))))

(defsetf symbol-value
  set
  "Change the contents of the value cell of symbol to the given value.")

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defsetf get (symbol indicator &optional default) (value)
  `(setf (getf (symbol-plist ,symbol) ,indicator ,default) ,value))


(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

