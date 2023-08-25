;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: share.lisp,v 1.10 2004/09/02 06:59:43 yuji Exp $
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

(deftype proper-list () '(satisfies proper-list-p))
(deftype proper-sequence () '(satisfies proper-sequence-p))
(deftype string-designator () '(or character symbol string))
(deftype package-designator () '(or string-designator package))
(deftype function-designator () '(or symbol function))
(deftype extended-function-designator ()
  '(or function (satisfies function-name-p)))
(deftype character-designator-simbol ()
  '(satisfies character-designator-symbol-p))

(defun character-designator-symbol-p (object)
  (and (symbolp object) (= (length (symbol-name object)) 1)))

(defun function-name-p (object)
  (or (symbolp object)
      (and (consp object)
	   (eq (car object) 'setf)
	   (symbolp (cadr object))
	   (null (cddr object)))))

(defun proper-list-p (object)
  (when (listp object)
    (do ((fast object (cddr fast))
	 (slow object (cdr slow)))
	(nil)
      (when (atom fast)
	(return (null fast)))
      (when (atom (cdr fast))
	(return (null (cdr fast))))
      (when (and (eq fast slow) (not (eq fast object)))
	(return nil)))))

(defun proper-sequence-p (object)
  (or (vectorp object) (proper-list-p object)))

(defun error-circular-list (list)
  (error 'type-error :datum list :expected-type 'proper-list))

(defun error-index-too-large (sequence index)
  (error 'type-error
	 :datum index
	 :expected-type `(integer 0 ,(1- (length sequence)))))

(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
     ,element))


(defmacro do-sublist ((var list start end from-end result) &body body)
  (let ((rev (gensym))
	(i   (gensym))
	(x   (gensym)))
    `(symbol-macrolet ((,var (car ,x)))
       (if ,from-end
	   (let ((,rev nil))
	     (do ((x (nthcdr ,start ,list) (cdr x))
		  (i ,start (1+ i)))
		 ((>= i ,end))
	       (setq ,rev (cons x ,rev)))
	     (do* ((,rev ,rev (cdr ,rev))
		   (,x (car ,rev) (car ,rev)))
		 ((null ,rev) ,result)
	       ,@body))
	 (do ((,x (nthcdr ,start ,list) (cdr ,x))
	      (,i ,start (1+ ,i)))
	     ((>= ,i ,end) ,result)
	   ,@body)))))

(defmacro do-subvector ((var vector start end from-end result) &body body)
  (let ((i     (gensym))
	(step  (gensym))
	(limit (gensym)))
    `(symbol-macrolet ((,var (aref ,vector ,i)))
       (let ((,step (if ,from-end -1 1))
	     (,limit (if ,from-end (1- ,start) ,end)))
	 (do ((,i (if ,from-end (1- ,end) ,start) (+ ,i ,step)))
	     ((= ,i ,limit) ,result)
	  ,@body)))))

(defmacro do-subsequence ((var sequence-form start-form &optional end-form
			       from-end-form result-form) &body body)
  (let ((sequence (gensym))
	(start    (gensym))
	(end      (gensym)))
    `(let* ((,sequence ,sequence-form)
	    (,start ,start-form)
	    (,end (or ,end-form (length ,sequence))))
       (check-subsequence ,sequence ,start ,end)
       (etypecase ,sequence
	 (list
	  (do-sublist (,var ,sequence ,start ,end ,from-end-form ,result-form)
             ,@body))
	 (vector
	  (do-subvector (,var ,sequence ,start ,end ,from-end-form ,result-form)
	     ,@body))))))

(defun declarationp (expr)
  (and (consp expr) (eq (car expr) 'declare)))

(defun declarations-and-forms (body)
  (block nil
    (let ((decls nil)
	  (forms body))
      (tagbody
       top
	 (when (not (declarationp (car forms)))
	   (return (values (reverse decls) forms)))
	 (push (car forms) decls) (psetq forms (cdr forms))
	 (go top)))))


(defun required-argument ()
  (error "required argument not specified."))


(defun %symbol (designator)
  (if (symbolp designator)
      designator
      (error 'type-error :datum designator :expected-type 'symbol)))
(defun %keyword (designator)
  (intern (string designator) "KEYWORD"))
(defun %list (designator)
  (if (listp designator)
      designator
      (list designator)))
(defun symbol-list (designator) (mapcar #'%symbol (%list designator)))
(defun string-list (designator) (mapcar #'string (%list designator)))



(defun store-value-report (stream place)
  (format stream "Supply a new value for ~S." place))
(defun store-value-interactive ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))



(defun mapappend (function &rest lists)
  (apply #'append (apply #'mapcar function lists)))

(define-condition simple-program-error (simple-condition program-error) ())

(define-modify-macro appendf (&rest args)
   append "Append onto list")


(defvar *message-prefix* "")
;; for debug
(defvar *error-function* #'error)
(defun error (datum &rest arguments)
  (if (stringp datum)
      (let ((format-control (concatenate 'string *message-prefix* datum)))
        (apply *error-function* format-control arguments))
      (apply *error-function* datum arguments)))
