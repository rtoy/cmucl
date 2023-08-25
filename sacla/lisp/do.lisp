;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: do.lisp,v 1.12 2004/05/26 07:57:52 yuji Exp $
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


(defun do-expand (var-init-step-list test-and-result-forms body parallel-p)
  (let ((top (gensym))
	(test-form (first test-and-result-forms))
	(result-forms (rest test-and-result-forms))
	(let-operator (if parallel-p 'let 'let*))
	(setq-operator (if parallel-p 'psetq 'setq)))
    (multiple-value-bind (declarations forms) (declarations-and-forms body)
      `(block nil
	(,let-operator (,@(mapcar #'(lambda (x) (if (atom x) x (subseq x 0 2)))
				  var-init-step-list))
	 ,@declarations
	 (tagbody
	    ,top
	    (when ,test-form (return (progn ,@result-forms)))
	    ,@forms
	    (,setq-operator ,@(mapcan #'(lambda (x)
					  (when (and (consp x) (= (length x) 3))
					    `(,(first x) ,(third x))))
				      var-init-step-list))
	    (go ,top)))))))

(defmacro do (var-init-step-list test-and-result-forms &body body)
  (do-expand var-init-step-list test-and-result-forms body 'parallel))

(defmacro do* (var-init-step-list test-and-result-forms &body body)
  (do-expand var-init-step-list test-and-result-forms body nil))

(defmacro dotimes ((var count-form &optional result-form) &body body)
  (let ((max (gensym)))
    `(do* ((,max ,count-form)
	   (,var 0 (1+ ,var)))
      ((>= ,var ,max) ,result-form)
      ,@body)))

(defmacro dolist ((var list-form &optional result-form) &body body)
  (let ((top (gensym))
	(tag (gensym))
	(list (gensym)))
    (multiple-value-bind (declarations forms) (declarations-and-forms body)
      `(block nil
	(let ((,list ,list-form)
	      (,var nil))
	  (declare (ignorable ,var))
	  (unless (atom ,list)
	    (let ((,var (car ,list)))
	      ,@declarations
	      (block ,tag
		(tagbody
		   ,top
		   ,@forms
		   (setq ,list (cdr ,list))
		   (when (atom ,list) (return-from ,tag))
		   (setq ,var (car ,list))
		   (go ,top)))))
	  ,result-form)))))

