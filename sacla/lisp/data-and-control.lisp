;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: data-and-control.lisp,v 1.17 2004/09/02 06:59:43 yuji Exp $
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

(defun expand-case (keyform clauses &key (test #'eql))
  (let ((key (gensym))
	(last (car (last clauses))))
    `(let ((,key ,keyform))
      (declare (ignorable ,key))
      (cond
	,@(mapcar
	   #'(lambda (clause)
	       (let ((key-list (first clause))
		     (forms (rest clause)))
		 (cond
		   ((and (eq clause last) (member key-list '(otherwise t)))
		    `(t ,@forms))
		   ((not (listp key-list))
		    `((funcall ',test ,key ',key-list) ,@forms))
		   ((null key-list)
		    `(nil ,@forms))
		   ((rest key-list)
		    `((member ,key ',key-list :test ',test) ,@forms))
		   (t
		    `((funcall ',test ,key ',(car key-list)) ,@forms)))))
	   clauses)))))

(defmacro psetq (&rest pairs)
  ;; not use reverse for build order consistency
  (do* ((pairs pairs (cddr pairs))
	(tmp (gensym) (gensym))
	(inits (list nil))
	(inits-splice inits)
	(setqs (list nil))
	(setqs-splice setqs))
      ((null pairs) (when (cdr inits)
		      `(let ,(cdr inits)
			 (setq ,@(cdr setqs))
			 nil)))
    (setq inits-splice
	  (cdr (rplacd inits-splice (list (list tmp (cadr pairs)))))
	  setqs-splice
	  (cddr (rplacd setqs-splice (list (car pairs) tmp))))))


(defmacro return (&optional result)
  `(return-from nil ,result))

(defun not (x)
  (if x nil t))

(defun equal (x y)
  (cond
    ((eql x y) t)
    ((consp x) (and (consp y) (equal (car x) (car y)) (equal (cdr x) (cdr y))))
    ((stringp x) (and (stringp y) (string= x y)))
    ((bit-vector-p x) (and (bit-vector-p y) (= (length x) (length y))
			   (dotimes (i (length x) t)
			     (unless (eql (aref x i) (aref y i))
			       (return nil)))))
    ((pathnamep x) (and (pathnamep y)
			(equal (pathname-host x) (pathname-host y))
			(equal (pathname-device x) (pathname-device y))
			(equal (pathname-directory x) (pathname-directory y))
			(equal (pathname-name x) (pathname-name y))
			(equal (pathname-type x) (pathname-type y))
			(equal (pathname-version x) (pathname-version y))))
    (t nil)))

(defun identity (object)
  object)

(defun complement (function)
  #'(lambda (&rest arguments) (not (apply function arguments))))

(defun constantly (object)
  #'(lambda (&rest arguments)
      (declare (ignore arguments))
      object))

(defmacro and (&rest forms)
  (cond
   ((null forms) t)
   ((null (cdr forms)) (car forms))
   (t `(when ,(car forms)
	   (and ,@(cdr forms))))))

(defmacro or (&rest forms)
  (cond
   ((null forms) nil)
   ((null (cdr forms)) (car forms))
   (t (let ((tmp (gensym)))
	  `(let ((,tmp ,(car forms)))
	     (if ,tmp
		 ,tmp
	       (or ,@(cdr forms))))))))

(defmacro cond (&rest clauses)
  (when clauses
    (let ((test1 (caar clauses))
	  (forms1 (cdar clauses)))
      (if forms1
	  `(if ,test1
	    (progn ,@forms1)
	    (cond ,@(cdr clauses)))
	  (let ((tmp (gensym)))
	    `(let ((,tmp ,test1))
	      (if ,tmp
		  ,tmp
		  (cond ,@(cdr clauses)))))))))

(defmacro when (test-form &rest forms)
  `(if ,test-form
	 (progn ,@forms)
     nil))

(defmacro unless (test-form &rest forms)
  `(if ,test-form
	 nil
     (progn ,@forms)))


(defmacro case (keyform &rest clauses)
  (expand-case keyform clauses))

(defmacro ccase (keyplace &rest clauses)
  (let* ((clauses (mapcar #'(lambda (clause)
			      (let ((key (first clause))
				    (forms (rest clause)))
				`(,(%list key) ,@forms)))
			  clauses))
	 (expected-type `(member ,@(apply #'append (mapcar #'car clauses))))
	 (block-name (gensym))
	 (tag (gensym)))
    `(block ,block-name
      (tagbody
	 ,tag
	 (return-from ,block-name
	   (case ,keyplace
	     ,@clauses
	     (t (restart-case (error 'type-error :datum ,keyplace
				     :expected-type ',expected-type)
		  (store-value (value)
		    :report (lambda (stream)
			      (store-value-report stream ',keyplace))
		    :interactive store-value-interactive
		    (setf ,keyplace value)
		    (go ,tag))))))))))
    
    
(defmacro ecase (keyform &rest clauses)
  (let* ((clauses (mapcar #'(lambda (clause)
			      (let ((key (first clause))
				    (forms (rest clause)))
				`(,(%list key) ,@forms)))
			  clauses))
	 (expected-type `(member ,@(apply #'append (mapcar #'car clauses)))))
    `(case ,keyform
      ,@clauses
      (t (error 'type-error :datum ,keyform :expected-type ',expected-type)))))

(defmacro typecase (keyform &rest clauses)
  (let* ((last (car (last clauses)))
	 (clauses (mapcar #'(lambda (clause)
			      (let ((type (first clause))
				    (forms (rest clause)))
				(if (and (eq clause last)
					 (member type '(otherwise t)))
				    clause
				    `((,type) ,@forms))))
			  clauses)))
    (expand-case keyform clauses :test #'typep)))

(defmacro ctypecase (keyplace &rest clauses)
  (let ((expected-type `(or ,@(mapcar #'car clauses)))
	(block-name (gensym))
	(tag (gensym)))
    `(block ,block-name
      (tagbody
	 ,tag
	 (return-from ,block-name
	   (typecase ,keyplace
	     ,@clauses
	     (t (restart-case (error 'type-error
				     :datum ,keyplace
				     :expected-type ',expected-type)
		  (store-value (value)
		    :report (lambda (stream)
			      (store-value-report stream ',keyplace))
		    :interactive store-value-interactive
		    (setf ,keyplace value)
		    (go ,tag))))))))))
		  


(defmacro etypecase (keyform &rest clauses)
  `(typecase ,keyform
    ,@clauses
    (t (error 'type-error
	:datum ',keyform :expected-type '(or ,@(mapcar #'car clauses))))))


(defmacro multiple-value-bind (vars values-form &body body)
  (cond
   ((null vars)
    `(progn ,@body))
   ((null (cdr vars))
    `(let ((,(car vars) ,values-form))
       ,@body))
   (t
    (let ((rest (gensym)))
      `(multiple-value-call #'(lambda (&optional ,@vars &rest ,rest)
				(declare (ignore ,rest))
				,@body)
			    ,values-form)))))



(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defmacro multiple-value-setq (vars form)
  `(values (setf (values ,@vars) ,form)))
;;  (let ((temps (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) vars)))
;;    `(multiple-value-bind ,temps ,form
;;	 (setq ,@(mapcan #'(lambda (var temp) (list var temp)) vars temps))
;;	 ,(car temps))))

(defun values-list (list)
  (check-type list proper-list)
  (apply #'values list))

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))

(define-setf-expander values (&rest places &environment env)
  (let (all-temps all-vars 1st-newvals rest-newvals all-setters all-getters)
    (dolist (place places)
      (multiple-value-bind (temps vars newvals setter getter)
	  (get-setf-expansion place env)
	(setq all-temps    (cons temps all-temps)
	      all-vars     (cons vars all-vars)
	      1st-newvals  (cons (car newvals) 1st-newvals)
	      rest-newvals (cons (cdr newvals) rest-newvals)
	      all-setters  (cons setter all-setters)
	      all-getters  (cons getter all-getters))))
    (values (apply #'append (reverse (append rest-newvals all-temps)))
	    (append (apply #'append (reverse all-vars))
		    (make-list (reduce #'+ rest-newvals :key #'length)))
	    (reverse 1st-newvals)
	    `(values ,@(reverse all-setters))
	    `(values ,@(reverse all-getters)))))

;;(define-setf-expander apply (function &rest args)
;;  (assert (and (listp function)
;;		 (= (list-length function) 2)
;;		 (eq (first function) 'function)
;;		 (symbolp (second function))))
;;  (let ((function (cadr function))
;;	  (newvals (list (gensym)))
;;	  (temps (mapcar #'(lambda (arg) (gensym)) args)))
;;    (values temps
;;	      args
;;	      newvals
;;	      `(apply #'(setf ,function) ,(car newvals) ,@vars)
;;	      `(apply #',function ,@temps))))

(defmacro prog (vars &body body)
  (flet ((declare-p (expr)
	   (and (consp expr) (eq (car expr) 'declare))))
    (do ((decls nil)
	 (forms body (cdr forms)))
	((not (declare-p (car forms))) `(block nil
					  (let ,vars
					    ,@(reverse decls)
					    (tagbody ,@forms))))
      (push (car forms) decls))))

(defmacro prog* (vars &body body)
  (multiple-value-bind (decls forms) (declarations-and-forms body)
    `(block nil
       (let* ,vars
	 ,@(reverse decls)
	 (tagbody ,@forms)))))

(defmacro prog1 (first-form &rest more-forms)
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@more-forms
       ,result)))

(defmacro prog2 (first-form second-form &rest more-forms)
  `(prog1 (progn ,first-form ,second-form) ,@more-forms))


(defmacro setf (&rest pairs &environment env)
  (let ((nargs (length pairs)))
    (assert (evenp nargs))
    (cond
     ((zerop nargs) nil)
     ((= nargs 2)
      (let ((place (car pairs))
	    (value-form (cadr pairs)))
	(cond
	 ((symbolp place)
	  `(setq ,place ,value-form))
	 ((consp place)
	  (if (eq (car place) 'the)
	      `(setf ,(caddr place) (the ,(cadr place) ,value-form))
	    (multiple-value-bind (temps vars newvals setter getter)
		(get-setf-expansion place env)
	      (declare (ignore getter))
	      `(let (,@(mapcar #'list temps vars))
		 (multiple-value-bind ,newvals ,value-form
		   ,setter))))))))
     (t
      (do* ((pairs pairs (cddr pairs))
	    (setfs (list 'progn))
	    (splice setfs))
	  ((endp pairs) setfs)
	(setq splice (cdr (rplacd splice
				  `((setf ,(car pairs) ,(cadr pairs)))))))))))

(defmacro psetf (&rest pairs &environment env)
  (let ((nargs (length pairs)))
    (assert (evenp nargs))
    (if (< nargs 4)
	`(progn (setf ,@pairs) nil)
      (let ((setters nil))
	(labels ((expand (pairs)
                   (if pairs
		       (multiple-value-bind (temps vars newvals setter getter)
			   (get-setf-expansion (car pairs) env)
			 (declare (ignore getter))
			 (setq setters (cons setter setters))
			 `(let (,@(mapcar #'list temps vars))
			    (multiple-value-bind ,newvals ,(cadr pairs)
			      ,(expand (cddr pairs)))))
		     `(progn ,@setters nil))))
	  (expand pairs))))))

(defmacro shiftf (&rest places-and-newvalue &environment env)
  (let ((nargs (length places-and-newvalue)))
    (assert (>= nargs 2))
    (let ((place (car places-and-newvalue)))
      (multiple-value-bind (temps vars newvals setter getter)
	  (get-setf-expansion place env)
	`(let (,@(mapcar #'list temps vars))
	   (multiple-value-prog1 ,getter
	     (multiple-value-bind ,newvals
		 ,(if (= nargs 2)
		      (cadr places-and-newvalue)
		    `(shiftf ,@(cdr places-and-newvalue)))
	       ,setter)))))))

(defmacro rotatef (&rest places &environment env)
  (if (< (length places) 2)
      nil
    (multiple-value-bind (temps vars newvals setter getter)
	(get-setf-expansion (car places) env)
      `(let (,@(mapcar #'list temps vars))
	 (multiple-value-bind ,newvals (shiftf ,@(cdr places) ,getter)
	   ,setter)
	 nil))))
