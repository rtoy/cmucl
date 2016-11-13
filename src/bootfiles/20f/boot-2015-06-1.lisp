;; Fix #6.
;;
;; Use this to bootstrap the change using the snapshot-2015-06 binary.
(in-package "KERNEL")
(export '(invalid-case))
(in-package "CONDITIONS")

(ext:without-package-locks
(define-condition invalid-case (reference-condition error)
  ((name :initarg :name
	 :reader invalid-case-name)
   (format :initarg :format-control
	   :reader invalid-case-format)
   (args :initarg :format-arguments
	 :reader invalid-case-format-args))
  (:report (lambda (condition stream)
	     (format stream "~A: " (invalid-case-name condition))
	     (apply #'format stream (invalid-case-format condition) (invalid-case-format-args condition))
	     (print-references (reference-condition-references condition) stream))))
)

(in-package "LISP")

(ext:without-package-locks 
(defun case-body (name keyform cases multi-p test proceedp &optional allow-otherwise)
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (do* ((case-list cases (cdr case-list))
	  (case (first case-list) (first case-list)))
	 ((null case-list))
      (cond ((atom case)
	     (error (intl:gettext "~S -- Bad clause in ~S.") case name))
	    ((and (not allow-otherwise)
		  (memq (car case) '(t otherwise)))
	     (cond ((null (cdr case-list))
		    ;; The CLHS says OTHERWISE clause is an OTHERWISE clause
		    ;; only if it's the last case.  Otherwise, it's just a
		    ;; normal clause.
		    (push `(t nil ,@(rest case)) clauses))
		   ((and (eq name 'case))
		    (let ((key (first case)))
		      (error 'kernel:invalid-case
			     :name name
			     :format-control (intl:gettext
					      "~<~A is a key designator only in the final otherwise-clause. ~
                                              Use (~A) to use it as a normal-clause or move the clause to the ~
                                              correct position.~:@>")
			     :format-arguments (list (list key key))
			     :references (list '(:ansi-cl :section (5 3))
					       (list :ansi-cl :macro name)))))
		   ((eq (first case) t)
		    ;; The key T is normal clause, because it's not
		    ;; the last clause.
		    (push (first case) keys)
		    (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
	    ((and multi-p (listp (first case)))
	     (setf keys (append (first case) keys))
	     (push `((or ,@(mapcar #'(lambda (key)
				       `(,test ,keyform-value ',key))
				   (first case)))
		     nil ,@(rest case))
		   clauses))
	    (t
	     (when (and allow-otherwise
			(memq (car case) '(t otherwise)))
	       (warn 'kernel:simple-style-warning
		     :format-control (intl:gettext "Bad style to use ~S in ~S")
		     :format-arguments (list (car case) name)))
	     (push (first case) keys)
	     (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
    (case-body-aux name keyform keyform-value clauses keys proceedp
		   allow-otherwise
		   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled all the
;;; cases.  Note: it is not necessary that the resulting code signal
;;; case-failure conditions, but that's what KMP's prototype code did.  We call
;;; CASE-BODY-ERROR, because of how closures are compiled.  RESTART-CASE has
;;; forms with closures that the compiler causes to be generated at the top of
;;; any function using the case macros, regardless of whether they are needed.
;;;
(defun case-body-aux (name keyform keyform-value clauses keys
		      proceedp allow-otherwise expected-type)
  (if proceedp
      (let ((block (gensym))
	    (again (gensym)))
	`(let ((,keyform-value ,keyform))
	   (block ,block
	     (tagbody
	      ,again
	      (return-from
	       ,block
	       (cond ,@(nreverse clauses)
		     (t
		      (setf ,keyform-value
			    (setf ,keyform
				  (case-body-error
				   ',name ',keyform ,keyform-value
				   ',expected-type ',keys)))
		      (go ,again))))))))
      `(let ((,keyform-value ,keyform))
	 ,keyform-value ; prevent warnings when key not used eg (case key (t))
	 (cond
	  ,@(nreverse clauses)
	  ,@(if allow-otherwise
		`((t (error 'conditions::case-failure
			    :name ',name
			    :datum ,keyform-value
			    :expected-type ',expected-type
			    :possibilities ',keys))))))))

(defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value
  of Keyform.  If a singleton key is T or Otherwise then the clause is
  a default clause."
  (case-body 'case keyform cases t 'eql nil))

(defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql t t))

(defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql nil t))

(defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform
  and Type is true.  If a singleton key is T or Otherwise then the
  clause is a default clause."
  (case-body 'typecase keyform cases nil 'typep nil))

(defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t))

(defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep nil t))


)

