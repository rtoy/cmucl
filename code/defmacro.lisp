;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Written by Blaine Burks.
;;;
(in-package "LISP")


;;;; Some variable definitions.

;;; Variables for amassing the results of parsing a defmacro.  Declarations
;;; in DEFMACRO are the reason this isn't as easy as it sounds.
;;;
(defvar *arg-tests* ()
  "A list of tests that do argument counting at expansion time.")

(defvar *system-lets* ()
  "Let bindings that are done to make lambda-list parsing possible.")

(defvar *user-lets* ()
  "Let bindings that the user has explicitly supplied.")

(defvar *default-default* nil
  "Unsupplied optional and keyword arguments get this value defaultly.")

(defvar *key-finder* 'keyword-argument
  "The way we want to lookup keywords in macros at expansion time.  The default
   does the obvious thing, returning a keyword's argument.  DEFTRANSFORM,
   however does something more clever.")


;;;; Stuff to parse DEFMACRO, MACROLET, DEFINE-SETF-METHOD, and DEFTYPE.

;;; PARSE-DEFMACRO returns, as multiple-values, a body, possibly a declare
;;; form to put where this code is inserted, and the documentation for the
;;; parsed body.
;;;
(defun parse-defmacro (lambda-list arg-list-name code name error-kind
				   &key (annonymousp nil)
				   (doc-string-allowed t)
				   ((:environment env-arg-name))
				   ((:default-default *default-default*))
				   ((:key-finder *key-finder*)))
  "Returns as multiple-values a parsed body, any local-declarations that
   should be made where this body is inserted, and a doc-string if there is
   one."
  (multiple-value-bind (body declarations documentation)
		       (parse-body code nil doc-string-allowed)
    (let* ((*arg-tests* ())
	   (*user-lets* ())
	   (*system-lets* ()))
      (multiple-value-bind
	  (env-arg-used minimum maximum)
	  (parse-defmacro-lambda-list lambda-list arg-list-name name
				      error-kind (not annonymousp)
				      nil env-arg-name)
	(values
	 `(let* ,(nreverse *system-lets*)
	    ,@*arg-tests*
	    (let* ,(nreverse *user-lets*)
	      ,@declarations
	      ,@body))
	 (if (and env-arg-name (not env-arg-used))
	     `((declare (ignore ,env-arg-name)))
	     nil)
	 documentation
	 minimum
	 maximum)))))


(defun parse-defmacro-lambda-list (lambda-list arg-list-name name error-kind
			  &optional top-level env-illegal env-arg-name)
  (let ((path (if top-level `(cdr ,arg-list-name) arg-list-name))
	(now-processing :required)
	(maximum 0)
	(minimum 0)
	(keys ())
	rest-name restp allow-other-keys-p env-arg-used)
    ;; This really strange way to test for '&whole is neccessary because member
    ;; does not have to work on dotted lists, and dotted lists are legal
    ;; in lambda-lists.
    (when (and (do ((list lambda-list (cdr list)))
		   ((atom list) nil)
		 (when (eq (car list) '&whole) (return t)))
	       (not (eq (car lambda-list) '&whole)))
      (error "&Whole must appear first in ~S lambda-list." error-kind))
    (do ((rest-of-args lambda-list (cdr rest-of-args)))
	((atom rest-of-args)
	 (cond ((null rest-of-args) nil)
	       ;; Varlist is dotted, treat as &rest arg and exit.
	       (t (push-let-binding rest-of-args path nil)
		  (setf restp t))))
      (let ((var (car rest-of-args)))
	(cond ((eq var '&whole)
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) arg-list-name nil))
		     (t
		      (defmacro-error name "&whole"))))
	      ((eq var '&environment)
	       (cond (env-illegal
		      (error "&Environment not valid with ~S." error-kind))
		     ((not top-level)
		      (error "&Environment only valid at top level of ~
		      lambda-list.")))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) env-arg-name nil)
		      (setf env-arg-used t))
		     (t
		      (defmacro-error name "&environment"))))
	      ((or (eq var '&rest) (eq var '&body))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (setf restp t)
		      (push-let-binding (car rest-of-args) path nil))
		     ;;
		     ;; This branch implements an incompatible extension to
		     ;; Common Lisp.  In place of a symbol following &body,
		     ;; there may be a list of up to three elements which will
		     ;; be bound to the body, declarations, and doc-string of
		     ;; the body.
		     ((and (cdr rest-of-args)
			   (consp (cadr rest-of-args))
			   (symbolp (caadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (setf restp t)
		      (let ((body-name (caar rest-of-args))
			    (declarations-name (cadar rest-of-args))
			    (doc-string-name (caddar rest-of-args))
			    (parse-body-values (gensym)))
			(push-let-binding
			 parse-body-values
			 `(multiple-value-list
			   (parse-body ,path ,env-arg-name
				       ,(not (null doc-string-name))))
			 t)
			(when body-name
			  (push-let-binding body-name
					    `(car ,parse-body-values) nil))
			(when declarations-name
			  (push-let-binding declarations-name
					    `(cadr ,parse-body-values) nil))
			(when doc-string-name
			  (push-let-binding doc-string-name
					    `(caddr ,parse-body-values) nil))))
		     (t
		      (defmacro-error name (symbol-name var)))))
	      ((eq var '&optional)
	       (setf now-processing :optionals))
	      ((eq var '&key)
	       (setf now-processing :keywords)
	       (setf rest-name (gensym "KEYWORDS-"))
	       (setf restp t)
	       (push-let-binding rest-name path t))
	      ((eq var '&allow-other-keys)
	       (setf allow-other-keys-p t))
	      ((eq var '&aux)
	       (setf now-processing :auxs))
	      ((symbolp var)
	       (case now-processing
		 (:required
		  (incf minimum)
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil)
		  (setf path `(cdr ,path)))
		 (:optionals
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil `(not (null ,path)))
		  (setf path `(cdr ,path)))
		 (:keywords
		  (let ((key (make-keyword var)))
		    (push-let-binding var `(keyword-argument ,key ,rest-name)
				      nil)
		    (push key keys)))
		 (:auxs
		  (push-let-binding var nil nil))))
	      ((atom var)
	       (error "Non-symbol in lambda-list - ~S." var))
	      (t
	       (case now-processing
		 (:required
		  (let ((sub-list-name (gensym "SUBLIST-")))
		    (push-sub-list-binding sub-list-name `(car ,path) var
					   name error-kind)
		    (parse-defmacro-lambda-list var sub-list-name name
						error-kind))
		  (setf path `(cdr ,path))
		  (incf minimum)
		  (incf maximum))
		 (:optionals
		  (when (> (length var) 3)
		    (cerror "Ignore extra noise."
			    "More than variable, initform, and suppliedp ~
			    in &optional binding - ~S"
			    var))
		  (push-optional-binding (car var) (cadr var) (caddr var)
					 `(not (null ,path)) `(car ,path)
					 name error-kind)
		  (setf path `(cdr ,path))
		  (incf maximum))
		 (:keywords
		  (let* ((keyword-given (consp (car var)))
			 (variable (if keyword-given
				       (cadar var)
				       (car var)))
			 (keyword (if keyword-given
				      (caar var)
				      (make-keyword variable)))
			 (supplied-p (caddr var)))
		    (push-optional-binding variable (cadr var) supplied-p
					   `(keyword-supplied-p ,keyword
								,rest-name)
					   `(keyword-argument ,keyword
							      ,rest-name)
					   name error-kind)
		    (push keyword keys)))
		 (:auxs (push-let-binding (car var) (cadr var) nil)))))))
    (push `(destructuring-arg-count-test ,(if top-level
					      `(cdr ,arg-list-name)
					      arg-list-name)
					 ,minimum
					 ,(or (not (null restp)) maximum)
					 ',lambda-list
					 ',name
					 ',error-kind)
	  *arg-tests*)
    (if keys
	(push `(destructuring-key-test ,rest-name
				       ,(if allow-other-keys-p t `',keys))
	      *arg-tests*))
    (values env-arg-used minimum (if (null restp) maximum nil))))

(defun push-sub-list-binding (variable path object name error-kind)
  (push `(,variable (if (consp ,path)
			,path
			(error "Error in ~S ~S.  ~S should have been a list ~
			        to match ~S."
			       ',error-kind ',name ,path ',object)))
	*system-lets*))

(defun push-let-binding (variable path systemp &optional condition
				  (init-form *default-default*))
  (let ((let-form (if condition
		      `(,variable (if ,condition ,path ,init-form))
		      `(,variable ,path))))
    (if systemp
	(push let-form *system-lets*)
	(push let-form *user-lets*))))

(defun push-optional-binding (value-var init-form supplied-var condition path
					name error-kind)
  (unless supplied-var
    (setf supplied-var (gensym "SUPLIEDP-")))
  (push-let-binding supplied-var condition t)
  (cond ((consp value-var)
	 (let ((whole-thing (gensym "OPTIONAL-SUBLIST-")))
	   (push-let-binding whole-thing path t supplied-var init-form)
	   (parse-defmacro-lambda-list value-var whole-thing name error-kind)))
	((symbolp value-var)
	 (push-let-binding value-var path nil supplied-var init-form))
	(t
	 (error "Illegal optional variable name: ~S" value-var))))

(defun defmacro-error (location kind)
  (error "Illegal or ill-formed ~A argument in ~S." kind location))



;;;; Destructuring argument testing routines.

(proclaim '(inline destructuring-arg-count-error))
;;;
(defun destructuring-arg-count-error (string name error-kind &rest args)
  (error "While expanding ~A ~A:~%  ~?" error-kind name string args))

;;; DESTRUCTURING-ARG-COUNT-TEST tests an argument list against a maximum
;;; and minimum number of arguments.  NIL for maximum means there is no
;;; limit.  Keywords are checked.  T for keylist means allow-other-keys.
;;;
(defun destructuring-arg-count-test (arg-list minimum maximum lambda-list
					      name error-kind)
  (unless (listp arg-list)
    (destructuring-arg-count-error
     "~S should have been a list of arguments for lambda-list ~S."
     name error-kind arg-list lambda-list))
  (let ((length (length arg-list)))
    (cond ((< length minimum)
	   (destructuring-arg-count-error
	    "Too few arguments in ~S to satisfy lambda-list ~S.~%  Expected at ~
	    least ~D."
	    name error-kind arg-list lambda-list minimum))
	  ((and (not (eq maximum t))
		(> length maximum))
	   (destructuring-arg-count-error
	    "Too many arguments in ~S to satisfy lambda-list ~S.~%  Expected ~
	    no more than ~D."
	    name error-kind arg-list lambda-list maximum)))))

(defun destructuring-key-test (key-bindings valid-keys)
  (when (keyword-argument :allow-other-keys key-bindings)
    (setf valid-keys t))
  (do ((test-key key-bindings (cddr test-key)))
      ((endp test-key))
    (let ((key (car test-key)))
      (unless (and (keywordp key)
		   (or (eq valid-keys t)
		       (member key valid-keys)))
	(cerror "Ignore it."
		"~S is not an allowed keyword."
		(car test-key))))))



;;;; Helpful Keyword Routines.

(defun make-keyword (symbol)
  "Takes a non-keyword symbol, symbol, and returns the corresponding keyword."
  (intern (symbol-name symbol) *keyword-package*))

(defun keyword-supplied-p (keyword key-list)
  "Return T iff the given keyword is a key in key-list. Signal an error if it
  appears twice."
  (let ((foundp nil))
    (do ((keys key-list (cddr keys)))
	((endp keys) foundp)
      (let ((key (car keys)))
	(cond ((not (eq key keyword)))
	      (foundp
	       (cerror "Use first occurrence."
		       "Keyword ~S appears in keyword-list ~S more than once."
		       key key-list)
	       (return t))
	      ((consp (cdr keys))
	       (setf foundp t))
	      (t
	       (cerror
		"Stick a NIL on the end and go on."
		"Unpaired item in keyword portion of macro call.")
	       (setf (cdr keys) (list nil))))))))

(defun keyword-argument (keyword key-list)
  "If keyword is present in the keylist, return it's associated value."
  (do ((keys key-list (cddr keys)))
      ((endp keys))
    (when (eq (car keys) keyword)
      (return (cadr keys)))))


