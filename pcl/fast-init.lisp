;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;;
;;; This file defines the optimized make-instance functions.
;;; 

(in-package 'pcl)

;make-instance-functions take up more space when compiled.
(defvar *inhibit-compile-make-instance-functions-p* nil)

(defvar *compile-make-instance-functions-p* nil) ; this gets bound to t below

(defvar *make-instance-function-table* (make-hash-table :test 'equal))

(defvar gmif-class nil)

(defun update-make-instance-function-table (&optional (gmif-class *the-class-t*))
  (when (symbolp gmif-class) (setq gmif-class (find-class gmif-class)))
  (let ((class gmif-class))
    (when (eq class *the-class-t*) (setq class *the-class-slot-object*))
    (when (memq *the-class-slot-object* (class-precedence-list class))
      (map-all-classes #'reset-class-initialize-info class)))
  (maphash #'get-make-instance-function *make-instance-function-table*))

(defun get-make-instance-function-key (class initargs)
  (let ((keys nil)(allow-other-keys-p nil) key value)
    (when initargs
      (let ((initargs-tail initargs))
	(setq key (pop initargs-tail) value (pop initargs-tail))
	(when (eq key ':allow-other-keys)
	  (setq allow-other-keys-p value))
	(setq keys (cons key nil))
	(when initargs-tail
	  (let ((keys-tail keys))
	    (loop (setq key (pop initargs-tail) value (pop initargs-tail))
		  (when (eq key ':allow-other-keys)
		    (setq allow-other-keys-p value))
		  (if keys-tail
		      (setf (cdr keys-tail) (cons key nil)
			    keys-tail (cdr keys-tail))
		      (setf keys-tail (setf keys (cons key nil))))
		  (when (null initargs-tail) (return nil)))))))
    (list class keys allow-other-keys-p)))

(defmacro %get-make-instance-function (class initargs)
  `(let ((key (get-make-instance-function-key ,class ,initargs)))
     (or (car (gethash key *make-instance-function-table*))
	 (get-make-instance-function key))))

(defun constant-symbol-p (form)
  (and (constantp form) (symbolp (eval form))))

(defvar *make-instance-function-keys* nil)
(defvar *skip-boundp-check* nil)

(defun expand-make-instance-form (form &optional
				       (skip-boundp-check *skip-boundp-check*))
  (let ((class (cadr form)) (initargs (cddr form))
	(keys nil)(allow-other-keys-p nil) key value)
    (when (and (constant-symbol-p class)
	       (let ((initargs-tail initargs))
		 (loop (when (null initargs-tail) (return t))
		       (unless (constant-symbol-p (car initargs-tail))
			 (return nil))		       
		       (setq key (eval (pop initargs-tail)))
		       (setq value (pop initargs-tail))
		       (when (eq ':allow-other-keys key)
			 (setq allow-other-keys-p value))
		       (push key keys))))
      (let* ((class (eval class))
	     (keys (nreverse keys))
	     (key (list class keys allow-other-keys-p))
	     (sym (make-instance-function-symbol key)))
	(push key *make-instance-function-keys*)
	(if (and sym skip-boundp-check)
	    `(,sym ',class (list ,@initargs))
	    `(funcall ,(if sym
			   `(if (#-akcl fboundp #+akcl %fboundp ',sym)
			        (#-akcl symbol-function #+akcl %symbol-function ',sym)
			        (get-make-instance-function ',key))
			   `(get-make-instance-function ',key))
	             ',class (list ,@initargs)))))))

(defmacro expanding-make-instance-top-level (&rest forms &environment env)
  (let* ((*make-instance-function-keys* nil)
	 (*skip-boundp-check* t)
	 (form (macroexpand `(expanding-make-instance ,@forms) env)))
    `(progn
       ,@(when *make-instance-function-keys*
	   `((get-make-instance-functions ',*make-instance-function-keys*)))
       ,form)))
	  
(defmacro expanding-make-instance (&rest forms &environment env)
  `(progn
     ,@(mapcar #'(lambda (form)
		   (walk-form form env 
			      #'(lambda (subform context env)
				  (declare (ignore env))
				  (or (and (eq context ':eval)
					   (consp subform)
					   (eq (car subform) 'make-instance)
					   (expand-make-instance-form subform))
				      subform))))
	       forms)))

(defmacro defconstructor
	  (name class lambda-list &rest initialization-arguments)
  `(expanding-make-instance-top-level
    (defun ,name ,lambda-list
      (make-instance ',class ,@initialization-arguments))))

(defun get-make-instance-functions (key-list &optional compile-p)
  (let ((*compile-make-instance-functions-p* compile-p))
    (dolist (key key-list)
      (if compile-p
	  (get-make-instance-function key)
	  (set-make-instance-function (make-instance-function-symbol key)
				      key
				      (make-lazy-get-make-instance-function key)
				      nil)))))

(defun make-instance-function-symbol (key)
  (let ((class (car key)))
    (when (or (symbolp class) (classp class))
      (let* ((class-name (if (symbolp class) class (class-name class)))
	     (keys (cadr key))
	     (allow-other-keys-p (caddr key)))
	(let ((*package* *the-pcl-package*)
	      (*print-length* nil) (*print-level* nil)
	      (*print-circle* nil) (*print-case* :upcase)
	      (*print-pretty* nil))
	  (intern (format nil "MAKE-INSTANCE ~S ~S ~S"
			  class-name keys allow-other-keys-p)))))))

(defun make-lazy-get-make-instance-function (key)
  #'(lambda (class initargs)
      (if (eq *boot-state* 'complete)
	  (funcall (get-make-instance-function key) class initargs)
	  (apply #'make-instance class initargs))))

(defun get-make-instance-function (key &optional (value (list nil nil)))
  (let* ((class (car key))
	 (keys (cadr key))
	 (gmif-class-1 gmif-class) (gmif-class nil)
	 name)
    (flet ((return-function (function)
	     (return-from get-make-instance-function
	       (set-make-instance-function name key function nil))))
      (unless (eq *boot-state* 'complete)
	(return-function (make-lazy-get-make-instance-function key)))
      (when (symbolp class)
	(let ((real-class (find-class class nil)))
	  (unless real-class
	    (if gmif-class-1
		(return-function (make-lazy-get-make-instance-function key))
		(error "class ~S not found" class)))
	  (setq class real-class)))
      (when (classp class)
	(unless (class-finalized-p class) (finalize-inheritance class))
	(when (and gmif-class-1
		   (not (member gmif-class-1 (class-precedence-list class))))
	  (return-from get-make-instance-function nil))
	(setq name (make-instance-function-symbol key))
      (when (and gmif-class-1 (not *compile-make-instance-functions-p*))
	(return-function 
	 #'(lambda (class initargs)
	     (funcall (get-make-instance-function key) class initargs)))))
    (let* ((initargs (mapcan #'(lambda (key) (list key nil)) keys))
	   (class-and-initargs (list* class initargs))
	   (make-instance (gdefinition 'make-instance))
	   (make-instance-methods
	    (compute-applicable-methods make-instance class-and-initargs))
	   (std-mi-meth (find-standard-ii-method make-instance-methods 'class))
	   (class+initargs (list class initargs))
	   (default-initargs (gdefinition 'default-initargs))
	   (default-initargs-methods
	    (compute-applicable-methods default-initargs class+initargs))
	   (proto (and (classp class) (class-prototype class)))
	   (initialize-instance-methods
	    (when proto
	      (compute-applicable-methods (gdefinition 'initialize-instance)
					  (list* proto initargs))))
	   (shared-initialize-methods
	    (when proto
	      (compute-applicable-methods (gdefinition 'shared-initialize)
					  (list* proto t initargs)))))
      (when (null make-instance-methods)
	(return-function 
	 #'(lambda (class initargs)
	     (apply #'no-applicable-method make-instance class initargs))))
      (unless (and (null (cdr make-instance-methods))
		   (eq (car make-instance-methods) std-mi-meth)
		   (null (cdr default-initargs-methods))
		   (eq (car (method-specializers (car default-initargs-methods)))
		       *the-class-slot-class*)
		   (flet ((check-meth (meth)
			    (let ((quals (method-qualifiers meth)))
			      (if (null quals)
				  (eq (car (method-specializers meth))
				      *the-class-slot-object*)
				  (and (null (cdr quals))
				       (or (eq (car quals) ':before)
					   (eq (car quals) ':after)))))))
		     (and (every #'check-meth initialize-instance-methods)
			  (every #'check-meth shared-initialize-methods))))
	(return-function
	 #'(lambda (class initargs)
	     (apply #'make-instance class initargs))))
      (get-make-instance-function-internal 
       class key (default-initargs class initargs) 
       initialize-instance-methods shared-initialize-methods
       name 
       (or (cadr value)
	   (and *compile-make-instance-functions-p*
		(not *inhibit-compile-make-instance-functions-p*))))))))

(defun get-make-instance-function-internal (class key initargs 
						  initialize-instance-methods
						  shared-initialize-methods
						  &optional (name nil name-p)
						  compile-p)
  (let* ((*compile-make-instance-functions-p* compile-p)
	 (keys (cadr key))
	 (allow-other-keys-p (caddr key))
	 (allocate-instance-methods
	  (compute-applicable-methods (gdefinition 'allocate-instance)
				      (list* class initargs))))
    (unless allow-other-keys-p
      (unless (check-initargs-1
	       class initargs
	       (append allocate-instance-methods
		       initialize-instance-methods
		       shared-initialize-methods)
	       t nil)
	(return-from get-make-instance-function-internal
	  (make-lazy-get-make-instance-function key))))
    (let ((function (if (or (cdr allocate-instance-methods)
			    (some #'complicated-instance-creation-method
				  initialize-instance-methods)
			    (some #'complicated-instance-creation-method
				  shared-initialize-methods))
			(make-instance-function-complex
			 key class keys
			 initialize-instance-methods shared-initialize-methods)
			(make-instance-function-simple
			 key class keys
			 initialize-instance-methods shared-initialize-methods))))
      (when name-p (set-make-instance-function name key function compile-p))
      function)))

(defun set-make-instance-function (name key function compile-p)
  #-cmu (set-function-name function name)
  (when name (setf (symbol-function name) function))
  (setf (gethash key *make-instance-function-table*) 
	(list function compile-p))
  function)

(defun complicated-instance-creation-method (m)
  (let ((qual (method-qualifiers m)))
    (if qual 
	(not (and (null (cdr qual)) (eq (car qual) ':after)))
	(let ((specl (car (method-specializers m))))
	  (or (not (classp specl))
	      (not (eq 'slot-object (class-name specl))))))))

(defun find-standard-ii-method (methods class-names)
  (dolist (m methods)
    (when (null (method-qualifiers m))
      (let ((specl (car (method-specializers m))))
	(when (and (classp specl)
		   (if (listp class-names)
		       (member (class-name specl) class-names)
		       (eq (class-name specl) class-names)))
	  (return m))))))

(defmacro call-initialize-function (initialize-function instance initargs)
  `(let ((.function. ,initialize-function))
     (if (and (consp .function.)
	      (eq (car .function.) 'call-initialize-instance-simple))
	 (initialize-instance-simple (cadr .function.) (caddr .function.)
				     ,instance ,initargs)
	 (funcall .function. ,instance ,initargs))))

(defun make-instance-function-simple (key class keys 
					  initialize-instance-methods 
					  shared-initialize-methods)
  (multiple-value-bind (initialize-function constants)
      (get-simple-initialization-function class keys (caddr key))
    (let* ((wrapper (class-wrapper class))
	   (lwrapper (list wrapper))
	   (allocate-function 
	    (cond ((structure-class-p class)
		   #'allocate-structure-instance)
		  ((standard-class-p class)
		   #'allocate-standard-instance)
		  ((funcallable-standard-class-p class)
		   #'allocate-funcallable-instance)
		  (t 
		   (error "error in make-instance-function-simple"))))
	   (std-si-meth (find-standard-ii-method shared-initialize-methods
						 'slot-object))
	   (shared-initfns
	    (nreverse (mapcar #'(lambda (method)
				  (make-effective-method-function
				   #'shared-initialize
				   `(call-method ,method nil)
				   nil lwrapper))
			      (remove std-si-meth shared-initialize-methods))))
	   (std-ii-meth (find-standard-ii-method initialize-instance-methods
						 'slot-object))
	   (initialize-initfns 
	    (nreverse (mapcar #'(lambda (method)
				  (make-effective-method-function
				   #'initialize-instance
				   `(call-method ,method nil)
				   nil lwrapper))
			      (remove std-ii-meth
				      initialize-instance-methods)))))
      #'(lambda (class1 initargs)
	  (declare (ignore class1))
	  (if (not (eq wrapper (class-wrapper class)))
	      (funcall (get-make-instance-function key) class initargs)
	      (let* ((instance (funcall allocate-function wrapper constants))
		     (initargs (call-initialize-function initialize-function
							 instance initargs)))
		(dolist (fn shared-initfns)
		  (invoke-effective-method-function fn t instance t initargs))
		(dolist (fn initialize-initfns)
		  (invoke-effective-method-function fn t instance initargs))
		instance))))))

(defun make-instance-function-complex (key class keys
					   initialize-instance-methods
					   shared-initialize-methods)
  (multiple-value-bind (initargs-function initialize-function)
      (get-complex-initialization-functions class keys (caddr key))
    (let* ((wrapper (class-wrapper class))
	   (shared-initialize
	    (get-secondary-dispatch-function
	     #'shared-initialize shared-initialize-methods
	     `((class-eq ,class) t t)
	     `((,(find-standard-ii-method shared-initialize-methods 'slot-object)
		,#'(lambda (instance init-type &rest initargs)
		     (declare (ignore init-type))
		     (call-initialize-function initialize-function 
					       instance initargs)
		     instance)))
	     (list wrapper *the-wrapper-of-t* *the-wrapper-of-t*)))
	   (initialize-instance
	    (get-secondary-dispatch-function
	     #'initialize-instance initialize-instance-methods
	     `((class-eq ,class) t)
	     `((,(find-standard-ii-method initialize-instance-methods 'slot-object)
		,#'(lambda (instance &rest initargs)
		     (invoke-effective-method-function
		      shared-initialize t instance t initargs))))
	     (list wrapper *the-wrapper-of-t*))))
      #'(lambda (class1 initargs)
	  (declare (ignore class1))
	  (if (not (eq wrapper (class-wrapper class)))
	      (funcall (get-make-instance-function key) class initargs)
	      (let* ((initargs (call-initialize-function initargs-function 
							 nil initargs))
		     (instance (apply #'allocate-instance class initargs)))
		(invoke-effective-method-function
		 initialize-instance t instance initargs)
		instance))))))

(defmacro define-cached-reader (type name trap)
  (let ((reader-name (intern (format nil "~A-~A" type name)))
	(cached-name (intern (format nil "~A-CACHED-~A" type name))))
    `(defmacro ,reader-name (info)
       `(let ((value (,',cached-name ,info)))
	  (if (eq value ':unknown)
	      (progn
		(,',trap ,info ',',name)
		(,',cached-name ,info))
	      value)))))

(eval-when (compile load eval)
(defparameter initialize-info-cached-slots
  '(valid-p				; t or (:invalid key)
    ri-valid-p
    initargs-form-list
    new-keys
    default-initargs-function
    shared-initialize-t-function
    shared-initialize-nil-function
    constants
    combined-initialize-function)))

(defmacro define-initialize-info ()
  (flet ((cached-slot-name (name)
	   (intern (format nil "CACHED-~A" name)))
	 (cached-name (name)
	   (intern (format nil "~A-CACHED-~A" 'initialize-info name))))
    `(progn
       (defstruct initialize-info 
	 key wrapper 
	 ,@(mapcar #'cached-slot-name initialize-info-cached-slots))
       (defun reset-initialize-info (info)
	 ,@(mapcar #'(lambda (name)
		       `(setf (,(cached-name name) info) ':unknown))
		   initialize-info-cached-slots)
	 info)
      ,@(mapcar #'(lambda (name)
		    `(define-cached-reader initialize-info ,name 
		      update-initialize-info-internal))
	        initialize-info-cached-slots))))

(define-initialize-info)

(defvar *initialize-info-cache-class* nil)
(defvar *initialize-info-cache-initargs* nil)
(defvar *initialize-info-cache-info* nil)

(defun reset-class-initialize-info (class)
  (reset-class-initialize-info-1 (class-initialize-info class)))

(defun reset-class-initialize-info-1 (cell)
  (when (consp cell)
    (when (car cell)
      (setf (initialize-info-wrapper (car cell)) nil))
    (let ((alist (cdr cell)))
      (dolist (a alist)
	(reset-class-initialize-info-1 (cdr a))))))

(defun initialize-info (class initargs &optional (plist-p t) allow-other-keys-arg)
  (let ((info nil))
    (if (and (eq *initialize-info-cache-class* class)
	     (eq *initialize-info-cache-initargs* initargs))
	(setq info *initialize-info-cache-info*)
	(let ((initargs-tail initargs)
	      (cell (or (class-initialize-info class)
			(setf (class-initialize-info class) (cons nil nil)))))
	  (loop (when (null initargs-tail) (return nil))
		(let ((keyword (pop initargs-tail))
		      (alist-cell cell))
		  (when plist-p
		    (if (eq keyword :allow-other-keys)
			(setq allow-other-keys-arg (pop initargs-tail))
			(pop initargs-tail)))
		  (loop (let ((alist (cdr alist-cell)))
			  (when (null alist)
			    (setq cell (cons nil nil))
			    (setf (cdr alist-cell) (list (cons keyword cell)))
			    (return nil))
			  (when (eql keyword (caar alist))
			    (setq cell (cdar alist))
			    (return nil))
			  (setq alist-cell alist)))))
	  (setq info (or (car cell)
			 (setf (car cell) (make-initialize-info))))))
    (let ((wrapper (initialize-info-wrapper info)))
      (unless (eq wrapper (class-wrapper class))
	(unless wrapper
	  (let* ((initargs-tail initargs)
		 (klist-cell (list nil))
		 (klist-tail klist-cell))
	    (loop (when (null initargs-tail) (return nil))
		  (let ((key (pop initargs-tail)))
		    (setf (cdr klist-tail) (list key)))
		  (setf klist-tail (cdr klist-tail))
		  (when plist-p (pop initargs-tail)))
	    (setf (initialize-info-key info)
		  (list class (cdr klist-cell) allow-other-keys-arg))))
	(update-initialize-info info)))
    (setq *initialize-info-cache-class* class)
    (setq *initialize-info-cache-initargs* initargs)
    (setq *initialize-info-cache-info* info)    
    info))

(defun update-initialize-info (info)
  (let* ((key (initialize-info-key info))
	 (class (car key)))
    (setf (initialize-info-wrapper info) (class-wrapper class))
    (reset-initialize-info info)
    info))

(defun update-initialize-info-internal (info name)
  (let* ((key (initialize-info-key info))
	 (class (car key))
	 (keys (cadr key))
	 (allow-other-keys-arg (caddr key)))
    (ecase name
      ((initargs-form-list new-keys)
       (multiple-value-bind (initargs-form-list new-keys)
	   (make-default-initargs-form-list class keys)
	 (setf (initialize-info-cached-initargs-form-list info) initargs-form-list)
	 (setf (initialize-info-cached-new-keys info) new-keys)))
      ((default-initargs-function)
       (let ((initargs-form-list (initialize-info-initargs-form-list info)))
	 (setf (initialize-info-cached-default-initargs-function info)
	       (initialize-instance-simple-function class initargs-form-list))))
      ((valid-p ri-valid-p)
       (flet ((compute-valid-p (methods)
		(or (not (null allow-other-keys-arg))
		    (multiple-value-bind (legal allow-other-keys)
			(check-initargs-values class methods)
		      (or (not (null allow-other-keys))
			  (dolist (key keys t)
			    (unless (member key legal)
			      (return (cons :invalid key)))))))))
	 (let ((proto (class-prototype class)))
	   (setf (initialize-info-cached-valid-p info)
		 (compute-valid-p (list (list* 'allocate-instance class nil)
					(list* 'initialize-instance proto nil)
					(list* 'shared-initialize proto t nil))))
	   (setf (initialize-info-cached-ri-valid-p info)
		 (compute-valid-p (list (list* 'reinitialize-instance proto nil)
					(list* 'shared-initialize proto nil nil)))))))
      ((shared-initialize-t-function)
       (multiple-value-bind (initialize-form-list ignore)
	   (make-shared-initialize-form-list class keys t nil)
	 (declare (ignore ignore))
	 (setf (initialize-info-cached-shared-initialize-t-function info)
	       (initialize-instance-simple-function class initialize-form-list))))
      ((shared-initialize-nil-function)
       (multiple-value-bind (initialize-form-list ignore)
	   (make-shared-initialize-form-list class keys nil nil)
	 (declare (ignore ignore))
	 (setf (initialize-info-cached-shared-initialize-nil-function info)
	       (initialize-instance-simple-function class initialize-form-list))))
      ((constants combined-initialize-function)
       (let ((initargs-form-list (initialize-info-initargs-form-list info))
	     (new-keys (initialize-info-new-keys info)))
	 (multiple-value-bind (initialize-form-list constants)
	     (make-shared-initialize-form-list class new-keys t t)
	   (setf (initialize-info-cached-constants info) constants)
	   (setf (initialize-info-cached-combined-initialize-function info)
		 (initialize-instance-simple-function 
		  class (append initargs-form-list initialize-form-list))))))))
  info)

(defun get-simple-initialization-function (class keys &optional allow-other-keys-arg)
  (let ((info (initialize-info class keys nil allow-other-keys-arg)))
    (values (initialize-info-combined-initialize-function info)
	    (initialize-info-constants info))))

(defun get-complex-initialization-functions (class keys &optional allow-other-keys-arg
						   separate-p)
  (let* ((info (initialize-info class keys nil allow-other-keys-arg))
	 (default-initargs-function (initialize-info-default-initargs-function info)))
    (if separate-p
	(values default-initargs-function
		(initialize-info-shared-initialize-t-function info))
	(values default-initargs-function
		(initialize-info-shared-initialize-t-function
		 (initialize-info class (initialize-info-new-keys info)
				  nil allow-other-keys-arg))))))

(defun add-forms (forms forms-list)
  (when forms
    (setq forms (copy-list forms))
    (if (null (car forms-list))
	(setf (car forms-list) forms)
	(setf (cddr forms-list) forms))
    (setf (cdr forms-list) (last forms)))
  (car forms-list))

(defun make-default-initargs-form-list (class keys &optional (separate-p t))
  (let ((initargs-form-list (cons nil nil))
	(default-initargs (class-default-initargs class))
	(nkeys keys))
    (dolist (default default-initargs)
      (let ((key (car default))
	    (function (cadr default)))
	(unless (member key nkeys)
	  (add-forms `((funcall ,function) (push-initarg ,key))
		     initargs-form-list)
	  (push key nkeys))))
    (when separate-p
      (add-forms `((update-initialize-info-cache
		    ,class ,(initialize-info class nkeys nil)))
		 initargs-form-list))
    (add-forms `((finish-pushing-initargs))
	       initargs-form-list)
    (values (car initargs-form-list) nkeys)))

(defun make-shared-initialize-form-list (class keys si-slot-names simple-p)
  (let* ((initialize-form-list (cons nil nil))
	 (type (cond ((structure-class-p class)
		      'structure)
		     ((standard-class-p class)
		      'standard)
		     ((funcallable-standard-class-p class)
		      'funcallable)
		     (t (error "error in make-shared-initialize-form-list"))))
	 (wrapper (class-wrapper class))
	 (constants (when simple-p
		      (make-list (wrapper-no-of-instance-slots wrapper)
				 ':initial-element *slot-unbound*)))
	 (slots (class-slots class))
	 (slot-names (mapcar #'slot-definition-name slots))
	 (slots-key (mapcar #'(lambda (slot)
				(let ((index most-positive-fixnum))
				  (dolist (key (slot-definition-initargs slot))
				    (let ((pos (position key keys)))
				      (when pos (setq index (min index pos)))))
				  (cons slot index)))
			    slots))
	 (slots (stable-sort slots-key #'< :key #'cdr)))
    (let ((n-popped 0))
      (dolist (slot+index slots)
	(let* ((slot (car slot+index))
	       (name (slot-definition-name slot))
	       (npop (1+ (- (cdr slot+index) n-popped))))
	  (unless (eql (cdr slot+index) most-positive-fixnum)
	    (let* ((pv-offset (1+ (position name slot-names))))
	      (add-forms `(,@(when (plusp npop)
			       `((pop-initargs ,(* 2 npop))))
			   (instance-set ,pv-offset ,slot))
			 initialize-form-list))
	    (incf n-popped npop)))))
    (dolist (slot+index slots)
      (let* ((slot (car slot+index))
	     (name (slot-definition-name slot)))
	(when (and (eql (cdr slot+index) most-positive-fixnum)
		   (or (eq si-slot-names 't)
		       (member name si-slot-names)))
	  (let* ((initform (slot-definition-initform slot))
		 (initfunction (slot-definition-initfunction slot))
		 (location (unless (eq type 'structure)
			     (slot-definition-location slot)))
		 (pv-offset (1+ (position name slot-names)))
		 (forms (cond ((null initfunction)
			       nil)
			      ((constantp initform)
			       (let ((value (funcall initfunction)))
				 (if (and simple-p (integerp location))
				     (progn (setf (nth location constants) value)
					    nil)
				     `((const ,value)
				       (instance-set ,pv-offset ,slot)))))
			      (t
			       `((funcall ,(slot-definition-initfunction slot))
				 (instance-set ,pv-offset ,slot))))))
	    (add-forms `(,@(unless (or simple-p (null forms))
			     `((skip-when-instance-boundp ,pv-offset ,slot
				,(length forms))))
			 ,@forms)
		       initialize-form-list)))))
    (values (car initialize-form-list) constants)))

(defvar *class-pv-table-table* (make-hash-table :test 'eq))

(defun get-pv-cell-for-class (class)
  (let* ((slot-names (mapcar #'slot-definition-name (class-slots class)))
	 (slot-name-lists (list (cons nil slot-names)))
	 (pv-table (gethash class *class-pv-table-table*)))
    (unless (and pv-table
		 (equal slot-name-lists (pv-table-slot-name-lists pv-table)))
      (setq pv-table (intern-pv-table :slot-name-lists slot-name-lists))
      (setf (gethash class *class-pv-table-table*) pv-table))
    (pv-table-lookup pv-table (class-wrapper class))))    

(defvar *initialize-instance-simple-alist* nil)
(defvar *note-iis-entry-p* nil)

(defun initialize-instance-simple-function (class form-list)
  (let ((pv-cell (get-pv-cell-for-class class)))
    (if (and *compile-make-instance-functions-p*
	     (not *inhibit-compile-make-instance-functions-p*))
	(multiple-value-bind (form args)
	    (form-list-to-lisp pv-cell form-list)
	  (let ((entry (assoc form *initialize-instance-simple-alist*
			      :test #'equal)))
	    (unless entry
	      (setq entry (list form
				(unless *note-iis-entry-p* (compile-lambda form))
				nil))
	      (setq *initialize-instance-simple-alist*
		    (nconc *initialize-instance-simple-alist*
			   (list entry))))
	    (if (cadr entry)
		(apply (cadr entry) args)
		`(call-initialize-instance-simple ,pv-cell ,form-list))))
	#||
	#'(lambda (instance initargs)
	    (initialize-instance-simple pv-cell form-list instance initargs))
	||#
	`(call-initialize-instance-simple ,pv-cell ,form-list))))

(defun load-precompiled-iis-entry (form function system)
  (let ((entry (assoc form *initialize-instance-simple-alist*
		      :test #'equal)))
    (unless entry
      (setq entry (list form nil nil))
      (setq *initialize-instance-simple-alist*
	    (nconc *initialize-instance-simple-alist*
		   (list entry))))
    (setf (cadr entry) function)
    (setf (caddr entry) system)))

(defmacro precompile-iis-functions (&optional system)
  (let ((index -1))
    `(progn
      ,@(gathering1 (collecting)
	 (dolist (iis-entry *initialize-instance-simple-alist*)
	   (when (or (null (caddr iis-entry))
		     (eq (caddr iis-entry) system))
	     (when system (setf (caddr iis-entry) system))
	     (gather1
	      (make-top-level-form
	       `(precompile-initialize-instance-simple ,system ,(incf index))
	       '(load)
	       `(load-precompiled-iis-entry
		 ',(car iis-entry)
		 #',(car iis-entry)
		 ',system)))))))))

(defun compile-iis-functions (after-p)
  (let ((*compile-make-instance-functions-p* t)
	(*note-iis-entry-p* (not after-p)))
    (declare (special *compile-make-instance-functions-p*))
    (when (eq *boot-state* 'complete)
      (update-make-instance-function-table))))


;(const const)
;(funcall function)
;(push-initarg const)
;(pop-supplied count) ; a positive odd number 
;(instance-set pv-offset slotd)
;(skip-when-instance-boundp pv-offset slotd n)

(defun initialize-instance-simple (pv-cell form-list instance initargs)
  (let ((pv (car pv-cell))
	(initargs-tail initargs)
	(slots (get-slots-or-nil instance))
	(class (class-of instance))
	value)
    (loop (when (null form-list) (return nil))
	  (let ((form (pop form-list)))
	    (ecase (car form)
	      (push-initarg 
	       (push value initargs)
	       (push (cadr form) initargs))
	      (const
	       (setq value (cadr form)))
	      (funcall
	       (setq value (funcall (cadr form))))
	      (pop-initargs
	       (setq initargs-tail (nthcdr (1- (cadr form)) initargs-tail))
	       (setq value (pop initargs-tail)))
	      (instance-set
	       (instance-write-internal 
		pv slots (cadr form) value
		(setf (slot-value-using-class class instance (caddr form)) value)))
	      (skip-when-instance-boundp
	       (when (instance-boundp-internal 
		      pv slots (cadr form)
		      (slot-boundp-using-class class instance (caddr form)))
		 (dotimes (i (cadddr form))
		   (pop form-list))))
	      (update-initialize-info-cache
	       (when (consp initargs)
		 (setq initargs (cons (car initargs) (cdr initargs))))
	       (setq *initialize-info-cache-class* (cadr form))
	       (setq *initialize-info-cache-initargs* initargs)
	       (setq *initialize-info-cache-info* (caddr form)))
	      (finish-pushing-initargs
	       (setq initargs-tail initargs)))))
    initargs))

(defun add-to-cvector (cvector constant)
  (or (position constant cvector)
      (prog1 (fill-pointer cvector)
	(vector-push-extend constant cvector))))

(defvar *inline-iis-instance-locations-p* t)

(defun first-form-to-lisp (forms cvector pv)
  (flet ((const (constant)
	   (cond ((or (numberp constant) (characterp constant))
		  constant)
		 ((and (symbolp constant) (symbol-package constant))
		  `',constant)
		 (t
		  `(svref cvector ,(add-to-cvector cvector constant))))))
    (let ((form (pop (car forms))))
      (ecase (car form)
	(push-initarg
	 `((push value initargs)
	   (push ,(const (cadr form)) initargs)))
	(const
	 `((setq value ,(const (cadr form)))))
	(funcall
	 `((setq value (funcall (the function ,(const (cadr form)))))))
	(pop-initargs
	 `((setq initargs-tail (,@(let ((pop (1- (cadr form))))
				    (case pop
				      (1 `(cdr))
				      (3 `(cdddr))
				      (t `(nthcdr ,pop))))
				initargs-tail))
	   (setq value (pop initargs-tail))))
	(instance-set
	 (let* ((pv-offset (cadr form))
		(location (pvref pv pv-offset))
		(default `(setf (slot-value-using-class class instance
							,(const (caddr form)))
				value)))
	   (if *inline-iis-instance-locations-p*
	       (typecase location
		 (fixnum `((setf (%instance-ref slots ,(const location)) value)))
		 (cons `((setf (cdr ,(const location)) value)))
		 (t `(,default)))
	       `((instance-write-internal pv slots ,(const pv-offset) value
		  ,default
		  ,(typecase location
		     (fixnum ':instance)
		     (cons ':class)
		     (t ':default)))))))
	(skip-when-instance-boundp
	 (let* ((pv-offset (cadr form))
		(location (pvref pv pv-offset))
		(default `(slot-boundp-using-class class instance
			   ,(const (caddr form)))))
	   `((unless ,(if *inline-iis-instance-locations-p*
			  (typecase location
			    (fixnum `(not (eq (%instance-ref slots ,(const location))
					      ',*slot-unbound*)))
			    (cons `(not (eq (cdr ,(const location)) ',*slot-unbound*)))
			    (t default))
			  `(instance-boundp-internal pv slots ,(const pv-offset)
			    ,default
			    ,(typecase (pvref pv pv-offset)
			       (fixnum ':instance)
			       (cons ':class)
			       (t ':default))))
	       ,@(let ((sforms (cons nil nil)))
		   (dotimes (i (cadddr form) (car sforms))
		     (add-forms (first-form-to-lisp forms cvector pv) sforms)))))))
	(update-initialize-info-cache
	 `((when (consp initargs)
	     (setq initargs (cons (car initargs) (cdr initargs))))
	   (setq *initialize-info-cache-class* ,(const (cadr form)))
	   (setq *initialize-info-cache-initargs* initargs)
	   (setq *initialize-info-cache-info* ,(const (caddr form)))))
	(finish-pushing-initargs
	 `((setq initargs-tail initargs)))))))

(defmacro iis-body (&body forms)
  `(let ((initargs-tail initargs)
	 (slots (get-slots-or-nil instance))
	 (class (class-of instance))
	 (pv (car pv-cell))
	 value)
     initargs instance initargs-tail pv cvector slots class value
     ,@forms))

(defun form-list-to-lisp (pv-cell form-list)
  (let* ((forms (list form-list))
	 (cvector (make-array (floor (length form-list) 2)
			      :fill-pointer 0 :adjustable t))
	 (pv (car pv-cell))
	 (body (let ((rforms (cons nil nil)))
		 (loop (when (null (car forms)) (return (car rforms)))
		       (add-forms (first-form-to-lisp forms cvector pv)
				  rforms))))
	 (cvector-type `(simple-vector ,(length cvector))))
    (values
     `(lambda (pv-cell cvector)
        (declare (type ,cvector-type cvector))
        #'(lambda (instance initargs)
	    (declare #.*optimize-speed*)
	    (iis-body ,@body)
	    initargs))
     (list pv-cell (coerce cvector cvector-type)))))
