;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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

(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/defcombin.lisp,v 1.15 2002/08/26 16:09:34 pmai Exp $")
;;;

(in-package :pcl)

;;;
;;; DEFINE-METHOD-COMBINATION
;;;

(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  (if (and (cddr form)
	   (listp (caddr form)))
      (expand-long-defcombin form)
      (expand-short-defcombin form)))


;;;
;;; Implementation of INVALID-METHOD-ERROR and METHOD-COMBINATION-ERROR
;;;
;;; See combin.lisp for rest of the implementation.  This method is
;;; defined here because compute-effective-method is still a function
;;; in combin.lisp.
;;;
(defmethod compute-effective-method :around
    ((generic-function generic-function)
     (method-combination method-combination)
     applicable-methods)
  (declare (ignore applicable-methods))
  (flet ((real-invalid-method-error (method format-string &rest args)
	   (declare (ignore method))
	   (apply #'error format-string args))
	 (real-method-combination-error (format-string &rest args)
	   (apply #'error format-string args)))
    (let ((*invalid-method-error* #'real-invalid-method-error)
	  (*method-combination-error* #'real-method-combination-error))
      (call-next-method))))



;;;
;;; STANDARD method combination
;;;
;;; The STANDARD method combination type is implemented directly by the class
;;; STANDARD-METHOD-COMBINATION.  The method on COMPUTE-EFFECTIVE-METHOD does
;;; standard method combination directly and is defined by hand in the file
;;; combin.lisp.  The method for FIND-METHOD-COMBINATION must appear in this
;;; file for bootstrapping reasons.
;;;
;;; A commented out copy of this definition appears in combin.lisp.
;;; If you change this definition here, be sure to change it there
;;; also.
;;;
(defmethod find-method-combination ((generic-function generic-function)
				    (type (eql 'standard))
				    options)
  (when options
    (method-combination-error
      "The method combination type STANDARD accepts no options."))
  *standard-method-combination*)



;;;
;;; short method combinations
;;;
;;; Short method combinations all follow the same rule for computing the
;;; effective method.  So, we just implement that rule once.  Each short
;;; method combination object just reads the parameters out of the object
;;; and runs the same rule.
;;;
;;;
(defclass short-method-combination (standard-method-combination)
     ((operator
	:reader short-combination-operator
	:initarg :operator)
      (identity-with-one-argument
	:reader short-combination-identity-with-one-argument
	:initarg :identity-with-one-argument))
  (:predicate-name short-method-combination-p))

(defun expand-short-defcombin (whole)
  (let* ((type (cadr whole))
	 (documentation
	   (getf (cddr whole) :documentation ""))
	 (identity-with-one-arg
	   (getf (cddr whole) :identity-with-one-argument nil))
	 (operator 
	   (getf (cddr whole) :operator type)))
    (make-top-level-form `(define-method-combination ,type)
			 '(load eval)
      `(load-short-defcombin
	 ',type ',operator ',identity-with-one-arg ',documentation))))

(defun load-short-defcombin (type operator ioa doc)
  (let* ((truename (load-truename))
	 (specializers
	   (list (find-class 'generic-function)
		 (intern-eql-specializer type)
		 *the-class-t*))
	 (old-method
	   (get-method #'find-method-combination () specializers nil))
	 (new-method nil))
    (setq new-method
	  (make-instance 'standard-method
	    :qualifiers ()
	    :specializers specializers
	    :lambda-list '(generic-function type options)
	    :function (lambda(args nms &rest cm-args)
			(declare (ignore nms cm-args))
			(apply 
			 (lambda (gf type options)
			   (declare (ignore gf))
			   (do-short-method-combination
			       type options operator ioa new-method doc))
			 args))
	    :definition-source `((define-method-combination ,type) ,truename)))
    (when old-method
      (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)))

(defun do-short-method-combination (type options operator ioa method doc)
  (cond ((null options) (setq options '(:most-specific-first)))
	((equal options '(:most-specific-first)))
	((equal options '(:most-specific-last)))
	(t
	 (method-combination-error
	   "Illegal options to a short method combination type.~%~
            The method combination type ~S accepts one option which~%~
            must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
	   type)))
  (make-instance 'short-method-combination
		 :type type
		 :options options
		 :operator operator
		 :identity-with-one-argument ioa
		 :definition-source method
		 :documentation doc))

(defmethod compute-effective-method ((generic-function generic-function)
				     (combin short-method-combination)
				     applicable-methods)
  (let ((type (method-combination-type combin))
	(operator (short-combination-operator combin))
	(ioa (short-combination-identity-with-one-argument combin))
	(order (car (method-combination-options combin)))
	(around ())
	(primary ()))
    (dolist (m applicable-methods)
      (let ((qualifiers (method-qualifiers m)))
	(flet ((lose (method why)
		 (invalid-method-error
		   method
		   "The method ~S ~A.~%~
                    The method combination type ~S was defined with the~%~
                    short form of DEFINE-METHOD-COMBINATION and so requires~%~
                    all methods have either the single qualifier ~S or the~%~
                    single qualifier :AROUND."
		   method why type type)))
	  (cond ((null qualifiers)
		 (lose m "has no qualifiers"))
		((cdr qualifiers)
		 (lose m "has more than one qualifier"))
		((eq (car qualifiers) :around)
		 (push m around))
		((eq (car qualifiers) type)
		 (push m primary))
		(t
		 (lose m "has an illegal qualifier"))))))
    (setq around (nreverse around))
    (unless (eq order :most-specific-last)
      (setq primary (nreverse primary)))
    (let ((main-method
	    (if (and (null (cdr primary))
		     (not (null ioa)))
		`(call-method ,(car primary) ())
		`(,operator ,@(mapcar (lambda (m) `(call-method ,m ()))
				      primary)))))
      (cond ((null primary)
	     `(error "No ~S methods for the generic function ~S."
		     ',type ',generic-function))
	    ((null around) main-method)
	    (t
	     `(call-method ,(car around)
			   (,@(cdr around) (make-method ,main-method))))))))


;;;
;;; long method combinations
;;;
;;;

(defclass long-method-combination (standard-method-combination)
     ((function :initarg :function
		:reader long-method-combination-function)))

(defun expand-long-defcombin (form)
  (let ((type (cadr form))
	(lambda-list (caddr form))
	(method-group-specifiers (cadddr form))
	(body (cddddr form))
	(arguments-option ())
	(gf-var nil))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq arguments-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (setq gf-var (cadr (pop body))))
    (multiple-value-bind (documentation function)
	(make-long-method-combination-function
	  type lambda-list method-group-specifiers arguments-option gf-var
	  body)
      (make-top-level-form `(define-method-combination ,type)
			   '(load eval)
	`(load-long-defcombin ',type ',documentation #',function)))))

(defvar *long-method-combination-functions* (make-hash-table :test #'eq))

(defun load-long-defcombin (type doc function)
  (let* ((specializers
	   (list (find-class 'generic-function)
		 (intern-eql-specializer type)
		 *the-class-t*))
	 (old-method
	   (get-method #'find-method-combination () specializers nil))
	 (new-method
	   (make-instance 'standard-method
	     :qualifiers ()
	     :specializers specializers
	     :lambda-list '(generic-function type options)
	     :function (lambda (args nms &rest cm-args)
			 (declare (ignore nms cm-args))
			 (apply
			  (lambda (generic-function type options)
			    (declare (ignore generic-function))
			    (make-instance 'long-method-combination
					   :type type
					   :options options
					   :documentation doc))
			  args))
	 :definition-source `((define-method-combination ,type)
			      ,(load-truename)))))
    (setf (gethash type *long-method-combination-functions*) function)
    (when old-method (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)))

(defmethod compute-effective-method ((generic-function generic-function)
				     (combin long-method-combination)
				     applicable-methods)
  (funcall (gethash (method-combination-type combin)
		    *long-method-combination-functions*)
	   generic-function
	   combin
	   applicable-methods))

;;;
;;;
;;;
(defun make-long-method-combination-function
       (type ll method-group-specifiers arguments-option gf-var body)
  ;;(declare (values documentation function))
  (declare (ignore type))
  (multiple-value-bind (real-body declarations documentation)
      ;; Note that PARSE-BODY ignores its second arg ENVIRONMENT.
      (system:parse-body body nil)

    (let ((wrapped-body
	    (wrap-method-group-specifier-bindings method-group-specifiers
						  declarations
						  real-body)))
      (when gf-var
	(push `(,gf-var .generic-function.) (cadr wrapped-body)))
      
      (when arguments-option
	(setq wrapped-body (deal-with-arguments-option wrapped-body
						       arguments-option)))

      (when ll
	(setq wrapped-body
	      `(apply (lambda ,ll ,wrapped-body)
		      (method-combination-options .method-combination.))))

      (values
	documentation
	`(lambda (.generic-function. .method-combination. .applicable-methods.)
	   (progn .generic-function. .method-combination. .applicable-methods.)
	   (block .long-method-combination-function. ,wrapped-body))))))
;;
;; parse-method-group-specifiers parse the method-group-specifiers
;;

(defun wrap-method-group-specifier-bindings
       (method-group-specifiers declarations real-body)
  (let ((names ())
	(specializer-caches ())
	(cond-clauses ())
	(required-checks ())
	(order-cleanups ()))
    (dolist (method-group-specifier method-group-specifiers)
      (multiple-value-bind (name tests description order required)
	  (parse-method-group-specifier method-group-specifier)
	(declare (ignore description))
	(let ((specializer-cache (gensym)))
	  (push name names)
	  (push specializer-cache specializer-caches)
	  (push `((or ,@tests)
		      (if  (equal ,specializer-cache .specializers.)
			   (return-from .long-method-combination-function.
			     '(error "More than one method of type ~S ~
                                      with the same specializers."
				     ',name))
			   (setq ,specializer-cache .specializers.))
		      (push .method. ,name))
		cond-clauses)
	  (when required
	    (push `(when (null ,name)
		     (return-from .long-method-combination-function.
		       '(error "No ~S methods." ',name)))
		  required-checks))
	  (loop (unless (and (constantp order)
			     (neq order (setq order (eval order))))
		  (return t)))
	  (push (cond ((eq order :most-specific-first)
		       `(setq ,name (nreverse ,name)))
		      ((eq order :most-specific-last) ())
		      (t
		       `(ecase ,order
			 (:most-specific-first
			  (setq ,name (nreverse ,name)))
			 (:most-specific-last))))
		order-cleanups))))
    `(let (,@(nreverse names) ,@(nreverse specializer-caches))
        ,@declarations
        (dolist (.method. .applicable-methods.)
	  (let ((.qualifiers. (method-qualifiers .method.))
		(.specializers. (method-specializers .method.)))
	    (progn .qualifiers. .specializers.)
	    (cond ,@(nreverse cond-clauses))))
      ,@(nreverse required-checks)
      ,@(nreverse order-cleanups)
      ,@real-body)))
   
(defun parse-method-group-specifier (method-group-specifier)
  ;;(declare (values name tests description order required))
  (loop
     with name = (pop method-group-specifier)
     for pattern = (pop method-group-specifier)
     until (null pattern)
     until (memq pattern '(:description :order :required))
     collect pattern into patterns
     collect (parse-qualifier-pattern name pattern) into tests
     finally
       (return
	 (values name
		 tests
		 (getf method-group-specifier :description
		       (make-default-method-group-description
			(nreverse patterns)))
		 (getf method-group-specifier :order :most-specific-first)
		 (getf method-group-specifier :required nil)))))

(defun parse-qualifier-pattern (name pattern)
  (cond ((eq pattern '()) `(null .qualifiers.))
	((eq pattern '*) t)
	((symbolp pattern) `(,pattern .qualifiers.))
	((listp pattern) `(qualifier-check-runtime ',pattern .qualifiers.))
	(t (error "In the method group specifier ~S,~%~
                   ~S isn't a valid qualifier pattern."
		  name pattern))))

(defun qualifier-check-runtime (pattern qualifiers)
  (loop (cond ((and (null pattern) (null qualifiers))
	       (return t))
	      ((eq pattern '*) (return t))
	      ((and pattern qualifiers
		    (let ((element (car pattern)))
		      (or (eq element (car qualifiers))
			  (eq element '*))))
	       (pop pattern)
	       (pop qualifiers))	      
	      (t (return nil)))))

(defun make-default-method-group-description (patterns)
  (if (cdr patterns)
      (format nil
	      "methods matching one of the patterns: ~{~S, ~} ~S"
	      (butlast patterns) (car (last patterns)))
      (format nil
	      "methods matching the pattern: ~S"
	      (car patterns))))



;;;
;;; This baby is a complete mess.  I can't believe we put it in this
;;; way.  No doubt this is a large part of what drives MLY crazy.
;;;
;;; At runtime (when the effective-method is run), we bind an intercept
;;; lambda-list to the arguments to the generic function.
;;; 
;;; At compute-effective-method time, the symbols in the :arguments
;;; option are bound to the symbols in the intercept lambda list.
;;;
(defun deal-with-arguments-option (wrapped-body arguments-option)
  (let* ((intercept-lambda-list
	  (loop for arg in arguments-option
		collect (if (memq arg lambda-list-keywords)
			    arg
			    (gensym))))
	 (intercept-rebindings
	  (loop for arg in arguments-option
		and int in intercept-lambda-list
		unless (memq arg lambda-list-keywords)
		  collect `(,arg ',int))))
    ;;
    ;;
    (setf (cadr wrapped-body)
	  (append intercept-rebindings (cadr wrapped-body)))
    ;;
    ;; Be sure to fill out the intercept lambda list so that it can
    ;; be too short if it wants to.
    ;; 
    (cond ((memq '&rest intercept-lambda-list))
	  ((memq '&allow-other-keys intercept-lambda-list))
	  ((memq '&key intercept-lambda-list)
	   (setq intercept-lambda-list
		 (append intercept-lambda-list '(&allow-other-keys))))
	  (t
	   (setq intercept-lambda-list
		 (append intercept-lambda-list '(&rest .ignore.)))))

    `(let ((inner-result. ,wrapped-body))
       `(apply (lambda ,',intercept-lambda-list
		   ,,(when (memq '.ignore. intercept-lambda-list)
		       ''(declare (ignore .ignore.)))
		   ,inner-result.)
	       .combined-method-args.))))


