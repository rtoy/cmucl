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

(in-package 'pcl)

;;;
;;; METHODS
;;;
;;; Methods themselves are simple inanimate objects.  Most properties of
;;; methods are immutable, methods cannot be reinitialized.  The following
;;; properties of methods can be changed:
;;;   METHOD-GENERIC-FUNCTION
;;;   METHOD-FUNCTION            ??
;;;   
;;;

(defclass method (metaobject) ()
  (:predicate-name method-p))

(defclass standard-method (definition-source-mixin plist-mixin method)
     ((generic-function
	:initform nil	
	:accessor method-generic-function)
;     (qualifiers
;	:initform ()
;	:initarg  :qualifiers
;	:reader method-qualifiers)
      (specializers
	:initform ()
	:initarg  :specializers
	:reader method-specializers)
      (lambda-list
	:initform ()
	:initarg  :lambda-list
	:reader method-lambda-list)
      (function
	:initform nil
	:initarg :function		;no writer
	:reader method-function)
      (function-name
        :initform nil
        :accessor method-function-name)
      (closure-generator
        :initform nil
	:reader method-closure-generator)
;     (documentation
;	:initform nil
;	:initarg  :documentation
;	:reader method-documentation)
      )
  (:predicate-name standard-method-p))

(defclass standard-accessor-method (standard-method)
     ((slot-name :initform nil
		 :initarg :slot-name
		 :reader accessor-method-slot-name))
  (:predicate-name standard-accessor-method-p))

(defclass standard-reader-method (standard-accessor-method) ()
  (:predicate-name standard-reader-method-p))

(defclass standard-writer-method (standard-accessor-method) ()
  (:predicate-name standard-writer-method-p))

(defclass standard-boundp-method (standard-accessor-method) ()
  (:predicate-name standard-boundp-method-p))

(defvar *the-class-method*                    (find-class 'method))
(defvar *the-class-standard-method*           (find-class 'standard-method))
(defvar *the-class-standard-reader-method*    (find-class 'standard-reader-method))
(defvar *the-class-standard-writer-method*    (find-class 'standard-writer-method))
(defvar *the-class-standard-boundp-method*    (find-class 'standard-boundp-method))

(defmethod print-object ((method standard-method) stream)
  (printing-random-thing (method stream)
    (let ((generic-function (method-generic-function method))
	  (class-name (capitalize-words (class-name (class-of method)))))
      (format stream "~A ~S ~{~S ~}~:S"
	      class-name
	      (and generic-function (generic-function-name generic-function))
	      (method-qualifiers method)
	      (unparse-specializers method)))))

(defmethod print-object ((method standard-accessor-method) stream)
  (printing-random-thing (method stream)
    (let ((generic-function (method-generic-function method))
	  (class-name (capitalize-words (class-name (class-of method)))))
      (format stream "~A ~S, slot:~S, ~:S"
	      class-name
	      (and generic-function (generic-function-name generic-function))
	      (accessor-method-slot-name method)
	      (unparse-specializers method)))))

;;; closure-generators are used only by method-function-for-caching
;;; and make-not-for-caching-method-function (in vector.lisp).
(defmethod method-function-for-caching ((method standard-method) wrappers)
  (let* ((function (slot-value method 'function))
	 (closure-generator (slot-value method 'closure-generator)))
    (if closure-generator
	(let ((new-function (function-funcall closure-generator wrappers)))
	  (setq new-function (set-function-name-1 new-function
						  (method-function-name method) nil))
	  #+(and kcl turbo-closure) (si:turbo-closure new-function)
	  new-function)
	function)))

(defmethod method-function-for-caching-p ((method standard-method))
  (slot-value method 'closure-generator))

(defmethod method-function-for-caching ((method standard-accessor-method) wrappers)
  (let* ((slot-name (slot-value method 'slot-name))
	 (type (cond ((standard-reader-method-p method) 'reader)
		     ((standard-writer-method-p method) 'writer)
		     ((standard-boundp-method-p method) 'boundp)))
	 (class (wrapper-class (if (eq type 'writer) (cadr wrappers) (car wrappers))))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-accessor-function slotd type)
	(slot-value method 'function))))

(defmethod method-function-for-caching-p ((method standard-accessor-method))
  t)

;;;
;;; INITIALIZATION
;;;
;;; Error checking is done in before methods.  Because of the simplicity of
;;; standard method objects the standard primary method can fill the slots.
;;;
;;; Methods are not reinitializable.
;;; 

(defmethod reinitialize-instance ((method standard-method) &rest initargs)
  (declare (ignore initargs))
  (error "Attempt to reinitialize the method ~S.~%~
          Method objects cannot be reinitialized."
	 method))

(defmethod legal-documentation-p ((object standard-method) x)
  (if (or (null x) (stringp x))
      t
      "a string or NULL"))

(defmethod legal-lambda-list-p ((object standard-method) x)
  (declare (ignore x))
  t)

(defmethod legal-method-function-p ((object standard-method) x)
  (if (functionp x)
      t
      "a function"))

(defmethod legal-qualifiers-p ((object standard-method) x)
  (flet ((improper-list ()
	   (return-from legal-qualifiers-p "Is not a proper list.")))
    (dolist-carefully (q x improper-list)
      (let ((ok (legal-qualifier-p object q)))
	(unless (eq ok t)
	  (return-from legal-qualifiers-p
	    (format nil "Contains ~S which ~A" q ok)))))
    t))

(defmethod legal-qualifier-p ((object standard-method) x)
  (if (and x (atom x))
      t
      "is not a non-null atom"))

(defmethod legal-slot-name-p ((object standard-method) x)
  (cond ((not (symbolp x)) "is not a symbol and so cannot be bound")
	((keywordp x)      "is a keyword and so cannot be bound")
	((memq x '(t nil)) "cannot be bound")
	(t t)))

(defmethod legal-specializers-p ((object standard-method) x)
  (flet ((improper-list ()
	   (return-from legal-specializers-p "Is not a proper list.")))
    (dolist-carefully (s x improper-list)
      (let ((ok (legal-specializer-p object s)))
	(unless (eq ok t)
	  (return-from legal-specializers-p
	    (format nil "Contains ~S which ~A" s ok)))))
    t))

(defvar *allow-experimental-specializers-p* nil)

(defmethod legal-specializer-p ((object standard-method) x)
  (if (if *allow-experimental-specializers-p*
	  (specializerp x)
	  (or (classp x)
	      (eql-specializer-p x)))
      t
      "is neither a class object nor an eql specializer"))

(defmethod shared-initialize :before ((method standard-method)
				      slot-names
				      &key qualifiers
					   lambda-list
					   specializers
					   function
					   documentation)
  (declare (ignore slot-names))
  (flet ((lose (initarg value string)
	   (error "When initializing the method ~S:~%~
                   The ~S initialization argument was: ~S.~%~
                   which ~A."
		  method initarg value string)))
    (let ((check-qualifiers    (legal-qualifiers-p method qualifiers))
	  (check-lambda-list   (legal-lambda-list-p method lambda-list))
	  (check-specializers  (legal-specializers-p method specializers))
	  (check-function      (legal-method-function-p method function))
	  (check-documentation (legal-documentation-p method documentation)))
      (unless (eq check-qualifiers t)
	(lose :qualifiers qualifiers check-qualifiers))
      (unless (eq check-lambda-list t)
	(lose :lambda-list lambda-list check-lambda-list))
      (unless (eq check-specializers t)
	(lose :specializers specializers check-specializers))
      (unless (eq check-function t)
	(lose :function function check-function))
      (unless (eq check-documentation t)
	(lose :documentation documentation check-documentation)))))

(defmethod shared-initialize :before ((method standard-accessor-method)
				      slot-names
				      &key slot-name)
  (declare (ignore slot-names))
  (let ((legalp (legal-slot-name-p method slot-name)))
    (unless (eq legalp t)
      (error "The value of the :SLOT-NAME initarg ~A." legalp))))

(defmethod shared-initialize :after ((method standard-method) slot-names 
				     &key qualifiers)
  (declare (ignore slot-names))
  (setf (plist-value method 'qualifiers) qualifiers)
  (setf (slot-value method 'closure-generator) 
	(method-function-closure-generator (slot-value method 'function))))

(defmethod method-qualifiers ((method standard-method))
  (plist-value method 'qualifiers))



(defclass generic-function (dependent-update-mixin
			    definition-source-mixin
			    documentation-mixin
			    metaobject)
     ()
  (:metaclass funcallable-standard-class)
  (:predicate-name generic-function-p))
    
(defclass standard-generic-function (generic-function)
     ((name
	:initform nil
	:initarg :name
	:accessor generic-function-name)
      (methods
	:initform ()
	:accessor generic-function-methods)
      (method-class
	:initarg :method-class
	:accessor generic-function-method-class)
      (method-combination
	:initarg :method-combination
	:accessor generic-function-method-combination)

;     (permutation
;	:accessor gf-permutation)
      (arg-info
        :initform (make-arg-info)
	:reader gf-arg-info)
      (dfun-state
	:initform ()
	:accessor gf-dfun-state)
      (valid-p
	:initform nil
	:accessor gf-valid-p)
      (pretty-arglist
	:initform ()
	:accessor gf-pretty-arglist)
      )
  (:metaclass funcallable-standard-class)
  (:predicate-name standard-generic-function-p)
  (:default-initargs :method-class *the-class-standard-method*
		     :method-combination *standard-method-combination*))


(defvar *the-class-generic-function*          (find-class 'generic-function))
(defvar *the-class-standard-generic-function* (find-class 'standard-generic-function))



(defmethod print-object ((generic-function generic-function) stream)
  (named-object-print-function
    generic-function
    stream
    (list (length (generic-function-methods generic-function)))))


(defmethod shared-initialize :before
	   ((generic-function standard-generic-function)
	    slot-names
	    &key (name nil namep)
		 (lambda-list () lambda-list-p)
		 argument-precedence-order
		 declarations
		 documentation
		 (method-class nil method-class-supplied-p)
		 (method-combination nil method-combination-supplied-p))
  (declare (ignore slot-names
		   declarations argument-precedence-order documentation
		   lambda-list lambda-list-p name))

  (when namep
    (set-function-name generic-function name))
		   
  (flet ((initarg-error (initarg value string)
	   (error "When initializing the generic-function ~S:~%~
                   The ~S initialization argument was: ~A.~%~
                   It must be ~A."
		  generic-function initarg value string)))
    (cond (method-class-supplied-p
	   (when (symbolp method-class)
	     (setq method-class (find-class method-class)))
	   (unless (and (classp method-class)
			(*subtypep (class-eq-specializer method-class)
				   *the-class-method*))
	     (initarg-error :method-class
			    method-class
			    "a subclass of the class METHOD"))
	   (setf (slot-value generic-function 'method-class) method-class))
	  ((slot-boundp generic-function 'method-class))
	  (t
	   (initarg-error :method-class
			  "not supplied"
			  "a subclass of the class METHOD")))
    (cond (method-combination-supplied-p
	   (unless (method-combination-p method-combination)
	     (initarg-error :method-combination
			    method-combination
			    "a method combination object")))
	  ((slot-boundp generic-function 'method-combination))
	  (t
	   (initarg-error :method-combination
			  "not supplied"
			  "a method combination object")))))


#||
(defmethod reinitialize-instance ((generic-function standard-generic-function)
				  &rest initargs
				  &key name
				       lambda-list
				       argument-precedence-order
				       declarations
				       documentation
				       method-class
				       method-combination)
  (declare (ignore documentation declarations argument-precedence-order
		   lambda-list name method-class method-combination))
  (macrolet ((add-initarg (check name slot-name)
	       `(unless ,check
		  (push (slot-value generic-function ,slot-name) initargs)
		  (push ,name initargs))))
;   (add-initarg name :name 'name)
;   (add-initarg lambda-list :lambda-list 'lambda-list)
;   (add-initarg argument-precedence-order
;		 :argument-precedence-order
;		 'argument-precedence-order)
;   (add-initarg declarations :declarations 'declarations)
;   (add-initarg documentation :documentation 'documentation)
;   (add-initarg method-class :method-class 'method-class)
;   (add-initarg method-combination :method-combination 'method-combination)
    (apply #'call-next-method generic-function initargs)))
||#


;;;
;;; These three are scheduled for demolition.
;;; 
(defmethod remove-named-method (generic-function-name argument-specifiers
						      &optional extra)
  (let ((generic-function ())
	(method ()))
    (cond ((or (null (fboundp generic-function-name))
	       (not (generic-function-p
		      (setq generic-function
			    (symbol-function generic-function-name)))))
	   (error "~S does not name a generic-function."
		  generic-function-name))
	  ((null (setq method (get-method generic-function
					  extra
					  (parse-specializers
					    argument-specifiers)
					  nil)))
	   (error "There is no method for the generic-function ~S~%~
                   which matches the argument-specifiers ~S."
		  generic-function
		  argument-specifiers))
	  (t
	   (remove-method generic-function method)))))

(defun real-add-named-method (generic-function-name
			      qualifiers
			      specializers
			      lambda-list
			      function
			      &rest other-initargs)
  ;; What about changing the class of the generic-function if there is
  ;; one.  Whose job is that anyways.  Do we need something kind of
  ;; like class-for-redefinition?
  (let* ((generic-function
	   (ensure-generic-function generic-function-name
				    :lambda-list (method-ll->generic-function-ll
						  lambda-list)))
	 (specs (parse-specializers specializers))
;	 (existing (get-method generic-function qualifiers specs nil))
	 (proto (method-prototype-for-gf generic-function-name))
	 (new (apply #'make-instance (class-of proto)
				     :qualifiers qualifiers
				     :specializers specs
				     :lambda-list lambda-list
				     :function function
				     other-initargs)))
;   (when existing (remove-method generic-function existing))
    (add-method generic-function new)))

	
(defun make-specializable (function-name &key (arglist nil arglistp))
  (cond ((not (null arglistp)))
	((not (fboundp function-name)))
	((fboundp 'function-arglist)
	 ;; function-arglist exists, get the arglist from it.
	 (setq arglist (function-arglist function-name)))
	(t
	 (error
	   "The :arglist argument to make-specializable was not supplied~%~
            and there is no version of FUNCTION-ARGLIST defined for this~%~
            port of Portable CommonLoops.~%~
            You must either define a version of FUNCTION-ARGLIST (which~%~
            should be easy), and send it off to the Portable CommonLoops~%~
            people or you should call make-specializable again with the~%~
            :arglist keyword to specify the arglist.")))
  (let ((original (and (fboundp function-name)
		       (symbol-function function-name)))
	(generic-function (make-instance 'standard-generic-function
					 :name function-name))
	(nrequireds 0))
    (if (generic-function-p original)
	original
	(progn
	  (dolist (arg arglist)
	    (if (memq arg lambda-list-keywords)
		(return)
		(incf nrequireds)))
	  (setf (symbol-function function-name) generic-function)
	  (set-function-name generic-function function-name)
	  (when arglistp
	    (setf (gf-pretty-arglist generic-function) arglist))
	  (when original
	    (add-named-method function-name
			      ()
			      (make-list nrequireds :initial-element 't)
			      arglist
			      original))
	  generic-function))))



(defun real-get-method (generic-function qualifiers specializers
					 &optional (errorp t))
  (let ((hit
	  (dolist (method (generic-function-methods generic-function))
	    (when (and (equal qualifiers (method-qualifiers method))
		       (every #'same-specializer-p specializers
			      (method-specializers method)))
	      (return method)))))
    (cond (hit hit)
	  ((null errorp) nil)
	  (t
	   (error "No method on ~S with qualifiers ~:S and specializers ~:S."
		  generic-function qualifiers specializers)))))


;;;
;;; Compute various information about a generic-function's arglist by looking
;;; at the argument lists of the methods.  The hair for trying not to use
;;; &rest arguments lives here.
;;;  The values returned are:
;;;    number-of-required-arguments
;;;       the number of required arguments to this generic-function's
;;;       discriminating function
;;;    &rest-argument-p
;;;       whether or not this generic-function's discriminating
;;;       function takes an &rest argument.
;;;    specialized-argument-positions
;;;       a list of the positions of the arguments this generic-function
;;;       specializes (e.g. for a classical generic-function this is the
;;;       list: (1)).
;;;
(defmethod compute-discriminating-function-arglist-info
	   ((generic-function standard-generic-function))
  ;;(declare (values number-of-required-arguments &rest-argument-p
  ;;                 specialized-argument-postions))
  (let ((number-required nil)
        (restp nil)
        (specialized-positions ())
	(methods (generic-function-methods generic-function)))
    (dolist (method methods)
      (multiple-value-setq (number-required restp specialized-positions)
        (compute-discriminating-function-arglist-info-internal
	  generic-function method number-required restp specialized-positions)))
    (values number-required restp (sort specialized-positions #'<))))

(defun compute-discriminating-function-arglist-info-internal
       (generic-function method number-of-requireds restp
	specialized-argument-positions)
  (declare (ignore generic-function) (type (or null fixnum) number-of-requireds))
  (let ((requireds 0))
    (declare (fixnum requireds))
    ;; Go through this methods arguments seeing how many are required,
    ;; and whether there is an &rest argument.
    (dolist (arg (method-lambda-list method))
      (cond ((eq arg '&aux) (return))
            ((memq arg '(&optional &rest &key))
             (return (setq restp t)))
	    ((memq arg lambda-list-keywords))
            (t (incf requireds))))
    ;; Now go through this method's type specifiers to see which
    ;; argument positions are type specified.  Treat T specially
    ;; in the usual sort of way.  For efficiency don't bother to
    ;; keep specialized-argument-positions sorted, rather depend
    ;; on our caller to do that.
    (iterate ((type-spec (list-elements (method-specializers method)))
              (pos (interval :from 0)))
      (unless (eq type-spec *the-class-t*)
	(pushnew pos specialized-argument-positions)))
    ;; Finally merge the values for this method into the values
    ;; for the exisiting methods and return them.  Note that if
    ;; num-of-requireds is NIL it means this is the first method
    ;; and we depend on that.
    (values (min (or number-of-requireds requireds) requireds)
            (or restp
		(and number-of-requireds (/= number-of-requireds requireds)))
            specialized-argument-positions)))

(defun make-discriminating-function-arglist (number-required-arguments restp)
  (nconc (gathering ((args (collecting)))
           (iterate ((i (interval :from 0 :below number-required-arguments)))
             (gather (intern (format nil "Discriminating Function Arg ~D" i))
		     args)))
         (when restp
               `(&rest ,(intern "Discriminating Function &rest Arg")))))


;;;
;;;
;;;
(defstruct (arg-info
	     (:conc-name nil)
	     (:constructor make-arg-info ()))
  arg-info-precedence
  arg-info-metatypes
  arg-info-number-optional
  arg-info-key/rest-p
  arg-info-keywords ;nil         no keyword or rest allowed
	            ;(k1 k2 ..)  each method must accept these keyword arguments
	            ;T           must have &key or &rest

  gf-info-simple-accessor-type ; nil, reader, writer, boundp
  (gf-precompute-dfun-and-emf-p nil) ; set by set-arg-info

  gf-info-static-c-a-m-emf
  gf-info-c-a-m-emf-std-p
  (arg-info-lambda-list :no-lambda-list))

(defun arg-info-valid-p (arg-info)
  (not (null (arg-info-number-optional arg-info))))

(defun arg-info-applyp (arg-info)
  (or (plusp (arg-info-number-optional arg-info))
      (arg-info-key/rest-p arg-info)))

(defun arg-info-number-required (arg-info)
  (length (arg-info-metatypes arg-info)))

(defun arg-info-nkeys (arg-info)
  (count-if #'(lambda (x) (neq x 't)) (arg-info-metatypes arg-info)))

(defun set-arg-info (gf precedence metatypes number-optional key/rest-p keywords
			&optional (lambda-list nil lambda-list-p))
  (let ((arg-info (gf-arg-info gf)))
    (setf (arg-info-precedence arg-info) precedence)
    (setf (arg-info-metatypes arg-info) metatypes)
    (setf (arg-info-number-optional arg-info) number-optional)
    (setf (arg-info-key/rest-p arg-info) key/rest-p)
    (setf (arg-info-keywords arg-info) keywords)
    (when lambda-list-p
      (setf (arg-info-lambda-list arg-info) lambda-list))
    (setf (gf-precompute-dfun-and-emf-p arg-info)
	  (let* ((name (generic-function-name gf))
		 (sym (if (atom name) name (cadr name)))
		 (pkg-list (cons *the-pcl-package* 
				 (package-use-list *the-pcl-package*))))
	    (not (null (memq (symbol-package sym) pkg-list)))))
    arg-info))

(defun new-arg-info-from-generic-function (gf lambda-list argument-precedence-order)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list lambda-list)
    (declare (ignore allow-other-keys-p))
    (let ((metatypes (make-list nreq))
	  (precedence (compute-precedence lambda-list nreq argument-precedence-order)))
      (set-arg-info gf
		    precedence
		    metatypes
		    nopt
		    (or keysp restp)
		    keywords
		    lambda-list))))

(defun new-arg-info-from-method (gf method)
  (multiple-value-bind (nreq nopt keysp restp)
      (analyze-lambda-list (method-lambda-list method))
    (set-arg-info gf
		  (compute-precedence (method-lambda-list method) nreq ())
		  (mapcar #'raise-metatype 
			  (make-list nreq) (method-specializers method))
		  nopt
		  (or keysp restp)
		  ())))

(defun add-arg-info (generic-function method)
  (let ((arg-info (gf-arg-info generic-function)))
    (if (not (arg-info-valid-p arg-info))
	(new-arg-info-from-method generic-function method)
	(flet ((lose (string &rest args)
		 (error "Attempt to add the method ~S to the generic function ~S.~%~
                   But ~A"
			method
			generic-function
			(apply #'format nil string args)))
	       (compare (x y)
		 (if (> x y) "more" "fewer")))
	  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
	      (analyze-lambda-list (method-lambda-list method))
	    (let ((gf-nreq (arg-info-number-required arg-info))
		  (gf-nopt (arg-info-number-optional arg-info))
		  (gf-key/rest-p (arg-info-key/rest-p arg-info))
		  (gf-keywords (arg-info-keywords arg-info)))

	      (unless (= nreq gf-nreq)
		(lose "the method has ~A required arguments than the generic function."
		      (compare nreq gf-nreq)))
	      (unless (= nopt gf-nopt)
		(lose "the method has ~S optional arguments than the generic function."
		      (compare nopt gf-nopt)))
	      (unless (eq (or keysp restp) gf-key/rest-p)
		(error "the method and generic function differ in whether they accept~%~
                  rest or keyword arguments."))
	      (when gf-keywords
		(unless (or (and restp (not keysp))
			    allow-other-keys-p
			    (every #'(lambda (k) (memq k keywords)) gf-keywords))
		  (error
		   "the generic function requires each method to accept the keyword arguments~%~
               ~S.  The method does not accept all of these."
		   gf-keywords)))
	      (set-arg-info generic-function
			    (arg-info-precedence arg-info)
			    (mapcar #'raise-metatype (arg-info-metatypes arg-info)
				    (method-specializers method))
			    gf-nopt
			    gf-key/rest-p
			    gf-keywords)))))))

(defun remove-arg-info (generic-function method)
  (let ((arg-info (gf-arg-info generic-function)))
    (set-arg-info generic-function
		  (arg-info-precedence arg-info)
		  (let* ((nreq (arg-info-number-required arg-info))
			 (metatypes (make-list nreq))
			 (old-methods (generic-function-methods generic-function))
			 (methods (remove method old-methods)))
		    (dolist (specls (mapcar #'method-specializers methods))
		      (setq metatypes (mapcar #'raise-metatype metatypes specls)))
		    metatypes)
		  (arg-info-number-optional arg-info)
		  (arg-info-key/rest-p arg-info)
		  (arg-info-keywords arg-info))))

(defmethod generic-function-lambda-list ((gf generic-function))
  (let ((arg-info (gf-arg-info gf)))
    (if (or (not (arg-info-valid-p arg-info))
	    (and (eq ':no-lambda-list (arg-info-lambda-list arg-info))
		 (null (generic-function-methods gf))))
	(progn
	  (warn "No way to determine the lambda list for ~S." gf)
	  nil)
	(if (eq ':no-lambda-list (arg-info-lambda-list arg-info))
	    (let* ((ll (method-lambda-list (car (last (generic-function-methods gf)))))
		   (k (member '&key ll)))
	      (if k 
		  (append (ldiff ll (cdr k)) '(&allow-other-keys))
		  ll))
	    (arg-info-lambda-list arg-info)))))

(defmethod initialize-instance :after ((gf standard-generic-function)
				       &key (lambda-list nil lambda-list-p)
				       argument-precedence-order)
  (when lambda-list-p
    (new-arg-info-from-generic-function gf lambda-list argument-precedence-order))
  (when (arg-info-valid-p (gf-arg-info gf))
    (update-dfun gf)))

(defmethod reinitialize-instance :after ((gf standard-generic-function)
					 &key (lambda-list nil lambda-list-p)
					 argument-precedence-order)
  (let* ((arg-info (gf-arg-info gf))
	 (valid-p (arg-info-valid-p arg-info)))
    (when lambda-list-p
      (if (not valid-p)
	  (new-arg-info-from-generic-function gf lambda-list argument-precedence-order)
	  (setf (arg-info-lambda-list arg-info) lambda-list)))
    (when valid-p
      (update-dfun gf))))

;;;
;;;
;;;
(defun compute-precedence (lambda-list nreq argument-precedence-order)
  (let ((nreq (analyze-lambda-list lambda-list)))
    (if (null argument-precedence-order)
	(let ((list nil))(dotimes (i nreq list) (push (- (1- nreq) i) list)))
	(mapcar #'(lambda (x) (position x lambda-list)) argument-precedence-order))))


(defmethod no-applicable-method (generic-function &rest args)
  (cerror "Retry call to ~S"
	  "No matching method for the generic-function ~S,~@
          when called with arguments ~S."
	  generic-function args)
  (apply generic-function args))

(proclaim '(special *lazy-dfun-compute-p*))

(defun real-add-method (generic-function method)
  (if (method-generic-function method)
      (error "The method ~S is already part of the generic~@
              function ~S.  It can't be added to another generic~@
              function until it is removed from the first one."
	     method (method-generic-function method))

      (let* ((name (generic-function-name generic-function))
	     (qualifiers   (method-qualifiers method))
	     (lambda-list  (method-lambda-list method))
	     (specializers (method-specializers method))
	     (existing (get-method generic-function qualifiers specializers nil)))
	;;
	;; If there is already a method like this one then we must
	;; get rid of it before proceeding.  Note that we call the
	;; generic function remove-method to remove it rather than
	;; doing it in some internal way.
	;; 
	(when existing (remove-method generic-function existing))
	;;
	(setf (method-generic-function method) generic-function)
	(setf (method-function-name method)
	      (intern-function-name
	       (make-method-spec name
				 qualifiers
				 (unparse-specializers specializers))))
	(add-arg-info generic-function method)
	(pushnew method (generic-function-methods generic-function))
	(dolist (specializer specializers)
	  (add-direct-method specializer method))
	(update-gf-info generic-function)
	(update-dfun generic-function)
	(maybe-update-constructors generic-function method)
	method)))
  
(defun real-remove-method (generic-function method)
  (if  (neq generic-function (method-generic-function method))
       (error "The method ~S is attached to the generic function~@
               ~S.  It can't be removed from the generic function~@
               to which it is not attached."
	      method (method-generic-function method))
       (let* ((methods      (generic-function-methods generic-function))
	      (new-methods  (remove method methods)))	      
	 (setf (method-generic-function method) nil)
	 (setf (generic-function-methods generic-function) new-methods)
	 (dolist (specializer (method-specializers method))
	   (remove-direct-method specializer method))
	 (remove-arg-info generic-function method)
	 (update-dfun generic-function)
	 (maybe-update-constructors generic-function method)
	 generic-function)))


(defun compute-applicable-methods-function (generic-function arguments)
  (values (compute-applicable-methods-using-types 
	   generic-function
	   (types-from-arguments generic-function arguments 'eql))))
  
(defmethod compute-applicable-methods 
    ((generic-function generic-function) arguments)
  (values (compute-applicable-methods-using-types 
	   generic-function
	   (types-from-arguments generic-function arguments 'eql))))

(defmethod compute-applicable-methods-using-classes 
    ((generic-function generic-function) classes)
  (compute-applicable-methods-using-types 
   generic-function
   (types-from-arguments generic-function classes 'class-eq)))

(defun compute-applicable-methods-using-types (generic-function types)
  (let ((definite-p t) (possibly-applicable-methods nil))
    (dolist (method (generic-function-methods generic-function))
      (let ((specls (method-specializers method))
	    (types types)
	    (possibly-applicable-p t) (applicable-p t))
	(dolist (specl specls)
	  (multiple-value-bind (specl-applicable-p specl-possibly-applicable-p)
	      (specializer-applicable-using-type-p specl (pop types))
	    (unless specl-applicable-p
	      (setq applicable-p nil))
	    (unless specl-possibly-applicable-p
	      (setq possibly-applicable-p nil)
	      (return nil))))
	(when possibly-applicable-p
	  (unless applicable-p (setq definite-p nil))
	  (push method possibly-applicable-methods))))
    (values (sort-applicable-methods generic-function
				     (nreverse possibly-applicable-methods)
				     types)
	    definite-p)))

;This is used to speed up precompute-effective-methods.
(defvar *in-precompute-effective-methods-p* nil)

;used only in specializer-applicable-using-type-p
(defun class-applicable-using-class-p (specl type)
  (let ((pred (memq specl (class-precedence-list type))))
    (values pred
	    (or pred
		(if (not *in-precompute-effective-methods-p*)
		    ;; classes might get common subclass
		    (superclasses-compatible-p specl type)
		    ;; worry only about existing classes
		    (classes-have-common-subclass-p specl type))))))

;used only in map-all-orders
(defun class-might-precede-p (class1 class2)
  (if (not *in-precompute-effective-methods-p*)
      (not (member class1 (cdr (class-precedence-list class2))))
      (class-can-precede-p class1 class2)))

(defun saut-and (specl type)
  (let ((applicable nil)
	(possibly-applicable t))
    (dolist (type (cdr type))
      (multiple-value-bind (appl poss-appl)
	  (specializer-applicable-using-type-p specl type)
	(when appl (return (setq applicable t)))
	(unless poss-appl (return (setq possibly-applicable nil)))))
    (values applicable possibly-applicable)))

(defun saut-not (specl type)
  (let ((ntype (cadr type)))
    (values nil
	    (case (car ntype)
	      (class      (saut-not-class specl ntype))
	      (class-eq   (saut-not-class-eq specl ntype))
	      (eql        (saut-not-eql specl ntype))
	      (t (error "~s cannot handle the second argument ~s"
			'specializer-applicable-using-type-p type))))))

(defun saut-not-class (specl ntype)
  (let* ((class (type-class specl))
	  (cpl (class-precedence-list class)))
     (not (memq (cadr ntype) cpl))))

(defun saut-not-class-eq (specl ntype)
  (let ((class (case (car specl)
		 (eql      (class-of (cadr specl)))
		 (class-eq (cadr specl)))))
    (not (eq class (cadr ntype)))))

(defun saut-not-eql (specl ntype)
  (case (car specl)
    (eql (not (eql (cadr specl) (cadr ntype))))
    (t   t)))

(defun saut-class (specl type)
  (case (car specl)
    (class (class-applicable-using-class-p (cadr specl) (cadr type)))
    (t     (values nil (let ((class (type-class specl)))
			 (memq (cadr type)
			       (class-precedence-list class)))))))

(defun saut-class-eq (specl type)
  (if (eq (car specl) 'eql)
      (values nil (eq (class-of (cadr specl)) (cadr type)))
      (let ((pred (case (car specl)
		    (class-eq   (eq (cadr specl) (cadr type)))
		    (class      (or (eq (cadr specl) (cadr type))
				    (memq (cadr specl) 
					  (class-precedence-list (cadr type))))))))
	(values pred pred))))

(defun saut-eql (specl type) 
  (let ((pred (case (car specl)
		(eql        (eql (cadr specl) (cadr type)))
		(class-eq   (eq (cadr specl) (class-of (cadr type))))
		(class      (memq (cadr specl)
				  (class-precedence-list 
				   (class-of (cadr type))))))))
    (values pred pred)))

(defun specializer-applicable-using-type-p (specl type)
  (setq specl (type-from-specializer specl))
  (when (eq specl 't)
    (return-from specializer-applicable-using-type-p (values t t)))
  ;; This is used by c-a-m-u-t and generate-discrimination-net-internal,
  ;; and has only what they need.
  (if (or (atom type) (eq (car type) 't))
      (values nil t)
      (case (car type)
	(and        (saut-and specl type))
	(not        (saut-not specl type))
	(class      (saut-class specl type))
	(class-eq   (saut-class-eq specl type))
	(eql        (saut-eql specl type))
	(t          (error "~s cannot handle the second argument ~s"
			   'specializer-applicable-using-type-p type)))))

(defun proclaim-incompatible-superclasses (classes)
  (setq classes (mapcar #'(lambda (class)
			    (if (symbolp class)
				(find-class class)
				class))
			classes))
  (dolist (class classes)
    (dolist (other-class classes)
      (unless (eq class other-class)
	(pushnew other-class (class-incompatible-superclass-list class))))))

(defun superclasses-compatible-p (class1 class2)
  (let ((cpl1 (class-precedence-list class1))
	(cpl2 (class-precedence-list class2)))
    (dolist (sc1 cpl1 t)
      (dolist (ic (class-incompatible-superclass-list sc1))
	(when (memq ic cpl2)
	  (return-from superclasses-compatible-p nil))))))

(mapc
 #'proclaim-incompatible-superclasses
 '(;; superclass class
   (built-in-class std-class structure-class) ; direct subclasses of pcl-class
   (standard-class funcallable-standard-class)
   ;; superclass metaobject
   (class eql-specializer class-eq-specializer method ; method-combination
    generic-function slot-definition)
   ;; metaclass built-in-class
   (number sequence character symbol	; direct subclasses of t, but not array
    standard-object structure-object)
   (number array character symbol	; direct subclasses of t, but not sequence
    standard-object structure-object)
   (complex float rational)		; direct subclasses of number
   (integer ratio)			; direct subclasses of rational
   (list vector)			; direct subclasses of sequence
   (cons null)				; direct subclasses of list
   (string bit-vector)			; direct subclasses of vector
   ))

(defun classes-have-common-subclass-p (class1 class2)
  (or (eq class1 class2)
      (let ((class1-subs (class-direct-subclasses class1)))
	(or (memq class2 class1-subs)
	    (dolist (class1-sub class1-subs nil)
	      (when (classes-have-common-subclass-p class1-sub class2)
		(return t)))))))

(defun order-specializers (specl1 specl2 index compare-classes-function)
  (let ((type1 (specializer-type specl1))
	(type2 (specializer-type specl2)))
    (cond ((eq specl1 specl2)
	   nil)
	  ((atom type1)
	   specl2)
	  ((atom type2)
	   specl1)
	  (t
	   (case (car type1)
	     (class    (case (car type2)
			 (class (funcall compare-classes-function specl1 specl2 index))
			 (t specl2)))
	     (class-eq (case (car type2)
			 (eql specl2)
			 (class-eq nil)
			 (class type1)))
	     (eql      (case (car type2)
			 (eql nil)
			 (t specl1))))))))

(defun sort-applicable-methods (generic-function methods types)
  (sort-methods methods
		(arg-info-precedence (gf-arg-info generic-function))
		#'(lambda (class1 class2 index)
		    (let* ((class (type-class (nth index types)))
			   (cpl (class-precedence-list class)))
		      (if (memq class2 (memq class1 cpl))
			  class1 class2)))))

(defun sort-methods (methods precedence compare-classes-function)
  (flet ((sorter (method1 method2)
	   (dolist (index precedence)
	     (let* ((specl1 (nth index (method-specializers method1)))
		    (specl2 (nth index (method-specializers method2)))
		    (order (order-specializers 
			     specl1 specl2 index compare-classes-function)))
	       (when order
		 (return-from sorter (eq order specl1)))))))
    (stable-sort methods #'sorter)))

(defun map-all-orders (methods precedence function)
  (let ((choices nil))
    (flet ((compare-classes-function (class1 class2 index)
	     (declare (ignore index))
	     (let ((choice nil))
	       (dolist (c choices nil)
		 (when (or (and (eq (first c) class1)
				(eq (second c) class2))
			   (and (eq (first c) class2)
				(eq (second c) class1)))
		   (return (setq choice c))))
	       (unless choice
		 (setq choice
		       (if (class-might-precede-p class1 class2)
			   (if (class-might-precede-p class2 class1)
			       (list class1 class2 nil t)
			       (list class1 class2 t))
			   (if (class-might-precede-p class2 class1)
			       (list class2 class1 t)
			       (let ((name1 (class-name class1))
				     (name2 (class-name class2)))
				 (if (and name1 name2 (symbolp name1) (symbolp name2)
					  (string< (symbol-name name1)
						   (symbol-name name2)))
				     (list class1 class2 t)
				     (list class2 class1 t))))))
		 (push choice choices))
	       (car choice))))
      (loop (funcall function
		     (sort-methods methods precedence #'compare-classes-function))
	    (unless (dolist (c choices nil)
		      (unless (third c)
			(rotatef (car c) (cadr c))
			(return (setf (third c) t))))
	      (return nil))))))



(defmethod same-specializer-p ((specl1 specializer) (specl2 specializer))
  nil)

(defmethod same-specializer-p ((specl1 class) (specl2 class))
  (eq specl1 specl2))

(defmethod specializer-class ((specializer class))
  specializer)

(defmethod same-specializer-p ((specl1 class-eq-specializer)
			       (specl2 class-eq-specializer))
  (eq (specializer-class specl1) (specializer-class specl2)))

(defmethod same-specializer-p ((specl1 eql-specializer)
			       (specl2 eql-specializer))
  (eq (specializer-object specl1) (specializer-object specl2)))

(defmethod specializer-class ((specializer eql-specializer))
  (class-of (slot-value specializer 'object)))

(defvar *in-gf-arg-info-p* nil)
(defvar arg-info-reader (make-internal-reader-method-function 'arg-info))

(defun types-from-arguments (generic-function arguments &optional type-modifier)
  (let* ((arg-info (if *in-gf-arg-info-p*
		       (function-funcall arg-info-reader generic-function)
		       (let ((*in-gf-arg-info-p* t))
			 (gf-arg-info generic-function))))
	 (metatypes (arg-info-metatypes arg-info))
	 (types-rev nil))
    (dolist (mt metatypes)
      (declare (ignore mt))
      (unless arguments
	(error "The function ~S requires at least ~D arguments"
	       (generic-function-name generic-function)
	       (length metatypes)))
      (let ((arg (pop arguments)))
	(push (if type-modifier `(,type-modifier ,arg) arg) types-rev)))
    (values (nreverse types-rev) arg-info)))

(defun get-wrappers-from-classes (nkeys wrappers classes metatypes)
  (let* ((w wrappers) (w-tail w) (mt-tail metatypes))
    (dolist (class (if (listp classes) classes (list classes)))
      (unless (eq 't (car mt-tail))
	(let ((c-w (class-wrapper class)))
	  (unless c-w (return-from get-wrappers-from-classes nil))
	  (if (eql nkeys 1)
	      (setq w c-w)
	      (setf (car w-tail) c-w
		    w-tail (cdr w-tail)))))
      (setq mt-tail (cdr mt-tail)))
    w))

;;;
;;; Given a generic function and a set of arguments to that generic function,
;;; returns a mess of values.
;;;
;;;  <function>   The compiled effective method function for this set of
;;;               arguments. 
;;;
;;;  <applicable> Sorted list of applicable methods. 
;;;
;;;  <wrappers>   Is a single wrapper if the generic function has only
;;;               one key, that is arg-info-nkeys of the arg-info is 1.
;;;               Otherwise a list of the wrappers of the specialized
;;;               arguments to the generic function.
;;;
;;;               Note that all these wrappers are valid.  This function
;;;               does invalid wrapper traps when it finds an invalid
;;;               wrapper and then returns the new, valid wrapper.
;;;
;;;  <invalidp>   True if any of the specialized arguments had an invalid
;;;               wrapper, false otherwise.
;;;
;;;  <type>       READER or WRITER when the only method that would be run
;;;               is a standard reader or writer method.  To be specific,
;;;               the value is READER when the method combination is eq to
;;;               *standard-method-combination*; there are no applicable
;;;               :before, :after or :around methods; and the most specific
;;;               primary method is a standard reader method.
;;;
;;;  <index>      If <type> is READER or WRITER, and the slot accessed is
;;;               an :instance slot, this is the index number of that slot
;;;               in the object argument.
;;;

(defun cache-miss-values (gf args state)
  (let* ((arg-info (function-funcall arg-info-reader gf))
	 (metatypes (arg-info-metatypes arg-info))
	 (for-accessor-p (eq state 'accessor))
	 (for-cache-p (or (eq state 'caching) (eq state 'accessor)))
	 (cam-std-p (gf-info-c-a-m-emf-std-p arg-info))
	 (args-tail args) (invalid-wrapper-p nil)
	 (wrappers-rev nil) (dfun-wrappers-rev nil)
	 (types-rev nil) (classes-rev nil))
    (dolist (mt metatypes)
      (unless args-tail
	(error "The function ~S requires at least ~D arguments"
	       (generic-function-name gf) (length metatypes)))
      (let ((arg (pop args-tail)))
	(multiple-value-bind (wrapper class type)
	    (if (eq mt 't)
		(values nil *the-class-t* 't)
		(let ((wrapper (wrapper-of arg)))
		  (when (invalid-wrapper-p wrapper)
		    (setq invalid-wrapper-p t)
		    (setq wrapper (check-wrapper-validity arg)))
		  (push wrapper dfun-wrappers-rev)
		  (let ((wclass (wrapper-class wrapper)))
		    (values wrapper wclass `(class-eq ,wclass)))))
	  (push wrapper wrappers-rev) (push class classes-rev) (push type types-rev))))
    (let* ((wrappers (nreverse wrappers-rev))
	   (classes (nreverse classes-rev))
	   (types (mapcar #'(lambda (class) `(class-eq ,class)) classes)))
      (multiple-value-bind (methods all-applicable-and-sorted-p)
	  (if cam-std-p
	      (compute-applicable-methods-using-types gf types)
	      (compute-applicable-methods-using-classes gf classes))
	(let ((sdfun (if (or all-applicable-and-sorted-p cam-std-p)
			 (function-funcall (get-secondary-dispatch-function1 
					    gf methods types
					    all-applicable-and-sorted-p)
					   nil (and for-cache-p wrappers))
			 (default-secondary-dispatch-function gf))))
	  (multiple-value-bind (accessor-type index)
	      (and for-accessor-p all-applicable-and-sorted-p methods
		   (accessor-values arg-info classes methods))
	    (values (if (and dfun-wrappers-rev (null (cdr dfun-wrappers-rev)))
			(car dfun-wrappers-rev)
			(nreverse dfun-wrappers-rev))
		    invalid-wrapper-p
		    sdfun methods accessor-type index)))))))

(defun accessor-values (arg-info classes methods)
  (let* ((accessor-type (gf-info-simple-accessor-type arg-info))
	 (accessor-class (case accessor-type
			   (reader (car classes))
			   (writer (cadr classes))
			   (boundp (car classes))))
	 (slot-name (and accessor-class
			 (member *the-class-standard-object*
				 (class-precedence-list accessor-class))
			 (accessor-method-slot-name (car methods))))
	 (slotd (and accessor-class
		     (find-slot-definition accessor-class slot-name))))
    (when (and slotd 
	       (slot-accessor-std-p slotd accessor-type))
      (values accessor-type (slot-definition-location slotd)))))

(defun sdfun-for-caching (gf classes)
  (let ((types (mapcar #'class-eq-type classes)))
    (multiple-value-bind (methods all-applicable-and-sorted-p)
	(compute-applicable-methods-using-types gf types)
      (function-funcall (get-secondary-dispatch-function1 
			 gf methods types all-applicable-and-sorted-p)
			nil (mapcar #'class-wrapper classes)))))

(defun value-for-caching (gf classes)
  (let ((methods (compute-applicable-methods-using-types 
		   gf (mapcar #'class-eq-type classes))))
    (getf (method-function-plist (method-function (car methods)))
	  :constant-value)))

(defun default-secondary-dispatch-function (generic-function)
  #'(lambda (&rest args)
      (let ((methods (compute-applicable-methods generic-function args)))
	(if methods
	    (function-apply (get-effective-method-function generic-function methods)
			    args)
	    (apply #'no-applicable-method generic-function args)))))

(defun list-eq (x y)
  (loop (when (atom x) (return (eq x y)))
	(when (atom y) (return nil))
	(unless (eq (car x) (car y)) (return nil))
	(setq x (cdr x)  y (cdr y))))

(defvar *std-cam-methods* nil)

(defun compute-applicable-methods-emf (generic-function)  
  (if (eq *boot-state* 'complete)
      (let* ((cam (gdefinition 'compute-applicable-methods))
	     (cam-methods (compute-applicable-methods-using-types
			   cam (list `(eql ,generic-function) t))))
	(values (get-effective-method-function cam cam-methods)
		(list-eq cam-methods 
			 (or *std-cam-methods*
			     (setq *std-cam-methods*
				   (compute-applicable-methods-using-types
				    cam (list `(eql ,cam) t)))))))
      (values #'compute-applicable-methods-function t)))

(defun compute-applicable-methods-emf-std-p (gf)
  (gf-info-c-a-m-emf-std-p (gf-arg-info gf)))

(defvar *old-c-a-m-gf-methods* nil)

(defun update-all-c-a-m-gf-info (c-a-m-gf)
  (let ((methods (generic-function-methods c-a-m-gf)))
    (if (every #'(lambda (old-method)
		   (member old-method methods))
	       *old-c-a-m-gf-methods*)
	(let ((gfs-to-do nil)
	      (gf-classes-to-do nil))
	  (dolist (method methods)
	    (unless (member method *old-c-a-m-gf-methods*)
	      (let ((specl (car (method-specializers method))))
		(if (eql-specializer-p specl)
		    (pushnew (specializer-object specl) gfs-to-do)
		    (pushnew (specializer-class specl) gf-classes-to-do)))))
	  (map-all-generic-functions 
	   #'(lambda (gf)
	       (when (or (member gf gfs-to-do)
			 (dolist (class gf-classes-to-do nil)
			   (member class (class-precedence-list (class-of gf)))))
		 (update-c-a-m-gf-info gf)))))
	(map-all-generic-functions #'update-c-a-m-gf-info))
    (setq *old-c-a-m-gf-methods* methods)))

(defun update-gf-info (gf)
  (update-c-a-m-gf-info gf)
  (update-gf-simple-accessor-type gf))

(defun update-c-a-m-gf-info (gf)
  (multiple-value-bind (c-a-m-emf std-p)
      (compute-applicable-methods-emf gf)
    (let ((arg-info (gf-arg-info gf)))
      (setf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
      (setf (gf-info-c-a-m-emf-std-p arg-info) std-p))))

(defun update-gf-simple-accessor-type (gf)
  (let ((arg-info (gf-arg-info gf)))
    (setf (gf-info-simple-accessor-type arg-info)
	  (let* ((methods (generic-function-methods gf))
		 (class (and methods (class-of (car methods))))
		 (type (and class (cond ((eq class *the-class-standard-reader-method*)
					 'reader)
					((eq class *the-class-standard-writer-method*)
					 'writer)
					((eq class *the-class-standard-boundp-method*)
					 'boundp)))))
	    (when (and (gf-info-c-a-m-emf-std-p arg-info)
		       type
		       (dolist (method (cdr methods) t)
			 (unless (eq class (class-of method)) (return nil)))
		       (eq (generic-function-method-combination gf)
			   *standard-method-combination*))
	      type)))))

(defun get-accessor-method-function (gf type class slotd)  
  (let* ((std-method (standard-svuc-method type))
	 (str-method (structure-svuc-method type))
	 (types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
	 (types (if (eq type 'writer) `(t ,@types1) types1))
	 (methods (compute-applicable-methods-using-types gf types))
	 (std-p (null (cdr methods))))
    (values
     (if std-p
	 (get-optimized-std-accessor-method-function class slotd type)
	 (get-accessor-from-svuc-method-function
	  class slotd
	  (get-secondary-dispatch-function 
	   gf methods types
	   `((,(car (or (member std-method methods)
			(member str-method methods)
			(error "error in get-accessor-method-function")))
	      ,(get-optimized-std-slot-value-using-class-method-function
		class slotd type)))
	   (unless (and (eq type 'writer)
			(dolist (method methods t)
			  (unless (eq (car (method-specializers method))
				      *the-class-t*)
			    (return nil))))
	     (let ((wrappers (list (wrapper-of class)
				   (class-wrapper class)
				   (wrapper-of slotd))))
	       (if (eq type 'writer)
		   (cons (class-wrapper *the-class-t*) wrappers)
		   wrappers))))
	  type))
     std-p)))

(defvar *new-class* nil)

;used by optimize-slot-value-by-class-p (vector.lisp)
(defun update-slot-value-gf-info (gf type)
  (unless *new-class*
    (update-std-or-str-methods gf type))
  (when (and (standard-svuc-method type) (structure-svuc-method type))
    (flet ((update-class (class)
	     (when (class-finalized-p class)
	       (dolist (slotd (class-slots class))
		 (multiple-value-bind (function std-p)
		     (get-accessor-method-function gf type class slotd)
		   #+kcl (si:turbo-closure function)
		   (setf (slot-accessor-std-p slotd type) std-p)
		   (setf (slot-accessor-function slotd type) function))))))
      (if *new-class*
	  (update-class *new-class*)
	  (map-all-classes #'update-class 'slot-object)))))

(defvar *standard-slot-value-using-class-method* nil)
(defvar *standard-setf-slot-value-using-class-method* nil)
(defvar *standard-slot-boundp-using-class-method* nil)
(defvar *structure-slot-value-using-class-method* nil)
(defvar *structure-setf-slot-value-using-class-method* nil)
(defvar *structure-slot-boundp-using-class-method* nil)

(defun standard-svuc-method (type)
  (case type
    (reader *standard-slot-value-using-class-method*)
    (writer *standard-setf-slot-value-using-class-method*)
    (boundp *standard-slot-boundp-using-class-method*)))

(defun set-standard-svuc-method (type method)
  (case type
    (reader (setq *standard-slot-value-using-class-method* method))
    (writer (setq *standard-setf-slot-value-using-class-method* method))
    (boundp (setq *standard-slot-boundp-using-class-method* method))))

(defun structure-svuc-method (type)
  (case type
    (reader *structure-slot-value-using-class-method*)
    (writer *structure-setf-slot-value-using-class-method*)
    (boundp *structure-slot-boundp-using-class-method*)))

(defun set-structure-svuc-method (type method)
  (case type
    (reader (setq *structure-slot-value-using-class-method* method))
    (writer (setq *structure-setf-slot-value-using-class-method* method))
    (boundp (setq *structure-slot-boundp-using-class-method* method))))

(defun update-std-or-str-methods (gf type)
  (dolist (method (generic-function-methods gf))
    (let ((specls (method-specializers method)))
      (when (and (or (not (eq type 'writer))
		     (eq (pop specls) *the-class-t*))
		 (every #'classp specls))
	(cond ((and (eq (class-name (car specls))
			'std-class)
		    (eq (class-name (cadr specls)) 
			'standard-object)
		    (eq (class-name (caddr specls)) 
			'standard-effective-slot-definition))
	       (set-standard-svuc-method type method))
	      ((and (eq (class-name (car specls))
			'structure-class)
		    (eq (class-name (cadr specls))
			'structure-object)
		    (eq (class-name (caddr specls)) 
			'structure-effective-slot-definition))
	       (set-structure-svuc-method type method)))))))

(defvar *free-hash-tables* (mapcar #'list '(eq equal eql)))

(defmacro with-hash-table ((table test) &body forms)
  `(let* ((.free. (assoc ',test *free-hash-tables*))
	  (,table (if (cdr .free.)
		      (pop (cdr .free.))
		      (make-hash-table :test ',test))))
     (multiple-value-prog1
	 (progn ,@forms)
       (clrhash ,table)
       (push ,table (cdr .free.)))))

(defmacro with-eq-hash-table ((table) &body forms)
  `(with-hash-table (,table eq) ,@forms))

(defmacro with-equal-hash-table ((table) &body forms)
  `(with-hash-table (,table equal) ,@forms))

(defun make-accessor-table (generic-function type &optional table)
  (unless table (setq table (make-hash-table :test 'eq)))
  (let ((methods (generic-function-methods generic-function))
	(all-index nil)
	(no-class-slots-p t)
	first second (size 0))
    (declare (fixnum size))
    ;; class -> {(specl slotd)}
    (dolist (method methods)
	(let* ((specializers (method-specializers method))
	       (specl (if (eq type 'reader)
			  (car specializers)
			  (cadr specializers)))
	       (specl-cpl (class-precedence-list specl))
	       (so-p (member *the-class-standard-object* specl-cpl))
	       (slot-name (accessor-method-slot-name method)))
	  (when (member *the-class-structure-object* specl-cpl)
	    (return-from make-accessor-table nil))
	  (maphash #'(lambda (class slotd)
		       (let ((cpl (class-precedence-list class)))
			 (when (memq specl cpl)
			   (unless (and (or so-p
					    (member *the-class-standard-object* cpl))
					(slot-accessor-std-p slotd type))
			     (return-from make-accessor-table nil))
			   (push (cons specl slotd) (gethash class table)))))
		   (gethash slot-name *name->class->slotd-table*))))
    (maphash #'(lambda (class specl+slotd-list)
		 (dolist (sclass (class-precedence-list class) 
			  (error "This can't happen"))
		   (let ((a (assq sclass specl+slotd-list)))
		     (when a
		       (let* ((slotd (cdr a))
			      (index (slot-definition-location slotd)))
			 (unless index (return-from make-accessor-table nil))
			 (setf (gethash class table) index)
			 (when (consp index) (setq no-class-slots-p nil))
			 (setq all-index (if (or (null all-index)
						 (eql all-index index))
					     index t))
			 (incf size)
			 (cond ((= size 1) (setq first class))
			       ((= size 2) (setq second class)))
			 (return nil))))))
	     table)
    (values table all-index first second size no-class-slots-p)))

(defun mec-all-classes-internal (spec precompute-p)
  (cons (specializer-class spec)
	(and (classp spec)
	     precompute-p
	     (not (or (eq spec *the-class-t*)
		      (eq spec *the-class-slot-object*)
		      (eq spec *the-class-standard-object*)
		      (eq spec *the-class-structure-object*)))
	     (let ((sc (class-direct-subclasses spec)))
	       (when sc
		 (mapcan #'(lambda (class)
			     (mec-all-classes-internal class precompute-p))
			 sc))))))

(defun mec-all-classes (spec precompute-p)
  (let ((classes (mec-all-classes-internal spec precompute-p)))
    (if (null (cdr classes))
	classes
	(let* ((a-classes (cons nil classes))
	       (tail classes))
	  (loop (when (null (cdr tail))
		  (return (cdr a-classes)))
		(let ((class (cadr tail))
		      (ttail (cddr tail)))
		  (if (dolist (c ttail nil)
			(when (eq class c) (return t)))
		      (setf (cdr tail) (cddr tail))
		      (setf tail (cdr tail)))))))))

(defun mec-all-class-lists (spec-list precompute-p)
  (if (null spec-list)
      (list nil)
      (let* ((car-all-classes (mec-all-classes (car spec-list) precompute-p))
	     (all-class-lists (mec-all-class-lists (cdr spec-list) precompute-p)))
	(mapcan #'(lambda (list)
		    (mapcar #'(lambda (c) (cons c list)) car-all-classes))
		all-class-lists))))

(defun make-emf-cache (generic-function valuep cache classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
	 (nkeys (arg-info-nkeys arg-info))
	 (metatypes (arg-info-metatypes arg-info))
	 (wrappers (unless (eq nkeys 1) (make-list nkeys)))
	 (precompute-p (gf-precompute-dfun-and-emf-p arg-info))
	 (default '(default)))
    (flet ((add-class-list (classes)
	     (when (or (null new-class) (memq new-class classes))
	       (let ((wrappers (get-wrappers-from-classes 
				nkeys wrappers classes metatypes)))
		 (when (and wrappers
			    (eq default (probe-cache cache wrappers default)))
		   (let ((value (cond ((eq valuep t)
				       (sdfun-for-caching generic-function classes))
				      ((eq valuep :constant-value)
				       (value-for-caching generic-function classes)))))
		     (setq cache (fill-cache cache wrappers value t))))))))
      (if classes-list
	  (mapc #'add-class-list classes-list)
	  (dolist (method (generic-function-methods generic-function))
	    (mapc #'add-class-list
		  (mec-all-class-lists (method-specializers method) precompute-p))))
      cache)))

(defun methods-contain-eql-specializer-p (methods)
  (dolist (method methods nil)
    (when (dolist (spec (method-specializers method) nil)
	    (when (eql-specializer-p spec) (return t)))
      (return t))))

(defmacro class-test (arg class)
  (cond ((eq class *the-class-t*)
	 't)
	((eq class *the-class-slot-object*)
	 `(not (eq *the-class-built-in-class* 
		(wrapper-class (std-instance-wrapper (class-of ,arg))))))
	((eq class *the-class-standard-object*)
	 `(or (std-instance-p ,arg) (fsc-instance-p ,arg)))
	;; TYPEP is now faster than doing memq of the cpl
	(t
	 `(typep ,arg ',(class-name class)))))

(defmacro class-eq-test (arg class)
  `(eq (class-of ,arg) ',class))

(defmacro eql-test (arg object)
  `(eql ,arg ',object))

(defun dnet-methods-p (form)
  (and (consp form)
       (or (eq (car form) 'methods)
	   (eq (car form) 'unordered-methods))))

(defmacro scase (arg &rest clauses) ; This is case, but without gensyms
  `(let ((.case-arg. ,arg))
     (cond ,@(mapcar #'(lambda (clause)
			 (list* (cond ((listp (car clause))
				       `(member .case-arg. ',(car clause)))
				      ((member (car clause) '(t otherwise))
				       `t)
				      (t
				       `(eql .case-arg. ',(car clause))))
				nil
				(cdr clause)))
		     clauses))))

(defmacro mcase (arg &rest clauses) `(scase ,arg ,@clauses))

(defun generate-discrimination-net (generic-function methods types sorted-p)
  (let* ((arg-info (gf-arg-info generic-function))
	 (precedence (arg-info-precedence arg-info)))
    (generate-discrimination-net-internal 
     generic-function methods types
     #'(lambda (methods known-types)
	 (if (or sorted-p
		 (block one-order-p
		   (let ((sorted-methods nil))
		     (map-all-orders 
		      (copy-list methods) precedence
		      #'(lambda (methods)
			  (when sorted-methods (return-from one-order-p nil))
			  (setq sorted-methods methods)))
		     (setq methods sorted-methods))
		   t))
	     `(methods ,methods ,known-types)
	     `(unordered-methods ,methods ,known-types)))
     #'(lambda (position type true-value false-value)
	 (let ((arg (dfun-arg-symbol position)))
	   (if (eq (car type) 'eql)
	       (let* ((false-case-p (and (consp false-value)
					 (or (eq (car false-value) 'scase)
					     (eq (car false-value) 'mcase))
					 (eq arg (cadr false-value))))
		      (false-clauses (if false-case-p
					 (cddr false-value)
					 `((t ,false-value))))
		      (case-sym (if (and (dnet-methods-p true-value)
					 (if false-case-p
					     (eq (car false-value) 'mcase)
					     (dnet-methods-p false-value)))
				    'mcase
				    'scase))
		      (type-sym (if (member (cadr type) '(t nil otherwise))
				    `(,(cadr type))
				    (cadr type))))
		 `(,case-sym ,arg
		    (,type-sym ,true-value)
		    ,@false-clauses))
	       `(if ,(let ((arg (dfun-arg-symbol position)))
		       (case (car type)
			 (class    `(class-test    ,arg ,(cadr type)))
			 (class-eq `(class-eq-test ,arg ,(cadr type)))))
		    ,true-value
		    ,false-value))))
     #'identity)))

(defun class-from-type (type)
  (if (or (atom type) (eq (car type) 't))
      *the-class-t*
      (case (car type)
	(and (dolist (type (cdr type) *the-class-t*)
	       (when (and (consp type) (not (eq (car type) 'not)))
		 (return (class-from-type type)))))
	(not *the-class-t*)
        (eql (class-of (cadr type)))
        (class-eq (cadr type))
        (class (cadr type)))))

(defun precompute-effective-methods (gf &optional classes-list-p)
  (let* ((arg-info (gf-arg-info gf))
	 (methods (generic-function-methods gf))
	 (precedence (arg-info-precedence arg-info))
	 (*in-precompute-effective-methods-p* t)
	 (classes-list nil))
    (generate-discrimination-net-internal 
     gf methods nil
     #'(lambda (methods known-types)
	 (when methods
	   (when classes-list-p
	     (push (mapcar #'class-from-type known-types) classes-list))
	   (map-all-orders 
	      methods precedence
	      #'(lambda (methods)
		  (get-secondary-dispatch-function gf methods known-types)))))
     #'(lambda (position type true-value false-value)
	 (declare (ignore position type true-value false-value))
	 nil)
     #'(lambda (type)
	 (if (and (consp type) (eq (car type) 'eql))
	     `(class-eq ,(class-of (cadr type)))
	     type)))
    classes-list))

; we know that known-type implies neither new-type nor `(not ,new-type) 
(defun augment-type (new-type known-type)
  (if (or (eq known-type 't)
	  (eq (car new-type) 'eql))
      new-type
      (let ((so-far (if (and (consp known-type) (eq (car known-type) 'and))
			(cdr known-type)
			(list known-type))))
	(unless (eq (car new-type) 'not)
	  (setq so-far
		(mapcan #'(lambda (type)
			    (unless (*subtypep type new-type)
			      (list type)))
			so-far)))
	(if (null so-far)
	    new-type
	    `(and ,new-type ,@so-far)))))

#+lcl3.0 (dont-use-production-compiler)

(defun generate-discrimination-net-internal 
    (gf methods types methods-function test-function type-function)
  (let* ((arg-info (gf-arg-info gf))
	 (precedence (arg-info-precedence arg-info))
	 (nreq (arg-info-number-required arg-info))
	 (metatypes (arg-info-metatypes arg-info)))
    (labels ((do-column (p-tail contenders known-types)
	       (if p-tail
		   (let* ((position (car p-tail))
			  (known-type (or (nth position types) t)))
		     (if (eq (nth position metatypes) 't)
			 (do-column (cdr p-tail) contenders
				    (cons (cons position known-type) known-types))
			 (do-methods p-tail contenders 
				     known-type () known-types)))
		   (funcall methods-function contenders 
			    (let ((k-t (make-list nreq)))
			      (dolist (index+type known-types)
				(setf (nth (car index+type) k-t) (cdr index+type)))
			      k-t))))			      
	     (do-methods (p-tail contenders known-type winners known-types)
	       ;;
               ;; <contenders>
	       ;;   is a (sorted) list of methods that must be discriminated
               ;; <known-type>
	       ;;   is the type of this argument, constructed from tests already made.
               ;; <winners>
	       ;;   is a (sorted) list of methods that are potentially applicable
	       ;;   after the discrimination has been made.
	       ;;   
               (if (null contenders)
		   (do-column (cdr p-tail) winners
			      (cons (cons (car p-tail) known-type) known-types))
                   (let* ((position (car p-tail))
			  (method (car contenders))
			  (specl (nth position (method-specializers method)))
                          (type (funcall type-function (type-from-specializer specl))))
		     (multiple-value-bind (app-p maybe-app-p)
			 (specializer-applicable-using-type-p type known-type)
		       (flet ((determined-to-be (truth-value)
				(if truth-value app-p (not maybe-app-p)))
			      (do-if (truth &optional implied)
				(let ((ntype (if truth type `(not ,type))))
				  (do-methods p-tail
				    (cdr contenders)
				    (if implied
					known-type
					(augment-type ntype known-type))
				    (if truth
					(append winners `(,method))
					winners)
				    known-types))))
			 (cond ((determined-to-be nil) (do-if nil t))
			       ((determined-to-be t)   (do-if t   t))
			       (t (funcall test-function position type 
					   (do-if t) (do-if nil))))))))))
      (do-column precedence methods ()))))

#+lcl3.0 (use-previous-compiler)

(defun compute-secondary-dispatch-function (generic-function net &optional 
					    method-alist wrappers)
  (function-funcall (compute-secondary-dispatch-function1 generic-function net)
		    method-alist wrappers))

(defvar *case-table-limit* 10)

(defun net-test-converter (form)
  (cond ((and (consp form) (eq (car form) 'methods))
	 '.methods.)
	((and (consp form) (eq (car form) 'unordered-methods))
	 '.umethods.)
	((and (consp form) (eq (car form) 'mcase)
	      (< *case-table-limit* (length (cddr form))))
	 '.mcase.)
	(t (default-test-converter form))))

(defun net-code-converter (form metatypes applyp)
  (cond ((and (consp form) (or (eq (car form) 'methods)
			       (eq (car form) 'unordered-methods)))
	 (let ((gensym (gensym)))
	   (values (make-dfun-call metatypes applyp gensym)
		   (list gensym))))
	((and (consp form) (eq (car form) 'mcase)
	      (< *case-table-limit* (length (cddr form))))
	 (let ((gensym (gensym)) (default (gensym)))
	   (values (make-dfun-call metatypes applyp 
				   `(gethash ,(cadr form) ,gensym ,default))
		   (list gensym default))))
	(t (default-code-converter form))))

(defun net-constant-converter (form generic-function)
  (or (let ((c (methods-converter form generic-function)))
	(when c (list c)))
      (cond ((and (consp form) (eq (car form) 'mcase)
		  (< *case-table-limit* (length (cddr form))))
	     (let* ((list (mapcar #'(lambda (clause)
				      (let ((key (car clause))
					    (meth (cadr clause)))
					(cons (if (consp key) (car key) key)
					      (methods-converter meth
								 generic-function))))
				  (cddr form)))
		    (default (car (last list))))
	       (list (list* '.table. (nbutlast list))
		     (cdr default))))
	    (t (default-constant-converter form)))))

(defun methods-converter (form generic-function)
  (cond ((and (consp form) (eq (car form) 'methods))
	 (cons '.methods.
	       (get-effective-method-function1 generic-function (cadr form))))
	((and (consp form) (eq (car form) 'unordered-methods))
	 (default-secondary-dispatch-function generic-function))))

(defun convert-methods (constant method-alist wrappers)
  (if (and (consp constant)
	   (eq (car constant) '.methods.))
      (funcall (cdr constant) method-alist wrappers)
      constant))

(defun convert-table (constant method-alist wrappers)
  (when (and (consp constant)
	     (eq (car constant) '.table.))
    (let ((table (make-hash-table :test 'eql)))
      (dolist (k+v (cdr constant))
	(setf (gethash (car k+v) table)
	      (convert-methods (cdr k+v) method-alist wrappers)))
      table)))

(defun compute-secondary-dispatch-function1 (generic-function net)
  (if (eq (car net) 'methods)
      (get-effective-method-function1 generic-function (cadr net))
      (multiple-value-bind (cfunction constants)
	  (let* ((arg-info (gf-arg-info generic-function))
		 (metatypes (arg-info-metatypes arg-info))
		 (applyp (arg-info-applyp arg-info)))
	    (get-function1 `(lambda ,(make-dfun-lambda-list metatypes applyp) ,net)
			   #'net-test-converter
			   #'(lambda (form)
			       (net-code-converter form metatypes applyp))
			   #'(lambda (form)
			       (net-constant-converter form generic-function))))
	#'(lambda (method-alist wrappers)
	    (apply cfunction
		   (mapcar #'(lambda (constant)
			       (or (convert-table constant method-alist wrappers)
				   (convert-methods constant method-alist wrappers)))
			   constants))))))

(defvar *show-make-unordered-methods-emf-calls* nil)

(defun make-unordered-methods-emf (generic-function methods)
  (when *show-make-unordered-methods-emf-calls*
    (format t "~&make-unordered-methods-emf ~s~%" 
	    (generic-function-name generic-function)))
  #'(lambda (&rest args)
      (let* ((types (types-from-arguments generic-function args 'eql))
	     (smethods (sort-applicable-methods generic-function methods types))
	     (emf (get-effective-method-function generic-function smethods)))
	(function-apply emf args))))

;;;
;;; NOTE: We are assuming a restriction on user code that the method
;;;       combination must not change once it is connected to the
;;;       generic function.
;;;
;;;       This has to be legal, because otherwise any kind of method
;;;       lookup caching couldn't work.  See this by saying that this
;;;       cache, is just a backing cache for the fast cache.  If that
;;;       cache is legal, this one must be too.
;;;
;;; Don't clear this table!  
(defvar *effective-method-table* (make-hash-table :test 'eq))

(defun get-secondary-dispatch-function (gf methods types &optional 
							 method-alist wrappers)
  (function-funcall (get-secondary-dispatch-function1 
		     gf methods types
		     (not (methods-contain-eql-specializer-p methods)))
		    method-alist wrappers))

(defun get-secondary-dispatch-function1 (gf methods types 
					    &optional all-applicable-p
					    (all-sorted-p t))
  (if (null methods)
      #'(lambda (method-alist wrappers)
	  (declare (ignore method-alist wrappers))
	  #'(lambda (&rest args)
	      (apply #'no-applicable-method gf args)))
      (let* ((key (car methods))
	     (ht-value (or (gethash key *effective-method-table*)
			   (setf (gethash key *effective-method-table*)
				 (cons nil nil)))))
	(if (and (null (cdr methods)) all-applicable-p) ; the most common case
	    (or (car ht-value)
		(setf (car ht-value)
		      (get-secondary-dispatch-function2 
		       gf methods types all-applicable-p all-sorted-p)))
	    (let ((akey (list methods (if all-applicable-p 'all-applicable types))))
	      (or (cdr (assoc akey (cdr ht-value) :test #'equal))
		  (let ((value (get-secondary-dispatch-function2 
				gf methods types all-applicable-p all-sorted-p)))
		    (push (cons akey value) (cdr ht-value))
		    value)))))))

(defun get-secondary-dispatch-function2 (gf methods types all-applicable-p all-sorted-p)
  (if (and all-applicable-p all-sorted-p)
      (let* ((combin (generic-function-method-combination gf))
	     (effective (compute-effective-method gf combin methods)))
	(make-effective-method-function1 gf effective))
      (let ((net (generate-discrimination-net 
		  gf methods types all-sorted-p)))
	(compute-secondary-dispatch-function1 gf net))))

(defun get-effective-method-function (gf methods &optional method-alist wrappers)
  (function-funcall (get-secondary-dispatch-function1 gf methods nil t)
		    method-alist wrappers))

(defun get-effective-method-function1 (gf methods &optional (sorted-p t))
  (get-secondary-dispatch-function1 gf methods nil t sorted-p))

(defun get-dispatch-function (gf)
  (let ((methods (generic-function-methods gf)))
    (function-funcall (get-secondary-dispatch-function1 gf methods nil nil nil)
		      nil nil)))

;;;
;;; The value returned by compute-discriminating-function is a function
;;; object.  It is called a discriminating function because it is called
;;; when the generic function is called and its role is to discriminate
;;; on the arguments to the generic function and then call appropriate
;;; method functions.
;;; 
;;; A discriminating function can only be called when it is installed as
;;; the funcallable instance function of the generic function for which
;;; it was computed.
;;;
;;; More precisely, if compute-discriminating-function is called with an
;;; argument <gf1>, and returns a result <df1>, that result must not be
;;; passed to apply or funcall directly.  Rather, <df1> must be stored as
;;; the funcallable instance function of the same generic function <gf1>
;;; (using set-funcallable-instance-function).  Then the generic function
;;; can be passed to funcall or apply.
;;;
;;; An important exception is that methods on this generic function are
;;; permitted to return a function which itself ends up calling the value
;;; returned by a more specific method.  This kind of `encapsulation' of
;;; discriminating function is critical to many uses of the MOP.
;;; 
;;; As an example, the following canonical case is legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (let ((std (call-next-method)))
;;;       #'(lambda (arg)
;;;            (print (list 'call-to-gf gf arg))
;;;            (funcall std arg))))
;;;
;;; Because many discriminating functions would like to use a dynamic
;;; strategy in which the precise discriminating function changes with
;;; time it is important to specify how a discriminating function is
;;; permitted itself to change the funcallable instance function of the
;;; generic function.
;;;
;;; Discriminating functions may set the funcallable instance function
;;; of the generic function, but the new value must be generated by making
;;; a call to COMPUTE-DISCRIMINATING-FUNCTION.  This is to ensure that any
;;; more specific methods which may have encapsulated the discriminating
;;; function will get a chance to encapsulate the new, inner discriminating
;;; function.
;;;
;;; This implies that if a discriminating function wants to modify itself
;;; it should first store some information in the generic function proper,
;;; and then call compute-discriminating-function.  The appropriate method
;;; on compute-discriminating-function will see the information stored in
;;; the generic function and generate a discriminating function accordingly.
;;;
;;; The following is an example of a discriminating function which modifies
;;; itself in accordance with this protocol:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     #'(lambda (arg)
;;;         (cond (<some condition>
;;;                <store some info in the generic function>
;;;                (set-funcallable-instance-function
;;;                  gf
;;;                  (compute-discriminating-function gf))
;;;                (funcall gf arg))
;;;               (t
;;;                <call-a-method-of-gf>))))
;;;
;;; Whereas this code would not be legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     #'(lambda (arg)
;;;         (cond (<some condition>
;;;                (set-funcallable-instance-function
;;;                  gf
;;;                  #'(lambda (a) ..))
;;;                (funcall gf arg))
;;;               (t
;;;                <call-a-method-of-gf>))))
;;;
;;; NOTE:  All the examples above assume that all instances of the class
;;;        my-generic-function accept only one argument.
;;;
;;;
;;;
;;;
(defun gf-dfun-cache (gf)
  (let ((state (gf-dfun-state gf)))
    (typecase state
      (function nil)
      (cons (cadr state)))))

(defun gf-dfun-info (gf)
  (let ((state (gf-dfun-state gf)))
    (typecase state
      (function nil)
      (cons (cddr state)))))

(defun slot-value-using-class-dfun (class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-reader-function slotd) object))

(defun setf-slot-value-using-class-dfun (new-value class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-writer-function slotd) new-value object))

(defun slot-boundp-using-class-dfun (class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-boundp-function slotd) object))

(defmethod compute-discriminating-function ((gf standard-generic-function))
  (with-slots (dfun-state arg-info) gf
    (typecase dfun-state
      (null (let ((name (generic-function-name gf)))
	      (when (eq name 'compute-applicable-methods)
		(update-all-c-a-m-gf-info gf))
	      (cond ((eq name 'slot-value-using-class)
		     (update-slot-value-gf-info gf 'reader)
		     #'slot-value-using-class-dfun)
		    ((equal name '(setf slot-value-using-class))
		     (update-slot-value-gf-info gf 'writer)
		     #'setf-slot-value-using-class-dfun)
		    ((eq name 'slot-boundp-using-class)
		     (update-slot-value-gf-info gf 'boundp)
		     #'slot-boundp-using-class-dfun)
		    ((gf-precompute-dfun-and-emf-p arg-info)
		     (make-final-dfun gf))
		    (t
		     (make-initial-dfun gf)))))
      (function dfun-state)
      (cons (car dfun-state)))))

(defun set-dfun (generic-function &optional dfun cache info)
  (setf (gf-dfun-state generic-function) 
	(if (and dfun (or cache info))
	    (list* dfun cache info)
	    dfun))
  dfun)

(defun update-dfun (generic-function &optional dfun cache info)
  (let ((ocache (gf-dfun-cache generic-function)))
    (set-dfun generic-function dfun cache info)
    (setq dfun (doctor-dfun-for-the-debugger 
		generic-function
		(set-function-name
		 (compute-discriminating-function generic-function)
		 (generic-function-name generic-function))))
    (set-funcallable-instance-function generic-function dfun)
    (when (and ocache (not (eq ocache cache))) (free-cache ocache))
    dfun))

(defmethod update-gf-dfun ((class std-class) gf)
  (let ((*new-class* class)
	(name (generic-function-name gf))
	(arg-info (gf-arg-info gf)))
    (cond ((eq name 'slot-value-using-class)
	   (update-slot-value-gf-info gf 'reader))
	  ((equal name '(setf slot-value-using-class))
	   (update-slot-value-gf-info gf 'writer))
	  ((eq name 'slot-boundp-using-class)
	   (update-slot-value-gf-info gf 'boundp))
	  ((gf-precompute-dfun-and-emf-p arg-info)
	   (multiple-value-bind (dfun cache info)
	       (make-final-dfun-internal gf)
	     (set-dfun gf dfun cache info) ; otherwise cache might get freed twice
	     (update-dfun gf dfun cache info))))))

;;;
;;;
;;;
(defmethod function-keywords ((method standard-method))
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (method-lambda-list method))
    (declare (ignore nreq nopt keysp restp))
    (values keywords allow-other-keys-p)))

(defun method-ll->generic-function-ll (ll)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords keyword-parameters)
      (analyze-lambda-list ll)
    (declare (ignore nreq nopt keysp restp allow-other-keys-p keywords))
    (remove-if #'(lambda (s)
		   (or (memq s keyword-parameters)
		       (eq s '&allow-other-keys)))
	       ll)))


;;;
;;; This is based on the rules of method lambda list congruency defined in
;;; the spec.  The lambda list it constructs is the pretty union of the
;;; lambda lists of all the methods.  It doesn't take method applicability
;;; into account at all yet.
;;; 
(defmethod generic-function-pretty-arglist
	   ((generic-function standard-generic-function))
  (let ((methods (generic-function-methods generic-function))
	(arglist ()))      
    (when methods
      (multiple-value-bind (required optional rest key allow-other-keys)
	  (method-pretty-arglist (car methods))
	(dolist (m (cdr methods))
	  (multiple-value-bind (method-key-keywords
				method-allow-other-keys
				method-key)
	      (function-keywords m)
	    ;; we've modified function-keywords to return what we want as
	    ;;  the third value, no other change here.
	    (declare (ignore method-key-keywords))
	    (setq key (union key method-key))
	    (setq allow-other-keys (or allow-other-keys
				       method-allow-other-keys))))
	(when allow-other-keys
	  (setq arglist '(&allow-other-keys)))
	(when key
	  (setq arglist (nconc (list '&key) key arglist)))
	(when rest
	  (setq arglist (nconc (list '&rest rest) arglist)))
	(when optional
	  (setq arglist (nconc (list '&optional) optional arglist)))
	(nconc required arglist)))))
  

(defmethod method-pretty-arglist ((method standard-method))
  (let ((required ())
	(optional ())
	(rest nil)
	(key ())
	(allow-other-keys nil)
	(state 'required)
	(arglist (method-lambda-list method)))
    (dolist (arg arglist)
      (cond ((eq arg '&optional)         (setq state 'optional))
	    ((eq arg '&rest)             (setq state 'rest))
	    ((eq arg '&key)              (setq state 'key))
	    ((eq arg '&allow-other-keys) (setq allow-other-keys 't))
	    ((memq arg lambda-list-keywords))
	    (t
	     (ecase state
	       (required (push arg required))
	       (optional (push arg optional))
	       (key      (push arg key))
	       (rest     (setq rest arg))))))
    (values (nreverse required)
	    (nreverse optional)
	    rest
	    (nreverse key)
	    allow-other-keys)))

