;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fdefinition.lisp,v 1.23 2003/05/17 11:06:29 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions that hack on the global function namespace (primarily
;;; concerned with SETF functions here).  Also, function encapsulation
;;; and routines that set and return definitions disregarding whether
;;; they might be encapsulated.
;;;
;;; Written by Rob MacLachlan
;;; Modified by Bill Chiles (wrote encapsulation stuff) 
;;; Modified more by William Lott (added ``fdefn'' objects)
;;;

(in-package "EXTENSIONS")

(export '(encapsulate unencapsulate encapsulated-p
	  basic-definition argument-list *setf-fdefinition-hook*
	  define-function-name-syntax valid-function-name-p))


(in-package "KERNEL")

(export '(fdefn make-fdefn fdefn-p fdefn-name fdefn-function fdefn-makunbound
	  %coerce-to-function raw-definition))


(in-package "LISP")

(export '(fdefinition fboundp fmakunbound))



;;;; Function names.

(defvar *valid-function-names* ())

(defun %define-function-name-syntax (name syntax-checker)
  (let ((found (assoc name *valid-function-names* :test #'eq)))
    (if found
	(setf (cdr found) syntax-checker)
	(setq *valid-function-names*
	      (acons name syntax-checker *valid-function-names*)))))

(defmacro define-function-name-syntax (name (var) &body body)
  "Define (NAME ...) to be a valid function name whose syntax is checked
  by BODY.  In BODY, VAR is bound to an actual function name of the
  form (NAME ...) to check.  BODY should return two values.
  First value true means the function name is valid.  Second value
  is the name, a symbol, of the function for use in the BLOCK of DEFUNs
  and in similar situations."
  (let ((syntax-checker (symbolicate '%check- name '-function-name)))
    `(progn
       (defun ,syntax-checker (,var) ,@body)
       (%define-function-name-syntax ',name #',syntax-checker))))

(defun valid-function-name-p (name)
  "First value is true if NAME has valid function name syntax.
  Second value is the name, a symbol, to use as a block name in DEFUNs
  and in similar situations."
  (typecase name
    (cons
     (when (and (symbolp (car name))
		(consp (cdr name)))
       (let ((syntax-checker (cdr (assoc (car name) *valid-function-names*
					 :test #'eq))))
	 (when syntax-checker
	   (funcall syntax-checker name)))))
    (symbol (values t name))
    (otherwise nil)))

(define-function-name-syntax setf (name)
  (destructuring-bind (setf fn &rest rest) name
    (declare (ignore setf))
    (when (null rest)
      (typecase fn
	(symbol
	 (values t fn))
	(cons
	 (unless (eq 'setf (car fn))
	   (valid-function-name-p fn)))))))

(define-function-name-syntax :macro (name)
  (when (eql 2 (length name))
    (valid-function-name-p (second name))))

(define-function-name-syntax :compiler-macro (name)
  (when (eql 2 (length name))
    (valid-function-name-p (second name))))

(define-function-name-syntax flet (name)
  (valid-function-name-p (cadr name)))

(define-function-name-syntax labels (name)
  (valid-function-name-p (cadr name)))


;;;; Fdefinition (fdefn) objects.

(defun make-fdefn (name)
  (make-fdefn name))

(defun fdefn-name (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-name fdefn))

(defun fdefn-function (fdefn)
  (declare (type fdefn fdefn)
	   (values (or function null)))
  (fdefn-function fdefn))

(defun (setf fdefn-function) (fun fdefn)
  (declare (type function fun)
	   (type fdefn fdefn)
	   (values function))
  (setf (fdefn-function fdefn) fun))

(defun fdefn-makunbound (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-makunbound fdefn))


;;; FDEFN-INIT -- internal interface.
;;;
;;; This function is called by %INITIAL-FUNCTION after the globaldb has been
;;; initialized, but before anything else.  We need to install these fdefn
;;; objects into the globaldb *before* any top level forms run, or we will
;;; end up with two different fdefn objects being used for the same function
;;; name.  *INITIAL-FDEFN-OBJECTS* is set up by GENESIS.
;;;
(defvar *initial-fdefn-objects*)

(defun fdefn-init ()
  (dolist (fdefn *initial-fdefn-objects*)
    (setf (info function definition (fdefn-name fdefn)) fdefn))
  (makunbound '*initial-fdefn-objects*))

;;; FDEFINITION-OBJECT -- internal interface.
;;;
(defun fdefinition-object (name create)
  "Return the fdefn object for NAME.  If it doesn't already exist and CREATE
   is non-NIL, create a new (unbound) one."
  (declare (values (or fdefn null)))
  (unless (valid-function-name-p name)
    (error 'simple-type-error
	   :datum name
	   :expected-type '(or symbol list)
	   :format-control "Invalid function name: ~S"
	   :format-arguments (list name)))
  (let ((fdefn (info function definition name)))
    (if (and (null fdefn) create)
	(setf (info function definition name) (make-fdefn name))
	fdefn)))

(declaim (inline fdefn-or-lose))
(defun fdefn-or-lose (name)
  "Return the FDEFN of NAME.  Signal an error if there is none
   or if it's function is null."
  (let ((fdefn (fdefinition-object name nil)))
    (unless (and fdefn (fdefn-function fdefn))
      (error 'undefined-function :name name))
    fdefn))

;;; %COERCE-TO-FUNCTION -- public.
;;;
;;; The compiler emits calls to this when someone tries to funcall a symbol.
;;;
(defun %coerce-to-function (name)
  "Returns the definition for name, including any encapsulations.  Settable
   with SETF."
  (fdefn-function (fdefn-or-lose name)))

;;; RAW-DEFINITION -- public.
;;;
;;; Just another name for %coerce-to-function.
;;; 
(declaim (inline raw-definition))
(defun raw-definition (name)
  (declare (optimize (inhibit-warnings 3)))
  ;; We know that we are calling %coerce-to-function, so don't tell us about
  ;; it.
  (%coerce-to-function name))

(defun (setf raw-definition) (function name)
  (let ((fdefn (fdefinition-object name t)))
    (setf (fdefn-function fdefn) function)))



;;;; Definition Encapsulation.

(defstruct (encapsulation
	    (:alternate-metaclass funcallable-instance
				  funcallable-structure-class
				  make-funcallable-structure-class)
	    (:type funcallable-structure))
  ;;
  ;; Something like the symbol TRACE for an encapsulation done
  ;; by TRACE, for instance.
  type
  ;;
  ;; The function definition before this encapsulation was added.
  (next #'null :type function))

(defmacro do-encapsulations ((var fdefn &optional result) &body body)
  "Evaluate BODY with VAR bound to consecutive encapsulations of
   FDEFN.  Return RESULT at the end."
  `(loop for ,var = (encapsulation (fdefn-function ,fdefn))
	 then (encapsulation (encapsulation-next ,var))
	 while ,var do (locally ,@body)
	 finally (return ,result)))

(defun encapsulation (fun)
  "Return FUN if it is an encapsulation or NIL if it isn't."
  (and (functionp fun)
       ;; Necessary for cold-load reasons.
       (= (get-type fun) vm:funcallable-instance-header-type)
       (encapsulation-p fun)
       fun))

(declaim (inline last-encapsulation))
(defun last-encapsulation (fdefn)
  "Return tha last encapsulation of FDEFN or NIL if none."
  (declare (type fdefn fdefn))
  (do-encapsulations (e fdefn)
    (when (null (encapsulation (encapsulation-next e)))
      (return e))))

(defun push-encapsulation (e name)
  "Prepend encapsulation E to the definition of NAME.
   Signal an error if NAME is an undefined function."
  (declare (type encapsulation e))
  (let ((fdefn (fdefn-or-lose name)))
    (setf (encapsulation-next e) (fdefn-function fdefn))
    (setf (fdefn-function fdefn) e)))

(defun encapsulate (name type body)
  "Replace the definition of NAME with a function that binds NAME's
   arguments to a variable named ARGUMENT-LIST, binds NAME's
   definition to a variable named BASIC-DEFINITION, and evaluates BODY
   in that context.  TYPE is whatever you would like to associate with
   this encapsulation for identification in case you need multiple
   encapsulations of the same name."
  (let ((e (make-encapsulation :type type)))
    (setf (funcallable-instance-function e)
	  #'(instance-lambda (&rest argument-list)
	      (declare (special argument-list))
	      (let ((basic-definition (encapsulation-next e)))
		(declare (special basic-definition))
		(eval body))))
    (push-encapsulation e name)))

(defun unencapsulate (name type)
  "Remove the first encapsulation of type TYPE, if any, from the
  definition of NAME."
  (let ((fdefn (fdefinition-object name nil))
	(prev nil))
    (do-encapsulations (e fdefn)
      (when (eq (encapsulation-type e) type)
	(if prev
	    (setf (encapsulation-next prev) (encapsulation-next e))
	    (setf (fdefn-function fdefn) (encapsulation-next e)))
	  (return))
      (setq prev e))))

(defun encapsulated-p (name type)
  "Return true if NAME has an encapsulation of type TYPE."
  (do-encapsulations (e (fdefinition-object name nil))
    (when (eq (encapsulation-type e) type)
      (return t))))



;;;; FDEFINITION.

(defun fdefinition (name)
  "Return name's global function definition taking care to regard any
   encapsulations and to return the innermost encapsulated definition.
   This is SETF'able."
  (let* ((fdefn (fdefn-or-lose name))
	 (last (last-encapsulation fdefn)))
      (if last
	  (encapsulation-next last)
	  (fdefn-function fdefn))))

(defvar *setf-fdefinition-hook* nil
  "This holds functions that (SETF FDEFINITION) invokes before storing the
   new value.  These functions take the function name and the new value.")

(defun %set-fdefinition (name new-value)
  "Set NAME's global function definition to NEW-VALUE."
  (declare (type function new-value) (optimize (safety 1)))
  (let ((fdefn (fdefinition-object name t)))
    (when (boundp '*setf-fdefinition-hook*)
      (dolist (f *setf-fdefinition-hook*)
	(funcall f name new-value)))
    (let ((last (last-encapsulation fdefn)))
      (if last
	  (setf (encapsulation-next last) new-value)
	  (setf (fdefn-function fdefn) new-value)))))

(defsetf fdefinition %set-fdefinition)



;;;; FBOUNDP and FMAKUNBOUND.

(defun fboundp (name)
  "Return true if name has a global function definition."
  (let ((fdefn (fdefinition-object name nil)))
    (and fdefn (fdefn-function fdefn) t)))

(defun fmakunbound (name)
  "Make Name have no global function definition."
  (let ((fdefn (fdefinition-object name nil)))
    (when fdefn
      (fdefn-makunbound fdefn)))
  (kernel:undefine-function-name name)
  name)
