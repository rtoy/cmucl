;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fdefinition.lisp,v 1.22 2003/03/31 11:13:23 gerd Exp $")
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
;;;
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

;;; %COERCE-TO-FUNCTION -- public.
;;;
;;; The compiler emits calls to this when someone tries to funcall a symbol.
;;;
(defun %coerce-to-function (name)
  "Returns the definition for name, including any encapsulations.  Settable
   with SETF."
  (let ((fdefn (fdefinition-object name nil)))
    (or (and fdefn (fdefn-function fdefn))
	(error 'undefined-function :name name))))

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
;;;
(defun (setf raw-definition) (function name)
  (let ((fdefn (fdefinition-object name t)))
    (setf (fdefn-function fdefn) function)))



;;;; Definition Encapsulation.

(defstruct (encapsulation-info (:print-function print-encapsulation-info)
			       (:constructor make-encapsulation-info
					     (type definition)))
  ;; This is definition's encapsulation type.  The encapsulated definition is
  ;; in the previous encapsulation-info element or installed as the global
  ;; definition of some function name.
  type
  ;; Previous definition.  This used to be installed as a global definition
  ;; for some function name, but it was replaced by an encapsulation of type
  ;; type.
  (definition nil :type function))
;;;
(defun print-encapsulation-info (obj str n)
  (declare (ignore n))
  (format str "#<Encapsulation-Info  Definition: ~S  Type: ~S>"
	  (%function-name (encapsulation-info-definition obj))
	  (encapsulation-info-type obj)))


;;; ENCAPSULATE -- Public.
;;;
;;; We must bind and close over info.  Consider the case where we encapsulate
;;; (the second) an encapsulated (the first) definition, and later someone
;;; unencapsulates the encapsulated (first) definition.  We don't want our
;;; encapsulation (second) to bind basic-definition to the encapsulated (first)
;;; definition when it no longer exists.  When unencapsulating, we make sure to
;;; clobber the appropriate info structure to allow basic-definition to be
;;; bound to the next definition instead of an encapsulation that no longer
;;; exists.
;;;
(defun encapsulate (name type body)
  "Replaces the definition of NAME with a function that binds name's arguments
   a variable named argument-list, binds name's definition to a variable named
   basic-definition, and evaluates BODY in that context.  TYPE is
   whatever you would like to associate with this encapsulation for
   identification in case you need multiple encapsuations of the same name."
  (let ((fdefn (fdefinition-object name nil)))
    (unless (and fdefn (fdefn-function fdefn))
      (error 'undefined-function :name name))
    (let ((info (make-encapsulation-info type (fdefn-function fdefn))))
      (setf (fdefn-function fdefn)
	    #'(lambda (&rest argument-list)
		(declare (special argument-list))
		(let ((basic-definition (encapsulation-info-definition info)))
		  (declare (special basic-definition))
		  (eval body)))))))

;;; ENCAPSULATION-INFO -- internal.
;;;
;;; Finds the encapsulation info that has been closed over.
;;; 
(defun encapsulation-info (fun)
  (and (functionp fun)
       (= (get-type fun) vm:closure-header-type)
       (find-if-in-closure #'encapsulation-info-p fun)))

;;; UNENCAPSULATE -- Public.
;;;
;;; When removing an encapsulation, we must remember that encapsulating
;;; definitions close over a reference to the encapsulation-info that describes
;;; the encapsulating definition.  When you find an info with the target type,
;;; the previous info in the chain has the ensulating definition of that type.
;;; We take the encapsulated definition from the info with the target type, and
;;; we store it in the previous info structure whose encapsulating definition
;;; it describes looks to this previous info structure for a definition to
;;; bind (see ENCAPSULATE).  When removing the first info structure, we do
;;; something conceptually equal, but mechanically it is different.
;;;
(defun unencapsulate (name type)
  "Removes name's most recent encapsulation of the specified type."
  (let* ((fdefn (fdefinition-object name nil))
	 (encap-info (encapsulation-info (fdefn-function fdefn))))
    (declare (type (or encapsulation-info null) encap-info))
    (cond ((not encap-info)
	   ;; It disappeared on us, so don't worry about it.
	   )
	  ((eq (encapsulation-info-type encap-info) type)
	   ;; It's the first one, so change the fdefn object.
	   (setf (fdefn-function fdefn)
		 (encapsulation-info-definition encap-info)))
	  (t
	   ;; It must be an interior one, so find it.
	   (loop
	     (let ((next-info (encapsulation-info
			       (encapsulation-info-definition encap-info))))
	       (unless next-info
		 ;; Not there, so don't worry about it.
		 (return))
	       (when (eq (encapsulation-info-type next-info) type)
		 ;; This is it, so unlink us.
		 (setf (encapsulation-info-definition encap-info)
		       (encapsulation-info-definition next-info))
		 (return))
	       (setf encap-info next-info))))))
  t)

;;; ENCAPSULATED-P -- Public.
;;;
(defun encapsulated-p (name type)
  "Returns t if name has an encapsulation of the given type, otherwise nil."
  (let ((fdefn (fdefinition-object name nil)))
    (do ((encap-info (encapsulation-info (fdefn-function fdefn))
		     (encapsulation-info
		      (encapsulation-info-definition encap-info))))
	((null encap-info) nil)
      (declare (type (or encapsulation-info null) encap-info))
      (when (eq (encapsulation-info-type encap-info) type)
	(return t)))))


;;;; FDEFINITION.

(defun fdefinition (name)
  "Return name's global function definition taking care to regard any
   encapsulations and to return the innermost encapsulated definition.
   This is SETF'able."
  (let ((fun (raw-definition name)))
    (loop
      (let ((encap-info (encapsulation-info fun)))
	(if encap-info
	    (setf fun (encapsulation-info-definition encap-info))
	    (return fun))))))

(defvar *setf-fdefinition-hook* nil
  "This holds functions that (SETF FDEFINITION) invokes before storing the
   new value.  These functions take the function name and the new value.")

(defun %set-fdefinition (name new-value)
  "Set name's global function definition."
  (declare (type function new-value) (optimize (safety 1)))
  (let ((fdefn (fdefinition-object name t)))
    ;; *setf-fdefinition-hook* won't be bound when
    ;; initially running top-level forms in the kernel
    ;; core startup.
    (when (boundp '*setf-fdefinition-hook*)
      (dolist (f *setf-fdefinition-hook*)
	(funcall f name new-value)))

    (let ((encap-info (encapsulation-info (fdefn-function fdefn))))
      (cond (encap-info
	     (loop
	       (let ((more-info
		      (encapsulation-info
		       (encapsulation-info-definition encap-info))))
		 (if more-info
		     (setf encap-info more-info)
		     (return
		      (setf (encapsulation-info-definition encap-info)
			    new-value))))))
	    (t
	     (setf (fdefn-function fdefn) new-value))))))
;;;
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
