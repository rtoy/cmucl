;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/fdefinition.lisp $")
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

(intl:textdomain "cmucl")

(export '(encapsulate unencapsulate encapsulated-p
	  basic-definition argument-list *setf-fdefinition-hook*
	  define-function-name-syntax valid-function-name-p))


(in-package "KERNEL")

(export '(fdefn make-fdefn fdefn-p fdefn-name fdefn-function fdefn-makunbound
	  fdefn-or-lose %coerce-to-function raw-definition))


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
     (cond
       ((and (symbolp (car name))
	     (consp (cdr name)))
	(let ((syntax-checker (cdr (assoc (car name) *valid-function-names*
					  :test #'eq))))
	  (if syntax-checker
	      (funcall syntax-checker name)
	      (values nil name))))
       (t
	(values nil name))))
    (symbol (values t name))
    (otherwise (values nil name))))

(define-function-name-syntax setf (name)
  (destructuring-bind (setf fn &rest rest) name
    (declare (ignore setf))
    (if rest
	(values nil name)
	(typecase fn
	  (symbol
	   (values t fn))
	  (cons
	   (cond ((eq 'setf (car fn))
		  (values nil fn))
		 (t
		  (valid-function-name-p fn))))
	  (otherwise
	   (values nil fn))))))

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
  (setq *valid-function-names* nil)
  (dolist (fdefn *initial-fdefn-objects*)
    (setf (info function definition (fdefn-name fdefn)) fdefn))
  (makunbound '*initial-fdefn-objects*))

;;; FDEFINITION-OBJECT -- internal interface.
;;;
(defun fdefinition-object (name create)
  "Return the fdefn object for NAME.  If it doesn't already exist and CREATE
   is non-NIL, create a new (unbound) one."
  (declare (values (or fdefn null)))
  (multiple-value-bind (valid-name-p fname)
      (valid-function-name-p name)
    (unless valid-name-p
      (error 'simple-type-error
	     :datum fname
	     :expected-type '(satisfies valid-function-name-p)
	     :format-control (intl:gettext "Invalid function name: ~S")
	     :format-arguments (list name))))
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


;;;; FDEFINITION.

(defun fdefinition (function-name)
  "Return FUNCTION-NAME's global function definition.
   If FUNCTION-NAME is fwrapped, return the primary function definition
   stored in the innermost fwrapper."
  (let* ((fdefn (fdefn-or-lose function-name))
	 (last (fwrappers:last-fwrapper fdefn)))
      (if last
	  (fwrappers:fwrapper-next last)
	  (fdefn-function fdefn))))

(defvar *setf-fdefinition-hook* nil
  "This holds functions that (SETF FDEFINITION) invokes before storing the
   new value.  These functions take the function name and the new value.")

(defun %set-fdefinition (function-name new-value)
  "Set FUNCTION-NAME's global function definition to NEW-VALUE.
   If FUNCTION-NAME is fwrapped, set the primary function stored
   in the innermost fwrapper."
  (declare (type function new-value) (optimize (safety 1)))
  (let ((fdefn (fdefinition-object function-name t)))
    (when (boundp '*setf-fdefinition-hook*)
      (dolist (f *setf-fdefinition-hook*)
	(funcall f function-name new-value)))
    (let ((last (fwrappers:last-fwrapper fdefn)))
      (if last
	  (setf (fwrappers:fwrapper-next last) new-value)
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



(defstruct callsite
  (type (ext:required-argument) :type kernel:function-type :read-only t)
  (fdefn (ext:required-argument) :type kernel:fdefn :read-only t))

(defstruct linkage
  (callsites nil :type (or callsite list))
  (adapters nil :type (or function list)))

(defun listify (x)
  (if (listp x) x (list x)))

(defmacro push-unlistified (new-value (reader object))
  `(let ((new-value ,new-value) (object ,object))
     (let ((old-value (,reader ,object)))
       (setf (,reader object)
	     (typecase old-value
	       (null new-value)
	       (cons (cons new-value old-value))
	       (t (list new-value old-value)))))))

(defun find-typed-entry-point-in-code (code name)
  (loop for ep = (%code-entry-points code) then (%function-next ep)
	while ep do
	(let ((fname (%function-name ep)))
	  (when (and (consp fname)
		     (eq (car fname) :typed-entry-point)
		     (consp (cdr fname))
		     (equal (cadr fname) name))
	    (return ep)))))

(defun find-typed-entry-point-for-fdefn (fdefn)
  (let ((xep (fdefn-function fdefn)))
    (let ((code (function-code-header xep)))
      (find-typed-entry-point-in-code code (fdefn-name fdefn)))))

;; find-typed-entry-point is called at load-time and returns the
;; fdefn that should be called.
;;
;; 1. We go through the list of existing callsites to see if we
;; already have one with the same type and reuse it if possible.
;;
;; 2. We look at the current definition.  If the types match, we
;; create a callsite object, store it in the info db, and return the
;; fdefn.
;;
;; 3. Now we know that the types don't match we need to use adapters.
;; First again, we look at existing adapters and reuse them if possible.
;;
;; 4. An adapter is created that boxes the arguments and forwards the
;; call to the "normal" entry point.
;;
;; 5. If we are not allowed to create adapters, we look again at the
;; current definition to handle the case where no current definition
;; exists.  If so, we return an empty fdefn object that will call the
;; undefined-tramp assembly routine.
;;
;; 6. If all else fails we link the callsite to our error handler.
;;
(declaim (ftype (function (t t) kernel:fdefn) find-typed-entry-point))
(defun find-typed-entry-point (name callsite-typespec)
  (let* ((cs-type (kernel:specifier-type callsite-typespec))
	 (linkage (multiple-value-bind (info foundp)
		      (ext:info function linkage name)
		    (cond (foundp info)
			  (t (setf (ext:info function linkage name)
				   (make-linkage)))))))
    (cond ((and nil (dolist (cs (listify (linkage-callsites linkage)))
	     (let* ((ep-type (callsite-type cs)))
	       (when (function-types-compatible-p cs-type ep-type)
		 (return (callsite-fdefn cs)))))))
	  ((let ((fdefn (fdefinition-object name nil)))
	     (when fdefn
	       (let ((fun (find-typed-entry-point-for-fdefn fdefn)))
		 (when fun
		   (let ((ep-type (kernel:extract-function-type fun)))
		     (when (function-types-compatible-p cs-type ep-type)
		       (let* ((aname (kernel:%function-name fun))
			      (fdefn (kernel:make-fdefn aname))
			      (cs (make-callsite :type cs-type :fdefn fdefn)))
			 (setf (kernel:fdefn-function fdefn) fun)
			 (push-unlistified cs (linkage-callsites linkage))
			 fdefn))))))))
	  ((or (not (lisp::fdefinition-object name nil))
	       (not (kernel:fdefn-function
		     (lisp::fdefinition-object name nil))))
	   (let* ((aname `(:typed-entry-point #:undefined))
		  (fdefn (kernel:make-fdefn aname))
		  (cs (make-callsite :type cs-type :fdefn fdefn)))
	     (push-unlistified cs (linkage-callsites linkage))
	     fdefn))
	  ((dolist (fun (listify (linkage-adapters linkage)))
	     (let ((ep-type (kernel:extract-function-type fun)))
	       (when (function-types-compatible-p cs-type ep-type)
		 (let* ((aname (kernel:%function-name fun))
			(fdefn (kernel:make-fdefn aname))
			(cs (make-callsite :type cs-type :fdefn fdefn)))
		   (setf (kernel:fdefn-function fdefn) fun)
		   (push-unlistified cs (linkage-callsites linkage))
		   (return fdefn))))))
	  (t
	   (let* ((fun (generate-adapter-function cs-type name))
		  (fdefn (kernel:make-fdefn (kernel:%function-name fun)))
		  (cs (make-callsite :type cs-type :fdefn fdefn)))
	     (setf (kernel:fdefn-function fdefn) fun)
	     (push-unlistified fun (linkage-adapters linkage))
	     (push-unlistified cs (linkage-callsites linkage))
	     fdefn)))))

(defun linkage-error (&rest args)
  (declare (ignore args))
  (error "Linking callsite to typed-entry-point failed"))

;; Generate an adapter function that changes the representation of the
;; arguments (specified with FTYPE) and forwards the call to NAME.
;; The adapter has also a typed entry point.  It should also check
;; that the values returned by NAME match FTYPE.
;;
;; In practice, the compiler infered type may not match exactly FTYPE,
;; even if we add lotso declarations.  This is annyoingly brittle.
(defun generate-adapter-function (ftype name)
  (let* ((atypes (kernel:function-type-required ftype))
	 (tmps (loop for nil in atypes collect (gensym)))
	 (fname `(:typed-entry-point
		  :boxing-adapter ,(make-symbol (string name))))
	 (ftypespec (kernel:type-specifier ftype)))
    (proclaim `(ftype ,ftypespec ,fname))
    (compile fname
	     `(lambda ,tmps
		(declare
		 ,@(loop for tmp in tmps
			 for type in atypes
			 collect `(type ,(kernel:type-specifier type) ,tmp)))
		(the ,(kernel:type-specifier
		       (kernel:function-type-returns ftype))
		   (funcall (function ,name) . ,tmps))))
    (let ((fun (fdefinition fname)))
      (unless (eq name 'linkage-error)
	(fix-ftype fun ftype))
      fun)))

(defun fix-ftype (fun ftype)
  (let ((etype (kernel:extract-function-type fun)))
    (unless (function-types-compatible-p ftype etype t)
      (break)))
  fun)

;; This is our rule to decide when a type at a callsite matches the
;; type of the entry point.
;;
;; 1. The arguments at the callsite should be subtypes of the
;; arguments at the entry point.
;;
;; 2. The return value at the callsite should be supertypes of the
;; return values at the entry point.
;;
;; 3. The representations must agree.  Representations should probably
;; decided in the backend, but for now we assume only double-floats
;; are unboxed.
(defun function-types-compatible-p (callsite-type entrypoint-type
				    &optional ignore-representation)
  (flet ((return-types (ftype)
	   (let ((type (kernel:function-type-returns ftype)))
	     (cond ((kernel:values-type-p type)
		    (assert (and (not (kernel:values-type-rest type))
				 (not (kernel:values-type-keyp type))))
		    (kernel:values-type-required type))
		   (t
		    (list type)))))
	 (ptype= (type1 type2)
	   (let ((double-float (kernel:specifier-type 'double-float)))
	     (cond (ignore-representation t)
		   ((kernel:type= type1 double-float)
		    (kernel:type= type2 double-float))
		   ((kernel:type= type2 double-float)
		    nil)
		   (t t)))))
    (and (every #'kernel:csubtypep
		(kernel:function-type-required callsite-type)
		(kernel:function-type-required entrypoint-type))
	 (every #'ptype=
		(kernel:function-type-required callsite-type)
		(kernel:function-type-required entrypoint-type))
	 (or
	  (and (every #'kernel:csubtypep
		      (return-types entrypoint-type)
		      (return-types callsite-type))
	       (every #'ptype=
		      (return-types entrypoint-type)
		      (return-types callsite-type)))
	  (kernel:type= (kernel:function-type-returns entrypoint-type)
			(kernel:specifier-type 'nil))))))


;; check-function-redefinition is used as setf-fdefinition-hook.
;; We go through all existing callsites and
;;
;; 1. If the new type matches, we patch the callsite with the new function.
;;
;; 2. If the types don't match and if allowed, we redirect the
;; callsite to and adapter.
;;
;; 3. If the callsites doesn't want adapters we link the callsite to
;; an error handler.
(defun check-function-redefinition (name new-fun)
  (multiple-value-bind (linkage foundp) (ext:info function linkage name)
    (when foundp
      (let* ((new-code (function-code-header new-fun))
	     (new-tep (find-typed-entry-point-in-code new-code name))
	     (new-type (extract-function-type new-tep)))
	(dolist (cs (listify (linkage-callsites linkage)))
	  (let ((cs-type (callsite-type cs))
		(fdefn (callsite-fdefn cs)))
	    (cond ((function-types-compatible-p cs-type new-type)
		   (patch-fdefn fdefn new-tep))
		  ((dolist (fun (listify (linkage-adapters linkage)))
		     (let ((ep-type (kernel:extract-function-type fun)))
		       (when (function-types-compatible-p cs-type ep-type)
			 (patch-fdefn fdefn fun)
			 (return t)))))
		  (t
		   (let ((fun (generate-adapter-function cs-type name)))
		     (push-unlistified fun (linkage-adapters linkage))
		     (patch-fdefn fdefn fun))))))))))

;; This lets us set the name in fdefn objects.  We use that for
;; debugging.
#-bootstrap
(eval-when (:compile-toplevel)
  (c:defknown set-fdefn-name (kernel:fdefn t) t)
  (c:def-setter set-fdefn-name vm:fdefn-name-slot vm:other-pointer-type))

(defun patch-fdefn (fdefn new-fun)
  (setf (kernel:fdefn-function fdefn) new-fun)
  (let ((name (kernel:%function-name new-fun)))
    (set-fdefn-name fdefn name))
  fdefn)

(pushnew 'check-function-redefinition ext:*setf-fdefinition-hook*)
