;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/proclaim.lisp,v 1.18 1991/05/28 17:54:59 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains load-time support for declaration processing.  It is
;;; split off from the compiler so that the compiler doesn'thave to be in the
;;; cold load.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

(in-package "EXTENSIONS")
(export '(inhibit-warnings freeze-type optimize-interface constant-function))
(in-package "LISP")
(export '(declaim proclaim))
(in-package "C")


;;; True if the type system has been properly initialized, and thus is o.k. to
;;; use.
;;;
(defvar *type-system-initialized* nil)

;;; The Cookie holds information about the compilation environment for a node.
;;; See the Lexenv definition for a description of how it is used.
;;;
(defstruct cookie
  (speed nil :type (or (rational 0 3) null))
  (space nil :type (or (rational 0 3) null))
  (safety nil :type (or (rational 0 3) null))
  (cspeed nil :type (or (rational 0 3) null))
  (brevity nil :type (or (rational 0 3) null))
  (debug nil :type (or (rational 0 3) null)))


;;; The *default-cookie* represents the current global compiler policy
;;; information.  Whenever the policy is changed, we copy the structure so that
;;; old uses will still get the old values.  *default-interface-cookie* holds
;;; any values specified by an OPTIMIZE-INTERFACE declaration.
;;;
(proclaim '(type cookie *default-cookie* *default-interface-cookie*))
(defvar *default-cookie* (make-cookie :safety 1 :speed 1 :space 1 :cspeed 1
				      :brevity 1 :debug 2))
(defvar *default-interface-cookie* (make-cookie))


;;; A list of UNDEFINED-WARNING structures representing the calls to unknown
;;; functions.  This is bound by WITH-COMPILATION-UNIT.
;;;
(defvar *undefined-warnings*)
(proclaim '(list *undefined-warnings*))

;;; NOTE-NAME-DEFINED  --  Interface
;;;
;;;    Delete any undefined warnings for Name and Kind.  We do the BOUNDP check
;;; because this function can be called when not in a compilation unit (as when
;;; loading top-level forms.)
;;;
(defun note-name-defined (name kind)
  (when (boundp '*undefined-warnings*)
    (setq *undefined-warnings*
	  (delete-if #'(lambda (x)
			 (and (equal (undefined-warning-name x) name)
			      (eq (undefined-warning-kind x) kind)))
		     *undefined-warnings*)))
  (undefined-value))


;;; Parse-Lambda-List  --  Interface
;;;
;;;    Break a lambda-list into its component parts.  We return eight values:
;;;  1] A list of the required args.
;;;  2] A list of the optional arg specs.
;;;  3] True if a rest arg was specified.
;;;  4] The rest arg.
;;;  5] A boolean indicating whether keywords args are present.
;;;  6] A list of the keyword arg specs.
;;;  7] True if &allow-other-keys was specified.
;;;  8] A list of the &aux specifiers.
;;;
;;; The top-level lambda-list syntax is checked for validity, but the arg
;;; specifiers are just passed through untouched.  If something is wrong, we
;;; use Compiler-Error, aborting compilation to the last recovery point.
;;;
(proclaim '(function parse-lambda-list (list)
		     (values list list boolean t boolean list boolean list)))
(defun parse-lambda-list (list)
  (collect ((required)
	    (optional)
	    (keys)
	    (aux))
    (let ((restp nil)
	  (rest nil)
	  (keyp nil)
	  (allowp nil)
	  (state :required))
      (dolist (arg list)
	(if (and (symbolp arg)
		 (let ((name (symbol-name arg)))
		   (and (/= (length name) 0)
			(char= (char name 0) #\&))))
	    (case arg
	      (&optional
	       (unless (eq state :required)
		 (compiler-error "Misplaced &optional in lambda-list: ~S." list))
	       (setq state '&optional))
	      (&rest
	       (unless (member state '(:required &optional))
		 (compiler-error "Misplaced &rest in lambda-list: ~S." list))
	       (setq state '&rest))
	      (&key
	       (unless (member state '(:required &optional :post-rest))
		 (compiler-error "Misplaced &key in lambda-list: ~S." list))
	       (setq keyp t)
	       (setq state '&key))
	      (&allow-other-keys
	       (unless (eq state '&key)
		 (compiler-error "Misplaced &allow-other-keys in lambda-list: ~S." list))
	       (setq allowp t  state '&allow-other-keys))
	      (&aux
	       (when (eq state '&rest)
		 (compiler-error "Misplaced &aux in lambda-list: ~S." list))
	       (setq state '&aux))
	      (t
	       (compiler-error "Unknown &keyword in lambda-list: ~S." arg)))
	    (case state
	      (:required (required arg))
	      (&optional (optional arg))
	      (&rest
	       (setq restp t  rest arg  state :post-rest))
	      (&key (keys arg))
	      (&aux (aux arg))
	      (t
	       (compiler-error "Found garbage in lambda-list when expecting a keyword: ~S." arg)))))
    (values (required) (optional) restp rest keyp (keys) allowp (aux)))))


;;; Check-Function-Name  --  Interface
;;;
;;;    Check that Name is a valid function name, returning the name if OK, and
;;; doing an error if not.  In addition to checking for basic well-formedness,
;;; we also check that symbol names are not NIL or the name of a special form.
;;;
(defun check-function-name (name)
  (typecase name
    (list
     (unless (and (consp name) (consp (cdr name))
		  (null (cddr name)) (eq (car name) 'setf)
		  (symbolp (cadr name)))
       (compiler-error "Illegal function name: ~S." name))
     name)
    (symbol
     (when (eq (info function kind name) :special-form)
       (compiler-error "Special form is an illegal function name: ~S." name))
     name)
    (t
     (compiler-error "Illegal function name: ~S." name))))


;;; NOTE-IF-SETF-FUNCTION-AND-MACRO  --  Interface
;;;
;;;    Called to do something about SETF functions that overlap with setf
;;; macros.  Perhaps we should interact with the user to see if the macro
;;; should be blown away, but for now just give a warning.  Due to the weak
;;; semantics of the (SETF FUNCTION) name, we can't assume that they aren't
;;; just naming a function (SETF FOO) for the heck of it.  Name is already
;;; known to be well-formed.
;;;
(defun note-if-setf-function-and-macro (name)
  (when (consp name)
    (when (or (info setf inverse name)
	      (info setf expander name))
      (compiler-warning
       "Defining as a SETF function a name that already has a SETF macro:~
       ~%  ~S"
       name)))
  (undefined-value))


;;; Define-Function-Name  --  Interface
;;;
;;;    Check the legality of a function name that is being introduced.
;;; -- If it names a macro, then give a warning and blast the macro
;;;    information.
;;; -- If it is a structure slot accessor, give a warning and blast the
;;;    structure. 
;;; -- Check for conflicting setf macros.
;;;
(proclaim '(function define-function-name (t) void))
(defun define-function-name (name)
  (check-function-name name)
  (ecase (info function kind name)
    (:function
     (let ((for (info function accessor-for name)))
       (when for
	 (compiler-warning
	  "Undefining structure type:~%  ~S~@
	  so that this slot accessor can be redefined:~%  ~S"
	  (dd-name for) name)
	 (undefine-structure for)
	 (setf (info function kind name) :function)))
     (when (info function alien-operator name)
       (compiler-warning "Redefining alien operator as normal function:~%  ~S"
			 name)
       (setf (info function alien-operator name) nil)
       (setf (info function source-transform name) nil)))
    (:macro
     (compiler-warning "~S previously defined as a macro." name)
     (setf (info function kind name) :function)
     (setf (info function where-from name) :assumed)
     (clear-info function macro-function name))
    ((nil)
     (setf (info function kind name) :function)))
  

  (note-if-setf-function-and-macro name)
  name)


;;; UNDEFINE-FUNCTION-NAME  --  Interface
;;;
;;;    Make Name no longer be a function name: clear everything back to the
;;; default.
;;;
(defun undefine-function-name (name)
  (when name
    (macrolet ((frob (type &optional val)
		 `(unless (eq (info function ,type name) ,val)
		    (setf (info function ,type name) ,val))))
      (frob info)
      (frob type (specifier-type 'function))
      (frob where-from :assumed)
      (frob inlinep)
      (frob kind)
      (frob accessor-for)
      (frob inline-expansion)
      (frob alien-operator)
      (frob source-transform)
      (frob assumed-type)))
  (undefined-value))


;;; Process-Optimize-Declaration  --  Interface
;;;
;;;    Return a new cookie containing the policy information represented by the
;;; optimize declaration Spec.  Any parameters not specified are defaulted from
;;; Cookie.
;;;
(proclaim '(function process-optimize-declaration (list cookie) cookie))
(defun process-optimize-declaration (spec cookie)
  (let ((res (copy-cookie cookie)))
    (dolist (quality (cdr spec))
      (let ((quality (if (atom quality) (list quality 3) quality)))
	(if (and (consp (cdr quality)) (null (cddr quality))
		 (typep (second quality) 'real) (<= 0 (second quality) 3))
	    (let ((value (rational (second quality))))
	      (case (first quality)
		(speed (setf (cookie-speed res) value))
		(space (setf (cookie-space res) value))
		(safety (setf (cookie-safety res) value))
		(compilation-speed (setf (cookie-cspeed res) value))
		((inhibit-warnings brevity) (setf (cookie-brevity res) value))
		((debug-info debug) (setf (cookie-debug res) value))
		(t
		 (compiler-warning "Unknown optimization quality ~S in ~S."
				   (car quality) spec))))
	    (compiler-warning
	     "Malformed optimization quality specifier ~S in ~S."
	     quality spec))))
    res))


;;; DECLAIM  --  Public
;;;
;;;    For now, just PROCLAIM without any EVAL-WHEN.
;;;
(defmacro declaim (&rest specs)
  "DECLAIM Declaration*
  Do a declaration for the global environment."
  `(progn ,@(mapcar #'(lambda (x)
			`(proclaim ',x))
		    specs)))
  

;;; %Proclaim  --  Interface
;;;
;;;    This function is the guts of proclaim, since it does the global
;;; environment updating.
;;;
(defun %proclaim (form)
  (unless (consp form)
    (error "Malformed PROCLAIM spec: ~S." form))
  
  (let ((kind (first form))
	(args (rest form)))
    (case kind
      (special
       (dolist (name args)
	 (unless (symbolp name)
	   (error "Variable name is not a symbol: ~S." name))
	 (clear-info variable constant-value name)
	 (setf (info variable kind name) :special)))
      (type
       (when *type-system-initialized*
	 (let ((type (specifier-type (first args))))
	   (dolist (name (rest args))
	     (unless (symbolp name)
	       (error "Variable name is not a symbol: ~S." name))
	     (setf (info variable type name) type)
	     (setf (info variable where-from name) :declared)))))
      (ftype
       (when *type-system-initialized*
	 (let ((type (specifier-type (first args))))
	   (unless (csubtypep type (specifier-type 'function))
	     (error "Declared functional type is not a function type: ~S."
		    (first args)))
	   (dolist (name (rest args))
	     (define-function-name name)
	     (note-name-defined name :function)
	     (setf (info function type name) type)
	     (setf (info function where-from name) :declared)))))
      (freeze-type
       (dolist (type args)
	 (specifier-type type); Give undefined type warnings...
	 (when (eq (info type kind type) :structure)
	   (freeze-structure-type type))))
      (function
       ;;
       ;; Handle old-style FUNCTION declaration, which is a shorthand for
       ;; FTYPE.
       (when *type-system-initialized*
	 (if (and (<= 2 (length args) 3) (listp (second args)))
	     (%proclaim `(ftype (function . ,(rest args)) ,(first args)))
	     (%proclaim `(type function . ,args)))))
      (optimize
       (setq *default-cookie*
	     (process-optimize-declaration form *default-cookie*)))
      (optimize-interface
       (setq *default-interface-cookie*
	     (process-optimize-declaration form *default-interface-cookie*)))
      ((inline notinline maybe-inline)
       (dolist (name args)
	 (define-function-name name)
	 (setf (info function inlinep name)
	       (case kind
		 (inline :inline)
		 (notinline :notinline)
		 (maybe-inline :maybe-inline)))))
      (constant-function
       (let ((info (make-function-info
		    :attributes (ir1-attributes movable foldable flushable
						unsafe))))
	 (dolist (name args)
	   (define-function-name name)
	   (setf (info function info name) info))))
      (declaration
       (dolist (decl args)
	 (unless (symbolp decl)
	   (error "Declaration to be RECOGNIZED is not a symbol: ~S." decl))
	 (setf (info declaration recognized decl) t)))
      (t
       (if (member kind type-specifier-symbols)
	   (%proclaim `(type . ,form))
	   (error "Unrecognized proclamation: ~S." form)))))
  (undefined-value))
;;;
(setf (symbol-function 'proclaim) #'%proclaim)


;;; UNDEFINE-STRUCTURE  --  Interface
;;;
;;;    Blow away all the compiler info for the structure described by Info.
;;; This recursively descends the inheritance hierarchy.
;;; 
(defun undefine-structure (info)
  (declare (type defstruct-description info))
  (let* ((name (dd-name info))
	 (all-types (cons name (dd-included-by info))))
    ;;
    ;; Iterate over this type and all subtypes, clearing the compiler structure
    ;; type info, and undefining all the associated functions.
    (dolist (type all-types)
      (let ((this-info (info type structure-info type)))
	(setf (info type kind type) nil)
	(setf (info type structure-info type) nil)
	(undefine-function-name (dd-copier this-info))
	(undefine-function-name (dd-predicate this-info))
	(dolist (slot (dd-slots this-info))
	  (let ((fun (dsd-accessor slot)))
	    (undefine-function-name fun)
	    (unless (dsd-read-only slot)
	      (undefine-function-name `(setf ,fun)))))))
    ;;
    ;; Iterate over all types that include this type, removing this type and
    ;; all subtypes from the list of subtypes of the included type.  We copy
    ;; the DD and included list so that we don't clobber the type in the
    ;; compiler's Lisp.
    (dolist (include (dd-includes info))
      (let ((new (copy-defstruct-description
		  (info type structure-info include))))
	(setf (dd-included-by new)
	      (set-difference (dd-included-by new) all-types))
	(setf (info type structure-info include) new))))
  ;;
  ;; Clear out the SPECIFIER-TYPE cache so that subsequent references are
  ;; unknown types.
  (values-specifier-type-cache-clear)
  (undefined-value))


;;; DEFINE-DEFSTRUCT-NAME  --  Internal
;;;
;;;    Like DEFINE-FUNCTION-NAME, but we also set the kind to :DECLARED and
;;; blow away any ASSUMED-TYPE.  Also, if the thing is a slot accessor
;;; currently, quietly unaccessorize it.  And if there are any undefined
;;; warnings, we nuke them.
;;;
(defun define-defstruct-name (name)
  (when name
    (when (info function accessor-for name)
      (setf (info function accessor-for name) nil))
    (define-function-name name)
    (note-name-defined name :function)
    (setf (info function where-from name) :declared)
    (when (info function assumed-type name)
      (setf (info function assumed-type name) nil)))
  (undefined-value))


;;; FREEZE-STRUCTURE-TYPE  --  Internal
;;;
;;;    Freeze the named structure type and all its inferiors.
;;;
(defun freeze-structure-type (name)
  (let ((def (info type structure-info name)))
    (when def
      (setf (info type frozen name) t)
      (dolist (incl (dd-included-by def))
	(setf (info type frozen incl) t))))
  (undefined-value))


;;; CHECK-FOR-STRUCTURE-REDEFINITION  --  Internal
;;;
;;;    Called when we process a DEFSTRUCT for a type that is already defined
;;; for a structure.  We check for incompatible redefinition and undefine the
;;; old structure if so.  We ignore the structures that DEFSTRUCT is built out
;;; of, since they have to be hackishly defined in type-boot.  If the structure
;;; is not incompatibly redefined, then we copy the old INCLUDED-BY into the
;;; new structure.
;;;
(defun check-for-structure-redefinition (info)
  (declare (type defstruct-description info))
  (let* ((name (dd-name info))
	 (old (info type structure-info name)))
    (cond ((member name
		   '(defstruct-description defstruct-slot-description)))
	  ((and (equal (dd-includes old) (dd-includes info))
		(equalp (dd-slots old) (dd-slots info)))
	   (setf (dd-included-by info) (dd-included-by old)))
	  (t
	   (compiler-warning
	    "Incompatibly redefining structure ~S.~@
	    Removing the old definition~:[.~;~:* and these subtypes:~%  ~S~]"
	    name (dd-included-by old))
	   (undefine-structure old))))
  (undefined-value))


;;; ADD-NEW-SUBTYPE  --  Internal
;;;
;;;    Add a new subtype NAME to the structure type INC.  INFO is INC's current
;;; info.
;;;
(defun add-new-subtype (name inc info)
  (let ((new (copy-defstruct-description info)))
    (setf (info type structure-info inc) new)
    (push name (dd-included-by new))
    (when (info type frozen inc)
      (compiler-warning "Adding new subtype ~S to frozen type ~S.~@
      			 Unfreezing this type and its inferiors.~@
			 Previously compiled type tests must be recompiled."
			name inc)
      (setf (info type frozen inc) nil)
      (dolist (subtype (dd-included-by info))
	(setf (info type frozen subtype) nil)))))


;;; %%Compiler-Defstruct  --  Interface
;;;
;;;    This function updates the global compiler information to represent the
;;; definition of the the structure described by Info.  In addition to defining
;;; all the functions and slots, we also update the INCLUDED-BY info in the
;;; compiler's environment.  Note that at the first time the DEFSTRUCT is
;;; loaded, STRUCTURE-INFO is EQ to DEFINED-STRUCTURE-INFO, so the name will
;;; already be in INCLUDED-BY.  When we do update this info, we copy the
;;; defstruct description so that the type definition in the compiler's Lisp
;;; isn't trashed.
;;;
(defun %%compiler-defstruct (info)
  (declare (type defstruct-description info))
  (let ((name (dd-name info)))
    (ecase (info type kind name)
      ((nil))
      (:structure
       (check-for-structure-redefinition info))
      (:primitive
       (compiler-error "Illegal to redefine standard type ~S." name))
      (:defined
       (compiler-warning "Redefining DEFTYPE type to be a DEFSTRUCT: ~S."
			 name)
       (setf (info type expander name) nil)))
    
    (dolist (inc (dd-includes info))
      (let ((info (info type structure-info inc)))
	(unless info
	  (error "Structure type ~S is included by ~S but not defined."
		 inc name))
	(unless (member name (dd-included-by info))
	  (add-new-subtype name inc info))))
    
    (setf (info type kind name) :structure)
    (setf (info type structure-info name) info)
    (%note-type-defined name)
    
    (let ((copier (dd-copier info)))
      (when copier
	(%proclaim `(ftype (function (,name) ,name) ,copier))))
    
    (let ((pred (dd-predicate info)))
      (when pred
	(define-defstruct-name pred)
	(setf (info function inlinep pred) :inline)
	(setf (info function inline-expansion pred)
	      `(lambda (x) (typep x ',name))))))

  (dolist (slot (dd-slots info))
    (let* ((fun (dsd-accessor slot))
	   (setf-fun `(setf ,fun)))
      (define-defstruct-name fun)
      (setf (info function accessor-for fun) info)
      (unless (dsd-read-only slot)
	(define-defstruct-name setf-fun)
	(setf (info function accessor-for setf-fun) info))))
  (undefined-value))

(setf (symbol-function '%compiler-defstruct) #'%%compiler-defstruct)


;;; %NOTE-TYPE-DEFINED  --  Interface
;;;
;;;    Note that the type Name has been (re)defined, updating the undefined
;;; warnings and VALUES-SPECIFIER-TYPE cache.
;;; 
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (when (boundp '*values-specifier-type-cache-vector*)
    (values-specifier-type-cache-clear))
  (undefined-value))


;;;; Dummy definitions of COMPILER-ERROR, etc.
;;;
;;;    Until the compiler is properly loaded, we make the compiler error
;;; functions synonyms for the obvious standard error function.
;;;

(defun compiler-error (string &rest args)
  (apply #'error string args))

(defun compiler-warning (string &rest args)
  (apply #'warn string args))

(defun compiler-note (string &rest args)
  (apply #'warn string args))

(defun compiler-error-message (string &rest args)
  (apply #'warn string args))


;;; Alien=>Lisp-Transform  --  Internal
;;;
;;;    This is the transform for alien-operators and other alien-valued
;;; things which may be evaluated normally to yield an alien-value structure.
;;;
(defun alien=>lisp-transform (form)
  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil form)
    `(let* ,(reverse binds)
       ,(ignore-unreferenced-vars binds)
       ,@(nreverse stuff)
       ,(if (ct-a-val-alien res)
	    (ct-a-val-alien res)
	    `(lisp::make-alien-value
	      ,(ct-a-val-sap res)
	      ,(ct-a-val-offset res)
	      ,(ct-a-val-size res)
	      ',(ct-a-val-type res))))))
