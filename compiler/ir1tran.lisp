;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains code which does the translation from Lisp code to the
;;; first intermediate representation (IR1).
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(export '(*compile-time-define-macros* *compiling-for-interpreter*))

(in-package 'ext)
(export '(ignorable truly-the maybe-inline))

(in-package 'c)


(proclaim '(special *compiler-error-bailout* *template-names*))

;;; The *fenv* is used to keep track of the meaning of the names of functional
;;; objects during IR1 translation.  It is an alist with the car of each entry
;;; being the name of the function.  If the cdr is a Leaf structure, then the
;;; name is bound to the function it represents.  If the cdr is 
;;; (macro . (lambda ...)) then the lambda is the macroexpansion function for a
;;; local macro.
;;;
(defvar *fenv*)

;;; *inlines* is used to keep track of which functions may and may not be coded
;;; as inline functions.  It is an alist (Leaf . Flag), where Leaf is the
;;; structure that represents the function and Flag is either :Inline or
;;; :Notinline, indicating the sense of the declaration.
;;;
(defvar *inlines*)

;;; *type-restrictions* is an alist (Leaf . CType) which is used to keep track
;;; of "pervasive" type declarations.  A pervasive type declaration is a type
;;; declaration that pertains to the type in a syntactic extent which does not
;;; correspond to a binding of the affected name.  Currently Common Lisp only
;;; allows pervasive type declarations on variables in the function namespace.
;;;
(defvar *type-restrictions*)

;;; *venv* translates from variable names to Leaf structures.  A special
;;; binding is indicated by a :Special Global-Var leaf.  Each special binding
;;; within the code gets a distinct var structure, as does the current "global"
;;; value on entry to the code compiled.  (locally (special ...)) is handled by
;;; adding the most recent special binding to the front of the list.
;;;
(defvar *venv*)

;;; *benv* and *tenv* are alists from block and go-tag names to 2-lists of the
;;; form (<entry> <continuation>), where <continuation> is the continuation to
;;; exit to, and <entry> is the corresponding Entry node.
;;;
(defvar *benv*)
(defvar *tenv*)
(proclaim '(list *fenv* *inlines* *type-restrictions* *venv* *benv* *tenv*))

;;; *free-variables* translates from the names of variables referenced globally
;;; to the Leaf structures for them.  *free-functions* is like
;;; *free-variables*, only it deals with function names.
;;;
;;; We must preserve the property that a proclamation for a global thing
;;; only affects the code after it.  This takes some work, since a proclamation
;;; may appear in the middle of a block being compiled.  If there are
;;; references before the proclaim, then we copy the current entry before
;;; modifying it.  Code converted before the proclaim sees the old Leaf, while
;;; code after it sees the new Leaf.
;;;
(defvar *free-variables*)
(defvar *free-functions*)
(proclaim '(hash-table *free-variables* *free-functions*))

;;; We use the same Constant structure to represent all equal anonymous
;;; constants.  This hashtable translates from constants to the Leafs that
;;; represent them.
;;;
(defvar *constants*)
(proclaim '(hash-table *constants*))

;;; *Current-Cookie* represents locally bound state information; declaration
;;; processing binds it.
;;;
(proclaim '(type cookie *current-cookie*))
(defvar *current-cookie*)

;;; *source-paths* is a hashtable from source code forms to the path taken
;;; through the source to reach the form.  This provides a way to keep track of
;;; the location of original source forms, even when macroexpansions and other
;;; arbitary permutations of the code happen.  This table is initialized by
;;; calling Find-Source-Paths on the original source.
;;;
(proclaim '(hash-table *source-paths*))
(defvar *source-paths*)

;;; *current-lambda* is used to keep track of the function currently being
;;; translated.  Blocks point to the current lambda so that we can find the
;;; environment that they run in.
;;;
(proclaim '(type (or clambda null) *current-lambda*))
(defvar *current-lambda*)

;;; *Current-Component* is the Component structure which we link blocks into as
;;; we generate them.  This just serves to glue the emitted blocks together
;;; until local call analysis and flow graph canonicalization figure out what
;;; is really going on.  We need to keep track of all the blocks generated so
;;; that we can delete them if they turn out to be unreachable.
;;;
(proclaim '(type (or component null) *current-component*))
(defvar *current-component*)

;;; *Current-Cleanup* is similar to *Current-Lambda*, but used to keep track
;;; of the enclosing Cleanup.  If there is no cleanup, then this is the same as
;;; *Current-Lambda*.
;;;
(proclaim '(type (or cleanup clambda null) *current-cleanup*))
(defvar *current-cleanup*)

;;; *Current-Path* is the source path of the form we are currently translating.
;;; If the path for the form is unknown, then this is the path for the
;;; innermost enclosing form for which the path is known.
;;;
(proclaim '(list *current-path*))
(defvar *current-path* nil)

;;; *Current-Form* is the form that is currently being converted into IR1.
;;; This is used to provide context for errors found during IR1 conversion.
;;;
(defvar *current-form* nil)

;;; *Converting-For-Interpreter* is true when we are creating IR1 to be
;;; interpreted rather than compiled.  This inhibits source tranformations and
;;; stuff.
;;;
(defvar *converting-for-interpreter* nil)

;;; *Compile-Time-Define-Macros* is true when we want DEFMACRO definitions to
;;; be installed in the compilation environment as interpreted functions.  We
;;; set this to false when compiling some parts of the system.
;;;
(defvar *compile-time-define-macros* t)


;;; IR1-Error-Bailout  --  Internal
;;;
;;;    Bind *compiler-error-bailout* to a function throws out of the body and
;;; converts a proxy form instead.
;;;
(defmacro ir1-error-bailout ((start cont form) &body body)
  `(catch 'ir1-error-abort 
     (let ((*bailout-start* ,start)
	   (*bailout-cont* ,cont)
	   (*bailout-form* ,form)
	   (*compiler-error-bailout*
	    #'(lambda ()
		(ir1-convert
		 *bailout-start* *bailout-cont*
		 `(error "Execution of a form compiled with errors:~% ~S"
			 ',*bailout-form*))
		(throw 'ir1-error-abort nil))))
       (declare (special *bailout-start* *bailout-cont* *bailout-form*))
       ,@body
       nil)))

			
;;; IR1-Convert  --  Interface
;;;
;;;    Translate Form into IR1.  The code is inserted as the Next of the
;;; continuation Start.  Cont is the continuation which receives the value of
;;; the Form to be translated.  The translators call this function recursively
;;; to translate their subnodes.
;;;
;;;    As a special hack to make life easier in the compiler, a Leaf
;;; IR1-converts into a reference to that leaf structure.  This allows the
;;; creation using backquote of forms that contain leaf references, without
;;; having to introduce dummy names into the namespace.
;;;
(proclaim '(function ir1-convert (continuation continuation t) void))
(defun ir1-convert (start cont form)
  (ir1-error-bailout (start cont form)
    (if (atom form)
	(cond ((and (symbolp form) (not (keywordp form)))
	       (ir1-convert-variable start cont form))
	      ((leaf-p form)
	       (reference-leaf start cont form nil))
	      ((constantp form)
	       (reference-constant start cont form form))
	      (t
	       (compiler-error "Cannot evaluate this object: ~S" form)))
	(let ((fun (car form))
	      (*current-form* form)
	      (*current-path* (or (gethash form *source-paths*)
				  *current-path*)))
	  (cond
	   ((symbolp fun)
	    (let ((lexical-def (cdr (assoc fun *fenv*))))
	      (cond
	       ((not lexical-def)
		(ir1-convert-global-functoid start cont form))
	       ((leaf-p lexical-def)
		(ir1-convert-local-function start cont form lexical-def))
	       ((and (consp lexical-def) (eq (car lexical-def) 'macro))
		(ir1-convert-macro start cont (cdr lexical-def) form))
	       (t
		(error "Malformed *fenv* entry: ~S." lexical-def)))))
	   ((or (atom fun) (not (eq (car fun) 'lambda)))
	    (compiler-error "Illegal function call."))
	   (t
	    (ir1-convert-combination start cont form
				     (ir1-convert-lambda fun))))))))

;;; IR1-Convert-Global-Functoid  --  Internal
;;;
;;;    Convert anything that looks like a special-form, global function or
;;; macro call.  If the thing has a ir1-convert method, but we are at top level
;;; and the method can't be used there, then convert the form in the initial
;;; component.
;;;
(defun ir1-convert-global-functoid (start cont form)
  (declare (type continuation start cont)
	   (list form))
  (let* ((fun (first form))
	 (translator (info function ir1-convert fun)))
    (if translator
	(funcall translator start cont form)
	(ecase (info function kind fun)
	  (:macro
	   (let ((expander (info function macro-function fun)))
	     (assert expander (expander)
		     "No macro-function for global macro ~S." fun)
	     (ir1-convert-macro start cont expander form)))
	  ((nil :function)
	   (ir1-convert-global-function start cont form)))))
  (undefined-value))


;;; IR1-Convert-Macro  --  Internal
;;;
;;;    Trap errors during the macroexpansion.
;;;
(defun ir1-convert-macro (start cont fun form)
  (declare (type continuation start cont))
  (ir1-convert start cont
	       (handler-case (funcall fun form *fenv*)
		 (error (condition)
		   (compiler-error "(during macroexpansion)~%~A"
				   condition)))))


;;; Leaf-Inlinep  --  Internal
;;;
;;;    Return the current Inlinep value for references to Leaf.
;;;
(defun leaf-inlinep (leaf)
  (declare (type leaf leaf))
  (let ((found (assoc leaf *inlines*)))
    (if found
	(cdr found)
	(etypecase leaf
	  (functional nil)
	  (global-var
	   (assert (eq (global-var-kind leaf) :global-function))
	   (info function inlinep (leaf-name leaf)))))))


;;; IR1-Convert-Local-Function  --  Internal
;;;
;;;    Convert a call to a local function.  If speed is important, we a have an
;;; inline expansion and the function is :inline, then convert the inline
;;; expansion instead of a reference to the existing function.
;;;
(proclaim '(function ir1-convert-local-function 
		     (continuation continuation t leaf) void))
(defun ir1-convert-local-function (start cont form var)
  (let ((inlinep (leaf-inlinep var))
	(expansion (if (functional-p var)
		       (functional-inline-expansion var))))
    (if (and expansion (eq inlinep :inline)
	     (policy nil (>= speed space) (>= speed cspeed)))
	(ir1-convert-combination start cont form 
				 (let ((*fenv* (functional-fenv var))
				       (*venv* (functional-venv var))
				       (*tenv* (functional-tenv var))
				       (*benv* (functional-benv var)))
				   (ir1-convert-lambda expansion form))
				 :inline)
	(ir1-convert-combination start cont form var inlinep))))


;;; IR1-Convert-Combination  --  Internal
;;;
;;;    Convert a function call where the function is a Leaf.  Inlinep is the
;;; value of Inlinep for the Ref.  We return the Combination node so that we
;;; can poke at it if we want to.
;;;
(proclaim '(function ir1-convert-combination
		     (continuation continuation list leaf &optional inlinep)
		     combination))
(defun ir1-convert-combination (start cont form fun &optional (inlinep nil))
  (let ((fun-cont (make-continuation)))
    (reference-leaf start fun-cont fun inlinep)
    (ir1-convert-combination-args fun-cont cont form)))


;;; IR1-Convert-Combination-Args  --  Internal
;;;
;;;    Convert the arguments to a call and make the Combination node.  Fun-Cont
;;; is the continuation which yields the function to call.  Form is the source 
;;; for the call.  Source is the source for the call.  Args is the list of
;;; arguments for the call, which defaults to the cdr of source.  We return the
;;; Combination node.
;;;
(proclaim '(function ir1-convert-combination-args
		     (continuation continuation list &optional list)
		     combination))
(defun ir1-convert-combination-args (fun-cont cont source &optional (args (cdr source)))
  (let ((node (make-combination source fun-cont)))
    (setf (continuation-dest fun-cont) node)
    (assert-continuation-type fun-cont
			      (specifier-type '(or function symbol)))
    (collect ((arg-conts))
      (let ((this-start fun-cont))
	(dolist (arg args)
	  (let ((this-cont (make-continuation node)))
	    (ir1-convert this-start this-cont arg)
	    (setq this-start this-cont)
	    (arg-conts this-cont)))
	(prev-link node this-start)
	(use-continuation node cont)
	(setf (combination-args node) (arg-conts))))
    node))


;;; IR1-Convert-Progn-Body  --  Internal
;;;
;;;    Convert a bunch of forms, discarding all the values except the last.
;;; If there aren't any forms, then translate a NIL.
;;;
(proclaim '(function ir1-convert-progn-body (continuation continuation list) void))
(defun ir1-convert-progn-body (start cont body)
  (if (endp body)
      (reference-constant start cont nil nil)
      (let ((this-start start)
	    (forms body))
	(loop
	  (let ((form (car forms)))
	    (when (endp (cdr forms))
	      (ir1-convert this-start cont form)
	      (return))
	    (let ((this-cont (make-continuation)))
	      (ir1-convert this-start this-cont form)
	      (setq this-start this-cont  forms (cdr forms))))))))


;;; IR1-Convert-Global-Function  --  Internal
;;;
;;;    Convert a call to a global function.  If the function has a
;;; source-transform and inline expansion is enabled then we convert its
;;; expansion.  If the source transform returns a non-null second value, then
;;; we act as though there was no source transformation, and directly convert
;;; the call.
;;;
(proclaim '(function ir1-convert-global-function (continuation continuation list) void))
(defun ir1-convert-global-function (start cont form)
  (let ((name (car form)))
    (multiple-value-bind (var inlinep)
			 (find-free-function name "in a reasonable place")
      (cond
       ((eq inlinep :notinline)
	(ir1-convert-combination start cont form var inlinep))
       (*converting-for-interpreter*
	(ir1-convert-ok-combination-fer-sher start cont form var))
       (t
	(let ((transform (info function source-transform name))
	      (expansion (info function inline-expansion name)))
	  (cond
	   (transform
	    (multiple-value-bind (result pass)
				 (funcall transform form)
	      (if pass
		  (ir1-convert-ok-combination start cont form var)
		  (ir1-convert start cont result))))
	   (expansion
	    (ir1-convert-global-inline start cont form var inlinep expansion))
	   (t
	    (when (and (eq inlinep :inline) (policy nil (> speed brevity)))
	      (compiler-note "~S is declared globally inline, but has no expansion."
			     name))
	    (ir1-convert-ok-combination start cont form var)))))))))


;;; IR1-Convert-Ok-Combination  --  Internal
;;;
;;;    Convert a global function call that we are allowed to early bind.  We
;;; find any Function-Info for Var.  Although Var is not necessarily a
;;; Global-Var, it is in the global namespace, so we can assume that we know
;;; about it if we recognize the name.
;;;
;;;    If the function has the Predicate attribute, and the CONT's DEST isn't
;;; an IF, then we convert (IF <form> T NIL), ensuring that a predicate always
;;; appears in a conditional context.
;;;
;;;    If the function isn't a predicate, then we call
;;; IR1-Convert-OK-Combination-Fer-Sher.
;;;
(defun ir1-convert-ok-combination (start cont form var)
  (declare (type continuation start cont) (list form) (type leaf var))
  (let ((info (info function info (leaf-name var))))
    (if (and info
	     (ir1-attributep (function-info-attributes info) predicate)
	     (not (if-p (continuation-dest cont))))
	(ir1-convert start cont `(if ,form t nil))
	(ir1-convert-ok-combination-fer-sher start cont form var))))


;;; IR1-Convert-OK-Combination-Fer-Sher  --  Internal
;;;
;;;    Actually really convert a global function call that we are allowed to
;;; early-bind.
;;;
;;; If we know the function type of the function, then we check the call for
;;; syntactic legality with respect to the declared function type.  If it is
;;; impossible to determine whether the call is correct due to non-constant
;;; keywords, then we give up, marking the Ref as :Notinline to inhibit further
;;; error messages.  We return true when the call is legal.
;;;
;;; If the call is legal, we also propagate type assertions from the function
;;; type to the arg and result continuations.  We do this now so that IR1
;;; optimize doesn't have to redundantly do the check later so that it can do
;;; the type propagation.
;;;
;;; If the function is unknown, then we note the name and error context so that
;;; we can give a warning if the function is never defined.
;;;
(defun ir1-convert-ok-combination-fer-sher (start cont form var)
  (declare (type continuation start cont) (list form) (type leaf var))
  (let ((fun-cont (make-continuation)))
    (reference-leaf start fun-cont var nil)
    (let ((type (leaf-type var))
	  (node (ir1-convert-combination-args fun-cont cont form)))
      (cond
       ((eq (leaf-where-from var) :assumed)
	(let ((name (leaf-name var)))
	  (when (and (eq (info function where-from name) :assumed)
		     (eq (info function kind name) :function))
	    (let ((*compiler-error-context* node))
	      (note-undefined-reference name :function))
	    (setf (info function assumed-type name)
		  (note-function-use node
				     (info function assumed-type name)))))
	nil)
       ((not (function-type-p type)) nil)
       ((valid-function-use node type
			    :argument-test #'always-subtypep
			    :result-test #'always-subtypep
			    :error-function #'compiler-warning
			    :warning-function #'compiler-note)
	(recognize-known-call node)
	(assert-call-type node type)
	(setf (continuation-%derived-type fun-cont) type)
	(setf (continuation-reoptimize fun-cont) nil)
	(setf (continuation-%type-check fun-cont) nil)
	t)
       (t
	(setf (ref-inlinep (continuation-use fun-cont)) :notinline)
	nil)))))


;;; In-Null-Environment  --  Internal
;;;
;;;    Return true if the lexical environment is null.  If Macros-OK is true,
;;; then it is ok for there there to be local macros and other compile-time
;;; stuff in the environment.
;;;
(defun in-null-environment (&optional macros-ok)
  (and (if macros-ok
	   (every #'(lambda (x)
		      (let ((val (cdr x)))
			(and (consp val)
			     (eq (car val) 'macro))))
		  *fenv*)
	   (null *fenv*))
       (null *benv*) (null *venv*) (null *tenv*)))


;;; IR1-Convert-Global-Lambda  --  Internal
;;;
;;;    Like IR1-Convert-Lambda except that we null out the environment
;;; variables around the conversion.
;;;
(proclaim '(function ir1-convert-global-lambda (t t) functional))
(defun ir1-convert-global-lambda (fun source)
  (let ((*fenv* ())
	(*benv* ())
	(*venv* ())
	(*tenv* ()))
    (ir1-convert-lambda fun source)))


;;; IR1-Convert-Global-Inline  --  Internal
;;;
;;;    Convert a call to a global function which has an inline expansion.  We
;;; make a number of speed v.s. space policy decisions using information from
;;; our extended inline declaration.  If space is more important than speed,
;;; then we never do anything funny unless the function is :Always-Inline, in
;;; which case we do a standard inline substitution.  Otherwise, we do a
;;; standard inline expansion if it is :Inline or a semi-inline call if it is
;;; :Semi-Inline.
;;;
(proclaim '(function ir1-convert-global-inline
		     (continuation continuation t leaf inlinep list)
		     void))
(defun ir1-convert-global-inline (start cont form var inlinep expansion)
  (if (and (policy nil
		   (>= speed cspeed)
		   (or (> speed space)
		       (and (= speed space) (eq inlinep :inline))))
	   (not (functional-p var)))
      (let* ((name (leaf-name var))
	     (dummy (make-functional :name name)))
	(setf (gethash name *free-functions*) dummy)
	(let ((fun (ir1-convert-global-lambda expansion form)))
	  (setf (leaf-name fun) name)
	  (substitute-leaf fun dummy)
	  (setf (gethash name *free-functions*)
		(if (or (eq inlinep :inline)
			(policy nil (= space 0)))
		    var
		    fun))
	  (ir1-convert-combination start cont form fun)))
      (ir1-convert-ok-combination start cont form var)))


;;;; Lambda hackery:  

;;; Varify-Lambda-Arg  --  Internal
;;;
;;;    Verify that a thing is a legal name for a variable and return a Var
;;; structure for it, filling in info if it is globally special.  If it is
;;; losing, we punt with a Compiler-Error.  Names-So-Far is an alist of names
;;; which have previously been bound.  If the name is in this list, then we
;;; error out.
;;;
(proclaim '(function varify-lambda-arg (t list) var))
(defun varify-lambda-arg (name names-so-far)
  (unless (symbolp name)
    (compiler-error "Lambda-variable is not a symbol: ~S." name))
  (when (member name names-so-far)
    (compiler-error "Repeated variable in lambda-list: ~S." name))
  (let ((kind (info variable kind name)))
    (when (or (keywordp name) (eq kind :constant))
      (compiler-error "Name of lambda-variable is a constant: ~S." name))
    (if (eq kind :special)
	(let ((specvar (find-free-variable name)))
	  (make-lambda-var :name name
			   :type (leaf-type specvar)
			   :where-from (leaf-where-from specvar)
			   :specvar specvar))
	(make-lambda-var :name name))))


;;; Make-Keyword  --  Internal
;;;
;;;    Make the keyword for a keyword arg, checking that the keyword isn't
;;; already used by one of the Vars.  We also check that the keyword isn't the
;;; magical :allow-other-keys.
;;;
(proclaim '(function make-keyword (symbol list) keyword))
(defun make-keyword (symbol vars)
  (let ((key (if (keywordp symbol) symbol
		 (intern (symbol-name symbol) "KEYWORD"))))
    (when (eq key :allow-other-keys)
      (compiler-error "You can't have a keyword arg called :allow-other-keys."))
    (dolist (var vars)
      (let ((info (lambda-var-arg-info var)))
	(when (and info
		   (eq (arg-info-kind info) :keyword)
		   (eq (arg-info-keyword info) key))
	  (compiler-error "Multiple uses of keyword ~S in lambda-list." key))))
    key))


;;; Find-Lambda-Vars  --  Internal
;;;
;;;    Parse a lambda-list into a list of Var structures, stripping off any aux
;;; bindings.  Each arg name is checked for legality, and duplicate names are
;;; checked for.  If an arg is globally special, the var is marked as :special
;;; instead of :lexical.  Keyword, optional and rest args are annotated with an
;;; arg-info structure which contains the extra information.  If we hit
;;; something losing, we bug out with Compiler-Error.  These values are
;;; returned:
;;;  1] A list of the var structures for each top-level argument.
;;;  2] A flag indicating whether &key was specified.
;;;  3] A flag indicating whether other keyword args are allowed.
;;;  4] A list of the &aux variables.
;;;  5] A list of the &aux values.
;;;
(proclaim '(function find-lambda-vars (list) (values list boolean list list)))
(defun find-lambda-vars (list)
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
		       (parse-lambda-list list)
    (collect ((vars)
	      (names-so-far)
	      (aux-vars)
	      (aux-vals))
      ;;
      ;; Parse-Default deals with defaults and supplied-p args for optionals
      ;; and keywords args.
      (flet ((parse-default (spec info)
	       (when (consp (cdr spec))
		 (setf (arg-info-default info) (second spec))
		 (when (consp (cddr spec))
		   (let* ((supplied-p (third spec))
			  (supplied-var (varify-lambda-arg supplied-p (names-so-far))))
		     (setf (arg-info-supplied-p info) supplied-var)
		     (names-so-far supplied-p)
		     (when (> (length spec) 3)
		       (compiler-error "Arg specifier is too long: ~S." spec)))))))
	
	(dolist (name required)
	  (let ((var (varify-lambda-arg name (names-so-far))))
	    (vars var)
	    (names-so-far name)))
	
	(dolist (spec optional)
	  (if (atom spec)
	      (let ((var (varify-lambda-arg spec (names-so-far))))
		(setf (lambda-var-arg-info var) (make-arg-info :kind :optional))
		(vars var)
		(names-so-far spec))
	      (let* ((name (first spec))
		     (var (varify-lambda-arg name (names-so-far)))
		     (info (make-arg-info :kind :optional)))
		(setf (lambda-var-arg-info var) info)
		(vars var)
		(names-so-far name)
		(parse-default spec info))))
	
	(when restp
	  (let ((var (varify-lambda-arg rest (names-so-far))))
	    (setf (lambda-var-arg-info var) (make-arg-info :kind :rest))
	    (vars var)
	    (names-so-far rest)))
	
	(dolist (spec keys)
	  (cond ((atom spec)
		 (let ((var (varify-lambda-arg spec (names-so-far))))
		   (setf (lambda-var-arg-info var)
			 (make-arg-info :kind :keyword
					:keyword (make-keyword spec (vars))))
		   (vars var)
		   (names-so-far spec)))
		((atom (first spec))
		 (let* ((name (first spec))
			(var (varify-lambda-arg name (names-so-far)))
			(info (make-arg-info :kind :keyword
					     :keyword (make-keyword name (vars)))))
		   (setf (lambda-var-arg-info var) info)
		   (vars var)
		   (names-so-far name)
		   (parse-default spec info)))
		(t
		 (let ((head (first spec)))
		   (unless (= (length head) 2)
		     (error "Malformed keyword arg specifier: ~S." spec))
		   (let* ((name (second head))
			  (var (varify-lambda-arg name (names-so-far)))
			  (info (make-arg-info :kind :keyword
					       :keyword (make-keyword (first head) (vars)))))
		     (setf (lambda-var-arg-info var) info)
		     (vars var)
		     (names-so-far name)
		     (parse-default spec info))))))
	
	(dolist (spec aux)
	  (cond ((atom spec)
		 (let ((var (varify-lambda-arg spec (names-so-far))))
		   (aux-vars var)
		   (aux-vals nil)
		   (names-so-far spec)))
		(t
		 (unless (<= 1 (length spec) 2)
		   (compiler-error "Malformed &aux binding specifier: ~S." spec))
		 (let* ((name (first spec))
			(var (varify-lambda-arg name (names-so-far))))
		   (aux-vars var)
		   (aux-vals (second spec))
		   (names-so-far name)))))
	  
	(values (vars) keyp allowp (aux-vars) (aux-vals))))))


;;; Find-In-Bindings  --  Internal
;;;
;;;    Given a list of Lambda-Var structures and a variable name, return the
;;; structure for that name, or NIL if it isn't found.
;;;
(proclaim '(function find-in-bindings (list symbol) (or lambda-var null)))
(defun find-in-bindings (vars name)
  (dolist (var vars)
    (when (eq (leaf-name var) name) (return var))
    (let ((info (lambda-var-arg-info var)))
      (when info
	(let ((supplied-p (arg-info-supplied-p info)))
	  (when (and supplied-p
		     (eq (leaf-name supplied-p) name))
	    (return supplied-p)))))))


;;; Find-Lexically-Apparent-Function  --  Internal
;;;
;;;    Return the Leaf structure for the lexically apparent function definition
;;; of Name.  The second value is the inlinep information which currently
;;; applies to the variable.
;;;
(proclaim '(function find-lexically-apparent-function (symbol string)
		     (values var inlinep)))
(defun find-lexically-apparent-function (name context)
  (let ((var (cdr (assoc name *fenv* :test #'equal))))
    (cond (var
	   (unless (leaf-p var)
	     (assert (and (consp var) (eq (car var) 'macro)) ()
		     "Strange *fenv* entry: ~S." var)
	     (compiler-error "Found macro name ~S ~A." name context))
	   (values var (leaf-inlinep var)))
	  (t
	   (find-free-function name context)))))


;;; Process-Type-Declaration  --  Internal
;;;
;;;    Called by Process-Declarations to deal with a variable type declaration.
;;;
(proclaim '(function process-type-declaration (list list) void))
(defun process-type-declaration (decl vars)
  (let ((type (specifier-type (first decl))))
    (dolist (var-name (rest decl))
      (let ((var (find-in-bindings vars var-name)))
	(cond
	 ((not var)
	  (compiler-warning "Type declaration for unbound variable: ~S."
			    var-name))
	 ((or (function-type-p type)
	      (function-type-p (leaf-type var)))
	  ;;
	  ;; ### For now, just quietly set type...
	  (setf (leaf-type var) type))
	 (t
	  (let ((int (type-intersection (leaf-type var) type)))
	    (if (eq int *empty-type*)
		(compiler-warning
		 "Conflicting type declarations ~S and ~S for ~S."
		 (type-specifier (leaf-type var))
		 (type-specifier type) var-name)
		(setf (leaf-type var) int)))))))))


;;; Process-Ftype-Declaration  --  Internal
;;;
;;;    Somewhat similar to Process-Type-Declaration, but handles declarations
;;; for function variables.  In addition to allowing declarations for functions
;;; being bound, we must also deal with declarations that constrain the type of
;;; lexically apparent functions.
;;;
;;; [In the non-pervasive case, we should propagate type constraints into the
;;; function getting the declaration.  We should also think about checking for
;;; incompatible declarations and possibly intersecting the declared types.
;;; Handling of pervasive declarations is also sub-optimal, but with any luck
;;; this feature will go away.]
;;;
(proclaim '(function process-ftype-declaration (t list list) list))
(defun process-ftype-declaration (spec names fvars)
  (let ((type (specifier-type spec)))
    (collect ((res))
      (dolist (name names)
	(let ((found (find name fvars :key #'leaf-name)))
	  (if found
	      (setf (leaf-type found) type)
	      (res (cons (find-lexically-apparent-function
			  name "in a function type declaration")
			 type)))))
      (res))))



;;; Process-Declarations  --  Internal
;;;
;;;    Use a list of Declare forms to annotate the lists of Lambda-Var and
;;; Functional structures which are being bound.  In addition to filling in
;;; slots in the leaf structures, we return new values for *venv*, *inlines*,
;;; *type-restrictions* and *current-cookie* which reflect pervasive special
;;; and function type declarations, (not)inline declarations and optimize
;;; declarations.
;;;
(proclaim '(function process-declarations (list list list) (values list list list cookie)))
(defun process-declarations (decls vars fvars)
  (let ((new-cookie *current-cookie*))
    (collect ((new-venv nil cons)
	      (new-inlines nil cons)
	      (new-restrictions *type-restrictions* nconc))
      (dolist (decl decls)
	(dolist (spec (rest decl))
	  (unless (consp spec)
	    (compiler-error "Malformed declaration specifier ~S in ~S." spec decl))
	  (case (first spec)
	    (special
	     (dolist (name (cdr spec))
	       (let ((var (find-in-bindings vars name))
		     (specvar (specvar-for-binding name)))
		 (cond (var
			(when (lambda-var-ignorep var)
			  (compiler-warning "Ignored variable ~S is being declared special." name))
			(setf (lambda-var-specvar var) specvar))
		       ((assoc name (new-venv)))
		       (t
			(new-venv (cons name specvar)))))))
	    (ftype
	     (unless (cdr spec)
	       (compiler-error "No type specified in FTYPE declaration: ~S." decl))
	     (new-restrictions
	      (process-ftype-declaration (second spec) (cddr spec) fvars)))
	    (function
	     (unless (cdr spec)
	       (compiler-error "No function name specified in FUNCTION declaration: ~S." decl))
	     (new-restrictions
	      (process-ftype-declaration `(function ,@(cddr spec)) (list (second spec))
					 fvars)))
	    ((inline notinline maybe-inline)
	     (let ((sense (case (first spec)
			    (inline :inline)
			    (notinline :notinline)
			    (maybe-inline :maybe-inline))))
	       (dolist (name (rest spec))
		 (let* ((var (or (find name fvars :key #'leaf-name)
				 (find-lexically-apparent-function
				  name "in an inline or notinline declaration")))
			(found (cdr (assoc var (new-inlines)))))
		   (if found
		       (unless (eq found sense)
			 (compiler-warning "Conflicting inline/notinline declarations in ~S." decl))
		       (new-inlines (cons var sense)))))))
	    ((ignore ignorable)
	     (dolist (name (rest spec))
	       (let ((var (find-in-bindings vars name)))
		 (cond
		  ((not var)
		   (compiler-warning
		    "Ignore declaration for unknown variable ~S." name))
		  ((lambda-var-specvar var)
		   (compiler-warning
		    "Declaring special variable ~S to be ignored." name))
		  ((eq (first spec) 'ignorable)
		   (setf (leaf-ever-used var) t))
		  (t
		   (setf (lambda-var-ignorep var) t))))))
	    (optimize
	     (setq new-cookie (process-optimize-declaration spec new-cookie)))
	    (type
	     (process-type-declaration (cdr spec) vars))
	    (t
	     (let ((what (first spec)))
	       (cond ((member what type-specifier-symbols)
		      (process-type-declaration spec vars))
		     ((info declaration recognized what))
		     (t
		      (compiler-warning "Unrecognized declaration: ~S." spec))))))))

      (values (nconc (new-venv) *venv*) (nconc (new-inlines) *inlines*)
	      (new-restrictions) new-cookie))))


;;; Specvar-For-Binding  --  Internal
;;;
;;;    Return the Specvar for Name to use when we see a local SPECIAL
;;; declaration.  If there is a global variable of that name, then check that
;;; it isn't a constant and return it.  Otherwise, create an anonymous
;;; GLOBAL-VAR.
;;;
(defun specvar-for-binding (name)
  (cond ((not (eq (info variable where-from name) :assumed))
	 (let ((found (find-free-variable name)))
	   (when (or (not (global-var-p found))
		     (eq (global-var-kind found) :constant))
	     (compiler-error "Declaring a constant to be special: ~S." name))
	   found))
	(t
	 (make-global-var :kind :special  :name name  :where-from :declared))))


;;; IR1-Convert-Aux-Bindings  --  Internal
;;;
;;;    Similar to IR1-Convert-Progn-Body except that we sequentially bind each
;;; Aux-Var to the corresponding Aux-Val before converting the body.  If there
;;; are no bindings, just convert the body, otherwise do one binding and
;;; recurse on the rest.
;;;
(proclaim '(function ir1-convert-aux-bindings
		     (continuation continuation list list list)
		     void))
(defun ir1-convert-aux-bindings (start cont body source aux-vars aux-vals)
  (if (null aux-vars)
      (ir1-convert-progn-body start cont body)
      (let ((fun-cont (make-continuation))
	    (fun (ir1-convert-lambda-body body (list (first aux-vars)) source
					  (rest aux-vars) (rest aux-vals))))
	(reference-leaf start fun-cont fun nil)
	(ir1-convert-combination-args fun-cont cont source
				      (list (first aux-vals))))))


;;; IR1-Convert-Special-Bindings  --  Internal
;;;
;;;    Similar to IR1-Convert-Progn-Body except that code to bind the Specvar
;;; for each Svar to the value of the variable is wrapped around the body.  If
;;; there are no special bindings, we just convert the body, otherwise we do
;;; one special binding and recurse on the rest.
;;;
;;;    We make a cleanup, bind *Current-Cleanup* to it, and also set the
;;; Block-End-Cleanup for the start block.  If there are multiple special
;;; bindings, the cleanup for the blocks will end up being the innermost one.
;;; We force Cont to start a block outside of this cleanup, causing cleanup
;;; code to be emitted when the scope is exited.
;;;
(defun ir1-convert-special-bindings (start cont body aux-vars aux-vals
					   svars source)
  (declare (type continuation start cont)
	   (list body aux-vars aux-vals svars))
  (cond
   ((null svars)
    (ir1-convert-aux-bindings start cont body source aux-vars aux-vals))
   (t
    (continuation-starts-block cont)
    (let* ((cleanup (make-cleanup :kind :special-bind))
	   (*current-cleanup* cleanup)
	   (var (first svars))
	   (next-cont (make-continuation)))
      (setf (cleanup-start cleanup) next-cont)
      (setf (block-end-cleanup (continuation-block start)) cleanup)
      (ir1-convert start next-cont
		   `(%special-bind ',(lambda-var-specvar var) ,var))
      (ir1-convert-special-bindings next-cont cont body aux-vars aux-vals
				    (rest svars) source)))))


;;; IR1-Convert-Lambda-Body  --  Internal
;;;
;;;    Create a lambda node out of some code, returning the result.  The
;;; bindings are specified by the list of var structures Vars.  We deal with
;;; adding the names to the *venv* for the conversion.  The result is added to
;;; the New-Functions in the *Current-Component* and linked to the component
;;; head and tail.
;;;
;;; We detect special bindings here, replacing the original Var in the lambda
;;; list with a temporary variable.  We then pass a list of the special vars to
;;; IR1-Convert-Special-Bindings, which actually emits the special binding
;;; code.
;;;
;;; We ignore any Arg-Info in the Vars, trusting that someone else is dealing
;;; with &nonsense.
;;;
;;; Aux-Vars is a list of Var structures for variables that are to be
;;; sequentially bound.  Each Aux-Val is a form that is to be evaluated to get
;;; the initial value for the corresponding Aux-Var.
;;;
(proclaim '(function ir1-convert-lambda-body (list list t &optional list list)
		     lambda))
(defun ir1-convert-lambda-body (body vars source &optional aux-vars aux-vals)
  (let* ((bind (make-bind :source source))
	 (lambda (make-lambda :vars vars  :bind bind))
	 (*current-lambda* lambda)
	 (*current-cleanup* lambda)
	 (result (make-continuation)))
    (setf (lambda-home lambda) lambda)
    (collect ((svars)
	      (new-venv *venv* cons))

      (dolist (var vars)
	(setf (lambda-var-home var) lambda)
	(let ((specvar (lambda-var-specvar var)))
	  (cond (specvar
		 (svars var)
		 (new-venv (cons (leaf-name specvar) specvar)))
		(t
		 (new-venv (cons (leaf-name var) var))))))
      
      (setf (bind-lambda bind) lambda)
      (let ((*venv* (new-venv))
	    (cont1 (make-continuation))
	    (cont2 (make-continuation)))
	(continuation-starts-block cont1)
	(prev-link bind cont1)
	(use-continuation bind cont2)
	(ir1-convert-special-bindings cont2 result body aux-vars aux-vals
				      (svars) source))

      (let ((block (continuation-block result)))
	(when block
	  (let ((return (make-return :source source  :result result
				     :lambda lambda))
		(tail-set (make-tail-set :functions (list lambda)))
		(dummy (make-continuation)))
	    (setf (lambda-tail-set lambda) tail-set)
	    (setf (lambda-return lambda) return)
	    (setf (continuation-dest result) return)
	    (setf (block-last block) return)
	    (prev-link return result)
	    (use-continuation return dummy))
	  (link-blocks block (component-tail *current-component*)))))

    (link-blocks (component-head *current-component*) (node-block bind))
    (push lambda (component-new-functions *current-component*))
    lambda))


;;; Convert-Optional-Entry  --  Internal
;;;
;;;    Create the actual entry-point function for an optional entry point.  The
;;; lambda binds copies of each of the Vars, then calls Fun with the argument
;;; Vals and the Defaults.  Presumably the Vals refer to the Vars by name.  The
;;; Vals are passed in in reverse order.
;;;
;;;    If any of the copies of the vars are referenced more than once, then we
;;; mark the corresponding var as Ever-Used to inhibit "defined but not read"
;;; warnings for arguments that are only used by default forms.
;;;
(proclaim '(function convert-optional-entry (lambda list list list t) lambda))
(defun convert-optional-entry (fun vars vals defaults source)
  (let* ((fvars (reverse vars))
	 (arg-vars (mapcar #'(lambda (var)
			       (make-lambda-var
				:name (leaf-name var)
				:type (leaf-type var)
				:where-from (leaf-where-from var)
				:specvar (lambda-var-specvar var)))
			   fvars))
	 (fun
	  (ir1-convert-lambda-body
	   `((%funcall ,fun ,@(reverse vals) ,@defaults))
	   arg-vars source)))
    (mapc #'(lambda (var arg-var)
	      (when (cdr (leaf-refs arg-var))
		(setf (leaf-ever-used var) t)))
	  fvars arg-vars)
    fun))


;;; Generate-Optional-Default-Entry  --  Internal
;;;
;;;    This function deals with supplied-p vars in optional arguments.  If the
;;; there is no supplied-p arg, then we just call IR1-Convert-Hairy-Args on the
;;; remaining arguments, and generate a optional entry that calls the result.
;;; If there is a supplied-p var, then we add it into the default vars and
;;; throw a T into the entry values.  The resulting entry point function is
;;; returned.
;;;
(proclaim '(function generate-optional-default-entry
		     (optional-dispatch list list list list list boolean list
					list list list)
		     lambda))
(defun generate-optional-default-entry (res default-vars default-vals
					    entry-vars entry-vals
					    vars supplied-p-p body source
					    aux-vars aux-vals)
  (let* ((arg (first vars))
	 (arg-name (leaf-name arg))
	 (info (lambda-var-arg-info arg))
	 (supplied-p (arg-info-supplied-p info))
	 (ep (if supplied-p
		 (ir1-convert-hairy-args
		  res
		  (list* supplied-p arg default-vars)
		  (list* (leaf-name supplied-p) arg-name default-vals)
		  (cons arg entry-vars)
		  (list* t arg-name entry-vals)
		  (rest vars) t body source aux-vars aux-vals)
		 (ir1-convert-hairy-args 
		  res
		  (cons arg default-vars)
		  (cons arg-name default-vals)
		  (cons arg entry-vars)
		  (cons arg-name entry-vals)
		  (rest vars) supplied-p-p body source aux-vars aux-vals))))
		 
    (convert-optional-entry ep default-vars default-vals
			    (if supplied-p
				(list (arg-info-default info) nil)
				(list (arg-info-default info)))
			    source)))


;;; Convert-More-Entry  --  Internal
;;;
;;;    Create the More-Entry function for the Optional-Dispatch Res.
;;; Entry-Vars and Entry-Vals describe the fixed arguments.  Rest is the var
;;; for any Rest arg.  Keys is a list of the keyword arg vars.
;;;
;;;    The most interesting thing that we do is parse keywords.  We create a
;;; bunch of temporary variables to hold the result of the parse, and then loop
;;; over the supplied arguments, setting the appropriate temps for the supplied
;;; keyword.
;;;
;;;    If there is no supplied-p var, then we initialize the temp to the
;;; default and just pass the temp into the main entry.  Since non-constant
;;; keyword args are forcibly given a supplied-p var, we know that the default
;;; is constant, and thus safe to evaluate out of order.
;;;
;;;    If there is a supplied-p var, then we create temps for both the value
;;; and the supplied-p, and pass them into the main entry, letting it worry
;;; about defaulting.
;;;
;;;    We deal with :allow-other-keys by delaying unknown keyword errors until
;;; we have scanned all the keywords.
;;;
(proclaim '(function convert-more-entry
		     (optional-dispatch list list (or lambda-var null)
					list list)
		     void))
(defun convert-more-entry (res entry-vars entry-vals rest keys source)
  (collect ((arg-vars)
	    (arg-vals (reverse entry-vals))
	    (temps)
	    (body))
    
    (dolist (var (reverse entry-vars))
      (arg-vars (make-lambda-var
		 :name (leaf-name var)
		 :type (leaf-type var)
		 :where-from (leaf-where-from var))))

    (let* ((n-context (gensym))
	   (context-temp (make-lambda-var :name n-context))
	   (n-count (gensym))
	   (count-temp (make-lambda-var :name n-count
					:type (specifier-type 'fixnum))))
      (arg-vars context-temp count-temp)

      (when rest
	(arg-vals `(%listify-rest-args ,n-context ,n-count)))

      (when (optional-dispatch-keyp res)
	(let ((n-index (gensym))
	      (n-key (gensym))
	      (n-value-temp (gensym))
	      (n-allowp (gensym))
	      (n-losep (gensym))
	      (allowp
	       (or (optional-dispatch-allowp res)
		   (policy nil (or (> space safety) (> speed safety))))))
	  
	  (temps `(,n-index 0) n-key n-value-temp)
	  (body `(declare (fixnum ,n-index) (ignorable ,n-key ,n-value-temp)))

	  (collect ((tests))
	    (dolist (key keys)
	      (let* ((info (lambda-var-arg-info key))
		     (default (arg-info-default info))
		     (keyword (arg-info-keyword info))
		     (supplied-p (arg-info-supplied-p info))
		     (n-value (gensym)))
		(cond (supplied-p
		       (let ((n-supplied (gensym)))
			 (temps n-value n-supplied)
			 (arg-vals n-value n-supplied)
			 (tests `((eq ,n-key ,keyword)
				  (setq ,n-supplied t)
				  (setq ,n-value ,n-value-temp)))))
		      (t
		       (temps `(,n-value ,default))
		       (arg-vals n-value)
		       (tests `((eq ,n-key ,keyword)
				(setq ,n-value ,n-value-temp)))))))

	    (unless allowp
	      (temps n-allowp n-losep)
	      (tests `((eq ,n-key :allow-other-keys)
		       (setq ,n-allowp ,n-value-temp)))
	      (tests `(t
		       (setq ,n-losep ,n-key))))

	    (body
	     `(when (oddp ,n-count)
		(%odd-keyword-arguments-error)))

	    (body
	     `(locally
		(declare (optimize (safety 0)))
		(loop
		  (when (= ,n-index ,n-count) (return))
		  (setq ,n-key (%more-arg ,n-context ,n-index))
		  (incf ,n-index)
		  (setf ,n-value-temp (%more-arg ,n-context ,n-index))
		  (incf ,n-index)
		  (cond ,@(tests)))))

	    (unless allowp
	      (body `(when (and ,n-losep (not ,n-allowp))
		       (%unknown-keyword-argument-error ,n-losep)))))))
      
      (let ((ep (ir1-convert-lambda-body
		 `((let ,(temps)
		     ,@(body)
		     (%funcall ,(optional-dispatch-main-entry res)
			      . ,(arg-vals))))
		 (arg-vars) source)))
	(setf (optional-dispatch-more-entry res) ep)))))


;;; IR1-Convert-More  --  Internal
;;;
;;;    Called by IR1-Convert-Hairy-Args when we run into a rest or keyword arg.
;;; The arguments are similar to that function, but we split off any rest arg
;;; and pass it in separately.  Rest is the rest arg var, or NIL if there is no
;;; rest arg.  Keys is a list of the keyword argument vars.
;;;
(proclaim '(function ir1-convert-more
		     (optional-dispatch list list list list (or lambda-var null)
					list boolean list list list list)
		     lambda))
(defun ir1-convert-more (res default-vars default-vals entry-vars entry-vals
			     rest keys supplied-p-p body source aux-vars
			     aux-vals)
  (collect ((main-vars (reverse default-vars))
	    (main-vals default-vals cons)
	    (bind-vars)
	    (bind-vals))
    (when rest
      (main-vars rest)
      (main-vals '()))

    (dolist (key keys)
      (let* ((info (lambda-var-arg-info key))
	     (default (arg-info-default info))
	     (hairy-default (not (compiler-constantp default)))
	     (supplied-p (arg-info-supplied-p info))
	     (n-val (gensym))
	     (val-temp (make-lambda-var :name n-val)))
	(main-vars val-temp)
	(bind-vars key)
	(cond ((or hairy-default supplied-p)
	       (let* ((n-supplied (gensym))
		      (supplied-temp (make-lambda-var :name n-supplied)))
		 (unless supplied-p
		   (setf (arg-info-supplied-p info) supplied-temp))
		 (setf (arg-info-default info) nil)
		 (main-vars supplied-temp)
		 (main-vals nil nil)
		 (bind-vals `(if ,n-supplied ,n-val ,default))
		 (when supplied-p
		   (bind-vars supplied-p)
		   (bind-vals n-supplied))))
	      (t
	       (main-vals (arg-info-default info))
	       (bind-vals n-val)))))

    (let* ((main-entry (ir1-convert-lambda-body body (main-vars) source
						(append (bind-vars) aux-vars)
						(append (bind-vals) aux-vals)))
	   (last-entry (convert-optional-entry main-entry default-vars
					       (main-vals) () source)))
      (setf (optional-dispatch-main-entry res) main-entry)
      (convert-more-entry res entry-vars entry-vals rest keys source)

      (push (if supplied-p-p
		(convert-optional-entry last-entry entry-vars entry-vals
					() source)
		last-entry)
	    (optional-dispatch-entry-points res))
      last-entry)))


;;; IR1-Convert-Hairy-Args  --  Internal
;;;
;;;    This function generates the entry point functions for the
;;; optional-dispatch Res.  We accomplish this by recursion on the list of
;;; arguments, analyzing the arglist on the way down and generating entry
;;; points on the way up.
;;;
;;;    Default-Vars is a reversed list of all the argument vars processed so
;;; far, including supplied-p vars.  Default-Vals is a list of the names of the
;;; Default-Vars.
;;;
;;;    Entry-Vars is a reversed list of processed argument vars, excluding
;;; supplied-p vars.  Entry-Vals is a list things that can be evaluated to get
;;; the values for all the vars from the Entry-Vars.  It has the var name for
;;; each required or optional arg, and has T for each supplied-p arg.
;;;
;;;    Vars is a list of the Lambda-Var structures for arguments that haven't
;;; been processed yet.  Supplied-p-p is true if a supplied-p argument has
;;; already been processed; only in this case are the Default-XXX and Entry-XXX
;;; different.
;;;
;;;    The result at each point is a lambda which should be called by the above
;;; level to default the remaining arguments and evaluate the body.  We cause
;;; the body to be evaluated by converting it and returning it as the result
;;; when the recursion bottoms out.
;;;
;;;    Each level in the recursion also adds its entry point function to the
;;; result Optional-Dispatch.  For most arguments, the defaulting function and
;;; the entry point function will be the same, but when supplied-p args are
;;; present they may be different.
;;;
;;;     When we run into a rest or keyword arg, we punt out to
;;; IR1-Convert-More, which finishes for us in this case.
;;;
(proclaim '(function ir1-convert-hairy-args
		     (optional-dispatch list list list list list boolean list
					list list list)
		     lambda))
(defun ir1-convert-hairy-args (res default-vars default-vals
				   entry-vars entry-vals
				   vars supplied-p-p body source aux-vars
				   aux-vals)
  (cond ((not vars)
	 (let ((fun (ir1-convert-lambda-body body (reverse default-vars) source
					     aux-vars aux-vals)))
	   (setf (optional-dispatch-main-entry res) fun)
	   (push (if supplied-p-p
		     (convert-optional-entry fun entry-vars entry-vals
					     () source)
		     fun)
		 (optional-dispatch-entry-points res))
	   fun))
	((not (lambda-var-arg-info (first vars)))
	 (let* ((arg (first vars))
		(nvars (cons arg default-vars))
		(nvals (cons (leaf-name arg) default-vals)))
	   (ir1-convert-hairy-args res nvars nvals nvars nvals
				   (rest vars) nil body source aux-vars
				   aux-vals)))
	(t
	 (let* ((arg (first vars))
		(info (lambda-var-arg-info arg))
		(kind (arg-info-kind info)))
	   (ecase kind
	     (:optional
	      (let ((ep (generate-optional-default-entry
			 res default-vars default-vals
			 entry-vars entry-vals vars supplied-p-p body source
			 aux-vars aux-vals)))
		(push (if supplied-p-p
			  (convert-optional-entry ep entry-vars entry-vals
						  () source)
			  ep)
		      (optional-dispatch-entry-points res))
		ep))
	     (:rest
	      (ir1-convert-more res default-vars default-vals entry-vars entry-vals
				arg (rest vars) supplied-p-p body source
				aux-vars aux-vals))
	     (:keyword
	      (ir1-convert-more res default-vars default-vals entry-vars entry-vals
				nil vars supplied-p-p body source aux-vars
				aux-vals)))))))


;;; IR1-Convert-Hairy-Lambda  --  Internal
;;;
;;;     This function deals with the case where we have to make an
;;; Optional-Dispatch to represent a lambda.  We cons up the result and call
;;; IR1-Convert-Hairy-Args to do the work.  When it is done, we figure out the
;;; min-args and max-args. 
;;;
(proclaim '(function ir1-convert-hairy-lambda
		     (list list boolean boolean list list list)
		     optional-dispatch))
(defun ir1-convert-hairy-lambda (body vars keyp allowp source aux-vars aux-vals)
  (let ((res (make-optional-dispatch :arglist vars  :allowp allowp
				     :keyp keyp))
	(min (or (position-if #'lambda-var-arg-info vars) (length vars))))
    (push res (component-new-functions *current-component*))
    (ir1-convert-hairy-args res () () () () vars nil body source aux-vars
			    aux-vals)
    (setf (optional-dispatch-min-args res) min)
    (setf (optional-dispatch-max-args res)
	  (+ (1- (length (optional-dispatch-entry-points res))) min))

    (flet ((frob (ep)
	     (when ep
	       (setf (functional-kind ep) :optional)
	       (setf (lambda-optional-dispatch ep) res))))
      (dolist (ep (optional-dispatch-entry-points res)) (frob ep))
      (frob (optional-dispatch-more-entry res))
      (frob (optional-dispatch-main-entry res)))
      
    res))
    
    
;;; IR1-Convert-Lambda  --  Internal
;;;
;;;    Convert a Lambda into a Lambda or Optional-Dispatch leaf.
;;;
(proclaim '(function ir1-convert-lambda (t &optional t) functional))
(defun ir1-convert-lambda (form &optional (source form))
  (unless (and (consp form) (eq (car form) 'lambda) (consp (cdr form))
	       (listp (cadr form)))
    (compiler-error "Malformed lambda expression: ~S." form))

  (multiple-value-bind (vars keyp allow-other-keys aux-vars aux-vals)
		       (find-lambda-vars (cadr form))
    (multiple-value-bind
	(body decls)
	(system:parse-body (cddr form) *fenv* t)
      (multiple-value-bind
	  (*venv* *inlines* *type-restrictions* *current-cookie*)
	  (process-declarations decls (append aux-vars vars) nil)
	(let ((res (if (or (find-if #'lambda-var-arg-info vars) keyp)
		       (ir1-convert-hairy-lambda body vars keyp
						 allow-other-keys source
						 aux-vars aux-vals)
		       (ir1-convert-lambda-body body vars source aux-vars
						aux-vals))))
	  (setf (functional-inline-expansion res) form)
	  (setf (functional-arg-documentation res) (cadr form))
	  res)))))


;;;; Variable hacking:


;;; Find-Free-Really-Function  --  Internal
;;;
;;;    Return a Global-Var structure usable for referencing the global function
;;; Name.
;;;
(defun find-free-really-function (name)
  (unless (info function kind name)
    (setf (info function kind name) :function)
    (setf (info function where-from name) :assumed))

  (make-global-var :kind :global-function  :name name
		   :type (info function type name)
		   :where-from (info function where-from name)))


;;; Find-Slot-Accessor  --  Internal
;;;
;;;    Return a Slot-Accessor structure usable for referencing the slot
;;; accessor Name.  Info is the structure definition.
;;;
(defun find-slot-accessor (info name)
  (declare (type defstruct-description info))
  (let* ((accessor (if (listp name) (cadr name) name))
	 (slot (find accessor (dd-slots info)
		     :key #'dsd-accessor))
	 (type (dd-name info))
	 (slot-type (dsd-type slot)))
    (assert slot () "Can't find slot ~S." type)
    (make-slot-accessor
     :name name
     :type (specifier-type
	    (if (listp name)
		`(function (,type ,slot-type) ,slot-type)
		`(function (,type) ,slot-type)))
     :for info
     :slot slot)))


;;; Find-Free-Function  --  Internal
;;;
;;;    If Name is already entered in *free-functions*, then return the value.
;;; Otherwise, make a new Global-Var using information from the global
;;; environment and enter it in *free-functions*.  If Name names a macro or
;;; special form, then we error out using the supplied context which indicates
;;; what we were trying to do that demanded a function.  The second value is
;;; the inlinep information which currently applies to the variable.
;;;
(proclaim '(function find-free-function (t string) (values global-var inlinep)))
(defun find-free-function (name context)
  (let ((found (gethash name *free-functions*)))
    (if found
	(values found (leaf-inlinep found))
	(ecase (info function kind name)
	  (:macro
	   (compiler-error "Found macro name ~S ~A." name context))
	  (:special-form
	   (compiler-error "Found special-form name ~S ~A." name context))
	  ((:function nil)
	   (check-function-name name)
	   (let ((info (info function accessor-for name)))
	     (values (setf (gethash name *free-functions*)
			   (if info
			       (find-slot-accessor info name)
			       (find-free-really-function name)))
		     (info function inlinep name))))))))


;;; IR1-Convert-Variable  --  Internal
;;;
;;;    Convert a reference to a symbolic constant or variable.  If the symbol
;;; is entered in *venv*, then we use that definition, otherwise we find the
;;; current global definition.
;;;
(proclaim '(function ir1-convert-variable (continuation continuation symbol) void))
(defun ir1-convert-variable (start cont name)
  (let ((var (or (cdr (assoc name *venv*)) (find-free-variable name))))
    (when (and (lambda-var-p var) (lambda-var-ignorep var))
      (compiler-warning "Reading an ignored variable: ~S." name))
    (if (lisp::ct-a-val-p var)
	(ir1-convert start cont `(alien-value ,name))
	(reference-leaf start cont var nil))))


;;; Find-Free-Variable  --  Internal
;;;
;;;    Return the Leaf node for a global variable reference to Name.  If Name
;;; is already entered in *free-variables*, then we just return the
;;; corresponding value.  Otherwise, we make a new leaf using information from
;;; the global environment and enter it in *free-variables*.  If the variable
;;; is unknown, then we emit a warning.
;;;
(proclaim '(function find-free-variable (symbol) leaf))
(defun find-free-variable (name)
  (unless (symbolp name)
    (compiler-error "Variable name is not a symbol: ~S." name))
  (or (gethash name *free-variables*)
      (let ((kind (info variable kind name))
	    (type (info variable type name))
	    (where-from (info variable where-from name)))
	(when (and (eq where-from :assumed) (eq kind :global))
	  (compiler-warning "Unknown variable: ~S." name))

	(multiple-value-bind (val valp)
			     (info variable constant-value name)
	  (setf (gethash name *free-variables*)
		(if (and (eq kind :constant) valp)
		    (make-constant :value val  :name name
				   :type (ctype-of val)
				   :where-from where-from)
		    (make-global-var :kind kind  :name name  :type type
				     :where-from where-from)))))))


;;; Reference-Constant  --  Internal
;;;
;;;    Generate a reference to a manifest constant, creating a new leaf if
;;; necessary.
;;;
(proclaim '(function reference-constant (continuation continuation t t)
		     void))
(defun reference-constant (start cont value source)
  (let* ((leaf (find-constant value))
	 (res (make-ref (leaf-type leaf) source leaf nil)))
    (push res (leaf-refs leaf))
    (prev-link res start)
    (use-continuation res cont)))


;;; Reference-Leaf  --  Internal
;;;
;;;    Generate a Ref node for a Leaf, frobbing the Leaf structure as
;;; needed.  Inlinep specifies the legality of inline coding for a 
;;; function-valued variable. 
;;;
(proclaim '(function reference-leaf
		     (continuation continuation leaf inlinep)
		     void))
(defun reference-leaf (start cont leaf inlinep)
  (let ((res (make-ref (or (cdr (assoc leaf *type-restrictions*))
			   (leaf-type leaf))
		       (leaf-name leaf)
		       leaf
		       inlinep)))
    (push res (leaf-refs leaf))
    (setf (leaf-ever-used leaf) t)
    (prev-link res start)
    (use-continuation res cont)))


;;; Set-Variable  --  Internal
;;;
;;;    Kind of like Reference-Leaf, but we generate a Set node.  This
;;; should only need to be called in Setq.
;;;
(proclaim '(function set-variable (continuation continuation basic-var t list) void)) 
(defun set-variable (start cont var value source)
  (let ((dest (make-continuation)))
    (setf (continuation-asserted-type dest) (leaf-type var))
    (ir1-convert start dest value)
    (let ((res (make-set :source source :var var :value dest)))
      (setf (continuation-dest dest) res)
      (setf (leaf-ever-used var) t)
      (push res (basic-var-sets var))
      (prev-link res dest)
      (use-continuation res cont))))
      

;;;; Some flow-graph hacking utilities:

;;; Prev-Link  --  Internal
;;;
;;;    This function sets up the back link between the node and the
;;; continuation which continues at it. 
;;;
(proclaim '(function prev-link (node continuation) void))
(defun prev-link (node cont)
  (assert (not (continuation-next cont)) () "~S already has a next." cont)
  (assert (not (node-prev node)) () "Garbage in Prev for ~S." node)
  (setf (continuation-next cont) node)
  (setf (node-prev node) cont))


;;; Use-Continuation  --  Internal
;;;
;;;    This function is used to set the continuation for a node, and thus
;;; determine what recieves the value and what is evaluated next.  If the
;;; continuation has no block, then we make it be in the block that the node is
;;; in.  If the continuation heads its block, we end our block and link it to
;;; that block.  If the continuation is not currently used, then we set the
;;; derived-type for the continuation to that of the node, so that a little
;;; type propagation gets done.
;;;
(proclaim '(function use-continuation (node continuation) void))
(defun use-continuation (node cont)
  (let ((block (continuation-block cont))
	(node-block (continuation-block (node-prev node))))
    (assert (not (node-cont node)) () "Garbage in Cont for ~S." node)
    (ecase (continuation-kind cont)
      (:unused
       (setf (continuation-block cont) node-block)
       (setf (continuation-kind cont) :inside-block)
       (setf (continuation-use cont) node)
       (setf (node-cont node) cont))
      (:block-start
       (assert (not (block-last node-block)) () "~S has already ended."
	       node-block)
       (setf (block-last node-block) node)
       (assert (null (block-succ node-block)) () "~S already has successors."
	       node-block)
       (setf (block-succ node-block) (list block))
       (assert (not (member node-block (block-pred block))) ()
	       "~S is already a predecessor of ~S." node-block block)
       (push node-block (block-pred block))
       (add-continuation-use node cont)))))


;;; Continuation-Starts-Block  --  Internal
;;;
;;;    Return the block that Continuation is the start of, making a block if
;;; necessary.  This function is called by IR1 translators which may cause a
;;; continuation to be used more than once.  Every continuation which may be
;;; used more than once must start a block by the time that anyone does a
;;; Use-Continuation on it.
;;;
;;;    Note that the block is made in the *current-lambda*, so continuations
;;; without blocks cannot be passed across function boundaries since they might
;;; get a block assigned which is in the wrong function.  Basically this means
;;; that BLOCK and TAGBODY must make sure that their named continuations have
;;; blocks.
;;;
;;;    We also throw the block into the next/prev list for the
;;; *current-component* so that we keep track of which blocks we have made.
;;;
(defun continuation-starts-block (cont)
  (declare (type continuation cont))
  (ecase (continuation-kind cont)
    (:unused
     (assert (not (continuation-block cont)))
     (let* ((head (component-head *current-component*))
	    (next (block-next head))
	    (new-block (make-block cont)))
       (setf (block-next new-block) next)
       (setf (block-prev new-block) head)
       (setf (block-prev next) new-block)
       (setf (block-next head) new-block)
       (setf (continuation-block cont) new-block)
       (setf (continuation-use cont) nil)
       (setf (continuation-kind cont) :block-start)
       new-block))
    (:block-start
     (continuation-block cont))))


;;;; Exported functions:

;;; IR1-Top-Level  --  Interface
;;;
;;;    This function takes a form and the top-level form number for that form,
;;; and returns a lambda representing the translation of that form in the
;;; current global environment.  The lambda is top-level lambda that can be
;;; called to cause evaluation of the forms.  This lambda is in the initial
;;; component.  If For-Value is T, then the value of the form is returned from
;;; the function, otherwise NIL is returned.
;;;
;;;    This function may have arbitrary effects on the global environment due
;;; to processing of Proclaims and Eval-Whens.  All syntax error checking is
;;; done, with erroneous forms being replaced by a proxy which signals an error
;;; if it is evaluated.  Warnings about possibly inconsistent or illegal
;;; changes to the global environment will also be given.
;;;
;;;    We make the initial component and convert the form in a progn (and an
;;; optional NIL tacked on the end.)  We then return the lambda.  We bind all
;;; of our state variables here, rather than relying on the global value (if
;;; any) so that IR1 conversion will be reentrant.  This is necessary for
;;; eval-when processing, etc.
;;;
;;;    The hashtables used to hold global namespace info must be reallocated
;;; elsewhere.  Note also that *fenv* is not rebound, so that local macro
;;; definitions can be introduced by enclosing code.
;;;
(defun ir1-top-level (form tlf-num for-value)
  (declare (type index tlf-num))
  (let* ((*current-path* (or (gethash form *source-paths*)
			     (list 0 tlf-num)))
	 (*inlines* ())
	 (*type-restrictions* ())
	 (*venv* ())
	 (*benv* ())
	 (*tenv* ())
	 (*current-cookie* (make-cookie))
	 (*current-lambda* nil)
	 (*current-cleanup* nil)
	 (*current-form* nil)
	 (component (make-empty-component))
	 (*current-component* component))
    (setf (component-name component) "initial component")
    (setf (component-kind component) :initial)
    (let* ((forms (if for-value `(,form) `(,form nil)))
	   (res (ir1-convert-lambda-body forms () `(progn ,@forms))))
      (setf (leaf-name res) "Top-Level Form")
      (setf (functional-entry-function res) res)
      (setf (functional-arg-documentation res) ())
      (setf (functional-kind res) :top-level)
      res)))


;;; *CURRENT-FORM-NUMBER* is used in FIND-SOURCE-PATHS to compute the form
;;; number to associate with a source path.  This should be bound to 0 around
;;; the processing of each truly top-level form.
;;;
(proclaim '(type index *current-form-number*))
(defvar *current-form-number*)

;;; Find-Source-Paths  --  Interface
;;;
;;;    This function is called on freshly read forms to record the initial
;;; location of each form (and subform.)  Form is the form to find the paths
;;; in, and TLF-Num is the top-level form number of the truly top-level form.
;;;
;;;    This gets a bit interesting when the source code is circular.  This can
;;; (reasonably?) happen in the case of circular list constants. 
;;;
(defun find-source-paths (form tlf-num)
  (declare (type index tlf-num))
  (let ((*current-form-number* 0))
    (sub-find-source-paths form (list tlf-num)))
  (undefined-value))
;;;
(defun sub-find-source-paths (form path)
  (unless (gethash form *source-paths*)
    (setf (gethash form *source-paths*)
	  (cons *current-form-number* path))
    (incf *current-form-number*)
    (let ((pos 0)
	  (subform form)
	  (trail form))
      (declare (fixnum pos))
      (macrolet ((frob ()
		   '(progn
		      (when (atom subform) (return))
		      (let ((fm (car subform)))
			(when (consp fm)
			  (sub-find-source-paths fm (cons pos path)))
			(incf pos))
		      (setq subform (cdr subform))
		      (when (eq subform trail) (return)))))
	(loop
	  (frob)
	  (frob)
	  (setq trail (cdr trail)))))))


;;;; Control special forms:

(def-ir1-translator progn ((&rest forms) start cont)
  "Progn Form*
  Evaluates each Form in order, returing the values of the last form.  With no
  forms, returns NIL."
  (ir1-convert-progn-body start cont forms))

(def-ir1-translator if ((test then &optional else &whole source) start cont)
  "If Predicate Then [Else]
  If Predicate evaluates to non-null, evaluate Then and returns its values,
  otherwise evaluate Else and return its values.  Else defaults to NIL."
  (let* ((pred (make-continuation))
	 (then-cont (make-continuation))
	 (then-block (continuation-starts-block then-cont))
	 (else-cont (make-continuation))
	 (else-block (continuation-starts-block else-cont))
	 (dummy-cont (make-continuation))
	 (node (make-if :test pred
			:consequent then-block  :alternative else-block
			:source source)))
    (setf (continuation-dest pred) node)
    (ir1-convert start pred test)
    (prev-link node pred)
    (use-continuation node dummy-cont)
    
    (let ((start-block (continuation-block pred)))
      (setf (block-last start-block) node)
      (continuation-starts-block cont)
      
      (link-blocks start-block then-block)
      (link-blocks start-block else-block)
      
      (ir1-convert then-cont cont then)
      (ir1-convert else-cont cont else))))


;;;; Block and Tagbody:
;;;
;;;    We make an Entry node to mark the start and a :Entry cleanup to
;;; mark its extent.  When doing Go or Return-From, we emit an Exit node.
;;; 

;;; Block IR1 convert  --  Internal
;;;
;;;    Make a :entry cleanup and emit an Entry node, then convert the body in
;;; the modified environment.  We make Cont start a block now, since if it was
;;; done later, the block would be in the wrong environment.
;;;
(def-ir1-translator block ((name &rest forms &whole source) start cont)
  "Block Name Form*
  Evaluate the Forms as a PROGN.  Within the lexical scope of the body,
  (RETURN-FROM Name Value-Form) can be used to exit the form, returning the
  result of Value-Form."
  (unless (symbolp name)
    (compiler-error "Block name is not a symbol: ~S." name))
  (continuation-starts-block cont)
  (let* ((dummy (make-continuation))
	 (entry (make-entry :exits (list cont)  :source source))
	 (cleanup (make-cleanup :kind :entry  :start dummy)))
    (push entry (lambda-entries *current-lambda*))
    (prev-link entry start)
    (use-continuation entry dummy)
    (setf (block-end-cleanup (continuation-block dummy)) cleanup)
    (let ((*benv* (acons name (list entry cont) *benv*))
	  (*current-cleanup* cleanup))
      (ir1-convert-progn-body dummy cont forms))))

;;; We make Cont start a block just so that it will have a block assigned.
;;; People assume that when they pass a continuation into IR1-Convert as Cont,
;;; it will have a block when it is done.
;;;
(def-ir1-translator return-from ((name &optional value &whole source)
				 start cont)
  "Return-From Block-Name Value-Form
  Evaluate the Value-Form, returning its values from the lexically enclosing
  BLOCK Block-Name.  This is constrained to be used only within the dynamic
  extent of the BLOCK."
  (continuation-starts-block cont)
  (let* ((found (or (cdr (assoc name *benv*))
		    (compiler-error "Return for unknown block: ~S." name)))
	 (value-cont (make-continuation))
	 (exit (make-exit :entry (first found)  :value value-cont
			  :source source)))
    (setf (continuation-dest value-cont) exit)
    (ir1-convert start value-cont value)
    (prev-link exit value-cont)
    (use-continuation exit (second found))))


;;; Parse-Tagbody  --  Internal
;;;
;;;    Return a list of the segments of a tagbody.  Each segment looks like
;;; (<tag> <form>* (go <next tag>)).  That is, we break up the tagbody into
;;; segments of non-tag statements, and explicitly represent the drop-through
;;; with a GO.  The first segment has a dummy NIL tag, since it represents code
;;; before the first tag.  The last segment (which may also be the first
;;; segment) ends in NIL rather than a GO.
;;;
(defun parse-tagbody (body)
  (declare (list body))
  (collect ((segments))
    (let ((current (cons nil body)))
      (loop
	(let ((tag-pos (position-if #'atom current :start 1)))
	  (unless tag-pos
	    (segments `(,@current nil))
	    (return))
	  (let ((tag (elt current tag-pos)))
	    (when (assoc tag (segments))
	      (compiler-error "Repeated tagbody tag: ~S." tag))
	    (unless (or (symbolp tag) (integerp tag))
	      (compiler-error "Illegal tagbody statement: ~S." tag))	      
	    (segments `(,@(subseq current 0 tag-pos) (go ,tag))))
	  (setq current (nthcdr tag-pos current)))))
    (segments)))
  

;;; Tagbody IR1 convert  --  Internal
;;;
;;;    Set up the cleanup, emitting the entry node.  Then make a block for each
;;; tag, building up the tag list for *tenv* as we go.  Finally, convert each
;;; segment with the precomputed Start and Cont values.
;;;
(def-ir1-translator tagbody ((&rest statements &whole source) start cont)
  "Tagbody {Tag | Statement}*
  Define tags for used with GO.  The Statements are evaluated in order
  (skipping Tags) and NIL is returned.  If a statement contains a GO to a
  defined Tag within the lexical scope of the form, then control is transferred
  to the next statement following that tag.  A Tag must an integer or a
  symbol.  A statement must be a list.  Other objects are illegal within the
  body."
  (continuation-starts-block cont)
  (let* ((dummy (make-continuation))
	 (entry (make-entry :source source))
	 (*current-cleanup* (make-cleanup :kind :entry  :start dummy))
	 (segments (parse-tagbody statements)))
    (push entry (lambda-entries *current-lambda*))
    (prev-link entry start)
    (use-continuation entry dummy)
    (setf (block-end-cleanup (continuation-block start)) *current-cleanup*)

    (collect ((tags)
	      (starts)
	      (conts))
      (starts dummy)
      (dolist (segment (rest segments))
	(let ((tag-cont (make-continuation)))
	  (conts tag-cont)
	  (starts tag-cont)
	  (continuation-starts-block tag-cont)
	  (tags (list (car segment) entry tag-cont))))
      (conts cont)
      
      (setf (entry-exits entry) (rest (starts)))
      
      (let ((*tenv* (append (tags) *tenv*)))
	(mapc #'(lambda (segment start cont)
		  (ir1-convert-progn-body start cont (rest segment)))
	      segments (starts) (conts))))))


;;; Go IR1 convert  --  Internal
;;;
;;;    Emit an Exit node without any value.
;;;
(def-ir1-translator go ((tag &whole source) start cont)
  "Go Tag
  Transfer control to the named Tag in the lexically enclosing TAGBODY.  This
  is constrained to be used only within the dynamic extent of the TAGBODY."
  (continuation-starts-block cont)
  (let* ((found (or (cdr (assoc tag *tenv*))
		    (compiler-error "Go to nonexistent tag: ~S." tag)))
	 (exit (make-exit :entry (first found)  :source source)))
    (prev-link exit start)
    (use-continuation exit (second found))))


;;;; Translators for compiler-magic special forms:

(def-ir1-translator compiler-let ((bindings &rest body) start cont)
  (collect ((vars)
	    (values))
    (dolist (bind bindings)
      (typecase bind
	(symbol
	 (vars bind)
	 (values nil))
	(list
	 (unless (= (length bind) 2)
	   (compiler-error "Bad compiler-let binding spec: ~S." bind))
	 (vars (first bind))
	 (values (eval (second bind))))
	(t
	 (compiler-error "Bad compiler-let binding spec: ~S." bind))))
    (progv (vars) (values)
      (ir1-convert-progn-body start cont body))))


#-new-compiler
;;;
;;; This flag is used by Eval-When to keep track of when code has already been
;;; evaluated so that it can avoid multiple evaluation of nested Eval-When
;;; (Compile)s.
(defvar *already-evaled-this* nil)

#-new-compiler
;;; DO-EVAL-WHEN-STUFF  --  Interface
;;;
;;;    Do stuff to do an EVAL-WHEN.  This is split off from the IR1 convert
;;; method so that it can be shared by the special-case top-level form
;;; processing code.  We play with the dynamic environment and eval stuff, then
;;; call Fun with a list of forms to be processed at load time.
;;; 
;;;    We have to go through serious contortions to ensure that the forms get
;;; eval'ed exactly once.  If *already-evaled-this* is true then we *do not*
;;; eval since some enclosing eval-when already did.  If we do eval, we
;;; throw a binding of the funny lexical variable %compiler-eval-when-marker%
;;; into the %venv% before we eval the code.  This is to inform the eval-when
;;; in the interpreter that it should eval forms even if they contain only
;;; a COMPILE.  We don't want to use a special as a flag, since that would
;;; pervasively alter the semantics of eval-when, when we just want to
;;; alter it within the lexical scope of this eval-when.
;;;
;;;    We know we are eval'ing for load since we wouldn't get called otherwise.
;;; If LOAD is a situation we convert the body like a progn.  If we eval'ed the
;;; body, then we bind *already-evaled-this* to T around the conversion of body
;;; inhibiting the evaluation of any nested eval-when's.  If we aren't
;;; evaluating for load, then we just convert NIL for the result of the
;;; Eval-When.
;;;
(defun do-eval-when-stuff (situations body fun)
  (when (or (not (listp situations))
	    (set-difference situations '(compile load eval)))
    (compiler-error "Bad Eval-When situation list: ~S." situations))

  (let* ((compilep (member 'compile situations))
	 (evalp (member 'eval situations))
	 (do-eval (and compilep (not *already-evaled-this*))))
    (when do-eval
      (let ((lisp::%venv% '((lisp::%compile-eval-when-marker% t))))
	(lisp::eval-as-progn body)))
    (if (member 'load situations)
	(let ((*already-evaled-this* (or do-eval (and *already-evaled-this* evalp))))
	  (funcall fun body))
	(funcall fun ()))))

#+new-compiler
(proclaim '(special lisp::*already-evaled-this*))

#+new-compiler
;;; DO-EVAL-WHEN-STUFF  --  Interface
;;;
;;;    Do stuff to do an EVAL-WHEN.  This is split off from the IR1 convert
;;; method so that it can be shared by the special-case top-level form
;;; processing code.  We play with the dynamic environment and eval stuff, then
;;; call Fun with a list of forms to be processed at load time.
;;;
;;; Note: the EVAL situation is always ignored: this is conceptually a
;;; compile-only implementation.
;;;
;;; We have to interact with the interpreter to ensure that the forms get
;;; eval'ed exactly once.  We bind *already-evaled-this* to true to inhibit
;;; evaluation of any enclosed EVAL-WHENs, either by IR1 conversion done by
;;; EVAL, or by conversion of the body for load-time processing.  If
;;; *already-evaled-this* is true then we *do not* eval since some enclosing
;;; eval-when already did.
;;;
;;;    We know we are eval'ing for load since we wouldn't get called otherwise.
;;; If LOAD is a situation we call Fun on body. If we aren't evaluating for
;;; load, then we call Fun on NIL for the result of the EVAL-WHEN.
;;;
(defun do-eval-when-stuff (situations body fun)
  (when (or (not (listp situations))
	    (set-difference situations '(compile load eval)))
    (compiler-error "Bad Eval-When situation list: ~S." situations))

  (let* ((do-eval (and (member 'compile situations)
		       (not lisp::*already-evaled-this*)))
	 (lisp::*already-evaled-this* t))
    (when do-eval
      (eval `(progn ,@body)))
    (if (member 'load situations)
	(funcall fun body)
	(funcall fun '(nil)))))

  
(def-ir1-translator eval-when ((situations &rest body) start cont)
  (do-eval-when-stuff situations body
		      #'(lambda (forms)
			  (ir1-convert-progn-body start cont forms))))


;;; DO-MACROLET-STUFF  --  Interface
;;;
;;;    Like DO-EVAL-WHEN-STUFF, only do a macrolet.  Fun is not passed any
;;; arguments.  We cleverly make the FENV entry for a macro look like the
;;; interpreter version so that we can pass in *fenv* as a macroexpansion
;;; environment.
;;;
(defun do-macrolet-stuff (definitions fun)
  (declare (list definitions) (function fun))
  (let ((*fenv* *fenv*)
	(whole (gensym))
	(environment (gensym)))
    (dolist (def definitions)
      (let ((name (first def))
	    (arglist (second def))
	    (body (cddr def)))
	(multiple-value-bind (body local-decs)
			     (lisp::parse-defmacro
			      arglist whole body name
			      :environment environment
			      :error-string 'lisp::defmacro-error-string)
	  (unless (symbolp name)
	    (compiler-error "Macro name ~S is not a symbol." name))
	  (when (< (length def) 3)
	    (compiler-error "Local macro ~S is too short to be a legal definition." name))
	  #-new-compiler
	  (push `(,(first def) macro lambda
		  (,whole ,environment) ,@local-decs (block ,name ,body))
		*fenv*)
	  #+new-compiler
	  (push `(,(first def) macro .
		  ,(coerce `(lambda (,whole ,environment)
			      ,@local-decs (block ,name ,body))
			   'function))
		*fenv*))))
    (funcall fun))

  (undefined-value))


(def-ir1-translator macrolet ((definitions &rest body) start cont)
  (do-macrolet-stuff definitions
		     #'(lambda ()
			 (ir1-convert-progn-body start cont body))))


;;; Not really a special form, but...
;;;
(def-ir1-translator declare ((&rest stuff) start cont)
  (declare (ignore stuff))
  start cont; Ignore hack
  (compiler-error "Misplaced declaration."))


;;;; %Primitive:
;;;
;;;    Uses of %primitive are either expanded into Lisp code or turned into a
;;; funny function.
;;;

;;; Eval-Info-Args  --  Internal
;;;
;;;    Carefully evaluate a list of forms, returning a list of the results.
;;;
(defun eval-info-args (args)
  (declare (list args))
  (handler-case (mapcar #'eval args)
    (error (condition)
      (compiler-error "Lisp error during evaluation of info args:~%~A"
		      condition))))

;;; A hashtable that translates from primitive names to translation functions.
;;;
(defvar *primitive-translators* (make-hash-table :test #'eq))

;;; IR1-Convert-%Primitive  --  Internal
;;;
;;;    If there is a primitive translator, then we expand the call.  Otherwise,
;;; we convert to the %%Primitive funny function.  The first argument is the
;;; template, the second is a list of the results of any codegen-info args, and
;;; the remaining arguments are the runtime arguments.
;;;
;;;    We do a bunch of error checking now so that we don't bomb out with a
;;; fatal error during IR2 conversion.
;;;
(def-ir1-translator system:%primitive ((name &rest args &whole form)
				       start cont)
  
  (unless (symbolp name)
    (compiler-error "%Primitive name is not a symbol: ~S." name))

  (let* ((name (intern (symbol-name name) "C"))
	 (translator (gethash name *primitive-translators*)))
    (if translator
	(ir1-convert start cont (funcall translator (cdr form)))
	(let* ((template (or (gethash name *template-names*)
			     (compiler-error "Undefined primitive name: ~A."
					     name)))
	       (required (length (template-arg-types template)))
	       (info (template-info-arg-count template))
	       (min (+ required info))
	       (nargs (length args)))
	  (if (template-more-args-type template)
	      (when (< nargs min)
		(compiler-error "Primitive called with ~R argument~:P, ~
	    		         but wants at least ~R."
				nargs min))
	      (unless (= nargs min)
		(compiler-error "Primitive called with ~R argument~:P, ~
				 but wants exactly ~R."
				nargs min)))

	  (when (eq (template-result-types template) :conditional)
	    (compiler-error "%Primitive used with a conditional template."))

	  (when (template-more-results-type template)
	    (compiler-error
	     "%Primitive used with an unknown values template."))
	  
	  (ir1-convert start cont
		      `(%%primitive ',template
				    ',(eval-info-args
				       (subseq args required min))
				    ,@(subseq args 0 required)
				    ,@(subseq args min)))))))


;;;; Quote and Function:

(def-ir1-translator quote ((thing &whole source) start cont)
  (reference-constant start cont thing source))


(def-ir1-translator function ((thing) start cont)
  (if (and (consp thing) (eq (car thing) 'lambda))
      (reference-leaf start cont (ir1-convert-lambda thing) nil)
      (multiple-value-bind (var inlinep)
			   (find-lexically-apparent-function
			    thing "as the argument to FUNCTION")
	(reference-leaf start cont var inlinep))))


;;;; Magic functions:
;;;
;;;    Various global functions must be treated magically in IR1 conversion.
;;; If a function is always magical, then we just define an IR1-Convert method
;;; for it.  If the magic is effectively a form of inline expansion, then we
;;; define a source transform which transforms to an internal thing which we
;;; pretend is a special form.
;;;
;;; %Funcall is used by people who want the call to be open-coded regardless of
;;; user policy settings.
;;;

(def-source-transform funcall (function &rest args)
  `(%funcall ,function ,@args))

(def-ir1-translator %funcall ((function &rest args) start cont)
  (let ((fun-cont (make-continuation)))
    (ir1-convert start fun-cont function)
    (ir1-convert-combination-args fun-cont cont `(funcall ,function ,@args)
				  args)))


;;;; Proclaim:
;;;
;;;    Proclaim changes the global environment, so we must special-case it if
;;; we are to keep the information in the *FREE-xxx* variables up to date.
;;; When there is a var structure we disown it by replacing it with an updated
;;; copy.  Uses of the variable which were translated before the PROCLAIM will
;;; get the old version, while subsequent references will get the updated
;;; information. 


;;; Get-Old-Vars  --  Internal
;;;
;;;    Look up some symbols in *free-variables*, returning the var structures
;;; for any which exist.  If any of the names aren't symbols, we complain.
;;;
(proclaim '(function get-old-vars (list) list))
(defun get-old-vars (names)
  (collect ((vars))
    (dolist (name names (vars))
      (unless (symbolp name)
	(compiler-error "Name is not a symbol: ~S." name))
      (let ((old (gethash name *free-variables*)))
	(when old (vars old))))))


;;; Process-Type-Proclamation  --  Internal
;;;
;;;    Replace each old var entry with one having the new type.  If the new
;;; type doesn't intersect with the old type, give a warning.  
;;;
;;;    We also check that the old type of each variable intersects with the new
;;; one, giving a warning if not.  This isn't as serious as conflicting local
;;; declarations, since we assume a redefinition semantics rather than an
;;; intersection semantics.
;;;
(proclaim '(function process-type-proclamation (t list) void))
(defun process-type-proclamation (spec names)
  (let ((type (single-value-type (specifier-type spec))))
    (when (policy nil (>= safety brevity))
      (dolist (name names)
	(let ((old-type (info variable type name)))
	  (unless (or (function-type-p type)
		      (function-type-p old-type)
		      (types-intersect type old-type))
	    (compiler-warning
	     "New proclaimed type ~S for ~S conflicts with old type ~S."
	     (type-specifier type) name (type-specifier old-type))))))

    (dolist (var (get-old-vars names))
      (let ((new (etypecase var
		   (global-var (copy-global-var var))
		   (constant (copy-constant var)))))
	(setf (leaf-type new) type)
	(setf (leaf-where-from new) :declared)
	(setf (gethash (leaf-name var) *free-variables*) new)))))



;;; Process-1-Ftype-Proclamation  --  Internal
;;;
;;;    For now, just update the type of any old var and remove the name from
;;; the list of undefined functions.  Eventually we whould check for
;;; incompatible redefinition.
;;;
(defun process-1-ftype-proclamation (name type)
  (declare (type function-type type))
  (let ((var (gethash (define-function-name name) *free-functions*)))
    (when var
      (let ((new (copy-global-var var))
	    (name (leaf-name var)))
	(setf (leaf-type new) type)
	(setf (leaf-where-from new) :declared)
	(setf (gethash name *free-functions*) new))))

  (note-name-defined name :function)
  (undefined-value))


;;; Process-Ftype-Proclamation  --  Internal
;;;
(proclaim '(function process-ftype-proclamation (t list) void))
(defun process-ftype-proclamation (spec names)
  (let ((type (specifier-type spec)))
    (unless (function-type-p type)
      (compiler-error
       "Declared functional type is not a function type: ~S." spec))
    (dolist (name names)
      (process-1-ftype-proclamation name type))))


(def-ir1-translator proclaim ((what) start cont :kind :function)
  (when (constantp what)
    (let ((form (eval what)))
      (unless (consp form)
	(compiler-error "Malformed PROCLAIM spec: ~S." form))
       
      (let ((name (first form))
	    (args (rest form)))
	(case (first form)
	  (special
	   (dolist (old (get-old-vars (rest form)))
	     (when (or (constant-p old)
		       (eq (global-var-kind old) :constant))
	       (compiler-error
		"Attempt to proclaim constant ~S to be special." name))

	     (ecase (global-var-kind old)
	       (:special)
	       (:global
		(let ((new (copy-global-var old)))
		  (setf (global-var-kind new) :special)
		  (setf (gethash name *free-variables*) new))))))
	  (type
	   (when (endp args)
	     (compiler-error "Malformed TYPE proclamation: ~S." form))
	   (process-type-proclamation (first args) (rest args)))
	  (function
	   (when (endp args)
	     (compiler-error "Malformed FUNCTION proclamation: ~S." form))
	   (process-ftype-proclamation `(function . ,(rest args))
				       (list (first args))))
	  (ftype
	   (when (endp args)
	     (compiler-error "Malformed FTYPE proclamation: ~S." form))
	   (process-ftype-proclamation (first args) (rest args)))
	  ;;
	  ;; No non-global state to be updated.
	  ((inline notinline maybe-inline optimize declaration))
	  (t
	   (cond ((member name type-specifier-symbols)
		  (process-type-proclamation name args))
		 ((info declaration recognized name))
		 (t
		  (compiler-warning "Unrecognized proclamation: ~S." form))))))

      (funcall #'%proclaim form)))
  (ir1-convert start cont `(%proclaim ,what)))


;;; %Compiler-Defstruct IR1 Convert  --  Internal
;;;
;;;    This is a frob that DEFMACRO expands into to establish the compiler
;;; semantics.  This is similar to the translator for proclaim: its job is to
;;; keep the *free-functions* in sync.
;;;
(def-ir1-translator %compiler-defstruct ((info) start cont :kind :function)
  (let* ((info (eval info))
	 (name (dd-name info))
	 (copier (dd-copier info))
	 (predicate (dd-predicate info)))
    (setf (info type kind name) :structure)
    (setf (info type structure-info name) info)
    (dolist (slot (dd-slots info))
      (let ((fun (dsd-accessor slot))
	    (type (dsd-type slot)))
	(process-1-ftype-proclamation
	 fun
	 (specifier-type `(function (,name) ,type)))
	(unless (dsd-read-only slot)
	  (process-1-ftype-proclamation
	   `(setf ,fun)
	   (specifier-type `(function (,name ,type) ,type))))))

    (collect ((forms))
      (when copier
	(forms `(proclaim '(ftype (function (,name) ,name) ,copier))))
      
      (when predicate
	(forms `(proclaim '(ftype (function (t) boolean) ,predicate))))
      
      (funcall #'%%compiler-defstruct info)
      (ir1-convert start cont `(progn
				 ,@(forms)
				 (%%compiler-defstruct ',info))))))


;;;; Let and Let*:
;;;
;;;    Let and Let* can't be implemented as macros due to the fact that
;;; any pervasive declarations also affect the evaluation of the arguments.

;;; Extract-Let-Variables  --  Internal
;;;
;;;    Given a list of binding specifiers in the style of Let, return:
;;;  1] The list of var structures for the variables bound.
;;;  2] The initial value form for each variable.
;;;
;;; The variable names are checked for legality and globally special variables
;;; are marked as such.  Context is the name of the form, for error reporting
;;; purposes.
;;;
(proclaim '(function extract-let-variables (list symbol) (values list list)))
(defun extract-let-variables (bindings context)
  (collect ((vars)
	    (vals)
	    (names))
    (dolist (spec bindings)
      (cond ((atom spec)
	     (let ((var (varify-lambda-arg spec (names))))
	       (vars var)
	       (names (cons spec var)) 
	       (vals nil)))
	    (t
	     (when (/= (length spec) 2)
	       (compiler-error "Malformed ~S binding spec: ~S." context spec))
	     (let* ((name (first spec))
		    (var (varify-lambda-arg name (names))))
	       (vars var)
	       (names name)
	       (vals (second spec))))))

    (values (vars) (vals) (names))))


(def-ir1-translator let ((bindings &body (body decls) &whole source) start cont)
  (multiple-value-bind (vars values)
		       (extract-let-variables bindings 'let)
    (multiple-value-bind (*venv* *inlines* *type-restrictions* *current-cookie*)
			 (process-declarations decls vars nil)

      (let ((fun-cont (make-continuation))
	    (fun (ir1-convert-lambda-body body vars source)))
	(reference-leaf start fun-cont fun nil)
	(ir1-convert-combination-args fun-cont cont source values)))))


(def-ir1-translator let* ((bindings &body (body decls) &whole source) start cont)
  (multiple-value-bind (vars values)
		       (extract-let-variables bindings 'let*)
    (multiple-value-bind (*venv* *inlines* *type-restrictions* *current-cookie*)
			 (process-declarations decls vars nil)
      (ir1-convert-aux-bindings start cont body source vars values))))


;;;; Flet and Labels:

;;; Extract-Flet-Variables  --  Internal
;;;
;;;    Given a list of local function specifications in the style of Flet,
;;; return lists of the function names and of the lambdas which are their
;;; definitions.
;;;
;;; The function names are checked for legality.  Context is the name of the
;;; form, for error reporting.
;;;
(proclaim '(function extract-flet-variables (list symbol) (values list list)))
(defun extract-flet-variables (definitions context)
  (collect ((names)
	    (defs))
    (dolist (def definitions)
      (when (or (atom def) (< (length def) 2))
	(compiler-error "Malformed ~S definition spec: ~S." context def))

      (let ((name (check-function-name (first def))))
	(names name)
	(defs (cons 'lambda (rest def)))))
    (values (names) (defs))))


(def-ir1-translator flet ((definitions &body (body decls))
			  start cont)
  (multiple-value-bind (names defs)
		       (extract-flet-variables definitions 'flet)
    (let ((fvars (mapcar #'(lambda (n d)
			     (let ((res (ir1-convert-lambda d)))
			       (setf (leaf-name res) n)
			       res))
			 names defs)))
      (multiple-value-bind (*venv* *inlines* *type-restrictions* *current-cookie*)
			   (process-declarations decls nil fvars)
	(let ((*fenv* (pairlis names fvars *fenv*)))
	  (ir1-convert-progn-body start cont body))))))


;;; For Labels, we have to create dummy function vars and add them to *fenv*
;;; while converting the functions.  We then modify all the references to these
;;; leaves so that they point to the real functional leaves.
;;;
;;; [Perhaps not totally correct, since the declarations aren't processed until
;;; after the function definitions.  This means that declarations for local
;;; functions may not have their full effect on references within the local
;;; functions.]
;;;
(def-ir1-translator labels ((definitions &body (body decls)) start cont)
  (multiple-value-bind (names defs)
		       (extract-flet-variables definitions 'labels)
    (let* ((dummies (mapcar #'(lambda (x)
				(make-functional :name x)) names))
	   (real-funs 
	    (let ((*fenv* (pairlis names dummies *fenv*)))
	      (mapcar #'(lambda (n d)
			  (let ((res (ir1-convert-lambda d)))
			    (setf (leaf-name res) n)
			    res))
		      names defs))))

      (mapc #'substitute-leaf real-funs dummies)

      (multiple-value-bind
	  (*venv* *inlines* *type-restrictions* *current-cookie*)
	  (process-declarations decls nil real-funs)

	(let ((*fenv* (pairlis names real-funs *fenv*)))
	  (ir1-convert-progn-body start cont body))))))


;;;; THE
;;;
;;;    This is somewhat involved, since a type assertion may only be made on a
;;; continuation, not on a node.  If the continuation has no uses before the
;;; form is converted, then we may make the type the assertion for the
;;; continuation.  If there are other uses, then we accept some loss of
;;; information, but try to retain as much as possible.  In this case, we make
;;; the type assertion the union of the pre-existing type assertion and the new
;;; type.

(def-ir1-translator the ((type value) start cont)
  (let ((ctype (specifier-type type)))
    (if (null (find-uses cont))
	(let* ((old-type (continuation-asserted-type cont))
	       (int (values-type-intersection old-type ctype)))
	  (when (eq int *empty-type*)
	    (compiler-warning
	     "Type ~S in THE declaration conflicts with previous type ~S."
	     (type-specifier ctype) (type-specifier old-type)))
	  (setf (continuation-asserted-type cont) int))
	(setf (continuation-asserted-type cont)
	      (values-type-union ctype (continuation-asserted-type cont)))))
  (ir1-convert start cont value))


;;; Truly-The IR1 convert  --  Internal
;;;
;;;    Since the Continuation-Derive-Type is computed as the union of its
;;; uses's types, setting it won't work.  Instead we must intersect the type
;;; with the uses's Derived-Type.
;;;
(def-ir1-translator truly-the ((type value) start cont)
  "Truly-The Type Value
  Like the THE special form, except that it believes whatever you tell it.  It
  will never generate a type check, but will cause a warning if the compiler
  can prove the assertion is wrong."
  (let ((type (specifier-type type)))
    (ir1-convert start cont value)
    (do-uses (use cont)
      (derive-node-type use type))))


;;;; Setq
;;;
;;;    If there is a definition in *venv*, just set that, otherwise
;;; look at the global information.  If the name is for a constant, then
;;; error out.

(def-ir1-translator setq ((&rest things &whole source) start cont)
  (let ((len (length things)))
    (when (oddp len)
      (compiler-error "Odd number of args to SETQ: ~S." source))
    (if (= len 2)
	(let* ((name (first things))
	       (leaf (or (cdr (assoc name *venv*))
			 (find-free-variable name))))
	  (when (or (constant-p leaf)
		    (and (global-var-p leaf)
			 (eq (global-var-kind leaf) :constant)))
	    (compiler-error "Attempt to set constant ~S." name))
	  (when (and (lambda-var-p leaf)
		     (lambda-var-ignorep leaf))
	    (compiler-warning "Setting an ignored variable: ~S." name))
	  (set-variable start cont leaf (second things) source))
	(collect ((sets))
	  (do ((thing things (cddr thing)))
	      ((endp thing)
	       (ir1-convert-progn-body start cont (sets)))
	    (sets `(setq ,(first thing) ,(second thing))))))))

;;;; Catch, Throw and Unwind-Protect:
;;;

;;; Throw  --  Public
;;;
;;;    Although throw could be a macro, it seems this would cause unnecessary
;;; confusion.  We turn THROW into a multiple-value-call of a magical function,
;;; since as as far as IR1 is concerned, it has no interesting properties other
;;; than receiving multiple-values.
;;;
(def-ir1-translator throw ((tag result) start cont)
  "Throw Tag Form
  Do a non-local exit, return the values of Form from the CATCH whose tag
  evaluates to the same thing as Tag."
  (ir1-convert start cont
	       `(multiple-value-call #'%throw ,tag ,result)))


;;; This is a special special form used to instantiate a cleanup as the current
;;; cleanup within the body.  Kind is a the kind of cleanup to make.  We make
;;; the Start be Start, then set Block-End-Cleanup and bind *Current-Cleanup*.
;;;
(def-ir1-translator %within-cleanup ((kind &body body) start cont)
  (let* ((cleanup (make-cleanup :kind kind  :start start))
	 (*current-cleanup* cleanup))
    (setf (block-end-cleanup (continuation-block start)) cleanup)
    (ir1-convert-progn-body start cont body)))

;;; This is a special special form that makes an "escape function" which
;;; returns unknown values from named block.  We convert the function, set its
;;; kind to :Escape, and then reference it.  The :Escape kind indicates that
;;; this function's purpose is to represent a non-local control transfer, and
;;; that it might not actually have to be compiled.
;;;
;;; Note that environment analysis replaces references to escape functions
;;; with references to the corresponding NLX-Info structure.
;;;
(def-ir1-translator %escape-function ((tag) start cont)
  (let ((fun (ir1-convert-lambda
	      `(lambda ()
		 (return-from ,tag (%unknown-values))))))
    (setf (functional-kind fun) :escape)
    (reference-leaf start cont fun nil)))

;;; Yet another special special form.  This one looks up a local function and
;;; smashes it to a :Cleanup function, as well as referencing it.
;;;
(def-ir1-translator %cleanup-function ((name) start cont)
  (let ((fun (cdr (assoc name *fenv*))))
    (assert (lambda-p fun))
    (setf (functional-kind fun) :cleanup)
    (reference-leaf start cont fun nil)))


;;; Catch  --  Public
;;;
;;;    Catch could be a macro, but it's somewhat tasteless to expand into
;;; implementation-dependent special forms.
;;;
;;;    We represent the possibility of the control transfer by making an
;;; "escape function" that does a lexical exit, and instantiate the cleanup
;;; using %within-cleanup.  It is crucial that %Within-Cleanup immediately
;;; follows the %Catch call so that the Cleanup-Start's Use will be the
;;; combination node for the %Catch.
;;;
(def-ir1-translator catch ((tag &body body) start cont)
  "Catch Tag Form*
  Evaluates Tag and instantiates it as a catcher while the body forms are
  evaluated in an implicit PROGN.  If a THROW is done to Tag within the dynamic
  scope of the body, then control will be transferred to the end of the body
  and the thrown values will be returned."
  (ir1-convert
   start cont
   (let ((exit-block (gensym)))
     `(block ,exit-block
	(%catch (%escape-function ,exit-block) ,tag)
	(%within-cleanup :catch
	  ,@body)))))


;;; Unwind-Protect  --  Public
;;;
;;;    Unwind-Protect is similar to Catch, but more hairy.  We make the cleanup
;;; forms into a local function so that they can be referenced both in the case
;;; where we are unwound and in any local exits.  We use %Cleanup-Function on
;;; this to indicate that reference by %Unwind-Protect isn't "real", and thus
;;; doesn't cause creation of an XEP.
;;;
(def-ir1-translator unwind-protect ((protected &body cleanup) start cont)
  "Unwind-Protect Protected Cleanup*
  Evaluate the form Protected, returning its values.  The cleanup forms are
  evaluated whenever the dynamic scope of the Protected form is exited (either
  due to normal completion or a non-local exit such as THROW)."
  (ir1-convert
   start cont
   (let ((cleanup-fun (gensym))
	 (drop-thru-tag (gensym))
	 (exit-tag (gensym))
	 (next (gensym))
	 (start (gensym))
	 (count (gensym)))
     `(flet ((,cleanup-fun () ,@cleanup nil))
	(block ,drop-thru-tag
	  (multiple-value-bind
	      (,next ,start ,count)
	      (block ,exit-tag
		(%unwind-protect (%escape-function ,exit-tag)
				 (%cleanup-function ,cleanup-fun))
		(%within-cleanup :unwind-protect
				 (return-from ,drop-thru-tag ,protected)))
	    (,cleanup-fun)
	    (%continue-unwind ,next ,start ,count)))))))


;;;; MV stuff.

;;; If there are arguments, multiple-value-call turns into an MV-Combination.
;;;
;;; If there are no arguments, then we convert to a normal combination,
;;; ensuring that a MV-Combination always has at least one argument.  This can
;;; be regarded as an optimization, but it is more important for simplifying
;;; compilation of MV-Combinations.
;;;
(def-ir1-translator multiple-value-call ((fun &rest args &whole source)
					 start cont)
  (let* ((fun-cont (make-continuation))
	 (node (if args
		   (make-mv-combination source fun-cont)
		   (make-combination source fun-cont))))
    (ir1-convert start fun-cont fun)
    (setf (continuation-dest fun-cont) node)
    (assert-continuation-type fun-cont
			      (specifier-type '(or function symbol)))
    (collect ((arg-conts))
      (let ((this-start fun-cont))
	(dolist (arg args)
	  (let ((this-cont (make-continuation node)))
	    (ir1-convert this-start this-cont arg)
	    (setq this-start this-cont)
	    (arg-conts this-cont)))
	(prev-link node this-start)
	(use-continuation node cont)
	(setf (basic-combination-args node) (arg-conts))))))


;;; IR1 convert Multiple-Value-Prog1  --  Internal
;;;
;;; Multiple-Value-Prog1 is represented implicitly in IR1 by having a the
;;; result code use result continuation (CONT), but transfer control to the
;;; evaluation of the body.  In other words, the result continuation isn't
;;; Immediately-Used-P by the nodes that compute the result.
;;;
;;; In order to get the control flow right, we convert the result with a dummy
;;; result continuation, then convert all the uses of the dummy to be uses of
;;; CONT.  If a use is an Exit, then we also substitute CONT for the dummy in
;;; the corresponding Entry node so that they are consistent.  Note that this
;;; doesn't amount to changing the exit target, since the control destination
;;; of an exit is determined by the block successor; we are just indicating the
;;; continuation that the result is delivered to.
;;;
;;; We then convert the body, using another dummy continuation in its own block
;;; as the result.  After we are done converting the body, we move all
;;; predecessors of the dummy end block to CONT's block.
;;;
;;; Note that we both exploit and maintain the invariant that the CONT to an
;;; IR1 convert method either has no block or starts the block that control
;;; should transfer to after completion for the form.  Nested MV-Prog1's work
;;; because during conversion of the result form, we use dummy continuation
;;; whose block is the true control destination.
;;;
(def-ir1-translator multiple-value-prog1 ((result &rest forms) start cont)
  (continuation-starts-block cont)
  (let* ((dummy-result (make-continuation))
	 (dummy-start (make-continuation))
	 (cont-block (continuation-block cont)))
    (continuation-starts-block dummy-start)
    (ir1-convert start dummy-start result)

    (substitute-continuation-uses cont dummy-start)

    (continuation-starts-block dummy-result)
    (ir1-convert-progn-body dummy-start dummy-result forms)
    (let ((end-block (continuation-block dummy-result)))
      (dolist (pred (block-pred end-block))
	(unlink-blocks pred end-block)
	(link-blocks pred cont-block))
      (assert (not (continuation-dest dummy-result)))
      (delete-continuation dummy-result)
      (remove-from-dfo end-block))))


;;;; Interface to defining macros:
;;;
;;;    DEFMACRO, DEFUN and DEFCONSTANT expand into calls to %DEFxxx functions
;;; so that we get a chance to see what is going on.  We define IR1 translators
;;; for these functions which look at the definition and then generate a call
;;; to the %%DEFxxx function. 
;;;

;;; Warn about incompatible or illegal definitions and add the macro to the
;;; compiler environment.  
;;;
;;; Someday we could check for macro arguments being incompatibly redefined.
;;; Doing this right will involve finding the old macro lambda-list and
;;; comparing it with the new one.  We don't want to use min-args and max-args
;;; since they don't completely describe the macro's syntax.
;;;
(def-ir1-translator %defmacro ((name def lambda-list doc) start cont
			       :kind :function)
  (let ((name (eval name))
	(def (second def))) ; Don't want to make a function just yet...
    (unless (symbolp name)
      (compiler-error "Macro name is not a symbol: ~S." name))

    (force-output)
    (ecase (info function kind name)
      ((nil))
      (:function
       (remhash name *free-functions*)
       (compiler-warning
	"Defining ~S to be a macro when it was ~(~A~) to be a function."
	name (info function where-from name)))
      (:macro)
      (:special-form
       (compiler-error "Attempt to redefine special form ~S as a macro."
		       name)))

    (setf (info function kind name) :macro)
    (setf (info function where-from name) :defined)

    (when *compile-time-define-macros*
      (setf (info function macro-function name)
	    #+new-compiler (coerce def 'function)
	    #-new-compiler def))

    (let ((fun (ir1-convert-lambda def)))
      (setf (leaf-name fun)
	    (concatenate 'string "DEFMACRO " (symbol-name name)))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%defmacro ',name ,fun ,doc)))

    (compiler-mumble "Converted ~S.~%" name)))

;;;
;;; Convert the definition and substitute for previous global uses.  We only
;;; do the substitution if the function is in the null environment, ensuring
;;; that the function cannot be called outside of the correct environment.
;;; Emit top-level code to install the definition.
;;;
(def-ir1-translator %defun ((name def doc source) start cont
			    :kind :function)
  (let* ((name (define-function-name (eval name)))
	 (expansion
	  (if (and (member (info function inlinep name)
			   '(:inline :maybe-inline))
		   (in-null-environment))
	      (cadr def) nil)))
    (setf (info function where-from name) :defined)
    (setf (info function inline-expansion name) expansion)
    ;;
    ;; If there is a previous defun for the same name, disown the leaf for it.
    (let ((old (gethash name *free-functions*)))
      (when (functional-p old) (remhash name *free-functions*)))

    (let ((fun (ir1-convert-lambda (cadr def) (cadr source)))
	  (new (gethash name *free-functions*))
	  (info (info function info name)))
      (setf (leaf-name fun) name)
      (cond ((and (in-null-environment t)
		  (not (eq (info function inlinep name) :notinline))
		  (or (not info)
		      (and (null (function-info-transforms info))
			   (null (function-info-templates info))
			   (not (function-info-ir2-convert info)))))
	     (setf (gethash name *free-functions*) fun)
	     (when new (substitute-leaf fun new)))
	    (t
	     (let ((var (find-free-function name "in a strange place")))
	       (setf (leaf-where-from var) :defined))))

      (ir1-convert start cont
		   `(%%defun ',name ,fun ,doc
			     ,@(when expansion `(',expansion))))
      (compiler-mumble "Converted ~S.~%" name))))


;;; Update the global environment to correspond to the new definition.  We only
;;; record a constant-value when the value is obviously constant.  We can have
;;; an optimizer for %%Defconstant that notices when the value becomes constant
;;; and substitutes for the Global-Var structure.
;;;
(def-ir1-translator %defconstant ((name value doc) start cont
				  :kind :function)
  (let ((name (eval name)))
    (unless (symbolp name)
      (compiler-error "Constant name is not a symbol: ~S." name))

    (setf (info variable kind name) :constant)
    (setf (info variable where-from name) :defined)

    (when (and (constantp value) (not (symbolp value)))
      (setf (info variable constant-value name) (eval value)))
    (remhash name *free-variables*))

  (ir1-convert start cont `(%%defconstant ,name ,value ,doc)))
