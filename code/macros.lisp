;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file contains the macros that are part of the standard
;;; Spice Lisp environment.
;;;
;;; Written by Scott Fahlman and Rob MacLachlan.
;;; Modified by Bill Chiles to adhere to
;;;
(in-package 'lisp)
(export '(defvar defparameter defconstant when unless loop setf
	  defsetf define-setf-method psetf shiftf rotatef push pushnew pop
	  incf decf remf case typecase with-open-file
	  with-open-stream with-input-from-string with-output-to-string
	  locally etypecase ctypecase ecase ccase
	  get-setf-method get-setf-method-multiple-value
          define-modify-macro
          otherwise)) ; Sacred to CASE and related macros.

(in-package "EXTENSIONS")
(export '(do-anonymous collect iterate))

(in-package "LISP")


;;; Parse-Body  --  Public
;;;
;;;    Parse out declarations and doc strings, *not* expanding macros.
;;; Eventually the environment arg should be flushed, since macros can't expand
;;; into declarations anymore.
;;;
(defun parse-body (body environment &optional (doc-string-allowed t))
  "This function is to parse the declarations and doc-string out of the body of
  a defun-like form.  Body is the list of stuff which is to be parsed.
  Environment is ignored.  If Doc-String-Allowed is true, then a doc string
  will be parsed out of the body and returned.  If it is false then a string
  will terminate the search for declarations.  Three values are returned: the
  tail of Body after the declarations and doc strings, a list of declare forms,
  and the doc-string, or NIL if none."
  (declare (ignore environment))
  (let ((decls ())
	(doc nil))
    (do ((tail body (cdr tail)))
	((endp tail)
	 (values tail (nreverse decls) doc))
      (let ((form (car tail)))
	(cond ((and (stringp form) (cdr tail))
	       (if doc-string-allowed
		   (setq doc form)
		   (return (values tail (nreverse decls) doc))))
	      ((not (and (consp form) (symbolp (car form))))
	       (return (values tail (nreverse decls) doc)))
	      ((eq (car form) 'declare)
	       (push form decls))
	      (t
	       (return (values tail (nreverse decls) doc))))))))


;;;; DEFMACRO:

#-new-compiler
(proclaim '(special *in-compilation-unit*))

(defparameter defmacro-error-string "Macro ~S cannot be called with ~S args.")

;;; Defmacro  --  Public
;;;
;;;    Parse the definition and make an expander function.  The actual
;;; definition is done by %defmacro which we expand into.
;;;
(defmacro defmacro (name lambda-list &body body)
  (let ((whole (gensym)) (environment (gensym)))
    (multiple-value-bind
	(body local-decs doc)
	(parse-defmacro lambda-list whole body name
			:environment environment
			:error-string 'defmacro-error-string)
      (let ((def `(lambda (,whole ,environment)
		    ,@local-decs
		    (block ,name
		      ,body))))
	;;
	;; ### Bootstrap hack...
	;; When in old compiler, call %%defmacro with #'(lambda ...) so that
	;; the function gets compiled.  When in old interpreter (neither in old
	;; or new compiler), just setf the macro-function so that we can have
	;; interpreted macros.
	(cond #-new-compiler
	      (system:*in-the-compiler*
	       `(c::%%defmacro ',name #',def ,doc))
	      #-new-compiler
	      ((not *in-compilation-unit*)
	       `(setf (symbol-function ',name)
		      (cons 'macro #',def)))
	      (t
	       `(c::%defmacro ',name
			      #+new-compiler #',def
			      #-new-compiler ',def
			      ',lambda-list ,doc)))))))


(eval-when (compile load eval)

;;; %Defmacro, %%Defmacro  --  Internal
;;;
;;;    Defmacro expands into %Defmacro which is a function that is treated
;;; magically the compiler.  After the compiler has gotten the information it
;;; wants out of macro definition, it compiles a call to %%Defmacro which
;;; happens at load time.  We have a %Defmacro function which just calls
;;; %%Defmacro in order to keep the interpreter happy.
;;;
;;;    Eventually %%Defmacro should deal with clearing old compiler information
;;; for the functional value.
;;;
(defun c::%defmacro (name definition lambda-list doc)
  #+new-compiler
  ;; ### bootstrap hack...
  ;; This WHEN only necessary to make cross-compiling of this file work.
  ;; Necessary because the EVAL-WHEN COMPILE goes into the bootstrap
  ;; environment, but is read with the NEW-COMPILER feature.
  (when (fboundp 'eval:interpreted-function-p)
    (assert (eval:interpreted-function-p definition))
    (setf (eval:interpreted-function-name definition)
	  (format nil "DEFMACRO ~S" name))
    (setf (eval:interpreted-function-arglist definition) lambda-list))
  (c::%%defmacro name definition doc))
;;;
(defun c::%%defmacro (name definition doc)
  (clear-info function where-from name)
  (setf (info function macro-function name) definition)
  (setf (info function kind name) :macro)
  (setf (documentation name 'function) doc)
  name)

); Eval-When

;;; ### Bootstrap hack...
;;;
;;; Redefine the top-level defmacro handler to do nothing special when
;;; *bootstrap-defmacro* is true so that our defmacro gets called.
;;;
#-new-compiler
(eval-when (compile load eval)
  (defvar *old-pdm* #'clc::process-defmacro)
  (defvar *bootstrap-defmacro* nil)
  (defun clc::process-defmacro (form)
    (ecase *bootstrap-defmacro*
      ((t)
       (clc::process-random (macroexpand form) nil))
      ((nil)
       (funcall *old-pdm* form))
      (:both
       (clc::process-random (macroexpand form) nil)
       (funcall *old-pdm* form))))))

;;; ### Bootstrap hack...
;;; At load time, get defmacro from the old place and store it in the new
;;; place.
#-new-compiler
(c::%%defmacro 'defmacro (macro-function 'defmacro) nil)


;;; ### Bootstrap hack...
;;; Install macro definitions in this file only into the new compiler's
;;; environment.
(eval-when (compile)
  (setq *bootstrap-defmacro* t))


;;; DEFTYPE is a lot like DEFMACRO.

(defparameter deftype-error-string "Type ~S cannot be used with ~S args.")

(defmacro deftype (name arglist &body body)
  "Syntax like DEFMACRO, but defines a new type."
  (unless (symbolp name)
    (error "~S -- Type name not a symbol." name))
  
  (let ((whole (gensym)))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro arglist whole body name
					 :default-default ''*
					 :error-string 'deftype-error-string
					 )
      `(eval-when (compile load eval)
	 (setf (info type kind ',name) :defined)
	 (setf (info type expander ',name)
	       #'(lambda (,whole) ,@local-decs (block ,name ,body)))
	 ,@(when doc
	     `((setf (documentation ',name 'type) ,doc)))
	 ',name))))

;;; And so is DEFINE-SETF-METHOD.

(defparameter defsetf-error-string "Setf expander for ~S cannot be called with ~S args.")

(compiler-let ((*bootstrap-defmacro* :both))

(defmacro define-setf-method (access-fn lambda-list &body body)
  "Syntax like DEFMACRO, but creates a Setf-Method generator.  The body
  must be a form that returns the five magical values."
  (unless (symbolp access-fn)
    (error "~S -- Access-function name not a symbol in DEFINE-SETF-METHOD."
	   access-fn))

  (let ((whole (gensym)) (environment (gensym)))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro lambda-list whole body access-fn
					 :environment environment
					 :error-string 'defsetf-error-string)
      `(eval-when (load compile eval)
	 (setf (info setf inverse ',access-fn) nil)
	 (setf (info setf expander ',access-fn)
	       #'(lambda (,whole ,environment)
		   ,@local-decs
		   (block ,access-fn ,body)))
	 ,@(when doc
	     `((setf (documentation ',access-fn 'setf) ,doc)))
	 ',access-fn))))

); compiler-let


;;;; Defun, Defvar, Defparameter, Defconstant:

;;; Defun  --  Public
;;;
;;;    Very similar to Defmacro, but simpler.  We don't have to parse the
;;; lambda-list.
;;;
(defmacro defun (name lambda-list &body (body decls doc) &whole source)
  (let ((def `(lambda ,lambda-list
		,@decls
		(block ,(if (and (consp name) (eq (car name) 'setf))
			    (cadr name)
			    name)
		  ,@body))))
    `(c::%defun ',name #',def ,doc ',source)))


;;; %Defun, %%Defun  --  Internal
;;;
;;;    Similar to %Defmacro, ...
;;;
(defun c::%%defun (name def doc &optional inline-expansion)
  (setf (fdefinition name) def)
  (when doc
    (if (and (consp name) (eq (first name) 'setf))
	(setf (documentation (second name) 'setf) doc)
	(setf (documentation name 'function) doc)))

  (unless (eq (info function kind name) :function)
    (setf (info function kind name) :function))

  (when (info function accessor-for name)
    (setf (info function accessor-for name) nil))
    
  (when (or inline-expansion
	    (info function inline-expansion name))
    (setf (info function inline-expansion name) inline-expansion))
  name)
;;;
(defun c::%defun (name def doc source)
  (declare (ignore source))
  #+new-compiler
  (assert (eval:interpreted-function-p def))
  #+new-compiler
  (setf (eval:interpreted-function-name def) name)
  (c::%%defun name def doc))


;;; DEFCONSTANT  --  Public
;;;
(defmacro defconstant (var val &optional doc)
  "For defining global constants at top level.  The DEFCONSTANT says that the
  value is constant and may be compiled into code.  If the variable already has
  a value, and this is not equal to the init, an error is signalled.  The third
  argument is an optional documentation string for the variable."
  `(c::%defconstant ',var ,val ',doc))

;;; %Defconstant, %%Defconstant  --  Internal
;;;
;;;    Like the other %mumbles except that we currently actually do something
;;; interesting at load time, namely checking if the constant is being
;;; redefined.
;;;
(defun c::%defconstant (name value doc)
  (c::%%defconstant name value doc))
;;;
(defun c::%%defconstant (name value doc)
  (when doc
    (setf (documentation name 'variable) doc))
  (when (boundp name)
    (unless (equalp (symbol-value name) value)
      (cerror "Go ahead and change the value."
	      "Constant ~S being redefined." name)))
  (setf (symbol-value name) value)
  (setf (info variable kind name) :constant)
  (clear-info variable constant-value name)
  name)


(defmacro defvar (var &optional (val nil valp) (doc nil docp))
  "For defining global variables at top level.  Declares the variable
  SPECIAL and, optionally, initializes it.  If the variable already has a
  value, the old value is not clobbered.  The third argument is an optional
  documentation string for the variable."
  `(progn
    (proclaim '(special ,var))
     ,@(when valp
	 `((unless (boundp ',var)
	     (setq ,var ,val))))
    ,@(when docp
	`((setf (documentation ',var 'variable) ',doc)))
    ',var))

(defmacro defparameter (var val &optional (doc nil docp))
  "Defines a parameter that is not normally changed by the program,
  but that may be changed without causing an error.  Declares the
  variable special and sets its value to VAL.  The third argument is
  an optional documentation string for the parameter."
  `(progn
    (proclaim '(special ,var))
    (setq ,var ,val)
    ,@(when docp
	`((setf (documentation ',var 'variable) ',doc)))
    ',var))


;;;; ASSORTED CONTROL STRUCTURES


(defmacro when (test &body forms)
  "First arg is a predicate.  If it is non-null, the rest of the forms are
  evaluated as a PROGN."
  `(cond (,test nil ,@forms)))

(defmacro unless (test &rest forms)
  "First arg is a predicate.  If it is null, the rest of the forms are
  evaluated as a PROGN."
  `(cond ((not ,test) nil ,@forms)))


(defmacro return (&optional (value nil))
  `(return-from nil ,value))

(defmacro prog (varlist &body (body decls))
  `(block nil
     (let ,varlist
       ,@decls
       (tagbody ,@body))))

(defmacro prog* (varlist &body (body decls))
  `(block nil
     (let* ,varlist
       ,@decls
       (tagbody ,@body))))


;;; Prog1, Prog2  --  Public
;;;
;;;    These just turn into a Let.
;;;
(defmacro prog1 (result &rest body)
  (let ((n-result (gensym)))
    `(let ((,n-result ,result))
       ,@body
       ,n-result)))
;;;
(defmacro prog2 (form1 result &rest body)
  `(prog1 (progn ,form1 ,result) ,@body))


;;; And, Or  --  Public
;;;
;;;    AND and OR are defined in terms of IF.
;;;
(defmacro and (&rest forms)
  (cond ((endp forms) t)
	((endp (rest forms)) (first forms))
	(t
	 `(if ,(first forms)
	      (and ,@(rest forms))
	      nil))))
;;;
(defmacro or (&rest forms)
  (cond ((endp forms) nil)
	((endp (rest forms)) (first forms))
	(t
	 (let ((n-result (gensym)))
	   `(let ((,n-result ,(first forms)))
	      (if ,n-result
		  ,n-result
		  (or ,@(rest forms))))))))


;;; Cond  --  Public
;;;
;;;    COND also turns into IF.
;;;
(defmacro cond (&rest clauses)
  (if (endp clauses)
      nil
      (let ((clause (first clauses)))
	(when (atom clause)
	  (error "Cond clause is not a list: ~S." clause))
	(let ((test (first clause))
	      (forms (rest clause)))
	  (if (endp forms)
	      (let ((n-result (gensym)))
		`(let ((,n-result ,test))
		   (if ,n-result
		       ,n-result
		       (cond ,@(rest clauses)))))
	      `(if ,test
		   (progn ,@forms)
		   (cond ,@(rest clauses))))))))


;;;; Multiple value macros:

;;; Multiple-Value-XXX  --  Public
;;;
;;;    All the multiple-value receiving forms are defined in terms of
;;; Multiple-Value-Call.
;;;
(defmacro multiple-value-setq (varlist value-form)
  (unless (and (listp varlist) (every #'symbolp varlist))
    (error "Varlist is not a list of symbols: ~S." varlist))
  (let ((temps (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) varlist)))
    `(multiple-value-bind ,temps ,value-form
       ,@(mapcar #'(lambda (var temp)
		     `(setq ,var ,temp))
		 varlist temps)
       ,(car temps))))
;;;
(defmacro multiple-value-bind (varlist value-form &body body)
  (unless (and (listp varlist) (every #'symbolp varlist))
    (error "Varlist is not a list of symbols: ~S." varlist))
  (if (= (length varlist) 1)
      `(let ((,(car varlist) ,value-form))
	 ,@body)
      (let ((ignore (gensym)))
	`(multiple-value-call #'(lambda (&optional ,@varlist &rest ,ignore)
				  (declare (ignore ,ignore))
				  ,@body)
	   ,value-form))))
;;;
(defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))


;;;; SETF and friends.

;;; Note: The expansions for SETF and friends sometimes create needless
;;; LET-bindings of argument values.  The compiler will remove most of
;;; these spurious bindings, so SETF doesn't worry too much about creating
;;; them. 

;;; The inverse for a generalized-variable reference function is stored in
;;; one of two ways:
;;;
;;; A SETF-INVERSE property corresponds to the short form of DEFSETF.  It is
;;; the name of a function takes the same args as the reference form, plus a
;;; new-value arg at the end.
;;;
;;; A SETF-METHOD-EXPANDER property is created by the long form of DEFSETF or
;;; by DEFINE-SETF-METHOD.  It is a function that is called on the reference
;;; form and that produces five values: a list of temporary variables, a list
;;; of value forms, a list of the single store-value form, a storing function,
;;; and an accessing function.

(eval-when (compile load eval)

;;; ### bootstrap hack...
;;; Rename get-setf-method so that we don't blow away setf in the bootstrap
;;; lisp.  All references in this file are to the renamed function, and should
;;; eventually be renamed back.
;;;
#+new-compiler
(defun get-setf-method (form &optional environment)
  (foo-get-setf-method form environment))
;;;
(defun foo-get-setf-method (form &optional environment)
  "Returns five values needed by the SETF machinery: a list of temporary
  variables, a list of values with which to fill them, the temporary for the
  new value in a list, the setting function, and the accessing function."
  (let (temp)
    (cond ((symbolp form)
	   (let ((new-var (gensym)))
	     (values nil nil (list new-var) `(setq ,form ,new-var) form)))
	  ((atom form)
	   (error "~S illegal atomic form for GET-SETF-METHOD." form))
	  ;;
	  ;; ### Bootstrap hack...
	  ;; Ignore any DEFSETF info for structure accessors.
	  ((info function accessor-for (car form))
	   (get-setf-method-inverse form `(funcall #'(setf ,(car form)))))
	  ((setq temp (info setf inverse (car form)))
	   (get-setf-method-inverse form `(,temp)))
	  ((setq temp (info setf expander (car form)))
	   (funcall temp form environment))
	  (t
	   (multiple-value-bind (res win)
				(macroexpand-1 form environment)
	     (if win
		 (foo-get-setf-method res environment)
		 (get-setf-method-inverse
		  form
		  `(funcall #'(setf ,(car form))))))))))

(defun get-setf-method-inverse (form inverse)
  (let ((new-var (gensym))
	(vars nil)
	(vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
	    `(,@inverse ,@vars ,new-var)
	    `(,(car form) ,@vars))))


(defun get-setf-method-multiple-value (form &optional environment)
  "Like Get-Setf-Method, but may return multiple new-value variables."
  (get-setf-method form environment))

(defun defsetter (fn rest env)
  (let* ((arglist (car rest))
	 (new-var (car (cadr rest)))
	 (%arg-count 0)
	 (%min-args 0)
	 (%restp nil)
	 (%let-list nil)
	 (%keyword-tests nil))
    (declare (special %arg-count %min-args %restp %let-list %keyword-tests))
    (multiple-value-bind (body local-decs doc)
			 (parse-body (cddr rest) env)
      ;; Analyze the defmacro argument list.
      (analyze1 arglist '(cdr %access-arglist) fn '%access-arglist)
      ;; Now build the body of the transform.
      (values 
       `(lambda (%access-arglist ,new-var)
	  ,@(when (null arglist)
	      '((declare (ignore %access-arglist))))
	  (let* ,(nreverse %let-list)
	    ,@ local-decs
	    ,@ %keyword-tests
	    ,@ body))
       doc))))

) ; End of Eval-When.


(compiler-let ((*bootstrap-defmacro* :both))

(defmacro defsetf (access-fn &rest rest &environment env)
  "Associates a SETF update function or macro with the specified access
  function or macro.  The format is complex.  See the manual for
  details."
  (cond ((not (listp (car rest)))
	 `(eval-when (load compile eval)
	    (setf (info setf inverse ',access-fn) ',(car rest))
	    ;;
	    ;; ### Bootstrap hack...
	    ;; In bootstrap env, also install inverse in old place so that we
	    ;; can still compile defstructs.
	    #-new-compiler
	    (setf (get ',access-fn 'setf-inverse) ',(car rest))
	    (setf (info setf expander ',access-fn) nil)
	    ,@(if (and (car rest) (stringp (cadr rest)))
		  `((eval-when (load eval)
		      (%put ',access-fn '%setf-documentation ,(cadr rest)))))
	    ',access-fn))
	((and (listp (car rest)) (cdr rest) (listp (cadr rest)))
	 (if (not (= (length (cadr rest)) 1))
	     (cerror "Ignore the extra items in the list."
		     "Only one new-value variable allowed in DEFSETF."))
	 (multiple-value-bind (setting-form-generator doc)
			      (defsetter access-fn rest env)
	   `(eval-when (load compile eval)
	      (setf (info setf inverse ',access-fn) nil)
	      (setf (info setf expander ',access-fn)
		    #'(lambda (access-form environment)
			(declare (ignore environment))
			(do* ((args (cdr access-form) (cdr args))
			      (dummies nil (cons (gensym) dummies))
			      (newval-var (gensym))
			      (new-access-form nil))
			     ((atom args)
			      (setq new-access-form 
				    (cons (car access-form) dummies))
			      (values
			       dummies
			       (cdr access-form)
			       (list newval-var)
			       (funcall (function ,setting-form-generator)
					new-access-form newval-var)
			       new-access-form)))))
	      ,@(if doc
		    `((eval-when (load eval)
			(%put ',access-fn '%setf-documentation ',doc)))
		    `((eval-when (load eval)             ;SKH 4/17/84
			(remprop ',access-fn '%setf-documentation))))
	      ',access-fn)))
	(t (error "Ill-formed DEFSETF for ~S." access-fn))))

); Compiler-Let

(defmacro setf (&rest args &environment env)
  "Takes pairs of arguments like SETQ.  The first is a place and the second
  is the value that is supposed to go into that place.  Returns the last
  value.  The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
  (let ((temp (length args)))
    (cond ((= temp 2)
	   (cond ((atom (car args))
		  `(setq ,(car args) ,(cadr args)))
		 ((info function accessor-for (caar args))
		  `(funcall #'(setf ,(caar args)) ,@(cdar args) ,(cadr args)))
		 ((setq temp (info setf inverse (caar args)))
		  `(,temp ,@(cdar args) ,(cadr args)))
		 (t (multiple-value-bind (dummies vals newval setter getter)
					 (foo-get-setf-method (car args) env)
		      (declare (ignore getter))
		      (do* ((d dummies (cdr d))
			    (v vals (cdr v))
			    (let-list nil))
			   ((null d)
			    (setq let-list
				  (nreverse (cons (list (car newval)
							(cadr args))
						  let-list)))
			    `(let* ,let-list ,setter))
			(setq let-list
			      (cons (list (car d) (car v)) let-list)))))))
	  ((oddp temp) 
	   (error "Odd number of args to SETF."))
	  (t (do ((a args (cddr a)) (l nil))
		 ((null a) `(progn ,@(nreverse l)))
	       (setq l (cons (list 'setf (car a) (cadr a)) l)))))))


(defmacro psetf (&rest args &environment env)
  "This is to SETF as PSETQ is to SETQ.  Args are alternating place
  expressions and values to go into those places.  All of the subforms and
  values are determined, left to right, and only then are the locations
  updated.  Returns NIL."
  (do ((a args (cddr a))
       (let-list nil)
       (setf-list nil))
      ((atom a)
       `(let* ,(nreverse let-list) ,@(nreverse setf-list) nil))
    (if (atom (cdr a))
	(error "Odd number of args to PSETF."))
    (multiple-value-bind (dummies vals newval setter getter)
      (foo-get-setf-method (car a) env)
      (declare (ignore getter))
      (do* ((d dummies (cdr d))
	    (v vals (cdr v)))
	   ((null d))
	(push (list (car d) (car v)) let-list))
      (push (list (car newval) (cadr a)) let-list)
      (push setter setf-list))))



(defmacro shiftf (&rest args &environment env)
  "One or more SETF-style place expressions, followed by a single
  value expression.  Evaluates all of the expressions in turn, then
  assigns the value of each expression to the place on its left,
  returning the value of the leftmost."
  (if (< (length args) 2)
      (error "Too few argument forms to a SHIFTF."))
  (let ((leftmost (gensym)))
    (do ((a args (cdr a))
	 (let-list nil)
	 (setf-list nil)
	 (next-var leftmost))
	((atom (cdr a))
	 (push (list next-var (car a)) let-list)
	 `(let* ,(nreverse let-list) ,@(nreverse setf-list) ,leftmost))
      (multiple-value-bind (dummies vals newval setter getter)
	(foo-get-setf-method (car a) env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v)))
	     ((null d))
	  (push (list (car d) (car v)) let-list))
	(push (list next-var getter) let-list)
	(push setter setf-list)
	(setq next-var (car newval))))))


(defmacro rotatef (&rest args &environment env)
  "Takes any number of SETF-style place expressions.  Evaluates all of the
  expressions in turn, then assigns to each place the value of the form to
  its right.  The rightmost form gets the value of the leftmost.  Returns NIL."
  (cond ((null args) nil)
	((null (cdr args)) `(progn ,(car args) nil))
	(t (do ((a args (cdr a))
		(let-list nil)
		(setf-list nil)
		(next-var nil)
		(fix-me nil))
	       ((atom a)
		  (rplaca fix-me next-var)
		  `(let* ,(nreverse let-list) ,@(nreverse setf-list) nil))
	       (multiple-value-bind (dummies vals newval setter getter)
                 (foo-get-setf-method (car a) env)
		 (do ((d dummies (cdr d))
		      (v vals (cdr v)))
		     ((null d))
		   (push (list (car d) (car v)) let-list))
		 (push (list next-var getter) let-list)
		 ;; We don't know the newval variable for the last form yet,
		 ;; so fake it for the first getter and fix it at the end.
		 (unless fix-me (setq fix-me (car let-list)))
		 (push setter setf-list)
		 (setq next-var (car newval)))))))


(compiler-let ((*bootstrap-defmacro* :both))

(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
	(rest-arg nil)
	(env (gensym))
	(reference (gensym)))
	     
    ;; Parse out the variable names and rest arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
	 (arg nil))
	((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
	    ((eq arg '&rest)
	     (if (symbolp (cadr ll))
		 (setq rest-arg (cadr ll))
		 (error "Non-symbol &rest arg in definition of ~S." name))
	     (if (null (cddr ll))
		 (return nil)
		 (error "Illegal stuff after &rest arg in Define-Modify-Macro.")))
	    ((memq arg '(&key &allow-other-keys &aux))
	     (error "~S not allowed in Define-Modify-Macro lambda list." arg))
	    ((symbolp arg)
	     (push arg other-args))
	    ((and (listp arg) (symbolp (car arg)))
	     (push (car arg) other-args))
	    (t (error "Illegal stuff in lambda list of Define-Modify-Macro."))))
    (setq other-args (nreverse other-args))
    `(defmacro ,name (,reference ,@lambda-list &environment ,env)
       ,doc-string
       (multiple-value-bind (dummies vals newval setter getter)
	 (foo-get-setf-method ,reference ,env)
	 (do ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil (cons (list (car d) (car v)) let-list)))
	     ((null d)
	      (push 
	       (list (car newval)
		     ,(if rest-arg
			  `(list* ',function getter ,@other-args ,rest-arg)
			  `(list ',function getter ,@other-args)))
	       let-list)
	      `(let* ,(nreverse let-list)
		 ,setter)))))))

); Compiler-Let


(defmacro push (obj place &environment env)
  "Takes an object and a location holding a list.  Conses the object onto
  the list, returning the modified list."
  (if (symbolp place)
      `(setq ,place (cons ,obj ,place))
      (multiple-value-bind (dummies vals newval setter getter)
			   (foo-get-setf-method place env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil))
	     ((null d)
	      (push (list (car newval) `(cons ,obj ,getter))
		    let-list)
	      `(let* ,(nreverse let-list)
		 ,setter))
	  (push (list (car d) (car v)) let-list)))))


(defmacro pushnew (obj place &rest keys &environment env)
  "Takes an object and a location holding a list.  If the object is already
  in the list, does nothing.  Else, conses the object onto the list.  Returns
  NIL.  If there is a :TEST keyword, this is used for the comparison."
  (if (symbolp place)
      `(setq ,place (adjoin ,obj ,place ,@keys))
      (multiple-value-bind (dummies vals newval setter getter)
			   (foo-get-setf-method place env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil))
	     ((null d)
	      (push (list (car newval) `(adjoin ,obj ,getter ,@keys))
		    let-list)
	      `(let* ,(nreverse let-list)
		 ,setter))
	  (push (list (car d) (car v)) let-list)))))


(defmacro pop (place &environment env)
  "The argument is a location holding a list.  Pops one item off the front
  of the list and returns it."
  (if (symbolp place)
      `(prog1 (car ,place) (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
			   (foo-get-setf-method place env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil))
	     ((null d)
	      (push (list (car newval) getter) let-list)
	      `(let* ,(nreverse let-list)
		 (prog1 (car ,(car newval))
			(setq ,(car newval) (cdr ,(car newval)))
			,setter)))
	  (push (list (car d) (car v)) let-list)))))


(define-modify-macro incf (&optional (delta 1)) +
  "The first argument is some location holding a number.  This number is
  incremented by the second argument, DELTA, which defaults to 1.")


(define-modify-macro decf (&optional (delta 1)) -
  "The first argument is some location holding a number.  This number is
  decremented by the second argument, DELTA, which defaults to 1.")
#|
(defmacro putf (place indicator value &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or ().  This list is destructively altered so
  that (GETF place indicator) will find the specified newvalue.  Returns
  the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (val-temp (gensym)))
	 ((null d)
	  (push (list (car newval) getter) let-list)
	  (push (list ind-temp indicator) let-list)
	  (push (list val-temp value) let-list)
	  `(let* ,(nreverse let-list)
	     (setq ,(car newval)
		   (%primitive putf ,(car newval) ,ind-temp ,val-temp))
	     ,setter
	     ,val-temp))
      (push (list (car d) (car v)) let-list))))
|#


(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or ().  This list is destructively altered to
  remove the property specified by the indicator.  Returns T if such a
  property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
	  (push (list (car newval) getter) let-list)
	  (push (list ind-temp indicator) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))


;;; The built-in DEFSETFs.

(defsetf car %rplaca)
(defsetf cdr %rplacd)
(defsetf caar (x) (v) `(%rplaca (car ,x) ,v))
(defsetf cadr (x) (v) `(%rplaca (cdr ,x) ,v))
(defsetf cdar (x) (v) `(%rplacd (car ,x) ,v))
(defsetf cddr (x) (v) `(%rplacd (cdr ,x) ,v))
(defsetf caaar (x) (v) `(%rplaca (caar ,x) ,v))
(defsetf cadar (x) (v) `(%rplaca (cdar ,x) ,v))
(defsetf cdaar (x) (v) `(%rplacd (caar ,x) ,v))
(defsetf cddar (x) (v) `(%rplacd (cdar ,x) ,v))
(defsetf caadr (x) (v) `(%rplaca (cadr ,x) ,v))
(defsetf caddr (x) (v) `(%rplaca (cddr ,x) ,v))
(defsetf cdadr (x) (v) `(%rplacd (cadr ,x) ,v))
(defsetf cdddr (x) (v) `(%rplacd (cddr ,x) ,v))
(defsetf caaaar (x) (v) `(%rplaca (caaar ,x) ,v))
(defsetf cadaar (x) (v) `(%rplaca (cdaar ,x) ,v))
(defsetf cdaaar (x) (v) `(%rplacd (caaar ,x) ,v))
(defsetf cddaar (x) (v) `(%rplacd (cdaar ,x) ,v))
(defsetf caadar (x) (v) `(%rplaca (cadar ,x) ,v))
(defsetf caddar (x) (v) `(%rplaca (cddar ,x) ,v))
(defsetf cdadar (x) (v) `(%rplacd (cadar ,x) ,v))
(defsetf cdddar (x) (v) `(%rplacd (cddar ,x) ,v))
(defsetf caaadr (x) (v) `(%rplaca (caadr ,x) ,v))
(defsetf cadadr (x) (v) `(%rplaca (cdadr ,x) ,v))
(defsetf cdaadr (x) (v) `(%rplacd (caadr ,x) ,v))
(defsetf cddadr (x) (v) `(%rplacd (cdadr ,x) ,v))
(defsetf caaddr (x) (v) `(%rplaca (caddr ,x) ,v))
(defsetf cadddr (x) (v) `(%rplaca (cdddr ,x) ,v))
(defsetf cdaddr (x) (v) `(%rplacd (caddr ,x) ,v))
(defsetf cddddr (x) (v) `(%rplacd (cdddr ,x) ,v))

(defsetf first %rplaca)
(defsetf second (x) (v) `(%rplaca (cdr ,x) ,v))
(defsetf third (x) (v) `(%rplaca (cddr ,x) ,v))
(defsetf fourth (x) (v) `(%rplaca (cdddr ,x) ,v))
(defsetf fifth (x) (v) `(%rplaca (cddddr ,x) ,v))
(defsetf sixth (x) (v) `(%rplaca (cdr (cddddr ,x)) ,v))
(defsetf seventh (x) (v) `(%rplaca (cddr (cddddr ,x)) ,v))
(defsetf eighth (x) (v) `(%rplaca (cdddr (cddddr ,x)) ,v))
(defsetf ninth (x) (v) `(%rplaca (cddddr (cddddr ,x)) ,v))
(defsetf tenth (x) (v) `(%rplaca (cdr (cddddr (cddddr ,x))) ,v))
(defsetf rest %rplacd)

(defsetf elt %setelt)
(defsetf aref %aset)
(defsetf svref %svset)
(defsetf char %charset)
(defsetf bit %bitset)
(defsetf schar %scharset)
(defsetf sbit %sbitset)
(defsetf symbol-value set)
(defsetf symbol-function %sp-set-definition)
(defsetf symbol-plist %sp-set-plist)
(defsetf documentation %set-documentation)
(defsetf nth %setnth)
(defsetf fill-pointer %set-fill-pointer)
(defsetf search-list %set-search-list)


(define-setf-method getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
		       (foo-get-setf-method place env)
    (let ((newval (gensym))
	  (ptemp (gensym))
	  (def-temp (gensym)))
      (values `(,@temps ,(car stores) ,ptemp ,@(if default `(,def-temp)))
	      `(,@values ,get ,prop ,@(if default `(,default)))
	      `(,newval)
	      `(progn (setq ,(car stores)
			    (%primitive putf ,(car stores) ,ptemp ,newval))
		      ,set
		      ,newval)
	      `(getf ,(car stores) ,ptemp ,@(if default `(,def-temp)))))))

(define-setf-method get (symbol prop &optional default)
  "Get turns into %put. Don't put in the default unless it really is supplied and 
  non-nil, so that we can transform into the get instruction whenever possible."
  (let ((symbol-temp (gensym))
	(prop-temp (gensym))
	(def-temp (gensym))
	(newval (gensym)))
    (values `(,symbol-temp ,prop-temp ,@(if default `(,def-temp)))
	    `(,symbol ,prop ,@(if default `(,default)))
	    (list newval)
	    `(%put ,symbol-temp ,prop-temp ,newval)
	    `(get ,symbol-temp ,prop-temp ,@(if default `(,def-temp))))))

(define-setf-method gethash (key hashtable &optional default)
  (let ((key-temp (gensym))
	(hashtable-temp (gensym))
	(default-temp (gensym))
	(new-value-temp (gensym)))
    (values
     `(,key-temp ,hashtable-temp ,@(if default `(,default-temp)))
     `(,key ,hashtable ,@(if default `(,default)))
     `(,new-value-temp)
     `(%puthash ,key-temp ,hashtable-temp ,new-value-temp)
     `(gethash ,key-temp ,hashtable-temp ,@(if default `(,default-temp))))))

(defsetf subseq (sequence start &optional (end nil)) (v)
  `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
	  ,v))


;;; Evil hack invented by the gnomes of Vassar Street.  The function
;;; arg must be constant.  Get a setf method for this function, pretending
;;; that the final (list) arg to apply is just a normal arg.  If the
;;; setting and access forms produced in this way reference this arg at
;;; the end, then just splice the APPLY back onto the front and the right
;;; thing happens.

(define-setf-method apply (function &rest args &environment env)
  (if (and (listp function)
	   (= (list-length function) 2)
	   (eq (first function) 'function)
	   (symbolp (second function)))
      (setq function (second function))
      (error
       "Setf of Apply is only defined for function args of form #'symbol."))
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method (cons function args) env)
    ;; Special case aref and svref.
    (cond ((or (eq function 'aref) (eq function 'svref))
	   (let ((nargs (subseq setter 0 (1- (length setter))))
		 (fcn (if (eq function 'aref) 'lisp::%apply-aset 'lisp::%apply-svset)))
	     (values dummies vals newval
		     `(apply (function ,fcn) ,(car newval) ,@(cdr nargs))
		     `(apply (function ,function) ,@(cdr getter)))))
	  ;; Make sure the place is one that we can handle.
	  (T (unless (and (eq (car (last args)) (car (last vals)))
			  (eq (car (last getter)) (car (last dummies)))
			  (eq (car (last setter)) (car (last dummies))))
	       (error "Apply of ~S not understood as a location for Setf."
		      function))
	     (values dummies vals newval
		     `(apply (function ,(car setter)) ,@(cdr setter))
		     `(apply (function ,(car getter)) ,@(cdr getter)))))))


(define-setf-method ldb (bytespec place &environment env)
  "The first argument is a byte specifier.  The second is any place form
  acceptable to SETF.  Replaces the specified byte of the number in this
  place with bits from the low-order end of the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values (cons btemp dummies)
	      (cons bytespec vals)
	      (list gnuval)
	      `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
		 ,setter
		 ,gnuval)
	      `(ldb ,btemp ,getter)))))


(define-setf-method mask-field (bytespec place &environment env)
  "The first argument is a byte specifier.  The second is any place form
  acceptable to SETF.  Replaces the specified byte of the number in this place
  with bits from the corresponding position in the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values (cons btemp dummies)
	      (cons bytespec vals)
	      (list gnuval)
	      `(let ((,(car newval) (deposit-field ,gnuval ,btemp ,getter)))
		 ,setter
		 ,gnuval)
	      `(mask-field ,btemp ,getter)))))


(define-setf-method char-bit (place bit-name &environment env)
  "The first argument is any place form acceptable to SETF.  Replaces the
  specified bit of the character in this place with the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values `(,@dummies ,btemp)
	      `(,@vals ,bit-name)
	      (list gnuval)
	      `(let ((,(car newval)
		      (set-char-bit ,getter ,btemp ,gnuval)))
		 ,setter
		 ,gnuval)
	      `(char-bit ,getter ,btemp)))))


(define-setf-method the (type place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
		       (foo-get-setf-method place env)
      (values dummies
	      vals
	      newval
	      (subst `(the ,type ,(car newval)) (car newval) setter)
	      `(the ,type ,getter))))



;;;; CASE, TYPECASE, & Friends.

(eval-when (compile load eval)

;;; CASE-BODY returns code for all the standard "case" macros.  Name is the
;;; macro name, and keyform is the thing to case on.  Multi-p indicates whether
;;; a branch may fire off a list of keys; otherwise, a key that is a list is
;;; interpreted in some way as a single key.  When multi-p, test is applied to
;;; the value of keyform and each key for a given branch; otherwise, test is
;;; applied to the value of keyform and the entire first element, instead of
;;; each part, of the case branch.  When errorp, no t or otherwise branch is
;;; permitted, and an ERROR form is generated.  When proceedp, it is an error
;;; to omit errorp, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing keyform to be set and retested.
;;;
(defun case-body (name keyform cases multi-p test errorp proceedp)
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (dolist (case cases)
      (cond ((atom case)
	     (error "~S -- Bad clause in ~S." case name))
	    ((memq (car case) '(t otherwise))
	     (if errorp
		 (error "No default clause allowed in ~S: ~S" name case)
		 (push `(t nil ,@(rest case)) clauses)))
	    ((and multi-p (listp (first case)))
	     (setf keys (append (first case) keys))
	     (push `((or ,@(mapcar #'(lambda (key)
				       `(,test ,keyform-value ',key))
				   (first case)))
		     nil ,@(rest case))
		   clauses))
	    (t
	     (push (first case) keys)
	     (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
		   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled all the
;;; cases.  Note: it is not necessary that the resulting code signal
;;; case-failure conditions, but that's what KMP's prototype code did.  We call
;;; CASE-BODY-ERROR, because of how closures are compiled.  RESTART-CASE has
;;; forms with closures that the compiler causes to be generated at the top of
;;; any function using the case macros, regardless of whether they are needed.
;;;
(defun case-body-aux (name keyform keyform-value clauses keys
		      errorp proceedp expected-type)
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
	 (cond
	  ,@(nreverse clauses)
	  ,@(if errorp
		`((t (error 'conditions::case-failure
			    :name ',name
			    :datum ,keyform-value
			    :expected-type ',expected-type
			    :possibilities ',keys))))))))

); eval-when

(defun case-body-error (name keyform keyform-value expected-type keys)
  (restart-case
      (error 'conditions::case-failure
	     :name name
	     :datum keyform-value
	     :expected-type expected-type
	     :possibilities keys)
    (store-value (value)
      :report (lambda (stream)
		(format stream "Supply a new value for ~S." keyform))
      :interactive read-evaluated-form
      value)))



(defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil))

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
  (case-body 'ecase keyform cases t 'eql t nil))

(defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil))

(defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t))

(defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep t nil))


;;;; ASSERT and CHECK-TYPE.

;;; ASSERT is written this way, to call ASSERT-ERROR, because of how closures
;;; are compiled.  RESTART-CASE has forms with closures that the compiler
;;; causes to be generated at the top of any function using ASSERT, regardless
;;; of whether they are needed.
;;;
(defmacro assert (test-form &optional places datum &rest arguments)
  "Signals an error if the value of test-form is nil.  Continuing from this
   error using the CONTINUE restart will allow the user to alter the value of
   some locations known to SETF, starting over with test-form.  Returns nil."
  `(loop
     (when ,test-form (return nil))
     (assert-error ',test-form ',places ,datum ,@arguments)
     ,@(mapcar #'(lambda (place)
		   `(setf ,place (assert-prompt ',place ,place)))
	       places)))

(defun assert-error (test-form places datum &rest arguments)
  (restart-case (if datum
		    (apply #'error datum arguments)
		    (simple-assertion-failure test-form))
    (continue ()
      :report (lambda (stream) (assert-report places stream))
      nil)))

(defun simple-assertion-failure (assertion)
  (error 'simple-type-error
	 :datum assertion
	 :expected-type nil ;this needs some work in next revision. -kmp
	 :format-string "The assertion ~S failed."
	 :format-arguments (list assertion)))

(defun assert-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}."
	      (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
		  ~%Do you want to supply a new value? "
		   name value)
	 (format *query-io* "~&Type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name) ;help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))


;;; CHECK-TYPE is written this way, to call CHECK-TYPE-ERROR, because of how
;;; closures are compiled.  RESTART-CASE has forms with closures that the
;;; compiler causes to be generated at the top of any function using
;;; CHECK-TYPE, regardless of whether they are needed.  Because it would be
;;; nice if this were cheap to use, and some things can't afford this excessive
;;; consing (e.g., READ-CHAR), we bend backwards a little.
;;;

(defmacro check-type (place type &optional type-string)
  "Signals an error of type type-error if the contents of place are not of the
   specified type.  If an error is signaled, this can only return if
   STORE-VALUE is invoked.  It will store into place and start over."
  (let ((place-value (gensym)))
    `(loop
       (let ((,place-value ,place))
	 (when (typep ,place-value ',type) (return nil))
	 (setf ,place
	       (check-type-error ',place ,place-value ',type ,type-string))))))

(defun check-type-error (place place-value type type-string)
  (restart-case (if type-string
		    (error 'simple-type-error
			   :datum place :expected-type type
			   :format-string
			   "The value of ~S is ~S, which is not ~A."
			   :format-arguments
			   (list place place-value type-string))
		    (error 'simple-type-error
			   :datum place :expected-type type
			   :format-string
			   "The value of ~S is ~S, which is not of type ~S."
			   :format-arguments
			   (list place place-value type)))
    (store-value (value)
      :report (lambda (stream)
		(format stream "Supply a new value of ~S."
			place))
      :interactive read-evaluated-form
      value)))

;;; READ-EVALUATED-FORM is used as the interactive method for restart cases
;;; setup by the Common Lisp "casing" (e.g., CCASE and CTYPECASE) macros
;;; and by CHECK-TYPE.
;;;
(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))


;;;; With-XXX

(defmacro with-open-file ((var &rest open-args) &body (forms decls))
  "Bindspec is of the form (Stream File-Name . Options).  The file whose
   name is File-Name is opened using the Options and bound to the variable
   Stream.  If the call to open is unsuccessful, the forms are not
   evaluated.  The Forms are executed, and when they terminate, normally or
   otherwise, the file is closed."
  (let ((abortp (gensym)))
    `(let ((,var (open ,@open-args))
	   (,abortp t))
       ,@decls
       (when ,var
	 (unwind-protect
	     (multiple-value-prog1
		 (progn ,@forms)
	       (setq ,abortp nil))
	   (close ,var :abort ,abortp))))))



(defmacro with-open-stream ((var stream) &body (forms decls))
  "The form stream should evaluate to a stream.  VAR is bound
   to the stream and the forms are evaluated as an implicit
   progn.  The stream is closed upon exit."
  (let ((abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       ,@decls
       (unwind-protect
	 (multiple-value-prog1
	  (progn ,@forms)
	  (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp))))))


(defmacro with-input-from-string ((var string &key index start end) &body (forms decls))
  "Binds the Var to an input stream that returns characters from String and
  executes the body.  See manual for details."
  `(let ((,var
	  ,(if end
	       `(make-string-input-stream ,string ,(or start 0) ,end)
	       `(make-string-input-stream ,string ,(or start 0)))))
     ,@decls
     (unwind-protect
       (progn ,@forms)
       (close ,var)
       ,@(if index `((setf ,index (string-input-stream-current ,var)))))))


(defmacro with-output-to-string ((var &optional string) &body (forms decls))
  "If *string* is specified, it must be a string with a fill pointer; 
   the output is incrementally appended to the string (as if by use of
   VECTOR-PUSH-EXTEND)."
  (if string
      `(let ((,var (make-fill-pointer-output-stream ,string)))
	 ,@decls
	 (unwind-protect
	   (progn ,@forms)
	   (close ,var)))
      `(let ((,var (make-string-output-stream)))
	 ,@decls
	 (unwind-protect
	   (progn ,@forms)
	   (close ,var))
	 (get-output-stream-string ,var))))


;;;; Iteration macros:

(defmacro loop (&rest body)
  "Executes the body repeatedly until the form is exited by a Throw or
  Return.  The body is surrounded by an implicit block with name NIL."
  (let ((tag (gensym)))
    `(block nil (tagbody ,tag ,@body (go ,tag)))))


(defmacro dotimes ((var count &optional (result nil)) &body body)
  (cond ((numberp count)
         `(do ((,var 0 (1+ ,var)))
              ((>= ,var ,count) ,result)
            ,@body))
        (t (let ((v1 (gensym)))
             `(do ((,var 0 (1+ ,var)) (,v1 ,count))
                  ((>= ,var ,v1) ,result)
                ,@body)))))


;;; We repeatedly bind the var instead of setting it so that we never give the
;;; var a random value such as NIL (which might conflict with a declaration).
;;; ### Might not be legal...
;;;
(defmacro dolist ((var list &optional (result nil)) &body body)
  (let ((n-list (gensym)))
    `(do ((,n-list ,list (cdr ,n-list)))
	 ((endp ,n-list)
	  (let ((,var nil))
	    (declare (ignorable ,var))
	    ,result))
       (let ((,var (car ,n-list)))
	 ,@body))))


(defmacro do (varlist endlist &body (body decls))
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct.  Each Var is initialized in parallel to the value of the
  specified Init form.  On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in paralell.  The Test is evaluated before
  each evaluation of the body Forms.  When the Test is true, the the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO.  A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an laternate exit mechanism."

  (do-do-body varlist endlist body decls 'let 'psetq 'do nil))


(defmacro do* (varlist endlist &body (body decls))
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct.  Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form.  On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any).  The Test is
  evaluated before each evaluation of the body Forms.  When the Test is true,
  the the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO.  A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an laternate exit mechanism."
  (do-do-body varlist endlist body decls 'let* 'setq 'do* nil))


;;;; Miscellaneous macros:

(defmacro locally (&rest forms)
  "A form providing a container for locally-scoped variables."
  `(let () ,@forms))

(defmacro psetq (&rest pairs)
  (do ((lets nil)
       (setqs nil)
       (pairs pairs (cddr pairs)))
      ((atom (cdr pairs))
       `(let ,(nreverse lets) (setq ,@(nreverse setqs))))
    (let ((gen (gensym)))
      (push `(,gen ,(cadr pairs)) lets)
      (push (car pairs) setqs)
      (push gen setqs))))

;;; ### Bootstrap hack...
;;; Restore defmacro processing to normal.
;;;
(eval-when (compile)
  (setq *bootstrap-defmacro* nil))


;;;; With-Compilation-Unit:

;;; True if we are within a With-Compilation-Unit form, which normally causes
;;; nested uses to be NOOPS.
;;;
(defvar *in-compilation-unit* nil)

;;; Count of the number of compilation units dynamically enclosed by the
;;; current active WITH-COMPILATION-UNIT that were unwound out of.
;;;
(defvar *aborted-compilation-units*)

(compiler-let ((*bootstrap-defmacro* :both))

;;; With-Compilation-Unit  --  Public
;;;
;;;
(defmacro with-compilation-unit (options &body body)
  (let ((force nil)
	(n-fun (gensym))
	(n-abort-p (gensym)))
    (when (oddp (length options))
      (error "Odd number of key/value pairs: ~S." options))
    (do ((opt options (cddr opt)))
	((null opt))
      (case (first opt)
	(:force
	 (setq force (second opt)))
	(t
	 (warn "Ignoring unknown option: ~S." (first opt)))))

    `(flet ((,n-fun () ,@body))
       (if (or ,force (not *in-compilation-unit*))
	   (let ((c::*undefined-warnings* nil)
		 (c::*compiler-error-count* 0)
		 (c::*compiler-warning-count* 0)
		 (c::*compiler-note-count* 0)
		 (*in-compilation-unit* t)
		 (*aborted-compilation-units* 0)
		 (,n-abort-p t))
	     (handler-bind ((c::parse-unknown-type
			     #'(lambda (c)
				 (c::note-undefined-reference
				  (c::parse-unknown-type-specifier c)
				  :type))))
	       (unwind-protect
		   (multiple-value-prog1
		       (,n-fun)
		     (setq ,n-abort-p nil))
		 (c::print-summary ,n-abort-p *aborted-compilation-units*))))
	   (let ((,n-abort-p t))
	     (unwind-protect
		 (multiple-value-prog1
		     (,n-fun)
		   (setq ,n-abort-p nil))
	       (when ,n-abort-p
		 (incf *aborted-compilation-units*))))))))
); Compiler-Let
