;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Random types and macros used in writing the compiler.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(export '(lisp::with-compilation-unit) "LISP")

(export '(policy symbolicate def-ir1-translator def-source-transform
	  def-primitive-translator deftransform defknown defoptimizer
	  derive-type optimizer ltn-annotate ir2-convert attributes
	  def-boolean-attribute attributes-union attributes-intersection
	  attributes=))

(proclaim '(special *wild-type* *universal-type* *compiler-error-context*))

;;;; Deftypes:

;;;
;;; Should be standard:
(deftype boolean () '(member t nil))

;;;
;;; Inlinep is used to determine how a function is called.  The values have
;;; these meanings:
;;;        Nil	No declaration seen: do whatever you feel like, but don't dump
;;;		an inline expansion.
;;;
;;; :Notinline  Notinline declaration seen: always do full function call.
;;;
;;;    :Inline	Inline declaration seen: save expansion, expanding to it if
;;;		policy favors.
;;; 
;;; :Maybe-Inline
;;;		Retain expansion, but only use it opportunistically.
;;;
(deftype inlinep () '(member :inline :maybe-inline :notinline nil))


;;;; The Policy macro:

(proclaim '(special *lexical-environment*))

(eval-when (#-new-compiler compile load eval)
(defconstant policy-parameter-slots
  '((speed . cookie-speed) (space . cookie-space) (safety . cookie-safety)
    (cspeed . cookie-cspeed) (brevity . cookie-brevity)
    (debug . cookie-debug)))

;;; Find-Used-Parameters  --  Internal
;;;
;;;    Find all the policy parameters which are actually mentioned in Stuff,
;;; returning the names in a list.  We assume everything is evaluated.
;;;
(defun find-used-parameters (stuff)
  (if (atom stuff)
      (if (assoc stuff policy-parameter-slots) (list stuff) ())
      (collect ((res () nunion))
	(dolist (arg (cdr stuff) (res))
	  (res (find-used-parameters arg))))))

); Eval-When (Compile Load Eval)

;;; Policy  --  Public
;;;
;;;    This macro provides some syntactic sugar for querying the settings of
;;; the compiler policy parameters.
;;;
(defmacro policy (node &rest conditions)
  "Policy Node Condition*
  Test whether some conditions apply to the current compiler policy for Node.
  Each condition is a predicate form which accesses the policy values by
  referring to them as the variables SPEED, SPACE, SAFETY, CSPEED, BREVITY and
  DEBUG.  The results of all the conditions are combined with AND and returned
  as the result.

  Node is a form which is evaluated to obtain the node which the policy is for.
  If Node is NIL, then we use the current policy as defined by *default-cookie*
  and *current-cookie*.  This option is only well defined during IR1
  conversion."
  (let* ((form `(and ,@conditions))
	 (n-cookie (gensym))
	 (binds (mapcar
		 #'(lambda (name)
		     (let ((slot (cdr (assoc name policy-parameter-slots))))
		       `(,name (,slot ,n-cookie))))
		 (find-used-parameters form))))
    `(let* ((,n-cookie (lexenv-cookie
			,(if node
			     `(node-lexenv ,node)
			     '*lexical-environment*)))
	    ,@binds)
       ,form)))


;;;; Source-hacking defining forms:

(eval-when (#-new-compiler compile load eval)

;;; Symbolicate  --  Interface
;;;
;;;    Concatenate together the names of some strings and symbols, producing
;;; a symbol in the current package.
;;;
(proclaim '(function symbolicate (&rest (or string symbol)) symbol))
(defun symbolicate (&rest things)
  (values (intern (reduce #'(lambda (x y)
			      (concatenate 'string (string x) (string y)))
			  things))))

); Eval-When (Compile Load Eval)

;;; SPECIAL-FORM-FUNCTION  --  Internal
;;;
;;;    This function is stored in the SYMBOL-FUNCTION of special form names so
;;; that they are FBOUND.
;;;
(defun special-form-function (&rest stuff)
  (declare (ignore stuff))
  (error "Can't funcall the SYMBOL-FUNCTION of special forms."))

;;; Def-IR1-Translator  --  Interface
;;;
;;;    Parse defmacro style lambda-list, setting things up so that a compiler
;;; error happens if the syntax is invalid.
;;;
(defmacro def-ir1-translator (name (lambda-list start-var cont-var
						&key (kind :special-form))
				   &body body)
  "Def-IR1-Translator Name (Lambda-List Start-Var Cont-Var {Key Value}*)
                      [Doc-String] Form*
  Define a function that converts a Special-Form or other magical thing into
  IR1.  Lambda-List is a defmacro style lambda list.  Start-Var and Cont-Var
  are bound to the start and result continuations for the resulting IR1.
  This keyword is defined:
      Kind
          The function kind to associate with Name (default :special-form)."
  (let ((fn-name (symbolicate "IR1-CONVERT-" name))
	(n-form (gensym))
	(n-env (gensym)))
    (multiple-value-bind
	(body decls doc)
	(lisp::parse-defmacro lambda-list n-form body name 'special-form
			      :doc-string-allowed t
			      :environment n-env)
      `(progn
	 (proclaim '(function ,fn-name (continuation continuation t) void))
	 (defun ,fn-name (,start-var ,cont-var ,n-form)
	   (let ((,n-env *lexical-environment*))
	     ,@decls
	     (macrolet ((error (&rest args)
			       `(compiler-error ,@args)))
	       ,body)))
	 ,@(when doc
	     `((setf (documentation ',name 'function) ,doc)))
	 (setf (info function ir1-convert ',name) #',fn-name)
	 (setf (info function kind ',name) ,kind)
	 #+new-compiler
	 ,@(when (eq kind :special-form)
	     `((setf (symbol-function ',name) #'special-form-function)))))))


;;; Def-Source-Transform  --  Interface
;;;
;;;    Similar to Def-IR1-Translator, except that we pass if the syntax is
;;; invalid.
;;;
(defmacro def-source-transform (name lambda-list &body body)
  "Def-Source-Transform Name Lambda-List Form*
  Define a macro-like source-to-source transformation for the function Name.
  A source transform may \"pass\" by returning a non-nil second value.  If the
  transform passes, then the form is converted as a normal function call.  If
  the supplied arguments are not compatible with the specified lambda-list,
  then the transform automatically passes.
  
  Source-Transforms may only be defined for functions.  Source transformation
  is not attempted if the function is declared Notinline.  Source transforms
  should not examine their arguments.  If it matters how the function is used,
  then Deftransform should be used to define an IR1 transformation.
  
  If the desirability of the transformation depends on the current Optimize
  parameters, then the Policy macro should be used to determine when to pass."
  (let ((fn-name (symbolicate "SOURCE-TRANSFORM-" name))
	(n-form (gensym))
	(n-env (gensym)))
    (multiple-value-bind
	(body decls)
	(lisp::parse-defmacro lambda-list n-form body name
			      'def-source-transform
			      :environment n-env)
      `(progn
	 (defun ,fn-name (,n-form)
	   (let ((,n-env *lexical-environment*))
	     ,@decls
	     (macrolet ((error (&rest stuff)
			       (declare (ignore stuff))
			       `(return-from ,',fn-name (values nil t))))
	       ,body)))
	 (setf (info function source-transform ',name) #',fn-name)))))


(defmacro def-primitive-translator (name lambda-list &body body)
  "Def-Primitive-Translator Name Lambda-List Form*
  Define a function that converts a use of (%PRIMITIVE Name ...) into Lisp
  code.  Lambda-List is a defmacro style lambda list."
  (let ((fn-name (symbolicate "PRIMITIVE-TRANSLATE-" name))
	(n-form (gensym))
	(n-env (gensym)))
    (multiple-value-bind
	(body decls)
	(lisp::parse-defmacro lambda-list n-form body name
			      'def-primitive-translator
			      :environment n-env)
      `(progn
	 (defun ,fn-name (,n-form)
	   (let ((,n-env *lexical-environment*))
	     ,@decls
	     (macrolet ((error (&rest args)
			       `(compiler-error ,@args)))
	       ,body)))
	 (setf (gethash ',name *primitive-translators*) ',fn-name)))))


;;;; Lambda-list parsing utilities:
;;;
;;;    IR1 transforms, optimizers and type inferencers need to be able to parse
;;; the IR1 representation of a function call using a standard function
;;; lambda-list.


(eval-when (#-new-compiler compile load eval)

;;; Parse-Deftransform  --  Internal
;;;
;;;    Given a deftransform style lambda-list, generate code that parses the
;;; arguments of a combination with respect to that lambda-list.  Body is the
;;; the list of forms which are to be evaluated within the bindings.  Args is
;;; the variable that holds list of argument continuations.  Error-Form is a
;;; form which is evaluated when the syntax of the supplied arguments is
;;; incorrect or a non-constant argument keyword is supplied.  Defaults and
;;; other gunk are ignored.  The second value is a list of all the arguments
;;; bound.  We make the variables IGNORABLE so that we don't have to manually
;;; declare them Ignore if their only purpose is to make the syntax work.
;;;
(proclaim '(function parse-deftransform (list list symbol t) list))
(defun parse-deftransform (lambda-list body args error-form)
  (multiple-value-bind (req opt restp rest keyp keys allowp)
		       (parse-lambda-list lambda-list)
    (let* ((min-args (length req))
	   (max-args (+ min-args (length opt)))
	   (n-keys (gensym)))
      (collect ((binds)
		(vars)
		(pos 0 +)
		(keywords))
	(dolist (arg req)
	  (vars arg)
	  (binds `(,arg (nth ,(pos) ,args)))
	  (pos 1))

	(dolist (arg opt)
	  (let ((var (if (atom arg) arg (first  arg))))
	    (vars var)
	    (binds `(,var (nth ,(pos) ,args)))
	    (pos 1)))

	(when restp
	  (vars rest)
	  (binds `(,rest (nthcdr ,(pos) ,args))))

	(dolist (spec keys)
	  (if (or (atom spec) (atom (first spec)))
	      (let* ((var (if (atom spec) spec (first spec)))
		     (key (intern (symbol-name var) "KEYWORD")))
		(vars var)
		(binds `(,var (find-keyword-continuation ,n-keys ,key)))
		(keywords key))
	      (let* ((head (first spec))
		     (var (second head))
		     (key (first head)))
		(vars var)
		(binds `(,var (find-keyword-continuation ,n-keys ,key)))
		(keywords key))))

	(let ((n-length (gensym))
	      (limited-legal (not (or restp keyp))))
	  (values
	   `(let ((,n-length (length ,args))
		  ,@(when keyp `((,n-keys (nthcdr ,(pos) ,args)))))
	      (unless (and
		       ,(if limited-legal
			    `(<= ,min-args ,n-length ,max-args)
			    `(<= ,min-args ,n-length))
		       ,@(when keyp
			   (if allowp
			       `((check-keywords-constant ,n-keys))
			       `((check-transform-keys ,n-keys ',(keywords))))))
		,error-form)
	      (let ,(binds)
		;;; ### Bootstrap hack...
		#+new-compiler
		(declare (ignorable ,@(vars)))
		#-new-compiler
		(progn ,@(vars))
		,@body))
	   (vars)))))))

); Eval-When (Compile Load Eval)


;;;; Utilities used at run-time for parsing keyword args in IR1:

;;; Find-Keyword-Continuation  --  Internal
;;;
;;;    This function is used by the result of Parse-Deftransform to find the
;;; continuation for the value of the keyword argument Key in the list of
;;; continuations Args.  It returns the continuation if the keyword is present,
;;; or NIL otherwise.  The legality and constantness of the keywords should
;;; already have been checked. 
;;;
(proclaim '(function find-keyword-continuation (list keyword) (or continuation null)))
(defun find-keyword-continuation (args key)
  (do ((arg args (cddr arg)))
      ((null arg) nil)
    (when (eq (continuation-value (first arg)) key)
      (return (second arg)))))


;;; Check-Keywords-Constant  --  Internal
;;;
;;;    This function is used by the result of Parse-Deftransform to verify that
;;; alternating continuations in Args are constant and that there is an even
;;; number of args.
;;;
(proclaim '(function check-keywords-constant (list) boolean))
(defun check-keywords-constant (args)
  (do ((arg args (cddr arg)))
      ((null arg) t)
    (unless (and (rest arg)
		 (constant-continuation-p (first arg)))
      (return nil))))


;;; Check-Transform-Keys  --  Internal
;;;
;;;    This function is used by the result of Parse-Deftransform to verify that
;;; the list of continuations Args is a well-formed keyword arglist and that
;;; only keywords present in the list Keys are supplied.
;;;
(proclaim '(function check-transform-keys (list list) boolean))
(defun check-transform-keys (args keys)
  (and (check-keywords-constant args)
       (do ((arg args (cddr arg)))
	   ((null arg) t)
	 (unless (member (continuation-value (first arg)) keys)
	   (return nil)))))


;;;; Deftransform:

;;; Deftransform  --  Interface
;;;
;;;    Parse the lambda-list and generate code to test the policy and
;;; automatically create the result lambda.
;;;
(defmacro deftransform (name (lambda-list &optional (arg-types '*) (result-type '*)
					  &key result policy node defun-only)
			     &body body)
  "Deftransform Name (Lambda-List [Arg-Types] [Result-Type] {Key Value}*)
               Declaration* Form*
  Define an IR1 transformation for Name.  An IR1 transformation computes a
  lambda that replaces the function variable reference for the call.  A
  transform may pass (decide not to transform the call) by calling the Give-Up
  function.  Lambda-List both determines how the current call is parsed and
  specifies the Lambda-List for the resulting lambda.

  We parse the call and bind each of the lambda-list variables to the
  continuation which represents the value of the argument.  When parsing the
  call, we ignore the defaults, and always bind the variables for unsupplied
  arguments to NIL.  If a required argument is missing, an unknown keyword is
  supplied, or an argument keyword is not a constant, then the transform
  automatically passes.  The Declarations apply to the bindings made by
  Deftransform at transformation time, rather than to the variables of the
  resulting lambda.  Bound-but-not-referenced warnings are suppressed for the
  lambda-list variables.

  Normally, the body evaluates to a form which becomes the body of an
  automatically constructed lambda.  We make Lambda-List the lambda-list for
  the lambda, and automatically insert declarations of the argument and result
  types.  If the second value of the body is non-null, then it is a list of
  declarations which are to be inserted at the head of the lambda.  Automatic
  lambda generation may be inhibited by explicitly returning a lambda from the
  body.

  The Arg-Types and Result-Type are used to create a function type which the
  call must satisfy before transformation is attempted.  The function type
  specifier is constructed by wrapping (FUNCTION ...) around these values, so
  the lack of a restriction may be specified by omitting the argument or
  supplying *.  The argument syntax specified in the Arg-Types need not be the
  same as that in the Lambda-List, but the transform will never happen if
  the syntaxes can't be satisfied simultaneously.  If there is an existing
  transform for the same function that has the same type, then it is replaced
  with the new definition.

  These are the legal keyword options:
    :Result - A variable which is bound to the result continuation.
    :Node   - A variable which is bound to the combination node for the call.
    :Policy - A form which is supplied to the Policy macro to determine whether
              this transformation is appropriate.  If the result is false, then
              the transform automatically passes.
    :Defun-Only
            - Don't actually instantiate a transform, instead just DEFUN
              Name with the specified transform definition function.  This may
              be later instantiated with %Deftransform."

  (let ((n-args (gensym))
	(n-node (or node (gensym)))
	(n-decls (gensym))
	(n-lambda (gensym)))
    (multiple-value-bind (parsed-form vars)
			 (parse-deftransform
			  lambda-list
			  (if policy
			      `((unless (policy ,n-node ,policy) (give-up))
				,@body)
			      body)
			  n-args '(give-up))
      (let ((stuff
	     `((,n-node)
	       (let* ((,n-args (basic-combination-args ,n-node))
		      ,@(when result
			  `((,result (node-cont ,n-node)))))
		 (multiple-value-bind (,n-lambda ,n-decls)
				      ,parsed-form
		   (if (and (consp ,n-lambda) (eq (car ,n-lambda) 'lambda))
		       ,n-lambda
		       `(lambda ,',lambda-list
			  (declare (ignorable ,@',vars))
			  ,@,n-decls
			  ,,n-lambda)))))))
	(if defun-only
	    `(defun ,name ,@stuff)
	    `(%deftransform
	      ',name
	      '(function ,arg-types ,result-type)
	      #'(lambda ,@stuff)))))))

;;;; Defknown, Defoptimizer:

;;; Defknown  --  Interface
;;;
;;;    This macro should be the way that all implementation independent
;;; information about functions is made known to the compiler.
;;;
(defmacro defknown (name arg-types result-type &optional (attributes '(any))
			 &rest keys)
  "Defknown Name Arg-Types Result-Type [Attributes] {Key Value}* 
  Declare the function Name to be a known function.  We construct a type
  specifier for the function by wrapping (FUNCTION ...) around the Arg-Types
  and Result-Type.  Attributes is a an unevaluated list of the boolean
  attributes that the function has.  These attributes are meaningful here:
      call
         May call functions that are passed as arguments.  In order to determine
         what other effects are present, we must find the effects of all arguments
         that may be functions.
        
      unsafe
         May incorporate arguments in the result or somehow pass them upward.
        
      unwind
         May fail to return during correct execution.  Errors are O.K.
        
      any
         The (default) worst case.  Includes all the other bad things, plus any
         other possible bad thing.
        
      foldable
         May be constant-folded.  The function has no side effects, but may be
         affected by side effects on the arguments.  e.g. SVREF, MAPC.
        
      flushable
         May be eliminated if value is unused.  The function has no side effects
         except possibly CONS.  If a function is defined to signal errors, then
         it is not flushable even if it is movable or foldable.
        
      movable
         May be moved with impunity.  Has no side effects except possibly CONS,
         and is affected only by its arguments.

      predicate
          A true predicate likely to be open-coded.  This is a hint to IR1
	  conversion that it should ensure calls always appear as an IF test.
	  Not usually specified to Defknown, since this is implementation
	  dependent, and is usually automatically set by the Define-VOP
	  :Conditional option.

  Name may also be a list of names, in which case the same information is given
  to all the names.  The keywords specify the initial values for various
  optimizers that the function might have."
  (when (and (intersection attributes '(any call unwind))
	     (intersection attributes '(movable)))
    (error "Function cannot have both good and bad attributes: ~S" attributes))
  
  `(%defknown ',(if (and (consp name)
			 (not (eq (car name) 'setf)))
		    name
		    (list name))
	      '(function ,arg-types ,result-type)
	      (ir1-attributes ,@(if (member 'any attributes)
				    (union '(call unsafe unwind) attributes)
				    attributes))
	      ,@keys))


;;; Defoptimizer  --  Interface
;;;
;;;    Create a function which parses combination args according to a
;;; Lambda-List, optionally storing it in a function-info slot.
;;;
(defmacro defoptimizer (what (lambda-list &optional (n-node (gensym))
					  &rest vars)
			     &body body)
  "Defoptimizer (Function Kind) (Lambda-List [Node-Var] Var*)
                Declaration* Form*
  Define some Kind of optimizer for the named Function.  Function must be a
  known function.  Lambda-List is used to parse the arguments to the
  combination as in Deftransform.  If the argument syntax is invalid or there
  are non-constant keys, then we simply return NIL.

  The function is DEFUN'ed as Function-Kind-OPTIMIZER.  Possible kinds are
  DERIVE-TYPE, OPTIMIZER, LTN-ANNOTATE and IR2-CONVERT.  If a symbol is
  specified instead of a (Function Kind) list, then we just do a DEFUN with the
  symbol as its name, and don't do anything with the definition.  This is
  useful for creating optimizers to be passed by name to DEFKNOWN.

  If supplied, Node-Var is bound to the combination node being optimized.  If
  additional Vars are supplied, then they are used as the rest of the optimizer
  function's lambda-list.  LTN-ANNOTATE methods are passed an additional POLICY
  argument, and IR2-CONVERT methods are passed an additional IR2-BLOCK
  argument."

  (let ((name (if (symbolp what) what
		  (symbolicate (first what) "-" (second what) "-OPTIMIZER"))))

    (let ((n-args (gensym)))
      `(progn
	(defun ,name (,n-node ,@vars)
	  (let ((,n-args (basic-combination-args ,n-node)))
	    ,(parse-deftransform lambda-list body n-args
				 `(return-from ,name nil))))
	,@(when (consp what)
	    `((setf (,(symbolicate "FUNCTION-INFO-" (second what))
		     (function-info-or-lose ',(first what)))
		    #',name)))))))

 
;;;; IR groveling macros:

;;; Do-Blocks, Do-Blocks-Backwards  --  Interface
;;;    
(defmacro do-blocks ((block-var component &optional ends result) &body body)
  "Do-Blocks (Block-Var Component [Ends] [Result-Form]) {Declaration}* {Form}*
  Iterate over the blocks in a component, binding Block-Var to each block in
  turn.  The value of Ends determines whether to iterate over dummy head and
  tail blocks:
    NIL   -- Skip Head and Tail (the default)
    :Head -- Do head but skip tail
    :Tail -- Do tail but skip head
    :Both -- Do both head and tail

  If supplied, Result-Form is the value to return."
  (unless (member ends '(nil :head :tail :both))
    (error "Losing Ends value: ~S." ends))
  (let ((n-component (gensym))
	(n-tail (gensym)))
    `(let* ((,n-component ,component)
	    (,n-tail ,(if (member ends '(:both :tail))
			  nil
			  `(component-tail ,n-component))))
       (do ((,block-var ,(if (member ends '(:both :head))
			     `(component-head ,n-component)
			     `(block-next (component-head ,n-component)))
	                (block-next ,block-var)))
	   ((eq ,block-var ,n-tail) ,result)
	 ,@body))))
;;;
(defmacro do-blocks-backwards ((block-var component &optional ends result) &body body)
  "Do-Blocks-Backwards (Block-Var Component [Ends] [Result-Form]) {Declaration}* {Form}*
  Like Do-Blocks, only iterate over the blocks in reverse order."
  (unless (member ends '(nil :head :tail :both))
    (error "Losing Ends value: ~S." ends))
  (let ((n-component (gensym))
	(n-head (gensym)))
    `(let* ((,n-component ,component)
	    (,n-head ,(if (member ends '(:both :head))
			  nil
			  `(component-head ,n-component))))
       (do ((,block-var ,(if (member ends '(:both :tail))
			     `(component-tail ,n-component)
			     `(block-prev (component-tail ,n-component)))
	                (block-prev ,block-var)))
	   ((eq ,block-var ,n-head) ,result)
	 ,@body))))


;;; Do-Uses  --  Interface
;;;
;;;    Could change it not to replicate the code someday perhaps...
;;;
(defmacro do-uses ((node-var continuation &optional result) &body body)
  "Do-Uses (Node-Var Continuation [Result]) {Declaration}* {Form}*
  Iterate over the uses of Continuation, binding Node to each one succesively."
  (once-only ((n-cont continuation))
    `(ecase (continuation-kind ,n-cont)
       (:unused)
       (:inside-block 
	(block nil
	  (let ((,node-var (continuation-use ,n-cont)))
	    ,@body
	    ,result)))
       ((:block-start :deleted-block-start)
	(dolist (,node-var (block-start-uses (continuation-block ,n-cont))
			   ,result)
	  ,@body)))))


;;; Do-Nodes, Do-Nodes-Backwards  --  Interface
;;;
;;;    In the forward case, we terminate on Last-Cont so that we don't have to
;;; worry about our termination condition being changed when new code is added
;;; during the iteration.  In the backward case, we do NODE-PREV before
;;; evaluating the body so that we can keep going when the current node is
;;; deleted.
;;;
(defmacro do-nodes ((node-var cont-var block &optional result) &body body)
  "Do-Nodes (Node-Var Cont-Var Block [Result]) {Declaration}* {Form}*
  Iterate over the nodes in Block, binding Node-Var to the each node and
  Cont-Var to the node's Cont."
  (let ((n-block (gensym))
	(n-last-cont (gensym)))
    `(let* ((,n-block ,block)
	    (,n-last-cont (node-cont (block-last ,n-block))))
       (do* ((,node-var (continuation-next (block-start ,n-block))
			(continuation-next ,cont-var))
	     (,cont-var (node-cont ,node-var) (node-cont ,node-var)))
	    (())
	 ,@body
	 (when (eq ,cont-var ,n-last-cont)
	   (return ,result))))))
;;;
(defmacro do-nodes-backwards ((node-var cont-var block &optional result)
			      &body body)
  "Do-Nodes-Backwards (Node-Var Cont-Var Block [Result]) {Declaration}* {Form}*
  Like Do-Nodes, only iterates in reverse order."
  (let ((n-block (gensym))
	(n-start (gensym))
	(n-last (gensym))
	(n-next (gensym)))
    `(let* ((,n-block ,block)
	    (,n-start (block-start ,n-block))
	    (,n-last (block-last ,n-block)))
       (do* ((,cont-var (node-cont ,n-last) ,n-next)
	     (,node-var ,n-last (continuation-use ,cont-var))
	     (,n-next (node-prev ,node-var) (node-prev ,node-var)))
	    (())
	 ,@body
	 (when (eq ,n-next ,n-start)
	   (return ,result))))))


;;; With-IR1-Environment  --  Interface
;;;
;;;    The lexical environment is presumably already null...
;;;
(defmacro with-ir1-environment (node &rest forms)
  "With-IR1-Environment Node Form*
  Bind the IR1 context variables so that IR1 conversion can be done after the
  main conversion pass has finished."
  (let ((n-node (gensym)))
    `(let* ((,n-node ,node)
	    (*current-component* (block-component (node-block ,n-node)))
	    (*lexical-environment* (node-lexenv ,n-node))
	    (*current-path* (node-source-path ,n-node)))
       ,@forms)))


;;; WITH-IR1-NAMESPACE  --  Interface
;;;
;;;    Bind the hashtables used for keeping track of global variables,
;;; functions, &c.
;;;
(defmacro with-ir1-namespace (&body forms)
  `(let ((*free-variables* (make-hash-table :test #'eq))
	 (*free-functions* (make-hash-table :test #'equal))
	 (*constants* (make-hash-table :test #'equal))
	 (*source-paths* (make-hash-table :test #'eq))
	 (*failed-optimizations* (make-hash-table :test #'eq)))
     ,@forms))


;;; LEXENV-FIND  --  Interface
;;;
(defmacro lexenv-find (name slot &key test)
  "LEXENV-FIND Name Slot {Key Value}*
  Look up Name in the lexical environment namespace designated by Slot,
  returning the <value, T>, or <NIL, NIL> if no entry.  The :TEST keyword
  may be used to determine the name equality predicate."
  (once-only ((n-res `(assoc ,name (,(symbolicate "LEXENV-" slot)
				    *lexical-environment*)
			     ,@(when test `(:test ,test)))))
    `(if ,n-res
	 (values (cdr ,n-res) t)
	 (values nil nil))))


;;;; The Defprinter macro:

(defvar *defprint-pretty* nil
  "If true, defprinter print functions print each slot on a separate line.")


;;; Defprinter-Prin1, Defprinter-Princ  --  Internal
;;;
;;;    These functions are called by the expansion of the Defprinter
;;; macro to do the actual printing.
;;;
(proclaim '(ftype (function (symbol t stream) void)
		  defprinter-prin1 defprinter-princ))
(defun defprinter-prin1 (name value stream &optional indent)
  (declare (ignore indent))
  (write-string "  " stream)
  (when *print-pretty*
    (xp:pprint-newline :linear stream))
  (princ name stream)
  (write-string "= " stream)
  (prin1 value stream))
;;;
(defun defprinter-princ (name value stream &optional indent)
  (declare (ignore indent))
  (write-string "  " stream)
  (when *print-pretty*
    (xp:pprint-newline :linear stream))
  (princ name stream)
  (write-string "= " stream)
  (princ value stream))

(defmacro defprinter (name &rest slots)
  "Defprinter Name Slot-Desc*
  Define some kind of reasonable defstruct structure-print function.  Name
  is the name of the structure.  We define a function %PRINT-name which
  prints the slots in the structure in the way described by the Slot-Descs.
  Each Slot-Desc can be a slot name, indicating that the slot should simply
  be printed.  A Slot-Desc may also be a list of a slot name and other stuff.
  The other stuff is composed of keywords followed by expressions.  The
  expressions are evaluated with the variable which is the slot name bound
  to the value of the slot.  These keywords are defined:
  
  :PRIN1    Print the value of the expression instead of the slot value.
  :PRINC    Like :PRIN1, only princ the value
  :TEST     Only print something if the test is true.
  
  If no printing thing is specified then the slot value is printed as PRIN1.
  
  The structure being printed is bound to Structure and the stream is bound to
  Stream."
  
  (flet ((sref (slot) `(,(symbolicate name "-" slot) structure)))
    (collect ((prints))
      (dolist (slot slots)
	(if (atom slot)
	    (prints `(defprinter-prin1 ',slot ,(sref slot) stream))
	    (let ((sname (first slot))
		  (test t))
	      (collect ((stuff))
		(do ((option (rest slot) (cddr option)))
		    ((null option)
		     (prints		
		      `(let ((,sname ,(sref sname)))
			 (when ,test
			   ,@(or (stuff)
				 `((defprinter-prin1 ',sname ,sname
				     stream)))))))
		  (case (first option)
		    (:prin1
		     (stuff `(defprinter-prin1 ',sname ,(second option)
			       stream)))
		    (:princ
		     (stuff `(defprinter-princ ',sname ,(second option)
			       stream)))
		    (:test (setq test (second option)))
		    (t
		     (error "Losing Defprinter option: ~S."
			    (first option)))))))))
	     
	     `(defun ,(symbolicate "%PRINT-" name) (structure stream depth)
		(flet ((do-prints ()
			 ,@(prints)))
		  (cond (*print-pretty*
			 (xp:pprint-logical-block
			     (stream nil :prefix "#<" :suffix ">")
			   (prin1 ',name stream)
			   (write-char #\space stream)
			   (xp:pprint-indent :current 0 stream)
			   (let ((*print-base* 16)
				 (*print-radix* t))
			     (prin1 (system:%primitive make-fixnum structure)
				    stream))
			   (unless (and *print-level*
					(<= *print-level* (1+ depth)))
			     (do-prints))))
			(t
			 (format stream "#<~S ~X"
				 ',name
				 (system:%primitive make-fixnum structure))
			 (let ((*print-level*
				(if *print-level* (- *print-level* depth 1))))
			   (unless (and *print-level* (<= *print-level* 0))
			     (do-prints)))
			 (format stream ">"))))
		nil))))


;;;; Boolean attribute utilities:
;;;
;;;    We need to maintain various sets of boolean attributes for known
;;; functions and VOPs.  To save space and allow for quick set operations, we
;;; represent them as bits in a fixnum.
;;;

(deftype attributes () 'fixnum)

(eval-when (compile load eval)
;;; Compute-Attribute-Mask  --  Internal
;;;
;;;    Given a list of attribute names and an alist that translates them to
;;; masks, return the OR of the masks.
;;;
(defun compute-attribute-mask (names alist)
  (collect ((res 0 logior))
    (dolist (name names)
      (let ((mask (cdr (assoc name alist))))
	(unless mask
	  (error "Unknown attribute name: ~S." name))
	(res mask)))
    (res)))

); Eval-When (Compile Load Eval)

;;; Def-Boolean-Attribute  --  Interface
;;;
;;;    Parse the specification and generate some accessor macros.
;;;
(defmacro def-boolean-attribute (name &rest attribute-names)
  "Def-Boolean-Attribute Name Attribute-Name*
  Define a new class of boolean attributes, with the attributes havin the
  specified Attribute-Names.  Name is the name of the class, which is used to
  generate some macros to manipulate sets of the attributes: 

    NAME-attributep attributes attribute-name*
      Return true if one of the named attributes is present, false otherwise.
      When set with SETF, updates the place Attributes setting or clearing the
      specified attributes.

    NAME-attributes attribute-name*
      Return a set of the named attributes."

  (let ((const-name (symbolicate name "-ATTRIBUTE-TRANSLATIONS"))
	(test-name (symbolicate name "-ATTRIBUTEP")))
    (collect ((alist))
      (do ((mask 1 (ash mask 1))
	   (names attribute-names (cdr names)))
	  ((null names))
	(alist (cons (car names) mask)))
	 
      `(progn
	 (eval-when (compile load eval)
	   (defconstant ,const-name ',(alist)))
	 
	 (defmacro ,test-name (attributes &rest attribute-names)
	   "Automagically generated boolean attribute test function.  See
	    Def-Boolean-Attribute."
	   `(logtest ,(compute-attribute-mask attribute-names ,const-name)
		     (the attributes ,attributes)))

	 (define-setf-method ,test-name (place &rest attributes
					       &environment env)
	   
	   "Automagically generated boolean attribute setter.  See
	    Def-Boolean-Attribute."
	   (multiple-value-bind (temps values stores set get)
				(lisp::foo-get-setf-method place env)
	     (let ((newval (gensym))
		   (n-place (gensym))
		   (mask (compute-attribute-mask attributes ,const-name)))
	       (values `(,@temps ,n-place)
		       `(,@values ,get)
		       `(,newval)
		       `(let ((,(first stores)
			       (if ,newval
				   (logior ,n-place ,mask)
				   (logand ,n-place ,(lognot mask)))))
			  ,set
			  ,newval)
		       `(,',test-name ,n-place ,@attributes)))))
	 
	 (defmacro ,(symbolicate name "-ATTRIBUTES") (&rest attribute-names)
	   "Automagically generated boolean attribute creation function.  See
	    Def-Boolean-Attribute."
	   (compute-attribute-mask attribute-names ,const-name))))))


;;; Attributes-Union, Attributes-Intersection, Attributes=  --  Interface
;;;
;;;    And now for some gratuitous pseudo-abstraction...
;;;
(defmacro attributes-union (&rest attributes)
  "Returns the union of all the sets of boolean attributes which are its
  arguments." 
  `(the attributes
	(logior ,@(mapcar #'(lambda (x) `(the attributes ,x)) attributes))))
;;;
(defmacro attributes-intersection (&rest attributes)
  "Returns the intersection of all the sets of boolean attributes which are its
  arguments." 
  `(the attributes
	(logand ,@(mapcar #'(lambda (x) `(the attributes ,x)) attributes))))
;;;
(proclaim '(inline attributes=))
(proclaim '(function attributes= (attributes attributes) boolean))
(defun attributes= (attr1 attr2)
  "Returns true if the attributes present in Attr1 are indentical to those in
  Attr2."
  (eql attr1 attr2))


;;;; The Event statistics/trace utility:

(eval-when (#-new-compiler compile load eval)

(defstruct event-info
  ;;
  ;; The name of this event.
  (name nil :type symbol)
  ;;
  ;; The string rescribing this event.
  (description nil :type string)
  ;;
  ;; The name of the variable we stash this in.
  (var nil :type symbol)
  ;;
  ;; The number of times this event has happened.
  (count 0 :type fixnum)
  ;;
  ;; The level of significance of this event.
  (level nil :type unsigned-byte)
  ;;
  ;; If true, a function that gets called with the node that the event happened
  ;; to.
  (action nil :type (or function null)))


;;; A hashtable from event names to event-info structures.
;;;
(defvar *event-info* (make-hash-table :test #'eq))


;;; Event-Info-Or-Lose  --  Internal
;;;
;;;    Return the event info for Name or die trying.
;;;
(proclaim '(function event-info-or-lose (t) event-info))
(defun event-info-or-lose (name)
  (let ((res (gethash name *event-info*)))
    (unless res
      (error "~S is not the name of an event." name))
    res))

); Eval-When (Compile Load Eval)


;;; Event-Count, Event-Action, Event-Level  --  Interface
;;;
(proclaim '(function event-count (symbol) fixnum))
(defun event-count (name)
  "Return the number of times that Event has happened."
  (event-info-count (event-info-or-lose name)))
;;;
(proclaim '(function event-action (symbol) (or function null)))
(defun event-action (name)
  "Return the function that is called when Event happens.  If this is null,
  there is no action.  The function is passed the node to which the event
  happened, or NIL if there is no relevant node.  This may be set with SETF."
  (event-info-action (event-info-or-lose name)))
;;;
(proclaim '(function %set-event-action (symbol (or function null)) (or function null)))
(defun %set-event-action (name new-value)
  (setf (event-info-action (event-info-or-lose name))
	new-value))
;;;
(defsetf event-action %set-event-action)
;;;
(proclaim '(function event-level (symbol) unsigned-byte))
(defun event-level (name)
  "Return the non-negative integer which represents the level of significance
  of the event Name.  This is used to determine whether to print a message when
  the event happens.  This may be set with SETF."
  (event-info-level (event-info-or-lose name)))
;;;
(proclaim '(function %set-event-level (symbol unsigned-byte) unsigned-byte))
(defun %set-event-level (name new-value)
  (setf (event-info-level (event-info-or-lose name))
	new-value))
;;;
(defsetf event-level %set-event-level)


;;; Defevent  --  Interface
;;;
;;;    Make an event-info structure and stash it in a variable so we can get at
;;; it quickly.
;;;
(defmacro defevent (name description &optional (level 0))
  "Defevent Name Description
  Define a new kind of event.  Name is a symbol which names the event and
  Description is a string which describes the event.  Level (default 0) is the
  level of significance associated with this event; it is used to determine
  whether to print a Note when the event happens."
  (let ((var-name (symbolicate "*" name "-EVENT-INFO*")))
    `(eval-when (compile load eval)
       (defvar ,var-name
	 (make-event-info :name ',name :description ',description :var ',var-name
			  :level ,level))
       (setf (gethash ',name *event-info*) ,var-name)
       ',name)))

(proclaim '(type unsigned-byte *event-note-threshold*))
(defvar *event-note-threshold* 1
  "This variable is a non-negative integer specifying the lowest level of
  event that will print a Note when it occurs.")

;;; Event  --  Interface
;;;
;;;    Increment the counter and do any action.  Mumble about the event if
;;; policy indicates.
;;;
(defmacro event (name &optional node)
  "Event Name Node
  Note that the event with the specified Name has happened.  Node is evaluated
  to determine the node to which the event happened."
  `(%event ,(event-info-var (event-info-or-lose name)) ,node))
;;;
(proclaim '(function %event (event-info (or node null))))
(defun %event (info node)
  (incf (event-info-count info))
  (when (and (>= (event-info-level info) *event-note-threshold*)
	     (if node
		 (policy node (= brevity 0))
		 (policy nil (= brevity 0))))
    (let ((*compiler-error-context* node))
      (compiler-note (event-info-description info))))

  (let ((action (event-info-action info)))
    (when action (funcall action node))))


;;; Event-Statistics, Clear-Statistics  --  Interface
;;;
(proclaim '(function event-statistics (&optional unsigned-byte stream) void))
(defun event-statistics (&optional (min-count 1) (stream *standard-output*))
  "Print a listing of events and their counts, sorted by the count.  Events
  that happened fewer than Min-Count times will not be printed.  Stream is the
  stream to write to."
  (collect ((info))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (when (>= (event-info-count v) min-count)
		   (info v)))
	     *event-info*)
    (dolist (event (sort (info) #'> :key #'event-info-count))
      (format stream "~6D: ~A~%" (event-info-count event)
	      (event-info-description event)))
    (values)))
;;;
(proclaim '(function clear-statistics () void))
(defun clear-statistics ()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (setf (event-info-count v) 0))
	   *event-info*)
  (values))

