;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file implements local call analysis.  A local call is a function
;;; call between functions being compiled at the same time.  If we can tell at
;;; compile time that such a call is legal, then we change the combination
;;; to call the correct lambda, mark it as local, and add this link to our call
;;; graph.  Once a call is local, it is then eligible for let conversion, which
;;; places the body of the function inline.
;;;
;;;    We cannot always do a local call even when we do have the function being
;;; called.  Local call can be explicitly disabled by a NOTINLINE declaration.
;;; Calls that cannot be shown to have legal arg counts are also not converted.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Propagate-To-Args  --  Interface
;;;
;;;    This function propagates information from the variables in the function
;;; Fun to the actual arguments in Call.  This is also called by the VALUES IR1
;;; optimizer when it sleazily converts MV-BINDs to LETs.
;;;
;;;    We flush all arguments to Call that correspond to unreferenced variables
;;; in Fun.  We leave NILs in the Combination-Args so that the remaining args
;;; still match up with their vars.
;;;
;;;    We also apply the declared variable type assertion to the argument
;;; continuations.
;;;
(defun propagate-to-args (call fun)
  (declare (type combination call) (type clambda fun)) 
  (do ((args (basic-combination-args call) (cdr args))
       (vars (lambda-vars fun) (cdr vars)))
      ((null args))
    (let ((arg (car args))
	  (var (car vars)))
      (cond ((leaf-refs var)
	     (assert-continuation-type arg (leaf-type var)))
	    (t
	     (flush-dest arg)
	     (setf (car args) nil)))))
      
  (undefined-value))


;;; Convert-Call  --  Internal
;;;
;;;    Convert a combination into a local call.  We Propagate-To-Args, set the
;;; combination kind to :Local, add Fun to the Calls of the function that the
;;; call is in, then replace the function in the Ref node with the new
;;; function.
;;;
;;;    We change the Ref last, since changing the reference can trigger let
;;; conversion of the new function, but will only do so if the call is local.
;;;
(defun convert-call (ref call fun)
  (declare (type ref ref) (type combination call) (type clambda fun))
  (propagate-to-args call fun)
  (setf (basic-combination-kind call) :local)
  (pushnew fun (lambda-calls (node-home-lambda call)))
  (change-ref-leaf ref fun)
  (undefined-value))


;;;; External entry point creation:

;;; Make-XEP-Lambda  --  Internal
;;;
;;;    Return a Lambda form that can be used as the definition of the XEP for
;;; Fun.
;;;
;;;    If Fun is a lambda, then we check the number of arguments (conditional
;;; on policy) and call Fun with all the arguments.
;;;
;;;    If Fun is an Optional-Dispatch, then we dispatch off of the number of
;;; supplied arguments by doing do an = test for each entry-point, calling the
;;; entry with the appropriate prefix of the passed arguments.
;;;
;;;    If there is a more arg, then there are a couple of optimizations that we
;;; make (more for space than anything else):
;;; -- If Min-Args is 0, then we make the more entry a T clause, since no
;;;    argument count error is possible.
;;; -- We can omit the = clause for the last entry-point, allowing the case of
;;;    0 more args to fall through to the more entry.
;;;
;;;    We don't bother to policy conditionalize wrong arg errors in optional
;;; dispatches, since the additional overhead is negligible compared to the
;;; other hair going down.
;;;
;;;    Note that if policy indicates it, argument type declarations in Fun will
;;; be verified.  Since nothing is known about the type of the XEP arg vars,
;;; type checks will be emitted when the XEP's arg vars are passed to the
;;; actual function.
;;;
(defun make-xep-lambda (fun)
  (declare (type functional fun))
  (etypecase fun
    (clambda
     (let ((nargs (length (lambda-vars fun)))
	   (n-supplied (gensym)))
       (collect ((temps))
	 (dotimes (i nargs)
	   (temps (gensym)))
	 `(lambda (,n-supplied ,@(temps))
	    (declare (fixnum ,n-supplied))
	    ,(if (policy (lambda-bind fun) (zerop safety))
		 `(declare (ignore ,n-supplied))
		 `(%verify-argument-count ,n-supplied ,nargs))
	    (%funcall ,fun ,@(temps))))))
    (optional-dispatch
     (let* ((min (optional-dispatch-min-args fun))
	    (max (optional-dispatch-max-args fun))
	    (more (optional-dispatch-more-entry fun))
	    (n-supplied (gensym)))
       (collect ((temps)
		 (entries))
	 (dotimes (i max)
	   (temps (gensym)))

	 (do ((eps (optional-dispatch-entry-points fun) (rest eps))
	      (n min (1+ n)))
	     ((null eps))
	   (entries `((= ,n-supplied ,n)
		      (%funcall ,(first eps) ,@(subseq (temps) 0 n)))))

	 `(lambda (,n-supplied ,@(temps))
	    (declare (fixnum ,n-supplied))
	    (cond
	     ,@(if more (butlast (entries)) (entries))
	     ,@(when more
		 `((,(if (zerop min) 't `(>= ,n-supplied ,max))
		    ,(let ((n-context (gensym))
			   (n-count (gensym)))
		       `(multiple-value-bind
			    (,n-context ,n-count)
			    (%more-arg-context ,n-supplied ,max)
			  (%funcall ,more ,@(temps) ,n-context ,n-count))))))
	     (t
	      (%argument-count-error ,n-supplied)))))))))


;;; Make-External-Entry-Point  --  Internal
;;;
;;;    Make an external entry point (XEP) for Fun and return it.  We convert
;;; the result of Make-XEP-Lambda in the correct environment, then associate
;;; this lambda with Fun as its XEP.  After the conversion, we iterate over the
;;; function's associated lambdas, redoing local call analysis so that the XEP
;;; calls will get converted.
;;;
;;;    We set Reanalyze and Reoptimize in the component, just in case we
;;; discover an XEP after the initial local call analyze pass.
;;;
(defun make-external-entry-point (fun)
  (declare (type functional fun))
  (assert (not (functional-entry-function fun)))
  (with-ir1-environment (lambda-bind (main-entry fun))
    (let ((res (ir1-convert-lambda (make-xep-lambda fun))))
      (setf (functional-kind res) :external)
      (setf (leaf-ever-used res) t)
      (setf (functional-entry-function res) fun)
      (setf (functional-entry-function fun) res)
      (setf (component-reanalyze *current-component*) t)
      (setf (component-reoptimize *current-component*) t)
      (etypecase fun
	(clambda (local-call-analyze-1 fun))
	(optional-dispatch
	 (dolist (ep (optional-dispatch-entry-points fun))
	   (local-call-analyze-1 ep))
	 (when (optional-dispatch-more-entry fun)
	   (local-call-analyze-1 (optional-dispatch-more-entry fun)))))
      res)))


;;; Reference-Entry-Point  --  Internal
;;;
;;;    Notice a Ref that is not in a local-call context.  If the Ref is already
;;; to an XEP, then do nothing, otherwise change it to the XEP, making an XEP
;;; if necessary.
;;;
;;;    If Ref is to a special :Cleanup or :Escape function, then we treat it as
;;; though it was not an XEP reference (i.e. leave it alone.)
;;;
(defun reference-entry-point (ref)
  (declare (type ref ref))
  (let ((fun (ref-leaf ref)))
    (unless (or (external-entry-point-p fun)
		(member (functional-kind fun) '(:escape :cleanup)))
      (change-ref-leaf ref (or (functional-entry-function fun)
			       (make-external-entry-point fun))))))


;;; Local-Call-Analyze-1  --  Interface
;;;
;;;    Attempt to convert all references to Fun to local calls.  The reference
;;; cannot be :Notinline, and must be the function for a call.  The function
;;; continuation must be used only once, since otherwise we cannot be sure what
;;; function is to be called.  The call continuation would be multiply used if
;;; there is hairy stuff such as conditionals in the expression that computes
;;; the function.
;;;
;;;    Except in the interpreter, we don't attempt to convert calls that appear
;;; in a top-level lambda unless there is only one reference or the function is
;;; a unwind-protect cleanup.  This allows top-level components to contain only
;;; load-time code: any references to run-time functions will be as closures.
;;;
;;;    If we cannot convert a reference, then we mark the referenced function
;;; as an entry-point, creating a new XEP if necessary.
;;;
;;;    This is broken off from Local-Call-Analyze so that people can force
;;; analysis of newly introduced calls.  Note that we don't do let conversion
;;; here.
;;;
(defun local-call-analyze-1 (fun)
  (declare (type functional fun))
  (let ((refs (leaf-refs fun)))
    (dolist (ref refs)
      (let* ((cont (node-cont ref))
	     (dest (continuation-dest cont)))
	(cond ((and (basic-combination-p dest)
		    (eq (basic-combination-fun dest) cont)
		    (eq (continuation-use cont) ref)
		    (or (null (rest refs))
			*converting-for-interpreter*
			(eq (functional-kind fun) :cleanup)
			(not (eq (functional-kind (node-home-lambda ref))
				 :top-level))))
	       (ecase (ref-inlinep ref)
		 ((nil :inline)
		  (convert-call-if-possible ref dest))
		 ((:notinline)))
	       
	       (unless (eq (basic-combination-kind dest) :local)
		 (reference-entry-point ref)))
	      (t
	       (reference-entry-point ref))))))

  (undefined-value))


;;; Local-Call-Analyze  --  Interface
;;;
;;;    We examine all New-Functions in component, attempting to convert calls
;;; into local calls when it is legal.  We also attempt to convert each lambda
;;; to a let.  Let conversion is also triggered by deletion of a function
;;; reference, but functions that start out eligible for conversion must be
;;; noticed sometime.
;;;
;;;    Note that there is a lot of action going on behind the scenes here,
;;; triggered by reference deletion.  In particular, the Component-Lambdas are
;;; being hacked to remove newly deleted and let converted lambdas, so it is
;;; important that the lambda is added to the Component-Lambdas when it is.
;;;
(defun local-call-analyze (component)
  (declare (type component component))
  (loop
    (unless (component-new-functions component) (return))
    (let ((fun (pop (component-new-functions component))))
      (cond ((eq (functional-kind fun) :deleted))
	    ((and (null (leaf-refs fun))
		  (ecase (functional-kind fun)
		    ((nil :escape :cleanup) t)
		    ((:optional :top-level) nil)))
	     (delete-functional fun))
	    (t
	     (when (lambda-p fun)
	       (push fun (component-lambdas component)))
	     (local-call-analyze-1 fun)
	     (when (lambda-p fun)
	       (maybe-let-convert fun))))))

  (undefined-value))


;;; Convert-Call-If-Possible  --  Interface
;;;
;;;    Dispatch to the appropriate function to attempt to convert a call.  This
;;; is called in IR1 optimize as well as in local call analysis.  If the call
;;; is already :Local, we do nothing.  If the call is in the top-level
;;; component, also do nothing, since we don't want to join top-level code into
;;; normal components.
;;;
;;;    We bind *Compiler-Error-Context* to the node for the call so that
;;; warnings will get the right context.
;;;
(defun convert-call-if-possible (ref call)
  (declare (type ref ref) (type basic-combination call))
  (let ((fun (let ((fun (ref-leaf ref)))
	       (if (external-entry-point-p fun)
		   (functional-entry-function fun)
		   fun)))
	(*compiler-error-context* call))
      (cond ((eq (basic-combination-kind call) :local))
	    ((mv-combination-p call)
	     (convert-mv-call ref call fun))
	    ((lambda-p fun)
	     (convert-lambda-call ref call fun))
	    (t
	     (convert-hairy-call ref call fun))))
  (undefined-value))


;;; Convert-MV-Call  --  Internal
;;;
;;;    Attempt to convert a multiple-value call.  The only interesting case is
;;; a call to a function that Looks-Like-An-MV-Bind, has exactly one reference
;;; and no XEP, and is called with one values continuation.
;;;
;;;    We change the call to be to the last optional entry point and change the
;;; call to be local.  Due to our preconditions, the call should eventually be
;;; converted to a let, but we can't do that now, since there may be stray
;;; references to the e-p lambda due to optional defaulting code.
;;;
;;;    We also use variable types for the called function to construct an
;;; assertion for the values continuation.
;;;
(defun convert-mv-call (ref call fun)
  (declare (type ref ref) (type mv-combination call) (type functional fun))
  (when (and (looks-like-an-mv-bind fun)
	     (not (functional-entry-function fun))
	     (= (length (leaf-refs fun)) 1)
	     (= (length (basic-combination-args call)) 1))
    (let ((ep (car (last (optional-dispatch-entry-points fun)))))
      (change-ref-leaf ref ep)
      (setf (basic-combination-kind call) :local)
      (pushnew ep (lambda-calls (node-home-lambda call)))

      (assert-continuation-type
       (first (basic-combination-args call))
       (make-values-type :optional (mapcar #'leaf-type (lambda-vars ep))
			 :rest *universal-type*))))
  (undefined-value))


;;; Convert-Lambda-Call  --  Internal
;;;
;;;    Attempt to convert a call to a lambda.  If the number of args is wrong,
;;; we give a warning and mark the Ref as :Notinline to remove it from future
;;; consideration.  If the argcount is O.K. then we just convert it.
;;;
(defun convert-lambda-call (ref call fun)
  (declare (type ref ref) (type combination call) (type clambda fun))
  (let ((nargs (length (lambda-vars fun)))
	(call-args (length (combination-args call))))
    (cond ((= call-args nargs)
	   (convert-call ref call fun))
	  (t
	   (compiler-warning
	    "Function called with ~R argument~:P, but wants exactly ~R."
	    call-args nargs)
	   (setf (ref-inlinep ref) :notinline)))))



;;;; Optional, more and keyword calls:

;;; Convert-Hairy-Call  --  Internal
;;;
;;;    Similar to Convert-Lambda-Call, but deals with Optional-Dispatches.  If
;;; only fixed args are supplied, then convert a call to the correct entry
;;; point.  If keyword args are supplied, then dispatch to a subfunction.  We
;;; don't convert calls to functions that have a more (or rest) arg.
;;;
(defun convert-hairy-call (ref call fun)
  (declare (type ref ref) (type combination call)
	   (type optional-dispatch fun))
  (let ((min-args (optional-dispatch-min-args fun))
	(max-args (optional-dispatch-max-args fun))
	(call-args (length (combination-args call))))
    (cond ((< call-args min-args)
	   (compiler-warning "Function called with ~R argument~:P, but wants at least ~R."
			     call-args min-args)
	   (setf (ref-inlinep ref) :notinline))
	  ((<= call-args max-args)
	   (convert-call ref call
			 (elt (optional-dispatch-entry-points fun)
			      (- call-args min-args))))
	  ((optional-dispatch-more-entry fun)
	   (convert-more-call ref call fun))
	  (t
	   (compiler-warning "Function called with ~R argument~:P, but wants at most ~R."
			     call-args max-args)
	   (setf (ref-inlinep ref) :notinline))))

  (undefined-value))


;;; Convert-Hairy-Fun-Entry  --  Internal
;;;
;;;     This function is used to convert a call to an entry point when complex
;;; transformations need to be done on the original arguments.  Entry is the
;;; entry point function that we are calling.  Vars is a list of variable names
;;; which are bound to the oringinal call arguments.  Ignores is the subset of
;;; Vars which are ignored.  Args is the list of arguments to the entry point
;;; function.
;;;
;;;    In order to avoid gruesome graph grovelling, we introduce a new function
;;; that rearranges the arguments and calls the entry point.  We analyze the
;;; new function and the entry point immediately so that everything gets
;;; converted during the single pass.
;;;
(defun convert-hairy-fun-entry (ref call entry vars ignores args)
  (declare (list vars ignores args) (type ref ref) (type combination call)
	   (type clambda entry))
  (let ((new-fun
	 (with-ir1-environment call
	   (ir1-convert-lambda
	    `(lambda ,vars
	       (declare (ignorable . ,ignores))
	       (%funcall ,entry . ,args))))))
    (convert-call ref call new-fun)
    (dolist (ref (leaf-refs entry))
      (convert-call-if-possible ref (continuation-dest (node-cont ref))))))


;;; Convert-More-Call  --  Internal
;;;
;;;    Use Convert-Hairy-Fun-Entry to convert a more-arg call to a known
;;; function into a local call to the Main-Entry.
;;;
;;;    First we verify that all keywords are constant and legal.  If there
;;; aren't, then we warn the user and don't attempt to convert the call.
;;;
;;;    We massage the supplied keyword arguments into the order expected by the
;;; main entry.  This is done by binding all the arguments to the keyword call
;;; to variables in the introduced lambda, then passing these values variables
;;; in the correct order when calling the main entry.  Unused arguments
;;; (such as the keywords themselves) are discarded simply by not passing them
;;; along.
;;;
;;;    If there is a rest arg, then we bundle up the args and pass them to
;;; LIST.
;;;
(defun convert-more-call (ref call fun)
  (declare (type ref ref) (type combination call) (type optional-dispatch fun))
  (let* ((max (optional-dispatch-max-args fun))
	 (arglist (optional-dispatch-arglist fun))
	 (args (combination-args call))
	 (more (nthcdr max args))
	 (flame (policy call (or (> speed brevity) (> space brevity))))
	 (loser nil))
    (collect ((temps)
	      (more-temps)
	      (ignores)
	      (supplied)
	      (key-vars))

      (dolist (var arglist)
	(let ((info (lambda-var-arg-info var)))
	  (when info
	    (ecase (arg-info-kind info)
	      (:keyword
	       (key-vars var))
	      (:rest :optional)))))

      (dotimes (i max)
	(temps (gensym "FIXED-ARG-TEMP-")))

      (dotimes (i (length more))
	(more-temps (gensym "MORE-ARG-TEMP-")))

      (when (optional-dispatch-keyp fun)
	(when (oddp (length more))
	  (compiler-warning "Function called with odd number of ~
	  		     arguments in keyword portion.")
	  (setf (ref-inlinep ref) :notinline)
	  (return-from convert-more-call))

	(do ((key more (cddr key))
	     (temp (more-temps) (cddr temp)))
	    ((null key))
	  (let ((cont (first key)))
	    (unless (constant-continuation-p cont)
	      (when flame
		(compiler-note "Non-constant keyword in keyword call."))
	      (setf (ref-inlinep ref) :notinline)
	      (return-from convert-more-call))
	    
	    (let ((name (continuation-value cont))
		  (dummy (first temp))
		  (val (second temp)))
	      (dolist (var (key-vars)
			   (progn
			     (ignores dummy val)
			     (setq loser name)))
		(let ((info (lambda-var-arg-info var)))
		  (when (eq (arg-info-keyword info) name)
		    (ignores dummy)
		    (supplied (cons var val)))
		    (return))))))
	
	(when (and loser (not (optional-dispatch-allowp fun)))
	  (compiler-warning "Function called with unknown argument keyword ~S."
			    loser)
	  (setf (ref-inlinep ref) :notinline)
	  (return-from convert-more-call)))

      (collect ((call-args))
	(do ((var arglist (cdr var))
	     (temp (temps) (cdr temp)))
	    (())
	  (let ((info (lambda-var-arg-info (car var))))
	    (if info
		(ecase (arg-info-kind info)
		  (:optional
		   (call-args (car temp))
		   (when (arg-info-supplied-p info)
		     (call-args t)))
		  (:rest
		   (call-args `(list ,@(more-temps)))
		   (return))
		  (:keyword
		   (return)))
		(call-args (car temp)))))

	(dolist (var (key-vars))
	  (let ((info (lambda-var-arg-info var))
		(temp (cdr (assoc var (supplied)))))
	    (if temp
		(call-args temp)
		(call-args (arg-info-default info)))
	    (when (arg-info-supplied-p info)
	      (call-args (not (null temp))))))

	(convert-hairy-fun-entry ref call (optional-dispatch-main-entry fun)
				 (append (temps) (more-temps))
				 (ignores) (call-args)))))

  (undefined-value))


;;;; Let conversion:
;;;
;;;    Converting to a let has differing significance to various parts of the
;;; compiler:
;;; -- The body of a Let is spliced in immediately after the the corresponding
;;;    combination node, making the control transfer explicit and allowing lets
;;;    to mashed together into a single block.  The value of the let is
;;;    delivered directly to the original continuation for the call,
;;;    eliminating the need to propagate information from the dummy result
;;;    continuation. 
;;; -- As far as IR1 optimization is concerned, it is interesting in that there
;;;    is only one expression that the variable can be bound to, and this is
;;;    easily substitited for.
;;; -- Lets are interesting to environment analysis and the back end because in
;;;    most ways a let can be considered to be "the same function" as its home
;;;    function.
;;; -- Let conversion has dynamic scope implications, since control transfers
;;;    within the same environment are local.  In a local control transfer,
;;;    cleanup code must be emitted to remove dynamic bindings that are no
;;;    longer in effect.


;;; Merge-Lets  --  Internal
;;;
;;;    Handle the environment semantics of let conversion.  We add the lambda
;;; and its lets to lets for the call's home function.  We merge the calls for
;;; Fun with the calls for the home function, removing Fun in the process.  We
;;; also merge the Entries.
;;;
(defun merge-lets (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (let* ((prev (node-prev call))
	 (home (block-home-lambda (continuation-block prev)))
	 (home-env (lambda-environment home)))
    (push fun (lambda-lets home))
    (setf (lambda-home fun) home)
    (setf (lambda-environment fun) home-env)
    
    (let ((lets (lambda-lets fun)))
      (dolist (let lets)
	(setf (lambda-home let) home)
	(setf (lambda-environment let) home-env))

      (setf (lambda-lets home) (nconc lets (lambda-lets home)))
      (setf (lambda-lets fun) ()))

    (setf (lambda-calls home)
	  (nunion (lambda-calls fun)
		  (delete fun (lambda-calls home))))
    (setf (lambda-calls fun) ())

    (setf (lambda-entries home)
	  (nconc (lambda-entries fun) (lambda-entries home)))
    (setf (lambda-entries fun) ()))
  (undefined-value))


;;; Insert-Let-Body  --  Internal
;;;
;;;    Handle the control semantics of let conversion.  We split the call block
;;; immediately after the call, and link the head and tail of Fun to the call
;;; block and the following block.  We also unlink the function head and tail
;;; from the component head and tail and flush the function from the
;;; Component-Lambdas.  We set Component-Reanalyze to true to indicate that the
;;; DFO should be recomputed.
;;;
(defun insert-let-body (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (setf (lambda-call-lexenv fun) (node-lexenv call))
  (let* ((call-block (node-block call))
	 (bind-block (node-block (lambda-bind fun)))
	 (component (block-component call-block)))
    (let ((*current-component* component))
      (node-ends-block call))
    (setf (component-lambdas component)
	  (delete fun (component-lambdas component)))
    (assert (= (length (block-succ call-block)) 1))
    (let ((next-block (first (block-succ call-block))))
      (unlink-blocks call-block next-block)
      (unlink-blocks (component-head component) bind-block)
      (link-blocks call-block bind-block)
      (let ((return (lambda-return fun)))
	(when return
	  (let ((return-block (node-block return)))
	    (unlink-blocks return-block (component-tail component))
	    (link-blocks return-block next-block)))))
    (setf (component-reanalyze component) t))
  (undefined-value))


;;; Move-Return-Uses  --  Internal
;;;
;;;    Handle the value semantics of let conversion.  When Fun has a return
;;; node, we delete it and move all the uses of the result continuation to
;;; Call's Cont.
;;;
;;;    If the actual continuation is only used by the let call, then we
;;; intersect the type assertion on the dummy continuation with the assertion
;;; for the actual continuation; in all other cases assertions on the dummy
;;; continuation are lost.
;;;
;;;    We also intersect the derived type of the call with the derived type of
;;; all the dummy continuation's uses.  This serves mainly to propagate
;;; TRULY-THE through lets.
;;;
(defun move-return-uses (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (let ((return (lambda-return fun)))
    (when return
      (unlink-node return)
      (delete-return return)

      (let ((result (return-result return))
	    (cont (node-cont call))
	    (call-type (node-derived-type call)))
	(when (eq (continuation-use cont) call)
	  (assert-continuation-type cont (continuation-asserted-type result)))
	(unless (eq call-type *wild-type*)
	  (do-uses (use result)
	    (derive-node-type use call-type)))
	  
	(delete-continuation-use call)
	(add-continuation-use call (node-prev (lambda-bind fun)))
	(substitute-continuation-uses cont result))))

  (undefined-value))


;;; Let-Convert  --  Internal
;;;
;;;    Actually do let conversion.  We call subfunctions to do most of the
;;; work.  We change the Call's cont to be the continuation heading the bind
;;; block, and also do Reoptimize-Continuation on the args and Cont so that
;;; let-specific IR1 optimizations get a chance.  We blow away any entry for
;;; the function in *free-functions* so that nobody will create new reference
;;; to it.
;;;
(defun let-convert (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (insert-let-body fun call)
  (merge-lets fun call)
  (move-return-uses fun call)

  (let* ((fun (or (lambda-optional-dispatch fun) fun))
	 (entry (gethash (leaf-name fun) *free-functions*)))
    (when (eq entry fun)
      (remhash (leaf-name fun) *free-functions*)))

  (dolist (arg (basic-combination-args call))
    (when arg
      (reoptimize-continuation arg)))
  (reoptimize-continuation (node-cont call))
  (undefined-value))


;;; Maybe-Let-Convert  --  Interface
;;;
;;;    This function is called when there is some reason to believe that
;;; the lambda Fun might be converted into a let.  This is done after local
;;; call analysis, and also when a reference is deleted.  We only convert to a
;;; let when the function is a normal local function, has no XEP, and is
;;; referenced in exactly one local call.  Conversion is also inhibited if the
;;; only reference is in a block about to be deleted.
;;;
;;;    These rules may seem unnecessarily restrictive, since there are some
;;; cases where we could do the return with a jump that don't satisfy these
;;; requirements.  The reason for doing things this way is that it makes the
;;; concept of a let much more useful at the level of IR1 semantics.  Low-level
;;; control and environment optimizations can always be done later on.
;;;
;;;    We don't attempt to convert calls to functions that have an XEP, since
;;; we might be embarrassed later when we want to convert a newly discovered
;;; local call.
;;;
(defun maybe-let-convert (fun)
  (declare (type clambda fun))
  (let ((refs (leaf-refs fun)))
    (when (and refs (null (rest refs))
	       (not (block-delete-p (node-block (first refs))))
	       (not (functional-kind fun))
	       (not (functional-entry-function fun)))
      (let* ((ref-cont (node-cont (first refs)))
	     (dest (continuation-dest ref-cont)))
	(when (and (basic-combination-p dest)
		   (eq (basic-combination-fun dest) ref-cont)
		   (eq (basic-combination-kind dest) :local))
	  (let-convert fun dest)
	  (setf (functional-kind fun)
		(if (mv-combination-p dest) :mv-let :let))))))
  (undefined-value))
