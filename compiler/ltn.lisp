;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the LTN pass in the compiler.  LTN allocates
;;; expression evaluation TNs, makes nearly all the implementation policy
;;; decisions, and also does a few other random things.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Utilities:

;;; Translation-Policy  --  Internal
;;;
;;;    Return the policies keyword indicated by the node policy.
;;;
(defun translation-policy (node)
  (declare (type node node))
  (let* ((cookie (node-cookie node))
	 (dcookie (node-default-cookie node))
	 (safety (or (cookie-safety cookie)
		     (cookie-safety dcookie)))
	 (space (max (or (cookie-space cookie)
			 (cookie-space dcookie))
		     (or (cookie-cspeed cookie)
			 (cookie-cspeed dcookie))))
	 (speed (or (cookie-speed cookie)
		    (cookie-speed dcookie))))
    (cond ((>= safety speed space) :fast-safe)
	  ((>= speed (max space safety)) :fast)
	  ((>= space (max speed safety)) :small)
	  (t :safe))))


;;; Policy-Safe-P  --  Interface
;;;
;;;    Return true if Policy is a safe policy.
;;;
(proclaim '(inline policy-safe-p))
(defun policy-safe-p (policy)
  (declare (type policies policy))
  (or (eq policy :safe) (eq policy :fast-safe)))


;;; FLUSH-TYPE-CHECK  --  Internal
;;;
;;;    Called when an unsafe policy indicates that no type check should be done
;;; on CONT.  We delete the type check unless it is :ERROR (indicating a
;;; compile-time type error.)
;;;
(proclaim '(inline flush-type-check))
(defun flush-type-check (cont)
  (declare (type continuation cont))
  (when (member (continuation-type-check cont) '(t :no-check))
    (setf (continuation-%type-check cont) :deleted))
  (undefined-value))


;;; Continuation-PType  --  Internal
;;;
;;;    A annotated continuation's primitive-type.
;;;
(proclaim '(inline continuation-ptype))
(defun continuation-ptype (cont)
  (declare (type continuation cont))
  (ir2-continuation-primitive-type (continuation-info cont)))


;;; Continuation-Delayed-Leaf  --  Internal
;;;
;;;    If Cont is used only by a Ref to a leaf that can be delayed, then return
;;; the leaf, otherwise return NIL.
;;;
(defun continuation-delayed-leaf (cont)
  (declare (type continuation cont)) 
  (let ((use (continuation-use cont)))
    (and (ref-p use)
	 (let ((leaf (ref-leaf use)))
	   (etypecase leaf
	     (lambda-var (if (null (lambda-var-sets leaf)) leaf nil))
	     (constant leaf)
	     ((or functional global-var) nil))))))


;;; Annotate-1-Value-Continuation  --  Internal
;;;
;;;    Annotate a normal single-value continuation.  If its only use is a ref
;;; that we are allowed to delay the evaluation of, then we mark the
;;; continuation for delayed evaluation, otherwise we assign a TN to hold the
;;; continuation's value.
;;;
(defun annotate-1-value-continuation (cont)
  (declare (type continuation cont))
  (let ((info (continuation-info cont)))
    (assert (eq (ir2-continuation-kind info) :fixed))
    (if (continuation-delayed-leaf cont)
	(setf (ir2-continuation-kind info) :delayed)
	(setf (ir2-continuation-locs info)
	      (list (make-normal-tn (ir2-continuation-primitive-type info))))))
  (undefined-value))


;;; Annotate-Ordinary-Continuation  --  Internal
;;;
;;;    Make an IR2-Continuation corresponding to the continuation type and then
;;; do Annotate-1-Value-Continuation.  If Policy isn't a safe policy, then we
;;; clear the type-check flag.
;;;
(defun annotate-ordinary-continuation (cont policy)
  (declare (type continuation cont)
	   (type policies policy))
  (let ((info (make-ir2-continuation
	       (primitive-type (continuation-type cont)))))
    (setf (continuation-info cont) info)
    (annotate-1-value-continuation cont)
    (unless (policy-safe-p policy) (flush-type-check cont)))
  (undefined-value))


;;; Annotate-Full-Call-Continuation  --  Internal
;;;
;;;    Annotate a continuation that is an argument to a full call.  Kind of
;;; like Annotate-Ordinary-Continuation, but we always clear the type-check
;;; flag, since it is assumed that the callee does appropriate checking.
;;;
(defun annotate-full-call-continuation (cont)
  (declare (type continuation cont))
  (let ((info (or (continuation-info cont)
		  (setf (continuation-info cont)
			(make-ir2-continuation *any-primitive-type*))))
	(leaf (continuation-delayed-leaf cont)))
    (flush-type-check cont)
    (setf (ir2-continuation-primitive-type info) *any-primitive-type*)
    (if leaf
	(setf (ir2-continuation-kind info) :delayed)
	(setf (ir2-continuation-locs info)
	      (list (make-normal-tn *any-primitive-type*)))))
  (undefined-value))


;;; Annotate-Function-Continuation  --  Internal
;;;
;;;    Annotate the function continuation for a full call.  If the only
;;; reference is to a global symbol function and Delay is true, then we delay
;;; the reference, otherwise we annotate for a single value.
;;;
;;;   Unlike for an argument, we only clear the type check flag when the policy
;;; is unsafe, since the check for a valid function object must be done before
;;; the call.  Note that in the common case of a delayed global function
;;; reference, the type checking is postponed, letting the call sequence do the
;;; type checking however it wants.
;;;
(defun annotate-function-continuation (cont policy &optional (delay t))
  (declare (type continuation cont) (type policies policy))
  (let* ((ptype (primitive-type (continuation-derived-type cont)))
	 (info (make-ir2-continuation ptype)))
    (setf (continuation-info cont) info)
    (unless (policy-safe-p policy) (flush-type-check cont))
    (let ((name (continuation-function-name cont)))
      (if (and delay name (symbolp name))
	  (setf (ir2-continuation-kind info) :delayed)
	  (setf (ir2-continuation-locs info) (list (make-normal-tn ptype))))))
  (undefined-value))


;;; FLUSH-FULL-CALL-TAIL-TRANSFER  --  Internal
;;;
;;;    If TAIL-P is true, then we check to see if the call can really be a tail
;;; call by seeing if this function's return convention is :UNKNOWN.  If so, we
;;; unlink the call from the return block (after ensuring that they are in
;;; separate blocks.)  This allows the return to be deleted when there are no
;;; non-tail uses.
;;;
(defun flush-full-call-tail-transfer (call)
  (declare (type basic-combination call))
  (let ((tails (node-tail-p call)))
    (when tails
      (cond ((eq (return-info-kind (tail-set-info tails)) :unknown)
	     (node-ends-block call)
	     (let ((block (node-block call)))
	       (unlink-blocks block (first (block-succ block)))))
	    (t
	     (setf (node-tail-p call) nil)))))
  (undefined-value))


;;; LTN-Default-Call  --  Internal
;;;
;;;    Set up stuff to do a full call for Call.  We assume that that
;;; IR2-Continuation structures have already been assigned to the args.  We set
;;; the kind to :FULL or :FUNNY, depending on whether there is an IR2-CONVERT
;;; method.  If a funny function, then we inhibit tail recursion, since the IR2
;;; convert method is going to want to deliver values normally.
;;;
(defun ltn-default-call (call policy)
  (declare (type combination call) (type policies policy))

  (annotate-function-continuation (basic-combination-fun call) policy)
  (dolist (arg (basic-combination-args call))
    (annotate-full-call-continuation arg))

  (let ((kind (basic-combination-kind call)))
    (cond ((and (function-info-p kind)
		(function-info-ir2-convert kind))
	   (setf (basic-combination-info call) :funny)
	   (setf (node-tail-p call) nil))
	  (t
	   (setf (basic-combination-info call) :full)
	   (flush-full-call-tail-transfer call))))
  
  (undefined-value))


;;; Make-Unknown-Values-Locations  --  Interface
;;;
;;;    Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
;;;
(defun make-unknown-values-locations ()
  (make-n-tns 2 *any-primitive-type*))


;;; Annotate-Unknown-Values-Continuation  --  Internal
;;;
;;; Annotate a continuation for unknown multiple values:
;;; -- Delete any type check, regardless of policy, since we IR2 conversion
;;;    isn't prepared to check unknown-values continuations.  If we delete a
;;;    type check when the policy is safe, then we emit a warning.
;;; -- Add the continuation to the IR2-Block-Popped if it is used across a
;;;    block boundry.
;;; -- Assign a :Unknown IR2-Continuation.
;;;
;;; Note: it is critical that this be called only during LTN analysis of Cont's
;;; DEST, and called in the order that the continuations are received.
;;; Otherwise the IR2-Block-Popped and IR2-Component-Values-XXX will get all
;;; messed up.
;;;
(defun annotate-unknown-values-continuation (cont policy)
  (declare (type continuation cont) (type policies policy))
  (when (eq (continuation-type-check cont) t)
    (let* ((dest (continuation-dest cont))
	   (*compiler-error-context* dest))
      (when (and (policy-safe-p policy)
		 (policy dest (>= safety brevity)))
	(compiler-note "Unable to check type assertion in unknown-values ~
	                context:~% ~S"
		       (continuation-asserted-type cont))))
    (setf (continuation-%type-check cont) :deleted))

  (let* ((block (node-block (continuation-dest cont)))
	 (use (continuation-use cont))
	 (2block (block-info block)))
    (unless (and use (eq (node-block use) block))
      (setf (ir2-block-popped 2block)
	    (nconc (ir2-block-popped 2block) (list cont)))))

  (let ((2cont (make-ir2-continuation nil)))
    (setf (ir2-continuation-kind 2cont) :unknown)
    (setf (ir2-continuation-locs 2cont) (make-unknown-values-locations))
    (setf (continuation-info cont) 2cont))

  (undefined-value))


;;; Annotate-Fixed-Values-Continuation  --  Internal
;;;
;;;    Annotate Cont for a fixed, but arbitrary number of values, to be kept in
;;; the list of TNs Locs.
;;;
(defun annotate-fixed-values-continuation (cont policy locs)
  (declare (type continuation cont) (type policies policy) (list locs))
  (unless (policy-safe-p policy) (flush-type-check cont))

  (let ((res (make-ir2-continuation nil)))
    (setf (ir2-continuation-locs res) locs)
    (setf (continuation-info cont) res))

  (undefined-value))


;;;; Node-specific analysis functions:

;;; LTN-Analyze-Return  --  Internal
;;;
;;;    Annotate the result continuation for a function.  We use the Return-Info
;;; computed by GTN to determine how to represent the return values within the
;;; function.
;;;
;;;    If the kind is :Fixed, and the function being returned from isn't an
;;; XEP, then we allocate a fixed number of locations to compute the function
;;; result in.
;;;
;;;    Otherwise, we are going to use the unknown return convention.  We still
;;; try to annotate for a fixed number of values:
;;; -- If the tail-set has a fixed values count, then use that many values.
;;; -- If the actual uses of the result continuation in this function have a
;;;    fixed number of values, then use that number.  We throw out TAIL-P
;;;    :FULL and :LOCAL calls, since we know they will truly end up as TR
;;;    calls.  We can use the BASIC-COMBINATION-INFO even though it is assigned
;;;    by this phase, since the initial value NIL doesn't look like a TR call.
;;;
;;;    If there are *no* non-tail-call uses, then it falls out that we annotate
;;;    for one value (type is NIL), but the return will end up being deleted.
;;;
;;;    In non-perverse code, the DFO walk will reach all uses of the result
;;;    continuation before it reaches the RETURN.  In perverse code, we may
;;;    annotate for unknown values when we didn't have to. 
;;; -- Otherwise, we must annotate the continuation for unknown values.
;;;
(defun ltn-analyze-return (node policy)
  (declare (type creturn node) (type policies policy))
  (let* ((cont (return-result node))
	 (fun (return-lambda node))
	 (returns (tail-set-info (lambda-tail-set fun)))
	 (types (return-info-types returns)))
    (cond
     ((and (eq (return-info-kind returns) :fixed)
	   (not (external-entry-point-p fun)))
      (annotate-fixed-values-continuation cont policy
					 (mapcar #'make-normal-tn types)))
     ((not (eq (return-info-count returns) :unknown))
      (annotate-fixed-values-continuation
       cont policy
       (make-n-tns (return-info-count returns) *any-primitive-type*)))
     (t
      (collect ((res *empty-type* values-type-union))
	(do-uses (use (return-result node))
	  (unless (and (node-tail-p use)
		       (basic-combination-p use)
		       (member (basic-combination-info use) '(:local :full)))
	    (res (node-derived-type use))))
	
	(multiple-value-bind (types kind)
			     (values-types (res))
	  (if (eq kind :unknown)
	      (annotate-unknown-values-continuation cont policy)
	      (annotate-fixed-values-continuation
	       cont policy
	       (mapcar #'(lambda (x)
			   (make-normal-tn (primitive-type x)))
		       types))))))))
  (undefined-value))


;;; LTN-Analyze-MV-Bind  --  Internal
;;;
;;;    Annotate the single argument continuation as a fixed-values
;;; continuation.  We look at the called lambda to determine number and type of
;;; return values desired.  It is assumed that only a function that
;;; Looks-Like-An-MV-Bind will be converted to a local call.
;;;
(defun ltn-analyze-mv-bind (call policy)
  (declare (type mv-combination call)
	   (type policies policy))
  (setf (basic-combination-kind call) :local)
  (setf (node-tail-p call) nil)
  (annotate-fixed-values-continuation
   (first (basic-combination-args call)) policy
   (mapcar #'(lambda (var)
	       (make-normal-tn
		(primitive-type (basic-var-type var))))
	   (lambda-vars
	    (ref-leaf
	     (continuation-use
	      (basic-combination-fun call))))))
  (undefined-value))


;;; LTN-Analyze-MV-Call  --  Internal
;;;
;;;    We force all the argument continuations to use the unknown values
;;; convention.  The continuations are annotated in reverse order, since the
;;; last argument is on top, thus must be popped first.  We disallow delayed
;;; evaluation of the function continuation to simplify IR2 conversion of MV
;;; call.
;;;
;;;    We could be cleverer when we know the number of values returned by the
;;; continuations, but optimizations of MV-Call are probably unworthwhile.
;;;
;;;    We are also responsible for handling THROW, which is represented in IR1
;;; as an mv-call to the %THROW funny function.  We annotate the tag
;;; continuation for a single value and the values continuation for unknown
;;; values.
;;;
(defun ltn-analyze-mv-call (call policy)
  (declare (type mv-combination call))
  (let ((fun (basic-combination-fun call))
	(args (basic-combination-args call)))
    (cond ((eq (continuation-function-name fun) '%throw)
	   (setf (basic-combination-info call) :funny)
	   (annotate-ordinary-continuation (first args) policy)
	   (annotate-unknown-values-continuation (second args) policy)
	   (setf (node-tail-p call) nil))
	  (t
	   (setf (basic-combination-info call) :full)
	   (annotate-function-continuation (basic-combination-fun call)
					   policy nil)
	   (dolist (arg (reverse args))
	     (annotate-unknown-values-continuation arg policy))
	   (flush-full-call-tail-transfer call))))

  (undefined-value))


;;; LTN-Analyze-Local-Call  --  Internal
;;;
;;;    Annotate the arguments as ordinary single-value continuations.  If a
;;; tail call, swing the successor link to the start of the called function so
;;; that the return can be deleted.
;;;
(defun ltn-analyze-local-call (call policy)
  (declare (type combination call)
	   (type policies policy))
  (setf (basic-combination-info call) :local)

  (dolist (arg (basic-combination-args call))
    (when arg
      (annotate-ordinary-continuation arg policy)))

  (when (node-tail-p call)
    (node-ends-block call)
    (let ((block (node-block call)))
      (unlink-blocks block (first (block-succ block)))
      (link-blocks block
		   (node-block (lambda-bind (combination-lambda call))))))

  (undefined-value))


;;; LTN-Analyze-Set  --  Internal
;;;
;;;    Annotate the value continuation.
;;;
(defun ltn-analyze-set (node policy)
  (declare (type cset node) (type policies policy))
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation (set-value node) policy)
  (undefined-value))


;;; LTN-Analyze-If  --  Internal  
;;;
;;;    If the only use of the Test continuation is a combination annotated with
;;; a conditional template, then don't annotate the continuation so that IR2
;;; conversion knows not to emit any code, otherwise annotate as an ordinary
;;; continuation.  Since we only use a conditional template if the call
;;; immediately precedes the IF node in the same block, we know that any
;;; predicate will already be annotated.
;;;
(defun ltn-analyze-if (node policy)
  (declare (type cif node) (type policies policy))
  (setf (node-tail-p node) nil)
  (let* ((test (if-test node))
	 (use (continuation-use test)))
    (unless (and (combination-p use)
		 (let ((info (basic-combination-info use)))
		   (and (template-p info)
			(eq (template-result-types info) :conditional))))
      (annotate-ordinary-continuation test policy)))
  (undefined-value))


;;; LTN-Analyze-Exit  --  Internal
;;;
;;;    If there is a value continuation, then annotate it for unknown values.
;;; In this case, the exit is non-local, since all other exits are deleted or
;;; degenerate by this point.
;;;
(defun ltn-analyze-exit (node policy)
  (setf (node-tail-p node) nil)
  (let ((value (exit-value node)))
    (when value
      (annotate-unknown-values-continuation value policy)))
  (undefined-value))


;;; LTN annotate %Unwind-Protect  --  Internal
;;;
;;;    We need a special method for %Unwind-Protect that ignores the cleanup
;;; function.  We don't annotate either arg, since we don't need them at
;;; run-time.
;;;
;;; [The default is o.k. for %Catch, since environment analysis converted the
;;; reference to the escape function into a constant reference to the
;;; NLX-Info.]
;;;
(defoptimizer (%unwind-protect ltn-annotate) ((escape cleanup) node policy)
  policy ; Ignore...
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  )


;;; LTN annotate %Slot-Setter, %Slot-Accessor  --  Internal
;;;
;;;    Both of these functions need special LTN-annotate methods, since we only
;;; want to clear the Type-Check in unsafe policies.  If we allowed the call to
;;; be annotated as a full call, then no type checking would be done.
;;;
;;;    We also need a special LTN annotate method for %Slot-Setter so that the
;;; function is ignored.  This is because the reference to a SETF function
;;; can't be delayed, so IR2 conversion would have already emitted a call to
;;; FDEFINITION by the time the IR2 convert method got control.
;;;
(defoptimizer (%slot-accessor ltn-annotate) ((struct) node policy)
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation struct policy))
;;;
(defoptimizer (%slot-setter ltn-annotate) ((struct value) node policy)
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation struct policy)
  (annotate-ordinary-continuation value policy))


;;;; Known call annotation:

;;; OPERAND-RESTRICTION-OK  --  Internal
;;;
(proclaim '(inline operand-restriction-ok))
(defun operand-restriction-ok (restr type &optional cont)
  (declare (type (or (member *) cons) restr)
	   (type primitive-type type)
	   (type (or continuation null) cont))
  (if (eq restr '*)
      t
      (ecase (first restr)
	(:or
	 (dolist (mem (rest restr) nil)
	   (when (eq mem type) (return t))))
	(:constant
	 (funcall (second restr) (continuation-value cont))))))

  
;;; Template-Args-OK  --  Internal
;;;
;;;    Check that the argument type restriction for Template are satisfied in
;;; call.  If an argument's TYPE-CHECK is :NO-CHECK and our policy is safe,
;;; then only :SAFE templates are o.k.
;;;
(defun template-args-ok (template call safe-p)
  (declare (type template template)
	   (type combination call))
  (let ((mtype (template-more-args-type template)))
    (do ((args (basic-combination-args call) (cdr args))
	 (types (template-arg-types template) (cdr types)))
	((null types)
	 (cond ((null args) t)
	       ((not mtype) nil)
	       (t
		(dolist (arg args t)
		  (unless (operand-restriction-ok mtype
						  (continuation-ptype arg))
		    (return nil))))))
      (when (null args) (return nil))
      (let ((arg (car args))
	    (type (car types)))
	(when (and (eq (continuation-type-check arg) :no-check)
		   safe-p
		   (not (eq (template-policy template) :safe)))
	  (return nil))
	(unless (operand-restriction-ok type (continuation-ptype arg) arg)
	  (return nil))))))


;;; Template-Results-OK  --  Internal
;;;
;;;    Check that Template can be used with the specifed Result-Type.  Result
;;; type checking is pretty different from argument type checking due to the
;;; relaxed rules for values count.  We succeed if for each required result,
;;; there is a positional restriction on the value that is at least as good.
;;; If we run out of result types before we run out of restrictions, then we
;;; only suceed if the leftover restrictions are *.  If we run out of
;;; restrictions before we run out of result types, then we always win.
;;;
(defun template-results-ok (template result-type)
  (declare (type template template)
	   (type ctype result-type))
  (let ((types (template-result-types template)))
    (cond
     ((values-type-p result-type)
      (do ((ltypes (append (args-type-required result-type)
			   (args-type-optional result-type))
		   (rest ltypes))
	   (types types (rest types)))
	  ((null ltypes)
	   (dolist (type types t)
	     (unless (eq type '*)
	       (return nil))))
	(when (null types) (return t))
	(let ((type (first types)))
	  (unless (operand-restriction-ok type
					  (primitive-type (first ltypes)))
	    (return nil)))))
     (types
      (operand-restriction-ok (first types) (primitive-type result-type)))
     (t
      (let ((mtype (template-more-args-type template)))
	(or (not mtype)
	    (operand-restriction-ok mtype (primitive-type result-type))))))))


;;; Find-Template  --  Internal
;;;
;;;    Use operand type information to choose a template from the list
;;; Templates for a known Call.  We return three values:
;;; 1] The template we found.
;;; 2] Some template that we rejected due to unsatisfied type restrictions, or
;;;    NIL if none.
;;; 3] The tail of Templates for templates we haven't examined yet.
;;;
;;; What we do:
;;; -- If the template has a Guard that isn't true, then we ignore the
;;;    template, not even considering it to be rejected.
;;; -- If the argument type restrictions aren't satisfied, then we reject the
;;;    template.
;;; -- If the template is :Conditional, then we accept it only when the
;;;    destination of the value is an immediately following IF node.
;;; -- We accept a template if the Node-Derived-Type satisfies the
;;;    output assertion, since this type has been proven to be statisfied.
;;; -- Unless the policy is safe and the template is :Fast-Safe, we also accept
;;;    a template when the continuation derived type satisfies the output
;;;    assertion.  We only attempt this when TYPE-CHECK is non-null, since when
;;;    this is NIL, the assertion is a supertype of the node type.
;;;
(defun find-template (templates call safe-p)
  (declare (list templates) (type combination call))
  (do ((templates templates (rest templates))
       (rejected nil))
      ((null templates)
       (values nil rejected nil))
    (let* ((template (first templates))
	   (guard (template-guard template)))
      (when (and (or (not guard) (funcall guard))
		 (template-args-ok template call safe-p))
	(let* ((cont (node-cont call))
	       (atype (continuation-asserted-type cont))
	       (dtype (node-derived-type call)))
	  (when (or (and (eq (template-result-types template) :conditional)
			 (let ((dest (continuation-dest cont)))
			   (and (if-p dest)
				(immediately-used-p (if-test dest) call))))
		    (template-results-ok template dtype)
		    (and (not (and (eq (template-policy template) :fast-safe)
				   safe-p))
			 (continuation-type-check cont)
			 (template-results-ok template
					      (values-type-intersection
					       dtype atype))))
	    (return (values template rejected (rest templates))))))
      (setq rejected template))))


;;; Find-Template-For-Policy  --  Internal
;;;
;;;    Given a partially annotated known call and a translation policy, return
;;; the appropriate template, or NIL if none can be found.  We scan the
;;; templates (ordered by increasing cost) looking for a template whose
;;; restrictions are satisfied and that has our policy.
;;;
;;; If we find a template that doesn't have our policy, but has a legal
;;; alternate policy, then we also record that to return as a last resort.  If
;;; our policy is safe, then only safe policies are O.K., otherwise anything
;;; goes.
;;;
;;; If we rejected a template and Speed > Brevity, then we call
;;; Note-Rejected-Templates to emit any appropriate efficiency notes.
;;;
(defun find-template-for-policy (call policy)
  (declare (type combination call)
	   (type policies policy))
  (let ((safe-p (policy-safe-p policy)))
    (let ((current (function-info-templates (basic-combination-kind call)))
	  (fallback nil)
	  (rejected nil))
      (loop
	(multiple-value-bind (template this-reject more)
			     (find-template current call safe-p)
	  (unless rejected
	    (setq rejected this-reject))
	  (setq current more)
	  (unless template
	    (return (values fallback rejected)))
	  
	  (let ((tpolicy (template-policy template)))
	    (cond ((eq tpolicy policy)
		   (return (values template rejected)))
		  (fallback)
		  ((or (not safe-p) (policy-safe-p tpolicy))
		   (setq fallback template)))))))))


;;; Note-Rejected-Templates  --  Internal
;;;
;;;    This function emits efficiency notes describing all of the templates
;;; better (faster) than Template that we might have been able to use if there
;;; were better type declarations.  Template is null when we didn't find any
;;; template, and thus must do a full call.
;;;
;;; In order to be worth complaining about, a template must:
;;; -- be allowed by its guard,
;;; -- be safe if the current policy is safe,
;;; -- have argument/result type restrictions consistent with the known type
;;;    information, e.g. we don't consider float templates when an operand is
;;;    known to be an integer,
;;; -- be disallowed by the stricter operand subtype test (which resembles, but
;;;    is not identical to the test done by Find-Template.)
;;;
;;; Note that there may not be any possibly applicable templates, since we are
;;; called whenever any template is rejected.  That template might have the
;;; wrong policy or be inconsistent with the known type.
;;;
;;; We go to some trouble to make the whole multi-line output into a single
;;; call to Compiler-Note so that repeat messages are suppressed, etc.
;;;
(defun note-rejected-templates (call policy template)
  (declare (type combination call) (type policies policy)
	   (type (or template null) template))

  (collect ((losers))
    (let ((safe-p (policy-safe-p policy)))
      (dolist (try (function-info-templates (basic-combination-kind call)))
	(when (eq try template) (return))
	(let ((guard (template-guard try)))
	  (when (and (template-note try)
		     (or (not guard) (funcall guard))
		     (or (not safe-p)
			 (policy-safe-p (template-policy try)))
		     (valid-function-use
		      call (template-type try)
		      :argument-test #'types-intersect
		      :result-test #'values-types-intersect))
	    (losers try)))))

    (when (losers)
      (collect ((messages))
	(flet ((frob (string &rest stuff)
		 (messages string)
		 (messages stuff)))
	  (dolist (loser (losers))
	    (let* ((type (template-type loser))
		   (valid (valid-function-use call type))
		   (strict-valid (valid-function-use call type
						     :strict-result t)))
	      (when (or (not valid) (not strict-valid))
		(frob "Unable to do ~A (cost ~D) because:"
		      (template-note loser) (template-cost loser)))

	      (cond ((not valid)
		     (valid-function-use call type
					 :error-function #'frob
					 :warning-function #'frob))
		    ((not strict-valid)
		     (assert (policy-safe-p policy))
		     (frob "Can't trust output type assertion under safe ~
		            policy."))))))

	(let ((*compiler-error-context* call))
	  (compiler-note "~{~?~^~&~6T~}"
			 (if template
			     `("Forced to do ~A (cost ~D)."
			       (,(or (template-note template)
				     (template-name template))
				,(template-cost template))
			       . ,(messages))
			     `("Forced to do full call."
			       nil
			       . ,(messages))))))))
  (undefined-value))



;;; Flush-Type-Checks-According-To-Policy  --  Internal
;;;
;;;    Flush type checks according to policy.  If the policy is unsafe, then we
;;; never do any checks.  If our policy is safe, and we are using a safe
;;; template, then we can also flush arg type checks, but we must make the
;;; continuation type be *any-primitive-type* so that objects of the incorrect
;;; type can be represented.
;;;
(defun flush-type-checks-according-to-policy (call policy template)
  (declare (type combination call) (type policies policy)
	   (type template template))
  (if (policy-safe-p policy)
      (when (eq (template-policy template) :safe)
	(dolist (arg (basic-combination-args call))
	  (when (continuation-type-check arg)
	    (flush-type-check arg)
	    (setf (ir2-continuation-primitive-type (continuation-info arg))
		  *any-primitive-type*))))
      (dolist (arg (basic-combination-args call))
	(flush-type-check arg)))
  (undefined-value))


;;; LTN-Analyze-Known-Call  --  Internal
;;;
;;;    If a function has a special-case annotation method use that, otherwise
;;; annotate the argument continuations and try to find a template
;;; corresponding to the type signature. If there is none, convert a full
;;; call.
;;;
;;;    If we are unable to use some templates due to unstatisfied operand type
;;; restrictions and our policy enables efficiency notes, then we call
;;; Note-Rejected-Templates.
;;;
;;;    If we are forced to do a full call, we check to see if the function
;;; called is the same as the current function.  If so, we give a warning, as
;;; this is probably a botched interpreter stub.
;;;
(defun ltn-analyze-known-call (call policy)
  (declare (type combination call)
	   (type policies policy))
  (let ((method (function-info-ltn-annotate (basic-combination-kind call)))
	(args (basic-combination-args call)))
    (when method
      (funcall method call policy)
      (return-from ltn-analyze-known-call (undefined-value)))
    
    (dolist (arg args)
      (setf (continuation-info arg)
	    (make-ir2-continuation (primitive-type (continuation-type arg)))))

    (multiple-value-bind (template rejected)
			 (find-template-for-policy call policy)
      (when (and rejected
		 (policy call (> speed brevity)))
	(note-rejected-templates call policy template))
      (unless template
	(when (and (eq (continuation-function-name (combination-fun call))
		       (leaf-name
			(environment-function
			 (node-environment call))))
		   (not (function-info-ir2-convert
			 (basic-combination-kind call))))
	  (let ((*compiler-error-context* call))
	    (compiler-warning "Recursive known function definition.")))
	(ltn-default-call call policy)
	(return-from ltn-analyze-known-call (undefined-value)))
      (setf (basic-combination-info call) template)
      (setf (node-tail-p call) nil)
      
      (flush-type-checks-according-to-policy call policy template)
      
      (dolist (arg args)
	(annotate-1-value-continuation arg))))
  
  (undefined-value))


;;;; Interfaces:

(eval-when (compile eval)

;;; LTN-Analyze-Block-Macro  --  Internal
;;;
;;;    We make the main per-block code in for LTN into a macro so that it can
;;; be shared between LTN-Analyze and LTN-Analyze-Block, yet can cache policy
;;; across blocks in the normal (full component) case.
;;;
;;;    This code computes the policy and then dispatches to the appropriate
;;; node-specific function.
;;;
;;; Note: we deliberately don't use the DO-NODES macro, since the block can be
;;; split out from underneath us, and DO-NODES scans past the block end in this
;;; case.
;;;
(defmacro ltn-analyze-block-macro ()
  '(do* ((node (continuation-next (block-start block))
	       (continuation-next cont))
	 (cont (node-cont node) (node-cont node)))
	(())
     (unless (and (eq (node-cookie node) cookie)
		  (eq (node-default-cookie node) default-cookie))
       (setq policy (translation-policy node))
       (setq cookie (node-cookie node))
       (setq default-cookie (node-default-cookie node)))
	     
     (etypecase node
       (ref)
       (combination
	(case (basic-combination-kind node)
	  (:local (ltn-analyze-local-call node policy))
	  (:full (ltn-default-call node policy))
	  (t
	   (ltn-analyze-known-call node policy))))
       (cif
	(ltn-analyze-if node policy))
       (creturn
	(ltn-analyze-return node policy))
       ((or bind entry))
       (exit
	(ltn-analyze-exit node policy))
       (cset (ltn-analyze-set node policy))
       (mv-combination
	(ecase (basic-combination-kind node)
	  (:local (ltn-analyze-mv-bind node policy))
	  (:full (ltn-analyze-mv-call node policy)))))

     (when (eq node (block-last block)) (return))))

); Eval-When (Compile Eval)


;;; LTN-Analyze  --  Interface
;;;
;;;    Loop over the blocks in Component, doing stuff to nodes that receive
;;; values.  In addition to the stuff done by LTN-Analyze-Block-Macro, we also
;;; see if there are any unknown values receivers, making notations in the
;;; components Generators and Receivers as appropriate.
;;;
;;;    If any unknown-values continations are received by this block (as
;;; indicated by IR2-Block-Popped, then we add the block to the
;;; IR2-Component-Values-Receivers.
;;;
;;;    This is where we allocate IR2 blocks because it is the first place we
;;; need them.
;;;
(defun ltn-analyze (component)
  (declare (type component component))
  (let ((2comp (component-info component))
	(cookie nil)
	default-cookie policy)
    (do-blocks (block component)
      (assert (not (block-info block)))
      (let ((2block (make-ir2-block block)))
	(setf (block-info block) 2block)
	(ltn-analyze-block-macro)
	(let ((popped (ir2-block-popped 2block)))
	  (when popped
	    (push block (ir2-component-values-receivers 2comp)))))))
  (undefined-value))


;;; LTN-Analyze-Block  --  Interface
;;;
;;;    This function is used to analyze blocks that must be added to the flow
;;; graph after the normal LTN phase runs.  Such code is constrained not to
;;; use weird unknown values (and probably in lots of other ways).
;;;
(defun ltn-analyze-block (block)
  (declare (type cblock block))
  (let ((cookie nil)
	default-cookie policy)
    (ltn-analyze-block-macro))

  (assert (not (ir2-block-popped (block-info block))))
  (undefined-value))
