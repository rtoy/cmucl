;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file implements type check generation.  This is a phase that runs
;;; at the very end of IR1.  If a type check is too complex for the back end to
;;; directly emit in-line, then we transform the check into an explicit
;;; conditional using TYPEP.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Cost estimation:


;;; Function-Cost  --  Internal
;;;
;;;    Return some sort of guess about the cost of a call to a function.  If
;;; the function has some templates, we return the cost of the cheapest one,
;;; otherwise we return the cost of CALL-NAMED.  Calling this with functions
;;; that have transforms can result in relatively meaningless results
;;; (exaggerated costs.)
;;;
;;; We randomly special-case NULL, since it does have a source tranform and is
;;; interesting to us.
;;;
(defun function-cost (name)
  (declare (symbol name))
  (let ((info (info function info name))
	(call-cost (template-cost (template-or-lose 'call-named))))
    (if info
	(let ((templates (function-info-templates info)))
	  (if templates
	      (template-cost (first templates))
	      (case name
		(null (template-cost (template-or-lose 'if-eq)))
		(t call-cost))))
	call-cost)))

  
;;; Type-Test-Cost  --  Internal
;;;
;;;    Return some sort of guess for the cost of doing a test against TYPE.
;;; The result need not be precise as long as it isn't way out in space.  The
;;; units are based on the costs specified for various templates in the VM
;;; definition.
;;;
(defun type-test-cost (type)
  (declare (type ctype type))
  (or (let ((check (type-check-template type)))
	(if check
	    (template-cost check)
	    (let ((found (cdr (assoc type *type-predicates* :test #'type=))))
	      (if found
		  (function-cost found)
		  nil))))
      (typecase type
	(union-type
	 (collect ((res 0 +)) 
	   (dolist (mem (union-type-types type))
	     (res (type-test-cost mem)))
	   (res)))
	(member-type
	 (* (length (member-type-members type))
	    (function-cost 'eq)))
	(numeric-type
	 (* (if (numeric-type-complexp type) 2 1)
	    (function-cost
	     (if (csubtypep type (specifier-type 'fixnum)) 'fixnump 'numberp))
	    (+ 1
	       (if (numeric-type-low type) 1 0)
	       (if (numeric-type-high type) 1 0))))
	(t
	 (function-cost 'typep)))))


;;;; Checking strategy determination:


;;; MAYBE-WEAKEN-CHECK  --  Internal
;;;
;;;    Return the type we should test for when we really want to check for
;;; Type.   If speed, space or compilation speed is more important than safety,
;;; then we return a weaker type if it is easier to check.  First we try the
;;; defined type weakenings, then look for any predicate that is cheaper.
;;;
;;;    If the supertype is equal in cost to the type, we prefer the supertype.
;;; This produces a closer approximation of the right thing in the presence of
;;; poor cost info.
;;;
(defun maybe-weaken-check (type cont)
  (declare (type ctype type) (type continuation cont))
  (cond ((policy (continuation-dest cont)
		 (<= speed safety) (<= space safety) (<= cspeed safety))
	 type)
	(t
	 (let ((min-cost (type-test-cost type))
	       (min-type type)
	       (found-super nil))
	   (dolist (x *type-predicates*)
	     (let ((stype (car x)))
	       (when (and (csubtypep type stype)
			  (not (union-type-p stype))) ;Not #!% COMMON type.
		 (let ((stype-cost (type-test-cost stype)))
		   (when (or (< stype-cost min-cost)
			     (type= stype type))
		     (setq found-super t)
		     (setq min-type stype  min-cost stype-cost))))))
	   (if found-super
	       min-type
	       *universal-type*)))))


;;; NO-FUNCTION-VALUES-TYPES  --  Internal
;;;
;;;    Like VALUES-TYPES, only mash any complex function types to FUNCTION.
;;;
(defun no-function-values-types (type)
  (declare (type ctype type))
  (multiple-value-bind (res count)
		       (values-types type)
    (values (mapcar #'(lambda (type)
			(if (function-type-p type)
			    (specifier-type 'function)
			    type))
		    res)
	    count)))


;;; MAYBE-NEGATE-CHECK  --  Internal
;;;
;;;    Cont is a continuation we are doing a type check on and Types is a list
;;; of types that we are checking its values against.  If we have proven
;;; that Cont generates a fixed number of values, then for each value, we check
;;; whether it is cheaper to then difference between the the proven type and
;;; the corresponding type in Types.  If so, we opt for a :HAIRY check with
;;; that test negated.  Otherwise, we try to do a simple test, and if that is
;;; impossible, we do a hairy test with non-negated types.
;;;
;;;    When doing a non-negated hairy check, we call MAYBE-WEAKEN-CHECK to
;;; weaken the test to a convenient supertype (conditional on policy.)
;;;
(defun maybe-negate-check (cont types)
  (declare (type continuation cont) (list types))
  (multiple-value-bind
      (ptypes count)
      (no-function-values-types (continuation-proven-type cont))
    (if (eq count :unknown)
	(if (every #'type-check-template types)
	    (values :simple types)
	    (values :hairy
		    (mapcar #'(lambda (x)
				(list nil (maybe-weaken-check x cont) x))
			    types)))
	(let ((res (mapcar #'(lambda (p c)
			       (let ((diff (type-difference p c))
				     (weak (maybe-weaken-check c cont)))
				 (if (and diff
					  (< (type-test-cost diff)
					     (type-test-cost weak)))
				     (list t diff c)
				     (list nil weak c))))
			   ptypes types)))
	  (if (and (not (find-if #'first res))
		   (every #'type-check-template types))
	      (values :simple types)
	      (values :hairy res))))))
	    

;;; CONTINUATION-CHECK-TYPES  --  Interface
;;;
;;; Determines whether Cont's assertion is:
;;;  -- Checkable by the back end (:SIMPLE), or
;;;  -- Not checkable by the back end, but checkable via an explicit test in
;;;     type check conversion (:HAIRY), or
;;;  -- not reasonably checkable at all (:TOO-HAIRY).
;;;
;;; A type is checkable if it either represents a fixed number of values (as
;;; determined by VALUES-TYPES), or it is the assertion for an MV-Bind.  A type
;;; is simply checkable if all the type assertions have a TYPE-CHECK-TEMPLATE.
;;; In this :SIMPLE case, the second value is a list of the type restrictions
;;; specified for the leading positional values.
;;;
;;; In the :HAIRY case, the second value is a list of triples of the form:
;;;    (Not-P Type Original-Type)
;;;
;;; If true, the Not-P flag indicates a test that the corresponding value is
;;; *not* of the specified Type.  Original-Type is the type asserted on this
;;; value in the continuation, for use in error messages.  When Not-P is true,
;;; this will be different from Type.
;;;
;;; This allows us to take what has been proven about Cont's type into
;;; consideration.  If it is cheaper to test for the difference between the
;;; derived type and the asserted type, then we check for the negation of this
;;; type instead.
;;;
(defun continuation-check-types (cont)
  (declare (type continuation cont))
  (let ((type (continuation-asserted-type cont))
	(dest (continuation-dest cont)))
    (assert (not (eq type *wild-type*)))
    (multiple-value-bind (types count)
			 (no-function-values-types type)
      (cond ((not (eq count :unknown))
	     (maybe-negate-check cont types))
	    ((and (mv-combination-p dest)
		  (eq (basic-combination-kind dest) :local))
	     (assert (values-type-p type))
	     (maybe-negate-check cont (args-type-optional type)))
	    (t
	     (values :too-hairy nil))))))


;;; Probable-Type-Check-P  --  Internal
;;;
;;;    Return true if Cont is a continuation whose type the back end is likely
;;; to want to check.  Since we don't know what template the back end is going
;;; to choose to implement the continuation's DEST, we use a heuristic.  We
;;; always return T unless:
;;;  -- Nobody uses the value, or
;;;  -- Safety is totally unimportant, or
;;;  -- the continuation is an argument to an unknown function, or
;;;  -- the continuation is an argument to a known function that has no
;;;     IR2-Convert method or :fast-safe templates that are compatible with the
;;;     call's type.
;;;
;;; We must only return nil when it is *certain* that a check will not be done,
;;; since if we pass up this chance to do the check, it will be too late.  The
;;; penalty for being too conservative is duplicated type checks.
;;;
;;; We always return true if there is a compile-time type error on the
;;; continuation, so that this error will be signalled at runtime as well.
;;;
(defun probable-type-check-p (cont)
  (declare (type continuation cont))
  (let ((dest (continuation-dest cont)))
    (cond ((eq (continuation-type-check cont) :error))
	  ((or (not dest)
	       (policy dest (zerop safety)))
	   nil)
	  ((basic-combination-p dest)
	   (let ((kind (basic-combination-kind dest)))
	     (cond ((eq cont (basic-combination-fun dest)) t)
		   ((eq kind :local) t)
		   ((eq kind :full) nil)
		   ((function-info-ir2-convert kind) t)
		   (t
		    (dolist (template (function-info-templates kind) nil)
		      (when (eq (template-policy template) :fast-safe)
			(multiple-value-bind
			    (val win)
			    (valid-function-use dest (template-type template))
			  (when (or val (not win)) (return t)))))))))
	  (t t))))


;;; Make-Type-Check-Form  --  Internal
;;;
;;;    Return a form that we can convert to do a hairy type check of the
;;; specified Types.  Types is a list of the format returned by
;;; Continuation-Check-Types in the :HAIRY case.  In place of the actual
;;; value(s) we are to check, we use 'Dummy.  This constant reference is later
;;; replaced with the actual values continuation.
;;;
;;; Note that we don't attempt to check for required values being unsupplied.
;;; Such checking is impossible to efficiently do at the source level because
;;; our fixed-values conventions are optimized for the common MV-Bind case.
;;;
;;; We can always use Multiple-Value-Bind, since the macro is clever about
;;; binding a single variable.
;;;
(defun make-type-check-form (types)
  (collect ((temps))
    (dotimes (i (length types))
      (temps (gensym)))

    `(multiple-value-bind ,(temps)
			  'dummy
       ,@(mapcar #'(lambda (temp type)
		     (let* ((spec
			     (let ((*unparse-function-type-simplify* t))
			       (type-specifier (second type))))
			    (test (if (first type) `(not ,spec) spec)))
		       `(unless (typep ,temp ',test)
			  (%type-check-error
			   ,temp
			   ',(type-specifier (third type))))))
		 (temps) types)
       (values ,@(temps)))))
  

;;; Convert-Type-Check  --  Internal
;;;
;;;    Splice in explicit type check code immediately before the node that its
;;; Cont's Dest.  This code receives the value(s) that were being passed to
;;; Cont, checks the type(s) of the value(s), then passes them on to Cont.
;;; We:
;;;  -- Ensure that Cont starts a block, so that we can freely manipulate its
;;;     uses.
;;;  -- Make a new continuation and move Cont's uses to it.  Set type set
;;;     Type-Check in Cont to :DELETED to indicate that the check has been
;;;     done.
;;;  -- Make the Dest node start its block so that we can splice in the type
;;;     check code.
;;;  -- Splice in a new block before the Dest block, giving it all the Dest's
;;;     predecessors. 
;;;  -- Convert the check form, using the new block start as Start and a dummy
;;;     continuation as Cont.
;;;  -- Set the new block's start and end cleanups to the *start* cleanup of
;;;     Prev's block.  This overrides the incorrect default from
;;;     With-IR1-Environment.
;;;  -- Finish off the dummy continuation's block, and change the use to a use
;;;     of Cont.  (we need to use the dummy continuation to get the control
;;;     transfer right, since we want to go to Prev's block, not Cont's.)
;;;     Link the new block to Prev's block.
;;;  -- Substitute the new continuation for the dummy placeholder argument.
;;;     Since no let conversion has been done yet, we can find the placeholder.
;;;     The [mv-]combination node from the mv-bind in the check form will be
;;;     the Use of the new check continuation.  We substitute for the first
;;;     argument of this node.
;;;  -- Invoke local call analysis to convert the call to a let.
;;;
(defun convert-type-check (cont types)
  (declare (type continuation cont) (list types))
  (with-ir1-environment (continuation-dest cont)
    (ensure-block-start cont)    
    (let* ((new-start (make-continuation))
	   (dest (continuation-dest cont))
	   (prev (node-prev dest)))
      (continuation-starts-block new-start)
      (substitute-continuation-uses new-start cont)
      (setf (continuation-%type-check cont) :deleted)
      
      (when (continuation-use prev)
	(node-ends-block (continuation-use prev)))
      
      (let* ((prev-block (continuation-block prev))
	     (prev-cleanup (block-start-cleanup prev-block))
	     (new-block (continuation-block new-start))
	     (dummy (make-continuation)))
	(dolist (block (block-pred prev-block))
	  (change-block-successor block prev-block new-block))
	(ir1-convert new-start dummy (make-type-check-form types))
	(assert (eq (continuation-block dummy) new-block))

	(setf (block-start-cleanup new-block) prev-cleanup)
	(setf (block-end-cleanup new-block) prev-cleanup)

	(let ((node (continuation-use dummy)))
	  (setf (block-last new-block) node)
	  (delete-continuation-use node)
	  (add-continuation-use node cont))
	(link-blocks new-block prev-block))
      
      (let* ((node (continuation-use cont))
	     (args (basic-combination-args node))
	     (victim (first args)))
	(assert (and (= (length args) 1)
		     (eq (constant-value
			  (ref-leaf
			   (continuation-use victim)))
			 'dummy)))
	(substitute-continuation new-start victim)))

    (local-call-analyze *current-component*))
  
  (undefined-value))


;;; Generate-Type-Checks  --  Interface
;;;
;;;    Loop over all blocks in Component that have TYPE-CHECK set, looking for
;;; continuations with TYPE-CHECK T.  We do two mostly unrelated things: detect
;;; compile-time type errors and determine if and how to do run-time type
;;; checks.
;;;
;;;    If there is a compile-time type error, then we mark the continuation
;;; with a :ERROR kind, emit a warning if appropriate, and clear any
;;; FUNCTION-INFO if the continuation is an argument to a known call.  The last
;;; is done so that the back end doesn't have to worry about type errors in
;;; arguments to known functions.
;;;
;;;    If a continuation is too complex to be checked by the back end, or is
;;; better checked with explicit code, then convert to an explicit test.
;;; Assertions that can checked by the back end are passed through.  Assertions
;;; that can't be tested are flamed about and marked as not needing to be
;;; checked.
;;;
;;;    If we determine that a type check won't be done, then we set TYPE-CHECK
;;; to :NO-CHECK.  In the non-hairy cases, this is just to prevent us from
;;; wasting time coming to the same conclusion again on a later iteration.  In
;;; the hairy case, we must indicate to LTN that it must choose a safe
;;; implementation, since IR2 conversion will choke on the check.
;;;
(defun generate-type-checks (component)
  (do-blocks (block component)
    (when (block-type-check block)
      (do-nodes (node cont block)
	(when (eq (continuation-type-check cont) t)
	  
	  (let ((dtype (node-derived-type node))
		(atype (continuation-asserted-type cont)))
	    (unless (values-types-intersect dtype atype)
	      (setf (continuation-%type-check cont) :error)
	      (let ((dest (continuation-dest cont)))
		(when (and (combination-p dest)
			   (function-info-p (basic-combination-kind dest)))
		  (setf (basic-combination-kind dest) :full)))
	      (unless (policy node (= brevity 3))
		(let ((*compiler-error-context* node))
		  (if (and (ref-p node) (constant-p (ref-leaf node)))
		      (compiler-warning "This is not a ~S:~%  ~S"
					(type-specifier atype)
					(constant-value (ref-leaf node)))
		      (compiler-warning "Result is a ~S, not a ~S."
					(type-specifier dtype)
					(type-specifier atype)))))))
	  
	  (let ((check-p (probable-type-check-p cont)))
	    (multiple-value-bind (check types)
				 (continuation-check-types cont)
	      (ecase check
		(:simple
		 (unless check-p
		   (setf (continuation-%type-check cont) :no-check)))
		(:hairy
		 (if check-p
		     (convert-type-check cont types)
		     (setf (continuation-%type-check cont) :no-check)))
		(:too-hairy
		 (let* ((context (continuation-dest cont))
			(*compiler-error-context* context))
		   (when (policy context (>= safety brevity))
		     (compiler-note
		      "Type assertion too complex to check:~% ~S."
		      (type-specifier (continuation-asserted-type cont)))))
		 (setf (continuation-%type-check cont) :deleted)))))))

      (setf (block-type-check block) nil)))

  (undefined-value))
