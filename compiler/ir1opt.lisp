;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file implements the IR1 optimization phase of the compiler.  IR1
;;; optimization is a grab-bag of optimizations that don't make major changes
;;; to the block-level control flow and don't use flow analysis.  These
;;; optimizations can mostly be classified as "meta-evaluation", but there is a
;;; sizable top-down component as well.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;;
;;;    A hashtable from combination nodes to things describing how an
;;; optimization of the node failed.  If the thing is a list, then it is format
;;; arguments.  If it is a type, then the type is a type that the call failed
;;; to match.
;;;
(defvar *failed-optimizations* (make-hash-table :test #'eq))


;;;; Interface for obtaining results of constant folding:

;;; Constant-Continuation-P  --  Interface
;;;
;;;    Return true if the sole use of Cont is a reference to a constant leaf.
;;;
(proclaim '(function constant-continuation-p (continuation) boolean))
(defun constant-continuation-p (cont)
  (let ((use (continuation-use cont)))
    (and (ref-p use)
	 (constant-p (ref-leaf use)))))


;;; Continuation-Value  --  Interface
;;;
;;;    Return the constant value for a continuation whose only use is a
;;; constant node.
;;;
(proclaim '(function continuation-value (continuation) t))
(defun continuation-value (cont)
  (constant-value (ref-leaf (continuation-use cont))))


;;;; Interface for obtaining results of type inference:

;;; CONTINUATION-PROVEN-TYPE  --  Interface
;;;
;;;    Return a (possibly values) type that describes what we have proven about
;;; the type of Cont without taking any type assertions into consideration.
;;; This is just the union of the NODE-DERIVED-TYPE of all the uses.  Most
;;; often people use CONTINUATION-DERIVED-TYPE or CONTINUATION-TYPE instead of
;;; using this function directly.
;;;
(defun continuation-proven-type (cont)
  (declare (type continuation cont))
  (ecase (continuation-kind cont)
    ((:block-start :deleted-block-start)
     (let ((uses (block-start-uses (continuation-block cont))))
       (if uses
	   (do ((res (node-derived-type (first uses))
		     (values-type-union (node-derived-type (first current))
					res))
		(current (rest uses) (rest current)))
	       ((null current) res))
	   *empty-type*)))
    (:inside-block
     (node-derived-type (continuation-use cont)))))


;;; Continuation-Derived-Type  --  Interface
;;;
;;;    Our best guess for the type of this continuation's value.  Note that
;;; this may be Values or Function type, which cannot be passed as an argument
;;; to the normal type operations.  See Continuation-Type.  This may be called
;;; on deleted continuations, always returning *.
;;;
;;;    What we do is call CONTINUATION-PROVEN-TYPE and check whether the result
;;; is a subtype of the assertion.  If so, return the proven type and set
;;; TYPE-CHECK to nil.  Otherwise, return the intersection of the asserted and
;;; proven types, and set TYPE-CHECK T.  If TYPE-CHECK already has a non-null
;;; value, then preserve it.  Only in the somewhat unusual circumstance of
;;; a newly discovered assertion will we change TYPE-CHECK from NIL to T.
;;;
;;;    The result value is cached in the Continuation-%Derived-Type.  If the
;;; slot is true, just return that value, otherwise recompute and stash the
;;; value there.
;;;
(proclaim '(inline continuation-derived-type))
(defun continuation-derived-type (cont)
  (declare (type continuation cont))
  (or (continuation-%derived-type cont)
      (%continuation-derived-type cont)))
;;;
(defun %continuation-derived-type (cont)
  (declare (type continuation cont))
  (let ((proven (continuation-proven-type cont))
	(asserted (continuation-asserted-type cont)))
    (cond ((values-subtypep proven asserted)
	   (setf (continuation-%type-check cont) nil)
	   (setf (continuation-%derived-type cont) proven))
	  (t
	   (unless (or (continuation-%type-check cont)
		       (not (continuation-dest cont))
		       (eq asserted *universal-type*))
	     (setf (continuation-%type-check cont) t))

	   (setf (continuation-%derived-type cont)
		 (values-type-intersection asserted proven))))))


;;; CONTINUATION-TYPE-CHECK  --  Interface
;;;
;;;    Call CONTINUATION-DERIVED-TYPE to make sure the slot is up to date, then
;;; return it.
;;;
(proclaim '(inline continuation-type-check))
(defun continuation-type-check (cont)
  (declare (type continuation cont))
  (continuation-derived-type cont)
  (continuation-%type-check cont))


;;; Continuation-Type  --  Interface
;;;
;;;    Return the derived type for Cont's first value.  This is guaranteed not
;;; to be a Values or Function type.
;;;
(proclaim '(function continuation-type (continuation) type))
(defun continuation-type (cont)
  (single-value-type (continuation-derived-type cont)))


;;;; Interface routines used by optimizers:

;;; Reoptimize-Continuation  --  Interface
;;;
;;;    This function is called by optimizers to indicate that something
;;; interesting has happened to the value of Cont.  Optimizers must make sure
;;; that they don't call for reoptimization when nothing has happened, since
;;; optimization will fail to terminate.
;;;
;;;    We clear any cached type for the continuation and set the reoptimize
;;; flags on everything in sight, unless the continuation is deleted (in which
;;; case we do nothing.)
;;;
;;;    Since this can get called curing IR1 conversion, we have to be careful
;;; not to fly into space when the Dest's Prev is missing. 
;;;
(defun reoptimize-continuation (cont)
  (declare (type continuation cont))
  (unless (eq (continuation-kind cont) :deleted)
    (setf (continuation-%derived-type cont) nil)
    (let ((dest (continuation-dest cont)))
      (when dest
	(setf (continuation-reoptimize cont) t)
	(setf (node-reoptimize dest) t)
	(let ((prev (node-prev dest)))
	  (when prev
	    (let* ((block (continuation-block prev))
		   (component (block-component block)))
	      (setf (block-reoptimize block) t)
	      (setf (component-reoptimize component) t))))))
    (do-uses (node cont)
      (setf (block-type-check (node-block node)) t)))
  (undefined-value))


;;; Derive-Node-Type  --  Interface
;;;
;;;    Annotate Node to indicate that its result has been proven to be typep to
;;; RType.  After IR1 conversion has happened, this is the only correct way to
;;; supply information discovered about a node's type.  If you fuck with the
;;; Node-Derived-Type directly, then information may be lost and reoptimization
;;; may not happen. 
;;;
;;;    What we do is intersect Rtype with Node's Derived-Type.  If the
;;; intersection is different from the old type, then we do a
;;; Reoptimize-Continuation on the Node-Cont.
;;;
(defun derive-node-type (node rtype)
  (declare (type node node) (type ctype rtype))
  (let ((node-type (node-derived-type node)))
    (unless (eq node-type rtype)
      (let ((int (values-type-intersection node-type rtype)))
	(when (type/= node-type int)
	  (setf (node-derived-type node) int)
	  (reoptimize-continuation (node-cont node))))))
  (undefined-value))


;;; Assert-Continuation-Type  --  Interface
;;;
;;;    Similar to Derive-Node-Type, but asserts that it is an error for Cont's
;;; value not to be typep to Type.  If we improve the assertion, we set
;;; BLOCK-TYPE-CHECK to guarantee that the new assertion will be checked.
;;;
(defun assert-continuation-type (cont type)
  (declare (type continuation cont) (type ctype type))
  (let ((cont-type (continuation-asserted-type cont)))
    (unless (eq cont-type type)
      (let ((int (values-type-intersection cont-type type)))
	(when (type/= cont-type int)
	  (setf (continuation-asserted-type cont) int)
	  (do-uses (node cont)
	    (let ((block (node-block node)))
	      (setf (block-type-check block) t)
	      (setf (block-type-asserted block) t)))
	  (reoptimize-continuation cont)))))
  (undefined-value))


;;; Assert-Call-Type  --  Interface
;;;
;;;    Assert that Call is to a function of the specified Type.  It is assumed
;;; that the call is legal and has only constants in the keyword positions.
;;;
(defun assert-call-type (call type)
  (declare (type combination call) (type function-type type))
  (derive-node-type call (function-type-returns type))
  (let ((args (combination-args call)))
    (dolist (req (function-type-required type))
      (when (null args) (return-from assert-call-type))
      (let ((arg (pop args)))
	(assert-continuation-type arg req)))
    (dolist (opt (function-type-optional type))
      (when (null args) (return-from assert-call-type))
      (let ((arg (pop args)))
	(assert-continuation-type arg opt)))

    (let ((rest (function-type-rest type)))
      (when rest
	(dolist (arg args)
	  (assert-continuation-type arg rest))))

    (dolist (key (function-type-keywords type))
      (let ((name (key-info-name key)))
	(do ((arg args (cddr arg)))
	    ((null arg))
	  (when (eq (continuation-value (first arg)) name)
	    (assert-continuation-type
	     (second arg) (key-info-type key)))))))
  (undefined-value))


;;; IR1-Optimize  --  Interface
;;;
;;;    Do one forward pass over Component, deleting unreachable blocks and
;;; doing IR1 optimizations.  We can ignore all blocks that don't have
;;; Block-Reoptimize set.  If Component-Reoptimize is true when we are done,
;;; then another iteration would be beneficial.
;;;
;;;    We delete blocks when there is either no predecessor or the block is in
;;; a lambda that has been deleted.  These blocks would eventually be deleted
;;; by DFO recomputation, but doing it here immediately makes the effect
;;; avaliable to IR1 optimization.
;;;
(defun ir1-optimize (component)
  (declare (type component component))
  (setf (component-reoptimize component) nil)
  (do-blocks (block component)
    (cond
     ((or (block-delete-p block)
	  (null (block-pred block))
	  (eq (functional-kind (block-lambda block)) :deleted))
      (delete-block block))
     (t
      (loop
	(let ((succ (block-succ block)))
	  (unless (and succ (null (rest succ)))
	    (return)))
	
	(let ((last (block-last block)))
	  (typecase last
	    (cif
	     (flush-dest (if-test last))
	     (when (unlink-node last) (return)))
	    (exit
	     (when (maybe-delete-exit last) (return)))))
	
	(unless (join-successor-if-possible block)
	  (return)))

      (when (and (block-reoptimize block)
		 (block-component block))
	(assert (not (block-delete-p block)))
	(ir1-optimize-block block))

      (when (and (block-flush-p block)
		 (block-component block))
	(assert (not (block-delete-p block)))
	(flush-dead-code block)))))

  (undefined-value))


;;; IR1-Optimize-Block  --  Internal
;;;
;;;    Loop over the nodes in Block, looking for stuff that needs to be
;;; optimized.  We dispatch off of the type of each node with its reoptimize
;;; flag set:
;;; -- With a combination, we call Propagate-Function-Change whenever the
;;;    function changes, and call IR1-Optimize-Combination if any argument
;;;    changes.
;;; -- With an Exit, we derive the node's type from the Value's type.  We don't
;;;    propagate Cont's assertion to the Value, since if we did, this would
;;;    move the checking of Cont's assertion to the exit.  This wouldn't work
;;;    with Catch and UWP, where the Exit node is just a placeholder for the
;;;    actual unknown exit.
;;;
;;; Note that we clear the node & block reoptimize flags *before* doing the
;;; optimization.  This ensures that the node or block will be reoptimized if
;;; necessary.  We leave the NODE-OPTIMIZE flag set doing into
;;; IR1-OPTIMIZE-RETURN, since it wants to clear the flag itself.
;;;
(defun ir1-optimize-block (block)
  (declare (type cblock block))
  (setf (block-reoptimize block) nil)
  (do-nodes (node cont block)
    (when (node-reoptimize node)
      (setf (node-reoptimize node) nil)
      (typecase node
	(ref)
	(combination
	 (when (continuation-reoptimize (basic-combination-fun node))
	   (propagate-function-change node))
	 (when (dolist (arg (basic-combination-args node) nil)
		 (when (and arg (continuation-reoptimize arg))
		   (return t)))
	   (ir1-optimize-combination node)))
	(cif 
	 (ir1-optimize-if node))
	(creturn
	 (setf (node-reoptimize node) t)
	 (ir1-optimize-return node))
	(mv-combination
	 (when (and (eq (basic-combination-kind node) :local)
		    (continuation-reoptimize
		     (first (basic-combination-args node))))
	   (ir1-optimize-mv-bind node)))
	(exit
	 (let ((value (exit-value node)))
	   (when value
	     (derive-node-type node (continuation-derived-type value)))))
	(cset
	 (ir1-optimize-set node)))))
  (undefined-value))


;;; Join-Successor-If-Possible  --  Internal
;;;
;;;    We cannot combine with a successor block if:
;;;  1] The successor has more than one predecessor.
;;;  2] The last node's Cont is also used somewhere else.
;;;  3] The successor is the current block (infinite loop). 
;;;  4] The next block has a different cleanup, and thus we may want to insert
;;;     cleanup code between the two blocks at some point.
;;;  5] The next block has a different home lambda, and thus the control
;;;     transfer is a non-local exit.
;;;
;;; If we succeed, we return true, otherwise false.
;;;
;;;    Joining is easy when the successor's Start continuation is the same from
;;; our Last's Cont.  If they differ, then we can still join when the last
;;; continuation has no next and the next continuation has no uses.  In this
;;; case, we replace the next continuation with the last before joining the
;;; blocks.
;;;
(defun join-successor-if-possible (block)
  (declare (type cblock block))
  (let ((next (first (block-succ block))))
    (when (block-lambda next)
      (let* ((last (block-last block))
	     (last-cont (node-cont last))
	     (next-cont (block-start next))
	     (cleanup (block-end-cleanup block))
	     (next-cleanup (block-start-cleanup next))
	     (lambda (block-lambda block))
	     (next-lambda (block-lambda next)))
	(cond ((or (rest (block-pred next))
		   (not (eq (continuation-use last-cont) last))
		   (eq next block)
		   (not (eq (lambda-home lambda) (lambda-home next-lambda)))
		   (not (eq (find-enclosing-cleanup cleanup)
			    (find-enclosing-cleanup next-cleanup))))
	       nil)
	      ((eq last-cont next-cont)
	       (join-blocks block next)
	       t)
	      ((and (null (block-start-uses next))
		    (eq (continuation-kind last-cont) :inside-block))
	       (let ((next-node (continuation-next next-cont)))
		 (assert (not (continuation-dest next-cont)))
		 (delete-continuation next-cont)
		 (setf (node-prev next-node) last-cont)
		 (setf (continuation-next last-cont) next-node)
		 (setf (block-start next) last-cont)
		 (join-blocks block next))
	       t)
	      (t
	       nil))))))


;;; Join-Blocks  --  Internal
;;;
;;;    Join together two blocks which have the same ending/starting
;;; continuation.  The code in Block2 is moved into Block1 and Block2 is
;;; deleted from the DFO.  The End-Cleanup for Block1 is set to that for
;;; Block2 so that we don't lose cleanup info.  We combine the optimize flags
;;; for the two blocks so that any indicated optimization gets done.
;;;
(defun join-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let* ((last (block-last block2))
	 (last-cont (node-cont last))
	 (succ (block-succ block2))
	 (start2 (block-start block2)))
    (do ((cont start2 (node-cont (continuation-next cont))))
	((eq cont last-cont)
	 (when (eq (continuation-kind last-cont) :inside-block)
	   (setf (continuation-block last-cont) block1)))
      (setf (continuation-block cont) block1))

    (unlink-blocks block1 block2)
    (dolist (block succ)
      (unlink-blocks block2 block)
      (link-blocks block1 block))

    (setf (block-last block1) last)
    (setf (continuation-kind start2) :inside-block))

  (setf (block-end-cleanup block1) (block-end-cleanup block2))

  (setf (block-reoptimize block1)
	(or (block-reoptimize block1) (block-reoptimize block2)))
  (setf (block-type-asserted block1) t)
  (setf (block-test-modified block1) t)
  
  (let ((next (block-next block2))
	(prev (block-prev block2)))
    (setf (block-next prev) next)
    (setf (block-prev next) prev))

  (undefined-value))


;;;; Local call return type propagation:

;;; Find-Result-Type  --  Internal
;;;
;;;    This function is called on RETURN nodes that have their REOPTIMIZE flag
;;; set.  It iterates over the uses of the RESULT, looking for interesting
;;; stuff to update the TAIL-SET:
;;;  -- If a use is a local call, then we check that the called function has
;;;     the tail set Tails.  If we encounter any different tail set, we return
;;;     the second value true.
;;;  -- If a use isn't a local call, then we union its type together with the
;;;     types of other such uses.  We assign to the RETURN-RESULT-TYPE the
;;;     intersection of this type with the RESULT's asserted type.  We can make
;;;     this intersection now (potentially before type checking) because this
;;;     assertion on the result will eventually be checked (if appropriate.)
;;;
(defun find-result-type (node tails)
  (declare (type creturn node))
  (let ((result (return-result node))
	(retry nil))
    (collect ((use-union *empty-type* values-type-union))
      (do-uses (use result)
	(if (and (basic-combination-p use)
		 (eq (basic-combination-kind use) :local))
	    (when (merge-tail-sets use tails)
	      (setq retry t))
	    (use-union (node-derived-type use))))
      (let ((int (values-type-intersection
		  (continuation-asserted-type result)
		  (use-union))))
	(setf (return-result-type node) int)))
    retry))


;;; Merge-Tail-Sets  --  Internal
;;;
;;;    This function handles merging the tail sets if Call is a call to a
;;; function with a different TAIL-SET than Ret-Set.  We return true if we do
;;; anything.
;;;
;;;     It is assumed that Call sends its value to a RETURN node.  We
;;; destructively modify the set for the returning function to represent both,
;;; and then change all the functions in callee's set to reference the first.
;;;
;;;    If the called function has no tail set, then do nothing; if it doesn't
;;; return, then it can't affect the callers value.
;;;
(defun merge-tail-sets (call ret-set)
  (declare (type basic-combination call) (type tail-set ret-set))
  (let ((fun-set (lambda-tail-set (combination-lambda call))))
    (when (and fun-set (not (eq ret-set fun-set)))
      (let ((funs (tail-set-functions fun-set)))
	(dolist (fun funs)
	  (setf (lambda-tail-set fun) ret-set))
	(setf (tail-set-functions ret-set)
	      (nconc (tail-set-functions ret-set) funs)))
      t)))


;;; IR1-Optimize-Return  --  Internal
;;;
;;;    Do stuff to realize that something has changed about the value delivered
;;; to a return node.  Since we consider the return values of all functions in
;;; the tail set to be equivalent, this amounts to bringing the entire tail set
;;; up to date.  We iterate over the returns for all the functions in the tail
;;; set, reanalyzing them all (not treating Node specially.)
;;;
;;;    During this iteration, we may discover new functions that should be
;;; added to the tail set.  If this happens, we restart the iteration over the
;;; TAIL-SET-FUNCTIONS.  Note that this really doesn't duplicate much work, as
;;; we clear the NODE-REOPTIMIZE flags in the return nodes as we go, thus we
;;; don't call FIND-RESULT-TYPE on any given return more than once.
;;;
;;;    Restarting the iteration doesn't disturb the computation of the result
;;; type RES, since we will just be adding more types to the union.  (or when
;;; we iterate over a return multiple times, unioning in the same type more
;;; than once.)
;;;
;;;    When we are done, we check if the new type is different from the old
;;; TAIL-SET-TYPE.  If so, we set the type and also reoptimize all the
;;; continuations for references to functions in the tail set.  This will
;;; cause IR1-OPTIMIZE-COMBINATION to derive the new type as the results of the
;;; calls.
;;;
(defun ir1-optimize-return (node)
  (declare (type creturn node))
  (let ((tails (lambda-tail-set (return-lambda node))))
    (collect ((res *empty-type* values-type-union))
      (loop
	(block RETRY
	  (let ((funs (tail-set-functions tails)))
	    (dolist (fun funs)
	      (let ((return (lambda-return fun)))
		(when (node-reoptimize return)
		  (setf (node-reoptimize node) nil)
		  (when (find-result-type return tails) (return-from RETRY)))
		(res (return-result-type return)))))
	  (return)))
      
      (when (type/= (res) (tail-set-type tails))
	(setf (tail-set-type tails) (res))
	(dolist (fun (tail-set-functions tails))
	  (dolist (ref (leaf-refs fun))
	    (reoptimize-continuation (node-cont ref)))))))

  (undefined-value))


;;; IR1-Optimize-If  --  Internal
;;;
;;;    If the test has multiple uses, replicate the node when possible.  Also
;;; check if the predicate is known to be true or false, deleting the IF node
;;; in favor of the appropriate branch when this is the case.
;;;
(defun ir1-optimize-if (node)
  (declare (type cif node))
  (let ((test (if-test node))
	(block (node-block node)))
    
    (when (and (eq (block-start block) test)
	       (eq (continuation-next test) node)
	       (rest (block-start-uses block)))
      (do-uses (use test)
	(when (immediately-used-p test use)
	  (convert-if-if use node)
	  (when (continuation-use test) (return)))))

    (let* ((type (continuation-type test))
	   (victim
	    (cond ((constant-continuation-p test)
		   (if (continuation-value test)
		       (if-alternative node)
		       (if-consequent node)))
		  ((not (types-intersect type *null-type*))
		   (if-alternative node))
		  ((type= type *null-type*)
		   (if-consequent node)))))
      (when victim
	(flush-dest test)
	(when (rest (block-succ block))
	  (unlink-blocks block victim))
	(setf (component-reanalyze (block-component (node-block node))) t)
	(unlink-node node))))
  (undefined-value))


;;; Convert-If-If  --  Internal
;;;
;;;    Create a new copy of an IF Node that tests the value of the node Use.
;;; The test must have >1 use, and must be immediately used by Use.  Node must
;;; be the only node in its block (implying that block-start = if-test).
;;;
;;;    This optimization has an effect semantically similar to the
;;; source-to-source transformation:
;;;    (IF (IF A B C) D E) ==>
;;;    (IF A (IF B D E) (IF C D E))
;;;
(defun convert-if-if (use node)
  (declare (type node use) (type cif node))
  (with-ir1-environment node
    (let* ((block (node-block node))
	   (test (if-test node))
	   (cblock (if-consequent node))
	   (ablock (if-alternative node))
	   (use-block (node-block use))
	   (dummy-cont (make-continuation))
	   (new-cont (make-continuation))
	   (new-node (make-if :test new-cont  :source (node-source node)
			      :consequent cblock  :alternative ablock))
	   (new-block (continuation-starts-block new-cont)))
      (prev-link new-node new-cont)
      (setf (continuation-dest new-cont) new-node)
      (add-continuation-use new-node dummy-cont)
      (setf (block-last new-block) new-node)

      (unlink-blocks use-block block)
      (delete-continuation-use use)
      (add-continuation-use use new-cont)
      (link-blocks use-block new-block)
      
      (link-blocks new-block cblock)
      (link-blocks new-block ablock)

      (reoptimize-continuation test)
      (reoptimize-continuation new-cont)
      (setf (component-reanalyze *current-component*) t)))
  (undefined-value))


;;;; Exit IR1 optimization:

;;; Maybe-Delete-Exit  --  Interface
;;;
;;; This function attempts to delete an exit node, returning true if it
;;; deletes the block as a consequence:
;;; -- If the exit is degenerate (has no Entry), then we don't do anything,
;;;    since there is nothing to be done.
;;; -- If the exit node and its Entry have the same home lambda then we know
;;;    the exit is local, and can delete the exit.  We change uses of the
;;;    Exit-Value to be uses of the original continuation, then unlink the
;;;    node.
;;; -- If there is no value (as in a GO), then we skip the value semantics.
;;;
;;; This function is also called by environment analysis, since it wants all
;;; exits to be optimized even if normal optimization was omitted.
;;;
(defun maybe-delete-exit (node)
  (declare (type exit node))
  (let ((value (exit-value node))
	(entry (exit-entry node))
	(cont (node-cont node)))
    (when (and entry
	       (eq (lambda-home (block-lambda (node-block node)))
		   (lambda-home (block-lambda (node-block entry)))))
      (prog1
	  (unlink-node node)
	(when value
	  (substitute-continuation-uses cont value))))))


;;;; Combination IR1 optimization:

;;; Ir1-Optimize-Combination  --  Internal
;;;
;;;    Do IR1 optimizations on a Combination node.
;;;
(proclaim '(function ir1-optimize-combination (combination) void))
(defun ir1-optimize-combination (node)
  (let ((args (basic-combination-args node))
	(kind (basic-combination-kind node)))
    (case kind
      (:local
       (let ((fun (combination-lambda node)))
	 (if (eq (functional-kind fun) :let)
	     (propagate-let-args node fun)
	     (propagate-local-call-args node fun))))
      (:full
       (dolist (arg args)
	 (when arg
	   (setf (continuation-reoptimize arg) nil))))
      (t
       (dolist (arg args)
	 (when arg
	   (setf (continuation-reoptimize arg) nil)))

       (let ((attr (function-info-attributes kind)))
	 (when (and (ir1-attributep attr foldable)
		    (not (ir1-attributep attr call))
		    (every #'constant-continuation-p args)
		    (continuation-dest (node-cont node)))
	   (constant-fold-call node)
	   (return-from ir1-optimize-combination)))
       
       (let ((fun (function-info-derive-type kind)))
	 (when fun
	   (let ((res (funcall fun node)))
	     (when res
	       (derive-node-type node res)))))
       
       (let ((fun (function-info-optimizer kind)))
	 (unless (and fun (funcall fun node))
	   (dolist (x (function-info-transforms kind))
	     (unless (ir1-transform node (car x) (cdr x))
	       (return))))))))

  (undefined-value))


;;; Recognize-Known-Call  --  Interface
;;;
;;;    If Call is a call to a known function, mark it as such by setting the
;;; Kind.  In addition to a direct check for the function name in the table, we
;;; also must check for slot accessors.  If the function is a slot accessor,
;;; then we set the combination kind to the function info of %Slot-Setter or
;;; %Slot-Accessor, as appropriate.
;;;
(defun recognize-known-call (call)
  (declare (type combination call))
  (let* ((fun (basic-combination-fun call))
	 (name (continuation-function-name fun)))
    (when name
      (let ((info (info function info name)))
	(cond (info
	       (setf (basic-combination-kind call) info))
	      ((slot-accessor-p (ref-leaf (continuation-use fun)))
	       (setf (basic-combination-kind call)
		     (info function info
			   (if (consp name)
			       '%slot-setter
			       '%slot-accessor))))))))
  (undefined-value))


;;; Propagate-Function-Change  --  Internal
;;;
;;;    Called by Ir1-Optimize when the function for a call has changed.
;;; If the call is to a functional, then we attempt to convert it to a local
;;; call, otherwise we check the call for legality with respect to the new
;;; type; if it is illegal, we mark the Ref as :Notline and punt.
;;;
;;; If we do have a good type for the call, we propagate type information from
;;; the type to the arg and result continuations.  If we discover that the call
;;; is to a known global function, then we mark the combination as known.
;;;
(defun propagate-function-change (call)
  (declare (type combination call))
  (let* ((fun (combination-fun call))
	 (use (continuation-use fun))
	 (type (continuation-derived-type fun))
	 (*compiler-error-context* call))
    (setf (continuation-reoptimize fun) nil)
    (cond ((or (not (ref-p use))
	       (eq (ref-inlinep use) :notinline)))
	  ((functional-p (ref-leaf use))
	   (let ((leaf (ref-leaf use)))
	     (cond ((eq (combination-kind call) :local)
		    (let ((tail-set (lambda-tail-set leaf)))
		      (when tail-set
			(derive-node-type
			 call (tail-set-type tail-set)))))
		   ((not (eq (ref-inlinep use) :notinline))
		    (convert-call-if-possible use call)
		    (maybe-let-convert leaf)))))
	  ((not (function-type-p type)))
	  ((valid-function-use call type
			       :argument-test #'always-subtypep
			       :result-test #'always-subtypep
			       :error-function #'compiler-warning
			       :warning-function #'compiler-note)
	   (assert-call-type call type)
	   (recognize-known-call call))
	  (t
	   (setf (ref-inlinep use) :notinline))))

  (undefined-value))


;;;; Known function optimization:

;;; IR1-Transform  --  Internal
;;;
;;;    Attempt to transform Node using Function, subject to the call type
;;; constraint Type.  If we are inhibited from doing the transform for some
;;; reason and Flame is true, then we make a note of the message in 
;;; *failed-optimizations* for IR1 finalize to pick up.  We return true if
;;; the transform failed, and thus further transformation should be
;;; attempted.  We return false if either the transform suceeded or was
;;; aborted.
;;;
(defun ir1-transform (node type fun)
  (declare (type combination node) (type ctype type) (type function fun))
  (let ((constrained (function-type-p type))
	(flame (policy node (> speed brevity)))
	(*compiler-error-context* node))
    (cond ((or (not constrained)
	       (valid-function-use node type))
	   (multiple-value-bind
	       (severity args)
	       (catch 'give-up
		 (transform-call node (funcall fun node))
		 (remhash node *failed-optimizations*)
		 (values :none nil))
	     (ecase severity
	       (:none nil)
	       (:aborted
		(setf (combination-kind node) :full)
		(setf (ref-inlinep (continuation-use (combination-fun node)))
		      :notinline)
		(when args
		  (apply #'compiler-warning args))
		nil)
	       (:failure 
		(when (and flame args)
		  (setf (gethash node *failed-optimizations*) args))
		t))))
	  ((and flame
		(valid-function-use node type
				    :argument-test #'types-intersect
				    :result-test #'values-types-intersect))
	   (setf (gethash node *failed-optimizations*) type)
	   t))))


;;; GIVE-UP, ABORT-TRANSFORM  --  Interface
;;;
;;;    Just throw the severity and args...
;;;
(proclaim '(function give-up (&rest t) nil))
(defun give-up (&rest args)
  "This function is used to throw out of an IR1 transform, aborting this
  attempt to transform the call, but admitting the possibility that this or
  some other transform will later suceed.  If arguments are supplied, they are
  format arguments for an efficiency note."
  (throw 'give-up (values :failure args)))
;;;
(defun abort-transform (&rest args)
  "This function is used to throw out of an IR1 transform and force a normal
  call to the function at run time.  No further optimizations will be
  attempted."
  (throw 'give-up (values :aborted args)))


;;; Transform-Call  --  Internal
;;;
;;;    Take the lambda-expression Res, IR1 convert it in the proper
;;; environment, and then install it as the function for the call Node.  We do
;;; local call analysis so that the new function is integrated into the control
;;; flow.  We set the Reanalyze flag in the component to cause the DFO to be
;;; recomputed at soonest convenience.
;;;
(defun transform-call (node res)
  (declare (type combination node) (list res))
  (with-ir1-environment node
    (let ((new-fun (ir1-convert-lambda res (node-source node)))
	  (ref (continuation-use (combination-fun node))))
      (change-ref-leaf ref new-fun)
      (setf (combination-kind node) :full)
      (local-call-analyze *current-component*)))
  (undefined-value))


;;; Constant-Fold-Call  --  Internal
;;;
;;;    Replace a call to a foldable function of constant arguments with the
;;; result of evaluating the form.  We insert the resulting constant node after
;;; the call, stealing the call's continuation.  We give the call a
;;; continuation with no Dest, which should cause it and its arguments to go
;;; away.  If there is an error during the evaluation, we give a warning and
;;; leave the call alone, making the call a full call and marking it as
;;; :notinline to make sure that it stays that way.
;;;
;;;    For now, if the result is other than one value, we don't fold it.
;;;
(defun constant-fold-call (call)
  (declare (type combination call))
  (let* ((args (mapcar #'continuation-value (combination-args call)))
	 (ref (continuation-use (combination-fun call)))
	 (fun (leaf-name (ref-leaf ref))))
    
    (multiple-value-bind (values win)
			 (careful-call fun args call "constant folding")
      (cond
       ((not win)
	(setf (ref-inlinep ref) :notinline)
	(setf (combination-kind call) :full))
       ((= (length values) 1)
	(with-ir1-environment call
	  (let* ((leaf (find-constant (first values)))
		 (node (make-ref (leaf-type leaf)
				 (node-source call)
				 leaf
				 nil))
		 (dummy (make-continuation))
		 (cont (node-cont call))
		 (block (node-block call))
		 (next (continuation-next cont)))
	    (push node (leaf-refs leaf))
	    (setf (leaf-ever-used leaf) t)
	    
	    (delete-continuation-use call)
	    (add-continuation-use call dummy)
	    (prev-link node dummy)
	    (add-continuation-use node cont)
	    (setf (continuation-next cont) next)
	    (when (eq call (block-last block))
	      (setf (block-last block) node))
	    (reoptimize-continuation cont)))))))
  
  (undefined-value))


;;;; Local call optimization:

;;; Propagate-To-Refs  --  Internal
;;;
;;;    Propagate Type to Leaf and its Refs, marking things changed.  If the
;;; leaf type is a function type, then just leave it alone, since TYPE is never
;;; going to be more specific than that (and TYPE-INTERSECTION would choke.)
;;;
(defun propagate-to-refs (leaf type)
  (declare (type leaf leaf) (type ctype type))
  (let ((var-type (leaf-type leaf)))
    (unless (function-type-p var-type)
      (let ((int (type-intersection var-type type)))
	(when (type/= int var-type)
	  (setf (leaf-type leaf) int)
	  (dolist (ref (leaf-refs leaf))
	    (derive-node-type ref int))))
      (undefined-value))))


;;; PROPAGATE-FROM-SETS  --  Internal
;;;
;;;    Figure out the type of a LET variable that has sets.  We compute the
;;; union of the initial value Type and the types of all the set values and to
;;; a PROPAGATE-TO-REFS with this type.
;;;
(defun propagate-from-sets (var type)
  (collect ((res type type-union))
    (dolist (set (basic-var-sets var))
      (res (continuation-type (set-value set)))
      (setf (node-reoptimize set) nil))
    (propagate-to-refs var (res)))
  (undefined-value))


;;; IR1-OPTIMIZE-SET  --  Internal
;;;
;;;    If a let variable, find the initial value's type and do
;;; PROPAGATE-FROM-SETS.  We also derive the VALUE's type as the node's type. 
;;;
(defun ir1-optimize-set (node)
  (declare (type cset node))
  (let ((var (set-var node)))
    (when (and (lambda-var-p var) (leaf-refs var))
      (let ((home (lambda-var-home var)))
	(when (eq (functional-kind home) :let)
	  (let ((iv (let-var-initial-value var)))
	    (setf (continuation-reoptimize iv) nil)
	    (propagate-from-sets var (continuation-type iv)))))))
  
  (derive-node-type node (continuation-type (set-value node)))
  (undefined-value))


;;; CONSTANT-REFERENCE-P  --  Internal
;;;
;;;    Return true if the value of Ref will always be the same (and is thus
;;; legal to substitute.)
;;;
(defun constant-reference-p (ref)
  (declare (type ref ref))
  (let ((leaf (ref-leaf ref)))
    (typecase leaf
      (constant t)
      (functional t)
      (lambda-var
       (null (lambda-var-sets leaf)))
      (global-var
       (case (global-var-kind leaf)
	 (:global-function
	  (not (eq (ref-inlinep ref) :notinline)))
	 (:constant t))))))


;;; SUBSTITUTE-SINGLE-USE-CONTINUATION  --  Internal
;;;
;;;    If we have a non-set let var with a single use, then (if possible)
;;; replace the variable reference's CONT with the arg continuation.  This is
;;; inhibited when:
;;; -- CONT has other uses, or
;;; -- CONT receives multiple values, or
;;; -- the reference is in a different environment from the variable.
;;;
;;;    We change the Ref to be a reference to NIL with unused value, and let it
;;; be flushed as dead code.  A side-effect of this substitution is to delete
;;; the variable.
;;;
(defun substitute-single-use-continuation (arg var)
  (declare (type continuation arg) (type lambda-var var))
  (let* ((ref (first (leaf-refs var)))
	 (cont (node-cont ref))
	 (dest (continuation-dest cont)))
    (when (and (eq (continuation-use cont) ref)
	       dest
	       (not (typep dest '(or creturn exit mv-combination)))
	       (eq (lambda-home (block-lambda (node-block ref)))
		   (lambda-var-home var)))
      (assert-continuation-type arg (continuation-asserted-type cont))
      (change-ref-leaf ref (find-constant nil))
      (substitute-continuation arg cont)
      (reoptimize-continuation arg)
      t)))


;;; Propagate-Let-Args  --  Internal
;;;
;;;    This function is called when one of the arguments to a LET changes.  We
;;; look at each changed argument.  If the corresponding variable is set, then
;;; we call PROPAGATE-FROM-SETS.  Otherwise, we consider substituting for the
;;; variable, and also propagate derived-type information for the arg to all
;;; the Var's refs.
;;;
;;;    Substitution is inhibited when the Ref's derived type isn't a subtype of
;;; the argument's asserted type.  This prevents type checking from being
;;; defeated, and also ensures that the best representation for the variable
;;; can be used.
;;;
;;;    Note that we are responsible for clearing the Continuation-Reoptimize
;;; flags.
;;;
(defun propagate-let-args (call fun)
  (declare (type combination call) (type clambda fun))
  (mapc #'(lambda (arg var)
	    (when (and arg
		       (continuation-reoptimize arg))
	      (setf (continuation-reoptimize arg) nil)
	      (cond
	       ((lambda-var-sets var)
		(propagate-from-sets var (continuation-type arg)))
	       ((let ((use (continuation-use arg)))
		  (when (ref-p use)
		    (let ((leaf (ref-leaf use)))
		      (when (and (constant-reference-p use)
				 (values-subtypep
				  (node-derived-type use)
				  (continuation-asserted-type arg)))
			(substitute-leaf leaf var)
			(propagate-to-refs var (continuation-type arg))
			t)))))
	       ((and (null (rest (leaf-refs var)))
		     (substitute-single-use-continuation arg var)))
	       (t
		(propagate-to-refs var (continuation-type arg))))))
	(basic-combination-args call)
	(lambda-vars fun))
  (undefined-value))


;;; Propagate-Local-Call-Args  --  Internal
;;;
;;;    This function is called when one of the args to a non-let local call
;;; changes.  For each changed argument corresponding to an unset variable, we
;;; compute the union of the types across all calls and propagate this type
;;; information to the var's refs.
;;;
;;;    If the function has an XEP, then we don't do anything, since we won't
;;; discover anything.
;;;
;;;    We can clear the Continuation-Reoptimize flags for arguments in all calls
;;; corresponding to changed arguments in Call, since the only use in IR1
;;; optimization of the Reoptimize flag for local call args is right here.
;;;
(defun propagate-local-call-args (call fun)
  (declare (type combination call) (type clambda fun))

  (unless (functional-entry-function fun)
    (let* ((vars (lambda-vars fun))
	   (union (mapcar #'(lambda (arg var)
			      (when (and arg
					 (continuation-reoptimize arg)
					 (null (basic-var-sets var)))
				(continuation-type arg)))
			  (basic-combination-args call)
			  vars))
	   (this-ref (continuation-use (basic-combination-fun call))))
      
      (dolist (arg (basic-combination-args call))
	(when arg
	  (setf (continuation-reoptimize arg) nil)))
      
      (dolist (ref (leaf-refs fun))
	(unless (eq ref this-ref)
	  (setq union
		(mapcar #'(lambda (this-arg old)
			    (when old
			      (setf (continuation-reoptimize this-arg) nil)
			      (type-union (continuation-type this-arg) old)))
			(basic-combination-args
			 (continuation-dest (node-cont ref)))
			union))))
      
      (mapc #'(lambda (var type)
		(when type
		  (propagate-to-refs var type)))
	    vars union)))
  
  (undefined-value))


;;; IR1-OPTIMIZE-MV-BIND  --  Internal
;;;
;;;    Propagate derived type info from the values continuation to the vars.
;;;
(defun ir1-optimize-mv-bind (node)
  (declare (type mv-combination node))
  (let ((arg (first (basic-combination-args node)))
	(vars (lambda-vars (combination-lambda node))))
    (multiple-value-bind (types nvals)
			 (values-types (continuation-derived-type arg))
      (unless (eq nvals :unknown)
	(mapc #'(lambda (var type)
		  (if (basic-var-sets var)
		      (propagate-from-sets var type)
		      (propagate-to-refs var type)))
		vars
		(append types
			(make-list (max (- (length vars) nvals) 0)
				   :initial-element *null-type*)))))

    (setf (continuation-reoptimize arg) nil))
  (undefined-value))


;;; Flush-Dead-Code  --  Internal
;;;
;;;    Delete any nodes in Block whose value is unused and have no
;;; side-effects.  We can delete sets of lexical variables when the set
;;; variable has no references.
;;;
;;; [### For now, don't delete potentially flushable calls when they have the
;;; Call attribute.  Someday we should look at the funcitonal args to determine
;;; if they have any side-effects.] 
;;;
(defun flush-dead-code (block)
  (declare (type cblock block))
  (do-nodes-backwards (node cont block)
    (unless (continuation-dest cont)
      (typecase node
	(ref
	 (delete-ref node)
	 (unlink-node node))
	(combination
	 (let ((info (combination-kind node)))
	   (when (function-info-p info)	     
	     (let ((attr (function-info-attributes info)))
	       (when (and (ir1-attributep attr flushable)
			  (not (ir1-attributep attr call)))
		 (flush-dest (combination-fun node))
		 (dolist (arg (combination-args node))
		   (flush-dest arg))
		 (unlink-node node))))))
	(exit
	 (let ((value (exit-value node)))
	   (when value
	     (flush-dest value)
	     (setf (exit-value node) nil))))
	(cset
	 (let ((var (set-var node)))
	   (when (and (lambda-var-p var)
		      (null (leaf-refs var)))
	     (flush-dest (set-value node))
	     (setf (basic-var-sets var)
		   (delete node (basic-var-sets var)))
	     (unlink-node node)))))))

  (setf (block-flush-p block) nil)
  (undefined-value))

