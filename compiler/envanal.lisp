;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    The environment analysis phase for the compiler.  This phase annotates
;;; IR1 with a hierarchy environment structures, determining the environment
;;; that each Lambda allocates its variables and finding what values are closed
;;; over by each environment.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Environment-Analyze  --  Interface
;;;
;;;    Do environment analysis on the code in Component.  This involves various
;;; things:
;;;  1] Make an Environment structure for each non-let lambda, assigning the
;;;     lambda-environment for all lambdas.
;;;  2] Find all values that need to be closed over by each environment.
;;;  3] Find any unreferenced variables in the lambdas in the component.
;;;  4] Scan the blocks in the component closing over non-local-exit
;;;     continuations.
;;;
(defun environment-analyze (component)
  (declare (type component component))
  (assert (not (component-new-functions component)))
  (dolist (fun (component-lambdas component))
    (let ((res (make-environment :function fun)))
      (setf (lambda-environment fun) res)
      (dolist (lambda (lambda-lets fun))
	(setf (lambda-environment lambda) res))))
  
  (dolist (fun (component-lambdas component))
    (compute-closure fun)
    (dolist (let (lambda-lets fun))
      (compute-closure let)))
  
  (find-non-local-exits component)
  (find-cleanup-points component)
  (tail-annotate component)
  (undefined-value))


;;; Compute-Closure  --  Internal
;;;
;;;    Find any variables in Fun with references outside of the home
;;; environment and close over them.  If a closed over variable is set, then we
;;; set the Indirect flag so that we will know the closed over value is really
;;; a pointer to the value cell.  We also warn about unreferenced variables
;;; here, just because it's a convenient place to do it.
;;;
(defun compute-closure (fun)
  (declare (type clambda fun))
  (let ((env (lambda-environment fun)))
    (dolist (var (lambda-vars fun))
      (unless (or (leaf-ever-used var)
		  (lambda-var-ignorep var))
	(let ((*compiler-error-context* (lambda-bind fun)))
	  (compiler-warning "Variable ~S defined but never used."
			    (leaf-name var))))
      
      (dolist (ref (leaf-refs var))
	(let ((ref-env (node-environment ref)))
	  (unless (eq ref-env env)
	    (when (lambda-var-sets var)
	      (setf (lambda-var-indirect var) t))
	    (close-over var ref-env env))))
      
      (dolist (set (basic-var-sets var))
	(let ((set-env (node-environment set)))
	  (unless (eq set-env env)
	    (setf (lambda-var-indirect var) t)
	    (close-over var set-env env))))))
  
  (undefined-value))


;;; Close-Over  --  Internal
;;;
;;;    Make sure that Thing is closed over in Ref-Env and in all environments
;;; for the functions that reference Ref-Env's function (not just calls.)
;;; Home-Env is Thing's home environment.  When we reach the home environment,
;;; we stop propagating the closure.
;;;
(defun close-over (thing ref-env home-env)
  (declare (type environment ref-env home-env))
  (cond ((eq ref-env home-env))
	((member thing (environment-closure ref-env)))
	(t
	 (push thing (environment-closure ref-env))
	 (dolist (call (leaf-refs (environment-function ref-env)))
	   (close-over thing (node-environment call) home-env))))
  (undefined-value))


;;; Find-NLX-Cleanup  --  Internal
;;;
;;;    Given an Exit node, return the associated cleanup.  We do this by
;;; scanning up the cleanups from the ending cleanup of the Entry's block,
;;; looking for an :Entry cleanup whose mess-up is Entry.
;;;
;;; If the previous cleanup was a :Catch or :Unwind-Protect, then we return the
;;; previous cleanup instead, since we want to return the catch or UWP cleanup
;;; when that is what the exit really represents.  This assumes that the :Catch
;;; or :Unwind-Protect cleanup is always nested immediately inside the
;;; corresponding Entry cleanup, and that the catch or UWP mess-up is always
;;; converted in the same block as the Entry.
;;;
(defun find-nlx-cleanup (exit)
  (declare (type exit exit))
  (let ((entry (exit-entry exit)))
    (let ((cleanup (find-enclosing-cleanup
		    (block-end-cleanup (node-block entry))))
	  (return-prev nil))
      (loop
	(ecase (cleanup-kind cleanup)
	  (:special-bind
	   (assert (not return-prev)))
	  (:entry
	   (when (eq (continuation-use (cleanup-start cleanup)) entry)
	     (return (or return-prev cleanup)))
	   (setq return-prev nil))
	  ((:catch :unwind-protect)
	   (setq return-prev cleanup)))
	(setq cleanup (find-enclosing-cleanup (cleanup-enclosing cleanup)))))))


;;; Insert-NLX-Entry-Stub  --  Internal
;;;
;;;    Insert the entry stub before the original exit target, and add a new
;;; entry to the Environment-Nlx-Info.  The %NLX-Entry call in the stub is
;;; passed the NLX-Info as an argument so that the back end knows what entry is
;;; being done.
;;;
;;; The link from the Exit block to the entry stub is changed to be a lonk to
;;; the component head.  This leaves the entry stub reachable, but makes the
;;; flow graph less confusing to flow analysis.
;;;
;;; The ending cleanup of the entry stub is set to the enclosing cleanup for
;;; the entry's cleanup, since this represents the dynamic state that was saved
;;; at mess-up point.  It may be that additional local cleanups need to be done
;;; before actually transferring control to the destination.
;;;
(defun insert-nlx-entry-stub (exit env)
  (declare (type environment env) (type exit exit))
  (let* ((exit-block (node-block exit))
	 (next-block (first (block-succ exit-block)))
	 (cleanup (find-nlx-cleanup exit))
	 (info (make-nlx-info :cleanup cleanup
			      :continuation (node-cont exit)))
	 (new-block (insert-cleanup-code exit-block next-block
					 (exit-entry exit)
					 `(%nlx-entry ',info))))
    (unlink-blocks exit-block new-block)
    (link-blocks (component-head (block-component new-block)) new-block)
    
    (setf (nlx-info-target info) new-block)
    (push info (environment-nlx-info env))
    (push info (cleanup-nlx-info cleanup))
    (setf (block-end-cleanup new-block) (cleanup-enclosing cleanup)))

  (undefined-value))


;;; Note-Non-Local-Exit  --  Internal
;;;
;;;    Do stuff necessary to represent a non-local exit from the node Exit into
;;; Env.  This is called for each non-local exit node, of which there may be
;;; several per exit continuation.  This is what we do:
;;; -- If there isn't any NLX-Info entry in the environment, make an entry
;;;    stub, otherwise just unlink the exit block from its successor. 
;;; -- Close over the NLX-Info in the exit environment.
;;; -- If the exit is from an :Escape function, then substitute a constant
;;;    reference to NLX-Info structure for the escape function reference.  This
;;;    will cause the escape function to be deleted (although not removed from
;;;    the DFO.)  The escape function is no longer needed, and we don't want to
;;;    emit code for it.  We then also change the %NLX-ENTRY call to use
;;;    the NLX continuation so that there will be a use to represent the NLX
;;;    use.
;;;
(defun note-non-local-exit (env exit)
  (declare (type environment env) (type exit exit))
  (let ((entry (exit-entry exit))
	(cont (node-cont exit))
	(exit-fun (lambda-home (block-lambda (node-block exit)))))

    (if (find-nlx-info entry cont)
	(let ((block (node-block exit)))
	  (assert (= (length (block-succ block)) 1))
	  (unlink-blocks block (first (block-succ block))))
	(insert-nlx-entry-stub exit env))

    (let ((info (find-nlx-info entry cont)))
      (assert info)
      (close-over info (node-environment exit) env)
      (when (eq (functional-kind exit-fun) :escape)
	(substitute-leaf (find-constant info) exit-fun)
	(let ((node (block-last (nlx-info-target info))))
	  (delete-continuation-use node)
	  (add-continuation-use node (nlx-info-continuation info))))))

  (undefined-value))


;;; Find-Non-Local-Exits  --  Internal
;;;
;;;    Iterate over the blocks in Component, calling Note-Non-Local-Exit when
;;; we find a block that ends in a non-local Exit node.  We also ensure that
;;; all Exit nodes are either non-local or degenerate by calling
;;; IR1-Optimize-Exit on local exits.  This makes life simpler for later
;;; phases.
;;;
(defun find-non-local-exits (component)
  (declare (type component component))
  (do-blocks (block component)
    (let ((last (block-last block)))
      (when (exit-p last)
	(let ((target-env (lambda-environment
			   (block-lambda (first (block-succ block)))))) 
	    (if (eq (node-environment last) target-env)
		(unless *converting-for-interpreter*
		  (maybe-delete-exit last))
		(note-non-local-exit target-env last))))))

  (undefined-value))


;;; Emit-Cleanups  --  Internal
;;;
;;;    Zoom up the Cleanup-Enclosing thread until we hit Cleanup1, accumulating
;;; cleanup code as we go.  When we are done, convert the cleanup code in an
;;; implicit MV-Prog1.  We have to force local call analysis of new references
;;; to Unwind-Protect cleanup functions.
;;;
;;;    If we don't actually have to do anything, then we don't insert any
;;; cleanup code.  In this case, we set Block1's End-Cleanup to be the
;;; Start-Cleanup for block2 to indicate that no cleanup is necessary.
;;;
(defun emit-cleanups (block1 block2)
  (declare (type cblock block1 block2))
  (collect ((code)
	    (reanalyze-funs))
    (let ((cleanup2 (find-enclosing-cleanup (block-start-cleanup block2))))
      (do ((cleanup (find-enclosing-cleanup (block-end-cleanup block1))
		    (find-enclosing-cleanup (cleanup-enclosing cleanup))))
	  ((eq cleanup cleanup2))
	(let* ((node (continuation-use (cleanup-start cleanup)))
	       (args (when (basic-combination-p node)
		       (basic-combination-args node))))
	  (ecase (cleanup-kind cleanup)
	    (:special-bind
	     (code `(%special-unbind ',(continuation-value (first args)))))
	    (:catch
	     (code `(%catch-breakup)))
	    (:unwind-protect
	     (code `(%unwind-protect-breakup))
	     (let ((fun (ref-leaf (continuation-use (second args)))))
	       (reanalyze-funs fun)
	       (code `(%funcall ,fun))))
	    (:entry
	     (dolist (nlx (cleanup-nlx-info cleanup))
	       (code `(%lexical-exit-breakup ',nlx)))))))

      (cond ((code)
	     (let ((block (insert-cleanup-code block1 block2
					       (block-last block1)
					       `(progn ,@(code)))))
	       (setf (block-end-cleanup block) cleanup2))
	     (dolist (fun (reanalyze-funs))
	       (local-call-analyze-1 fun)))
	    (t
	     (setf (block-end-cleanup block1) cleanup2)))))

  (undefined-value))


;;; Find-Cleanup-Points  --  Internal
;;;
;;;    Loop over the blocks in component, calling Emit-Cleanups when we see a
;;; successor in the same environment with a different cleanup.  
;;;
(defun find-cleanup-points (component)
  (declare (type component component))
  (do-blocks (block1 component)
    (let ((env1 (lambda-environment (block-lambda block1)))
	  (cleanup1 (find-enclosing-cleanup (block-end-cleanup block1))))
      (dolist (block2 (block-succ block1))
	(let ((fun2 (block-lambda block2)))
	  (when fun2
	    (let ((env2 (lambda-environment fun2)))
	      (when (and (eq env2 env1)
			 (not (eq (find-enclosing-cleanup
				   (block-start-cleanup block2))
				  cleanup1)))
		(emit-cleanups block1 block2))))))))
  (undefined-value))


;;; Tail-Annotate  --  Internal
;;;
;;;    Mark all tail-recursive uses of function result continuations with the
;;; corresponding tail-set.  Nodes whose type is NIL (i.e. don't return) such
;;; as calls to ERROR are never annotated as tail, so as to preserve debugging
;;; information.
;;;
(defun tail-annotate (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let ((ret (lambda-return fun)))
      (when ret
	(let ((result (return-result ret))
	      (tails (lambda-tail-set fun)))
	  (do-uses (use result)
	    (when (and (immediately-used-p result use)
		       (not (eq (node-derived-type use) *empty-type*)))
	      (setf (node-tail-p use) tails)))))))
  (undefined-value))
