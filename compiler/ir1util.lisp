;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains random utilities used for manipulating the IR1
;;; representation.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Cleanup hackery:

;;; Find-Enclosing-Cleanup  --  Interface
;;;
;;;    Chain up the Lambda-Cleanup thread until we find a Cleanup or null.
;;;
(defun find-enclosing-cleanup (thing)
  (declare (type (or cleanup clambda null) thing))
  (etypecase thing
    ((or cleanup null) thing)
    (clambda (find-enclosing-cleanup (lambda-cleanup thing)))))


;;; Insert-Cleanup-Code  --  Interface
;;;
;;;    Convert the Form in a block inserted between Block1 and Block2 as an
;;; implicit MV-Prog1.  The inserted block is returned.  Node is used for IR1
;;; context when converting the form.  Note that the block is not assigned a
;;; number, and is linked into the DFO at the beginning.  We indicate that we
;;; have trashed the DFO by setting Component-Reanalyze.
;;;
(defun insert-cleanup-code (block1 block2 node form)
  (declare (type cblock block1 block2) (type node node))
  (with-ir1-environment node
    (setf (component-reanalyze *current-component*) t)
    (let* ((start (make-continuation))
	   (block (continuation-starts-block start))
	   (cont (make-continuation)))
      (change-block-successor block1 block2 block)
      (link-blocks block block2)
      (ir1-convert start cont form)
      (setf (block-last block) (continuation-use cont))
      block)))
  

;;;; Continuation use hacking:

;;; Find-Uses  --  Interface
;;;
;;;    Return a list of all the nodes which use Cont.
;;;
(proclaim '(function find-uses (continuation) list))
(defun find-uses (cont)
  (ecase (continuation-kind cont)
    ((:block-start :deleted-block-start)
     (block-start-uses (continuation-block cont)))
    (:inside-block (list (continuation-use cont)))
    (:unused nil)))

      
;;; Delete-Continuation-Use  --  Interface
;;;
;;;    Update continuation use information so that Node is no longer a use of
;;; its Cont.  If the old continuation doesn't start its block, then we don't
;;; update the Block-Start-Uses, since it will be deleted when we are done.
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-CONTINUATION to inform IR1 optimization that something has
;;; changed.
;;;
(proclaim '(function delete-continuation-use (node) void))
(defun delete-continuation-use (node)
  (let* ((cont (node-cont node))
	 (block (continuation-block cont)))
    (ecase (continuation-kind cont)
      (:deleted)
      ((:block-start :deleted-block-start)
       (let ((uses (delete node (block-start-uses block))))
	 (setf (block-start-uses block) uses)
	 (setf (continuation-use cont)
	       (if (cdr uses) nil (car uses)))))
      (:inside-block
       (setf (continuation-kind cont) :unused)
       (setf (continuation-block cont) nil)
       (setf (continuation-use cont) nil)
       (setf (continuation-next cont) nil)))
    (setf (node-cont node) nil)))


;;; Add-Continuation-Use  --  Interface
;;;
;;;    Update continuation use information so that Node uses Cont.  If Cont is
;;; :Unused, then we set its block to Node's Node-Block (which must be set.)
;;;
;;; Note: if you call this function, you may have to do a
;;; REOPTIMIZE-CONTINUATION to inform IR1 optimization that something has
;;; changed.
;;;
(proclaim '(function add-continuation-use (node continuation) void))
(defun add-continuation-use (node cont)
  (assert (not (node-cont node)))
  (let ((block (continuation-block cont)))
    (ecase (continuation-kind cont)
      (:deleted)
      (:unused
       (assert (not block))
       (let ((block (node-block node)))
	 (assert block)
	 (setf (continuation-block cont) block))
       (setf (continuation-kind cont) :inside-block)
       (setf (continuation-use cont) node))
      ((:block-start :deleted-block-start)
       (let ((uses (cons node (block-start-uses block))))
	 (setf (block-start-uses block) uses)
	 (setf (continuation-use cont)
	       (if (cdr uses) nil (car uses)))))))
  (setf (node-cont node) cont))


;;; Immediately-Used-P  --  Interface
;;;
;;;    Return true if Cont is the Node-Cont for Node and Cont is transferred to
;;; immediately after the evaluation of Node.
;;;
(defun immediately-used-p (cont node)
  (declare (type continuation cont) (type node node))
  (and (eq (node-cont node) cont)
       (not (eq (continuation-kind cont) :deleted))
       (let ((cblock (continuation-block cont))
	     (nblock (node-block node)))
	 (or (eq cblock nblock)
	     (let ((succ (block-succ nblock)))
	       (and (= (length succ) 1)
		    (eq (first succ) cblock)))))))


;;;; Continuation substitution:

;;; Substitute-Continuation  --  Interface
;;;
;;;    In Old's Dest, replace Old with New.  New's Dest must initially be NIL.
;;; When we are done, we call Flush-Dest on Old to clear its Dest and to note
;;; potential optimization opportunities.
;;;
(defun substitute-continuation (new old)
  (declare (type continuation old new))
  (assert (not (continuation-dest new)))
  (let ((dest (continuation-dest old)))
    (etypecase dest
      ((or ref bind))
      (cif (setf (if-test dest) new))
      (cset (setf (set-value dest) new))
      (creturn (setf (return-result dest) new))
      (exit (setf (exit-value dest) new))
      (basic-combination
       (if (eq old (basic-combination-fun dest))
	   (setf (basic-combination-fun dest) new)
	   (setf (basic-combination-args dest)
		 (nsubst new old (basic-combination-args dest))))))

    (flush-dest old)
    (setf (continuation-dest new) dest))
  (undefined-value))


;;; Ensure-Block-Start  --  Interface
;;;
;;;    Ensure that Cont is the start of a block (or deleted) so that the use
;;; set can be freely manipulated.
;;; -- If the continuation is :Unused or is :Inside-Block and the Cont of Last
;;;    in its block, then we make it the start of a new deleted block.
;;; -- If the continuation is :Inside-Block inside a block, then we split the
;;;    block using Node-Ends-Block, which makes the continuation be a
;;;    :Block-Start.
;;;
(defun ensure-block-start (cont)
  (declare (type continuation cont))
  (let ((kind (continuation-kind cont)))
    (ecase kind
      ((:deleted :block-start :deleted-block-start))
      ((:unused :inside-block)
       (let ((block (continuation-block cont)))
	 (cond ((or (eq kind :unused)
		    (eq (node-cont (block-last block)) cont))
		(setf (continuation-block cont)
		      (make-block-key :start cont :lambda nil
				      :start-cleanup nil :end-cleanup nil
				      :component nil))
		(setf (continuation-kind cont) :deleted-block-start))
	       (t
		(node-ends-block (continuation-use cont))))))))
  (undefined-value))


;;; Substitute-Continuation-Uses  --  Interface
;;;
;;;    Replace all uses of Old with uses of New, where New has an arbitary
;;; number of uses.  If a use is an Exit, then we also substitute New for Old
;;; in the Entry's Exits to maintain consistency between the two.
;;;
;;;    If New will end up with more than one use, then we must arrange for it
;;; to start a block if it doesn't already.
;;;
(defun substitute-continuation-uses (new old)
  (declare (type continuation old new))
  (unless (and (eq (continuation-kind new) :unused)
	       (eq (continuation-kind old) :inside-block))
    (ensure-block-start new))
  
  (do-uses (node old)
    (when (exit-p node)
      (let ((entry (exit-entry node)))
	(when entry
	  (setf (entry-exits entry)
		(nsubst new old (entry-exits entry))))))
    (delete-continuation-use node)
    (add-continuation-use node new))

  (reoptimize-continuation new)
  (undefined-value))

#|
;;; Substitute-Node-Cont  --  Interface
;;;
;;;    Replace Old's single use with a use of New.  This is used in contexts
;;; where we know that New has no use and Old has a single use.
;;;
(defun substitute-node-cont (new old)
  (declare (type continuation new old))
  (assert (member (continuation-kind new) '(:block-start :unused)))
  (assert (eq (continuation-kind old) :inside-block))

  (let ((use (continuation-use old)))
    (delete-continuation-use use)
    (add-continuation-use use new))

  (undefined-value))
|#


;;; NODE-BLOCK, NODE-ENVIRONMENT, NODE-TLF-NUMBER  --  Interface
;;;
;;;    Shorthand for common idiom.
;;;
(proclaim '(inline node-block node-environment node-tlf-number))
(defun node-block (node)
  (declare (type node node))
  (the cblock (continuation-block (node-prev node))))
;;;
(defun node-environment (node)
  (declare (type node node))
  (the environment (lambda-environment (block-lambda (node-block node)))))
;;;
(defun node-tlf-number (node)
  (declare (type node node))
  (car (last (node-source-path node))))


;;;; Flow/DFO/Component hackery:

;;; Link-Blocks, Unlink-Blocks  --  Interface
;;;
;;;    Join or separate Block1 and Block2.
;;;
(proclaim '(ftype (function (block block) void) link-blocks unlink-blocks))
(defun link-blocks (block1 block2)
  (assert (not (member block2 (block-succ block1))))
  (push block2 (block-succ block1))
  (push block1 (block-pred block2)))
;;;
(defun unlink-blocks (block1 block2)
  (assert (member block2 (block-succ block1)))
  (setf (block-succ block1)
	(delete block2 (block-succ block1)))
  (setf (block-pred block2)
	(delete block1 (block-pred block2))))


;;; Change-Block-Successor  --  Internal
;;;
;;;    Swing the succ/pred link between Block and Old to be between Block and
;;; New.  If Block ends in an IF, then we have to fix up the
;;; consequent/alternative blocks to point to New.
;;;
(defun change-block-successor (block old new)
  (declare (type cblock new old block))
  (unlink-blocks block old)
  (unless (member new (block-succ block))
    (link-blocks block new))
  
  (let ((last (block-last block)))
    (when (if-p last)
      (macrolet ((frob (slot)
		   `(when (eq (,slot last) old)
		      (setf (,slot last) new))))
	(frob if-consequent)
	(frob if-alternative))))
  
  (undefined-value))


;;; Remove-From-DFO  --  Interface
;;;
;;;    Unlink a block from the next/prev chain.  We also null out the
;;; Component.
;;;
(proclaim '(function remove-from-dfo (cblock) void))
(defun remove-from-dfo (block)
  (let ((next (block-next block))
	(prev (block-prev block)))
    (setf (block-component block) nil)
    (setf (block-next prev) next)
    (setf (block-prev next) prev)))

;;; Add-To-DFO  --  Interface
;;;
;;;    Add Block to the next/prev chain following After.  We also set the
;;; Component to be the same as for After.
;;;
(proclaim '(function add-to-dfo (block block) void))
(defun add-to-dfo (block after)
  (let ((next (block-next after)))
    (setf (block-component block) (block-component after))
    (setf (block-next after) block)
    (setf (block-prev block) after)
    (setf (block-next block) next)
    (setf (block-prev next) block)))


;;; Clear-Flags  --  Interface
;;;
;;;    Set the Flag for all the blocks in Component to NIL, except for the head
;;; and tail which are set to T.
;;;
(proclaim '(function clear-flags (component) void))
(defun clear-flags (component)
  (let ((head (component-head component))
	(tail (component-tail component)))
    (setf (block-flag head) t)
    (setf (block-flag tail) t)
    (do-blocks (block component)
      (setf (block-flag block) nil))))


;;; Make-Empty-Component  --  Interface
;;;
;;;    Make a component with no blocks in it.  The Block-Flag is initially true
;;; in the head and tail blocks.
;;;
(proclaim '(function make-empty-component () component))
(defun make-empty-component ()
  (let* ((head (make-block-key :start nil :lambda nil :start-cleanup nil
			       :end-cleanup nil :component nil))
	 (tail (make-block-key :start nil :lambda nil :start-cleanup nil
			       :end-cleanup nil :component nil))
	 (res (make-component :head head  :tail tail)))
    (setf (block-flag head) t)
    (setf (block-flag tail) t)
    (setf (block-component head) res)
    (setf (block-component tail) res)
    (setf (block-next head) tail)
    (setf (block-prev tail) head)
    res))


;;; Node-Ends-Block  --  Interface
;;;
;;;    Makes Node the Last node in its block, splitting the block if necessary.
;;;
;;;    If the mess-up for one of Block's End-Cleanups is moved into the new
;;; block, then we must adjust the end/start cleanups of the new and old blocks
;;; to reflect the movement of the mess-up.  If any of the old end cleanups
;;; were in the new block, then we scan up from that cleanup trying to find one
;;; that isn't.  When we do, that becomes the new start/end cleanup of the
;;; old/new block.  We set the start/end as a pair, since we don't want anyone
;;; to think that a cleanup is necessary.
;;;
(defun node-ends-block (node)
  (declare (type node node))
  (let* ((block (node-block node))
	 (start (node-cont node))
	 (last (block-last block))
	 (last-cont (node-cont last)))
    (unless (eq last node)
      (assert (eq (continuation-kind start) :inside-block))
      (let* ((succ (block-succ block))
	     (cleanup (block-end-cleanup block))
	     (new-block
	      (make-block-key :start start
			      :lambda (block-lambda block)
			      :start-cleanup cleanup
			      :end-cleanup cleanup
			      :component (block-component block)
			      :start-uses (list (continuation-use start))
			      :succ succ :last last)))
	(setf (continuation-kind start) :block-start)
	(dolist (b succ)
	  (setf (block-pred b)
		(cons new-block (remove block (block-pred b)))))
	(setf (block-succ block) ())
	(setf (block-last block) node)
	(link-blocks block new-block)
	(add-to-dfo new-block block)
	
	(do ((cont start (node-cont (continuation-next cont))))
	    ((eq cont last-cont)
	     (when (eq (continuation-kind last-cont) :inside-block)
	       (setf (continuation-block last-cont) new-block)))
	  (setf (continuation-block cont) new-block))

	(let ((start-cleanup (block-start-cleanup block)))
	  (do ((cup (find-enclosing-cleanup cleanup)
		    (find-enclosing-cleanup (cleanup-enclosing cup))))
	      ((null cup))
	    (when (eq (node-block (continuation-use (cleanup-start cup)))
		      new-block)
	      (do ((cup (find-enclosing-cleanup (cleanup-enclosing cup))
			(find-enclosing-cleanup (cleanup-enclosing cup))))
		  ((null cup)
		   (setf (block-end-cleanup block) start-cleanup)
		   (setf (block-start-cleanup new-block) start-cleanup))
		(let ((cb (node-block (continuation-use (cleanup-start cup)))))
		  (unless (eq cb new-block)
		    (setf (block-end-cleanup block) cup)
		    (setf (block-start-cleanup new-block) cup)
		    (return))))
	      (return))))

	(setf (block-type-asserted block) t)
	(setf (block-test-modified block) t))))

  (undefined-value))


;;;; Deleting stuff:

;;; Delete-Lambda-Var  --  Internal
;;;
;;;    Deal with deleting the last (read) reference to a lambda-var.  We
;;; iterate over all local calls flushing the corresponding argument, allowing
;;; the computation of the argument to be deleted.
;;;
;;;    The lambda-var may still have some sets, but this doesn't cause too much
;;; difficulty, since we can efficiently implement write-only variables.  We
;;; iterate over the sets, marking their blocks for dead code flushing, since
;;; we can delete sets whose value is unused.
;;;
(defun delete-lambda-var (leaf)
  (declare (type lambda-var leaf))
  (let* ((fun (lambda-var-home leaf))
	 (n (position leaf (lambda-vars fun))))
    (dolist (ref (leaf-refs fun))
      (let* ((cont (node-cont ref))
	     (dest (continuation-dest cont)))
	(when (and (combination-p dest)
		   (eq (basic-combination-fun dest) cont)
		   (eq (basic-combination-kind dest) :local))
	  (let ((args (basic-combination-args dest)))
	    (flush-dest (elt args n))
	    (setf (elt args n) nil))))))

  (dolist (set (lambda-var-sets leaf))
    (setf (block-flush-p (node-block set)) t))

  (undefined-value))


;;; Delete-Lambda  --  Internal
;;;
;;;    Deal with deleting the last reference to a lambda.  Since there is only
;;; one way into a lambda, deleting the last reference to a lambda ensures that
;;; there is no way to reach any of the code in it.  So we just set the
;;; Functional-Kind for Fun and its Lets to :Deleted, causing IR1 optimization
;;; to delete blocks in that lambda.
;;;
;;;    If the function isn't a Let, we unlink the function head and tail from
;;; the component head and tail to indicate that the code is unreachable.  We
;;; also delete the function Component-Lambdas (it won't be there before local
;;; call analysis, but no matter.)
;;;
;;;    If the lambda is an XEP, then we null out the Entry-Function in its
;;; Entry-Function so that people will know that it is not an entry point
;;; anymore.
;;;
(defun delete-lambda (leaf)
  (declare (type clambda leaf))
  (let ((kind (functional-kind leaf)))
    (assert (not (member kind '(:deleted :optional :top-level))))
    (setf (functional-kind leaf) :deleted)
    (dolist (let (lambda-lets leaf))
      (setf (functional-kind let) :deleted))

    (if (or (eq kind :let) (eq kind :mv-let))
	(let ((home (lambda-home leaf)))
	  (setf (lambda-lets home) (delete leaf (lambda-lets home))))
	(let* ((bind-block (node-block (lambda-bind leaf)))
	       (component (block-component bind-block))
	       (return (lambda-return leaf)))
	  (unlink-blocks (component-head component) bind-block)
	  (when return
	    (unlink-blocks (node-block return) (component-tail component)))
	  (setf (component-lambdas component)
		(delete leaf (component-lambdas component)))))

    (when (eq kind :external)
      (let ((fun (functional-entry-function leaf)))
	(setf (functional-entry-function fun) nil)
	(when (optional-dispatch-p fun)
	  (delete-optional-dispatch fun)))))

  (undefined-value))


;;; Delete-Optional-Dispatch  --  Internal
;;;
;;;    Deal with deleting the last reference to an Optional-Dispatch.  We have
;;; to be a bit more careful than with lambdas, since Delete-Ref is used both
;;; before and after local call analysis.  Afterward, all references to
;;; still-existing optional-dispatches have been moved to the XEP, leaving it
;;; with no references at all.  So we look at the XEP to see if an
;;; optional-dispatch is still really being used.  But before local call
;;; analysis, there are no XEPs, and all references are direct.
;;;
;;;    When we do delete the optional-dispatch, we grovel all of its
;;; entry-points, making them be normal lambdas, and then deleting the ones
;;; with no references.  This deletes any e-p lambdas that were either never
;;; referenced, or couldn't be deleted when the last deference was deleted (due
;;; to their :Optional kind.)
;;;
;;; Note that the last optional ep may alias the main entry, so when we process
;;; the main entry, its kind may have been changed to NIL or even converted to
;;; a let.
;;;
(defun delete-optional-dispatch (leaf)
  (declare (type optional-dispatch leaf))
  (let ((entry (functional-entry-function leaf)))
    (unless (and entry (leaf-refs entry))
      (assert (or (not entry) (eq (functional-kind entry) :deleted)))
      (setf (functional-kind leaf) :deleted)

      (flet ((frob (fun)
	       (unless (eq (functional-kind fun) :deleted)
		 (assert (eq (functional-kind fun) :optional))
		 (setf (functional-kind fun) nil)
		 (let ((refs (leaf-refs fun)))
		   (cond ((null refs)
			  (delete-lambda fun))
			 ((null (rest refs))
			  (maybe-let-convert fun)))))))
	
	(dolist (ep (optional-dispatch-entry-points leaf))
	  (frob ep))
	(when (optional-dispatch-more-entry leaf)
	  (frob (optional-dispatch-more-entry leaf)))
	(let ((main (optional-dispatch-main-entry leaf)))
	  (when (eq (functional-kind main) :optional)
	    (frob main))))))

  (undefined-value))


;;; Delete-Ref  --  Interface
;;;
;;;    Do stuff to delete the semantic attachments of a Ref node.  When this
;;; leaves zero or one reference, we do a type dispatch off of the leaf to
;;; determine if a special action is appropriate.
;;;
(defun delete-ref (ref)
  (declare (type ref ref))
  (let* ((leaf (ref-leaf ref))
	 (refs (delete ref (leaf-refs leaf))))
    (setf (leaf-refs leaf) refs)
    
    (cond ((null refs)
	   (typecase leaf
	     (lambda-var (delete-lambda-var leaf))
	     (clambda
	      (ecase (functional-kind leaf)
		((nil :external :let :mv-let :escape :cleanup)
		 (delete-lambda leaf))
		((:deleted :optional))))
	     (optional-dispatch
	      (unless (eq (functional-kind leaf) :deleted)
		(delete-optional-dispatch leaf)))))
	  ((null (rest refs))
	   (typecase leaf
	     (clambda (maybe-let-convert leaf))))))

  (undefined-value))


;;; Delete-Return  --  Interface
;;;
;;;    Do stuff to indicate that the return node Node is being deleted.
;;;
(defun delete-return (node)
  (declare (type creturn node))
  (let* ((fun (return-lambda node))
	 (tail-set (lambda-tail-set fun)))
    (assert (lambda-return fun))
    (setf (tail-set-functions tail-set)
	  (delete fun (tail-set-functions tail-set)))
    (setf (lambda-tail-set fun) nil)
    (setf (lambda-return fun) nil))
  (undefined-value))


;;; Flush-Dest  --  Interface
;;;
;;;    This function is called by people who delete nodes; it provides a way to
;;; indicate that the value of a continuation is no longer used.  We null out
;;; the Continuation-Dest, set Block-Flush-P in the blocks containing uses of
;;; Cont and set Component-Reoptimize.
;;;
;;;    If the continuation is :Deleted, then we don't do anything, since all
;;; semantics have already been flushed.  If the continuation is a
;;; :Deleted-Block-Start, then we delete the continuation, since its control
;;; semantics have already been deleted.  Deleting the continuation causes its
;;; uses to be reoptimized.  If the Prev of the use is deleted, then we blow
;;; off reoptimization.
;;;
(defun flush-dest (cont)
  (declare (type continuation cont))
  
  (ecase (continuation-kind cont)
    (:deleted)
    (:deleted-block-start
     (assert (continuation-dest cont))
     (setf (continuation-dest cont) nil)
     (delete-continuation cont))
    ((:inside-block :block-start)
     (assert (continuation-dest cont))
     (setf (continuation-dest cont) nil)
     (setf (component-reoptimize (block-component (continuation-block cont)))
	   t)
     (do-uses (use cont)
       (let ((prev (node-prev use)))
	 (unless (eq (continuation-kind prev) :deleted)
	   (let ((block (continuation-block prev)))
	     (setf (block-flush-p block) t)
	     (setf (block-type-asserted block) t)))))))

  (setf (continuation-%type-check cont) nil)
  
  (undefined-value))


;;; MARK-FOR-DELETION  --  Internal
;;;
;;;    Do a graph walk backward from Block, marking all predecessor blocks with
;;; the DELETE-P flag.
;;;
(defun mark-for-deletion (block)
  (declare (type cblock block))
  (unless (block-delete-p block)
    (setf (block-delete-p block) t)
    (dolist (pred (block-pred block))
      (mark-for-deletion pred)))
  (undefined-value))


;;; DELETE-CONTINUATION  --  Interface
;;;
;;;    Delete Cont, eliminating both control and value semantics.  We set
;;; FLUSH-P and COMPONENT-REOPTIMIZE similarly to in FLUSH-DEST.  Here we must
;;; get the component from the use block, since the continuation may be a
;;; :DELETED-BLOCK-START.
;;;
;;;    If Cont has DEST, then it must be the case that the DEST is unreachable,
;;; since we can't compute the value desired.  In this case, we call
;;; MARK-FOR-DELETION to cause the DEST block and its predecessors to tell
;;; people to ignore them, and to cause them to be deleted eventually.
;;;
(defun delete-continuation (cont)
  (declare (type continuation cont))
  (assert (not (eq (continuation-kind cont) :deleted)))
  
  (do-uses (use cont)
    (let ((prev (node-prev use)))
      (unless (eq (continuation-kind prev) :deleted)
	(let ((block (continuation-block prev)))
	  (setf (block-flush-p block) t)
	  (setf (block-type-asserted block) t)
	  (setf (component-reoptimize (block-component block)) t)))))

  (let ((dest (continuation-dest cont)))
    (when dest
      (let ((block (node-block dest)))
	(unless (block-delete-p block)
	  (mark-for-deletion block)))))
  
  (setf (continuation-kind cont) :deleted)
  (setf (continuation-dest cont) nil)
  (setf (continuation-next cont) nil)
  (setf (continuation-asserted-type cont) *empty-type*)
  (setf (continuation-%derived-type cont) *empty-type*)
  (setf (continuation-use cont) nil)
  (setf (continuation-block cont) nil)
  (setf (continuation-reoptimize cont) nil)
  (setf (continuation-%type-check cont) nil)
  (setf (continuation-info cont) nil)
  
  (undefined-value))


;;; Delete-Block  --  Interface
;;;
;;;    This function does what is necessary to eliminate the code in it from
;;; the IR1 representation.  This involves unlinking it from its predecessors
;;; and successors and deleting various node-specific semantic information.
;;;
;;;    We mark the Start as has having no next and remove the last node from
;;; its Cont's uses.  We also flush the DEST for all continuations whose values
;;; are received by nodes in the block.
;;;
(defun delete-block (block)
  (declare (type cblock block))
  (assert (block-component block) () "Block is already deleted.")
  (setf (block-delete-p block) t)

  (let* ((last (block-last block))
	 (cont (node-cont last)))
    (delete-continuation-use last)
    (cond ((eq (continuation-kind cont) :unused)
	   (assert (not (continuation-dest cont)))
	   (delete-continuation cont))
	  (t
	   (reoptimize-continuation cont))))

  (dolist (b (block-pred block))
    (unlink-blocks b block))
  (dolist (b (block-succ block))
    (unlink-blocks block b))

  (do-nodes (node cont block)
    (typecase node
      (ref (delete-ref node))
      (basic-combination
       (flush-dest (basic-combination-fun node))
       (dolist (arg (basic-combination-args node))
	 (when arg (flush-dest arg))))
      (cif
       (flush-dest (if-test node)))
      (bind
       (let ((lambda (bind-lambda node)))
	 (unless (eq (functional-kind lambda) :deleted)
	   (assert (member (functional-kind lambda) '(:let :mv-let)))
	   (delete-lambda lambda))))
      (exit
       (let ((value (exit-value node)))
	 (when value
	   (flush-dest value))))
      (creturn
       (flush-dest (return-result node))
       (delete-return node))
      (cset
       (flush-dest (set-value node))
       (let ((var (set-var node)))
	 (setf (basic-var-sets var)
	       (delete node (basic-var-sets var))))))

    (delete-continuation (node-prev node)))

  (remove-from-dfo block)
  (undefined-value))


;;; Unlink-Node  --  Interface
;;;
;;;    Delete a node from a block, deleting the block if there are no nodes
;;; left.  We remove the node from the uses of its CONT, but we don't deal with
;;; cleaning up any type-specific semantic attachments.  If the CONT is :UNUSED
;;; after deleting this use, then we delete CONT.  (Note :UNUSED is not the
;;; same as no uses.  A continuation will only become :UNUSED if it was
;;; :INSIDE-BLOCK before.) 
;;;
;;;    If the node is the last node, there must be exactly one successor.  We
;;; link all of our precedessors to the successor and unlink the block.  In
;;; this case, we return T, otherwise NIL.  If no nodes are left, and the block
;;; is a successor of itself, then we replace the only node with a degenerate
;;; exit node.  This provides a way to represent the bodyless infinite loop,
;;; given the prohibition on empty blocks in IR1.
;;;
(defun unlink-node (node)
  (declare (type node node))
  (let* ((cont (node-cont node))
	 (next (continuation-next cont))
	 (prev (node-prev node))
	 (block (continuation-block prev))
	 (prev-kind (continuation-kind prev))
	 (last (block-last block)))
    
    (unless (eq (continuation-kind cont) :deleted)
      (delete-continuation-use node)
      (when (eq (continuation-kind cont) :unused)
	(assert (not (continuation-dest cont)))
	(delete-continuation cont)))
    
    (setf (block-type-asserted block) t)
    (setf (block-test-modified block) t)

    (cond ((or (eq prev-kind :inside-block)
	       (and (eq prev-kind :block-start)
		    (not (eq node last))))
	   (cond ((eq node last)
		  (setf (block-last block) (continuation-use prev))
		  (setf (continuation-next prev) nil))
		 (t
		  (setf (continuation-next prev) next)
		  (setf (node-prev next) prev)))
	   (setf (node-prev node) nil)
	   nil)
	  (t
	   (assert (eq prev-kind :block-start))
	   (assert (eq node last))
	   (let* ((succ (block-succ block))
		  (next (first succ)))
	     (assert (and succ (null (cdr succ))))
	     (cond
	      ((member block succ)
	       (with-ir1-environment node
		 (let ((exit (make-exit :source (node-source node)))
		       (dummy (make-continuation)))
		   (setf (continuation-next prev) nil)
		   (prev-link exit prev)
		   (add-continuation-use exit dummy)
		   (setf (block-last block) exit)))
	       (setf (node-prev node) nil)
	       nil)
	      (t
	       (assert (eq (find-enclosing-cleanup (block-start-cleanup block))
			   (find-enclosing-cleanup (block-end-cleanup block))))
	       (unlink-blocks block next)
	       (dolist (pred (block-pred block))
		 (change-block-successor pred block next))
	       (remove-from-dfo block)
	       (cond ((continuation-dest prev)
		      (setf (continuation-next prev) nil)
		      (setf (continuation-kind prev) :deleted-block-start))
		     (t
		      (delete-continuation prev)))
	       (setf (node-prev node) nil)
	       t)))))))


;;; NODE-DELETED  --  Interface
;;;
;;;    Return true if NODE has been deleted, false if it is still a valid part
;;; of IR1.
;;;
(defun node-deleted (node)
  (declare (type node node))
  (let ((prev (node-prev node)))
    (and prev
	 (not (eq (continuation-kind prev) :deleted))
	 (let ((block (continuation-block prev)))
	   (and (block-component block)
		(not (block-delete-p block)))))))
  

;;;; Leaf hackery:

;;; Change-Ref-Leaf  --  Interface
;;;
;;;    Change the Leaf that a Ref refers to.
;;;
(defun change-ref-leaf (ref leaf)
  (declare (type ref ref) (type leaf leaf))
  (unless (eq (ref-leaf ref) leaf)
    (push ref (leaf-refs leaf))
    (delete-ref ref)
    (setf (ref-leaf ref) leaf)
    (derive-node-type ref (leaf-type leaf))
    (reoptimize-continuation (node-cont ref)))
  (undefined-value))


;;; Substitute-Leaf  --  Interface
;;;
;;;    Change all Refs for Old-Leaf to New-Leaf.
;;;
(defun substitute-leaf (new-leaf old-leaf)
  (declare (type leaf new-leaf old-leaf))
  (dolist (ref (leaf-refs old-leaf))
    (change-ref-leaf ref new-leaf))
  (undefined-value))


;;; Find-Constant  --  Interface
;;;
;;;    Return a Leaf which represents the specified constant object.  If the
;;; object is not in *constants*, then we create a new constant Leaf and
;;; enter it.
;;;
(defun find-constant (object)
  (or (gethash object *constants*)
      (setf (gethash object *constants*)
	    (make-constant :value object  :name nil
			   :type (ctype-of object)
			   :where-from :defined))))


;;;; Find-NLX-Info  --  Interface
;;;
;;;    If there is a non-local exit noted in Entry's environment that exits to
;;; Cont in that entry, then return it, otherwise return NIL.
;;;
(defun find-nlx-info (entry cont)
  (declare (type entry entry) (type continuation cont))
  (dolist (nlx (environment-nlx-info (node-environment entry)) nil)
    (let* ((cleanup (nlx-info-cleanup nlx))
	   (entry-cleanup (ecase (cleanup-kind cleanup)
			    ((:catch :unwind-protect)
			     (cleanup-enclosing cleanup))
			    (:entry cleanup))))
      (when (and (eq (nlx-info-continuation nlx) cont)
		 (eq (continuation-use (cleanup-start entry-cleanup))
		     entry))
	(return nlx)))))


;;;; Functional hackery:

;;; Main-Entry  --  Interface
;;;
;;;    If Functional is a Lambda, just return it; if it is an
;;; optional-dispatch, return the main-entry.
;;;
(proclaim '(function main-entry (functional) lambda))
(defun main-entry (functional)
  (if (lambda-p functional)
      functional
      (optional-dispatch-main-entry functional)))

;;; Looks-Like-An-MV-Bind  --  Interface
;;;
;;;    Returns true if Functional is a thing that can be treated like MV-Bind
;;; when it appears in an MV-Call.  All fixed arguments must be optional with
;;; null default and no supplied-p.  There must be a rest arg with no
;;; references.
;;;
(proclaim '(function looks-like-an-mv-bind (functional) boolean))
(defun looks-like-an-mv-bind (functional)
  (and (optional-dispatch-p functional)
       (do ((arg (optional-dispatch-arglist functional) (cdr arg)))
	   ((null arg) nil)
	 (let ((info (lambda-var-arg-info (car arg))))
	   (unless info (return nil))
	   (case (arg-info-kind info)
	     (:optional
	      (when (or (arg-info-supplied-p info) (arg-info-default info))
		(return nil)))
	     (:rest
	      (return (and (null (cdr arg)) (null (leaf-refs (car arg))))))
	     (t
	      (return nil)))))))

;;; External-Entry-Point-P  --  Interface
;;;
;;;    Return true if function is an XEP.  This is true of normal XEPs
;;; (:External kind) and top-level lambdas (:Top-Level kind.)
;;;
(defun external-entry-point-p (fun)
  (declare (type functional fun))
  (not (null (member (functional-kind fun) '(:external :top-level)))))


;;; Continuation-Function-Name  --  Interface
;;;
;;;    If Cont's only use is a global function reference, then return the
;;; referenced symbol, otherwise NIL.
;;;
(defun continuation-function-name (cont)
  (declare (type continuation cont))
  (let ((use (continuation-use cont)))
    (if (ref-p use)
	(let ((leaf (ref-leaf use)))
	  (if (and (global-var-p leaf)
		   (eq (global-var-kind leaf) :global-function))
	      (leaf-name leaf)
	      nil))
	nil)))


;;; LET-COMBINATION  --  Interface
;;;
;;;    Return the COMBINATION node that is the call to the let Fun.
;;;
(defun let-combination (fun)
  (declare (type clambda fun))
  (assert (eq (functional-kind fun) :let))
  (continuation-dest (node-cont (first (leaf-refs fun)))))


;;; LET-VAR-INITIAL-VALUE  --  Interface
;;;
;;;    Return the initial value continuation for a let variable or NIL if none.
;;;
(defun let-var-initial-value (var)
  (declare (type lambda-var var))
  (let ((fun (lambda-var-home var)))
    (elt (combination-args (let-combination fun))
	 (position var (lambda-vars fun)))))


;;; COMBINATION-LAMBDA  --  Interface
;;;
;;;    Return the LAMBDA that is called by the local Call.
;;;
(defun combination-lambda (call)
  (declare (type basic-combination call))
  (assert (eq (basic-combination-kind call) :local))
  (ref-leaf (continuation-use (basic-combination-fun call))))


;;;; Compiler error context determination:

(proclaim '(special *current-path* *current-form*))


;;; We separate the determination of compiler error contexts from the actual
;;; signalling of those errors by objectifying the error context.  This allows
;;; postponement of the determination of how (and if) to signal the error.
;;; We take care not to reference any of the IR1 so that pending potential
;;; error messages won't prevent the IR1 from being GC'd.
;;;
(defstruct (compiler-error-context
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (format stream "#<Compiler-Error-Context>"))))
  ;;
  ;; The form immediately responsible for this error (may be the result of
  ;; mecroexpansion, etc.)
  source
  ;;
  ;; The form in the original source that expanded into Source.
  original-source
  ;;
  ;; A list of prefixes of "interesting" forms that enclose original-source.
  context)

  
;;; If true, this is the node which is used as context in compiler warning
;;; messages.
;;;
(proclaim '(type (or null compiler-error-context node)
		 *compiler-error-context*))
(defvar *compiler-error-context* nil)


;;; A list of "DEFxxx" forms for which we should we should compute the source
;;; context by taking the CAR of the first arg when it is a list.
;;;
(defparameter defmumble-take-car-forms '(defstruct))


;;; Find-Original-Source  --  Internal
;;;
;;;    Given a source path, return the original source form and a description
;;; of the interesting aspects of the context in which it appeared.  The
;;; context is a list of lists, one sublist per context form.  The sublist is a
;;; list of some of the initial subforms of the context form.
;;;
;;; For now, we use the first two subforms of each interesting form.  A form is
;;; interesting if the first element is a symbol beginning with "DEF" and it is
;;; not the source form.  If there is no DEF-mumble, then we use the outermost
;;; containing form.  If the second subform is a list, then in some cases we
;;; return the car of that form rather than the whole form (i.e. don't show
;;; defstruct options, etc.)
;;;
(defun find-original-source (path)
  (declare (list path))
  (assert path)
  (let* ((rpath (reverse (rest path)))
	 (root (find-source-root (first rpath) *source-info*)))
    (collect ((context))
      (let ((form root)
	    (current (rest rpath)))
	(loop
	  (let ((head (first form)))
	    (when (symbolp head)
	      (let ((name (symbol-name head)))
		(when (and (>= (length name) 3) (string= name "DEF" :end1 3))
		  (if (>= (length form) 2)
		      (let ((next (second form)))
			(context
			 (list head
			       (if (and (listp next)
					(member head
						defmumble-take-car-forms))
				   (car next)
				   next))))
		      (context (list head)))))))
	  
	  (when (null current) (return))
	  (setq form (nth (pop current) form)))
	
	(cond ((context)
	       (values form (context)))
	      ((and path root)
	       (if (listp root)
		   (values form (list (subseq root 0 (min 2 (length root)))))
		   (values form ())))
	      (t
	       (values '(unable to locate source)
		       '((some strange place)))))))))


;;; FIND-ERROR-CONTEXT  --  Interface
;;;
;;;    Return a COMPILER-ERROR-CONTEXT structure describing the current error
;;; context, or NIL if we can't figure anything out.
;;;
(defun find-error-context ()
  (let ((context *compiler-error-context*))
    (if (compiler-error-context-p context)
	context
	(let ((source (cond (*current-form*)
			    (context (node-source context))
			    (t nil)))
	      (path (if context (node-source-path context) *current-path*)))
	  (when (and *source-info* path)
	    (multiple-value-bind (form src-context)
				 (find-original-source path)
	      (make-compiler-error-context
	       :source source
	       :original-source form
	       :context src-context)))))))


;;;; Printing error messages:

;;; A function that is called to unwind out of Compiler-Error.
;;;
(proclaim '(type (function () nil) *compiler-error-bailout*))
(defvar *compiler-error-bailout*
  #'(lambda () (error "Compiler-Error with no bailout.")))

;;; We bind print level and length when printing out messages so that we don't
;;; dump huge amounts of garbage.
;;;
(proclaim '(type (or unsigned-byte null) *error-print-level* *error-print-length*))
(defvar *error-print-level* 3
  "The value for *Print-Level* when printing compiler error messages.")
(defvar *error-print-length* 5
  "The value for *Print-Length* when printing compiler error messages.")


;;; We save the context information that we printed out most recently so that
;;; we don't print it out redundantly.
;;;
(proclaim '(list *last-source-context*))
(defvar *last-source-context* nil)
(defvar *last-original-source* nil)
(defvar *last-source-form* nil)
(defvar *last-format-string* nil)
(defvar *last-format-args* nil)
(defvar *last-message-count* 0)

;;; The stream that compiler error output is directed to, or NIL if error
;;; output is inhibited.
;;;
(defvar *compiler-error-output* (make-synonym-stream '*error-output*))
(proclaim '(type (or stream null) *compiler-error-output*))


;;; Note-Message-Repeats  --  Internal
;;;
;;;    If the last message was given more than once, then print out an
;;; indication of how many times it was repeated.  We reset the message count
;;; when we are done.
;;;
(defun note-message-repeats ()
  (when (> *last-message-count* 1)
    (format *compiler-error-output* "[Last message occurs ~D times]~%"
	    *last-message-count*))
  (setq *last-message-count* 0))


;;; Print-Error-Message  --  Internal
;;;
;;;    Print out the message, with appropriate context if we can find it.  If
;;; If the context is different from the context of the last message we
;;; printed, then we print the context.  If the original source is different
;;; from the source we are working on, then we print the current source in
;;; addition to the original source.
;;;
;;;    We suppress printing of messages identical to the previous, but record
;;; the number of times that the message is repeated.
;;;
(defun print-error-message (what format-string format-args)
  (declare (string what format-string) (list format-args))
  (let* ((*print-level* *error-print-level*)
	 (*print-length* *error-print-length*)
	 (stream *compiler-error-output*)
	 (context (find-error-context)))
    
    (unless stream (return-from print-error-message (undefined-value)))
    
    (cond
     (context
      (let ((context (compiler-error-context-context context))
	    (form (compiler-error-context-original-source context))
	    (source (compiler-error-context-source context)))
	
	(unless (equal context *last-source-context*)
	  (note-message-repeats)
	  (setq *last-source-context* context)
	  (setq *last-original-source* nil)
	  (format stream "~2&In:~{~<~%   ~4:;~{ ~S~}~>~^ =>~}~%" context))
	
	(unless (equal form *last-original-source*)
	  (note-message-repeats)
	  (setq *last-original-source* form)
	  (setq *last-source-form* nil)
	  (format stream "  ~S~%" form))
	
	(unless (equal source *last-source-form*)
	  (note-message-repeats)
	  (setq *last-source-form* source)
	  (setq *last-format-string* nil)
	  (unless (or (equal source form) (member source format-args))
	    (format stream "==>~%  ~S~%" source)))))
     (t
      (note-message-repeats)
      (format stream "~2&")))
    
    (unless (and (equal format-string *last-format-string*)
		 (equal format-args *last-format-args*))
      (note-message-repeats)
      (setq *last-format-string* format-string)
      (setq *last-format-args* format-args)
      (format stream "~&~A: ~?~&" what format-string format-args)))
  
  (incf *last-message-count*)
  (undefined-value))


;;; Keep track of how many times each kind of warning happens.
;;;
(proclaim '(type unsigned-byte *compiler-error-count* *compiler-warning-count*
		 *compiler-note-count*))
(defvar *compiler-error-count* 0)
(defvar *compiler-warning-count* 0)
(defvar *compiler-note-count* 0)


;;; Compiler-Error, ...  --  Interface
;;;
;;;    Increment the count and print the message.  Compiler-Note never prints
;;; anything when Brevity is 3.  Compiler-Error calls the bailout function
;;; so that it never returns.  Compiler-Error-Message returns like
;;; Compiler-Warning, but prints a message like Compiler-Error.
;;;
(proclaim '(ftype (function (string &rest t) void)
		  compiler-error compiler-warning compiler-note))
;;;
(defun compiler-error (format-string &rest format-args)
  (incf *compiler-error-count*)
  (print-error-message "Error" format-string format-args)
  (funcall *compiler-error-bailout*)
  (error "*Compiler-Error-Bailout* returned?"))
;;;
(defun compiler-error-message (format-string &rest format-args)
  (incf *compiler-error-count*)
  (print-error-message "Error" format-string format-args))
;;;
(defun compiler-warning (format-string &rest format-args)
  (incf *compiler-warning-count*)
  (print-error-message "Warning" format-string format-args))
;;;
(defun compiler-note (format-string &rest format-args)
  (incf *compiler-note-count*)
  (unless (if *compiler-error-context*
	      (policy *compiler-error-context* (= brevity 3))
	      (policy nil (= brevity 3)))
    (print-error-message "Note" format-string format-args)))


;;; Compiler-Mumble  --  Interface
;;;
;;;    The politically correct way to print out random progress messages and
;;; such like.  We clear the current error context so that we know that it
;;; needs to be reprinted, and we also Force-Output so that the message gets
;;; seen right away.
;;;
(proclaim '(function compiler-mumble (string &rest t) void))
(defun compiler-mumble (format-string &rest format-args)
  (when *last-format-string*
    (note-message-repeats)
    (terpri *compiler-error-output*)
    (setq *last-source-context* nil)
    (setq *last-format-string* nil))
  (apply #'format *compiler-error-output* format-string format-args)
  (force-output))


;;; Find-Component-Name  --  Interface
;;;
;;;    Return a string that somehow names the code in Component.  We use the
;;; source path for the bind node for an arbitrary entry point to find the
;;; source context, then return that as a string.
;;;
(proclaim  '(function find-component-name (component) simple-string))
(defun find-component-name (component)
  (let ((ep (first (block-succ (component-head component)))))
    (assert ep () "No entry points?")
    (multiple-value-bind
	(form context)
	(find-original-source
	 (node-source-path (continuation-next (block-start ep))))
      (declare (ignore form))
      (let ((*print-level* 2)
	    (*print-pretty* nil))
	(format nil "~{~{~S~^ ~}~^ => ~}" context)))))


;;;; Careful call:

;;; Careful-Call  --  Interface
;;;
;;;    Apply a function to some arguments, returning a list of the values
;;; resulting of the evaulation.  If an error is signalled during the
;;; application, then we print a warning message and return NIL as our second
;;; value to indicate this.  Node is used as the error context for any error
;;; message, and Context is a string that is spliced into the warning.
;;;
(proclaim '(function careful-call (function list node string) (values list boolean)))
(defun careful-call (function args node context)
  (values
   (multiple-value-list
    (handler-case (apply function args)
      (error (condition)
	(let ((*compiler-error-context* node))
	  (compiler-warning "Lisp error during ~A:~%~A" context condition)
	  (return-from careful-call (values nil nil))))))
   t))


;;;; Generic list (?) functions:

;;; Find-In  --  Interface
;;;
(defun find-in (next element list &key (key #'identity)
		     (test #'eql test-p) (test-not nil not-p))
  "Find Element in a null-terminated List linked by the accessor function
  Next.  Key, Test and Test-Not are the same as for generic sequence
  functions."
  (when (and test-p not-p)
    (error "Silly to supply both :Test and :Test-Not."))
  (if not-p
      (do ((current list (funcall next current)))
	  ((null current) nil)
	(unless (funcall test-not (funcall key current) element)
	  (return current)))
      (do ((current list (funcall next current)))
	  ((null current) nil)
	(when (funcall test (funcall key current) element)
	  (return current)))))

;;; Position-In  --  Interface
;;;
(defun position-in (next element list &key (key #'identity)
		     (test #'eql test-p) (test-not nil not-p))
  "Return the position of Element (or NIL if absent) in a null-terminated List
  linked by the accessor function Next.  Key, Test and Test-Not are the same as
  for generic sequence functions."
  (when (and test-p not-p)
    (error "Silly to supply both :Test and :Test-Not."))
  (if not-p
      (do ((current list (funcall next current))
	   (i 0 (1+ i)))
	  ((null current) nil)
	(unless (funcall test-not (funcall key current) element)
	  (return i)))
      (do ((current list (funcall next current))
	   (i 0 (1+ i)))
	  ((null current) nil)
	(when (funcall test (funcall key current) element)
	  (return i)))))


;;; Map-In  --  Interface
;;;
(defun map-in (next function list)
  "Map Function over the elements in a null-terminated List linked by the
  accessor function Next, returning a list of the results."
  (collect ((res))
    (do ((current list (funcall next current)))
	((null current))
      (res (funcall function current)))
    (res)))


;;; Deletef-In  --  Interface
;;;
(defmacro deletef-in (next place item &environment env)
  "Deletef-In Next Place Item
  Delete Item from a null-terminated list linked by the accessor function Next
  that is stored in Place.  Item must appear exactly once in the list."
  (multiple-value-bind
      (temps vals stores store access)
      #-new-compiler
      (if clc::*in-the-compiler*
	  (get-setf-method place env)
	  (lisp::foo-get-setf-method place env))
      #+new-compiler
      (lisp::foo-get-setf-method place env)
    (let ((n-item (gensym))
	  (n-place (gensym))
	  (n-current (gensym))
	  (n-prev (gensym)))
      `(let* (,@(mapcar #'list temps vals)
	      (,n-place ,access)
	      (,n-item ,item))
	 (if (eq ,n-place ,n-item)
	     (let ((,(first stores) (,next ,n-place)))
	       ,store)
	     (do ((,n-prev ,n-place ,n-current)
		  (,n-current (,next ,n-place)
			      (,next ,n-current)))
		 ((eq ,n-current ,n-item)
		  (setf (,next ,n-prev)
			(,next ,n-current)))))
	 (undefined-value)))))


;;; Push-In  --  Interface
;;;
(defmacro push-in (next item place &environment env)
  "Push Item onto a list linked by the accessor function Next that is stored in
  Place."
  (multiple-value-bind
      (temps vals stores store access)
      #-new-compiler
      (if clc::*in-the-compiler*
	  (get-setf-method place env)
	  (lisp::foo-get-setf-method place env))
      #+new-compiler
      (lisp::foo-get-setf-method place env)
    `(let (,@(mapcar #'list temps vals)
	   (,(first stores) ,item))
       (setf (,next ,(first stores)) ,access)
       ,store
       (undefined-value))))


;;; Compiler-Constantp  --  Interface
;;;
;;;    We don't want to assume that a variable is a constant just because it is
;;; in the current lisp environment.
;;;
;;; ### For now, just use CONSTANTP to avoid bootstrapping problems with having
;;; to have the INFO database available at meta-compile time.
;;;
(proclaim '(function compiler-constantp (t) boolean))
(defun compiler-constantp (exp)
  "Like constantp, only uses the compilation environment rather than the
  current Lisp environment."
#|
  (if (symbolp exp)
      (eq (info variable kind exp) :constant)
      (constantp exp))
|#
  (constantp exp))
