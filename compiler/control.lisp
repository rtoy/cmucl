;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    The control analysis pass in the compiler.  This pass determines the
;;; order in which the IR2 blocks are to be emitted, attempting to minimize the
;;; associated branching costs.
;;;
;;;    At this point, we commit to generating IR2 (and ultimately assembler)
;;; for reachable blocks.  Before this phase there might be blocks that are
;;; unreachable but still appear in the DFO, due in inadequate optimization,
;;; etc.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Add-To-Emit-Order  --  Interface
;;;
;;;    Insert Block in the emission order after the block After.
;;;
(defun add-to-emit-order (block after)
  (declare (type ir2-block block after))
  (let ((next (ir2-block-next after)))
    (setf (ir2-block-next after) block)
    (setf (ir2-block-prev block) after)
    (setf (ir2-block-next block) next)
    (setf (ir2-block-prev next) block))
  (undefined-value))


;;; Control-Analyze-Block  --  Internal
;;;
;;;    Do a graph walk linking blocks into the emit order as we go.  We treat
;;; blocks ending in tail local calls specially.  We can't walked the called
;;; function immediately, since it is in a different function and we must keep
;;; the code for a function contiguous.   Instead, we return the function that
;;; we want to call so that it can be walked as soon as possible, which is
;;; hopefully immediately.
;;;
;;;    If any of the recursive calls ends in a tail local call, then we return
;;; the last such function, since it is the only one we can possibly drop
;;; through to.  (But it doesn't have to be from the last block walked, since
;;; that call might not have added anything.)
;;;
(defun control-analyze-block (block tail)
  (declare (type cblock block) (type ir2-block tail))
  (unless (block-flag block)
    (setf (block-flag block) t)
    (assert (and (block-component block) (not (block-delete-p block))))
    (add-to-emit-order (or (block-info block)
			   (setf (block-info block) (make-ir2-block block)))
		       (ir2-block-prev tail))

    (let ((last (block-last block)))
      (cond ((and (combination-p last) (node-tail-p last)
		  (eq (basic-combination-kind last) :local))
	     (combination-lambda last))
	    (t
	     (let ((fun nil))
	       (dolist (succ (block-succ block))
		 (let ((res (control-analyze-block succ tail)))
		   (when res (setq fun res))))
	       fun))))))


;;; CONTROL-ANALYZE-1-FUN  --  Internal
;;;
;;;    Analyze all of the NLX EPs first to ensure that code reachable only from
;;; a NLX is emitted contiguously with the code reachable from the Bind.  Code
;;; reachable from the Bind is inserted *before* the NLX code so that the Bind
;;; marks the beginning of the code for the function.  The walk from a NLX EP
;;; will never reach the bind block, so we will always get to insert it at the
;;; beginning.
;;;
;;;    If the talk from the bind node encountered a tail local call, then we
;;; start over again there to help the call drop through.  Of course, it will
;;; never get a drop-through if either function has NLX code.
;;;
(defun control-analyze-1-fun (fun component)
  (declare (type clambda fun) (type component component))
  (let* ((tail-block (block-info (component-tail component)))
	 (prev-block (ir2-block-prev tail-block))
	 (bind-block (node-block (lambda-bind fun))))
    (unless (block-flag bind-block)
      (dolist (nlx (environment-nlx-info (lambda-environment fun)))
	(control-analyze-block (nlx-info-target nlx) tail-block))
      (assert (not (block-flag bind-block)))
      (let ((new-fun (control-analyze-block bind-block
					    (ir2-block-next prev-block))))
	(when new-fun
	  (control-analyze-1-fun new-fun component)))))
  (undefined-value))

  
;;; Control-Analyze  --  Interface
;;;
;;;    Do control analysis on Component, finding the emit order.  Our only
;;; cleverness here is that we walk XEP's first to increase the probability
;;; that the tail call will be a drop-through.
;;;
;;;    When we are done, we delete all blocks that weren't reached during our
;;; walk.  This allows IR2 phases to assume that all IR1 blocks in the DFO have
;;; valid IR2 blocks in their Info.  We delete all deleted blocks from the
;;; IR2-COMPONENT VALUES-GENERATORS and VALUES-RECEIVERS so that stack analysis
;;; won't get confused.
;;;
(defun control-analyze (component)
  (let* ((head (component-head component))
	 (head-block (make-ir2-block head))
	 (tail (component-tail component))
	 (tail-block (make-ir2-block tail)))
    (setf (block-info head) head-block)
    (setf (block-info tail) tail-block)
    (setf (ir2-block-prev tail-block) head-block)
    (setf (ir2-block-next head-block) tail-block)

    (clear-flags component)

    (dolist (fun (component-lambdas component))
      (when (external-entry-point-p fun)
	(control-analyze-1-fun fun component)))

    (dolist (fun (component-lambdas component))
      (control-analyze-1-fun fun component))

    (do-blocks (block component)
      (unless (block-flag block)
	(delete-block block))))

  (let ((2comp (component-info component)))
    (setf (ir2-component-values-receivers 2comp)
	  (delete-if-not #'block-component
			 (ir2-component-values-receivers 2comp)))
    (setf (ir2-component-values-generators 2comp)
	  (delete-if-not #'block-component
			 (ir2-component-values-generators 2comp))))

  (undefined-value))
