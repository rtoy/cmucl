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
;;;    Insert Block in the emission order after the block After.  We also add
;;; the block to the IR2-Environment-Blocks.
;;;
(defun add-to-emit-order (block after)
  (declare (type ir2-block block after))
  (let ((next (ir2-block-next after)))
    (setf (ir2-block-next after) block)
    (setf (ir2-block-prev block) after)
    (setf (ir2-block-next block) next)
    (setf (ir2-block-prev next) block))

  (let ((2env (environment-info
	       (lambda-environment
		(block-lambda (ir2-block-block block))))))
    (push-in ir2-block-environment-next block (ir2-environment-blocks 2env)))

  (undefined-value))


;;; Control-Analyze-Block  --  Internal
;;;
;;;    Do a graph walk linking blocks into the emit order as we go.  We treat
;;; blocks ending in TR nodes specially, since it may be that we want to go
;;; somewhere other than the return block.  In a TR local call, we drop through
;;; to the head of the called function, rather than the return node.
;;;
;;;    If the IR2 blocks haven't already been assigned, then we make them at
;;; this point.
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
      (when (and (combination-p last) (node-tail-p last)
		 (eq (basic-combination-kind last) :local))
	(control-analyze-block (node-block
				(lambda-bind
				 (ref-leaf
				  (continuation-use
				   (basic-combination-fun last)))))
			       tail)))
    
    (dolist (succ (block-succ block))
      (control-analyze-block succ tail))))


;;; Control-Analyze  --  Interface
;;;
;;;    Do control analysis on Component, finding the emit order.  Our only
;;; cleverness here is that we walk XEP's first to increase the probability
;;; that the tail call will be a drop-through.
;;;
;;;    When we are done, we delete all blocks that weren't reached during our
;;; walk.  This allows IR2 phases to assume that all IR1 blocks in the DFO have
;;; valid IR2 blocks in their Info.
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

    (dolist (succ (block-succ head))
      (let ((node (continuation-next (block-start succ))))
	(when (and (bind-p node)
		   (external-entry-point-p (bind-lambda node)))
	  (control-analyze-block succ tail-block))))
    
    (dolist (succ (block-succ head))
      (control-analyze-block succ tail-block))

    (do-blocks (block component)
      (unless (block-flag block)
	(delete-block block))))

  (undefined-value))
