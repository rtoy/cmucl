;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the implementation independent code for the
;;; representation selection phase in the compiler.  Representation selection
;;; decides whether to use non-descriptor representations for objects and emits
;;; the appropriate representation-specific move and coerce vops.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Error routines:
;;;
;;;    Problems in the VM definition often show up here, so we try to be as
;;; implementor-friendly as possible.
;;;

;;; GET-OPERAND-INFO  --  Interface
;;;
;;;    Given a TN ref for a VOP argument or result, return these values:
;;; 1] True if the operand is an argument, false otherwise.
;;; 2] The ordinal position of the operand.
;;; 3] True if the operand is a more operand, false otherwise.
;;; 4] The costs for this operand.
;;; 5] The load-scs vector for this operand (NIL if more-p.)
;;; 6] True if the costs or SCs in the VOP-INFO are inconsistent with the
;;;    currently record ones.
;;;
(defun get-operand-info (ref)
  (declare (type tn-ref ref))
  (let* ((arg-p (not (tn-ref-write-p ref)))
	 (vop (tn-ref-vop ref))
	 (info (vop-info vop)))
    (flet ((frob (refs costs load more-cost)
	     (do ((refs refs (tn-ref-across refs))
		  (costs costs (cdr costs))
		  (load load (cdr load))
		  (n 0 (1+ n)))
		 ((null costs)
		  (assert more-cost)
		  (values arg-p
			  (+ n (position-in #'tn-ref-across ref refs) 1)
			  t
			  more-cost
			  nil
			  nil))
	       (when (eq refs ref)
		 (let ((parse (vop-parse-or-lose (vop-info-name info))))
		   (multiple-value-bind
		       (ccosts cscs)
		       (compute-loading-costs
			(elt (if arg-p
				 (vop-parse-args parse)
				 (vop-parse-results parse))
			     n)
			arg-p)
		     
		     (return
		      (values arg-p
			      (1+ n)
			      nil
			      (car costs)
			      (car load)
			      (not (and (equalp ccosts (car costs))
					(equalp cscs (car load))))))))))))
      (if arg-p
	  (frob (vop-args vop) (vop-info-arg-costs info)
	        (vop-info-arg-load-scs info)
	        (vop-info-more-arg-costs info))
	  (frob (vop-results vop) (vop-info-result-costs info)
	        (vop-info-result-load-scs info)
	        (vop-info-more-result-costs info))))))


;;; LISTIFY-RESTRICTIONS  --  Interface
;;;
;;;    Convert a load-costs vector to the list of SCs allowed by the operand
;;; restriction.
;;;
(defun listify-restrictions (restr)
  (declare (type sc-vector restr))
  (collect ((res))
    (dotimes (i sc-number-limit)
      (when (eql (svref restr i) i)
	(res (svref *sc-numbers* i))))
    (res)))

    
;;; BAD-COSTS-ERROR  --  Internal
;;;
;;;    Try to give a helpful error message when Ref has no cost specified for
;;; some SC allowed by the TN's primitive-type.
;;;
(defun bad-costs-error (ref)
  (declare (type tn-ref ref))
  (let* ((tn (tn-ref-tn ref))
	 (ptype (tn-primitive-type tn)))
    (multiple-value-bind (arg-p pos more-p costs load-scs incon)
			 (get-operand-info ref)
      (collect ((losers))
	(dolist (scn (primitive-type-scs ptype))
	  (unless (svref costs scn)
	    (losers (svref *sc-numbers* scn))))

	(unless (losers)
	  (error "Representation selection flamed out for no obvious reason.~@
	          Try again after recompiling the VM definition."))
	
	(error "~S is not valid as the ~:R ~:[result~;argument~] to the~@
	        ~S VOP, since the TN's primitive type ~S allows SCs:~%  ~S~@
		~:[which cannot be coerced or loaded into the allowed SCs:~
		~%  ~S~;~]~:[~;~@
		Current cost info inconsistent with that in effect at compile ~
		time.  Recompile.~%Compilation order may be incorrect.~]"
	       tn pos arg-p
	       (template-name (vop-info (tn-ref-vop ref)))
	       (primitive-type-name ptype)
	       (mapcar #'sc-name (losers))
	       more-p
	       (mapcar #'sc-name (listify-restrictions load-scs))
	       incon)))))


;;; BAD-MOVE-ARG-ERROR  --  Internal
;;;
(defun bad-move-arg-error (val pass)
  (declare (type tn val pass))
  (error "No :MOVE-ARGUMENT VOP defined to move ~S (SC ~S) to ~
          ~S (SC ~S.)"
	 val (sc-name (tn-sc val))
	 pass (sc-name (tn-sc pass))))


;;;; VM Consistency Checking:
;;;
;;;    We do some checking of the consistency of the VM definition at load
;;; time.

;;; CHECK-MOVE-FUNCTION-CONSISTENCY  --  Interface
;;;
(defun check-move-function-consistency ()
  (dotimes (i sc-number-limit)
    (let ((sc (svref *sc-numbers* i)))
      (when sc
	(let ((moves (sc-load-functions sc)))
	  (dolist (const (sc-constant-scs sc))
	    (unless (svref moves (sc-number const))
	      (error "No move function defined to load SC ~S from constant ~
	              SC ~S."
		     (sc-name sc) (sc-name const))))

	  (dolist (alt (sc-alternate-scs sc))
	    (unless (svref moves (sc-number alt))
	      (error "No move function defined to load SC ~S from alternate ~
	              SC ~S."
		     (sc-name sc) (sc-name alt)))
	    (unless (svref (sc-load-functions alt) i)
	      (error "No move function defined to save SC ~S to alternate ~
	              SC ~S."
		     (sc-name sc) (sc-name alt)))))))))
;;;
(check-move-function-consistency)


;;; SELECT-TN-REPRESENTATION  --  Internal
;;;
;;;    Return the best representation for a normal TN.  SCs is a list of the SC
;;; numbers of the SCs to select from.  Costs is a scratch vector.
;;;
;;;     What we do is sum the costs for each reference to TN in each of the
;;; SCs, and then return the SC having the lowest cost.  We ignore references
;;; by the MOVE VOP, since counting them would spuriously encourage descriptor
;;; representations.  We won't actually need to coerce to descriptor and back,
;;; since we will replace the MOVE with a specialized move VOP.
;;;
(defun select-tn-representation (tn scs costs)
  (declare (type tn tn) (type sc-vector costs))
  (dolist (scn scs)
    (setf (svref costs scn) 0))
  
  (macrolet ((scan-refs (refs ops-slot costs-slot more-costs-slot)
	       `(do ((ref ,refs (tn-ref-next ref)))
		    ((null ref))
		  (let* ((vop (tn-ref-vop ref))
			 (info (vop-info vop)))
		    (unless (eq (vop-info-name info) 'move)
		      (do ((cost (,costs-slot info) (cdr cost))
			   (op (,ops-slot vop) (tn-ref-across op)))
			  ((null cost)
			   (add-costs (,more-costs-slot info)))
			(when (eq op ref)
			  (add-costs (car cost))
			  (return)))))))
	     (add-costs (cost)
	       `(let ((cost ,cost))
		  (dolist (scn scs)
		    (let ((res (svref cost scn)))
		      (unless res
			(bad-costs-error ref))
		      (incf (svref costs scn) res))))))
    
    (scan-refs (tn-reads tn) vop-args vop-info-arg-costs
	       vop-info-more-arg-costs)
    (scan-refs (tn-writes tn) vop-results vop-info-result-costs
	       vop-info-more-result-costs))
  
  (let ((min most-positive-fixnum)
	(min-scn nil))
    (dolist (scn scs)
      (let ((cost (svref costs scn)))
	(when (< cost min)
	  (setq min cost)
	  (setq min-scn scn))))
    
    (svref *sc-numbers* min-scn)))


;;; NOTE-NUMBER-STACK-TN  --  Internal
;;;
;;;    Prepare for the possibility of a TN being allocated on the number stack
;;; by setting NUMBER-STACK-P in all functions that TN is referenced in and in
;;; all the functions in their tail sets.  Refs is a TN-Refs list of references
;;; to the TN.
;;;
(defun note-number-stack-tn (refs)
  (declare (type (or tn-ref null) refs))
  
  (do ((ref refs (tn-ref-next ref)))
      ((null ref))
    (let* ((lambda (lambda-home
		    (block-lambda
		     (ir2-block-block
		      (vop-block (tn-ref-vop ref))))))
	   (tails (lambda-tail-set lambda)))
      (flet ((frob (fun)
	       (setf (ir2-environment-number-stack-p
		      (environment-info
		       (lambda-environment fun)))
		     t)))
	(frob lambda)
	(when tails
	  (dolist (fun (tail-set-functions tails))
	    (frob fun))))))

  (undefined-value))


;;; EMIT-COERCE-VOP  --  Internal
;;;
;;;    Emit a coercion VOP for Op Before the specifed VOP or die trying.  SCS
;;; is the operand's LOAD-SCS vector, which we use to determine what SCs the
;;; VOP will accept.  We pick any acceptable coerce VOP, since it practice it
;;; seems uninteresting to have more than one applicable.
;;;
;;;    What we do is look at each SC allowed by the operand restriction, and
;;; see if there is a move VOP which moves between the operand's SC and load
;;; SC.  If we find such a VOP, then we make a TN having the load SC as the
;;; representation.
;;;
;;;    If the TN is an unused result TN, then we don't actually emit the move;
;;; we just change to the right kind of TN.
;;;
(defun emit-coerce-vop (op scs before)
  (declare (type tn-ref op) (type sc-vector scs) (type (or vop null) before))
  (let* ((op-tn (tn-ref-tn op))
	 (op-sc (tn-sc op-tn))
	 (op-scn (sc-number op-sc))
	 (write-p (tn-ref-write-p op))
	 (vop (tn-ref-vop op))
	 (node (vop-node vop))
	 (block (vop-block vop)))
    (dotimes (i sc-number-limit (bad-costs-error op))
      (when (eql (svref scs i) i)
	(let ((res (if write-p
		       (svref (sc-move-vops op-sc) i)
		       (svref (sc-move-vops (svref *sc-numbers* i))
			      op-scn))))
	  (when res
	    (let ((temp (make-representation-tn i)))
	      (change-tn-ref-tn op temp)
	      (cond
	       ((not write-p)
		(emit-move-template node block res op-tn temp before))
	       ((null (tn-reads op-tn)))
	       (t
		(emit-move-template node block res temp op-tn before))))
	    (return)))))))


;;; COERCE-SOME-OPERANDS  --  Internal
;;;
;;;    Scan some operands and call EMIT-COERCE-VOP on any for which we can't
;;; load the operand.  The coerce VOP is inserted Before the specified VOP.
;;;
(proclaim '(inline coerce-some-operands))
(defun coerce-some-operands (ops load-scs before)
  (declare (type (or tn-ref null) ops) (list load-scs)
	   (type (or vop null) before))
  (do ((op ops (tn-ref-across op))
       (scs load-scs (cdr scs)))
      ((null scs))
    (unless (svref (car scs)
		   (sc-number (tn-sc (tn-ref-tn op))))
      (emit-coerce-vop op (car scs) before)))
  (undefined-value))


;;; COERCE-VOP-OPERANDS  --  Internal
;;;
;;;    Emit coerce VOPs for the args and results, as needed.
;;;
(defun coerce-vop-operands (vop)
  (declare (type vop vop))
  (let ((info (vop-info vop)))
    (coerce-some-operands (vop-args vop) (vop-info-arg-load-scs info) vop)
    (coerce-some-operands (vop-results vop) (vop-info-result-load-scs info)
			  (vop-next vop))))


;;; EMIT-ARG-MOVES  --  Internal
;;;
;;;    Iterate over the more operands to a call VOP, emitting move-arg VOPs and
;;; any necessary coercions.  We determine which FP to use by looking at the
;;; MOVE-ARGS annotation.
;;;
(defun emit-arg-moves (vop)
  (let* ((info (vop-info vop))
	 (node (vop-node vop))
	 (block (vop-block vop))
	 (how (vop-info-move-args info))
	 (args (vop-args vop))
	 (fp-tn (tn-ref-tn args))
	 (nfp-tn (if (eq how :local-call)
		     (tn-ref-tn (tn-ref-across args))
		     nil))
	 (pass-locs (first (vop-codegen-info vop)))
	 (prev (vop-prev vop)))
    (do ((val (do ((arg args (tn-ref-across arg))
		   (req (template-arg-types info) (cdr req)))
		  ((null req) arg))
	      (tn-ref-across val))
	 (pass pass-locs (cdr pass)))
	((null val)
	 (assert (null pass)))
      (let* ((val-tn (tn-ref-tn val))
	     (pass-tn (first pass))
	     (pass-sc (tn-sc pass-tn))
	     (res (svref (sc-move-arg-vops pass-sc)
			 (sc-number (tn-sc val-tn)))))
	(unless res
	  (bad-move-arg-error val-tn pass-tn))
	
	(change-tn-ref-tn val pass-tn)
	(let* ((this-fp
		(cond ((not (sc-number-stack-p pass-sc)) fp-tn)
		      (nfp-tn)
		      (t
		       (assert (eq how :known-return))
		       (setq nfp-tn
			     (make-representation-tn
			      (first (primitive-type-scs
				      *any-primitive-type*))))
		       (emit-context-template node block nfp-tn vop)
		       (assert (not (sc-number-stack-p (tn-sc nfp-tn))))
		       nfp-tn)))
	       (new (emit-move-arg-template node block res val-tn this-fp
					    pass-tn vop)))
	  (coerce-some-operands (vop-args new) (vop-info-arg-load-scs res)
				(if prev
				    (vop-next prev)
				    (ir2-block-start-vop block)))))))
  (undefined-value))


;;; EMIT-MOVES-AND-COERCIONS  --  Internal
;;;
;;;    Scan the IR2 looking for move operations that need to be replaced with
;;; special-case VOPs and emitting coercion VOPs for operands of normal VOPs.
;;;
(defun emit-moves-and-coercions (block)
  (declare (type ir2-block block))
  (do ((vop (ir2-block-start-vop block)
	    (vop-next vop)))
      ((null vop))
    (let ((info (vop-info vop))
	  (node (vop-node vop))
	  (block (vop-block vop)))
      (cond
       ((eq (vop-info-name info) 'move)
	(let* ((x (tn-ref-tn (vop-args vop)))
	       (y (tn-ref-tn (vop-results vop)))
	       (res (svref (sc-move-vops (tn-sc y))
			   (sc-number (tn-sc x)))))
	  (cond (res
		 (emit-move-template node block res x y vop)
		 (delete-vop vop))
		(t
		 (coerce-vop-operands vop)))))
       ((vop-info-move-args info)
	(emit-arg-moves vop))
       (t
	(coerce-vop-operands vop))))))


;;; NOTE-IF-NUMBER-STACK  --  Internal
;;;
;;;    If TN is in a number stack SC, make all the right annotations.  Note
;;; that this should be called after TN has been referenced, since it must
;;; iterate over the referencing environments.
;;;
(proclaim '(inline note-if-number-stack))
(defun note-if-number-stack (tn 2comp)
  (declare (type tn tn) (type ir2-component 2comp))
  (when (sc-number-stack-p (tn-sc tn))
    (unless (ir2-component-nfp 2comp)
      (setf (ir2-component-nfp 2comp) (make-nfp-tn)))
    (note-number-stack-tn (tn-reads tn))
    (note-number-stack-tn (tn-writes tn)))
  (undefined-value))


;;; SELECT-REPRESENTATIONS  --  Interface
;;;
;;;    Entry to representation selection.  First we select the representation
;;; for all normal TNs, setting the TN-SC.  We then scan all the IR2,
;;; emitting any necessary coerce and move-arg VOPs.  Finally, we scan all
;;; TNs looking for ones that might be placed on the number stack, noting
;;; this so that the number-FP can be allocated.  This must be done last,
;;; since references in new environments may be introduced by MOVE-ARG
;;; insertion.
;;;
(defun select-representations (component)
  (let ((costs (make-array sc-number-limit))
	(2comp (component-info component)))
	        
    (do ((tn (ir2-component-normal-tns 2comp)
	     (tn-next tn)))
	((null tn))
      (unless (tn-sc tn)
	(let* ((scs (primitive-type-scs (tn-primitive-type tn)))
	       (sc (if (rest scs)
		       (select-tn-representation tn scs costs)
		       (svref *sc-numbers* (first scs)))))
	  (assert sc)
	  (setf (tn-sc tn) sc))))

    (do-ir2-blocks (block component)
      (emit-moves-and-coercions block))
    
    (macrolet ((frob (slot)
		 `(do ((tn (,slot 2comp) (tn-next tn)))
		      ((null tn))
		    (note-if-number-stack tn 2comp))))
      (frob ir2-component-normal-tns)
      (frob ir2-component-wired-tns)
      (frob ir2-component-restricted-tns)))

  (undefined-value))
