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
;;;
(defun get-operand-info (ref)
  (declare (type tn-ref ref))
  (let* ((arg-p (not (tn-ref-write-p ref)))
	 (vop (tn-ref-vop ref))
	 (info (vop-info vop)))
    (flet ((frob (refs costs restr load more-cost)
	     (do ((refs refs (tn-ref-across refs))
		  (costs costs (cdr costs))
		  (load load (cdr load))
		  (n 0 (1+ i)))
		 ((null costs)
		  (assert more-cost)
		  (values arg-p
			  (+ n (position-in #'tn-ref-across ref refs))
			  t
			  more-cost
			  nil))
	       (when (eq refs ref)
		 (return
		  (values arg-p
			  n
			  nil
			  (car costs)
			  (car load)))))))
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
	 (vop (tn-ref-vop ref))
	 (ptype (tn-primitive-type tn)))
    (multiple-value-bind (arg-p pos more-p costs load-scs)
			 (get-operand-info ref)
      (collect ((losers))
	(dolist (sc (primitive-type-scs ptype))
	  (unless (svref costs (sc-number sc))
	    (losers sc)))

	(unless (losers)
	  (error "Representation selection flamed out for no obvious reason.~@
	          Try again after recompiling the VM definition."))
	
	(error "~S is not valid as the ~:R ~:[result~;argument~] to the~@
	        ~S VOP, since the TN's primitive type ~S allows SCs:~%  ~S~@
		~:[which cannot be coerced or loaded into the allowed SCs:~
		~%  ~S~;~]"
	       tn pos arg-p
	       (template-name info)
	       (primitive-type-name ptype)
	       (mapcar #'sc-name losers)
	       more-p
	       (mapcar #'sc-name (listify-restrictions load-scs)))))))


;;; BAD-MOVE-ARG-ERROR  --  Internal
;;;
(defun bad-move-arg-error (val pass)
  (declare (type tn val pass))
  (error "No :MOVE-ARGUMENT VOP defined to move ~S (SC ~S) to
          ~S (SC ~S.)"
	 val (sc-name (tn-sc val))
	 pass (sc-name (tn-sc pass))))


;;; SELECT-TN-REPRESENTATION  --  Internal
;;;
;;;    Return the best representation for a normal TN.  SCs is a list of the SC
;;; numbers of the SCs to select from.  Costs is a scratch vector.
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
			     (do ((cost (,costs-slot info) (cdr cost))
				  (op (,ops-slot vop) (tn-ref-across op)))
				 ((null cost)
				  (add-costs (,more-costs-slot info)))
			       (when (eq op ref)
				 (add-costs (car cost))
				 (return))))))
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
	  (setq min-scs scn))))
    
    (svref *sc-numbers* min-scn)))


;;; NOTE-NUMBER-STACK-TN  --  Internal
;;;
;;;    Prepare for the possibility of a TN being allocated on the number stack
;;; by allocating a number-FP in all functions that TN is referenced in and in
;;; all the functions in their tail sets.  Refs is a TN-Refs list of references
;;; to the TN.
;;;
(defun note-number-stack-tn (refs)
  (declare (type (or tn-ref null) refs))
  (do ((ref refs (tn-ref-next refs)))
      ((null ref))
    (let* ((lambda (block-lambda
		    (ir2-block-block
		     (vop-block (tn-ref-fop ref)))))
	   (tails (lambda-tail-set lambda)))
      (flet ((frob (fun)
	       (let ((env (lambda-environment lambda))
		     (2env (environment-info env)))
		 (unless (ir2-environment-number-fp 2env)
		   (setf (ir2-environment-number-fp 2env)
			 (make-number-fp-tn env))))))
	(frob lambda)
	(when tails
	  (dolist (fun (tail-set-functions tails))
	    (frob fun))))))

  (undefined-value))


;;; ALLOCATE-OLD-NUMBER-FP-TNS  --  Internal
;;;
;;;    Allocate Old-Number-FP and Old-Number-FP-Pass TNs for all functions in
;;; tail sets that have locations which might be allocated on the number stack.
;;;
(defun allocate-old-number-fp-tns (component)
  (dolist (fun (component-lambdas component))
    (let* ((env (lambda-environment fun))
	   (2env (environment-info env)))
      (unless (ir2-environment-old-number-fp 2env)
	(let ((tails (lambda-tail-set fun)))
	  (when tails
	    (let ((info (tail-set-info tails)))
	      (when (eq (return-info-kind info) :fixed)
		(dolist (loc (return-info-locations ))
		  (when (sc-number-stack-p (tn-sc loc))
		    (setf (ir2-environment-old-number-fp 2env)
			  (make-normal-tn *any-primitive-type*))
		    (setf (ir2-environment-old-number-fp-pass 2env)
			  (make-normal-tn *any-primitive-type*))
		    (return))))))))))

  (undefined-value))


;;; EMIT-COERCE-VOP  --  Internal
;;;
;;;    Emit a coercion VOP for Op or die trying.  SCS is the operand's LOAD-SCS
;;; vector, which we use to determine what SCs the VOP will accept.  We pick
;;; any acceptable coerce VOP, since it practice it seems uninteresting to have
;;; more than one applicable.
;;;
;;;    What we do is look at each SC allowed by the operand restriction, and
;;; see if there is a move VOP which moves between the operand's SC and load
;;; SC.  If we find such a VOP, then we make a TN having the load SC as the
;;; representation.
;;;
(defun emit-coerce-vop (op scs)
  (declare (type tn-ref op) (type sc-vector scs))
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
	      (if write-p 
		  (emit-move-template node block res temp op-tn vop)
		  (emit-move-template node block res op-tn temp vop)))
	    (return)))))))


;;; COERCE-VOP-OPERANDS  --  Internal
;;;
;;;    Scan the operands to VOP and call EMIT-COERCE-VOP on any for which we
;;; can't load the operand.
;;;
(defun coerce-vop-operands (vop)
  (declare (type vop vop))
  (let ((info (vop-info vop)))
    (macrolet ((scan (ops load-scs)
		 `(do ((op ,ops (tn-ref-across op))
		       (scs ,load-scs (cdr scs)))
		      ((null scs))
		    (unless (svref (car scs)
				   (sc-number (tn-sc (tn-ref-tn op))))
		      (emit-coerce-vop op (car scs))))))
      (scan (vop-args vop) (vop-info-arg-load-scs info))
      (scan (vop-results vop) (vop-info-result-load-scs info)))))


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
      (case (vop-info-name info)
	(move-arguments
	 (let* ((info-args (vop-codegen-info vop))
		(fp-tn (first info-args))
		(nfp-tn (second info-args))
		(pass-locs (third (info-args))))
	   (do ((val (vop-args vop) (tn-ref-across val))
		(pass pass-locs (cdr pass-loc)))
	       ((null val))
	     (let* ((val-tn (tn-ref-tn val))
		    (pass-tn (first pass))
		    (pass-sc (tn-sc pass-tn))
		    (res (svref (sc-move-arg-vops pass-sc)
				(sc-number (tn-sc val-tn)))))
	       (unless res
		 (bad-move-arg-error val-tn pass-tn))
	       (emit-move-arg-template node block res val-tn
				       (if (sc-number-stack-p pass-sc)
					   nfp-tn fp-tn)
				       pass-tn
				       vop)))))
	(move
	 (let ((x (tn-ref-tn (vop-args op)))
	       (y (tn-ref-tn (vop-results vop)))
	       (res (svref (sc-move-vops (tn-sc y))
			   (sc-number (tn-sc x)))))
	   (cond (res
		  (emit-move-template node block res x y vop)
		  (delete-vop vop))
		 (t
		  (coerce-vop-operands vop)))))
	(t
	 (coerce-vop-operands vop))))))


;;; SELECT-REPRESENTATIONS  --  Interface
;;;
;;;    Entry to representation selection.  First we select the representation
;;; for all normal TNs, setting the TN-SC.  If we select a representation that
;;; allows the number stack, then we note this so that the number-FP can be
;;; allocated.  Next we allocate old-NFP passing TNs for functions that may
;;; return values on the number stack.  Finally, we scan the IR2 looking for
;;; places that we need to insert coercions and representation-specific moves.
;;;
(defun select-representations (component)
  (let ((costs (make-array sc-number-limit)))
    (do ((tn (ir2-component-normal-tns (component-info component))
	     (tn-next tn)))
	((null tn))
      (let* ((scs (primitive-type-scs (tn-primitive-type tn)))
	     (sc (if (rest scs)
		     (select-tn-representation tn scs costs)
		     (svref *sc-numbers* (first scs)))))
	(setf (tn-sc tn) sc)
	(when (sc-number-stack-p tn)
	  (note-number-stack-tn tn)))))

  (allocate-old-number-fp-tns component)

  (do-ir2-blocks (block component)
    (emit-moves-and-coercions block))

  (undefined-value))
