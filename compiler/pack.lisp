;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the implementation independent code for Pack phase in
;;; the compiler and utilities used for manipulating TNs.  Pack is responsible
;;; for assigning TNs to storage allocations or "register allocation".
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Conflict determination:


;;; Offset-Conflicts-In-SB  --  Internal
;;;
;;; Return true if the element at the specified offset in SB has a conflict
;;; with TN:
;;; -- If an environment-live TN (:environment kind), then iterate over all the
;;;    blocks in its environment.  If the element at Offset is used anywhere in
;;;    any of the environment's blocks (always-live /= 0), then there is a
;;;    conflict.
;;; -- If TN is global (Confs true), then iterate over the blocks TN is live in
;;;    (using TN-Global-Conflicts).  If the TN is live everywhere in the block
;;;    (:Live), then there is a conflict if the element at offset is used
;;;    anywhere in the block (Always-Live /= 0).  Otherwise, we use the local
;;;    TN number for TN in block to find whether TN has a conflict at Offset in
;;;    that block.
;;; -- If TN is local, then we just check for a conflict in the block it is
;;;    local to.
;;;
(defun offset-conflicts-in-sb (tn sb offset)
  (declare (type tn tn) (type finite-sb sb) (type unsigned-byte offset))
  (let ((confs (tn-global-conflicts tn)))
    (cond
     ((eq (tn-kind tn) :environment)
      (let ((loc-live (svref (finite-sb-always-live sb) offset)))
	(do ((env-block (ir2-environment-blocks (tn-environment tn))
			(ir2-block-environment-next env-block)))
	    ((null env-block)
	     nil)
	  (when (/= (sbit loc-live (ir2-block-number env-block)) 0)
	    (return t)))))
     (confs
      (let ((loc-confs (svref (finite-sb-conflicts sb) offset))
	    (loc-live (svref (finite-sb-always-live sb) offset)))
	(do ((conf confs (global-conflicts-tn-next conf)))
	    ((null conf)
	     nil)
	  (let* ((block (global-conflicts-block conf))
		 (num (ir2-block-number block)))
	    (if (eq (global-conflicts-kind conf) :live)
		(when (/= (sbit loc-live num) 0)
		  (return t))
		(when (/= (sbit (svref loc-confs num)
				(global-conflicts-number conf))
			  0)
		  (return t)))))))
     (t
      (/= (sbit (svref (svref (finite-sb-conflicts sb) offset)
		       (ir2-block-number (tn-local tn)))
		(tn-local-number tn))
	  0)))))


;;; Conflicts-In-SC  --  Internal
;;;
;;;    Return true if TN has a conflict in SC at the specified offset.
;;;
(defun conflicts-in-sc (tn sc offset)
  (declare (type tn tn) (type sc sc) (type unsigned-byte offset))
  (let ((sb (sc-sb sc)))
    (dotimes (i (sc-element-size sc) nil)
      (when (offset-conflicts-in-sb tn sb (+ offset i))
	(return t)))))


;;; Add-Location-Conflicts  --  Internal
;;;
;;;    Add TN's conflicts into the conflicts for the location at Offset in SC.
;;; We iterate over each location in TN, adding to the conflicts for that
;;; location:
;;; -- If TN is a :Environment TN, then iterate over all the blocks in the
;;;    environment, setting all of the local conflict bits and the always-live
;;;    bit.  This records a conflict with any TN that has a LTN number in the
;;;    block, as well as with :Always-Live and :Environment TNs.
;;; -- If TN is global, then iterate over the blocks TN is live in.  In
;;;    addition to setting the always-live bit to represent the conflict with
;;;    TNs live throughout the block, we also set bits in the local conflicts.
;;;    If TN is :Always-Live in the block, we set all the bits, otherwise we or
;;;    in the local conflict bits.
;;; -- If the TN is local, then we just do the block it is local to, setting
;;;    always-live and OR'ing in the local conflicts.
;;;
(defun add-location-conflicts (tn sc offset)
  (declare (type tn tn) (type sc sc) (type unsigned-byte offset))
  (let ((confs (tn-global-conflicts tn))
	(sb (sc-sb sc)))
    (dotimes (i (sc-element-size sc))
      (let* ((this-offset (+ offset i))
	     (loc-confs (svref (finite-sb-conflicts sb) this-offset))
	     (loc-live (svref (finite-sb-always-live sb) this-offset)))
	(cond
	 ((eq (tn-kind tn) :environment)
	  (do ((env-block (ir2-environment-blocks (tn-environment tn))
			  (ir2-block-environment-next env-block)))
	      ((null env-block))
	    (let ((num (ir2-block-number env-block)))
	      (setf (sbit loc-live num) 1)
	      (set-bit-vector (svref loc-confs num)))))
	 (confs
	  (do ((conf confs (global-conflicts-tn-next conf)))
	      ((null conf))
	    (let* ((block (global-conflicts-block conf))
		   (num (ir2-block-number block))
		   (local-confs (svref loc-confs num)))
	      (setf (sbit loc-live num) 1)
	      (if (eq (global-conflicts-kind conf) :live)
		  (set-bit-vector local-confs)
		  (bit-ior local-confs (global-conflicts-conflicts conf) t)))))
	 (t
	  (let ((num (ir2-block-number (tn-local tn))))
	    (setf (sbit loc-live num) 1)
	    (bit-ior (svref loc-confs num) (tn-local-conflicts tn) t))))))))


;;; Init-SB-Vectors  --  Internal
;;;
;;;    Ensure that the conflicts vectors for each :Finite SB are large enough
;;; for the number of blocks allocated.  Also clear any old conflicts and reset
;;; the current size to the initial size.
;;;
(defun init-sb-vectors (component)
  (let ((nblocks (1+ (ir2-block-number
		      (block-info
		       (block-next
			(component-head component)))))))
    (dolist (sb *sb-list*)
      (unless (eq (sb-kind sb) :non-packed)
	(let* ((conflicts (finite-sb-conflicts sb))
	       (always-live (finite-sb-always-live sb))
	       (current-size (length (svref conflicts 0))))
	  (when (> nblocks current-size)
	    (let ((new-size (max nblocks (* current-size 2))))
	      (dotimes (i (length conflicts))
		(let ((new-vec (make-array new-size)))
		  (dotimes (j new-size)
		    (setf (svref new-vec j)
			  (make-array local-tn-limit :element-type 'bit)))
		  (setf (svref conflicts i) new-vec))
		(setf (svref always-live i)  
		      (make-array new-size :element-type 'bit)))))

	  (dotimes (i (length conflicts))
	    (let ((conf (svref conflicts i)))
	      (dotimes (j (length conf))
		(clear-bit-vector (svref conf j))))
	    (clear-bit-vector (svref always-live i))))

	(setf (finite-sb-current-size sb) (sb-size sb))
	(setf (finite-sb-last-offset sb) 0)))))


;;; Grow-SC  --  Internal
;;;
;;;    Expand the :Unbounded SB backing SC by either the initial size or the SC
;;; element size, whichever is larger.  If Needed-Size is larger, then use that
;;; size.
;;;
(defun grow-sc (sc &optional (needed-size 0))
  (declare (type sc sc))
  (let* ((sb (sc-sb sc))
	 (size (finite-sb-current-size sb))
	 (inc (max (sb-size sb) (sc-element-size sc) (- needed-size size)))
	 (new-size (+ size inc))
	 (conflicts (finite-sb-conflicts sb))
	 (block-size (length (svref conflicts 0))))
    (assert (eq (sb-kind sb) :unbounded))

    (when (> new-size (length conflicts))
      (let ((new-conf (make-array new-size)))
	(replace new-conf conflicts)
	(do ((i size (1+ i)))
	    ((= i new-size))
	  (let ((loc-confs (make-array block-size)))
	    (dotimes (j block-size)
	      (setf (svref loc-confs j)
		    (make-array local-tn-limit
				:initial-element 0
				:element-type 'bit)))
	    (setf (svref new-conf i) loc-confs)))
	(setf (finite-sb-conflicts sb) new-conf))
      
      (let ((new-live (make-array new-size)))
	(replace new-live (finite-sb-always-live sb))
	(do ((i size (1+ i)))
	    ((= i new-size))
	  (setf (svref new-live i)
		(make-array block-size
			    :initial-element 0
			    :element-type 'bit)))
	(setf (finite-sb-always-live sb) new-live))

      (let ((new-tns (make-array new-size :initial-element nil)))
	(replace new-tns (finite-sb-live-tns sb))
	(fill (finite-sb-live-tns sb) nil) 
	(setf (finite-sb-live-tns sb) new-tns)))

    (setf (finite-sb-current-size sb) new-size))
  (undefined-value))


;;; This variable is true whenever we are in pack (and thus the per-SB
;;; conflicts information is in use.)
;;;
(defvar *in-pack* nil)


;;; Pack-Before-GC-Hook  --  Internal
;;;
;;;    In order to prevent the conflict data structures from growing
;;; arbitrarily large, we clear them whenever a GC happens and we aren't
;;; currently in pack.  We revert to the initial number of locations and 0
;;; blocks.
;;;
(defun pack-before-gc-hook ()
  (unless *in-pack*
    (dolist (sb *sb-list*)
      (unless (eq (sb-kind sb) :non-packed)
	(let ((size (sb-size sb)))
	  (fill nil (finite-sb-always-live sb))
	  (setf (finite-sb-always-live sb)
		(make-array size :initial-element #*))
	  
	  (fill nil (finite-sb-conflicts sb))
	  (setf (finite-sb-conflicts sb)
		(make-array size :initial-element '#()))
	  
	  (fill nil (finite-sb-live-tns sb))
	  (setf (finite-sb-live-tns sb)
		(make-array size :initial-element nil))))))
  (undefined-value))

(pushnew 'pack-before-gc-hook ext:*before-gc-hooks*)


;;;; Cost determination:

;;; Add-Cost-Vector  --  Internal
;;;
;;;    Add the cost vector Costs into the costs for TN.  The TN cost vector may
;;; have null entries, which we leave null to represent forbidden SCs.  The
;;; Costs vector must not have any null entries for SCs allowed by the TN's
;;; primitive type.
;;;
(defun add-cost-vector (tn costs)
  (declare (type tn tn) (type sc-vector costs))
  (let ((old-costs (tn-costs tn)))
    (dolist (scn (primitive-type-scs (tn-primitive-type tn)))
      (let ((old-cost (svref old-costs scn)))
	(when old-cost
	  (setf (svref old-costs scn)
		(the cost (+ (the cost old-cost)
			     (the cost (svref costs scn))))))))))


;;; Add-Operand-Costs  --  Internal
;;;
;;;    Given a list of costs vectors, a more-operand cost vector (or NIL) and a
;;; Tn-Ref list threaded by Across, add the costs into the TN-Costs for the
;;; referenced TNs.
;;;
(defun add-operand-costs (costs more-cost refs)
  (declare (list costs) (type (or tn-ref null) refs)
	   (type (or sc-vector null) more-cost))
  (do ((ref refs (tn-ref-across ref))
       (cost costs (rest cost)))
      ((null cost)
       (do ((ref ref (tn-ref-across ref)))
	   ((null ref))
	 (add-cost-vector (tn-ref-tn ref) more-cost)))
    (add-cost-vector (tn-ref-tn ref) (first cost))))

       
;;; Compute-Costs-And-Target  --  Internal
;;;
;;;    Loop over the VOPs in Block, adding the operand-specific costs into the
;;; TN-Costs and calling any target functions.
;;;
(defun compute-costs-and-target (block)
  (declare (type ir2-block block))
  (do ((vop (ir2-block-start-vop block) (vop-next vop)))
      ((null vop))
    (let ((info (vop-info vop)))
      (when (eq (vop-info-save-p info) t)
	(do-live-tns (tn (vop-save-set vop) block)
	  (add-cost-vector tn *save-costs*)
	  (add-cost-vector tn *restore-costs*)))

      (add-operand-costs (vop-info-arg-costs info)
			 (vop-info-more-arg-costs info)
			 (vop-args vop))
      (add-operand-costs (vop-info-result-costs info)
			 (vop-info-more-result-costs info)
			 (vop-results vop))
      
      (let ((target-fun (vop-info-target-function info)))
	(when target-fun
	  (funcall target-fun vop))))))


;;;; Register saving:

;;; Orignal-TN  --  Internal
;;;
;;;    If a save TN, return the saved TN, otherwise return TN.  Useful for
;;; getting the conflicts of a TN that might be a save TN.
;;;
(defun original-tn (tn)
  (declare (type tn tn))
  (if (member (tn-kind tn) '(:save :save-once))
      (tn-save-tn tn)
      tn))

#|
    (setf (tn-local res) (tn-local tn))
    (bit-vector-replace (tn-local-conflicts res) (tn-local-conflicts tn))
    (setf (tn-local-number res) (tn-local-number tn))
    (setf (tn-global-conflicts res) (tn-global-conflicts tn))
|#

;;; Pack-Save-TN  --  Internal
;;;
;;;    Make a save TN for TN, pack it, and return it.  We copy various conflict
;;; information from the TN so that pack does the right thing.
;;;    
(defun pack-save-tn (tn)
  (declare (type tn tn))
  (let ((res (make-tn 0 :save (tn-primitive-type tn) nil))
	(sc (svref *save-scs* (sc-number (tn-sc tn)))))
    (setf (tn-save-tn tn) res)
    (setf (tn-save-tn res) tn)
    (setf (svref (tn-costs res) (sc-number sc)) 0)
    (pack-tn res)
    res))


;;; Save-Complex-Writer-TN  --  Internal
;;;
;;;    For TNs that have other than one writer, we save the TN before each
;;; call.
;;;
(defun save-complex-writer-tn (tn vop)
  (let ((save (or (tn-save-tn tn)
		  (pack-save-tn tn)))
	(node (vop-node vop))
	(block (vop-block vop))
	(next (vop-next vop)))
    (emit-move-template node block
			(template-or-lose 'save-reg)
			tn save
			vop)
    (emit-move-template node block
			(template-or-lose 'restore-reg)
			save tn
			next)))


;;; Save-Single-Writer-TN  --  Internal
;;;
;;;    For TNs that have a single writer, we save the TN at the writer, and
;;; only restore after the call.
;;;
(defun save-single-writer-tn (tn vop)
  (let* ((old-save (tn-save-tn tn))
	 (save (or old-save (pack-save-tn tn))))

    (unless old-save
      (let ((writer (tn-ref-vop (tn-writes tn))))
	(emit-move-template (vop-node writer) (vop-block writer)
			    (template-or-lose 'save-reg)
			    tn save
			    (vop-next writer)))
      (setf (tn-kind save) :save-once))

    (emit-move-template (vop-node vop) (vop-block vop)
			(template-or-lose 'restore-reg)
			save tn
			(vop-next vop)))

  (undefined-value))


;;; Emit-Saves  --  Internal
;;;
;;;    Scan over the VOPs in Block, emiting saving code for TNs noted in the
;;; codegen info that are packed into saved SCs.
;;;
(defun emit-saves (block)
  (declare (type ir2-block block))
  (do ((vop (ir2-block-start-vop block) (vop-next vop)))
      ((null vop))
    (when (eq (vop-info-save-p (vop-info vop)) t)
      (do-live-tns (tn (vop-save-set vop) block)
	(when (svref *save-scs* (sc-number (tn-sc tn)))
	  (let ((writes (tn-writes tn))
		(save (tn-save-tn tn)))
	    (if (or (and save (eq (tn-kind save) :save-once))
		    (and writes (null (tn-ref-next writes))))
		(save-single-writer-tn tn vop)
		(save-complex-writer-tn tn vop)))))))

  (undefined-value))


;;;; Targeting:

;;; Target-If-Desirable  --  Internal
;;;
;;;    Link the TN-Refs Read and Write together using the TN-Ref-Target when
;;; this seems like a good idea.  Our current criterion is that the referenced
;;; TNs not conflict.  This is called by VOP target functions.
;;;
(defun target-if-desirable (read write)
  (declare (type tn-ref read write))
  (let ((rtn (tn-ref-tn read))
	(wtn (tn-ref-tn write)))
    (when (or (eq (tn-kind rtn) :constant)
	      (not (tns-conflict rtn wtn)))
      (setf (tn-ref-target read) write)
      (setf (tn-ref-target write) read))))


;;; Check-OK-Target  --  Internal
;;;
;;;    If TN can be packed into SC so as to honor a preference to Target, then
;;; return the offset to pack at, otherwise return NIL.  Target must be already
;;; packed.  We can honor a preference if:
;;; -- Target's location is in SC's locations.
;;; -- The element sizes of the two SCs are the same.
;;; -- TN doesn't conflict with target's location.
;;; 
(defun check-ok-target (target tn sc)
  (declare (type tn target tn)
	   (type sc sc))
  (let* ((loc (tn-offset target))
	 (target-sc (tn-sc target))
	 (target-sb (sc-sb target-sc)))
    (if (and (eq target-sb (sc-sb sc))
	     (or (eq (sb-kind target-sb) :unbounded)
		 (member loc (sc-locations sc)))
	     (= (sc-element-size target-sc) (sc-element-size sc))
	     (not (conflicts-in-sc tn sc loc)))
	loc
	nil)))


;;; Find-OK-Target-Offset  --  Internal
;;;
;;;    Scan along the target path from TN, looking at readers or writers.  When
;;; we find a packed TN, return Check-OK-Target of that TN.  If there is no
;;; target, or if the TN has multiple readers (writers), then we return NIL.
;;; We also always return NIL after 10 iterations to get around potential
;;; circularity problems.
;;;
(macrolet ((frob (slot)
	     `(let ((count 10)
		    (current tn))
		(loop
		  (let ((refs (,slot current)))
		    (unless (and (plusp count) refs (not (tn-ref-next refs)))
		      (return nil))
		    (let ((target (tn-ref-target refs)))
		      (unless target (return nil))
		      (setq current (tn-ref-tn target))
		      (when (tn-offset current)
			(return (check-ok-target current tn sc)))
		      (decf count)))))))
  (defun find-ok-target-offset (tn sc)
    (declare (type tn tn) (type sc sc))
    (or (frob tn-reads)
	(frob tn-writes))))



;;;; Location selection:

;;; Select-Location  --  Internal
;;;
;;;    Select some location for TN in SC, returning the offset if we succeed,
;;; and NIL if we fail.  We start scanning at the Last-Offset in an attempt
;;; to distribute the TNs across all storage.
;;;
;;; We call Offset-Conflicts-In-SB directly, rather than using Conflicts-In-SC.
;;; This allows us to more efficient in packing multi-location TNs: we don't
;;; have to multiply the number of tests by the TN size.  This falls out
;;; natually, since we have to be aware of TN size anyway so that we don't call
;;; Conflicts-In-SC on a bogus offset.
;;;
;;; We give up on finding a location after our current pointer has wrapped
;;; twice.  This will result in testing some locations twice in the case that
;;; we fail, but is simpler than trying to figure out the soonest failure
;;; point.
;;;
;;; ### Note that we actually try to pack as many consecutive TNs as possible
;;; in the same location, since we start scanning at the same offset that the
;;; last TN was successfully packed in.  This is a weakening of the scattering
;;; hueristic that was put in to prevent restricted VOP temps from hogging all
;;; of the registers.  This way, all of these temps probably end up in one
;;; register.
;;;
(defun select-location (tn sc)
  (declare (type tn tn) (type sc sc))
  (let* ((sb (sc-sb sc))
	 (element-size (sc-element-size sc))
	 (size (finite-sb-current-size sb))
	 (start-offset (finite-sb-last-offset sb)))
    (let ((current-start start-offset)
	  (wrap-p nil))
      (loop
	(when (> (+ current-start element-size) size)
	  (cond (wrap-p (return nil))
		(t
		 (setq current-start 0)
		 (setq wrap-p t))))

	(if (or (eq (sb-kind sb) :unbounded)
		(member current-start (sc-locations sc)))
	    (dotimes (i element-size
			(progn
			  (when (and (eq (sb-name sb) 'stack)
				     (= current-start 0))
			    (error "Baz!  Just selected OLD-CONT: ~S ~S."
				   tn sc))
			  (return-from select-location current-start)))
	      (let ((offset (+ current-start i)))
		(when (offset-conflicts-in-sb tn sb offset)
		  (setq current-start (1+ offset))
		  (return))))
	    (incf current-start))))))


;;; Find-Best-SC  --  Internal
;;;
;;;    Return the SC with lowest cost for TN, based on the TN-Costs.
;;;
(defun find-best-sc (tn)
  (declare (type tn tn))
  (let ((costs (tn-costs tn))
	(best-cost most-positive-cost)
	(best-scn nil))
    (dolist (scn (primitive-type-scs (tn-primitive-type tn)))
      (let ((cost (svref costs scn)))
	(when (and cost (< (the cost cost) best-cost))
	  (setq best-cost cost  best-scn scn))))
    (assert best-scn () "No legal SCS?")
    (svref *sc-numbers* best-scn)))




;;;; Load TN packing:


;;; These variables indicate the last location at which we computed the
;;; Live-TNs.  They hold the Block and VOP values that were passed to
;;; Compute-Live-TNs.
;;;
(defvar *live-block*)
(defvar *live-vop*)


;;; Init-Live-TNs  --  Internal
;;;
;;;    Set the Live-TNs vectors in all :Finite SBs to represent the TNs live at
;;; the end of Block.
;;;
(defun init-live-tns (block)
  (dolist (sb *sb-list*)
    (when (eq (sb-kind sb) :finite)
      (fill (finite-sb-live-tns sb) nil)))

  (let ((live (ir2-block-live-in block)))  
    (do ((conf (ir2-block-global-tns block) (global-conflicts-next conf)))
	((null conf))
      (when (or (eq (global-conflicts-kind conf) :live)
		(/= (sbit live (global-conflicts-number conf)) 0))
	(let* ((tn (global-conflicts-tn conf))
	       (sb (sc-sb (tn-sc tn))))
	  (when (eq (sb-kind sb) :finite)
	    (setf (svref (finite-sb-live-tns sb) (tn-offset tn)) tn))))))

  (setq *live-block* block)
  (setq *live-vop* (ir2-block-last-vop block))

  (undefined-value))


;;; Compute-Live-TNs  --  Internal
;;;
;;;    Set the Live-TNs in :Finite SBs to represent the TNs live immediately
;;; after the evaluation of VOP in Block.  If VOP is null, then compute the
;;; live TNs at the beginning of the block.  Sequential calls on the same block
;;; must be in reverse VOP order.
;;;
(defun compute-live-tns (block vop)
  (unless (eq block *live-block*)
    (init-live-tns block))
  
  (do ((current *live-vop* (vop-prev current)))
      ((eq current vop))
    (do ((ref (vop-refs current) (tn-ref-next-ref ref)))
	((null ref))
      (let* ((tn (tn-ref-tn ref))
	     (sb (sc-sb (tn-sc tn))))
	(when (eq (sb-kind sb) :finite)
	  (let ((tns (finite-sb-live-tns sb))
		(offset (tn-offset tn)))
	    (if (tn-ref-write-p ref)
		(setf (svref tns offset) nil)
		(let ((old (svref tns offset)))
		  (assert (or (null old) (eq old tn)) (old tn))
		  (setf (svref tns offset) tn))))))))

  (setq *live-vop* vop)
  (undefined-value))


;;; Find-Operand-Costs  --  Internal
;;;
;;;    Return the Costs vector representing the operand-specific costs for the
;;; operand OP.
;;;
(defun find-operand-costs (op)
  (declare (type tn-ref op))
  (let* ((write-p (tn-ref-write-p op))
	 (vop (tn-ref-vop op))
	 (info (vop-info vop)))
    (do ((ops (if write-p (vop-results vop) (vop-args vop))
	      (tn-ref-across ops))
	 (costs (if write-p
		    (vop-info-result-costs info)
		    (vop-info-arg-costs info))
		(cdr costs)))
	((eq ops op)
	 (car costs)))))


;;; Find-Load-SC  --  Internal
;;;
;;;    Return the lowest cost SC for operand that is allowed by SCs.  If there
;;; is no legal SC, then return NIL.
;;;
;;; [### The minimization is gratuitous, since legal SCs always have a cost of
;;; 0.  Maybe this shouldn't be the case?]
;;;
(defun find-load-sc (scs op)
  (declare (type sc-bit-vector scs) (type tn-ref op))
  (let ((costs (find-operand-costs op))
	(tn (tn-ref-tn op))
	(best-cost most-positive-cost)
	(best-sc nil))
    (dolist (scn (primitive-type-scs (tn-primitive-type tn)))
      (let ((cost (svref costs scn))
	    (sc (svref *sc-numbers* scn)))
	(when (and cost (< (the cost cost) best-cost)
		   (eq (sb-kind (sc-sb sc)) :finite)
		   (/= (sbit scs scn) 0))
	  (setq best-cost cost  best-sc sc))))
    best-sc))


;;; Load-TN-Conflicts-In-SB  --  Internal
;;;
;;;    Kind of like Offset-Conflicts-In-SB, except that it uses the Live-TNs
;;; (must already be computed) and the VOP refs to determine whether a Load-TN
;;; for OP could be packed in the specified location.  There is a conflict if
;;; either:
;;;  1] Live-TNs is non-null for that location.  For result, this means
;;;     that the location has a live non-load TN in it after the VOP.  For an
;;;     argument, this means that the location as a live non-load TN before the
;;;     VOP.
;;;  2] The reference is a result, and the same location is either:
;;;     -- Used in a write (other than by OP) any time after the first result
;;;        write (inclusive).
;;;     -- Used in a read after OP (exclusive).
;;;  3] The reference is an argument, and the same location is either:
;;;     -- Used in a read (other than by OP) any time before the last argument
;;;        (inclusive).
;;;     -- Used in a write before the reference (exclusive).
;;;
;;;    In 2 (and 3) above, the first bullet corresponds to a conflict with a
;;; result (argument).  Only load-TNs should hit this test, since original
;;; operands will be in the live-TNs.
;;;
;;;    In 2 and 3 above, the second bullet corresponds to a conflict with a
;;; temporary.  Note that this time interval overlaps with the previous case:
;;; during the overlap, any reference causes a conflict.
;;;
(defun load-tn-conflicts-in-sb (op sb offset)
  (assert (eq (sb-kind sb) :finite))
  (or (svref (finite-sb-live-tns sb) offset)
      (let ((vop (tn-ref-vop op)))
	(macrolet ((frob (first end on-match)
		     `(let ((end ,end))
			(do ((ref ,first (tn-ref-next-ref ref))
			     (before-op nil))
			    ((eq ref end)
			     nil)
			  (let ((tn (tn-ref-tn ref)))
			    (cond ((eq ref op)
				   (setq before-op t))
				  ((and (eq (sc-sb (tn-sc tn)) sb)
					(eql (tn-offset tn) offset))
				   ,on-match)))))))
	  (if (tn-ref-write-p op)
	      (frob (vop-refs vop)
		    (tn-ref-next-ref (vop-results vop))
		    (if (tn-ref-write-p ref)
			(return t)
			(unless before-op
			  (return t))))
	      (frob (do ((ref (vop-args vop) (tn-ref-across ref))
			 (prev nil ref))
			((null ref) prev))
		    nil
		    (if (tn-ref-write-p ref)
			(when before-op
			  (return t))
			(return t))))))))


;;; Find-Load-TN-Target  --  Internal
;;;
;;;    If a load-TN for Op is targeted to a legal location in SC, then return
;;; the offset, otherwise return NIL.  We see if the target of the operand is
;;; packed, and try that location.  There isn't any need to chain down the
;;; target path, since everything is packed now.
;;;
(defun find-load-tn-target (op sc)
  (let ((target (tn-ref-target op)))
    (when target
      (let* ((tn (tn-ref-tn target))
	     (loc (tn-offset tn))
	     (sb (sc-sb (tn-sc tn))))
	(if (and (eq (sc-sb sc) sb)
		 (member loc (sc-locations sc))
		 (not (load-tn-conflicts-in-sb op sb loc)))
	    loc
	    nil)))))


;;; Select-Load-Tn-Location  --  Internal
;;;
;;;    Select a legal location for a load TN for Op in SC.  We just iterate
;;; over the SCs locations.  If we can't find a legal location, return NIL.
;;;
(defun select-load-tn-location (op sc)
  (declare (type tn-ref op) (type sc sc))
  (assert (= (sc-element-size sc) 1))
  (let ((sb (sc-sb sc)))
    (dolist (loc (sc-locations sc) nil)
      (unless (load-tn-conflicts-in-sb op sb loc)
	(return loc)))))


;;; Failed-To-Pack-Load-TN-Error  --  Internal
;;;
;;;    If load TN packing fails, try to give a helpful error message.  We find
;;; which operand is losing, and flame if there is no way the restriction could
;;; ever be satisfied.
;;;
(defun failed-to-pack-load-tn-error (scs op)
  (declare (type sc-bit-vector scs) (type tn-ref op))
  (let* ((vop (tn-ref-vop op))
	 (write-p (tn-ref-write-p op))
	 (tn (tn-ref-tn op))
	 (pos (1+ (or (position-in #'tn-ref-across op
				   (if write-p
				       (vop-results vop)
				       (vop-args vop)))
		      (error "Couldn't find ~S in its VOP!" op)))))

    (if (dolist (scn (primitive-type-scs (tn-primitive-type tn)) nil)
	  (when (/= (sbit scs scn) 0)
	    (return t)))
	(error "Failed to satisfy SC restrictions for ~:R ~
	        ~:[argument to~;result of~]~%~S." pos write-p vop)
	(error "No intersection between primitive-type SCs and restriction ~
	        for ~:R ~:[argument to~;result of~]~%~S." pos write-p
		(template-name (vop-info vop)))))
  (undefined-value))


(defevent spill-tn "Spilled a TN to satisfy operand SC restriction.")

;;; Spill-And-Pack-Load-TN  --  Internal
;;;
;;;     Handle the case of Pack-Load-TN where there isn't any location free
;;; that we can pack into.  What we do is spill some live TN to memory, and
;;; then pack the load TN in the freed location.
;;;
;;;     We iterate over the feasible SCs in the same way as Pack-Load-TN, but
;;; when we find any location in a feasible SC that isn't in use within the
;;; VOP, we spill the TN in that location.  There must be some TN live in every
;;; feasible location, since normal load TN packing failed.
;;;
;;;     Spilling is done using the same mechanism as register saving.
;;;
(defun spill-and-pack-load-tn (scs op)
  (declare (type sc-bit-vector scs) (type tn-ref op))
  (let ((tn (tn-ref-tn op))
	(vop (tn-ref-vop op))
	(ok-scs scs))
    (event spill-tn (vop-node vop))
    (loop
      (let* ((sc (or (find-load-sc ok-scs op)
		     (failed-to-pack-load-tn-error scs op)))
	     (sb (sc-sb sc)))

	(dolist (loc (sc-locations sc))
	  (when (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
		    ((null ref) t)
		  (let ((tn (tn-ref-tn ref)))
		    (when (and (eq (sc-sb (tn-sc tn)) sb)
			       (eql (tn-offset tn) loc))
		      (return nil))))

	    (let ((victim (svref (finite-sb-live-tns sb) loc)))
	      (assert victim)
	      (save-complex-writer-tn victim vop))

	    (let ((res (make-tn 0 :load (tn-primitive-type tn) sc)))
	      (setf (tn-offset res) loc)
	      (return-from spill-and-pack-load-tn res))))

	(setq ok-scs (bit-vector-copy ok-scs))
	(setf (sbit ok-scs (sc-number sc)) 0)))))


;;; Pack-Load-TN  --  Internal
;;;
;;;    Loop over the possible load SCs in order of desirability, trying to find
;;; a location to pack a Load-TN for Op into.  If we run out of SCs, then we
;;; let Spill-And-Pack-Load-TN do its thing.  We return the packed load TN.
;;;
(defun pack-load-tn (scs op)
  (declare (type sc-bit-vector scs) (type tn-ref op))
  (let ((tn (tn-ref-tn op))
	(ok-scs scs))
    (loop
      (let ((sc (find-load-sc ok-scs op)))
	(cond ((not sc)
	       (return (spill-and-pack-load-tn scs op)))
	      (t
	       (let ((loc (or (find-load-tn-target op sc)
			      (select-load-tn-location op sc))))
		 (cond
		  (loc
		   (let ((res (make-tn 0 :load (tn-primitive-type tn) sc)))
		     (setf (tn-offset res) loc)
		     (return res)))
		  (t
		   (setq ok-scs (bit-vector-copy ok-scs))
		   (setf (sbit ok-scs (sc-number sc)) 0))))))))))


;;; Load-Operand  --  Internal
;;;
;;;    Emit code to load the operand Op into the specified Load-TN.
;;;
(defun load-operand (op load-tn)
  (declare (type tn-ref op) (type tn load-tn))
  (let* ((tn (tn-ref-tn op))
	 (vop (tn-ref-vop op))
	 (node (vop-node vop))
	 (block (vop-block vop)))
    (change-tn-ref-tn op load-tn)
    (if (tn-ref-write-p op)
	(emit-move-template node block
			    (template-or-lose 'store-operand)
			    load-tn tn (vop-next vop))
	(emit-move-template node block
			    (template-or-lose 'load-operand)
			    tn load-tn vop)))
  (undefined-value))


;;; Check-Operand-Restrictions  --  Internal
;;;
;;;    Scan a list of SC restriction bit-vectors and a list of TN-Refs threaded
;;; by TN-Ref-Across.  When we find a reference whoes TN doesn't satisfy the
;;; restriction, we pack a Load-TN and load the operand into it.
;;;
;;;    We compute the live TNs here so that we do it only once per VOP, and
;;; thus don't get confused when code is inserted for loading or saving of
;;; multiple operands.  That is, we don't want to scan the MOVE VOPs result
;;; saving until we are done saving all results, and don't want to scan the
;;; argument loading MOVEs until we are done loading all arguments.  This way,
;;; the live-TNs are guaranteed to represent any conflicts between load TNs
;;; and TNs that were originally operands, but were substituted for by load
;;; TNs.
;;;
(proclaim '(inline check-operand-restrictions))
(defun check-operand-restrictions (restr ops)
  (declare (list restr) (type (or tn-ref null) ops))
  (let ((computed nil))
    (do ((restr restr (cdr restr))
	 (op ops (tn-ref-across op)))
	((null restr))
      (when (zerop (sbit (car restr) (sc-number (tn-sc (tn-ref-tn op)))))
	(unless computed
	  (let ((vop (tn-ref-vop op)))
	    (compute-live-tns (vop-block vop)
			      (if (tn-ref-write-p op)
				  vop
				  (vop-prev vop))))
	  (setq computed t))
	(load-operand op (pack-load-tn (car restr) op)))))
  (undefined-value))
	

;;; Pack-Load-TNs  --  Internal
;;;
;;;    Scan the VOPs in Block, looking for operands whose SC restrictions
;;; aren't statisfied.  We do the results first, since they are evaluated
;;; later, and our conflict analysis is a backward scan.
;;;
(defun pack-load-tns (block)
  (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
      ((null vop))
    (let ((info (vop-info vop)))
      (check-operand-restrictions (vop-info-result-restrictions info)
				  (vop-results vop))
      (check-operand-restrictions (vop-info-arg-restrictions info)
				  (vop-args vop))))
  (undefined-value))


;;; Pack-TN  --  Internal
;;;
;;;    Attempt to pack TN in all possible SCs, in order of decreasing
;;; desirability (according to the costs.)
;;;
(defun pack-tn (tn)
  (declare (type tn tn))
  (loop
    (let* ((fsc (find-best-sc tn))
	   (original (original-tn tn))
	   (loc (or (find-ok-target-offset original fsc)
		    (select-location original fsc))))
      (cond (loc
	     (add-location-conflicts original fsc loc)
	     (setf (tn-sc tn) fsc)
	     (setf (tn-offset tn) loc)
	     (return))
	    ((eq (sb-kind (sc-sb fsc)) :unbounded)
	     (grow-sc fsc))
	    (t
	     (setf (svref (tn-costs tn) (sc-number fsc)) nil)))))
  (undefined-value))


(defun pack-targeting-tns (tn)
  )

;;; Pack-Wired-TN  --  Internal
;;;
;;;    Pack a wired TN, checking that the offset is in bounds for the SB, and
;;; that the TN doesn't conflict with some other TN already packed in that
;;; location.  If the TN is wired to a location beyond the end of a :Unbounded
;;; SB, then grow the SB enough to hold the TN.
;;;
(defun pack-wired-tn (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
	 (sb (sc-sb sc))
	 (offset (tn-offset tn))
	 (end (+ offset (sc-element-size sc))))
    (when (> end (finite-sb-current-size sb))
      (unless (eq (sb-kind sb) :unbounded)
	(error "~S wired to a location that is out of bounds." tn))
      (grow-sc sc end))
    (when (conflicts-in-sc tn sc offset)
      (error "~S wired to a location that it conflicts with." tn))
    (add-location-conflicts tn sc offset)))


;;; Pack  --  Interface
;;;
(defun pack (component)
  (let ((*in-pack* t))
    (init-sb-vectors component)
    
    (do-ir2-blocks (block component)
      (compute-costs-and-target block))
    
    (let ((2comp (component-info component)))
      (do ((tn (ir2-component-wired-tns 2comp) (tn-next tn)))
	  ((null tn))
	(pack-wired-tn tn))
      
      (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
	  ((null tn))
	(pack-tn tn))
      
      (do ((tn (ir2-component-normal-tns 2comp) (tn-next tn)))
	  ((null tn))
	(when (or (tn-global-conflicts tn)
		  (eq (tn-kind tn) :environment))
	  (pack-tn tn)
	  (pack-targeting-tns tn))))
    
    (let ((*live-block* nil)
	  (*live-vop* nil))
      (do-ir2-blocks (block component)
	(let ((ltns (ir2-block-local-tns block)))
	  (dotimes (i (ir2-block-local-tn-count block))
	    (let ((tn (svref ltns i)))
	      (unless (or (null tn)
			  (eq tn :more)
			  (tn-global-conflicts tn)
			  (tn-offset tn))
		(pack-tn tn)
		(pack-targeting-tns tn)))))
	
	(pack-load-tns block)
	(emit-saves block)))
    
    (undefined-value)))
