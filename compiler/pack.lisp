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
;;; the compiler.  Pack is responsible for assigning TNs to storage allocations
;;; or "register allocation".
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Conflict determination:


;;; Offset-Conflicts-In-SB  --  Internal
;;;
;;; Return true if the element at the specified offset in SB has a conflict
;;; with TN:
;;; -- If an component-live TN (:component kind), then iterate over all the
;;;    blocks.  If the element at Offset is used anywhere in any of the
;;;    environment's blocks (always-live /= 0), then there is a conflict.
;;; -- :Environment is similar to :Component, except that we iterate only over
;;;    the blocks in the environment.
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
  (let ((confs (tn-global-conflicts tn))
	(kind (tn-kind tn)))
    (cond
     ((eq kind :component)
      (let ((loc-live (svref (finite-sb-always-live sb) offset)))
	(dotimes (i (ir2-block-number
		     (block-info
		      (component-tail *compile-component*)))
		    nil)
	  (when (/= (sbit loc-live i) 0)
	    (return t)))))
     ((eq kind :environment)
      (let ((loc-live (svref (finite-sb-always-live sb) offset)))
	(do-environment-ir2-blocks (env-block (tn-environment tn) nil)
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
;;; -- If TN is a :Component TN, then iterate over all the blocks, setting
;;;    all of the local conflict bits and the always-live bit.  This records a
;;;    conflict with any TN that has a LTN number in the block, as well as with
;;;    :Always-Live and :Environment TNs.
;;; -- :Environment is similar to :Component, except that we iterate over only
;;;    the blocks in the environment.
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
	(sb (sc-sb sc))
	(kind (tn-kind tn)))
    (dotimes (i (sc-element-size sc))
      (let* ((this-offset (+ offset i))
	     (loc-confs (svref (finite-sb-conflicts sb) this-offset))
	     (loc-live (svref (finite-sb-always-live sb) this-offset)))
	(cond
	 ((eq kind :component)
	  (dotimes (num (ir2-block-number
			 (block-info
			  (component-tail *compile-component*)))
			nil)
	    (setf (sbit loc-live num) 1)
	    (set-bit-vector (svref loc-confs num))))
	 ((eq kind :environment)
	  (do-environment-ir2-blocks (env-block (tn-environment tn))
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


;;; IR2-BLOCK-COUNT  --  Internal
;;;
;;;    Return the total number of IR2 blocks in Component.
;;;
(defun ir2-block-count (component)
  (declare (type component component))
  (1+ (ir2-block-number
       (block-info
	(block-next
	 (component-head component))))))


;;; Init-SB-Vectors  --  Internal
;;;
;;;    Ensure that the conflicts vectors for each :Finite SB are large enough
;;; for the number of blocks allocated.  Also clear any old conflicts and reset
;;; the current size to the initial size.
;;;
(defun init-sb-vectors (component)
  (let ((nblocks (ir2-block-count component)))
    (dolist (sb *sb-list*)
      (unless (eq (sb-kind sb) :non-packed)
	(let ((conflicts (finite-sb-conflicts sb))
	      (always-live (finite-sb-always-live sb))
	      (max-locs (length conflicts)))
	  (unless (zerop max-locs)
	    (let ((current-size (length (svref conflicts 0))))
	      (when (> nblocks current-size)
		(let ((new-size (max nblocks (* current-size 2))))
		  (dotimes (i (length conflicts))
		    (let ((new-vec (make-array new-size)))
		      (dotimes (j new-size)
			(setf (svref new-vec j)
			      (make-array local-tn-limit :element-type 'bit)))
		      (setf (svref conflicts i) new-vec))
		    (setf (svref always-live i)  
			  (make-array new-size :element-type 'bit))))))
	    
	    (dotimes (i (length conflicts))
	      (let ((conf (svref conflicts i)))
		(dotimes (j (length conf))
		  (clear-bit-vector (svref conf j))))
	      (clear-bit-vector (svref always-live i)))))

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
	 (block-size (if (zerop (length conflicts))
			 (ir2-block-count *compile-component*)
			 (length (svref conflicts 0)))))
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


;;;; Internal errors:

;;; NO-LOAD-FUNCTION-ERROR  --  Internal
;;;
;;;    Give someone a hard time because there isn't any load function defined
;;; to move from Src to Dest.
;;;
(defun no-load-function-error (src dest)
  (let* ((src-sc (tn-sc src))
	 (src-name (sc-name src-sc))
	 (dest-sc (tn-sc dest))
	 (dest-name (sc-name dest-sc)))
    (cond ((eq (sb-kind (sc-sb src-sc)) :non-packed)
	   (unless (member src-sc (sc-constant-scs dest-sc))
	     (error "Loading from an invalid constant SC?~@
	             VM definition inconsistent, try recompiling."))
	   (error "No load function defined to load SC ~S ~
	           from its constant SC ~S."
		  dest-name src-name))
	  ((member src-sc (sc-alternate-scs dest-sc))
	   (error "No load function defined to load SC ~S from its ~
	           alternate SC ~S."
		  dest-name src-name))
	  ((member dest-sc (sc-alternate-scs src-sc))
	   (error "No load function defined to save SC ~S in its ~
	           alternate SC ~S."
		  src-name dest-name))
	  (t
	   (error "Loading to/from SCs that aren't alternates?~@
	           VM definition inconsistent, try recompiling.")))))


;;; FAILED-TO-PACK-ERROR  --  Internal
;;;
;;;    Called when we failed to pack TN.  If Restricted is true, then we we
;;; restricted to pack TN in its SC. 
;;;
(defun failed-to-pack-error (tn restricted)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
	 (scs (cons sc (sc-alternate-scs sc))))
    (cond
     (restricted
      (error "Failed to pack restricted TN ~S in its SC ~S."
	     tn (sc-name sc)))
     (t
      (assert (not (find :unbounded scs
			 :key #'(lambda (x) (sb-kind (sc-sb x))))))
      (let ((ptype (tn-primitive-type tn)))
	(cond
	 (ptype
	  (assert (member (sc-number sc) (primitive-type-scs ptype)))
	  (error "SC ~S doesn't have any :Unbounded alternate SCs, but is~@
	          a SC for primitive-type ~S."
		 (sc-name sc) (primitive-type-name ptype)))
	 (t
	  (error "SC ~S doesn't have any :Unbounded alternate SCs."
		 (sc-name sc))))))))
  (undefined-value))


;;; Failed-To-Pack-Load-TN-Error  --  Internal
;;;
;;;    If load TN packing fails, try to give a helpful error message.  We find
;;; which operand is losing, and flame if there is no way the restriction could
;;; ever be satisfied.
;;;
(defun failed-to-pack-load-tn-error (op)
  (declare (type tn-ref op))
  (multiple-value-bind (arg-p n more-p costs load-scs incon)
		       (get-operand-info op)
    (declare (ignore costs))
    (assert (not more-p))
    (let ((load-sc (svref *sc-numbers*
			  (svref load-scs
				 (sc-number
				  (tn-sc (tn-ref-tn op)))))))
      (assert load-sc)
      (error "Unable to pack a Load-TN in SC ~S for the ~:R ~
              ~:[result~;argument~] to~@
              the ~S VOP.~@
	      Perhaps all SC elements already in use by VOP?~:[~;~@
	      Current cost info inconsistent with that in effect at compile ~
	      time.  Recompile.~%Compilation order may be incorrect.~]"
	     (sc-name load-sc)
	     n arg-p
	     (vop-info-name (vop-info (tn-ref-vop op)))
	     incon))))


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


;;; Pack-Save-TN  --  Internal
;;;
;;;    Make a save TN for TN, pack it, and return it.  We copy various conflict
;;; information from the TN so that pack does the right thing.
;;;    
(defun pack-save-tn (tn)
  (declare (type tn tn))
  (let ((res (make-tn 0 :save nil nil)))
    (dolist (alt (sc-alternate-scs (tn-sc tn))
		 (error "No unbounded alternate for SC ~S."
			(sc-name (tn-sc tn))))
      (when (eq (sb-kind (sc-sb alt)) :unbounded)
	(setf (tn-save-tn tn) res)
	(setf (tn-save-tn res) tn)
	(setf (tn-sc res) alt)
	(pack-tn res t)
	(return res)))))


;;; EMIT-OPERAND-LOAD  --  Internal
;;;
;;;    Find the load function for moving from Src to Dest and emit a
;;; MOVE-OPERAND VOP with that function as its info arg.
;;;
(defun emit-operand-load (node block src dest after)
  (declare (type node node) (type ir2-block block)
	   (type tn src dest) (type (or vop null) after))
  (emit-load-template node block
		      (template-or-lose 'move-operand)
		      src dest
		      (list (or (svref (sc-load-functions (tn-sc dest))
				       (sc-number (tn-sc src)))
				(no-load-function-error src dest)))
		      after)
  (undefined-value))

  
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
    (emit-operand-load node block tn save vop)
    (emit-operand-load node block save tn next)))


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
	(emit-operand-load (vop-node writer) (vop-block writer)
			    tn save (vop-next writer)))
      (setf (tn-kind save) :save-once))

    (emit-operand-load (vop-node vop) (vop-block vop) save tn (vop-next vop)))

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
	(when (sc-save-p (tn-sc tn))
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
;;; this seems like a good idea.  Currently we always do, as this increases the
;;; sucess of load-TN targeting.
;;;
(defun target-if-desirable (read write)
  (declare (type tn-ref read write))
  (setf (tn-ref-target read) write)
  (setf (tn-ref-target write) read))


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
  (declare (type tn target tn) (type sc sc) (inline member))
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
  (declare (type tn tn) (type sc sc) (inline member))
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
			(return-from select-location current-start))
	      (let ((offset (+ current-start i)))
		(when (offset-conflicts-in-sb tn sb offset)
		  (setq current-start (1+ offset))
		  (return))))
	    (incf current-start))))))


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

  (do-live-tns (tn (ir2-block-live-in block) block)
    (let ((sb (sc-sb (tn-sc tn))))
      (when (eq (sb-kind sb) :finite)
	(setf (svref (finite-sb-live-tns sb) (tn-offset tn)) tn))))

  (setq *live-block* block)
  (setq *live-vop* (ir2-block-last-vop block))

  (undefined-value))


;;; Compute-Live-TNs  --  Internal
;;;
;;;    Set the Live-TNs in :Finite SBs to represent the TNs live immediately
;;; after the evaluation of VOP in Block, excluding results of the VOP.  If VOP
;;; is null, then compute the live TNs at the beginning of the block.
;;; Sequential calls on the same block must be in reverse VOP order.
;;;
(defun compute-live-tns (block vop)
  (declare (type ir2-block block) (type vop vop))
  (unless (eq block *live-block*)
    (init-live-tns block))
  
  (do ((current *live-vop* (vop-prev current)))
      ((eq current vop)
       (do ((res (vop-results vop) (tn-ref-across res)))
	   ((null res))
	 (let* ((tn (tn-ref-tn res))
		(sb (sc-sb (tn-sc tn))))
	   (when (eq (sb-kind sb) :finite)
	     (setf (svref (finite-sb-live-tns sb) (tn-offset tn))
		   nil)))))
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


;;; Load-TN-Conflicts-In-SB  --  Internal
;;;
;;;    Kind of like Offset-Conflicts-In-SB, except that it uses the Live-TNs
;;; (must already be computed) and the VOP refs to determine whether a Load-TN
;;; for OP could be packed in the specified location.  There is a conflict if
;;; either:
;;;  1] Live-TNs is non-null for that location.  This means that there is a
;;;     live non-load TN in that location after the VOP.
;;;  2] The reference is a result, and the same location is either:
;;;     -- Used by some other result.
;;;     -- Used in any way after the reference (exclusive).
;;;  3] The reference is an argument, and the same location is either:
;;;     -- Used by some other argument.
;;;     -- Used in any way before the reference (exclusive).
;;;
;;;    In 2 (and 3) above, the first bullet corresponds to result-result
;;; (and argument-argument) conflicts.  We need this case because there aren't
;;; any TN-REFs to represent the implicit reading of results or writing of
;;; arguments.
;;;
;;;    In 2 and 3 above, the second bullet corresponds conflicts with
;;; temporaries or between arguments and results.
;;;
;;;    In 2 and 3 above, we consider both the TN-REF-TN and the TN-REF-LOAD-TN
;;; (if any) to be referenced simultaneously and in the same way.  This causes
;;; load-TNs to appear live to the beginning (or end) of the VOP, as
;;; appropriate.
;;;
(defun load-tn-conflicts-in-sb (op sb offset)
  (assert (eq (sb-kind sb) :finite))
  (or (svref (finite-sb-live-tns sb) offset)
      (let ((vop (tn-ref-vop op)))
	(macrolet ((same (ref)
		     `(let ((tn (tn-ref-tn ,ref))
			    (ltn (tn-ref-load-tn ,ref)))
			(or (and (eq (sc-sb (tn-sc tn)) sb)
				 (eql (tn-offset tn) offset))
			    (and ltn
				 (eq (sc-sb (tn-sc ltn)) sb)
				 (eql (tn-offset ltn) offset)))))
		   (is-op (ops)
		     `(do ((ops ,ops (tn-ref-across ops)))
			  ((null ops) nil)
			(when (and (same ops)
				   (not (eq ops op)))
			  (return t))))
		   (is-ref (refs end)
		     `(do ((refs ,refs (tn-ref-next-ref refs)))
			  ((eq refs ,end) nil)
			(when (same refs) (return t)))))
	  
	  (if (tn-ref-write-p op)
	      (or (is-op (vop-results vop))
		  (is-ref (vop-refs vop) op))
	      (or (is-op (vop-args vop))
		  (is-ref (tn-ref-next-ref op) nil)))))))


;;; Find-Load-TN-Target  --  Internal
;;;
;;;    If a load-TN for Op is targeted to a legal location in SC, then return
;;; the offset, otherwise return NIL.  We see if the target of the operand is
;;; packed, and try that location.  There isn't any need to chain down the
;;; target path, since everything is packed now.
;;;
(defun find-load-tn-target (op sc)
  (declare (inline member))
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
  (unless (= (sc-element-size sc) 1)
    (error "Can't have a load-TN with SC element size /= 1."))
  (let ((sb (sc-sb sc)))
    (dolist (loc (sc-locations sc) nil)
      (unless (load-tn-conflicts-in-sb op sb loc)
	(return loc)))))


(defevent spill-conditional-arg-tn
	  "Spilled a TN that was an arg to a :CONDITIONAL VOP.")

;;; SPILL-CONDITIONAL-ARG-TN  --  Internal
;;;
;;;    Fix things up when we spill a TN for loading of an argument to a
;;; conditional VOP.  We have to insert a block on the branch target path that
;;; restores the spilled value.  In addition to inserting a block in the IR1
;;; flow graph, we must also insert an IR2 block into the emit order and frob
;;; the assembly level control flow by emitting or modifying branches.
;;;
;;;    We change the conditional's target label to be the new block's label.
;;; We insert the new block in the emit order immediately after the conditional
;;; block.  In order to do this, we must insert a branch at the end of the
;;; conditional block if it currently drops through.
;;;
(defun spill-conditional-arg-tn (victim vop)
  (declare (type tn tn) (type vop vop))
  (let* ((info-args (vop-codegen-info vop))
	 (lab (first info-args))
	 (node (vop-node vop))
	 (2block (vop-block vop))
	 (block (ir2-block-block 2block))
	 (succ (find lab (block-succ block) :key #'block-label))
	 (new (insert-cleanup-code block succ node
				   "<conditional spill hack>"))
	 (new-2block (make-ir2-block new)))
    (event spill-conditional-arg-tn node)
    (setf (block-info new) new-2block)
    (setf (first info-args) (block-label new))
    (emit-operand-load node new-2block (tn-save-tn victim) victim nil)
    (vop branch node new-2block lab)
    
    (let ((next-lab (block-label (ir2-block-block (ir2-block-next 2block)))))
      (add-to-emit-order new-2block 2block)
      (unless (eq (vop-info-name (ir2-block-last-vop 2block)) 'branch)
	(vop branch node 2block next-lab)))
    (undefined-value)))


(defevent spill-tn "Spilled a TN to satisfy operand SC restriction.")

;;; Spill-And-Pack-Load-TN  --  Internal
;;;
;;;     Handle the case of Pack-Load-TN where there isn't any location free
;;; that we can pack into.  What we do is spill some live TN to memory, and
;;; then pack the load TN in the freed location.
;;;
;;; When we find any location in SC that isn't in use within the VOP, we spill
;;; the TN in that location.  There must be some TN live in every location,
;;; since normal load TN packing failed.
;;;
;;;     Spilling is done using the same mechanism as register saving.
;;;
(defun spill-and-pack-load-tn (sc op)
  (declare (type sc sc) (type tn-ref op))
  (let ((vop (tn-ref-vop op))
	(sb (sc-sb sc)))
    (event spill-tn (vop-node vop))

    (dolist (loc (sc-locations sc)
		 (failed-to-pack-load-tn-error op))
      (when (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
		((null ref) t)
	      (let ((op (tn-ref-tn ref)))
		(when (and (eq (sc-sb (tn-sc op)) sb)
			   (eql (tn-offset op) loc))
		  (return nil))))
	
	(let ((victim (svref (finite-sb-live-tns sb) loc)))
	  (assert victim)
	  (save-complex-writer-tn victim vop)
	  (when (eq (template-result-types (vop-info vop)) :conditional)
	    (spill-conditional-arg-tn victim vop)))
	
	(let ((res (make-tn 0 :load nil sc)))
	  (setf (tn-offset res) loc)
	  (return res))))))


;;; Pack-Load-TN  --  Internal
;;;
;;;    Try to pack a load TN in the sc indicated by SCs.  If this fails, then
;;; we let Spill-And-Pack-Load-TN do its thing.  We return the packed load TN.
;;;
(defun pack-load-tn (scs op)
  (declare (type sc-vector scs) (type tn-ref op))
  (let ((vop (tn-ref-vop op)))
    (compute-live-tns (vop-block vop) vop))
  
  (let* ((sc (svref *sc-numbers*
		    (svref scs (sc-number (tn-sc (tn-ref-tn op))))))
	 (loc (or (find-load-tn-target op sc)
		  (select-load-tn-location op sc))))
    (if loc
	(let ((res (make-tn 0 :load nil sc)))
	  (setf (tn-offset res) loc)
	  res)
	(spill-and-pack-load-tn sc op))))


;;; Check-Operand-Restrictions  --  Internal
;;;
;;;    Scan a list of load-SCs vectors and a list of TN-Refs threaded by
;;; TN-Ref-Across.  When we find a reference whose TN doesn't satisfy the
;;; restriction, we pack a Load-TN and load the operand into it.
;;;
(proclaim '(inline check-operand-restrictions))
(defun check-operand-restrictions (scs ops)
  (declare (list scs) (type (or tn-ref null) ops))
  (do ((scs scs (cdr scs))
       (op ops (tn-ref-across op)))
      ((null scs))
    (let ((ref-scn (sc-number (tn-sc (tn-ref-tn op)))))
      (unless (eql (svref (car scs) ref-scn) ref-scn)
	(setf (tn-ref-load-tn op) (pack-load-tn (car scs) op)))))
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
      (check-operand-restrictions (vop-info-result-load-scs info)
				  (vop-results vop))
      (check-operand-restrictions (vop-info-arg-load-scs info)
				  (vop-args vop))))
  (undefined-value))


;;; Pack-TN  --  Internal
;;;
;;;    Attempt to pack TN in all possible SCs, in order of decreasing
;;; desirability (according to the costs.)  If Restricted, then we can only
;;; pack in TN-SC, not in any Alternate-SCs.
;;;
(defun pack-tn (tn restricted)
  (declare (type tn tn))
  (let* ((original (original-tn tn))
	 (fsc (tn-sc tn))
	 (alternates (unless restricted (sc-alternate-scs fsc))))
    (do ((sc fsc (pop alternates)))
	((null sc)
	 (failed-to-pack-error tn restricted))
      (let ((loc (or (find-ok-target-offset original sc)
		     (select-location original sc)
		     (when (eq (sb-kind (sc-sb sc)) :unbounded)
		       (grow-sc sc)
		       (or (select-location original sc)
			   (error "Failed to pack after growing SC?"))))))
	(when loc
	  (add-location-conflicts original sc loc)
	  (setf (tn-sc tn) sc)
	  (setf (tn-offset tn) loc)
	  (return)))))
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
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	  ((null vop))
	(let ((target-fun (vop-info-target-function (vop-info vop))))
	  (when target-fun
	    (funcall target-fun vop)))))

    (let ((2comp (component-info component)))
      (do ((tn (ir2-component-wired-tns 2comp) (tn-next tn)))
	  ((null tn))
	(pack-wired-tn tn))
      
      (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
	  ((null tn))
	(pack-tn tn t))
      
      (do ((tn (ir2-component-normal-tns 2comp) (tn-next tn)))
	  ((null tn))
	(unless (and (eq (tn-kind tn) :normal)
		     (not (tn-global-conflicts tn)))
	  (pack-tn tn nil)
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
		(pack-tn tn nil)
		(pack-targeting-tns tn)))))
	
	(pack-load-tns block)
	(emit-saves block)))
    
    (undefined-value)))
