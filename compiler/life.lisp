;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the lifetime analysis phase in the compiler.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Utilities:

;;; Add-Global-Conflict  --  Internal
;;;
;;;    Link in a global-conflicts structure for TN in Block with Number as the
;;; LTN number.  The conflict is inserted in the per-TN Global-Conflicts thread
;;; after the TN's Current-Conflict.  We change the Current-Conflict to point
;;; to the new conflict.  Since we scan the blocks in reverse DFO, this list is
;;; automatically built in order.  We have to actually scan the current
;;; Global-TNs for the block in order to keep that thread sorted.
;;;
(defun add-global-conflict (kind tn block number)
  (declare (type (member :read :write :read-only :live) kind)
	   (type tn tn) (type ir2-block block)
	   (type (or local-tn-number null) number))
  (let ((new (make-global-conflicts kind tn block number)))
    (let ((last (tn-current-conflict tn)))
      (if last
	  (shiftf (global-conflicts-tn-next new)
		  (global-conflicts-tn-next last)
		  new)
	  (shiftf (global-conflicts-tn-next new)
		  (tn-global-conflicts tn)
		  new)))
    (setf (tn-current-conflict tn) new)

    (let ((global-num (tn-number tn)))
      (do ((prev nil conf)
	   (conf (ir2-block-global-tns block)
		 (global-conflicts-next conf)))
	  ((or (null conf)
	       (> (tn-number (global-conflicts-tn conf)) global-num))
	   (if prev
	       (setf (global-conflicts-next prev) new)
	       (setf (ir2-block-global-tns block) new))
	   (setf (global-conflicts-next new) conf)))))
  (undefined-value))


;;; Reset-Current-Conflict  --  Internal
;;;
;;;    Reset the Current-Conflict slot in all packed TNs to point to the head
;;; of the Global-Conflicts thread.
;;;
(defun reset-current-conflict (component)
  (do-packed-tns (tn component)
    (setf (tn-current-conflict tn) (tn-global-conflicts tn))))


;;;; Pre-pass:

;;; Convert-To-Global  --  Internal
;;;
;;;    Convert TN (currently local) to be a global TN, since we discovered that
;;; it is referenced in more than one block.  We just add a global-conflicts
;;; structure with a kind derived from the Kill and Live sets.
;;;
(defun convert-to-global (tn)
  (declare (type tn tn))
  (let ((block (tn-local tn))
	(num (tn-local-number tn)))
    (add-global-conflict
     (if (zerop (sbit (ir2-block-written block) num))
	 :read-only
	 (if (zerop (sbit (ir2-block-live-out block) num))
	     :write
	     :read))
     tn block num))
  (undefined-value))


;;; Find-Local-References  --  Internal
;;;
;;;    Scan all references to packed TNs in block.  We assign LTN numbers to
;;; each referenced TN, and also build the Kill and Live sets that summarize
;;; the references to each TN for purposes of lifetime analysis.
;;;
;;;    It is possible that we will run out of LTN numbers.  If this happens,
;;; then we return the VOP that we were processing at the time we ran out,
;;; otherwise we return NIL.
;;;
;;;    If a TN is referenced in more than one block, then we must represent
;;; references using Global-Conflicts structures.  When we first see a TN, we
;;; assume it will be local.  If we see a reference later on in a different
;;; block, then we go back and fix the TN to global.
;;;
;;;    We must globalize TNs that have a block other than the current one in
;;; their Local slot and have no Global-Conflicts.  The latter condition is
;;; necessary because we always set Local and Local-Number when we process a
;;; reference to a TN, even when the TN is already known to be global.
;;;
;;;    When we see reference to global TNs during the scan, we add the
;;; global-conflict as :Read-Only, since we don't know the corrent kind until
;;; we are done scanning the block.
;;;
(defun find-local-references (block)
  (declare (type ir2-block block))
  (let ((kill (ir2-block-written block))
	(live (ir2-block-live-out block))
	(tns (ir2-block-local-tns block)))
    (let ((ltn-num (ir2-block-local-tn-count block)))
      (do ((vop (ir2-block-last-vop block)
		(vop-prev vop)))
	  ((null vop))
	(do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
	    ((null ref))
	  (let* ((tn (tn-ref-tn ref))
		 (local (tn-local tn))
		 (kind (tn-kind tn)))
	    (unless (or (eq kind :constant)
			(eq kind :environment))
	      (unless (eq local block)
		(when (= ltn-num local-tn-limit)
		  (return-from find-local-references vop))
		(when local
		  (unless (tn-global-conflicts tn)
		    (convert-to-global tn))
		  (add-global-conflict :read-only tn block ltn-num))
		
		(setf (tn-local tn) block)
		(setf (tn-local-number tn) ltn-num)
		(setf (svref tns ltn-num) tn)
		(incf ltn-num))
	      
	      (let ((num (tn-local-number tn)))
		(if (tn-ref-write-p ref)
		    (setf (sbit kill num) 1  (sbit live num) 0)
		    (setf (sbit live num) 1)))))))
      
      (setf (ir2-block-local-tn-count block) ltn-num)))
  nil)


;;; Init-Global-Conflict-Kind   --  Internal
;;;
;;;    Finish up the global conflicts for TNs referenced in Block according to
;;; the local Kill and Live sets.
;;;
;;;    We set the kind for TNs already in the global-TNs.  If not written at
;;; all, then is :Read-Only, the default.  Must have been referenced somehow,
;;; or we wouldn't have conflicts for it.
;;;
;;;    We also iterate over all the local TNs, looking for TNs local to this
;;; block that are still live at the block beginning, and thus must be global.
;;; This case is only important when a TN is read in a block but not written in
;;; any other, since otherwise the write would promote the TN to global.  But
;;; this does happen with various passing-location TNs that are magically
;;; written.  This also serves to propagate the lives of erroneously
;;; uninitialized TNs so that consistency checks can detect them.
;;;
(defun init-global-conflict-kind (block)
  (declare (type ir2-block block))
  (let ((live (ir2-block-live-out block)))
    (let ((kill (ir2-block-written block)))
      (do ((conf (ir2-block-global-tns block)
		 (global-conflicts-next conf)))
	  ((null conf))
	(let ((num (global-conflicts-number conf)))
	  (unless (zerop (sbit kill num))
	    (setf (global-conflicts-kind conf)
		  (if (zerop (sbit live num))
		      :write
		      :read))))))
    
    (let ((ltns (ir2-block-local-tns block)))
      (dotimes (i (ir2-block-local-tn-count block))
	(let ((tn (svref ltns i)))
	  (unless (or (eq tn :more)
		      (tn-global-conflicts tn)
		      (zerop (sbit live i)))
	    (convert-to-global tn)))))))
  
  (undefined-value))


(defevent split-ir2-block "Split an IR2 block to meet Local-TN-Limit.")

;;; Split-IR2-Blocks  --  Internal
;;;
;;;    Move the code after the VOP Lose in 2block into its own block.  The
;;; block is linked into the emit order following 2block.  Number is the block
;;; number assigned to the new block.  We return the new block.
;;;
(defun split-ir2-blocks (2block lose number)
  (declare (type ir2-block 2block) (type vop lose)
	   (type unsigned-byte number))
  (event split-ir2-block (vop-node lose))
  (let ((new (make-ir2-block (ir2-block-block 2block)))
	(new-start (vop-next lose)))
    (setf (ir2-block-number new) number)
    (add-to-emit-order new 2block)

    (do ((vop new-start (vop-next vop)))
	((null vop))
      (setf (vop-block vop) new))
    
    (setf (ir2-block-start-vop new) new-start)
    (shiftf (ir2-block-last-vop new) (ir2-block-last-vop 2block) lose)

    (setf (vop-next lose) nil)
    (setf (vop-prev new-start) nil)

    new))


;;; Clear-Lifetime-Info  --  Internal
;;;
;;;    Clear the global and local conflict info in Block so that we can
;;; recompute it without any old cruft being retained.  It is assumed that all
;;; LTN numbers are in use.
;;;
;;;    First we delete all the global conflicts.  The conflict we are deleting
;;; must be the last in the TN's global-conflicts, but we must scan for it in
;;; order to find the previous conflict.
;;;
;;;    Next, we scan the local TNs, nulling out the Local slot in all TNs with
;;; no global conflicts.  This allows these TNs to be treated as local when we
;;; scan the block again.
;;;
;;;    If there are conflicts, then we set Local to one of the conflicting
;;; blocks.  This ensures that Local doesn't hold over Block as its value,
;;; causing the subsequent reanalysis to think that the TN has already been
;;; seen in that block.
;;;
;;;    This function must not be called on blocks that have :More TNs.
;;;
(defun clear-lifetime-info (block)
  (declare (type ir2-block block))
  (setf (ir2-block-local-tn-count block) 0)
  
  (do ((conf (ir2-block-global-tns block)
	     (global-conflicts-next conf)))
      ((null conf)
       (setf (ir2-block-global-tns block) nil))
    (let ((tn (global-conflicts-tn conf)))
      (assert (eq (tn-current-conflict tn) conf))
      (assert (null (global-conflicts-tn-next conf)))
      (do ((current (tn-global-conflicts tn)
		    (global-conflicts-tn-next current))
	   (prev nil current))
	  ((eq current conf)
	   (if prev
	       (setf (global-conflicts-tn-next prev) nil)
	       (setf (tn-global-conflicts tn) nil))
	   (setf (tn-current-conflict tn) prev)))))
  
  (fill (ir2-block-written block) 0)
  (let ((ltns (ir2-block-local-tns block)))
    (dotimes (i local-tn-limit)
      (let ((tn (svref ltns i)))
	(assert (not (eq tn :more)))
	(let ((conf (tn-global-conflicts tn)))
	  (setf (tn-local tn)
		(if conf
		    (global-conflicts-block conf)
		    nil)))))))
  
  (undefined-value))


;;; Coalesce-More-LTN-Numbers  --  Internal
;;;
;;;    This provides a panic mode for assigning LTN numbers when there is a VOP
;;; with so many more operands that they can't all be assigned distinct
;;; numbers.  When this happens, we recover by assigning all the more operands
;;; the same LTN number.  We can get away with this, since all more args (and
;;; results) are referenced simultaneously as far as conflict analysis is
;;; concerned.
;;;
;;;     Block is the IR2-Block that the more VOP is at the end of.  Ops is the
;;; full argument or result TN-Ref list.  Fixed is the types of the fixed
;;; operands (used only to skip those operands.)
;;;
;;;     What we do is grab a LTN number, then make a :Read-Only global conflict
;;; for each more operand TN.  We require that there be no existing global
;;; conflict in Block for any of the operands.  Since conflicts must be cleared
;;; before the first call, this only prohibits the same TN being used both as a
;;; more operand and as any other operand to the same VOP.
;;;
;;;     We don't have to worry about getting the correct conflict kind, since
;;; Init-Global-Conflict-Kind will fix things up.
;;;
;;;     We also set the Local and Local-Number slots in each TN.
;;;
(defun coalesce-more-ltn-numbers (block ops fixed)
  (declare (type ir2-block block) (type tn-ref ops) (list fixed))
  (let ((num (ir2-block-local-tn-count block)))
    (assert (< num local-tn-limit))
    (incf (ir2-block-local-tn-count block))
    (setf (svref (ir2-block-local-tns block) num) :more)

    (do ((op (do ((op ops (tn-ref-across op))
		  (i 0 (1+ i)))
		 ((= i (length fixed)) op))
	     (tn-ref-across op)))
	((null op))
      (let ((tn (tn-ref-tn op)))
	(assert
	  (flet ((frob (refs)
		   (do ((ref refs (tn-ref-next ref)))
		       ((null ref) t)
		     (when (and (eq (vop-block (tn-ref-vop ref)) block)
				(not (eq ref op)))
		       (return nil)))))
	    (and (frob (tn-reads tn)) (frob (tn-writes tn))))
	  () "More operand ~S used more than once in its VOP." op)
	(assert (not (find-in #'global-conflicts-next tn
			      (ir2-block-global-tns block)
			      :key #'global-conflicts-tn)))

	(add-global-conflict :read-only tn block num)
	(setf (tn-local tn) block)
	(setf (tn-local-number tn) num))))
  (undefined-value))


(defevent coalesce-more-ltn-numbers
  "Coalesced LTN numbers for a more operand to meet Local-TN-Limit.")

;;; Lifetime-Pre-Pass  --  Internal
;;;
;;;    Loop over the blocks in Component, assigning LTN numbers and recording
;;; TN birth and death.  The only interesting action is when we run out of
;;; local TN numbers while finding local references.
;;;
;;;    If we run out of LTN numbers while processing a VOP within the block,
;;; then we just split off the VOPs we have successfully processed into their
;;; own block.
;;;
;;;    If we run out of LTN numbers while processing the our first VOP (the
;;; last in the block), then it must be the case that this VOP has large more
;;; operands.  We split the VOP into its own block, and then call
;;; Coalesce-More-Ltn-Numbers to assign all the more args/results the same LTN
;;; number(s).
;;;
;;;    In either case, we clear the lifetime information that we computed so
;;; far, recomputing it after taking corrective action.
;;;
;;;    Whenever we split a block, we finish the pre-pass on the split-off block
;;; by doing Find-Local-References and Init-Global-Conflict-Kind.  This can't
;;; run out of LTN numbers.
;;;
(defun lifetime-pre-pass (component)
  (declare (type component component))
  (let ((counter -1))
    (do-blocks-backwards (block component)
      (let ((2block (block-info block)))
	(do ((lose (find-local-references 2block)
		   (find-local-references 2block))
	     (last-lose nil lose)
	     (coalesced nil))
	    ((not lose)
	     (init-global-conflict-kind 2block)
	     (setf (ir2-block-number 2block) (incf counter)))
	  
	  (clear-lifetime-info 2block)
	  
	  (cond
	   ((vop-next lose)
	    (assert (not (eq last-lose lose)))
	    (let ((new (split-ir2-blocks 2block lose (incf counter))))
	      (assert (not (find-local-references new)))
	      (init-global-conflict-kind new)))
	   (t
	    (assert (not (eq lose coalesced)))
	    (setq coalesced lose)
	    (event coalesce-more-ltn-numbers (vop-node lose))
	    (let ((info (vop-info lose))
		  (new (if (vop-prev lose)
			   (split-ir2-blocks 2block (vop-prev lose)
					     (incf counter))
			   2block)))
	      (coalesce-more-ltn-numbers new (vop-args lose)
					 (vop-info-arg-types info))
	      (coalesce-more-ltn-numbers new (vop-results lose)
					 (vop-info-result-types info))
	      (assert (not (find-local-references new)))
	      (init-global-conflict-kind new))))))))
		     
  (undefined-value))


;;;; Flow analysis:

;;; Propagate-Live-TNs  --  Internal
;;;
;;;    For each Global-TN in Block2 that is :Live, :Read or :Read-Only, ensure
;;; that there is a corresponding Global-Conflict in Block1.  If there is none,
;;; make a :Live Global-Conflict.  If there is a :Read-Only conflict, promote
;;; it to :Live.
;;;
;;;    If we did added a new conflict, return true, otherwise false.  We don't
;;; need to return true when we promote a :Read-Only conflict, since it doesn't
;;; reveal any new information to predecessors of Block1.
;;;
;;;    We use the Tn-Current-Conflict to walk through the global
;;; conflicts.  Since the global conflicts for a TN are ordered by block, we
;;; can be sure that the Current-Conflict always points at or before the block
;;; that we are looking at.  This allows us to quickly determine if there is a
;;; global conflict for a given TN in Block1.
;;;
;;;    When we scan down the conflicts, we know that there must be at least one
;;; conflict for TN, since we got our hands on TN by picking it out of a
;;; conflict in Block2.
;;;
;;;    We leave the Current-Conflict pointing to the conflict for Block1.  The
;;; Current-Conflict must be initialized to the head of the Global-Conflicts
;;; for the TN between each flow analysis iteration.
;;;
(defun propagate-live-tns (block1 block2)
  (declare (type ir2-block block1 block2))
  (let ((live-in (ir2-block-live-in block1))
	(did-something nil))
    (do ((conf2 (ir2-block-global-tns block2)
		(global-conflicts-next conf2)))
	((null conf2))
      (ecase (global-conflicts-kind conf2)
	((:live :read :read-only)
	 (let* ((tn (global-conflicts-tn conf2))
		(tn-conflicts (tn-current-conflict tn))
		(number1 (ir2-block-number block1)))
	   (assert tn-conflicts)
	   (do ((current tn-conflicts (global-conflicts-tn-next current))
		(prev nil current))
	       ((or (null current)
		    (> (ir2-block-number (global-conflicts-block current))
		       number1))
		(setf (tn-current-conflict tn) prev)
		(add-global-conflict :live tn block1 nil)
		(setq did-something t))
	     (when (eq (global-conflicts-block current) block1)
	       (case (global-conflicts-kind current)
		 (:live)
		 (:read-only
		  (setf (global-conflicts-kind current) :live)
		  (setf (svref (ir2-block-local-tns block1)
			       (global-conflicts-number current))
			nil)
		  (setf (global-conflicts-number current) nil)
		  (setf (tn-current-conflict tn) current))
		 (t
		  (setf (sbit live-in (global-conflicts-number current)) 1)))
	       (return)))))
	(:write)))
    did-something))

		    
;;; Lifetime-Flow-Analysis  --  Internal
;;;
;;;    Do backward global flow analysis to find all TNs live at each block
;;; boundary.
;;;
(defun lifetime-flow-analysis (component)
  (loop
    (reset-current-conflict component)
    (let ((did-something nil))
      (do-blocks-backwards (block component)
	(let* ((2block (block-info block))
	       (last (do ((b (ir2-block-next 2block) (ir2-block-next b))
			  (prev 2block b))
			 ((not (eq (ir2-block-block b) block))
			  prev))))

	  (dolist (b (block-succ block))
	    (when (and (block-lambda b)
		       (propagate-live-tns last (block-info b)))
	      (setq did-something t)))

	  (do ((b (ir2-block-prev last) (ir2-block-prev b))
	       (prev last b))
	      ((not (eq (ir2-block-block b) block)))
	    (when (propagate-live-tns b prev)
	      (setq did-something t)))))

      (unless did-something (return))))

  (undefined-value))


;;;; Post-pass:

;;; Convert-To-Environment-TN  --  Internal
;;;
;;;    If TN isn't already a :Environment TN, then make it into one.  This
;;; requires deleting the existing conflict info.
;;;
(defun convert-to-environment-tn (tn)
  (declare (type tn tn))
  (ecase (tn-kind tn)
    (:environment)
    (:normal
     (let ((confs (tn-global-conflicts tn)))
       (if confs
	   (do ((conf confs (global-conflicts-tn-next conf)))
	       ((null conf))
	     (let ((block (global-conflicts-block conf)))
	       (unless (eq (global-conflicts-kind conf) :live)
		 (let ((ltns (ir2-block-local-tns block))
		       (num (global-conflicts-number conf)))
		   (assert (not (eq (svref ltns num) :more)))
		   (setf (svref ltns num) nil)))
	       (deletef-in global-conflicts-next (ir2-block-global-tns block)
			   conf)))
	   (setf (svref (ir2-block-local-tns (tn-local tn))
			(tn-local-number tn))
		 nil))
       (setf (tn-local tn) nil)
       (setf (tn-local-number tn) nil)
       (setf (tn-global-conflicts tn) nil)
       (setf (tn-kind tn) :environment)
       (push tn (ir2-environment-live-tns (tn-environment tn))))))
  (undefined-value))


;;; Note-Conflicts  --  Internal
;;;
;;;    Note that TN conflicts with all current live TNs.  Num is TN's LTN
;;; number.  We bit-ior Live-Bits with TN's Local-Conflicts, and set TN's
;;; number in the conflicts of all TNs in Live-List.
;;;
(defun note-conflicts (live-bits live-list tn num)
  (declare (type tn tn) (type (or tn null) live-list)
	   (type local-tn-bit-vector live-bits)
	   (type local-tn-number num))
  (let ((lconf (tn-local-conflicts tn)))
    (bit-ior live-bits lconf lconf))
  (do ((live live-list (tn-next* live)))
      ((null live))
    (setf (sbit (tn-local-conflicts live) num) 1))
  (undefined-value))


;;; Compute-Save-Set  --  Internal
;;;
;;;    Compute a list of the TNs live after VOP that aren't results.
;;;
(defun compute-save-set (vop block live-list)
  (declare (type vop vop) (type ir2-block block) (type (or tn null) live-list))
  (collect ((save))
    (let ((results (vop-results vop)))
      (do ((live live-list (tn-next* live)))
	  ((null live))
	(unless (find-in #'tn-ref-across live results :key #'tn-ref-tn)
	  (save live))))
    (do ((conf (ir2-block-global-tns block) (global-conflicts-next conf)))
	((null conf))
      (when (eq (global-conflicts-kind conf) :live)
	(save (global-conflicts-tn conf))))
    (save)))


;;; Compute-Initial-Conflicts  --  Internal
;;;
;;;    Return as values, a LTN bit-vector and a list (threaded by TN-Next*)
;;; representing the TNs live at the end of Block (exclusive of :Live TNs).
;;;
;;; We iterate over the TNs in the global conflicts that are live at the block
;;; end, setting up the TN-Local-Conflicts and TN-Local-Number, and adding the
;;; TN to the live list.
;;;
;;; ### Note: we alias the global-conflicts-conflicts here as the
;;; tn-local-conflicts.
;;;
(defun compute-initial-conflicts (block)
  (declare (type ir2-block block))
  (let ((live-bits (copy-seq (ir2-block-live-in block)))
	(live-list nil))

    (do ((conf (ir2-block-global-tns block)
	       (global-conflicts-next conf)))
	((null conf))
      (let ((bits (global-conflicts-conflicts conf))
	    (tn (global-conflicts-tn conf))
	    (num (global-conflicts-number conf)))
	(setf (tn-local-number tn) num)
	(unless (eq (global-conflicts-kind conf) :live)
	  (unless (zerop (sbit live-bits num))
	    (bit-vector-replace bits live-bits)
	    (setf (sbit bits num) 0)
	    (push-in tn-next* tn live-list))
	  (setf (tn-local-conflicts tn) bits))))

    (values live-bits live-list)))


(eval-when (compile eval)

;;; Frob-More-TNs  --  Internal
;;;
;;;    Used in the guts of Conflict-Analyze-1-Block to simultaneously do
;;; something to all of the TNs referenced by a big more arg.  We have to treat
;;; these TNs specially, since when we set or clear the bit in the live TNs,
;;; the represents a change in the liveness of all the more TNs.  If we
;;; iterated as normal, the next more ref would be thought to be not live when
;;; it was, etc.  We return true if there where more TNs.
;;;
(defmacro frob-more-tns (action)
  `(when (eq (svref ltns num) :more)
     (do ((mref (tn-ref-next-ref ref) (tn-ref-next-ref mref)))
	 ((null mref))
       (let ((mtn (tn-ref-tn mref)))
	 (unless (eql (tn-local-number mtn) num)
	   (return))
	 ,action))
     t))

); Eval-When (Compile Eval)


;;; Conflict-Analyze-1-Block  --  Internal
;;;
;;;    Compute the block-local conflict information for Block.  We iterate over
;;; all the TN-Refs in a block in reference order, maintaining the set of live
;;; TNs in both a list and a bit-vector representation.
;;;
(defun conflict-analyze-1-block (block)
  (declare (type ir2-block block))
  (multiple-value-bind
      (live-bits live-list)
      (compute-initial-conflicts block)
    (let ((ltns (ir2-block-local-tns block)))

      (do ((vop (ir2-block-last-vop block)
		(vop-prev vop)))
	  ((null vop))

	(let ((save-p (vop-info-save-p (vop-info vop))))
	  (when save-p
	    (let ((save-set (compute-save-set vop block live-list)))
	      (ecase save-p
		(:force-to-stack
		 (dolist (tn save-set)
		   (force-tn-to-stack tn)
		   (convert-to-environment-tn tn)))
		((t :compute-only)
		 (setf (first (vop-codegen-info vop)) save-set))))))

	(do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
	    ((null ref))
	  (let* ((tn (tn-ref-tn ref))
		 (num (tn-local-number tn)))
	      
	    (cond
	     ((not num))
	     ((not (zerop (sbit live-bits num)))
	      (when (tn-ref-write-p ref)
		(setf (sbit live-bits num) 0)
		(deletef-in tn-next* live-list tn)
		(when (frob-more-tns (deletef-in tn-next* live-list mtn))
		  (return))))
	     ((tn-ref-write-p ref)
	      (note-conflicts live-bits live-list tn num))
	     (t
              (note-conflicts live-bits live-list tn num)
	      (frob-more-tns (note-conflicts live-bits live-list mtn num))
	      (setf (sbit live-bits num) 1)
	      (push-in tn-next* tn live-list)
	      (when (frob-more-tns (push-in tn-next* mtn live-list))
		(return))))))))))


;;; Lifetime-Post-Pass  --  Internal
;;;
;;;    Conflict analyze each block, and also add it 
(defun lifetime-post-pass (component)
  (declare (type component component))
  (do-ir2-blocks (block component)
    (conflict-analyze-1-block block)))


;;; Lifetime-Analyze  --  Interface
;;;
;;;
(defun lifetime-analyze (component)
  (lifetime-pre-pass component)
  (lifetime-flow-analysis component)
  (lifetime-post-pass component))


;;;; Conflict testing:

;;; TNs-Conflict-Local-Global  --  Internal
;;;
;;;    Test for a conflict between the local TN X and the global TN Y.  We just
;;; look for a global conflict of Y in X's block, and then test for conflict in
;;; that block.
;;; [### Might be more efficient to scan Y's global conflicts.  This depends on
;;; whether there are more global TNs than blocks.]
;;;
(defun tns-conflict-local-global (x y)
  (let ((block (tn-local x)))
    (do ((conf (ir2-block-global-tns block)
	       (global-conflicts-next conf)))
	((null conf) nil)
      (when (eq (global-conflicts-tn conf) y)
	(let ((num (global-conflicts-number conf)))
	  (return (or (not num)
		      (not (zerop (sbit (tn-local-conflicts x)
					num))))))))))


;;; TNs-Conflict-Global-Global  --  Internal
;;;
;;;    Test for conflict between two global TNs X and Y.
;;;
(defun tns-conflict-global-global (x y)
  (declare (type tn x y))
  (let* ((x-conf (tn-global-conflicts x))
	 (x-num (ir2-block-number (global-conflicts-block x-conf)))
	 (y-conf (tn-global-conflicts y))
	 (y-num (ir2-block-number (global-conflicts-block y-conf))))

    (macrolet ((advance (n c)
		 `(progn
		    (setq ,c (global-conflicts-tn-next ,c))
		    (unless ,c (return-from tns-conflict-global-global nil))
		    (setq ,n (ir2-block-number (global-conflicts-block ,c)))))
	       (scan (g l lc)
		 `(do ()
		      ((>= ,g ,l))
		    (advance ,l ,lc))))

      (loop
	;; x-conf, y-conf true, x-num, y-num corresponding block numbers.
	(scan x-num y-num y-conf)
	(scan y-num x-num x-conf)
	(when (= x-num y-num)
	  (let ((ltn-num-x (global-conflicts-number x-conf)))
	    (unless (and ltn-num-x
			 (global-conflicts-number y-conf)
			 (zerop (sbit (global-conflicts-conflicts y-conf)
				      ltn-num-x)))
	      (return t))
	    (advance x-num x-conf)
	    (advance y-num y-conf)))))))


;;; TNs-Conflict-Environment-Global  --  Interface
;;;
;;;    Return true if any of Y's blocks are in X's environment.
;;;
(defun tns-conflict-environment-global (x y)
  (declare (type tn x y))
  (let ((env (tn-environment x)))
    (do ((conf (tn-global-conflicts y) (global-conflicts-tn-next conf)))
	((null conf)
	 nil)
      (when (eq (environment-info
		 (lambda-environment
		  (block-lambda
		   (ir2-block-block (global-conflicts-block conf)))))
		env)
	(return t)))))


;;; TNs-Conflict-Environment-Local  --  Interface
;;;
;;;    Return true if Y's block is in X's environment.
;;;
(defun tns-conflict-environment-local (x y)
  (declare (type tn x y))
  (eq (environment-info
       (lambda-environment
	(block-lambda
	 (ir2-block-block (tn-local y)))))
      (tn-environment x)))


;;; TNs-Conflict  --  Interface
;;;
;;;    Return true if the lifetimes of X and Y overlap at any point.
;;;
(defun tns-conflict (x y)
  (declare (type tn x y))
  (cond ((eq (tn-kind x) :environment)
	 (cond ((tn-global-conflicts y)
		(tns-conflict-environment-global x y))
	       ((eq (tn-kind y) :environment)
		(eq (tn-environment x) (tn-environment y)))
	       (t
		(tns-conflict-environment-local x y))))
	((eq (tn-kind y) :environment)
	 (if (tn-global-conflicts x)
	     (tns-conflict-environment-global y x)
	     (tns-conflict-environment-local y x)))
	((tn-global-conflicts x)
	 (if (tn-global-conflicts y)
	     (tns-conflict-global-global x y)
	     (tns-conflict-local-global y x)))
	((tn-global-conflicts y)
	 (tns-conflict-local-global x y))
	(t
	 (and (eq (tn-local x) (tn-local y))
	      (not (zerop (sbit (tn-local-conflicts x)
				(tn-local-number y))))))))
