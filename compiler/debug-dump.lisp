;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains stuff that creates debugger information from the
;;; compiler's internal data structures.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(defvar *byte-buffer*
  (make-array 10 :element-type '(unsigned-byte 8)
	      :fill-pointer 0  :adjustable t))


;;;; Debug blocks:

(deftype location-kind ()
  '(member :unknown-return :known-return :internal-error :non-local-exit
	   :block-start))


;;; The Location-Info structure holds the information what we need about
;;; locations which code generation decided were "interesting".
;;;
(defstruct (location-info
	    (:constructor make-location-info (kind label vop)))
  ;;
  ;; The kind of location noted.
  (kind nil :type location-kind)
  ;;
  ;; The label pointing to the interesting code location.
  (label nil :type label)
  ;;
  ;; The VOP that emitted this location (for node, save-set, ir2-block, etc.)
  (vop nil :type vop))


;;; NOTE-DEBUG-LOCATION  --  Interface
;;;
;;;    Called during code generation in places where there is an "interesting"
;;; location: some place where we are likely to end up in the debugger, and
;;; thus want debug info.
;;;
(defun note-vop-debug-location (vop label kind)
  (declare (type vop vop) (type label label) (type location-kind kind))
  (setf (ir2-block-locations (vop-block vop))
	(nconc (ir2-block-locations (vop-block vop))
	       (list (make-location-info kind label vop))))
  (undefined-value))


;;; IR2-BLOCK-ENVIRONMENT  --  Interface
;;;
(proclaim '(inline ir2-block-environment))
(defun ir2-block-environment (2block)
  (declare (type ir2-block 2block))
  (lambda-environment (block-lambda (ir2-block-block 2block))))


;;; COMPUTE-LIVE-VARS  --  Internal
;;;
;;;    Given a local conflicts vector and an IR2 block to represent the set of
;;; live TNs, and the Var-Locs hashtable representing the variables dumped,
;;; compute a bit-vector representing the set of live variables.
;;;
(defun compute-live-vars (live block var-locs)
  (declare (type ir2-block block) (type local-tn-bit-vector live)
	   (type hash-table var-locs))
  (let ((res (make-array (logandc2 (+ (hash-table-count var-locs) 7) #xFF)
			 :element-type 'bit
			 :initial-element 0)))
    (do-live-tns (tn live block)
      (let ((leaf (tn-leaf tn)))
	(when (lambda-var-p leaf)
	  (let ((num (gethash leaf var-locs)))
	    (when num
	      (setf (sbit res num) 1))))))
    res))


;;; The PC for the location most recently dumped.
;;;
(defvar *previous-location*)

;;; DUMP-1-LOCATION  --  Internal
;;;
;;;    Dump a compiled debug-location into *BYTE-BUFFER* that describes the
;;; code/source map and live info.
;;;
(defun dump-1-location (node block kind tlf-num label live var-locs)
  (declare (type node node) (type ir2-block block)
	   (type local-tn-bit-vector live) (type label label)
	   (type location-kind kind) (type (or index null) tlf-num)
	   (type hash-table var-locs))
  
  (vector-push-extend
   (dpb (position kind compiled-location-kinds) compiled-location-kind-byte 0)
   *byte-buffer*)
  
  (let ((loc (label-location label)))
    (write-var-integer (- loc *previous-location*) *byte-buffer*)
    (setq *previous-location* loc))
  
  (unless tlf-num
    (write-var-integer (node-tlf-number node) *byte-buffer*))
  (write-var-integer (first (node-source-path node)) *byte-buffer*)
  
  (write-packed-bit-vector (compute-live-vars live block var-locs)
			   *byte-buffer*)
  
  (undefined-value))


;;; DUMP-LOCATION-FROM-INFO  --  Internal
;;;
;;;    Extract context info from a Location-Info structure and use it to dump a
;;; compiled code-location.
;;;
(defun dump-location-from-info (loc tlf-num var-locs)
  (declare (type location-info loc) (type (or index null) tlf-num)
	   (type hash-table var-locs))
  (let ((vop (location-info-vop loc)))
    (dump-1-location (vop-node vop)
		     (vop-block vop)
		     (location-info-kind loc)
		     tlf-num
		     (location-info-label loc)
		     (vop-save-set vop)
		     var-locs))
  (undefined-value))


;;; COMPUTE-DEBUG-BLOCKS  --  Internal
;;;
;;;    Return a vector and an integer (or null) suitable for use as the BLOCKS
;;; and TLF-NUMBER in Fun's debug-function.  This requires three passes to
;;; compute:
;;; -- Scan all the blocks, caching the block numbering in the BLOCK-FLAG and
;;;    determining if all locations are in the same TLF.
;;; -- Scan all blocks, dumping the header and successors followed by all the
;;;    non-elsewhere locations.
;;; -- Dump the elsewhere block header and all the elsewhere locations.
;;;
(defun compute-debug-blocks (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (setf (fill-pointer *byte-buffer*) 0)
  (let ((*previous-location* 0)
	(tlf-num (node-tlf-number (lambda-bind fun))))
    (let ((num 0))
      (do-environment-ir2-blocks (2block (lambda-environment fun))
	(let ((block (ir2-block-block 2block)))
	  (when (eq (block-info block) 2block)
	    (setf (block-flag block) num)
	    (incf num)
	    (unless (eql (node-tlf-number
			  (continuation-next (block-start block)))
			 tlf-num)
	      (setq tlf-num nil)))
	  
	  (dolist (loc (ir2-block-locations 2block))
	    (unless (eql (node-tlf-number (vop-node (location-info-vop loc)))
			 tlf-num)
	      (setq tlf-num nil))))))

    (collect ((elsewhere))
      (do-environment-ir2-blocks (2block (lambda-environment fun))
	(let ((block (ir2-block-block 2block)))
	  (when (eq (block-info block) 2block)
	    (let ((succ (block-succ block)))
	      (vector-push-extend
	       (dpb (length succ) compiled-debug-block-nsucc-byte 0)
	       *byte-buffer*)
	      (dolist (b succ)
		(write-var-integer (block-flag b) *byte-buffer*)))
	    (dump-1-location (continuation-next (block-start block))
			     2block :block-start tlf-num
			     (ir2-block-%label 2block)
			     (ir2-block-live-out 2block)
			     var-locs))
	  
	  (dolist (loc (ir2-block-locations 2block))
	    (if (label-elsewhere-p (location-info-label loc))
		(elsewhere loc)
		(dump-location-from-info loc tlf-num var-locs)))))
      
      (vector-push-extend compiler-debug-block-elsewhere-p *byte-buffer*)
      (dolist (loc (elsewhere))
	(dump-location-from-info loc tlf-num)))

    (values (copy-seq *byte-buffer*) tlf-num)))


;;; DEBUG-SOURCE-FOR-INFO  --  Interface
;;;
;;;    Return a list of DEBUG-SOURCE structures containing information derived
;;; from Info.
;;;
(defun debug-source-for-info (info)
  (declare (type source-info info))
  (assert (not (source-info-current-file info)))
  (mapcar #'(lambda (x)
	      (let ((name (file-info-name x))
		    (res (make-debug-source
			  :from :file
			  :created (file-info-write-date x)
			  :compiled (source-info-start-time info)
			  :source-root (file-info-source-root x)
			  :start-positions
			  (when (policy nil (>= debug 2))
			    (coerce-to-smallest-eltype
			     (file-info-positions x))))))
		(cond ((pathnamep name)
		       (setf (debug-source-name res) name))
		      (t
		       (setf (debug-source-from res) name)
		       (when (eq name :lisp)
			 (setf (debug-source-name res)
			       (cadr (aref (file-info-forms x) 0))))))
		res))
	  (source-info-files info)))


;;; COERCE-TO-SMALLEST-ELTYPE  --  Internal
;;;
;;;    Given an arbirtary sequence, coerce it to an unsigned vector if
;;; possible.
;;;
(defun coerce-to-smallest-eltype (seq)
  (let ((max 0))
    (macrolet ((frob ()
		 '(if (and (integerp val) (>= val 0) max)
		      (when (> val max)
			(setq max val))
		      (setq max nil))))
      (if (listp seq)
	  (dolist (val seq)
	    (frob))
	  (dotimes (i (length seq))
	    (let ((val (aref seq i)))
	      (frob)))))
    
    (if max
	(coerce seq `(vector (integer 0 ,max)))
	(coerce seq 'simple-vector))))


;;;; Locations:

;;; TN-SC-OFFSET  --  Internal
;;;
;;;    Return a SC-OFFSET describing TN's location.
;;;
(defun tn-sc-offset (tn)
  (declare (type tn tn))
  (make-sc-offset (sc-number (tn-sc tn))
		  (tn-offset tn)))


;;; DUMP-1-VARIABLE  --  Internal
;;;
;;;    Dump info to represent Var's location being TN.  ID is an integer that
;;; makes Var's name unique in the function.  Buffer is the vector we stick the
;;; result in.
;;;
(defun dump-1-variable (var tn id buffer)
  (declare (type lambda-var var) (type tn tn) (type unsigned-byte id))
  (let* ((name (leaf-name var))
	 (package (symbol-package name))
	 (package-p (and package (not (eq package *package*))))
	 (save-tn (tn-save-tn tn))
	 (flags 0))
    (unless package
      (setq flags (logior flags compiled-location-uninterned)))
    (when package-p
      (setq flags (logior flags compiled-location-packaged)))
    (when (eq (tn-kind tn) :environment)
      (setq flags (logior flags compiled-location-environment-live)))
    (when save-tn
      (setq flags (logior flags compiled-location-save-loc-p)))
    (unless (zerop id)
      (setq flags (logior flags compiled-location-id-p)))
    (vector-push-extend flags buffer)
    (write-var-string (symbol-name name) buffer)
    (when package-p
      (write-var-string (package-name package) buffer))
    (unless (zerop id)
      (write-var-integer id buffer))
    (write-var-integer (tn-sc-offset tn) buffer)
    (when save-tn
      (write-var-integer (tn-sc-offset save-tn) buffer)))
  (undefined-value))


;;; COMPUTE-VARIABLES  --  Internal
;;;
;;;    Return a vector suitable for use as the DEBUG-FUNCTION-VARIABLES of Fun.
;;; Level is the current DEBUG-INFO quality.  Var-Locs is a hashtable in which
;;; we enter the translation from LAMBDA-VARS to the relative position of that
;;; variable's location in the resulting vector.
;;;
(defun compute-variables (fun level var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((vars))
    (labels ((frob-leaf (leaf tn gensym-p)
	       (let ((name (leaf-name leaf)))
		 (when (and name (leaf-refs leaf)
			    (or gensym-p (symbol-package name)))
		   (vars (cons leaf tn)))))
	     (frob-lambda (x gensym-p)
	       (dolist (leaf (lambda-vars x))
		 (frob-leaf leaf (leaf-info leaf) gensym-p))))
      (frob-lambda fun t)
      (when (>= level 2)
	(dolist (x (ir2-environment-environment
		    (environment-info (lambda-environment fun))))
	  (let ((thing (car x)))
	    (when (lambda-var-p thing)
	      (frob-leaf thing (cdr x) (= level 3)))))
	
	(dolist (let (lambda-lets fun))
	  (frob-lambda let (= level 3)))))
    
    (setf (fill-pointer *byte-buffer*) 0)
    (let ((sorted (sort (vars) #'string<
			:key #'(lambda (x)
				 (symbol-name (leaf-name (car x))))))
	  (prev-name nil)
	  (id 0)
	  (i 0))
      (declare (type (or simple-string null) prev-name))
      (dolist (x sorted)
	(let* ((var (car x))
	       (name (symbol-name (leaf-name var))))
	  (cond ((and prev-name (string= prev-name name))
		 (incf id))
		(t
		 (setq id 0  prev-name name)))
	  (dump-1-variable var (cdr x) id *byte-buffer*)
	  (setf (gethash var var-locs) i))
	(incf i)))

    (copy-seq *byte-buffer*)))


;;; DEBUG-LOCATION-FOR  --  Internal
;;;
;;;    Return Var's relative position in the function's variables (determined
;;; from the Var-Locs hashtable.)
;;;
(defun debug-location-for (var var-locs)
  (declare (type lambda-var var) (type hash-table var-locs))
  (let ((res (gethash var var-locs)))
    (assert res () "No location for ~S?" var)
    res))


;;;; Arguments/returns:

;;; COMPUTE-ARGUMENTS  --  Internal
;;;
;;;    Return a vector to be used as the COMPILED-DEBUG-FUNCTION-ARGUMENTS for
;;; Fun.  If fun is the MAIN-ENTRY for an optional dispatch, then look at the
;;; ARGLIST to determine the syntax, otherwise pretend all arguments are fixed.
;;;
;;; ### This assumption breaks down in EPs other than the main-entry, since
;;; they may or may not have supplied-p vars, etc.
;;;
(defun compute-arguments (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((res))
    (let ((od (lambda-optional-dispatch fun)))
      (if (and od (eq (optional-dispatch-main-entry od) fun))
	  (let ((actual-vars (lambda-vars fun)))
	    (dolist (arg (optional-dispatch-arglist od))
	      (let ((info (lambda-var-arg-info arg))
		    (actual (pop actual-vars)))
		(cond (info
		       (case (arg-info-kind info)
			 (:keyword
			  (res (arg-info-keyword info)))
			 (:rest
			  (res 'rest-arg)))
		       (res (debug-location-for actual var-locs))
		       (when (arg-info-supplied-p info)
			 (res 'supplied-p)
			 (res (debug-location-for (pop actual-vars) var-locs))))
		      (t
		       (res (debug-location-for actual var-locs)))))))
	  (dolist (var (lambda-vars fun))
	    (res (debug-location-for var var-locs)))))

    (coerce-to-smallest-eltype (res))))


;;; COMPUTE-DEBUG-RETURNS  --  Internal
;;;
;;;    Return a list of COMPILED-LOCATION structures for the fixed-values
;;; return from Fun.
;;;
(defun compute-debug-returns (fun)
  (coerce-to-smallest-eltype 
   (mapcar #'(lambda (loc)
	       (tn-sc-offset loc))
	   (return-info-locations (tail-set-info (lambda-tail-set fun))))))


;;; DEBUG-INFO-FOR-COMPONENT  --  Interface
;;;
;;; Return a debug-info structure describing component.  This has to be called
;;; at some particular time (after assembly) so that source map information is
;;; available.
;;; 
(defun debug-info-for-component (component assem-nodes count)
  (declare (type component component) (simple-vector assem-nodes)
	   (type index count))
  (let ((level (cookie-debug *default-cookie*))
	(res (make-compiled-debug-info :name (component-name component)
				       :package (package-name *package*))))
    (collect ((dfuns))
      (let ((var-locs (make-hash-table :test #'eq)))
	(dolist (fun (component-lambdas component))
	  (clrhash var-locs)
	  (let* ((2env (environment-info (lambda-environment fun)))
		 (dfun (make-compiled-debug-function
			:name (cond ((leaf-name fun))
				    ((let ((ef (functional-entry-function fun)))
				       (and ef (leaf-name ef))))
				    (t
				     (component-name component)))
			:kind (functional-kind fun)
			:return-pc (tn-sc-offset
				    (ir2-environment-return-pc 2env))
			:old-cont (tn-sc-offset
				   (ir2-environment-old-cont 2env))
			;; Not right...
			:start-pc 0)))
	    
	    (when (>= level 1)
	      (setf (compiled-debug-function-variables dfun)
		    (compute-variables fun level var-locs)))

	    (unless (= level 0)
	      (setf (compiled-debug-function-arguments dfun)
		    (compute-arguments fun var-locs)))

	    (when (>= level 2)
	      (multiple-value-bind (blocks tlf-num)
				   (compute-debug-blocks fun var-locs)
		(setf (debug-function-tlf-number dfun) tlf-num)
		(setf (debug-function-blocks dfun) blocks)))

	    (let ((tails (lambda-tail-set fun)))
	      (when tails
		(let ((info (tail-set-info tails)))
		  (cond ((eq (return-info-kind info) :unknown)
			 (setf (compiled-debug-function-returns dfun)
			       :standard))
			((/= level 0)
			 (setf (compiled-debug-function-returns dfun)
			       (compute-debug-returns fun)))))))

	    (dfuns (cons (label-location
			  (block-label
			   (node-block
			    (lambda-bind fun))))
			 dfun)))))

      (let* ((sorted (sort (dfuns) #'< :key #'car))
	     (len (1- (* (length sorted) 2)))
	     (funs-vec (make-array len)))
	(do ((i -1 (+ i 2))
	     (sorted sorted (cdr sorted)))
	    ((= i len))
	  (let ((dfun (car sorted)))
	    (unless (minusp i)
	      (setf (svref funs-vec i) (car dfun)))
	    (setf (svref funs-vec (1+ i)) (cdr dfun))))
	(setf (compiled-debug-info-function-map res) funs-vec)))

    res))
