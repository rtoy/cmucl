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
			  :start-position 0)))
		(cond ((pathnamep name)
		       (setf (debug-source-name res) name))
		      (t
		       (setf (debug-source-from res) name)
		       (when (eq name :lisp)
			 (setf (debug-source-name res)
			       (cadr (aref (file-info-forms x) 0))))))
		res))
	  (source-info-files info)))


;;; TN-SC-OFFSET  --  Internal
;;;
;;;    Return a SC-OFFSET describing TN's location.
;;;
(defun tn-sc-offset (tn)
  (declare (type tn tn))
  (make-sc-offset (sc-number (tn-sc tn))
		  (tn-offset tn)))


;;; DUMP-1-LOCATION  --  Internal
;;;
;;;    Dump info to represent Var's location being TN.  ID is an integer that
;;; makes Var's name unique in the function.  Buffer is the vector we stick the
;;; result in.
;;;
(defun dump-1-location (var tn id buffer)
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
	  (frob let (= level 3)))))
    
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
	  (dump-1-location var (cdr x) id *byte-buffer*))
	(setf (gethash var var-locs) i)
	(incf i)))

    (copy-seq *byte-buffer*)))


;;; DEBUG-LOCATION-FOR  --  Internal
;;;
;;;    Return Var's relative position in the function's variables (determined
;;; from the Var-Locs hashtable.)
;;;
(defun debug-location-for (var var-locs)
  (declare (type lambda-var var) (type hashtable var-locs))
  (let ((res (gethash var var-locs)))
    (assert res () "No location for ~S?" var)
    res))


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

    (coerce (res) 'simple-vector)))


;;; COMPUTE-DEBUG-RETURNS  --  Internal
;;;
;;;    Return a list of COMPILED-LOCATION structures for the fixed-values
;;; return from Fun.
;;;
(defun compute-debug-returns (fun)
  (let* ((locs (return-info-locations (tail-set-info (lambda-tail-set fun))))
	 (len (length locs))
	 (res (make-array len :element-type '(unsigned-byte 32))))
    (do ((i 0 (1+ i))
	 (loc locs (cdr loc)))
	((null loc))
      (setf (aref res i) (tn-sc-offset (car loc))))
    res))


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
	  (let ((dfun (make-compiled-debug-function
		       :name (cond ((leaf-name fun))
				   ((let ((ef (functional-entry-function fun)))
				      (and ef (leaf-name ef))))
				   (t
				    (component-name component)))
		       :kind (functional-kind fun))))
	    
	    (when (>= level 1)
	      (setf (compiled-debug-function-variables dfun)
		    (compute-variables fun level var-locs)))

	    (unless (= level 0)
	      (setf (compiled-debug-function-arguments dfun)
		    (compute-arguments fun var-locs)))

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
