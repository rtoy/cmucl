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


;;; DEBUG-LOCATION-FOR  --  Internal
;;;
;;;    Return a COMPILED-LOCATION structure describing Leaf.  If we already
;;; have a location for this var in this function, then return that.  If the
;;; var has no refs, return DELETED.
;;;
(defun debug-location-for (leaf var-locs)
  (declare (type lambda-var leaf) (type hash-table var-locs))
  (cond ((gethash leaf var-locs))
	((null (leaf-refs leaf)) 'deleted)
	(t
	 (let* ((name (leaf-name leaf))
		(package (symbol-package name))
		(loc (make-compiled-location
		      :name (symbol-name name)
		      :package-name (when package (package-name package))
		      :type (type-specifier (leaf-type leaf))))
		(tn (leaf-info leaf)))
	   
	   (setf (compiled-location-offset loc)
		 (tn-offset tn))
	   (setf (compiled-location-sc loc)
		 (sc-number (tn-sc tn)))
	   
	   (let ((save (or (tn-save-tn tn) tn)))
	     (setf (compiled-location-save-offset loc)
		   (tn-offset save))
	     (setf (compiled-location-save-sc loc)
		   (sc-number (tn-sc tn))))
	   
	   (setf (gethash leaf var-locs)
		 loc)))))


;;; COMPUTE-VARIABLES  --  Internal
;;;
;;;    Return a vector suitable for use as the DEBUG-FUNCTION-VARIABLES of Fun.
;;; If Max-Info-P is true, we dump gensym variables as well as named ones.
;;;
(defun compute-variables (fun max-info-p var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((locs))
    (flet ((frob (x)
	     (dolist (leaf (lambda-vars x))
	       (let ((name (leaf-name leaf)))
		 (when (and name (leaf-refs leaf)
			    (or max-info-p (symbol-package name)))
		   (locs (debug-location-for leaf var-locs)))))))
      (frob fun)
      (dolist (let (lambda-lets fun))
	(frob let)))
    
    (let ((res (coerce (sort (locs)
			     #'(lambda (x y)
				 (string< (compiled-location-name x)
					  (compiled-location-name y))))
		       'simple-vector)))
      (let ((prev-name nil))
	(declare (type (or simple-string null) prev-name))
	(dotimes (i (length res))
	  (let* ((loc (svref res i))
		 (name (compiled-location-name loc)))
	    (when (and prev-name (string= prev-name name))
	      (setf (compiled-location-id loc)
		    (1+ (compiled-location-id (svref res (1- i))))))
	    (setq prev-name name))))
      res)))


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
  (collect ((res))
    (dolist (tn (return-info-locations (tail-set-info (lambda-tail-set fun))))
      (let ((loc (make-compiled-location :name "RETURN-LOCATION")))
	(setf (compiled-location-offset loc) (tn-offset tn))
	(setf (compiled-location-sc loc) (sc-number (tn-sc tn)))
	(res loc)))
    (coerce (res) 'simple-vector)))


;;; DEBUG-INFO-FOR-COMPONENT  --  Interface
;;;
;;; Return a debug-info structure describing component.  This has to be called
;;; at some particular time (after assembly) so that source map information is
;;; available.
;;; 
(defun debug-info-for-component (component assem-nodes count)
  (declare (type component component) (simple-vector assem-nodes)
	   (type index count))
  (let ((min-info-p
	 (policy nil (or (= debug 0)
			 (and (> space debug) (= space 3)))))
	(normal-info-p
	 (policy nil (>= debug space)))
	(max-info-p
	 (policy nil (= debug 3)))
	(res (make-compiled-debug-info :name (component-name component))))

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
	    
	    (when normal-info-p
	      (setf (compiled-debug-function-variables dfun)
		    (compute-variables fun max-info-p var-locs)))

	    (unless min-info-p
	      (setf (compiled-debug-function-arguments dfun)
		    (compute-arguments fun var-locs)))

	    (let ((tails (lambda-tail-set fun)))
	      (when tails
		(let ((info (tail-set-info tails)))
		  (cond ((eq (return-info-kind info) :unknown)
			 (setf (compiled-debug-function-returns dfun)
			       :standard))
			((not min-info-p)
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
