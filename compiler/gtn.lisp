;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the GTN pass in the compiler.  GTN allocates the TNs
;;; that hold the values of lexical variables and determines the calling
;;; conventions and passing locations used in function calls.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; GTN-Analyze  --  Interface
;;;
;;;    We make a pass over the component's environments, assigning argument
;;; passing locations and return conventions and TNs for local variables.
;;;
(defun gtn-analyze (component)
  (setf (component-info component) (make-ir2-component))
  (let ((funs (component-lambdas component)))
    (dolist (fun funs)
      (assign-ir2-environment fun)
      (assign-return-locations fun)
      (assign-ir2-nlx-info fun)
      (assign-lambda-var-tns fun nil)
      (dolist (let (lambda-lets fun))
	(assign-lambda-var-tns let t))))

  (undefined-value))


;;; Assign-Lambda-Var-TNs  --  Internal
;;;
;;;    We have to allocate the home TNs for variables before we can call
;;; Assign-IR2-Environment so that we can close over TNs that haven't had their
;;; home environment assigned yet.  Here we evaluate the DEBUG-INFO/SPEED
;;; tradeoff to determine how variables are allocated.  If SPEED is 3, then all
;;; variables are subject to lifetime analysis.  Otherwise, only Let-P
;;; variables are allocated normally, and that can be inhibited by
;;; DEBUG-INFO = 3.
;;;
(defun assign-lambda-var-tns (fun let-p)
  (declare (type clambda fun))
  (dolist (var (lambda-vars fun))
    (when (leaf-refs var)
      (let* ((type (if (lambda-var-indirect var)
		       *any-primitive-type*
		       (primitive-type (leaf-type var))))
	     (temp (make-normal-tn type))
	     (node (lambda-bind fun))
	     (res (if (or (and let-p (policy node (< debug 3)))
			  (policy node (= speed 3)))
		      temp
		      (environment-debug-live-tn temp
						 (lambda-environment fun)))))
	(setf (tn-leaf res) var)
	(setf (leaf-info var) res))))
  (undefined-value))


;;; Assign-IR2-Environment  --  Internal
;;;
;;;    Give an IR2-Environment structure to Fun.  We make the TNs which hold
;;; environment values and the old-FP/return-PC.
;;;
(defun assign-ir2-environment (fun)
  (declare (type clambda fun))
  (let ((env (lambda-environment fun)))
    (collect ((env))
      (dolist (thing (environment-closure env))
	(let ((ptype (etypecase thing
		       (lambda-var
			(if (lambda-var-indirect thing)
			    *any-primitive-type*
			    (primitive-type (leaf-type thing))))
		       (nlx-info *any-primitive-type*))))
	  (env (cons thing (make-normal-tn ptype)))))

      (let ((res (make-ir2-environment
		  :environment (env)
		  :return-pc-pass (make-return-pc-passing-location
				   (external-entry-point-p fun)))))
	(setf (environment-info env) res)
	(setf (ir2-environment-old-fp res)
	      (make-old-fp-save-location env))
	(setf (ir2-environment-return-pc res)
	      (make-return-pc-save-location env)))))
  
  (undefined-value))


;;; Has-Full-Call-Use  --  Internal
;;;
;;;    Return true if Fun's result continuation is used in a TR full call.  We
;;; only consider explicit :Full calls.  It is assumed that known calls are
;;; never part of a tail-recursive loop, so we don't need to enforce
;;; tail-recursion.  In any case, we don't know which known calls will
;;; actually be full calls until after LTN.
;;;
(defun has-full-call-use (fun)
  (declare (type clambda fun))
  (do-uses (use (return-result (lambda-return fun)) nil)
    (when (and (node-tail-p use)
	       (basic-combination-p use)
	       (eq (basic-combination-kind use) :full))
      (return t))))


;;; Use-Standard-Returns  --  Internal
;;;
;;;    Return true if we should use the standard (unknown) return convention
;;; for a tail-set.  We use the standard return convention when:
;;; -- We must use the standard convention to preserve tail-recursion, since
;;;    the tail-set contains both an XEP and a TR full call.
;;; -- It appears to be more efficient to use the standard convention, since
;;;    there are no non-TR local calls that could benefit from a non-standard
;;;    convention.
;;;
(defun use-standard-returns (tails)
  (declare (type tail-set tails))
  (let ((funs (tail-set-functions tails)))
    (or (and (find-if #'external-entry-point-p funs)
	     (find-if #'has-full-call-use funs))
	(block punt
	  (dolist (fun funs t)
	    (dolist (ref (leaf-refs fun))
	      (let* ((cont (node-cont ref))
		     (dest (continuation-dest cont)))
		(when (and (not (node-tail-p dest))
			   (basic-combination-p dest)
			   (eq (basic-combination-fun dest) cont)
			   (eq (basic-combination-kind dest) :local))
		  (return-from punt nil)))))))))


;;; Return-Info-For-Set  --  Internal
;;;
;;;    Return a Return-Info structure describing how we should return from
;;; functions in the specified tail set.  We use the unknown values convention
;;; if the number of values is unknown, or if it is a good idea for some other
;;; reason.  Otherwise we allocate passing locations for a fixed number of
;;; values.
;;;
(defun return-info-for-set (tails)
  (declare (type tail-set tails))
  (multiple-value-bind (types count)
		       (values-types (tail-set-type tails))
    (let ((ptypes (mapcar #'primitive-type types)))
      (if (or (eq count :unknown)
	      (use-standard-returns tails))
	  (make-return-info :kind :unknown  :count count  :types ptypes)
	  (make-return-info
	   :kind :fixed
	   :count count
	   :types ptypes
	   :locations (mapcar #'make-normal-tn ptypes))))))


;;; Assign-Return-Locations  --  Internal
;;;
;;;    If Env has a Tail-Set, and the Tail-Set doesn't have any Info, then make
;;; a Return-Info for it.  If we choose a return convention other than
;;; :Unknown, and this environment is for an XEP, then break tail recursion on
;;; the XEP calls, since we must always use unknown values when returning from
;;; an XEP.
;;;
(defun assign-return-locations (fun)
  (declare (type clambda fun))
  (let ((tails (lambda-tail-set fun)))
    (when tails
      (let ((returns (or (tail-set-info tails)
			 (setf (tail-set-info tails)
			       (return-info-for-set tails)))))
	(when (and (not (eq (return-info-kind returns) :unknown))
		   (external-entry-point-p fun))
	  (do-uses (use (return-result (lambda-return fun)))
	    (setf (node-tail-p use) nil))))))
  (undefined-value))


;;; Assign-IR2-NLX-Info  --  Internal
;;;
;;;   Make an IR2-NLX-Info structure for each NLX entry point recorded.  We
;;; call a VM supplied function to make the Save-SP restricted on the stack.
;;; The NLX-Entry VOP's :Force-To-Stack Save-P value doesn't do this, since the
;;; SP is an argument to the VOP, and thus isn't live afterwards.
;;;
(defun assign-ir2-nlx-info (fun)
  (declare (type clambda fun))
  (let ((env (lambda-environment fun)))
    (dolist (nlx (environment-nlx-info env))
      (setf (nlx-info-info nlx)
	    (make-ir2-nlx-info
	     :home (when (member (cleanup-kind (nlx-info-cleanup nlx))
				 '(:block :tagbody))
		     (make-normal-tn *any-primitive-type*))
	     :save-sp (make-nlx-sp-tn env)))))
  (undefined-value))
