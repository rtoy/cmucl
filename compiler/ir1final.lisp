;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file implements the IR1 finalize phase, which checks for various
;;; semantic errors.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Note-Failed-Optimization  --  Interface
;;;
;;;    Give the user grief about optimizations that we weren't able to do.  It
;;; is assumed that they want to hear, or there wouldn't be any entries in the
;;; table.  If the node has been deleted or is no longer a known call, then do
;;; nothing; some other optimization must have gotten to it.
;;;
(defun note-failed-optimization (node failures)
  (declare (type combination node) (list failures))
  (unless (or (node-deleted node)
	      (not (function-info-p (combination-kind node))))
    (let ((*compiler-error-context* node))
      (dolist (failure failures)
	(let ((what (cdr failure)))
	  (cond
	   ((consp what)
	    (compiler-note "Unable to optimize because:~%~6T~?"
			   (first what) (rest what)))
	   ((valid-function-use node what
				:argument-test #'types-intersect
				:result-test #'values-types-intersect)
	    (collect ((messages))
	      (flet ((frob (string &rest stuff)
		       (messages string)
		       (messages stuff)))
		(valid-function-use node what
				    :warning-function #'frob
				    :error-function #'frob))
	      
	      (compiler-note "Unable to optimize due to type uncertainty:~@
	                      ~{~6T~?~^~&~}"
			     (messages))))))))))

	  
;;; Check-Free-Function  --  Interface
;;;
;;;    If the entry is a functional, then we update the global environment
;;; according to the new definition, checking for inconsistency.
;;;
(proclaim '(function check-free-function (t leaf) void))
(defun check-free-function (name leaf)
  (etypecase leaf
    (functional
     (let* ((where (info function where-from name))
	    (dtype (leaf-type leaf))
	    (*compiler-error-context* (lambda-bind (main-entry leaf))))
       (note-name-defined name :function)
       (ecase where
	 (:assumed
	  (let ((approx-type (info function assumed-type name)))
	    (when (and approx-type (function-type-p dtype))
	      (valid-approximate-type approx-type dtype))
	    (setf (info function type name) dtype)
	    (setf (info function assumed-type name) nil))
	  (setf (info function where-from name) :defined))
	 (:declared); No check for now, just keep declared type.
	 (:defined
	  (setf (info function type name) dtype)))))
    (global-var)))
