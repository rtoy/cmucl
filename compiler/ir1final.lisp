;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/ir1final.lisp,v 1.10 1991/03/11 17:14:09 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file implements the IR1 finalize phase, which checks for various
;;; semantic errors.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Note-Failed-Optimization  --  Internal
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


;;; Tail-Annotate  --  Internal
;;;
;;;    Mark all tail-recursive uses of function result continuations with the
;;; corresponding tail-set.  Nodes whose type is NIL (i.e. don't return) such
;;; as calls to ERROR are never annotated as tail, so as to preserve debugging
;;; information.
;;;
(defun tail-annotate (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let ((ret (lambda-return fun)))
      (when ret
	(let ((result (return-result ret))
	      (tails (lambda-tail-set fun)))
	  (do-uses (use result)
	    (when (and (immediately-used-p result use)
		       (or (not (eq (node-derived-type use) *empty-type*))
			   (not (basic-combination-p use))
			   (eq (basic-combination-kind use) :local)))
	      (setf (node-tail-p use) tails)))))))
  (undefined-value))


;;; IR1-FINALIZE  --  Interface
;;;
;;;    Do miscellaneous things that we want to do once all optimization has
;;; been done:
;;;  -- Record the derived result type before the back-end trashes the
;;;     flow graph.
;;;  -- For each named function with an XEP, note the definition of that name,
;;;     and add derived type information to the info environment.  We also
;;;     delete the FUNCTIONAL from *FREE-FUNCTIONS* to eliminate the
;;;     possibility that new references might be converted to it.
;;;  -- Note any failed optimizations.
;;; 
(defun ir1-finalize (component)
  (declare (type component component))
  (tail-annotate component)

  (dolist (fun (component-lambdas component))
    (case (functional-kind fun)
      (:external
       (let* ((leaf (functional-entry-function fun))
	      (name (leaf-name leaf))
	      (where (info function where-from name))
	      (dtype (definition-type leaf))
	      (*compiler-error-context* (lambda-bind (main-entry leaf))))
	 (setf (leaf-type leaf) dtype)
	 (when (eq leaf (gethash name *free-functions*))
	   (note-name-defined name :function)
	   (remhash name *free-functions*)
	   (ecase where
	     (:assumed
	      (let ((approx-type (info function assumed-type name)))
		(when (and approx-type (function-type-p dtype))
		  (valid-approximate-type approx-type dtype))
		(setf (info function type name) dtype)
		(setf (info function assumed-type name) nil))
	      (setf (info function where-from name) :defined))
	     (:declared); Just keep declared type.
	     (:defined
	      (setf (info function type name) dtype))))))
      ((nil)
       (setf (leaf-type fun) (definition-type fun)))))

  (maphash #'note-failed-optimization *failed-optimizations*)
  (clrhash *failed-optimizations*)
  (undefined-value))
