;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/entry.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    Code in this file handles VM-independent details of run-time
;;; function representation that primarily concern IR2 conversion and the
;;; dumper/loader. 
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package "C")
(intl:textdomain "cmucl")


;;; Entry-Analyze  --  Interface
;;;
;;;    This phase runs before IR2 conversion, initializing each XEP's
;;; Entry-Info structure.  We call the VM-supplied Select-Component-Format
;;; function to make VM-dependent initializations in the IR2-Component.  This
;;; includes setting the IR2-Component-Kind and allocating fixed implementation
;;; overhead in the constant pool.  If there was a forward reference to a
;;; function, then the ENTRY-INFO will already exist, but will be
;;; uninitialized.
;;;
(defun entry-analyze (component)
  (let ((2comp (component-info component)))
    (dolist (fun (component-lambdas component))
      (when (or (external-entry-point-p fun)
		(typed-entry-point-p fun))
	(let ((info (or (leaf-info fun)
			(setf (leaf-info fun) (make-entry-info)))))
	  (compute-entry-info fun info)
	  (push info (ir2-component-entries 2comp))
	  (when (getf (lambda-plist fun) :entry-point)
	    (setf (getf (lambda-plist fun) :code-start) (gen-label)))))))

  (select-component-format component)
  (undefined-value))


;;; Simplify-Lambda-List  --  Internal
;;;
;;;    Remove complex init forms from the debug arglist LAMBDA-LIST so
;;; that we can print it safely.
;;;
(defun simplify-lambda-list (lambda-list)
  (labels ((simplify-lambda-arg (arg)
	     (cond ((symbolp arg) arg)
		   (t (destructuring-bind (name &optional init supplied) arg
			(declare (ignore supplied))
			(cond ((simple-init-form-p init 3) arg)
			      ((consp name) (car name))
			      (t name))))))
	   (simple-init-form-p (form level)
	     (and (> level 0)
		  (typecase form
		    ((or symbol number character) t)
		    (cons (and (simple-init-form-p (car form) (1- level))
			       (simple-init-form-p (cdr form) (1- level))))))))
    (multiple-value-bind (required optional restp rest keyp keys
				   other aux morep morectx morecount)
	(c::parse-lambda-list lambda-list)
      (declare (ignore aux))
      `(,@required 
	,@(if optional `(&optional . ,(mapcar #'simplify-lambda-arg optional)))
	,@(if restp `(&rest ,rest))
	,@(if keyp `(&key . ,(mapcar #'simplify-lambda-arg keys)))
	,@(if other `(&allow-other-keys))
	,@(if morep `(&more ,morectx ,morecount))))))



;;; Make-Arg-Names  --  Internal
;;;
;;;    Takes the list representation of the debug arglist and turns it into a
;;; string.
;;;
(defun make-arg-names (x)
  (declare (type functional x))
  (let ((args (functional-arg-documentation x)))
    (assert (not (eq args :unspecified)))
    (if (null args)
	"()"
	(let ((package *package*))
	  (with-standard-io-syntax
	    (let ((*package* package)
		  (*print-pretty* t)
		  (*print-circle* t)
		  (*print-case* :downcase))
	      ;; Just try to print it.  If we can't, simplify the
	      ;; lambda-list and print again.  (See cmucl-imp mailing
	      ;; list, 2008/04/14 for examples.)
	      (handler-case
		  (write-to-string args)
		(print-not-readable ()
		  (write-to-string (simplify-lambda-list args))))))))))

;;; Compute-Entry-Info  --  Internal
;;;
;;;    Initialize Info structure to correspond to the XEP lambda Fun.
;;;
(defun compute-entry-info (fun info)
  (declare (type clambda fun) (type entry-info info))
  (let* ((bind (lambda-bind fun))
	 (internal-fun (functional-entry-function fun))
	 (internal-fun (cond ((typed-entry-point-p internal-fun)
			      (functional-entry-function internal-fun))
			     (t internal-fun)))
	 (tep (typed-entry-point-p fun)))
    (setf (entry-info-closure-p info)
	  (not (null (environment-closure (lambda-environment fun)))))
    (setf (entry-info-offset info) (gen-label))
    (setf (entry-info-name info)
	  (let ((name (leaf-name internal-fun)))
	    (cond (tep (list :typed-entry-point name))
		  (name)
		  (t (component-name (block-component (node-block bind)))))))
    (when (or (policy bind (>= debug 1)) tep)
      (setf (entry-info-arguments info) (make-arg-names internal-fun))
      (setf (entry-info-type info) (type-specifier (leaf-type internal-fun)))))
  (undefined-value))


;;; REPLACE-TOP-LEVEL-XEPS  --  Interface
;;;
;;;    Replace all references to Component's non-closure XEPS that appear in
;;; top-level components, changing to :TOP-LEVEL-XEP functionals.  If the
;;; cross-component ref is not in a :TOP-LEVEL component, or is to a closure,
;;; then substitution is suppressed.
;;;
;;; When a cross-component ref is not substituted, we return T to indicate that
;;; early deletion of this component's IR1 should not be done.  We also return
;;; T if this component contains :TOP-LEVEL lambdas (though it is not a
;;; :TOP-LEVEL component.)
;;;
;;; We deliberately don't use the normal reference deletion, since we don't
;;; want to trigger deletion of the XEP (although it shouldn't hurt, since this
;;; is called after Component is compiled.)  Instead, we just clobber the
;;; REF-LEAF.
;;;
(defun replace-top-level-xeps (component)
  (let ((res nil))
    (dolist (lambda (component-lambdas component))
      (case (functional-kind lambda)
	(:external
	 (let* ((ef (functional-entry-function lambda))
		(ef (cond ((typed-entry-point-p ef)
			   (functional-entry-function ef))
			  (t ef)))
		(new (make-functional :kind :top-level-xep
				      :info (leaf-info lambda)
				      :name (leaf-name ef)
				      :lexenv (make-null-environment)))
		(closure (environment-closure
			  (lambda-environment (main-entry ef)))))
	   (dolist (ref (leaf-refs lambda))
	     (let ((ref-component (block-component (node-block ref))))
	       (cond ((eq ref-component component))
		     ((or (not (eq (component-kind ref-component) :top-level))
			  closure)
		      (setq res t))
		     (t
		      (setf (ref-leaf ref) new)
		      (push ref (leaf-refs new))))))))
	(:top-level
	 (setq res t))))
    res))
