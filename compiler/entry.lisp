;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Code in this file handles VM-independent details of run-time
;;; function representation that primarily concern IR2 conversion and the
;;; dumper/loader. 
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


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
      (when (external-entry-point-p fun)
	(let ((info (or (leaf-info fun)
			(setf (leaf-info fun) (make-entry-info)))))
	  (compute-entry-info fun info)
	  (push info (ir2-component-entries 2comp))))))

  (select-component-format component)
  (undefined-value))


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
	(let ((*print-pretty* t)
	      (*print-escape* t)
	      (*print-base* 10)
	      (*print-radix* nil)
	      (*print-case* :downcase))
	  (write-to-string args)))))
  

;;; Compute-Entry-Info  --  Internal
;;;
;;;    Initialize Info structure to correspond to the XEP lambda Fun.
;;;
(defun compute-entry-info (fun info)
  (declare (type clambda fun) (type entry-info info))
  (let ((block (node-block (lambda-bind fun)))
	(internal-fun (functional-entry-function fun)))
    (setf (entry-info-closure-p info)
	  (not (null (environment-closure (lambda-environment fun)))))
    (setf (entry-info-offset info) (gen-label))
    (setf (entry-info-name info)
	  (let ((name (leaf-name internal-fun)))
	    (or name
		(component-name (block-component block)))))
    (setf (entry-info-arguments info) (make-arg-names internal-fun))
    (setf (entry-info-type info) (type-specifier (leaf-type internal-fun))))
  (undefined-value))


;;; REPLACE-TOP-LEVEL-XEPS  --  Interface
;;;
;;;    Replace all references in other components to non-closure XEPs in
;;; Component with :TOP-LEVEL-XEP functionals.  We return true if any closure
;;; references were encountered.  We deliberately don't use the normal
;;; reference deletion, since we don't want to trigger deletion of the XEP
;;; (although it shouldn't hurt, since this is called after Component is
;;; compiled.)  Instead, we just clobber the REF-LEAF.
;;;
(defun replace-top-level-xeps (component)
  (let ((res nil))
    (dolist (lambda (component-lambdas component))
      (when (eq (functional-kind lambda) :external)
	(let* ((ef (functional-entry-function lambda))
	       (new (make-functional :kind :top-level-xep
				     :info (leaf-info lambda)
				     :name (leaf-name ef)
				     :lexenv (make-null-environment)))
	       (closure (environment-closure
			 (lambda-environment (main-entry ef)))))
	  (dolist (ref (leaf-refs lambda))
	    (let ((ref-component (block-component (node-block ref))))
	      (when (and (not (eq ref-component component))
			 (eq (component-kind ref-component) :top-level))
		(cond (closure
		       (setq res t))
		      (t
		       (setf (ref-leaf ref) new)
		       (push ref (leaf-refs new))))))))))
    res))
