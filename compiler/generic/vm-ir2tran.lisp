;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-ir2tran.lisp,v 1.5 1992/12/18 11:19:50 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file
;;; 
(in-package :c)

(export '(slot set-slot make-unbound-marker fixed-alloc var-alloc))

(defoptimizer ir2-convert-reffer ((object) node block name offset lowtag)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns
		cont (list (backend-any-primitive-type *backend*))))
	 (res (first locs)))
    (vop slot node block (continuation-tn node block object)
	 name offset lowtag res)
    (move-continuation-result node block locs cont)))

#+gengc
(defun needs-remembering (cont)
  (if (csubtypep (continuation-type cont)
		 (load-time-value (specifier-type '(or fixnum character
						       (member t nil)))))
      nil
      t))

(defoptimizer ir2-convert-setter ((object value) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag #+gengc (needs-remembering value))
    (move-continuation-result node block (list value-tn) (node-cont node))))

(defoptimizer ir2-convert-setfer ((value object) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag #+gengc (needs-remembering value))
    (move-continuation-result node block (list value-tn) (node-cont node))))

(defun do-inits (node block name result lowtag inits args)
  (let ((unbound-marker-tn nil))
    (dolist (init inits)
      (let ((kind (car init))
	    (slot (cdr init)))
	(vop set-slot node block result
	     (ecase kind
	       (:arg
		(assert args)
		(continuation-tn node block (pop args)))
	       (:unbound
		(or unbound-marker-tn
		    (setf unbound-marker-tn
			  (let ((tn (make-restricted-tn
				     nil
				     (sc-number-or-lose 'vm::any-reg
							*backend*))))
			    (vop make-unbound-marker node block tn)
			    tn))))
	       (:null
		(emit-constant nil)))
	     name slot lowtag #+gengc nil))))
  (assert (null args)))

(defoptimizer ir2-convert-fixed-allocation
	      ((&rest args) node block name words type lowtag inits)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns
		cont (list (backend-any-primitive-type *backend*))))
	 (result (first locs)))
    (vop fixed-alloc node block name words type lowtag result)
    (do-inits node block name result lowtag inits args)
    (move-continuation-result node block locs cont)))

(defoptimizer ir2-convert-variable-allocation
	      ((extra &rest args) node block name words type lowtag inits)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns
		cont (list (backend-any-primitive-type *backend*))))
	 (result (first locs)))
    (if (constant-continuation-p extra)
	(let ((words (+ (continuation-value extra) words)))
	  (vop fixed-alloc node block name words type lowtag result))
	(vop var-alloc node block (continuation-tn node block extra) name words
	     type lowtag result))
    (do-inits node block name result lowtag inits args)
    (move-continuation-result node block locs cont)))



;;;; Replacements for stuff in ir2tran to make gengc work.

#+gengc
(defun ir2-convert-set (node block)
  (declare (type cset node) (type ir2-block block))
  (let* ((cont (node-cont node))
	 (leaf (set-var node))
	 (val (continuation-tn node block (set-value node)))
	 (locs (if (continuation-info cont)
		   (continuation-result-tns
		    cont (list (primitive-type (leaf-type leaf))))
		   nil)))
    (etypecase leaf
      (lambda-var
       (when (leaf-refs leaf)
	 (let ((tn (find-in-environment leaf (node-environment node))))
	   (if (lambda-var-indirect leaf)
	       (vop value-cell-set node block tn val
		    (needs-remembering val))
	       (emit-move node block val tn)))))
      (global-var
       (ecase (global-var-kind leaf)
	 ((:special :global)
	  (assert (symbolp (leaf-name leaf)))
	  (vop set node block (emit-constant (leaf-name leaf)) val
	       (needs-remembering val))))))

    (when locs
      (emit-move node block val (first locs))
      (move-continuation-result node block locs cont)))
  (undefined-value))


#+gengc
(defoptimizer (%lexical-exit-breakup ir2-convert) ((info) node block)
  (vop value-cell-set node block
       (find-in-environment (continuation-value info) (node-environment node))
       (emit-constant 0)
       nil))


#+gengc
(defoptimizer (%slot-setter ir2-convert) ((value str) node block)
  (let ((val (continuation-tn node block value)))
    (vop structure-set node block
	 (continuation-tn node block str)
	 val
	 (dsd-index
	  (slot-accessor-slot
	   (ref-leaf
	    (continuation-use
	     (combination-fun node)))))
	 (needs-remembering value))
  
    (move-continuation-result node block (list val) (node-cont node))))
