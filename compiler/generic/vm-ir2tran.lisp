;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-ir2tran.lisp,v 1.1 1992/12/13 15:14:50 wlott Exp $")
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

(defoptimizer ir2-convert-setter ((object value) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag)
    (move-continuation-result node block (list value-tn) (node-cont node))))

(defoptimizer ir2-convert-setfer ((value object) node block name offset lowtag)
  (let ((value-tn (continuation-tn node block value)))
    (vop set-slot node block (continuation-tn node block object) value-tn
	 name offset lowtag)
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
			  (let ((tn (make-normal-tn
				     (primitive-type-or-lose 'positive-fixnum
							     *backend*))))
			    (vop make-unbound-marker node block tn)
			    tn))))
	       (:null
		(emit-constant nil)))
	     name slot lowtag))))
  (assert (null args)))

(defoptimizer ir2-convert-fixed-allocation
	      ((&rest args) node block name words type lowtag inits)
  (let* ((cont (node-cont node))
	 (locs (continuation-result-tns
		cont (list (backend-any-primitive-type *backend*))))
	 (result (first locs)))
    (vop fixed-alloc node block name words
	 (and type (logior (ash words vm:type-bits) type))
	 lowtag result)
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
	  (vop fixed-alloc node block name words
	       (logior (ash words vm:type-bits) type)
	       lowtag result))
	(vop var-alloc node block (continuation-tn extra) name words
	     (logior (ash words vm:type-bits) type)
	     lowtag result))
    (do-inits node block name result lowtag inits args)
    (move-continuation-result node block locs cont)))
