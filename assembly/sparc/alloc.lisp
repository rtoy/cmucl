;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/alloc.lisp,v 1.3 1992/03/21 22:18:56 wlott Exp $
;;;
;;; Stuff to handle allocating simple objects.
;;;
;;; Written by William Lott.
;;;

(in-package "SPARC")

(define-for-each-primitive-object (obj)
  (let* ((options (primitive-object-options obj))
	 (alloc-trans (getf options :alloc-trans))
	 (alloc-vop (getf options :alloc-vop alloc-trans))
	 (header (primitive-object-header obj))
	 (lowtag (primitive-object-lowtag obj))
	 (size (primitive-object-size obj))
	 (variable-length (primitive-object-variable-length obj))
	 (need-unbound-marker nil))
    (collect ((args) (init-forms))
      (when (and alloc-vop variable-length)
	(args 'extra-words))
      (dolist (slot (primitive-object-slots obj))
	(let* ((name (slot-name slot))
	       (offset (slot-offset slot)))
	  (ecase (getf (slot-options slot) :init :zero)
	    (:zero)
	    (:null
	     (init-forms `(storew null-tn result ,offset ,lowtag)))
	    (:unbound
	     (setf need-unbound-marker t)
	     (init-forms `(storew temp result ,offset ,lowtag)))
	    (:arg
	     (args name)
	     (init-forms `(storew ,name result ,offset ,lowtag))))))
      (when (and (null alloc-vop) (args))
	(error "Slots ~S want to be initialized, but there is no alloc vop ~
	defined for ~S."
	       (args) (primitive-object-name obj)))
      (when alloc-vop
	(when (and variable-length (not header))
	  (error "Can't allocate variable-length objects with no header."))
	`(define-assembly-routine
	     (,alloc-vop
	      (:cost 35)
	      ,@(when alloc-trans
		  `((:translate ,alloc-trans)))
	      (:policy :fast-safe))
	     (,@(let ((arg-offsets (cdr register-arg-offsets)))
		  (mapcar #'(lambda (name)
			      (unless arg-offsets
				(error "Too many args in ~S" alloc-vop))
			      `(:arg ,name (descriptor-reg any-reg)
				     ,(pop arg-offsets)))
			  (args)))
		,@(when (or need-unbound-marker header variable-length)
		    '((:temp temp non-descriptor-reg nl1-offset)))
		(:res result descriptor-reg a0-offset))
	   (let ((alloc ,(if variable-length
			     0
			     `(pad-data-block ,size))))
	     (pseudo-atomic (:extra alloc)
	       ,@(if (logbitp 2 (eval lowtag))
		     `((inst or result alloc-tn ,lowtag))
		     `((inst andn result alloc-tn lowtag-mask)
		       (inst or result ,lowtag)))
	       ,@(cond (variable-length
			`((inst add temp extra-words (* (1+ ,size) word-bytes))
			  (inst andn temp 7)
			  (inst add alloc-tn temp)
			  (inst sll temp extra-words (- type-bits word-shift))
			  (inst add temp (+ (ash (1- ,size) type-bits)
					    ,header))
			  (storew temp result 0 ,lowtag)))
		       (header
			`((inst li temp
				,(logior (ash (1- size) type-bits)
					 (symbol-value header)))
			  (storew temp result 0 ,lowtag)))
		       (t
			nil))
	       ,@(when need-unbound-marker
		   `((inst li temp unbound-marker-type)))
	       ,@(init-forms))))))))
