;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/alloc.lisp,v 1.2 1990/11/03 17:22:39 wlott Exp $
;;;
;;; Stuff to handle allocating simple objects.
;;;
;;; Written by William Lott.
;;;

(in-package "MIPS")

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
		(:temp ndescr non-descriptor-reg nl0-offset)
		,@(when (or need-unbound-marker header variable-length)
		    '((:temp temp non-descriptor-reg nl1-offset)))
		(:res result descriptor-reg a0-offset))
	   (pseudo-atomic (ndescr)
	     (inst addu result alloc-tn ,lowtag)
	     ,@(cond ((and header variable-length)
		      `((inst addu temp extra-words (fixnum (1- ,size)))
			(inst addu alloc-tn alloc-tn temp)
			(inst sll temp temp (- type-bits word-shift))
			(inst or temp temp ,header)
			(storew temp result 0 ,lowtag)
			(inst addu alloc-tn alloc-tn
			      (+ (fixnum 1) lowtag-mask))
			(inst li temp (lognot lowtag-mask))
			(inst and alloc-tn alloc-tn temp)))
		     (variable-length
		      (error ":REST-P T with no header in ~S?"
			     (primitive-object-name obj)))
		     (header
		      `((inst addu alloc-tn alloc-tn
			      (pad-data-block ,size))
			(inst li temp
			      ,(logior (ash (1- size) type-bits)
				       (symbol-value header)))
			(storew temp result 0 ,lowtag)))
		     (t
		      `((inst addu alloc-tn alloc-tn
			      (pad-data-block ,size)))))
	     ,@(when need-unbound-marker
		 `((inst li temp unbound-marker-type)))
	     ,@(init-forms)))))))
