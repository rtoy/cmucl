;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/hppa/alloc.lisp,v 1.1 1992/07/13 03:48:14 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the HPPA port.
;;;
;;; Written by William Lott.
;;; 

(in-package "HPPA")


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (cond
     ((zerop num)
      (move null-tn result))
     ((and star (= num 1))
      (move (tn-ref-tn things) result))
     (t
      (macrolet
	  ((maybe-load (tn)
	     (once-only ((tn tn))
	       `(sc-case ,tn
		  ((any-reg descriptor-reg zero null)
		   ,tn)
		  (control-stack
		   (load-stack-tn temp ,tn)
		   temp)))))
	(let* ((cons-cells (if star (1- num) num))
	       (alloc (* (pad-data-block cons-size) cons-cells)))
	  (pseudo-atomic (:extra alloc)
	    (move alloc-tn res)
	    (inst dep list-pointer-type 31 3 res)
	    (move res ptr)
	    (dotimes (i (1- cons-cells))
	      (storew (maybe-load (tn-ref-tn things)) ptr
		      cons-car-slot list-pointer-type)
	      (setf things (tn-ref-across things))
	      (inst addi (pad-data-block cons-size) ptr ptr)
	      (storew ptr ptr
		      (- cons-cdr-slot cons-size)
		      list-pointer-type))
	    (storew (maybe-load (tn-ref-tn things)) ptr
		    cons-car-slot list-pointer-type)
	    (storew (if star
			(maybe-load (tn-ref-tn (tn-ref-across things)))
			null-tn)
		    ptr cons-cdr-slot list-pointer-type))
	  (move res result)))))))


(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))


;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
	 (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:generator 100
    (inst addi (fixnum (1+ code-trace-table-offset-slot)) boxed-arg boxed)
    (inst dep 0 31 3 boxed)
    (inst srl unboxed-arg word-shift unboxed)
    (inst addi lowtag-mask unboxed unboxed)
    (inst dep 0 31 3 unboxed)
    (pseudo-atomic ()
      ;; Note: we don't have to subtract off the 4 that was added by
      ;; pseudo-atomic, because depositing other-pointer-type just adds
      ;; it right back.
      (inst move alloc-tn result)
      (inst dep other-pointer-type 31 3 result)
      (inst add alloc-tn boxed alloc-tn)
      (inst add alloc-tn unboxed alloc-tn)
      (inst sll boxed (- type-bits word-shift) ndescr)
      (inst addi code-header-type ndescr ndescr)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (storew null-tn result code-debug-info-slot other-pointer-type))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result temp fdefn-type fdefn-size)
      (inst li (make-fixup "undefined_tramp" :foreign) temp)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))


;;;; Automatic allocators for primitive objects.

(define-for-each-primitive-object (obj)
  (collect ((forms))
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
	  (forms
	   `(define-vop (,alloc-vop)
	      (:args ,@(mapcar #'(lambda (name)
				   `(,name :scs (any-reg descriptor-reg)))
			       (args)))
	      ,@(when (or need-unbound-marker header variable-length)
		  `((:temporary (:scs (non-descriptor-reg)) temp)))
	      (:results (result :scs (descriptor-reg) :from :load))
	      (:policy :fast-safe)
	      ,@(when alloc-trans
		  `((:translate ,alloc-trans)))
	      (:generator 37
		(pseudo-atomic (,@(unless variable-length
				    `(:extra (pad-data-block ,size))))
		  (move alloc-tn result)
		  (inst dep ,lowtag 31 3 result)
		  ,@(cond
		     ((and header variable-length)
		      `((inst addi (* (1+ ,size) word-bytes) extra-words temp)
			(inst dep 0 31 3 temp)
			(inst add temp alloc-tn alloc-tn)
			(inst sll extra-words (- type-bits word-shift) temp)
			(inst addi (+ (ash (1- ,size) type-bits) ,header)
			      temp temp)
			(storew temp result 0 ,lowtag)))
		     (variable-length
		      (error ":REST-P T with no header in ~S?"
			     (primitive-object-name obj)))
		     (header
		      `((inst li ,(logior (ash (1- size) type-bits)
					  (symbol-value header))
			      temp)
			(storew temp result 0 ,lowtag)))
		     (t
		      nil))
		  ,@(when need-unbound-marker
		      `((inst li unbound-marker-type temp)))
		  ,@(init-forms))))))))
    (when (forms)
      `(progn
	 ,@(forms)))))
