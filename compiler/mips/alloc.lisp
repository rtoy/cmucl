;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/alloc.lisp,v 1.16 1992/03/11 21:26:13 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/alloc.lisp,v 1.16 1992/03/11 21:26:13 wlott Exp $
;;;
;;; Allocation VOPs for the MIPS port.
;;;
;;; Written by William Lott.
;;; 

(in-package "MIPS")


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (cond ((zerop num)
	   (move result null-tn))
	  ((and star (= num 1))
	   (move result (tn-ref-tn things)))
	  (t
	   (macrolet
	       ((store-car (tn list &optional (slot vm:cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    (zero zero-tn)
			    (null null-tn)
			    (control-stack
			     (load-stack-tn temp ,tn)
			     temp))))
		     (storew reg ,list ,slot vm:list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       (pseudo-atomic (ndescr)
		 (inst addu res alloc-tn vm:list-pointer-type)
		 (inst addu alloc-tn alloc-tn
		       (* (vm:pad-data-block vm:cons-size) cons-cells))
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (store-car (tn-ref-tn things) ptr)
		   (setf things (tn-ref-across things))
		   (inst addu ptr ptr (vm:pad-data-block vm:cons-size))
		   (storew ptr ptr
			   (- vm:cons-cdr-slot vm:cons-size)
			   vm:list-pointer-type))
		 (store-car (tn-ref-tn things) ptr)
		 (cond (star
			(setf things (tn-ref-across things))
			(store-car (tn-ref-tn things) ptr vm:cons-cdr-slot))
		       (t
			(storew null-tn ptr
				vm:cons-cdr-slot vm:list-pointer-type)))
		 (assert (null (tn-ref-across things)))
		 (move result res))))))))

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
    (inst li ndescr (lognot vm:lowtag-mask))
    (inst addu boxed boxed-arg (fixnum (1+ vm:code-trace-table-offset-slot)))
    (inst and boxed ndescr)
    (inst srl unboxed unboxed-arg vm:word-shift)
    (inst addu unboxed unboxed vm:lowtag-mask)
    (inst and unboxed ndescr)
    (pseudo-atomic (ndescr)
      (inst addu result alloc-tn vm:other-pointer-type)
      (inst addu alloc-tn boxed)
      (inst addu alloc-tn unboxed)
      (inst sll ndescr boxed (- vm:type-bits vm:word-shift))
      (inst or ndescr vm:code-header-type)
      (storew ndescr result 0 vm:other-pointer-type)
      (storew unboxed result vm:code-code-size-slot vm:other-pointer-type)
      (storew null-tn result vm:code-entry-points-slot vm:other-pointer-type)
      (storew null-tn result vm:code-debug-info-slot vm:other-pointer-type))))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result temp fdefn-type fdefn-size)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (inst li temp (make-fixup "undefined_tramp" :foreign))
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))


;;;; Automatic allocators for primitive objects.

(vm:define-for-each-primitive-object (obj)
  (collect ((forms))
    (let* ((options (vm:primitive-object-options obj))
	   (alloc-trans (getf options :alloc-trans))
	   (alloc-vop (getf options :alloc-vop alloc-trans))
	   (header (vm:primitive-object-header obj))
	   (lowtag (vm:primitive-object-lowtag obj))
	   (size (vm:primitive-object-size obj))
	   (variable-length (vm:primitive-object-variable-length obj))
	   (need-unbound-marker nil))
      (collect ((args) (init-forms))
	(when (and alloc-vop variable-length)
	  (args 'extra-words))
	(dolist (slot (vm:primitive-object-slots obj))
	  (let* ((name (vm:slot-name slot))
		 (offset (vm:slot-offset slot)))
	    (ecase (getf (vm:slot-options slot) :init :zero)
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
		 (args) (vm:primitive-object-name obj)))
	(when alloc-vop
	  (forms
	   `(define-vop (,alloc-vop)
	      (:args ,@(mapcar #'(lambda (name)
				   `(,name :scs (any-reg descriptor-reg)))
			       (args)))
	      (:temporary (:scs (non-descriptor-reg) :type random)
			  ndescr
			  ,@(when (or need-unbound-marker header
				      variable-length)
			      '(temp)))
	      (:temporary (:scs (descriptor-reg) :to (:result 0)
				:target real-result) result)
	      (:results (real-result :scs (descriptor-reg)))
	      (:policy :fast-safe)
	      ,@(when alloc-trans
		  `((:translate ,alloc-trans)))
	      (:generator 37
		(pseudo-atomic (ndescr)
		  (inst addu result alloc-tn ,lowtag)
		  ,@(cond ((and header variable-length)
			   `((inst addu temp extra-words
				   (fixnum (1- ,size)))
			     (inst addu alloc-tn alloc-tn temp)
			     (inst sll temp temp
				   (- vm:type-bits vm:word-shift))
			     (inst or temp temp ,header)
			     (storew temp result 0 ,lowtag)
			     (inst addu alloc-tn alloc-tn
				   (+ (fixnum 1) vm:lowtag-mask))
			     (inst li temp (lognot vm:lowtag-mask))
			     (inst and alloc-tn alloc-tn temp)))
			  (variable-length
			   (error ":REST-P T with no header in ~S?"
				  (vm:primitive-object-name obj)))
			  (header
			   `((inst addu alloc-tn alloc-tn
				   (vm:pad-data-block ,size))
			     (inst li temp
				   ,(logior (ash (1- size) vm:type-bits)
					    (symbol-value header)))
			     (storew temp result 0 ,lowtag)))
			  (t
			   `((inst addu alloc-tn alloc-tn
				   (vm:pad-data-block ,size)))))
		  ,@(when need-unbound-marker
		      `((inst li temp vm:unbound-marker-type)))
		  ,@(init-forms)
		  (move real-result result))))))))
    (when (forms)
      `(progn
	 ,@(forms)))))
