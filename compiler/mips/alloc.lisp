;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/alloc.lisp,v 1.17 1992/07/28 20:34:12 wlott Exp $")
;;;
;;; **********************************************************************
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
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
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
	       ((store-car (tn list &optional (slot cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    (zero zero-tn)
			    (null null-tn)
			    (control-stack
			     (load-stack-tn temp ,tn)
			     temp))))
		     (storew reg ,list ,slot list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       (pseudo-atomic (pa-flag
			       :extra (* (pad-data-block cons-size)
					 cons-cells))
		 (inst or res alloc-tn list-pointer-type)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (store-car (tn-ref-tn things) ptr)
		   (setf things (tn-ref-across things))
		   (inst addu ptr ptr (pad-data-block cons-size))
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-type))
		 (store-car (tn-ref-tn things) ptr)
		 (cond (star
			(setf things (tn-ref-across things))
			(store-car (tn-ref-tn things) ptr cons-cdr-slot))
		       (t
			(storew null-tn ptr
				cons-cdr-slot list-pointer-type)))
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
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 100
    (inst li ndescr (lognot lowtag-mask))
    (inst addu boxed boxed-arg (fixnum (1+ code-trace-table-offset-slot)))
    (inst and boxed ndescr)
    (inst srl unboxed unboxed-arg word-shift)
    (inst addu unboxed unboxed lowtag-mask)
    (inst and unboxed ndescr)
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn other-pointer-type)
      (inst addu alloc-tn boxed)
      (inst addu alloc-tn unboxed)
      (inst sll ndescr boxed (- type-bits word-shift))
      (inst or ndescr code-header-type)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (storew null-tn result code-debug-info-slot other-pointer-type))))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-type fdefn-size)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (inst li temp (make-fixup "undefined_tramp" :foreign))
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))


;;;; Automatic allocators for primitive objects.

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
	`(define-vop (,alloc-vop)
	   (:args ,@(mapcar #'(lambda (name)
				`(,name :scs (any-reg descriptor-reg)))
			    (args)))
	   (:temporary (:scs (non-descriptor-reg))
		       ,@(when (or header need-unbound-marker)
			   '(temp))
		       ,@(when variable-length '(temp2)))
	   (:temporary (:scs (descriptor-reg) :to (:result 0)
			     :target real-result) result)
	   (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
	   (:results (real-result :scs (descriptor-reg)))
	   (:policy :fast-safe)
	   ,@(when alloc-trans
	       `((:translate ,alloc-trans)))
	   (:generator ,(+ 4
			   (length (init-forms))
			   (if header 2 0)
			   (if variable-length 6 0)
			   (if need-unbound-marker 1 0))
	     (pseudo-atomic
		 (pa-flag :extra ,(if variable-length
				      0
				      `(pad-data-block ,size)))
	       (inst or result alloc-tn ,lowtag)
	       ,@(cond ((and header variable-length)
			`((inst addu temp extra-words (fixnum (1+ ,size)))
			  (inst li temp2 (lognot lowtag-mask))
			  (inst and temp temp2)
			  (inst addu alloc-tn temp)
			  (inst addu temp extra-words (fixnum (1- ,size)))
			  (inst sll temp extra-words
				(- type-bits word-shift))
			  (inst or temp temp ,header)
			  (storew temp result 0 ,lowtag)))
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
	       ,@(init-forms)
	       (move real-result result))))))))
