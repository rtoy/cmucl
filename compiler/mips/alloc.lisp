;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/alloc.lisp,v 1.22 1992/12/16 19:41:18 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the MIPS port.
;;;
;;; Written by William Lott.
;;; 

(in-package "MIPS")


;;;; LIST and LIST*

#-gengc
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

#-gengc
(define-vop (list list-or-list*)
  (:variant nil))

#-gengc
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
    (inst addu boxed boxed-arg
	  (fixnum (1+ #-gengc code-trace-table-offset-slot
		      #+gengc code-debug-info-slot)))
    (inst and boxed ndescr)
    (inst srl unboxed unboxed-arg word-shift)
    (inst addu unboxed unboxed lowtag-mask)
    (inst and unboxed ndescr)
    (inst sll ndescr boxed (- type-bits word-shift))
    (inst or ndescr code-header-type)
    
    #-gengc
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn other-pointer-type)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (inst addu alloc-tn boxed)
      (inst addu alloc-tn unboxed))
    #+gengc
    (without-scheduling ()
      (inst or result alloc-tn other-pointer-type)
      (storew ndescr alloc-tn 0)
      (storew unboxed alloc-tn code-code-size-slot)
      (storew null-tn alloc-tn code-entry-points-slot)
      (inst addu alloc-tn boxed)
      (inst addu alloc-tn unboxed))

    (storew null-tn result code-debug-info-slot other-pointer-type)))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  #-gengc (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-type fdefn-size)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (inst li temp (make-fixup "undefined_tramp" :foreign))
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    #-gengc
    (let ((size (+ length closure-info-offset)))
      (inst li temp (logior (ash (1- size) type-bits) closure-header-type))
      (pseudo-atomic (pa-flag :extra (pad-data-block size))
	(inst or result alloc-tn function-pointer-type)
	(storew temp result 0 function-pointer-type))
      (storew function result closure-function-slot function-pointer-type))
    #+gengc
    (let ((size (+ length closure-info-offset)))
      (inst li temp (logior (ash (1- size) type-bits) closure-header-type))
      (without-scheduling ()
	(inst or result alloc-tn function-pointer-type)
	(storew temp alloc-tn)
	(inst addu alloc-tn (pad-data-block size)))
      (inst addu temp function
	    (- (* function-code-offset word-bytes) function-pointer-type))
      (storew temp result closure-entry-point-slot function-pointer-type))))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  #-gengc (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
	(result pa-flag temp value-cell-header-type value-cell-size))
    (storew value result value-cell-value-slot other-pointer-type)))


;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li result unbound-marker-type)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 4
    #-gengc
    (pseudo-atomic (pa-flag :extra (pad-data-block words))
      (inst or result alloc-tn lowtag)
      (when type
	(inst li temp (logior (ash (1- words) type-bits) type))
	(storew temp result 0 lowtag)))
    #+gengc
    (progn
      (when type
	(inst li temp (logior (ash (1- words) type-bits) type)))
      (without-scheduling ()
	(inst or result alloc-tn lowtag)
	(when type
	  (storew temp alloc-tn))
	(inst addu alloc-tn (pad-data-block words))))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) bytes)
  #-gengc (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 6
    (inst addu bytes extra (* (1+ words) word-bytes))
    (inst sll header bytes (- type-bits 2))
    (inst addu header header (+ (ash -2 type-bits) type))
    (inst srl bytes bytes lowtag-bits)
    (inst sll bytes bytes lowtag-bits)
    #-gengc
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn lowtag)
      (storew header result 0 lowtag)
      (inst addu alloc-tn alloc-tn bytes))
    #+gengc
    (without-scheduling ()
      (inst or result alloc-tn lowtag)
      (storew header result 0 lowtag)
      (inst addu alloc-tn alloc-tn bytes))))
