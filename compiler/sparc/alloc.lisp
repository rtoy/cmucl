;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/alloc.lisp,v 1.6 1992/12/13 15:23:45 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the SPARC port.
;;;
;;; Written by William Lott.
;;; 

(in-package "SPARC")


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
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
		 (inst andn res alloc-tn lowtag-mask)
		 (inst or res list-pointer-type)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (storew (maybe-load (tn-ref-tn things)) ptr
			   cons-car-slot list-pointer-type)
		   (setf things (tn-ref-across things))
		   (inst add ptr ptr (pad-data-block cons-size))
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-type))
		 (storew (maybe-load (tn-ref-tn things)) ptr
			 cons-car-slot list-pointer-type)
		 (storew (if star
			     (maybe-load (tn-ref-tn (tn-ref-across things)))
			     null-tn)
			 ptr cons-cdr-slot list-pointer-type))
	       (move result res)))))))

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
    (inst add boxed boxed-arg (fixnum (1+ code-trace-table-offset-slot)))
    (inst and boxed (lognot lowtag-mask))
    (inst srl unboxed unboxed-arg word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    (pseudo-atomic ()
      ;; Note: we don't have to subtract off the 4 that was added by
      ;; pseudo-atomic, because oring in other-pointer-type just adds
      ;; it right back.
      (inst or result alloc-tn other-pointer-type)
      (inst add alloc-tn boxed)
      (inst add alloc-tn unboxed)
      (inst sll ndescr boxed (- type-bits word-shift))
      (inst or ndescr code-header-type)
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
      (inst li temp (make-fixup "_undefined_tramp" :foreign))
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))


(define-vop (make-closure)
  (:args (function :to :save))
  (:info length)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let ((size (+ length closure-info-offset)))
      (pseudo-atomic (:extra (pad-data-block size))
	(inst or result alloc-tn function-pointer-type)
	(inst li temp (logior (ash (1- size) type-bits) closure-header-type))
	(storew temp result 0 function-pointer-type)))
    (storew function result closure-function-slot function-pointer-type)))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
	(result temp value-cell-header-type value-cell-size))
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
  (:generator 4
    (pseudo-atomic (:extra (pad-data-block words))
      (inst or result alloc-tn lowtag)
      (when type
	(inst li temp (logior (ash (1- words) type-bits) type))
	(storew temp result 0 lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) temp1 temp2)
  (:generator 6
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn lowtag)
      (inst add temp1 extra (fixnum (1- words)))
      (inst sll temp2 temp2 (- type-bits 2))
      (inst or temp2 temp2 type)
      (storew temp2 result 0 lowtag)
      (inst li temp2 (lognot lowtag-mask))
      (inst and temp1 temp1 temp2)
      (inst add alloc-tn alloc-tn temp1))))
