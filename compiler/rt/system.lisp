;;; -*- Package: rt; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/rt/system.lisp,v 1.2 1991/04/21 19:55:59 wlott Exp $
;;;
;;; IBM RT VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; IBM RT conversion by Bill Chiles.
;;;

(in-package "RT")



;;;; Random pointer comparison VOPs

(define-vop (pointer-compare)
  (:args (x :scs (sap-reg))
	 (y :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 6
    (inst cl x y)
    (if not-p
	(inst bnc condition target)
	(inst bc condition target))))

(macrolet ((frob (name cond)
	     `(progn
		(def-primitive-translator ,name (x y) `(,',name ,x ,y))
		(defknown ,name (t t) boolean (movable foldable flushable))
		(define-vop (,name pointer-compare)
		  (:translate ,name)
		  (:variant ,cond)))))
  (frob pointer< :lt)
  (frob pointer> :gt))



;;;; Random assertions VOPS.

(define-vop (check-op)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:policy :fast-safe))

(define-vop (check<= check-op)
  (:translate check<=)
  (:generator 6
    (let ((target (generate-error-code vop not-<=-error x y)))
      (inst c x y)
      (inst bc :gt target))))

(define-vop (check= check-op)
  (:translate check=)
  (:generator 6
    (let ((target (generate-error-code vop not-=-error x y)))
      (inst c x y)
      (inst bnc :eq target))))



;;;; Type frobbing VOPs.

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst nilz result object lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (let ((other-ptr (gen-label))
	  (function-ptr (gen-label))
	  (lowtag-only (gen-label))
	  (done (gen-label)))
      (test-type object ndescr other-ptr nil other-pointer-type)
      (test-type object ndescr function-ptr nil function-pointer-type)
      (test-type object ndescr lowtag-only nil
		 even-fixnum-type odd-fixnum-type list-pointer-type
		 structure-pointer-type)
      (inst bx done)
      (inst nilz result object type-mask)

      (emit-label function-ptr)
      (load-type result object function-pointer-type)
      (inst b done)

      (emit-label lowtag-only)
      (inst bx done)
      (inst nilz result object lowtag-mask)

      (emit-label other-ptr)
      (load-type result object other-pointer-type)
      
      (emit-label done))))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 7
    (loadw res x 0 other-pointer-type)
    (inst sr res type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 7
    (loadw res x 0 function-pointer-type)
    (inst sr res type-bits)))

;;; SET-HEADER-DATA -- VOP.
;;;
;;; In the immediate case for data, we use the OIL instruction assuming the
;;; data fits in the number of bits determined by 16 minus type-bits.  Due to
;;; known uses of this VOP, which only store single digit tags, the above
;;; assumption is reasonable, although unnecessarily slimy.
;;;
(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (any-reg immediate) :target t2))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) t1)
  (:temporary (:scs (non-descriptor-reg) :type random :from (:argument 1)) t2)
  (:generator 15
    (loadw t1 x 0 other-pointer-type)
    (inst nilz t1 type-mask)
    (sc-case data
      (any-reg
       (move t2 data)
       ;; Since the data is in fixnum format, it is already shifted by 2 bits.
       (inst sl t2 (- type-bits 2))
       (inst o t1 t2))
      (immediate
       (let ((value (tn-value data)))
	 (unless (zerop value)
	   (inst oil t1 (ash value type-bits))))))
    (storew t1 x 0 other-pointer-type)
    (move res x)))

;;; MAKE-FIXNUM -- VOP.
;;;
;;; This is just used in hashing stuff.  It doesn't necessarily have to
;;; preserve all the bits in the pointer.  Some code expects a positive number,
;;; so make sure the right shift is logical.
;;;
(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg) :target temp))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 3
    (move temp ptr)
    (inst sl temp 3)
    (inst sr temp 1)
    (move res temp)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg) :target vtemp)
	 (type :scs (any-reg descriptor-reg immediate)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) vtemp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (move vtemp val)
       (inst sl vtemp type-bits)
       (inst oil res temp (tn-value type)))
      (t
       ;; Type is a fixnum, so lose those lowtag bits.
       (move temp type)
       (inst sr temp 2)
       ;; Val is a fixnum, so we can shift it left two less bits.
       (move res val)
       (inst sl res (- type-bits 2))
       (inst o res temp)))))



;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 6
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 6
    (load-symbol-value int *binding-stack-pointer*)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))



;;;; Code object frobbing.

(define-vop (code-instructions)
  (:args (code :scs (descriptor-reg) :target sap))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst sr ndescr type-bits)
    (inst sl ndescr word-shift)
    (inst s ndescr other-pointer-type)
    (move sap code)
    (inst a sap ndescr)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst sr ndescr type-bits)
    (inst sl ndescr word-shift)
    (inst a ndescr offset)
    (inst a ndescr (- function-pointer-type other-pointer-type))
    (inst a ndescr code)
    (move func ndescr)))



;;;; Other random VOPs.


(defknown mach::do-pending-interrupt () (values))
(define-vop (mach::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate mach::do-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))

