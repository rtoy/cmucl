;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/system.lisp,v 1.8 1992/04/14 02:59:28 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    MIPS VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Mips conversion by William Lott and Christopher Hoover.
;;;
(in-package "SPARC")



;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and result object vm:lowtag-mask)))

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
	  (done (gen-label)))
      (test-type object ndescr other-ptr nil vm:other-pointer-type)
      (test-type object ndescr function-ptr nil vm:function-pointer-type)
      (inst andcc result object
	    (logand vm:other-immediate-0-type vm:other-immediate-1-type))
      (inst b :eq done)
      (inst nop)

      (inst b done)
      (inst and result object vm:type-mask)

      (emit-label function-ptr)
      (load-type result object (- vm:function-pointer-type))
      (inst b done)
      (inst nop)

      (emit-label other-ptr)
      (load-type result object (- vm:other-pointer-type))
      (inst nop)
      
      (emit-label done))))

(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- vm:function-pointer-type))))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst stb type function (- 3 function-pointer-type))
    (move result type)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:other-pointer-type)
    (inst srl res res vm:type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:function-pointer-type)
    (inst srl res res vm:type-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) t1 t2)
  (:generator 6
    (loadw t1 x 0 vm:other-pointer-type)
    (inst and t1 vm:type-mask)
    (sc-case data
      (any-reg
       (inst sll t2 data (- vm:type-bits 2))
       (inst or t1 t2))
      (immediate
       (inst or t1 (ash (tn-value data) vm:type-bits)))
      (zero))
    (storew t1 x 0 vm:other-pointer-type)
    (move res x)))


(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (inst sll res ptr 3)
    (inst srl res res 1)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (inst sll temp val vm:type-bits)
       (inst or res temp (tn-value type)))
      (t
       (inst sra temp type 2)
       (inst sll res val (- vm:type-bits 2))
       (inst or res res temp)))))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move int alloc-tn)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int bsp-tn)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))


;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 vm:other-pointer-type)
    (inst srl ndescr vm:type-bits)
    (inst sll ndescr vm:word-shift)
    (inst sub ndescr vm:other-pointer-type)
    (inst add sap code ndescr)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 vm:other-pointer-type)
    (inst srl ndescr vm:type-bits)
    (inst sll ndescr vm:word-shift)
    (inst add ndescr offset)
    (inst add ndescr (- vm:function-pointer-type vm:other-pointer-type))
    (inst add func code ndescr)))



;;;; Other random VOPs.


(defknown unix::do-pending-interrupt () (values))
(define-vop (unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate unix::do-pending-interrupt)
  (:generator 1
    (inst unimp pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst unimp halt-trap)))



;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
	   (- (* (+ index vector-data-offset) word-bytes) other-pointer-type)))
      (assert (typep offset '(signed-byte 13)))
      (inst ld count count-vector offset)
      (inst add count 1)
      (inst st count count-vector offset))))
