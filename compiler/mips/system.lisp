;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/system.lisp,v 1.3 1990/03/06 19:59:02 ch Exp $
;;;
;;;    MIPS VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Mips conversion by William Lott and Christopher Hoover.
;;;
(in-package "C")

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
	 (offset :scs (any-reg descriptor-reg)))
  (:arg-types sap fixnum)
  (:results (res :scs (sap-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst addu res ptr offset)))

(define-vop (pointer-)
  (:args (ptr1 :scs (sap-reg))
	 (ptr2 :scs (sap-reg)))
  (:arg-types sap sap)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst subu res ptr1 ptr2)))

#+nil
(define-vop (vector-word-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec clc::g-vector-header-words)
    (inst niuo res res clc::g-vector-words-mask-16)))

#+nil
(define-vop (int-sap)
  (:args (x :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:translate int-sap)
  (:policy :fast-safe)
  #+nil
  (:generator 6
    (unless (location= res x)
      (inst lr res x))
    (let ((fixp (gen-label)))
      (test-simple-type res temp fixp t system:%bignum-type)
      (loadw res x (/ clc::bignum-header-size 4))
      (emit-label fixp))))

(define-vop (pointer-compare)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 3
    (three-way-comparison x y condition :unsigned not-p target temp)))

(macrolet ((frob (name cond)
	     `(progn
		(def-primitive-translator ,name (x y) `(,',name ,x ,y))
		(defknown ,name (t t) boolean (movable foldable flushable))
		(define-vop (,name pointer-compare)
		  (:translate ,name)
		  (:variant ,cond)))))
  (frob pointer< :lt)
  (frob pointer> :gt))

(define-vop (check-op)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:variant-vars condition not-p error)
  (:node-var node)
  (:policy :fast-safe)
  (:generator 3
    (let ((target (generate-error-code node error x y)))
      (three-way-comparison x y condition :signed not-p target temp))))

(define-vop (check<= check-op)
  (:variant :gt t di:not-<=-error)
  (:translate check<=))

(define-vop (check= check-op)
  (:variant :eq nil di:not-=-error)
  (:translate check=))



(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst sll res ptr 2)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate unsigned-immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      ((immediate unsigned-immediate)
       (inst sll temp val vm:type-bits)
       (inst ori res temp (tn-value type)))
      (t
       (inst sra temp type 2)
       (inst sll res val (- vm:type-bits 2))
       (inst or res res temp)))))




(define-vop (sap-ref)
  (:variant-vars size signed)
  (:args (object :scs (descriptor-reg sap-reg immediate-sap) :target sap)
	 (offset :scs (descriptor-reg any-reg)))
  (:arg-types sap fixnum)
  (:results (result :scs (descriptor-reg any-reg)))
  (:temporary (:scs (sap-reg) :type sap :from (:argument 0)) sap)
  (:temporary (:scs (non-descriptor-reg) :type random
		    :to (:result 0) :target result)
	      temp)
  (:generator 5
    (sc-case object
      (descriptor-reg
       (loadw sap object vm:sap-pointer-slot vm:other-pointer-type))
      (sap-reg
       (move sap object))
      (immediate-sap
       (loadi sap (tn-value object))))
    (ecase size
      (:byte
       (inst sra temp offset 2)
       (inst addu sap sap temp)
       (inst lb temp sap 0)
       (cond (signed
	      (inst sll temp temp 24)
	      (inst sra result temp 22))
	     (t
	      (inst sll result temp 2))))
      (:short
       (inst sra temp offset 1)
       (inst addu sap sap temp)
       (inst lh temp sap 0)
       (cond (signed
	      (inst sll temp temp 16)
	      (inst sra result temp 14))
	     (t
	      (inst sll result temp 2))))
      (:long
       (inst addu sap sap offset)
       (inst lw temp sap 0)
       ;; ### Need to assure that it doesn't overflow
       (inst sll result temp 2))
      (:pointer
       (inst addu sap sap offset)
       (inst lw temp sap 0)
       (sc-case result
	 (sap-reg
	  (move result temp))
	 (descriptor-reg
	  ;; ### Need to allocate the silly thing instead of stripping
	  ;; off the low two bits.
	  (inst sra temp temp 2)
	  (inst sll result temp 2)))))))


(define-vop (sap-set)
  (:variant-vars size)
  (:args (object :scs (descriptor-reg sap-reg immediate-sap) :target sap)
	 (offset :scs (descriptor-reg any-reg))
	 (value :scs (descriptor-reg sap-reg any-reg) :target temp))
  (:temporary (:scs (sap-reg) :type sap :from (:argument 0)) sap)
  (:temporary (:scs (non-descriptor-reg) :type random :from (:result 3)) temp)
  (:generator 5
    (sc-case object
      (descriptor-reg
       (loadw sap object vm:sap-pointer-slot vm:other-pointer-type))
      (sap-reg
       (move sap object))
      (immediate-sap
       (loadi sap (tn-value object))))
    (ecase size
      (:byte
       (inst sra temp offset 2)
       (inst addu sap sap temp))
      (:short
       (inst sra temp offset 1)
       (inst addu sap sap temp))
      ((:long :pointer)
       (inst addu sap sap offset)))
    (ecase size
      ((:byte :short)
       (inst sra temp value 2))
      (:long
       ;; ### Need to see if it's a fixnum or bignum
       (inst sra temp value 2))
      (:pointer
       (sc-case value
	 (sap-reg
	  (move temp value))
	 (descriptor-reg
	  (loadw temp value vm:sap-pointer-slot vm:other-pointer-type)))))
    (ecase size
      (:byte
       (inst sb temp sap 0))
      (:short
       (inst sh temp sap 0))
      ((:long :pointer)
       (inst sw temp sap 0)))))



(define-vop (pointer-sap-ref sap-ref)
  (:results (result :scs (descriptor-reg sap-reg)))
  (:variant :pointer nil))

(define-vop (pointer-sap-set sap-set)
  (:arg-types sap fixnum sap)
  (:variant :pointer))



(define-vop (32bit-system-ref sap-ref)
  (:variant :long nil))

(define-vop (signed-32bit-system-ref sap-ref)
  (:variant :long t))

(define-vop (32bit-system-set sap-set)
  (:arg-types sap fixnum t)
  (:variant :long))


(define-vop (16bit-system-ref sap-ref)
  (:translate sap-ref-16)
  (:variant :short nil))

(define-vop (signed-16bit-system-ref sap-ref)
  (:variant :short t))

(define-vop (16bit-system-set sap-set)
  (:translate (setf sap-ref-16))
  (:arg-types sap fixnum fixnum)
  (:variant :short))


(define-vop (8bit-system-ref sap-ref)
  (:translate sap-ref-8)
  (:variant :byte nil))

(define-vop (8bit-system-set sap-set)
  (:translate (setf sap-ref-8))
  (:arg-types sap fixnum fixnum)
  (:variant :byte))




(define-vop (current-sp)
  (:results (val :scs (any-reg descriptor-reg)))
  (:generator 1
    (move val csp-tn)))



(define-vop (halt)
  (:generator 1
    (inst break vm:halt-trap)))


;;; This guy makes sure that there aren't any random garbage pointers lying
;;; around in registers by clearing all of the boxed registers.  Our allocating
;;; all of the boxed registers as temporaries will prevent any TNs from being
;;; packed in those registers at the time this VOP is invoked.
;;;
#+nil
(define-vop (clear-registers)
  (:temporary (:sc any-reg :offset 1) a0)
  (:temporary (:sc any-reg :offset 3) a1)
  (:temporary (:sc any-reg :offset 5) a2)
  (:temporary (:sc any-reg :offset 4) t0)
  (:temporary (:sc any-reg :offset 7) l0)
  (:temporary (:sc any-reg :offset 8) l1)
  (:temporary (:sc any-reg :offset 9) l2)
  (:temporary (:sc any-reg :offset 10) l3)
  (:temporary (:sc any-reg :offset 11) l4)
  (:generator 10
    (inst lis a0 0)
    (inst lis a1 0)
    (inst lis a2 0)
    (inst lis t0 0)
    (inst lis l0 0)
    (inst lis l1 0)
    (inst lis l2 0)
    (inst lis l3 0)
    (inst lis l4 0)))
