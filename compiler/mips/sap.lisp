;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/sap.lisp,v 1.2 1990/03/22 23:50:34 ch Exp $
;;;
;;;    This file contains the MIPS VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;
(in-package "C")



;;;; The SAP-MOVE vop.

(define-vop (sap-move)
  (:args (x :target y :scs (sap-reg) :load nil))
  (:results (y :scs (sap-reg) :load nil))
  (:temporary (:scs (sap-reg) :type system-area-pointer
		    :from :argument :to :result) sap)
  (:temporary (:scs (descriptor-reg) :from :argument :to :result) temp)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:effects)
  (:affected)
  (:generator 0
    (sc-case x ((sap-reg sap-stack immediate-sap descriptor-reg control-stack)))
    (sc-case y ((sap-reg sap-stack descriptor-reg control-stack)))

    (let* ((x-sap-p (sc-is x sap-reg sap-stack immediate-sap))
	   (y-sap-p (sc-is y sap-reg sap-stack))
	   (src (cond ((sc-is x sap-reg descriptor-reg) x)
		      (x-sap-p sap)
		      (t temp)))
	   (dst (cond ((sc-is y sap-reg descriptor-reg) y)
		      (y-sap-p sap)
		      (t temp))))

      (sc-case x
	((control-stack sap-stack)
	 (load-stack-tn src x))
	((descriptor-reg sap-reg))
	(immediate-sap
	 (loadi src (tn-value x))))
      (cond ((and x-sap-p (not y-sap-p))
	     (pseudo-atomic (ndescr)
	       (inst addiu dst alloc-tn vm:other-pointer-type)
	       (inst addiu alloc-tn alloc-tn (pad-data-block vm:sap-size))
	       (loadi ndescr (logior (ash vm:sap-size vm:type-bits)
				     vm:sap-type))
	       (storew ndescr dst 0 vm:other-pointer-type)
	       (storew src dst vm:sap-pointer-slot vm:other-pointer-type)))
	    ((and y-sap-p (not x-sap-p))
	     (loadw sap src vm:sap-pointer-slot vm:other-pointer-type))
	    (t
	     (move dst src)))
      (sc-case y
	((control-stack sap-stack)
	 (store-stack-tn y dst))
	((sap-reg descriptor-reg))))))

(primitive-type-vop sap-move
		    (:coerce-to-t :coerce-from-t :move)
		    system-area-pointer)



;;;; SAP-INT and INT-SAP

(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:results (int :scs (any-reg descriptor-reg)))
  (:arg-types system-area-pointer)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 0
    ;; ### Need to check for fixnum overflow.
    (inst sll int sap 2)))

(define-vop (int-sap)
  (:args (int :scs (any-reg descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 0
    ;; ### Need to check to see if it is a bignum.
    (inst srl sap int 2)))



;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
	 (offset :scs (any-reg descriptor-reg)))
  (:arg-types system-area-pointer fixnum)
  (:results (res :scs (sap-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst addu res ptr offset)))

(define-vop (pointer-)
  (:args (ptr1 :scs (sap-reg))
	 (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst subu res ptr1 ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(define-vop (sap-ref)
  (:variant-vars size signed)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (descriptor-reg any-reg)))
  (:arg-types system-area-pointer fixnum)
  (:results (result :scs (descriptor-reg any-reg)))
  (:temporary (:scs (sap-reg) :type system-area-pointer :from (:argument 0))
	      sap)
  (:temporary (:scs (non-descriptor-reg) :type random
		    :to (:result 0) :target result)
	      temp)
  (:generator 5
    (move sap object)
    (ecase size
      (:byte
       (inst sra temp offset 2)
       (inst addu sap sap temp)
       (if signed
	   (inst lb temp sap 0)
	   (inst lbu temp sap 0))
       (inst sll result temp 2))
      (:short
       (inst sra temp offset 1)
       (inst addu sap sap temp)
       (if signed
	   (inst lh temp sap 0)
	   (inst lhu temp sap 0))
       (inst sll result temp 2))
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
	  (move result temp)))))))


(define-vop (sap-set)
  (:variant-vars size)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (descriptor-reg any-reg))
	 (value :scs (descriptor-reg sap-reg any-reg) :target temp))
  (:temporary (:scs (sap-reg) :type system-area-pointer :from (:argument 0))
	      sap)
  (:temporary (:scs (non-descriptor-reg) :type random :from (:result 3)) temp)
  (:generator 5
    (move sap object)
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



(define-vop (sap-system-ref sap-ref)
  (:translate sap-ref-sap)
  (:results (result :scs (sap-reg)))
  (:variant :pointer nil))

(define-vop (sap-system-set sap-set)
  (:translate (setf sap-ref-sap))
  (:arg-types system-area-pointer fixnum system-area-pointer)
  (:variant :pointer))



(define-vop (32bit-system-ref sap-ref)
  (:translate sap-ref-32)
  (:variant :long nil))

(define-vop (signed-32bit-system-ref sap-ref)
  (:translate signed-sap-ref-32)
  (:variant :long t))

(define-vop (32bit-system-set sap-set)
  (:translate (setf sap-ref-32))
  (:arg-types system-area-pointer fixnum t)
  (:variant :long))


(define-vop (16bit-system-ref sap-ref)
  (:translate sap-ref-16)
  (:variant :short nil))

(define-vop (signed-16bit-system-ref sap-ref)
  (:translate signed-sap-ref-16)
  (:variant :short t))

(define-vop (16bit-system-set sap-set)
  (:translate (setf sap-ref-16))
  (:arg-types system-area-pointer fixnum fixnum)
  (:variant :short))


(define-vop (8bit-system-ref sap-ref)
  (:translate sap-ref-8)
  (:variant :byte nil))

(define-vop (signed-8bit-system-ref sap-ref)
  (:translate signed-sap-ref-8)
  (:variant :byte t))

(define-vop (8bit-system-set sap-set)
  (:translate (setf sap-ref-8))
  (:arg-types system-area-pointer fixnum fixnum)
  (:variant :byte))



;;;; ### Noise to allow old forms to continue to work until they are gone.

(macrolet ((frob (prim func)
	     `(def-primitive-translator ,prim (&rest args)
		(warn "Someone used %primitive ~S -- should be ~S."
		      ',prim ',func)
		`(,',func ,@args))))
  (frob 32bit-system-ref sap-ref-32)
  (frob unsigned-32bit-system-ref sap-ref-32)
  (frob 16bit-system-ref sap-ref-16)
  (frob 8bit-system-ref sap-ref-8))

(macrolet ((frob (prim func)
	     `(def-primitive-translator ,prim (&rest args)
		(warn "Someone used %primitive ~S -- should be ~S."
		      ',prim (list 'setf ',func))
		`(setf ,',func ,@args))))
  (frob 32bit-system-set sap-ref-32)
  (frob signed-32bit-system-set sap-ref-32)
  (frob 16bit-system-set sap-ref-16)
  (frob 8bit-system-set sap-ref-8))
