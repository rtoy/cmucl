;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/sap.lisp,v 1.5 1990/04/25 21:45:44 wlott Exp $
;;;
;;;    This file contains the MIPS VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;
(in-package "C")


;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:generator 1
    (loadw y x vm:sap-pointer-slot vm:other-pointer-type)))

;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; Move an untagged SAP to a tagged representation.
;;;
(define-vop (move-from-sap)
  (:args (x :scs (sap-reg) :target sap))
  (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (y :scs (descriptor-reg)))
  (:generator 1
    (move sap x)
    (pseudo-atomic (ndescr)
      (inst addu y alloc-tn vm:other-pointer-type)
      (inst addu alloc-tn alloc-tn (vm:pad-data-block vm:sap-size))
      (inst li ndescr (logior (ash vm:sap-size vm:type-bits) vm:sap-type))
      (storew y ndescr 0 vm:other-pointer-type)
      (storew y sap vm:sap-pointer-slot vm:other-pointer-type))))
;;;
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))


;;; Move untagged sap values.
;;;
(define-vop (sap-move)
  (:args (x :target y
	    :scs (sap-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))


;;; Move untagged sap arguments/return-values.
;;;
(define-vop (move-sap-argument)
  (:args (x :target y
	    :scs (sap-reg))
	 (fp :scs (descriptor-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (sap-reg
       (move y x))
      (sap-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-sap-argument :move-argument
  (descriptor-reg sap-reg) (sap-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged sap to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (sap-reg) (descriptor-reg))



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
    ;; ### Need to check to see if it is a bignum first.
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
  (:policy :fast-safe)
  (:variant-vars size signed)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (descriptor-reg any-reg negative-immediate zero
				      immediate unsigned-immediate)))
  (:arg-types system-area-pointer fixnum)
  (:results (result :scs (signed-reg unsigned-reg)))
  (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (sc-case offset
      ((zero)
       (move sap object))
      ((negative-immediate immediate)
       (inst addu sap object
	     (ash (tn-value offset)
		  (ecase size (:byte 0) (:short 1) (:long 2)))))
      ((any-reg descriptor-reg)
       (ecase size
	 (:byte
	  (inst sra temp offset 2)
	  (inst addu sap object temp))
	 (:short
	  (inst sra temp offset 1)
	  (inst addu sap object temp))
	 ((:long :pointer)
	  (inst addu sap object offset)))))
    (ecase size
      (:byte
       (if signed
	   (inst lb result sap 0)
	   (inst lbu result sap 0)))
      (:short
       (if signed
	   (inst lh result sap 0)
	   (inst lhu result sap 0)))
      ((:long :pointer)
       (inst lw result sap 0)))
    (inst nop)))


(define-vop (sap-set)
  (:policy :fast-safe)
  (:variant-vars size)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (descriptor-reg any-reg negative-immediate
				      zero immediate))
	 (value :scs (signed-reg unsigned-reg) :target result))
  (:results (result :scs (signed-reg unsigned-reg sap-reg)))
  (:temporary (:scs (sap-reg) :from (:argument 0))
	      sap)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (sc-case offset
      ((zero)
       (move sap object))
      ((negative-immediate immediate)
       (inst addu sap object
	     (ash (tn-value offset)
		  (ecase size (:byte 0) (:short 1) (:long 2)))))
      ((any-reg descriptor-reg)
       (ecase size
	 (:byte
	  (inst sra temp offset 2)
	  (inst addu sap object temp))
	 (:short
	  (inst sra temp offset 1)
	  (inst addu sap object temp))
	 ((:long :pointer)
	  (inst addu sap object offset)))))
    (ecase size
      (:byte
       (inst sb value sap 0))
      (:short
       (inst sh value sap 0))
      ((:long :pointer)
       (inst sw value sap 0)))
    (move result value)))



(define-vop (sap-system-ref sap-ref)
  (:translate sap-ref-sap)
  (:results (result :scs (sap-reg)))
  (:variant :pointer nil))

(define-vop (sap-system-set sap-set)
  (:translate (setf sap-ref-sap))
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (descriptor-reg any-reg negative-immediate
				      zero immediate))
	 (value :scs (sap-reg) :target result))
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
  (:arg-types system-area-pointer fixnum *)
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
