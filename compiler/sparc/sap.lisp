;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/sap.lisp,v 1.1 1990/11/30 17:05:01 wlott Exp $
;;;
;;; This file contains the SPARC VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")


;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:generator 1
    (loadw y x sap-pointer-slot other-pointer-type)))

;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; Move an untagged SAP to a tagged representation.
;;;
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (res :scs (descriptor-reg)))
  (:generator 1
    (with-fixed-allocation (res ndescr sap-type sap-size)
      (storew sap res sap-pointer-slot other-pointer-type))))
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
	 (fp :scs (any-reg)
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
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 1
    (move int sap)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))



;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
	 (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (sc-case offset
      (signed-reg
       (inst add res ptr offset))
      (immediate
       (inst add res ptr (tn-value offset))))))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
	 (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst sub res ptr1 ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(define-vop (sap-ref)
  (:policy :fast-safe)
  (:variant-vars size signed)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg signed-reg zero immediate)))
  (:arg-types system-area-pointer positive-fixnum)
  (:results (result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let* ((shift (ecase size
		    (:byte 0)
		    (:short 1)
		    ((:long :single :double) 2)))
	   (offset
	    (sc-case offset
	      (zero offset)
	      (immediate
	       (let ((offset (ash (tn-value offset) shift)))
		 (cond ((typep offset '(signed-byte 13))
			offset)
		       (t
			(inst li temp offset)
			temp))))
	      ((any-reg signed-reg)
	       (when (sc-is offset any-reg)
		 (decf shift 2))
	       (cond ((plusp shift)
		      (inst sll temp offset shift)
		      temp)
		     ((minusp shift)
		      (inst sra temp offset (- shift))
		      temp)
		     (t
		      offset))))))
      (ecase size
	(:byte
	 (if signed
	     (inst ldsb result sap offset)
	     (inst ldub result sap offset)))
	(:short
	 (if signed
	     (inst ldsh result sap offset)
	     (inst lduh result sap offset)))
	(:long
	 (inst ld result sap offset))
	(:single
	 (inst ldf result sap offset))
	(:double
	 (inst lddf result sap offset))))))


(define-vop (sap-set)
  (:policy :fast-safe)
  (:variant-vars size)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg signed-reg zero immediate))
	 (value :scs (signed-reg unsigned-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum (:or signed-num unsigned-num))
  (:results (result :scs (signed-reg unsigned-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let* ((shift (ecase size
		    (:byte 0)
		    (:short 1)
		    ((:long :single :double) 2)))
	   (offset
	    (sc-case offset
	      (zero offset)
	      (immediate
	       (let ((offset (ash (tn-value offset) shift)))
		 (cond ((typep offset '(signed-byte 13))
			offset)
		       (t
			(inst li temp offset)
			temp))))
	      ((any-reg signed-reg)
	       (when (sc-is offset any-reg)
		 (decf shift 2))
	       (cond ((plusp shift)
		      (inst sll temp offset shift)
		      temp)
		     ((minusp shift)
		      (inst sra temp offset (- shift))
		      temp)
		     (t
		      offset))))))
      (ecase size
	(:byte
	 (inst stb value sap offset)
	 (move result value))
	(:short
	 (inst sth value sap offset)
	 (move result value))
	(:long
	 (inst st value sap offset)
	 (move result value))
	(:single
	 (inst stf value sap offset)
	 (unless (location= result value)
	   (inst fmovs result value)))
	(:double
	 (inst stdf value sap offset)
	 (unless (location= result value)
	   (inst fmovs result value)
	   (inst fmovs-odd result value)))))))



(define-vop (sap-system-ref sap-ref)
  (:translate sap-ref-sap)
  (:results (result :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:variant :long nil))

(define-vop (sap-system-set sap-set)
  (:translate %set-sap-ref-sap)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg signed-reg zero immediate))
	 (value :scs (sap-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum system-area-pointer)
  (:results (result :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:variant :long))



(define-vop (32bit-system-ref sap-ref)
  (:translate sap-ref-32)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant :long nil))

(define-vop (signed-32bit-system-ref sap-ref)
  (:translate signed-sap-ref-32)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:variant :long t))

(define-vop (32bit-system-set sap-set)
  (:translate %set-sap-ref-32)
  (:arg-types system-area-pointer positive-fixnum
	      (:or unsigned-num signed-num))
  (:variant :long))


(define-vop (16bit-system-ref sap-ref)
  (:translate sap-ref-16)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:variant :short nil))

(define-vop (signed-16bit-system-ref sap-ref)
  (:translate signed-sap-ref-16)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:variant :short t))

(define-vop (16bit-system-set sap-set)
  (:translate %set-sap-ref-16)
  (:arg-types system-area-pointer positive-fixnum tagged-num)
  (:variant :short))


(define-vop (8bit-system-ref sap-ref)
  (:translate sap-ref-8)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:variant :byte nil))

(define-vop (signed-8bit-system-ref sap-ref)
  (:translate signed-sap-ref-8)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:variant :byte t))

(define-vop (8bit-system-set sap-set)
  (:translate %set-sap-ref-8)
  (:arg-types system-area-pointer positive-fixnum tagged-num)
  (:variant :byte))



;;; Noise to convert normal lisp data objects into SAPs.

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst add sap vector
	  (- (* vector-data-offset word-bytes) other-pointer-type))))



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
