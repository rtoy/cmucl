;;; -*- Package: VM; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/sap.lisp,v 1.23 1992/02/21 23:57:03 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the MIPS VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;
(in-package "MIPS")


;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "system area pointer indirection")
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
  (:note "system area pointer allocation")
  (:generator 20
    (move sap x)
    (pseudo-atomic (ndescr)
      (inst addu y alloc-tn vm:other-pointer-type)
      (inst addu alloc-tn alloc-tn (vm:pad-data-block vm:sap-size))
      (inst li ndescr (logior (ash (1- vm:sap-size) vm:type-bits) vm:sap-type))
      (storew ndescr y 0 vm:other-pointer-type)
      (storew sap y vm:sap-pointer-slot vm:other-pointer-type))))
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
       (inst addu res ptr offset))
      (immediate
       (inst addu res ptr (tn-value offset))))))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
	 (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst subu res ptr1 ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(eval-when (compile eval)

(defmacro def-system-ref-and-set
	  (ref-name set-name sc type size &optional signed)
  `(progn
     (define-vop (,ref-name)
       (:translate ,ref-name)
       (:policy :fast-safe)
       (:args (object :scs (sap-reg) :target sap)
	      (offset :scs (unsigned-reg negative-immediate zero immediate)))
       (:arg-types system-area-pointer unsigned-num)
       (:results (result :scs (,sc)))
       (:result-types ,type)
       (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
       (:generator 5
	 (multiple-value-bind
	     (base offset)
	     (sc-case offset
	       ((zero)
		(values object 0))
	       ((negative-immediate immediate)
		(values object (tn-value offset)))
	       ((unsigned-reg)
		(inst addu sap object offset)
		(values sap 0)))
	   ,@(ecase size
	       (:byte
		(if signed
		    '((inst lb result base offset))
		    '((inst lbu result base offset))))
	       (:short
		(if signed
		    '((inst lh result base offset))
		    '((inst lhu result base offset))))
	       (:long
		'((inst lw result base offset)))
	       (:single
		'((inst lwc1 result base offset)))
	       (:double
		'((inst lwc1 result base offset)
		  (inst lwc1-odd result base (+ offset vm:word-bytes)))))
	   (inst nop))))
     (define-vop (,set-name)
       (:translate ,set-name)
       (:policy :fast-safe)
       (:args (object :scs (sap-reg) :target sap)
	      (offset :scs (unsigned-reg negative-immediate zero immediate))
	      (value :scs (,sc) :target result))
       (:arg-types system-area-pointer unsigned-num ,type)
       (:results (result :scs (,sc)))
       (:result-types ,type)
       (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
       (:generator 5
	 (multiple-value-bind
	     (base offset)
	     (sc-case offset
	       ((zero)
		(values object 0))
	       ((negative-immediate immediate)
		(values object (tn-value offset)))
	       ((unsigned-reg)
		(inst addu sap object offset)
		(values sap 0)))
	   ,@(ecase size
	       (:byte
		'((inst sb value base offset)
		  (move result value)))
	       (:short
		'((inst sh value base offset)
		  (move result value)))
	       (:long
		'((inst sw value base offset)
		  (move result value)))
	       (:single
		'((inst swc1 value base offset)
		  (unless (location= result value)
		    (inst move :single result value))))
	       (:double
		'((inst swc1 value base offset)
		  (inst swc1-odd value base (+ offset vm:word-bytes))
		  (unless (location= result value)
		    (inst move :double result value))))))))))

); eval-when (compile eval)

(def-system-ref-and-set sap-ref-8 %set-sap-ref-8
  unsigned-reg positive-fixnum :byte nil)
(def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
  signed-reg tagged-num :byte t)
(def-system-ref-and-set sap-ref-16 %set-sap-ref-16
  unsigned-reg positive-fixnum :short nil)
(def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
  signed-reg tagged-num :short t)
(def-system-ref-and-set sap-ref-32 %set-sap-ref-32
  unsigned-reg unsigned-num :long nil)
(def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
  signed-reg signed-num :long t)
(def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
  sap-reg system-area-pointer :long)
(def-system-ref-and-set sap-ref-single %set-sap-ref-single
  single-reg single-float :single)
(def-system-ref-and-set sap-ref-double %set-sap-ref-double
  double-reg double-float :double)


;;; Noise to convert normal lisp data objects into SAPs.

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst addu sap vector
	  (- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type))))



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
