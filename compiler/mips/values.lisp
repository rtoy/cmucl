;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/values.lisp,v 1.6 1990/04/03 03:01:51 wlott Exp $
;;;
;;;    This file contains the implementation of unknown-values VOPs.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for MIPS by William Lott.
;;; 

(in-package "C")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1
    (move csp-tn ptr)))


;;; Push some values onto the stack, returning the start and number of values
;;; pushed as results.  It is assumed that the Vals are wired to the standard
;;; argument locations.  Nvals is the number of values to push.
;;;
;;; The generator cost is pseudo-random.  We could get it right by defining a
;;; bogus SC that reflects the costs of the memory-to-memory moves for each
;;; operand, but this seems unworthwhile.
;;;
(define-vop (push-values)
  (:args
   (vals :more t))
  (:results
   (start :scs (descriptor-reg))
   (count :scs (any-reg descriptor-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)
	       :to (:result 0)
	       :target start)
	      start-temp)
  (:generator 20
    (move start-temp csp-tn)
    (inst addi csp-tn csp-tn (* nvals vm:word-bytes))
    (do ((val vals (tn-ref-across val))
	 (i 0 (1+ i)))
	((null val))
      (let ((tn (tn-ref-tn val)))
	(sc-case tn
	  (descriptor-reg
	   (storew tn start-temp i))
	  (control-stack
	   (load-stack-tn temp tn)
	   (storew temp start-temp i)))))
    (move start start-temp)
    (loadi count (fixnum nvals))))


;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
;;;
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg descriptor-reg))
	    (count :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg) :type list :from (:argument 0)) list)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:generator 0
    (let ((loop (gen-label))
	  (done (gen-label)))

      (move list arg)
      (move start csp-tn)

      (emit-label loop)
      (inst beq list null-tn done)
      (loadw temp list vm:cons-car-slot vm:list-pointer-type)
      (loadw list list vm:cons-cdr-slot vm:list-pointer-type)
      (inst addiu csp-tn csp-tn vm:word-bytes)
      (storew temp csp-tn -1)
      (test-simple-type list ndescr loop nil vm:list-pointer-type)
      (error-call di:bogus-argument-to-values-list-error list)

      (emit-label done)
      (inst sub count csp-tn start))))

