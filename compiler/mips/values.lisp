;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
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
	   (st tn start-temp i))
	  (stack
	   (load-stack-tn temp tn)
	   (st temp start-temp i)))))
    (move start start-temp)
    (inst ori count zero-tn nvals)))


;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
;;;
#+nil
(define-vop (values-list one-arg-two-value-miscop)
  (:variant 'clc::values-list))
