;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/values.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the implementation of unknown-values VOPs.
;;;
;;; Written by Rob MacLachlan
;;;
;;; 

(in-package "ARM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1
    (emit-not-implemented)
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
  (:args (vals :more t))
  (:results (start :scs (any-reg) :from :load)
	    (count :scs (any-reg)))
  (:info nvals)
  (:generator 20
    (emit-not-implemented)))

;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
;;;
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg)))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
	    (count :scs (any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (emit-not-implemented)))



;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
;;;
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg))
	 (skip :scs (any-reg immediate))
	 (num :scs (any-reg)))
  (:arg-types * positive-fixnum positive-fixnum)
  (:results (start :scs (any-reg))
	    (count :scs (any-reg)))
  (:generator 20
    (emit-not-implemented)))
