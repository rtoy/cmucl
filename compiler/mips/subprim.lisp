;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/subprim.lisp,v 1.3 1990/04/01 17:43:30 wlott Exp $
;;;
;;;    Linkage information for standard static functions, and random vops.
;;;
;;; Written by William Lott.
;;; 
(in-package "C")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
	      count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:node-var node)
  (:generator 50
    (let ((done (gen-label))
	  (loop (gen-label))
	  (not-list (generate-cerror-code node di:object-not-list-error
					  object)))
      (move ptr object)
      (inst beq ptr null-tn done)
      (move count zero-tn)
      (simple-test-simple-type ptr temp not-list t vm:list-pointer-type)

      (emit-label loop)
      (loadw ptr ptr vm:cons-cdr-slot vm:list-pointer-type)
      (inst addiu count count (fixnum 1))
      (simple-test-simple-type ptr temp loop nil vm:list-pointer-type)

      (cerror-call done di:object-not-list-error ptr)

      (emit-label done)
      (move result count))))


(define-static-function length (object) :translate length)

