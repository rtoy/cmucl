;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/subprim.lisp,v 1.10 1990/04/26 20:27:18 wlott Exp $
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
  (:generator 50
    (let ((done (gen-label))
	  (loop (gen-label))
	  (not-list (generate-cerror-code di:object-not-list-error object)))
      (move ptr object)
      (move count zero-tn)

      (emit-label loop)

      (inst beq ptr null-tn done)
      (inst nop)

      (simple-test-simple-type ptr temp not-list t vm:list-pointer-type)

      (loadw ptr ptr vm:cons-cdr-slot vm:list-pointer-type)
      (inst addu count count (fixnum 1))
      (simple-test-simple-type ptr temp loop nil vm:list-pointer-type)

      (cerror-call done di:object-not-list-error ptr)

      (emit-label done)
      (move result count))))
       

(define-static-function length (object) :translate length)




;;;; Debugger support

(define-vop (current-sp)
  (:results (res :scs (sap-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (read-control-stack sap-ref)
  (:results (result :scs (descriptor-reg)))
  (:variant :long nil))

(define-vop (write-control-stack sap-set)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (descriptor-reg any-reg negative-immediate
				      zero immediate))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:variant :long))




;;;; Foreign function call interfaces.

(define-vop (foreign-symbol-address)
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:generator 2
    (inst li res (make-fixup foreign-symbol :foreign))))

(define-vop (call-out)
  (:args (args :more t))
  (:ignore args)
  (:save-p t)
  (:info function)
  (:results (result :scs (sap-reg signed-reg unsigned-reg)))
  (:temporary (:sc any-reg :offset 2) v0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:sc any-reg :offset code-offset) code)
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:generator 0
    (let ((lra-label (gen-label)))
      (inst li v0 (make-fixup function :foreign))
      (inst li temp (make-fixup "call_into_c" :foreign))
      (inst j temp)
      (inst compute-lra-from-code lra code lra-label)
      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (move result v0))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg)))
  (:generator 0
    (inst addu nsp-tn nsp-tn (- amount))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (inst addu nsp-tn nsp-tn amount)))
