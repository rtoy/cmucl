;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/compiler/x86/subprim.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Linkage information for standard static functions, and random vops.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; 
(in-package :x86)
(intl:textdomain "cmucl-x86-vm")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:temporary (:scs (unsigned-reg)) temp)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (let ((done (gen-label))
	  (loop (gen-label))
	  (not-list (generate-error-code vop object-not-list-error object)))
      (move ptr object)
      (inst xor count count)

      (inst cmp ptr nil-value)
      (inst jmp :e done)

      (emit-label loop)

      (loadw ptr ptr cons-cdr-slot list-pointer-type)
      (inst add count (fixnumize 1))

      (move temp ptr)
      (inst and temp lowtag-mask)
      (inst cmp temp list-pointer-type)
      (inst jmp :ne not-list)

      (inst cmp ptr nil-value)
      (inst jmp :ne loop)

      (emit-label done))))

#+ignore
(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:temporary (:scs (unsigned-reg)) temp)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (let ((done (gen-label))
	  (loop (gen-label))
	  (not-list (generate-error-code vop object-not-list-error object)))
      (move ptr object)
      (inst xor count count)

      (inst cmp ptr nil-value)
      (inst jmp :e done)

      (emit-label loop)

      (loadw ptr ptr cons-cdr-slot list-pointer-type)
      (inst add count (fixnumize 1))

      (move temp ptr)
      (inst and temp lowtag-mask)
      (inst cmp temp list-pointer-type)
      (inst jmp :ne not-list)

      (inst cmp ptr nil-value)
      (inst jmp :ne loop)

      (emit-label done))))


(define-static-function length (object) :translate length)

