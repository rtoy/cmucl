;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/debug.lisp,v 1.11 1992/02/21 22:03:26 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/debug.lisp,v 1.11 1992/02/21 22:03:26 wlott Exp $
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; 
(in-package "MIPS")


(define-vop (debug-cur-sp)
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res fp-tn)))

(define-vop (read-control-stack)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg negative-immediate zero immediate)))
  (:arg-types system-area-pointer positive-fixnum)
  (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (sc-case offset
      ((zero)
       (inst lw result object 0))
      ((negative-immediate immediate)
       (inst lw result object (* (tn-value offset) vm:word-bytes)))
      ((any-reg)
       (inst addu sap object offset)
       (inst lw result sap 0)))))

(define-vop (write-control-stack)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg negative-immediate zero immediate))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
  (:generator 5
    (sc-case offset
      ((zero)
       (inst sw value object 0))
      ((negative-immediate immediate)
       (inst sw value object (* (tn-value offset) vm:word-bytes)))
      ((any-reg)
       (inst addu sap object offset)
       (inst sw value sap 0)))
    (move result value)))

(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
	  (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst srl temp vm:type-bits)
      (inst beq temp bogus)
      (inst sll temp (1- (integer-length vm:word-bytes)))
      (unless (= lowtag vm:other-pointer-type)
	(inst addu temp (- lowtag vm:other-pointer-type)))
      (inst subu code thing temp)
      (emit-label done)
      (assemble (*elsewhere*)
	(emit-label bogus)
	(inst b done)
	(move code null-tn)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate function-code-header)
  (:variant vm:function-pointer-type))

(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))

(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 function-pointer-type)
    (inst srl res vm:type-bits)))
