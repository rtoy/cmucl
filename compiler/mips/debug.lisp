;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/debug.lisp,v 1.5 1990/11/03 03:25:29 wlott Exp $
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; 
(in-package "MIPS")

(in-package "DI")
(import '(c::current-fp))
(import '(current-sp current-fp stack-ref %set-stack-ref lra-code-header
		     function-code-header make-lisp-obj get-lisp-obj-address)
	(find-package "C"))

(in-package "MIPS")


(defknown current-sp () system-area-pointer (movable flushable))
(defknown current-fp () system-area-pointer (movable flushable))
(defknown stack-ref (system-area-pointer index) t (flushable))
(defknown %set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown lra-code-header (t) t (movable flushable))
(defknown function-code-header (t) t (movable flushable))
(defknown make-lisp-obj ((unsigned-byte 32)) t (movable flushable))
(defknown get-lisp-obj-address (t) (unsigned-byte 32) (movable flushable))

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

(define-vop (read-control-stack sap-ref)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:variant :long nil))

(define-vop (write-control-stack sap-set)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg negative-immediate zero immediate))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:variant :long))

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
