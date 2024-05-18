;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/debug.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; 
(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")

(defknown di::current-sp () system-area-pointer (movable flushable))
(defknown di::current-fp () system-area-pointer (movable flushable))
(defknown di::stack-ref (system-area-pointer index) t (flushable))
(defknown di::%set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown di::lra-code-header (t) t (movable flushable))
(defknown di::function-code-header (t) t (movable flushable))
(defknown di::make-lisp-obj ((unsigned-byte 32)) t (movable flushable))
(defknown di::get-lisp-obj-address (t) (unsigned-byte 32) (movable flushable))
(defknown di::function-word-offset (function) index (movable flushable))

(define-vop (debug-cur-sp)
  (:translate di::current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (emit-not-implemented)
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate di::current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (emit-not-implemented)
    (move res cfp-tn)))

(define-vop (read-control-stack)
  (:translate kernel:stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (emit-not-implemented)
    (inst ldr result sap offset)))

(define-vop (write-control-stack)
  (:translate kernel:%set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (emit-not-implemented)
    (inst str value sap offset)
    (move result value)))

(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:variant-vars lowtag)
  (:generator 5
    (emit-not-implemented)))

(define-vop (code-from-lra code-from-mumble)
  (:translate di::lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate di::function-code-header)
  (:variant vm:function-pointer-type))

(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate di::make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (emit-not-implemented)
    (move result value)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (emit-not-implemented)
    (move result thing)))


(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate di::function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (emit-not-implemented)
    (loadw res fun 0 function-pointer-type)
    (inst lsr res res vm:type-bits)))
