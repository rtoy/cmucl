;;; -*- Package: RT; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/rt/debug.lisp,v 1.1 1991/02/18 15:07:50 chiles Exp $
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; Converted to RT by Bill Chiles.
;;;

(in-package "RT")


(defknown di::current-sp () system-area-pointer (movable flushable))
(defknown di::current-fp () system-area-pointer (movable flushable))
(defknown di::stack-ref (system-area-pointer index) t (flushable))
(defknown di::%set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown di::lra-code-header (t) t (movable flushable))
(defknown di::function-code-header (t) t (movable flushable))
(defknown di::make-lisp-obj ((unsigned-byte 32)) t (movable flushable))
(defknown di::get-lisp-obj-address (t) (unsigned-byte 32) (movable flushable))

(define-vop (debug-cur-sp)
  (:translate di::current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate di::current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res cfp-tn)))


(define-system-ref read-control-stack 2 di::stack-ref descriptor-reg *)

(define-system-set write-control-stack 2 di::%set-stack-ref (descriptor-reg) *)


(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg) :target code))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (sap-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
	  (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst sr temp vm:type-bits)
      (inst bc :eq bogus)
      (inst sl temp (1- (integer-length vm:word-bytes)))
      (unless (= lowtag vm:other-pointer-type)
	(inst cal temp temp (- lowtag vm:other-pointer-type)))
      (move code thing)
      (inst s code temp)
      (emit-label done)
      (assemble (*elsewhere*)
	(emit-label bogus)
	(inst bx done)
	(move code null-tn)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate di::lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate di::function-code-header)
  (:variant vm:function-pointer-type))

(define-vop (di::make-lisp-obj)
  (:policy :fast-safe)
  (:translate di::make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

(define-vop (di::get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))
