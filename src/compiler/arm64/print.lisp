;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/print.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains VOPs for things like printing during %initial-function
;;; before the world is initialized.
;;;
;;; Originally written by William Lott (SPARC).
;;; Ported to ARM64 (AArch64) from the SPARC backend.
;;;
;;; Porting notes:
;;;
;;;   SPARC  inst JAL lip, temp  (jump-and-link through temp, return addr in lip)
;;;   ARM64  inst BLR lip        (branch-with-link to address in lip; X30 = LR)
;;;
;;;   AArch64 has no branch-delay slots, so the SPARC (inst nop) filler
;;;   after JAL is removed.
;;;
;;;   The calling convention for call_into_c is:
;;;     cfunc  (X9)  -- address of the C function to call (debug_print)
;;;     lip    (X29) -- loaded with address of call_into_c, then BLR lip
;;;
;;;   nl0 (X0) carries the single Lisp-object argument and, after the call,
;;;   the return value, matching the AAPCS64 C ABI first-argument/return
;;;   register.

(in-package "ARM64")


(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc any-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 100
    (emit-not-implemented)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move nl0 object)
      (inst li cfunc (make-fixup (extern-alien-name "debug_print") :foreign))
      (inst li lip (make-fixup (extern-alien-name "call_into_c") :foreign))
      ;; AArch64 BLR: branch to address in LIP, hardware link register (X30)
      ;; receives the return address.  No delay slot.
      (inst blr lip)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
