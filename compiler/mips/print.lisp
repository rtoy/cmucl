;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/print.lisp,v 1.6 1990/11/03 03:25:43 wlott Exp $
;;;
;;; This file contains temporary printing utilities and similar noise.
;;;
;;; Written by William Lott.

(in-package "MIPS")


(define-vop (print)
  (:args (object :scs (descriptor-reg) :target a0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset 2) v0)
  (:temporary (:sc descriptor-reg :offset 4 :from (:argument 0)) a0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:sc any-reg :offset code-offset) code)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((lra-label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (move a0 object)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst addu nsp-tn nsp-tn -16)
      (inst compute-lra-from-code lra code lra-label temp)
      (inst li v0 (make-fixup "debug_print" :foreign))
      (inst li temp (make-fixup "call_into_c" :foreign))
      (inst j temp)
      (inst nop)

      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (inst addu nsp-tn nsp-tn 16)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move result v0))))
