;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/print.lisp,v 1.2 1990/04/24 02:56:30 wlott Exp $
;;;
;;; This file contains temporary printing utilities and similar noise.
;;;
;;; Written by William Lott.

(in-package "C")


(define-vop (print)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset 2) v0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:sc any-reg :offset code-offset) code)
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:generator 0
    (let ((lra-label (gen-label)))
      (inst addu nsp-tn nsp-tn -16)
      (storew object nsp-tn 0)
      (inst li v0 (make-fixup "print" :foreign))
      (inst li temp (make-fixup "call_into_c" :foreign))
      (inst j temp)
      (inst compute-lra-from-code lra code lra-label)
      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (inst addu nsp-tn nsp-tn 16)
      (move result v0))))
