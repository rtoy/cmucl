;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/print.lisp,v 1.1 1990/03/18 23:45:13 wlott Exp $
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
      (inst addiu nsp-tn nsp-tn -16)
      (storew object nsp-tn 0)
      (inst load-foreign-address v0 "print")
      (inst load-foreign-address temp "call_into_c")
      (inst jr temp)
      (inst compute-lra-from-code lra code lra-label)
      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (inst addiu nsp-tn nsp-tn 16)
      (move result v0))))
