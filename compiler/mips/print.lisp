;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/print.lisp,v 1.9 1992/07/28 20:37:46 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains temporary printing utilities and similar noise.
;;;
;;; Written by William Lott.

(in-package "MIPS")


(define-vop (print)
  (:args (object :scs (descriptor-reg) :target a0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset :target result :to (:result 0))
	      cfunc)
  (:temporary (:sc descriptor-reg :offset 4 :from (:argument 0)) a0)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (move a0 object)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst li cfunc (make-fixup "debug_print" :foreign))
      (inst jal (make-fixup "call_into_c" :foreign))
      (inst addu nsp-tn nsp-tn -16)
      (inst addu nsp-tn nsp-tn 16)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move result cfunc))))
