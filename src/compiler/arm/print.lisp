;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/print.lisp $")
;;;
;;; **********************************************************************
;;;
;;;
;;; This file contains VOPs for things like printing during %initial-function
;;; before the world is initialized.
;;;

(in-package "ARM")


(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:vop-var vop)
  (:generator 100
    (emit-not-implemented)))
