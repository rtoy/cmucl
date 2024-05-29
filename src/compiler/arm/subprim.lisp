;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/subprim.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    Linkage information for standard static functions, and random vops.
;;;
(in-package "ARM")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg)))
  (:arg-types list)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (emit-not-implemented)))
       

(define-static-function length (object) :translate length)

