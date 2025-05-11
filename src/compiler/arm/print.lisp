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
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:vop-var vop)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc non-descriptor-reg) temp)
  (:temporary (:sc any-reg) c-stack-pointer)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 100
    (emit-not-implemented)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	;; Save NFP to the Lisp stack
	(load-symbol-value temp *number-frame-pointer*)
	(store-stack-tn nfp-save temp))
      ;; Allocate space on the C stack.  call_into_c expects at least
      ;; 5 arguments on the stack: the C function to be called, and 4
      ;; more words to be copied to r0-r3.
      (load-symbol-value c-stack-pointer *number-stack-pointer*)
      (inst sub c-stack-pointer c-stack-pointer (* 5 vm:word-bytes))
      (store-symbol-value c-stack-pointer *number-stack-pointer*)

      ;; Instead of leaving random garbage, initialize the other stack
      ;; slots with 0.
      (inst mov temp 0)
      (inst str temp c-stack-pointer (* 4 vm:word-bytes))
      (inst str temp c-stack-pointer (* 3 vm:word-bytes))
      (inst str temp c-stack-pointer (* 2 vm:word-bytes))

      ;; Save the object to the C stack.  This is the first (and only)
      ;; arg to debug_print.
      (inst str object c-stack-pointer vm:word-bytes)

      ;; Get address of debug_print.  Then save to the C stack.
      (inst li temp (make-fixup (extern-alien-name "debug_print") :foreign))
      (inst str temp c-stack-pointer 0)

      ;; Get address of call_into_c.
      (inst li temp (make-fixup (extern-alien-name "call_into_c") :foreign))

      ;; Go!
      (inst blx temp)

      ;; Back.  Restore NFP
      (when cur-nfp
	(load-stack-tn temp nfp-save)
	(store-symbol-value temp *number-frame-pointer*))
      ;; Put the single return value to the result
      (move result nl0))))
