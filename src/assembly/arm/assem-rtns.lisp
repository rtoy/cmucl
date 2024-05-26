;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm/assem-rtns.lisp $")
;;;
;;; **********************************************************************
;;;
;;;
(in-package "ARM")


;;;; Return-multiple with other than one value

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp lra descriptor-reg lra-offset)

     (:temp temp descriptor-reg lexenv-offset))

  ;; Note, because of the way the return-multiple vop is written, we can
  ;; assume that we are never called with nvals == 1 and that a0 has already
  ;; been loaded.
  (emit-not-implemented))



;;;; tail-call-variable.

#+assembler ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp temp descriptor-reg lexenv-offset))

  (emit-not-implemented))



;;;; Non-local exit noise.

(define-assembly-routine (unwind
			  (:return-style :none)
			  (:translate %continue-unwind)
			  (:policy :fast-safe))
			 ((:arg block (any-reg descriptor-reg) a0-offset)
			  (:arg start (any-reg descriptor-reg) ocfp-offset)
			  (:arg count (any-reg descriptor-reg) nargs-offset))
  (emit-not-implemented))


(define-assembly-routine (throw
			  (:return-style :none))
			 ((:arg target descriptor-reg a0-offset)
			  (:arg start any-reg ocfp-offset)
			  (:arg count any-reg nargs-offset))

  (emit-not-implemented))




;; Assembly routines for undefined_tramp and closure_tramp

#+assembler
(define-assembly-routine (closure-tramp-function-alignment
			  (:return-style :none))
                         ()
  ;; Align to a dualword and put in the magic function header stuff so
  ;; that closure-tramp looks like a normal function with a function
  ;; tag.
  (align vm:lowtag-bits)
  (inst byte 0))

#+assembler
(define-assembly-routine (closure-tramp
			  (:return-style :none))
                         ()
  (inst byte 0)
  (inst byte 0)
  (inst byte vm:function-header-type)
  ;; This is supposed to be closure-tramp, not 0.
  (inst word 0)
  (inst word (kernel:get-lisp-obj-address nil))
  (inst word (kernel:get-lisp-obj-address nil))
  (inst word (kernel:get-lisp-obj-address nil))
  (inst word (kernel:get-lisp-obj-address nil))

  (emit-not-implemented)
  ;; Make sure following routine is dual-word aligned
  (align vm:lowtag-bits))

#+assembler
(define-assembly-routine (undefined-tramp-function-alignment
			  (:return-style :none))
                         ()
  ;; Align to a dualword and put in the magic function header stuff so
  ;; that closure-tramp looks like a normal function with a function
  ;; tag.
  (align vm:lowtag-bits)
  (inst byte 0))

#+assembler
(define-assembly-routine (undefined-tramp
			  (:return-style :none))
                         ()
  (inst byte 0)
  (inst byte 0)
  (inst byte vm:function-header-type)
  ;; This is supposed to be undefined-tramp, not 0.
  (inst word 0)
  (inst word (kernel:get-lisp-obj-address nil))
  (inst word (kernel:get-lisp-obj-address nil))
  (inst word (kernel:get-lisp-obj-address nil))
  (inst word (kernel:get-lisp-obj-address nil))

  (emit-not-implemented))
