;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/assem-rtns.lisp,v 1.16 1990/06/16 14:39:43 wlott Exp $
;;;
;;;
(in-package "C")



;;;; The undefined-function.

;;; Just signal an undefined-symbol error.  Note: this must look like a
;;; function, because it magically gets called in place of a function when
;;; there is no real function to call.

(eval-when (eval)

(define-assembly-routine (undefined-function
			  ()
			  (:temp cname any-reg cname-offset)
			  (:temp lexenv any-reg lexenv-offset)
			  (:temp function any-reg code-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp temp non-descriptor-reg nl0-offset))
  ;; Allocate function header.
  (align vm:lowtag-bits)
  (inst word vm:function-header-type)
  (dotimes (i (1- vm:function-header-code-offset))
    (inst word 0))
  ;; Cause the error.
  (cerror-call continue undefined-symbol-error cname)

  continue

  (let ((not-sym (generate-cerror-code object-not-symbol-error cname)))
    (test-simple-type cname temp not-sym t vm:symbol-header-type))

  (loadw lexenv cname vm:symbol-function-slot vm:other-pointer-type)
  (loadw function lexenv vm:closure-function-slot vm:function-pointer-type)
  (lisp-jump function lip))

); eval-when (eval)


;;;; Non-local exit noise.


(define-assembly-routine (unwind
			  ((:translate %continue-unwind)
			   (:policy :fast-safe))
			  (:arg block any-reg a0-offset)
			  (:arg start any-reg old-fp-offset)
			  (:arg count any-reg nargs-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp cur-uwp any-reg nl0-offset)
			  (:temp next-uwp any-reg nl1-offset)
			  (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code invalid-unwind-error)))
    (inst beq block zero-tn error))
  
  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst bne cur-uwp target-uwp do-uwp)
  (inst nop)
      
  (move cur-uwp block)

  do-exit
      
  (loadw fp-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
  (lisp-return lra lip :frob-code nil)

  do-uwp

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (inst b do-exit)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block*))



(define-assembly-routine (throw
			  ()
			  (:arg target descriptor-reg a0-offset)
			  (:arg start any-reg old-fp-offset)
			  (:arg count any-reg nargs-offset)
			  (:temp catch any-reg a1-offset)
			  (:temp tag descriptor-reg a2-offset)
			  (:temp ndescr non-descriptor-reg nl0-offset))
  
  (load-symbol-value catch lisp::*current-catch-block*)
  
  loop
  
  (let ((error (generate-error-code unseen-throw-tag-error target)))
    (inst beq catch zero-tn error)
    (inst nop))
  
  (loadw tag catch vm:catch-block-tag-slot)
  (inst beq tag target exit)
  (inst nop)
  (loadw catch catch vm:catch-block-previous-catch-slot)
  (inst b loop)
  (inst nop)
  
  exit
  
  (move target catch)
  (inst li ndescr (make-fixup 'unwind :assembly-routine))
  (inst j ndescr)
  (inst nop))


