;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/assem-rtns.lisp,v 1.20 1990/10/16 17:15:36 wlott Exp $
;;;
;;;
(in-package "C")


;;;; Return-multiple with other than one value

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     ()
     ;; These four are really arguments.
     (:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp old-fp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer
     (:temp lip interior-reg lip-offset)
     (:temp count any-reg nl2-offset)
     (:temp src any-reg nl3-offset)
     (:temp dst any-reg nl4-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))

  (inst blez nvals default-a0-and-on)
  (inst lw a0 vals (* 0 vm:word-bytes))
  (inst subu count nvals (fixnum 2))
  (inst blez count default-a2-and-on)
  (inst lw a1 vals (* 1 vm:word-bytes))
  (inst subu count (fixnum 1))
  (inst blez count default-a3-and-on)
  (inst lw a2 vals (* 2 vm:word-bytes))
  (inst subu count (fixnum 1))
  (inst blez count default-a4-and-on)
  (inst lw a3 vals (* 3 vm:word-bytes))
  (inst subu count (fixnum 1))
  (inst blez count default-a5-and-on)
  (inst lw a4 vals (* 4 vm:word-bytes))
  (inst subu count (fixnum 1))
  (inst blez count done)
  (inst lw a5 vals (* 5 vm:word-bytes))

  ;; Copy the remaining args to the top of the stack.
  (inst addu src vals (* 6 vm:word-bytes))
  (inst addu dst fp-tn (* 6 vm:word-bytes))

  LOOP
  (inst lw temp src)
  (inst addu src vm:word-bytes)
  (inst sw temp dst)
  (inst subu count (fixnum 1))
  (inst bne count zero-tn loop)
  (inst addu dst vm:word-bytes)
		
  (inst b done)
  (inst nop)

  DEFAULT-A0-AND-ON
  (inst move a0 null-tn)
  (inst move a1 null-tn)
  DEFAULT-A2-AND-ON
  (inst move a2 null-tn)
  DEFAULT-A3-AND-ON
  (inst move a3 null-tn)
  DEFAULT-A4-AND-ON
  (inst move a4 null-tn)
  DEFAULT-A5-AND-ON
  (inst move a5 null-tn)
  DONE
  
  ;; Clear the stack.
  (move old-fp-tn fp-tn)
  (move fp-tn old-fp)
  (inst addu csp-tn fp-tn nvals)
  
  ;; Return.
  (lisp-return return-pc lip))



;;;; tail-call-variable.

#+assembler ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     ()
     ;; These are really args.
     (:temp args any-reg nl0-offset)
     (:temp function descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; Needed for the jump
     (:temp lip interior-reg lip-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst subu nargs csp-tn args)
     
  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (inst lw a0 args (* 0 vm:word-bytes))
  (inst lw a1 args (* 1 vm:word-bytes))
  (inst lw a2 args (* 2 vm:word-bytes))
  (inst lw a3 args (* 3 vm:word-bytes))
  (inst lw a4 args (* 4 vm:word-bytes))
  (inst lw a5 args (* 5 vm:word-bytes))

  ;; Calc SRC, DST, and COUNT
  (inst addu count nargs (fixnum (- register-arg-count)))
  (inst blez count done)
  (inst addu src args (* vm:word-bytes register-arg-count))
  (inst addu dst fp-tn (* vm:word-bytes register-arg-count))
	
  LOOP
  ;; Copy one arg.
  (inst lw temp src)
  (inst addu src src vm:word-bytes)
  (inst sw temp dst)
  (inst addu count (fixnum -1))
  (inst bgtz count loop)
  (inst addu dst dst vm:word-bytes)
	
  DONE
  ;; Clear the number stack if anything is there.
  (let ((cur-nfp (current-nfp-tn vop)))
    (when cur-nfp
      (inst addu nsp-tn cur-nfp
	    (bytes-needed-for-non-descriptor-stack-frame))))

  ;; We are done.  Do the jump.
  (loadw temp lexenv vm:closure-function-slot vm:function-pointer-type)
  (lisp-jump temp lip))



;;;; Non-local exit noise.

(define-assembly-routine (unwind
			  ((:translate %continue-unwind)
			   (:policy :fast-safe))
			  (:arg block (any-reg descriptor-reg) a0-offset)
			  (:arg start (any-reg descriptor-reg) old-fp-offset)
			  (:arg count (any-reg descriptor-reg) nargs-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp cur-uwp any-reg nl0-offset)
			  (:temp next-uwp any-reg nl1-offset)
			  (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
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
  
  (progn start count) ; We just need them in the registers.

  (load-symbol-value catch lisp::*current-catch-block*)
  
  loop
  
  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
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
  (inst j (make-fixup 'unwind :assembly-routine))
  (inst nop))


