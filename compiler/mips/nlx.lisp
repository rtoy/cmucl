;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/nlx.lisp,v 1.2 1990/02/27 00:12:10 wlott Exp $
;;;
;;;    This file contains the definitions of VOPs used for non-local exit
;;; (throw, lexical exit, etc.)
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Save and restore dynamic environment.
;;;
;;;    These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and binding
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.  If there
;;; were any additional stacks, then this would be the place to restore the top
;;; pointers.


;;; Make-Dynamic-State-TNs  --  Interface
;;;
;;;    Return a list of TNs that can be used to snapshot the dynamic state for
;;; use with the Save/Restore-Dynamic-Environment VOPs.
;;;
(defun make-dynamic-state-tns ()
  (make-n-tns 4 *any-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (special :scs (descriptor-reg))
	    (number :scs (descriptor-reg))
	    (eval :scs (descriptor-reg)))
  (:generator 13
    (load-symbol-value catch lisp::*current-catch-block*)
    (move special bsp-tn)
    (move number nsp-tn)
    (load-symbol-value eval lisp::*eval-stack-top*)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
	 (special :scs (descriptor-reg))
	 (number :scs (descriptor-reg))
	 (eval :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 10
    (let ((done (gen-label))
	  (skip (gen-label))
	  (loop (gen-label)))

      (store-symbol-value catch lisp::*current-catch-block*)
      (store-symbol-value eval lisp::*eval-stack-top*)
      (move nsp-tn number)
      
      (inst beq special bsp-tn done)
      (nop)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst beq symbol zero-tn skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
      (emit-label skip)
      (inst addiu bsp-tn bsp-tn (* -2 vm:word-bytes))
      (inst bne bsp-tn special loop)
      (nop)

      (emit-label done))))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))


;;;; Unwind miscop VOPs:

(define-vop (unwind)
  (:translate %continue-unwind)
  (:args (block-arg :target block)
	 (start :target args)
	 (count :target nargs))
  (:temporary (:sc any-reg :offset (first register-arg-offsets)
		   :from (:argument 0)) block)
  (:temporary (:sc any-reg :offset args-offset :from (:argument 1)) args)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 2)) nargs)
  (:temporary (:scs (any-reg) :type fixnum) cur-uwp target-uwp next-uwp)
  (:temporary (:scs (descriptor-reg)) return-pc)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:node-var node)
  (:generator 0
    (let ((error (generate-error-code node di:invalid-unwind-error))
	  (do-uwp (gen-label))
	  (do-exit (gen-label)))
      (move block block-arg)
      (inst beq block zero-tn error)
      
      (move args start)
      (move nargs count)
      
      (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
      (loadw target-uwp block vm:unwind-block-current-uwp-slot)
      (inst bne cur-uwp target-uwp do-uwp)
      (nop)
      
      (move cur-uwp block)

      (emit-label do-exit)
      
      (loadw cont-tn cur-uwp vm:unwind-block-current-cont-slot)
      (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
      (loadw return-pc cur-uwp vm:unwind-block-entry-pc-slot)
      (lisp-return return-pc lip)
	     
      (emit-label do-uwp)

      (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
      (b do-exit)
      (store-symbol-value next-uwp lisp::*current-unwind-protect-block*))))


(define-vop (throw)
  (:args (target)
	 (start)
	 (count))
  (:temporary (:scs (any-reg) :type fixnum)
	      catch)
  (:temporary (:scs (descriptor-reg))
	      tag)
  (:node-var node)
  (:generator 0
    (let ((loop (gen-label))
	  (exit (gen-label))
	  (error (generate-error-code node di:unseen-throw-tag-error target)))
      (load-symbol-value catch lisp::*current-catch-block*)

      (emit-label loop)

      (inst bne catch zero-tn error)

      (loadw tag catch vm:catch-block-tag-slot)
      (inst beq tag target exit)
      (nop)
      (b loop)
      (loadw catch catch vm:catch-block-previous-catch-slot)

      (emit-label exit)

      ;; ### Need to call unwind somehow.

    )))



;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Cont, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn)
	 (entry-offset :scs (any-reg descriptor-reg)))
  (:results (block :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block) result)
  (:generator 22
    (inst addiu result cont-tn (* (tn-offset tn) vm:word-bytes))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp result vm:unwind-block-current-uwp-slot)
    (storew cont-tn result vm:unwind-block-current-cont-slot)
    (storew code-tn result vm:unwind-block-current-code-slot)
    (storew entry-offset result vm:unwind-block-entry-pc-slot)
    (move block result)))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (any-reg descriptor-reg))
	 (entry-offset :scs (any-reg descriptor-reg)))
  (:results (block :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block) result)
  (:generator 44
    (inst addiu result cont-tn (* (tn-offset tn) vm:word-bytes))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp result vm:catch-block-current-uwp-slot)
    (storew cont-tn result vm:catch-block-current-cont-slot)
    (storew code-tn result vm:catch-block-current-code-slot)
    (storew entry-offset result vm:catch-block-entry-pc-slot)

    (storew tag result vm:catch-block-tag-slot)
    (load-symbol-value temp lisp::*current-catch-block-slot*)
    (storew temp result vm:catch-block-previous-catch-slot)
    (store-symbol-value result lisp::*current-catch-block*)

    (move block result)))


;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst addiu new-uwp cont-tn (* (tn-offset tn) vm:word-bytes))
    (store-symbol-value new-uwp lisp::*current-unwind-protect-block*)))


(define-vop (unlink-catch-block)
  (:temporary (:scs (descriptor-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-catch-block*)
    (loadw block block vm:catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block*)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (descriptor-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-unwind-protect-block*)
    (loadw block block vm:unwind-block-current-uwp-slot)
    (store-symbol-value block lisp::*current-unwind-protect-block*)))


;;;; NLX entry VOPs:
;;;
;;;    We can't just make these miscop variants, since they take funny wired
;;; operands.
;;;

(define-vop (nlx-entry)
  (:args (top :scs (descriptor-reg))
	 (start)
	 (count))
  (:results (values :more t))
  (:info nvals)
  (:save-p :force-to-stack)
  #+nil
  (:generator 30
    (unless (location= a0 top)
      (inst lr a0 top))
    (inst miscopx 'clc::nlx-entry-default-values)
    (inst cal a1 zero-tn nvals)))


(define-vop (nlx-entry-multiple)
  (:args (top :scs (descriptor-reg))
	 (start)
	 (count))
  (:save-p :force-to-stack)
  #+nil
  (:generator 30
    (unless (location= a0 top)
      (inst lr a0 top))
    (inst miscop 'clc::nlx-entry-receive-values)
    (unless (location= a0 r)
      (inst lr r a0))
    (unless (location= a1 r1)
      (inst lr r1 a1))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:generator 0))
