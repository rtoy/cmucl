;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/nlx.lisp,v 1.6 1990/03/20 00:09:42 wlott Exp $
;;;
;;;    This file contains the definitions of VOPs used for non-local exit
;;; (throw, lexical exit, etc.)
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")


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

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res bsp-tn)))



;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Cont, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block) result)
  (:generator 22
    (inst addiu result cont-tn (* (tn-offset tn) vm:word-bytes))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp result vm:unwind-block-current-uwp-slot)
    (storew cont-tn result vm:unwind-block-current-cont-slot)
    (storew code-tn result vm:unwind-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label)
    (storew temp result vm:catch-block-entry-pc-slot)
    (move block result)))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (any-reg descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block) result)
  (:generator 44
    (inst addiu result cont-tn (* (tn-offset tn) vm:word-bytes))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp result vm:catch-block-current-uwp-slot)
    (storew cont-tn result vm:catch-block-current-cont-slot)
    (storew code-tn result vm:catch-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label)
    (storew temp result vm:catch-block-entry-pc-slot)

    (storew tag result vm:catch-block-tag-slot)
    (load-symbol-value temp lisp::*current-catch-block*)
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


(define-vop (nlx-entry)
  (:args (sp :scs (descriptor-reg))
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:node-var node)
  (:generator 30
    (emit-return-pc label)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (let ((no-values (gen-label)))
	     (inst beq count zero-tn no-values)
	     (move (tn-ref-tn values) null-tn)
	     (loadw (tn-ref-tn values) start)
	     (emit-label no-values)))
	  (t
	   (collect ((defaults))
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))
		 
		 (inst beq count zero-tn default-lab)
		 (inst addiu count count (fixnum -1))
		 (sc-case tn
		   ((descriptor-reg any-reg)
		    (loadw tn start i))
		   (control-stack
		    (loadw move-temp start i)
		    (store-stack-tn tn move-temp)))))
		    
	     (let ((defaulting-done (gen-label)))
	       
	       (emit-label defaulting-done)
	       
	       (unassemble
		(assemble-elsewhere node
		  (dolist (def (defaults))
		    (emit-label (car def))
		    (let ((tn (cdr def)))
		      (sc-case tn
			((descriptor-reg any-reg)
			 (move tn null-tn))
			(control-stack
			 (store-stack-tn tn null-tn)))))
		  (b defaulting-done)
		  (nop)))))))
    (move csp-tn sp)))


(define-vop (nlx-entry-multiple)
  (:args (top :scs (descriptor-reg) :target dst)
	 (start :target src)
	 (count :target num))
  (:results (new-start) (new-count))
  (:info label)
  (:temporary (:scs (any-reg descriptor-reg) :type fixnum :from (:argument 0))
	      dst)
  (:temporary (:scs (any-reg descriptor-reg) :type fixnum :from (:argument 1))
	      src)
  (:temporary (:scs (any-reg descriptor-reg) :type fixnum :from (:argument 2))
	      num)
  (:temporary (:scs (descriptor-reg)) temp)
  (:save-p :force-to-stack)
  (:generator 30
    (emit-return-pc label)
    (let ((loop (gen-label))
	  (done (gen-label)))
      
      ;; Copy args.
      (move dst top)
      (move src start)
      (move num count)
      
      ;; Establish results.
      (move new-start dst)
      (inst beq num zero-tn done)
      (move new-count num t)
      
      ;; Copy stuff on stack.
      (emit-label loop)
      (loadw temp src)
      (inst addiu src src (fixnum 1))
      (storew temp dst)
      (inst addiu num num (fixnum -1))
      (inst bne num zero-tn loop)
      (inst addiu dst dst (fixnum 1))

      (emit-label done)
      (move csp-tn dst))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:generator 0
    (emit-return-pc label)))

