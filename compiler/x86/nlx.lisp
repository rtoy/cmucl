;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/nlx.lisp,v 1.4.2.1 1997/09/09 01:23:21 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition of non-local exit for the x86.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996.
;;;
(in-package :x86)

;;; MAKE-NLX-SP-TN  --  Interface
;;;
;;;    Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(def-vm-support-routine make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *fixnum-primitive-type* any-reg-sc-number)
   env))

(defun catch-block-ea (tn)
  (assert (sc-is tn catch-block))
  (make-ea :dword :base ebp-tn
	   :disp (- (* (+ (tn-offset tn) catch-block-size) word-bytes))))


;;; Save and restore dynamic environment.
;;;
;;;    These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and eval
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.
;;;
;;; We don't need to save the BSP, because that is handled automatically.

;;; Make-Dynamic-State-TNs  --  Interface
;;;
;;;    Return a list of TNs that can be used to snapshot the dynamic state for
;;; use with the Save/Restore-Dynamic-Environment VOPs.
;;;
(def-vm-support-routine make-dynamic-state-tns ()
  (make-n-tns 4 *any-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (eval :scs (descriptor-reg))
	    (oldfp :scs (descriptor-reg))
	    (alien-stack :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    (load-symbol-value catch lisp::*current-catch-block*)
    (load-symbol-value eval lisp::*eval-stack-top*)
    (load-symbol-value alien-stack *alien-stack*)
    (loadw oldfp ebp-tn  (- (1+ old-fp-save-offset)))))
  

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
	 (eval :scs (descriptor-reg))
	 (oldfp :scs (descriptor-reg))
	 (alien-stack :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    (store-symbol-value catch lisp::*current-catch-block*)
    (store-symbol-value eval lisp::*eval-stack-top*)
    (store-symbol-value alien-stack *alien-stack*)
    (storew oldfp ebp-tn (- (1+ old-fp-save-offset)))))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg immediate-stack)))
  (:generator 1
    (move res esp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (load-symbol-value res *binding-stack-pointer*)))


;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.

(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:temporary (:sc dword-reg) temp)
  (:results (block :scs (any-reg)))
  (:generator 22
    (inst lea block (catch-block-ea tn))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp block unwind-block-current-uwp-slot)
    (storew ebp-tn block unwind-block-current-cont-slot)
    ;; The code slot is unused - fill with 0.
    (storew 0 block vm:unwind-block-current-code-slot)
    (storew (make-fixup nil :code-object entry-label)
	    block catch-block-entry-pc-slot)))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;

(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (descriptor-reg) :to (:result 1)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:sc descriptor-reg) temp)
  (:generator 44
    (inst lea block (catch-block-ea tn))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp block  unwind-block-current-uwp-slot)
    (storew ebp-tn block  unwind-block-current-cont-slot)
    ;; The code slot is unused - fill with 0.
    (storew 0 block vm:catch-block-current-code-slot)
    (storew (make-fixup nil :code-object entry-label)
	    block catch-block-entry-pc-slot)
    (storew tag block catch-block-tag-slot)
    (load-symbol-value temp lisp::*current-catch-block*)
    (storew temp block catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block*)))

;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:sc dword-reg) new-uwp)
  (:generator 7
    (inst lea new-uwp (catch-block-ea tn))
    (store-symbol-value new-uwp lisp::*current-unwind-protect-block*)))

(define-vop (unlink-catch-block)
  (:temporary (:sc dword-reg) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block*)))

(define-vop (unlink-unwind-protect)
    (:temporary (:sc dword-reg) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-unwind-protect-block*)
    (loadw block block unwind-block-current-uwp-slot)
    (store-symbol-value block lisp::*current-unwind-protect-block*)))


;;;; NLX entry VOPs:
#+nil ;; this is bogus?
(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
	      ; would be inserted before the return-pc label.
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:sc descriptor-reg) nil-temp move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (let ((no-values (gen-label)))
	     ;; (move (tn-ref-tn values) nil-value) -- jrd
	     (inst mov (tn-ref-tn values) nil-value)

	     (inst jecxz no-values)

	     ;; jrd rewrote this piece.  because we can't issue an sc-restriction, 
	     ;; as above, we generate a bogus EA here (see the expansion of LOADW)
	     ;; and croak.  Do the move by hand.
	     ;; (loadw (tn-ref-tn values) start -1)
	     (inst mov move-temp start)
	     (loadw (tn-ref-tn values) move-temp -1)

	     (emit-label no-values)))
	  (t
	   (collect ((defaults))
	     (inst mov nil-temp nil-value)
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))
		 
		 (inst cmp count (fixnum i))
		 (inst jmp :le default-lab)
		 (sc-case tn
		   ((descriptor-reg any-reg)
		    ;; see above -- jrd
		    ;; (loadw tn start (- (1+ i)))
		    (inst mov move-temp start)
		    (loadw tn move-temp (- (1+ i))))
		   ((descriptor-stack immediate-stack)
		    ;; (loadw move-temp start (- (1+ i)))
		    (inst mov move-temp start)
		    (loadw move-temp move-temp (- (1+ i)))
		    (move tn move-temp)))))
	     (let ((defaulting-done (gen-label)))
	       (emit-label defaulting-done)
	       (assemble (*elsewhere*)
		 (dolist (def (defaults))
		   (emit-label (car def))
		   (move (cdr def) nil-temp))
		 (inst jmp defaulting-done))))))
    (inst mov esp-tn sp)))

;;; see comments below regarding start arg to nlx-entry-multiple
(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
	      ; would be inserted before the return-pc label.
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:sc descriptor-reg) nil-temp move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (let ((no-values (gen-label)))
	     ;; (move (tn-ref-tn values) nil-value) -- jrd
	     (inst mov (tn-ref-tn values) nil-value)
	     
	     (inst jecxz no-values)
	     
	     (move move-temp start)
	     (loadw (tn-ref-tn values) move-temp -1)

	     (emit-label no-values)))
	  (t
	   (collect ((defaults))
	     (inst mov nil-temp nil-value)
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))
		 
		 (inst cmp count (fixnum i))
		 (inst jmp :le default-lab)
		 (sc-case tn
		   ((descriptor-reg any-reg)
		    ;; see above -- jrd
		    ;; (loadw tn start (- (1+ i)))
		    (move move-temp start)
		    (loadw tn move-temp (- (1+ i))))
		   ((descriptor-stack immediate-stack)
		    ;; (loadw move-temp start (- (1+ i)))
		    (move move-temp start)
		    (loadw move-temp move-temp (- (1+ i)))
		    (move tn move-temp)))))
	     (let ((defaulting-done (gen-label)))
	       (emit-label defaulting-done)
	       (assemble (*elsewhere*)
		 (dolist (def (defaults))
		   (emit-label (car def))
		   (move (cdr def) nil-temp))
		 (inst jmp defaulting-done))))))
    (inst mov esp-tn sp)))

#+nil ;;; this is badly broken and just kept here for reference -pw
      ;;; actually maybe it's not so broken -- check later
(define-vop (nlx-entry-multiple)
  (:args (top)
	 (source)
	 (count :target ecx))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  ;;(:temporary (:sc dword-reg :offset esi-offset) esi)
  (:temporary (:sc dword-reg :offset ecx-offset :from (:argument 2)) ecx)
  (:temporary (:sc dword-reg :offset esi-offset) esi)
  (:temporary (:sc dword-reg :offset edi-offset) edi)
  (:results (result :scs (any-reg) :from (:argument 0))
	    (num :scs (any-reg immediate-stack)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    #+x86-lra
    (progn 
      (align lowtag-bits #x90)
      (inst lra-header-word)
      (inst nop)
      (inst nop)
      (inst nop))
    (emit-label label)
    (note-this-location vop :non-local-entry)

    ;; Set up the result values and copy the arguments.
    (move result top)

    ;; -- jrd.  see comments above.  need to do extra moves by hand, to
    ;; get the ea's into registers
    ;; (inst lea esi (make-ea :dword :base source :disp (- word-bytes)))
    (inst lea edi (make-ea :dword :base result :disp (- word-bytes)))
    (inst mov esi source)
    (inst lea esi (make-ea :dword :base esi :disp (- word-bytes)))
    (inst mov edi result)
    (inst lea edi (make-ea :dword :base edi :disp (- word-bytes)))

    (move ecx count)			; fixnum words == bytes
    (move num ecx)
    (inst shr ecx word-shift)		; word count for <rep movs>
    ;; If we got zero, we be done.
    (inst jecxz done)

    ;; Copy them down.
    (inst std)
    (inst rep)
    (inst movs :dword)

    ;; Reset the CSP.
    DONE
    (inst lea esp-tn (make-ea :dword :base edi :disp word-bytes))))

(define-vop (nlx-entry-multiple)
  (:args (top)
	 (source)
	 (count :target ecx))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:sc dword-reg :offset ecx-offset :from (:argument 2)) ecx)
  (:temporary (:sc dword-reg :offset esi-offset) esi)
  (:temporary (:sc dword-reg :offset edi-offset) edi)
  (:results (result :scs (any-reg) :from (:argument 0))
	    (num :scs (any-reg immediate-stack)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    #+x86-lra
    (progn 
      (align lowtag-bits #x90)
      (inst lra-header-word)
      (inst nop)
      (inst nop)
      (inst nop))
    (emit-label label)
    (note-this-location vop :non-local-entry)

    ;; The RISC implementations have old-fp-save wired to a register.
    ;; ir2tran calls (make-old-fp-save-location) to retrieve that tn
    ;; as the source address for the thrown results. Doing that here
    ;; provides the S0 slot as that source which is the wrong end of
    ;; the stack. Examination of the throw-catch-unwind process shows
    ;; that %ebx is the analog of OCFP here and at least in testing so
    ;; far does actually point (1 above)  the values.
    ;; Actually, now it seems that 'unwind' is supposed to set up
    ;; edx and old-fp with certain values so that the compiler can
    ;; call these nlx entry points correctly. NOT.
    (move esi source)			; old-fp
    (inst lea esi (make-ea :dword :base esi :disp (- word-bytes)))
    ;; The 'top' arg contains the %esp value saved prior to creating
    ;; the catch block and dynamic save structures and points to where
    ;; the thrown values should sit.
    (move edi top)
    (move result edi)
    ;; not too sure here -- yes! this seems to be correct
    (inst lea edi (make-ea :dword :base edi :disp (- word-bytes)))

    (move ecx count)			; fixnum words == bytes
    (move num ecx)
    (inst shr ecx word-shift)		; word count for <rep movs>
    ;; If we got zero, we be done.
    (inst jecxz done)
    ;; Copy them down.
    (inst std)
    (inst rep)
    (inst movs :dword)

    ;; Reset the CSP (at last moved arg??)
    DONE
    (inst lea esp-tn (make-ea :dword :base edi :disp word-bytes))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  #+info-only
  (:args (target)(block)(start)(count))
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    #+x86-lra
    (progn 
      (align lowtag-bits #x90)
      (inst lra-header-word)
      (inst nop)
      (inst nop)
      (inst nop))
    (emit-label label)
    (note-this-location vop :non-local-entry)))
