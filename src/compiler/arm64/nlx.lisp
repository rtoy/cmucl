;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/nlx.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definitions of VOPs used for non-local exit
;;; (throw, lexical exit, etc.)
;;;
;;; Written by Rob MacLachlan (SPARC original).
;;; Translated to ARM64 (AArch64) by [ARM64 port contributors].
;;;
;;; AArch64 differences from SPARC to keep in mind throughout this file:
;;;
;;;   1. STACK DIRECTION: AArch64 control stack grows DOWNWARD.  Frame slots
;;;      are at negative offsets from CFP-TN.  LOAD-STACK-TN / STORE-STACK-TN
;;;      already account for this by negating TN offsets.
;;;
;;;   2. OFFSET RANGE: SPARC uses a signed-13-bit immediate for ADD; AArch64
;;;      uses a 12-bit unsigned immediate for ADD/SUB (0..4095 bytes) and a
;;;      9-bit signed immediate for LDUR/STUR (-256..255 bytes).  For offsets
;;;      that do not fit, materialise the value in a non-descriptor temp and
;;;      use the register-offset form.
;;;
;;;   3. LI REPLACEMENT: SPARC's single-instruction LI becomes ARM64's
;;;      (inst li ...) pseudo-instruction (MOVZ + optional MOVKs).
;;;
;;;   4. BRANCH CONDITIONS: SPARC B :EQ / B :LT → ARM64 B.EQ / B.LT (the
;;;      condition codes are set the same way; we use the same keywords).
;;;
;;;   5. COMPARISON: SPARC (inst cmp reg) tests against zero; ARM64 has no
;;;      single-operand CMP.  Use (inst cmp reg zero-tn) or (inst cbz reg lbl).
;;;
;;;   6. LOOP COPY: SPARC LDN/STN (indexed load/store with byte index) →
;;;      ARM64 (inst ldr tn (reg-offset base index)) / (inst str tn (reg-offset base index)).
;;;      reg-offset uses a 64-bit register offset with no hardware scaling (LSL 0),
;;;      so NUM must be a raw byte counter, not a tagged fixnum.
;;;
;;;   7. compute-lra-from-code: same pseudo-instruction name as SPARC.

(in-package "ARM64")

;;; MAKE-NLX-SP-TN  --  Interface
;;;
;;;    Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(def-vm-support-routine make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *fixnum-primitive-type* immediate-arg-scn)
   env))

;;; Make-NLX-Entry-Argument-Start-Location  --  Interface
;;;
;;;    Make a TN for the argument count passing location for a non-local entry.
;;;
(def-vm-support-routine make-nlx-entry-argument-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))


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
(def-vm-support-routine make-dynamic-state-tns ()
  (make-n-tns 4 *any-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (nfp :scs (descriptor-reg))
	    (nsp :scs (descriptor-reg))
	    (eval :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:generator 13
    (emit-not-implemented)
    (load-symbol-value catch lisp::*current-catch-block* ndescr)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move nfp cur-nfp)))
    (move nsp nsp-tn)
    (load-symbol-value eval lisp::*eval-stack-top* ndescr)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
	 (nfp :scs (descriptor-reg))
	 (nsp :scs (descriptor-reg))
	 (eval :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:generator 10
    (emit-not-implemented)
    (store-symbol-value catch lisp::*current-catch-block* ndescr)
    (store-symbol-value eval lisp::*eval-stack-top* ndescr)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move cur-nfp nfp)))
    (move nsp-tn nsp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (emit-not-implemented)
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (emit-not-implemented)
    (move res bsp-tn)))


;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
;;; On AArch64 the control stack grows downward, so frame slots live at
;;; *negative* byte offsets from CFP-TN.  SPARC added the positive TN offset;
;;; here we subtract it.  The offset range check mirrors the SPARC signed-13
;;; guard, but for ARM64 we test against ADD's 12-bit unsigned immediate
;;; (0..4095).  Offsets that do not fit are materialised in NDESCR and a
;;; SUB-register form is used instead.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 22
    (emit-not-implemented)
    (let ((byte-offset (* (tn-offset tn) vm:word-bytes)))
      ;; Stack grows down: frame slot is at CFP - byte-offset.
      (if (typep byte-offset '(unsigned-byte 12))
	  (inst sub block cfp-tn byte-offset)
	  (progn
	    (inst li ndescr byte-offset)
	    (inst sub block cfp-tn ndescr))))
    (load-symbol-value temp lisp::*current-unwind-protect-block* ndescr)
    (storew temp block vm:unwind-block-current-uwp-slot)
    (storew cfp-tn block vm:unwind-block-current-cont-slot)
    (storew code-tn block vm:unwind-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp block vm:catch-block-entry-pc-slot)))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (descriptor-reg any-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block :to (:result 0)) result)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 44
    (emit-not-implemented)
    (let ((byte-offset (* (tn-offset tn) vm:word-bytes)))
      ;; Stack grows down: frame slot is at CFP - byte-offset.
      (if (typep byte-offset '(unsigned-byte 12))
	  (inst sub result cfp-tn byte-offset)
	  (progn
	    (inst li ndescr byte-offset)
	    (inst sub result cfp-tn ndescr))))
    (load-symbol-value temp lisp::*current-unwind-protect-block* ndescr)
    (storew temp result vm:catch-block-current-uwp-slot)
    (storew cfp-tn result vm:catch-block-current-cont-slot)
    (storew code-tn result vm:catch-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp result vm:catch-block-entry-pc-slot)

    (storew tag result vm:catch-block-tag-slot)
    (load-symbol-value temp lisp::*current-catch-block* ndescr)
    (storew temp result vm:catch-block-previous-catch-slot)
    (store-symbol-value result lisp::*current-catch-block* ndescr)

    (move block result)))


;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 7
    (emit-not-implemented)
    (let ((byte-offset (* (tn-offset tn) vm:word-bytes)))
      ;; Stack grows down: frame slot is at CFP - byte-offset.
      (if (typep byte-offset '(unsigned-byte 12))
	  (inst sub new-uwp cfp-tn byte-offset)
	  (progn
	    (inst li ndescr byte-offset)
	    (inst sub new-uwp cfp-tn ndescr))))
    (store-symbol-value new-uwp lisp::*current-unwind-protect-block* ndescr)))


(define-vop (unlink-catch-block)
  (:temporary (:scs (any-reg)) block)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (emit-not-implemented)
    (load-symbol-value block lisp::*current-catch-block* ndescr)
    (loadw block block vm:catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block* ndescr)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (any-reg)) block)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (emit-not-implemented)
    (load-symbol-value block lisp::*current-unwind-protect-block* ndescr)
    (loadw block block vm:unwind-block-current-uwp-slot)
    (store-symbol-value block lisp::*current-unwind-protect-block* ndescr)))


;;;; NLX entry VOPs:

(define-vop (nlx-entry)
  ;; Note: we can't list SC restrictions for these args because any load
  ;; VOPs would be inserted before the LRA.
  (:args (sp)
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-not-implemented)
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (cond
      ;; No values: nothing to do.
      ((zerop nvals))

      ;; Exactly one value.
      ;; Pre-load NIL as the default, then conditionally overwrite with the
      ;; actual value.  CBZ skips the load if count is zero, leaving NIL.
      ;; Slot 0 from start: byte offset (- (* 0 8) 0) = 0, always fits.
      ((= nvals 1)
       (let ((no-values (gen-label)))
	 ;; Default the result to NIL.  If no value was supplied we are done.
	 (move (tn-ref-tn values) null-tn)
	 (inst cbz count no-values)
	 ;; At least one value was supplied: overwrite the default.
	 (loadw (tn-ref-tn values) start 0 0 ndescr)
	 (emit-label no-values)))

      ;; Multiple values.
      (t
       (collect ((defaults))
	 ;; On SPARC, SUBCC lived in the delay slot of B :LT and therefore
	 ;; always executed — decrementing count for the *next* value test
	 ;; regardless of whether the branch was taken.  ARM64 has no delay
	 ;; slots, so we restructure: SUBS comes first (tests and decrements),
	 ;; then B :LT (branches if the pre-decremented count was already
	 ;; zero, i.e. this value was not supplied), then LOADW (only reached
	 ;; when the value is present).  The initial SUBS before the loop
	 ;; handles the first value's test; each subsequent SUBS at the top
	 ;; of the next iteration handles that value's test.
	 (inst subs count count (fixnumize 1))
	 (do ((i 0 (1+ i))
	      (tn-ref values (tn-ref-across tn-ref)))
	     ((null tn-ref))
	   (let ((default-lab (gen-label))
		 (tn (tn-ref-tn tn-ref)))
	     (defaults (cons default-lab tn))

	     ;; Branch if this value was not supplied (count went negative).
	     (inst b :lt default-lab)
	     ;; Value is present: load it.  Pass ndescr so loadw can
	     ;; materialise large offsets when i exceeds the signed-9 range.
	     (sc-case tn
	       ((descriptor-reg any-reg)
		(loadw tn start i 0 ndescr))
	       (control-stack
		(loadw move-temp start i 0 ndescr)
		(store-stack-tn tn move-temp ndescr)))
	     ;; Decrement for the next value's test (was the delay-slot
	     ;; instruction on SPARC; must precede the next B :LT here).
	     (inst subs count count (fixnumize 1))))

	 (let ((defaulting-done (gen-label)))

	   (emit-label defaulting-done)

	   (assemble (*elsewhere*)
	     (dolist (def (defaults))
	       (emit-label (car def))
	       (let ((tn (cdr def)))
		 (sc-case tn
		   ((descriptor-reg any-reg)
		    (move tn null-tn))
		   (control-stack
		    (store-stack-tn tn null-tn ndescr)))))
	     (inst b defaulting-done))))))

    ;; Restore CSP from the saved SP.  This is unconditional and lives
    ;; outside the cond — it must execute for all nvals cases.
    (load-stack-tn csp-tn sp ndescr)))


(define-vop (nlx-entry-multiple)
  ;; No SC restrictions for args (loading would happen before the entry label).
  (:args (top :target result) (src) (count))
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (any-reg) :from (:argument 0))
	    (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-not-implemented)
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (let ((loop (gen-label))
	  (done (gen-label)))

      ;; Setup results, and test for the zero value case.
      (load-stack-tn result top ndescr)
      ;; NUM must be initialised before the branch: on SPARC (inst li num 0)
      ;; lived in the delay slot of B :EQ and always executed; ARM64 has no
      ;; delay slots so we hoist it above the CBZ.
      (inst li num 0)
      (inst cbz count done)

      ;; Compute dst as one slot (word-bytes) below result, because we
      ;; add the byte index before we use it.  ARM64 stack grows down, so
      ;; "below" in address terms means result - word-bytes.
      (inst sub dst result vm:word-bytes)

      ;; Copy values down the stack.  NUM is a raw byte counter stepped
      ;; by word-bytes.  reg-offset addressing uses it directly as a
      ;; 64-bit register offset with no hardware scaling (LSL 0).
      ;;
      ;; SPARC loop structure (delay slot shown explicitly):
      ;;   loop: ldn temp, src[num]
      ;;         add num, fixnumize(1)
      ;;         cmp num, count
      ;;         b :ne loop
      ;;         stn temp, dst[num]   ; delay slot -- always executes
      ;;
      ;; ARM64: no delay slots, so the store must come before B :NE.
      (emit-label loop)
      (inst ldr temp (reg-offset src num))
      (inst add num num vm:word-bytes)
      (inst cmp num count)
      ;; Store with the post-incremented num, matching the SPARC delay slot.
      (inst str temp (reg-offset dst num))
      (inst b :ne loop)

      ;; Reset the CSP: result + num bytes consumed.
      (emit-label done)
      (inst add csp-tn result num))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-not-implemented)
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)))
