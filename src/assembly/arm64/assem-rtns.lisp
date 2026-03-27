;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm64/assem-rtns.lisp $")
;;;
;;; **********************************************************************
;;;
;;; ARM64 assembly routines.
;;;
;;; Ported from the SPARC backend (src/assembly/sparc/assem-rtns.lisp).
;;;
;;; Key differences from SPARC:
;;;
;;;   - ARM64 has 4 Lisp argument registers (a0-a3, X22-X25) vs SPARC's 6.
;;;     The return-multiple and tail-call-variable routines are adjusted
;;;     accordingly.
;;;
;;;   - ARM64 has no branch-delay slots.  All (inst nop) delay-slot fillers
;;;     from the SPARC version are removed.
;;;
;;;   - SPARC uses a SUBCC/B :GT loop; ARM64 uses SUBS/B.GT (SUBS sets flags,
;;;     the branch tests them).  We use INST SUBS for the countdown and
;;;     INST B.GT for the loop-back.
;;;
;;;   - SPARC's (inst cmp reg) tests reg against zero (CMP Rn, %G0).
;;;     ARM64 uses (inst cmp reg 0) or (inst cbz reg label) / (inst cbnz ...).
;;;     Here we use (inst cmp reg 0) / (inst b.le ...) to match the original
;;;     control flow exactly.
;;;
;;;   - Loads/stores:
;;;       SPARC  (inst ld  dst base offset-bytes)
;;;       ARM64  (loadw  dst base offset-words lowtag)   [from macros.lisp]
;;;     For the loop bodies where the pointer is bumped by one word each
;;;     iteration we use pre- or post-index addressing directly:
;;;       (inst ldr  temp (post-index src vm:word-bytes))
;;;       (inst str  temp (post-index dst vm:word-bytes))
;;;
;;;   - SPARC uses (inst j reg) for an indirect branch; ARM64 uses (inst br reg).
;;;     Computed Lisp jumps go through LISP-JUMP (adds function-code-offset
;;;     bias, moves to code-tn, branches via LIP-TN).
;;;
;;;   - The function-header tramp bytes differ:
;;;       SPARC  (inst byte 0)(inst byte 0)(inst byte vm:function-header-type)
;;;              then four words of nil / metadata.
;;;       ARM64  same layout, but the byte-sized header type tag is still
;;;              vm:function-header-type; keep the same structure.
;;;
;;;   - SPARC's (inst move dst src) → ARM64 (move dst src)  [same macro name].
;;;
;;;   - (inst subcc count nvals (fixnumize 6)) → (inst subs count nvals (fixnumize 4))
;;;     adjusted for register-arg-count = 4.
;;;
;;;   - Stack grows DOWN on ARM64 (CFP-TN is the frame base; slots are at
;;;     negative offsets).  LOADW / STOREW already handle this via the macros;
;;;     no change needed in the assembly code that calls those macros.
;;;

(in-package "ARM64")


;;;; Return-multiple with other than one value.

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

    ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer.
     (:temp count any-reg nl2-offset)
     (:temp src any-reg nl3-offset)
     (:temp dst any-reg nl4-offset)
     (:temp temp descriptor-reg cname-offset)

     ;; These are needed so we can get at the register args.
     ;; ARM64 has four Lisp argument registers (a0-a3).
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset))

  ;; Note: we are never called with nvals == 1 and a0 is already loaded.
  (inst cmp nvals 0)
  (inst b.le default-a0-and-on)
  (inst cmp nvals (fixnumize 2))
  (inst b.le default-a2-and-on)
  (loadw a1 vals 1)
  (inst cmp nvals (fixnumize 3))
  (inst b.le default-a3-and-on)
  (loadw a2 vals 2)
  (inst cmp nvals (fixnumize 4))
  (inst b.le done)
  (loadw a3 vals 3)

  ;; Copy the remaining args (beyond the 4 register args) to the top of
  ;; the control stack.
  (inst add src vals (* 4 vm:word-bytes))
  (inst add dst cfp-tn (* 4 vm:word-bytes))
  (inst subs count nvals (fixnumize 4))

  LOOP
  (inst ldr temp (post-index src vm:word-bytes))
  (inst str temp (post-index dst vm:word-bytes))
  (inst subs count count (fixnumize 1))
  (inst b.gt loop)

  (inst b done)

  DEFAULT-A0-AND-ON
  (move a0 null-tn)
  (move a1 null-tn)
  DEFAULT-A2-AND-ON
  (move a2 null-tn)
  DEFAULT-A3-AND-ON
  (move a3 null-tn)
  DONE

  ;; Clear the stack.
  (move ocfp-tn cfp-tn)
  (move cfp-tn ocfp)
  (inst add csp-tn ocfp-tn nvals)

  ;; Return.
  (lisp-return lra))


;;;; tail-call-variable.

#+assembler ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this.
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg cname-offset)

     ;; These are needed so we can get at the register args.
     ;; ARM64 has four Lisp argument registers (a0-a3).
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset))

  ;; Calculate NARGS (as a fixnum).
  (inst sub nargs csp-tn args)

  ;; Load the argument regs (must do this now, because the blt might
  ;; trash these locations).
  (loadw a0 args 0)
  (loadw a1 args 1)
  (loadw a2 args 2)
  (loadw a3 args 3)

  ;; Calc SRC, DST, and COUNT.
  (inst subs count nargs (fixnumize register-arg-count))
  (inst b.le done)
  (inst add src args (* vm:word-bytes register-arg-count))
  (inst add dst cfp-tn (* vm:word-bytes register-arg-count))

  LOOP
  ;; Copy one arg.
  (inst ldr temp (post-index src vm:word-bytes))
  (inst str temp (post-index dst vm:word-bytes))
  (inst subs count count (fixnumize 1))
  (inst b.gt loop)

  DONE
  ;; We are done.  Do the jump.
  (loadw temp lexenv vm:closure-function-slot vm:function-pointer-type)
  (lisp-jump temp))


;;;; Non-local exit noise.

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %continue-unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) a0-offset)
                          (:arg start (any-reg descriptor-reg) ocfp-offset)
                          (:arg count (any-reg descriptor-reg) nargs-offset)
                          (:temp lra descriptor-reg lra-offset)
                          (:temp cur-uwp any-reg nl0-offset)
                          (:temp next-uwp any-reg nl1-offset)
                          (:temp target-uwp any-reg nl2-offset)
                          ;; Scratch for load/store-symbol-value address materialisation.
                          ;; Must be non-descriptor: LI writes a raw pointer into it.
                          (:temp sym-temp non-descriptor-reg nl3-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst cmp block 0)
    (inst b.eq error))

  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block* sym-temp)
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst cmp cur-uwp target-uwp)
  (inst b.ne do-uwp)

  (move cur-uwp block)

  DO-EXIT

  (loadw cfp-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
  (lisp-return lra :frob-code nil)

  DO-UWP

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (inst b do-exit)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block* sym-temp))


(define-assembly-routine (throw
                          (:return-style :none))
                         ((:arg target descriptor-reg a0-offset)
                          (:arg start any-reg ocfp-offset)
                          (:arg count any-reg nargs-offset)
                          (:temp catch any-reg a1-offset)
                          (:temp tag descriptor-reg a2-offset)
                          (:temp temp non-descriptor-reg nl0-offset))

  (declare (ignore start count))

  ;; temp (nl0) is non-descriptor-reg — safe to pass as scratch to
  ;; load-symbol-value for large static-symbol offset materialisation.
  (load-symbol-value catch lisp::*current-catch-block* temp)

  LOOP

  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst cmp catch 0)
    (inst b.eq error))

  (loadw tag catch vm:catch-block-tag-slot)
  (inst cmp tag target)
  (inst b.eq exit)
  (loadw catch catch vm:catch-block-previous-catch-slot)
  (inst b loop)

  EXIT

  (move target catch)
  (inst li temp (make-fixup 'unwind :assembly-routine))
  (inst br temp))


;;;; Assembly routines for undefined-tramp and closure-tramp.

#+assembler
(define-assembly-routine (closure-tramp-function-alignment
                          (:return-style :none))
                         ()
  ;; Align to a doubleword and emit the magic function header so that
  ;; closure-tramp looks like a normal Lisp function.
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

  (loadw lexenv-tn cname-tn fdefn-function-slot other-pointer-type)
  (loadw code-tn lexenv-tn closure-function-slot function-pointer-type)
  ;; ARM64: compute entry address into LIP-TN, then branch via BR.
  (inst add lip-tn code-tn
        (- (* function-code-offset word-bytes) function-pointer-type))
  (inst br lip-tn)
  ;; Ensure the following routine is doubleword-aligned.
  (align vm:lowtag-bits))

#+assembler
(define-assembly-routine (undefined-tramp-function-alignment
                          (:return-style :none))
                         ()
  ;; Align to a doubleword and emit the magic function header so that
  ;; undefined-tramp looks like a normal Lisp function.
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

  (let ((error (generate-cerror-code nil undefined-symbol-error cname-tn)))
    (inst b error)
    ;; If we ever return from the undefined-symbol-error handler, retry
    ;; the call via the fdefn raw-addr slot.
    (loadw code-tn cname-tn fdefn-raw-addr-slot other-pointer-type)
    (inst add lip-tn code-tn
          (- (* function-code-offset word-bytes) function-pointer-type))
    (inst br lip-tn)))
