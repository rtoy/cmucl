;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/debug.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Originally written by William Lott (SPARC version).
;;; Ported to ARM64 by [ARM64 port contributors].
;;;
;;; Porting notes:
;;;
;;;   SPARC inst LDN  -> ARM64 inst LDR  (64-bit load with register offset)
;;;   SPARC inst STN  -> ARM64 inst STR  (64-bit store with register offset)
;;;   SPARC inst SRLN -> ARM64 inst LSR  (logical shift right)
;;;   SPARC inst SLLN -> ARM64 inst LSL  (logical shift left)
;;;   SPARC inst CMP + B :EQ -> ARM64 inst CBZ (compare-and-branch-if-zero, one instruction)
;;;   SPARC inst B    -> ARM64 inst B    (unconditional branch)
;;;   SPARC inst ADD  -> ARM64 inst ADD
;;;   SPARC inst SUB  -> ARM64 inst SUB
;;;   SPARC inst MOVE -> ARM64 inst MOVE (register copy pseudo-instruction)
;;;
;;; AArch64 has no delay slots, so all branch/compare sequences are
;;; straightforward -- no NOP fill or slot-filling is required.
;;;
;;; The offset argument to read-control-stack / write-control-stack is an
;;; unscaled byte offset held in a register (any-reg, tagged as a positive
;;; fixnum on SPARC; we treat it identically here -- the caller is
;;; responsible for correct scaling).  LDR/STR with register-offset
;;; addressing handles this directly via (reg-offset sap offset).
;;;
;;; In code-from-mumble:
;;;   SPARC used SRLN/CMP/B :EQ/SLLN/ADD/SUB with a delay-slot move.
;;;   ARM64 uses LSR/CBZ/LSL/ADD/SUB with no delay slots.
;;;   The branch-to-bogus (out-of-line null return) and the main path
;;;   are structurally identical to the SPARC version.
;;;
;;; single-value-return-byte-offset is 8 on ARM64 (defined in vm.lisp),
;;; matching the SPARC value and matching the two-instruction LRA header.
;;;
(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")

(defknown di::current-sp () system-area-pointer (movable flushable))
(defknown di::current-fp () system-area-pointer (movable flushable))
(defknown di::stack-ref (system-area-pointer index) t (flushable))
(defknown di::%set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown di::lra-code-header (t) t (movable flushable))
(defknown di::function-code-header (t) t (movable flushable))
(defknown di::make-lisp-obj ((unsigned-byte 64)) t (movable flushable))
(defknown di::get-lisp-obj-address (t) (unsigned-byte 64) (movable flushable))
(defknown di::function-word-offset (function) index (movable flushable))


;;;; Current stack/frame-pointer accessors.

(define-vop (debug-cur-sp)
  (:translate di::current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate di::current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res cfp-tn)))


;;;; Control-stack slot accessors.

;;; READ-CONTROL-STACK -- read one word from the control stack at a
;;; byte offset supplied in a register.
;;;
;;; SPARC used:  (inst ldn result sap offset)
;;; ARM64 uses:  LDR with register-offset addressing mode.
;;;
(define-vop (read-control-stack)
  (:translate kernel:stack-ref)
  (:policy :fast-safe)
  (:args (sap    :scs (sap-reg))
         (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst ldr result (reg-offset sap offset))))

;;; WRITE-CONTROL-STACK -- write one word to the control stack at a
;;; byte offset supplied in a register, returning the written value.
;;;
;;; SPARC used:  (inst stn value sap offset) + (move result value)
;;; ARM64 uses:  STR with register-offset addressing mode + MOVE.
;;;
(define-vop (write-control-stack)
  (:translate kernel:%set-stack-ref)
  (:policy :fast-safe)
  (:args (sap    :scs (sap-reg))
         (offset :scs (any-reg))
         (value  :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst str value (reg-offset sap offset))
    (move result value)))


;;;; Code-object recovery from LRA or function pointer.

;;; CODE-FROM-MUMBLE -- given a THING tagged with LOWTAG, recover the
;;; enclosing code object.
;;;
;;; Algorithm (identical to SPARC):
;;;   1. Load the header word of THING.
;;;   2. Shift right by type-bits to get the size in words.
;;;   3. If zero, the object has no header (bogus) -> return NIL.
;;;   4. Shift left by log2(word-bytes) to get the byte size.
;;;   5. Adjust for lowtag difference from other-pointer-type.
;;;   6. Subtract from THING to get the code object base.
;;;
;;; SPARC instruction mapping:
;;;   (inst srln temp vm:type-bits)       -> (inst lsr  temp temp vm:type-bits)
;;;   (inst cmp temp) + (inst b :eq bogus) -> (inst cbz  temp bogus)  [one instruction]
;;;   (inst slln temp (1- ...))           -> (inst lsl  temp temp (1- ...))
;;;   (inst add temp ...)                 -> (inst sub  temp temp (- other-pointer-type lowtag))
;;;                                          [(- lowtag other-pointer-type) is always negative;
;;;                                           use SUB with the positive delta instead of ADD]
;;;   (inst sub code thing temp)          -> (inst sub  code thing temp)
;;;   (inst b done) [delay: (move ...)]   -> (inst b    done) [no delay slot]
;;;
;;; CBZ (Compare and Branch if Zero) replaces the SPARC CMP+B :EQ pair,
;;; saving one instruction and avoiding a flags-register write.
;;;
;;; AArch64 has no branch delay slots; the (move code null-tn) in the
;;; out-of-line bogus block executes unconditionally before the jump back.
;;;
(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
          (done  (gen-label)))
      (loadw temp thing 0 lowtag)
      ;; Extract the word count from the header (shift off type tag).
      (inst lsr temp temp vm:type-bits)
      ;; If word count is zero this isn't a real code object.
      ;; CBZ replaces SPARC's CMP+B :EQ -- one instruction, no flags written.
      (inst cbz temp bogus)
      ;; Convert word count to byte count.
      (inst lsl temp temp (1- (integer-length vm:word-bytes)))
      ;; Adjust for lowtag difference when LOWTAG != other-pointer-type.
      ;; (- lowtag other-pointer-type) is always negative (function-pointer-type
      ;; < other-pointer-type), so use SUB with the positive delta instead.
      (unless (= lowtag vm:other-pointer-type)
        (inst sub temp temp (- vm:other-pointer-type lowtag)))
      ;; Subtract byte size from tagged pointer to reach the code base.
      (inst sub code thing temp)
      (emit-label done)
      (assemble (*elsewhere*)
        (emit-label bogus)
        ;; No delay slot on AArch64 -- move executes before branch.
        (move code null-tn)
        (inst b done)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate di::lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate di::function-code-header)
  (:variant vm:function-pointer-type))


;;;; Lisp object / raw address conversion.

;;; MAKE-LISP-OBJ -- treat a raw (unsigned-byte 64) as a Lisp object.
;;; No actual instruction needed; the value is already in a register.
;;;
(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate di::make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

;;; GET-LISP-OBJ-ADDRESS -- return the raw machine address of a Lisp object.
;;; No actual instruction needed; the register already holds the address.
;;;
(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))


;;;; Function word offset.

;;; FUNCTION-WORD-OFFSET -- return the number of words from the start of
;;; the code object to the entry point of a function.
;;;
;;; SPARC used:  (loadw res fun 0 function-pointer-type) + (inst srln res vm:type-bits)
;;; ARM64 uses:  identical structure; LOADW and LSR replace LDN and SRLN.
;;;
(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate di::function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 vm:function-pointer-type)
    (inst lsr res res vm:type-bits)))
