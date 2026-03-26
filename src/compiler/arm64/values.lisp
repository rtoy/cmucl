;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/values.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the implementation of unknown-values VOPs.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for SPARC by William Lott.
;;; Ported to ARM64 from SPARC.
;;;
;;; Porting notes (SPARC -> ARM64):
;;;
;;;   inst move   -> inst mov          (register move)
;;;   inst add    -> inst add          (three-operand ADD)
;;;   inst sub    -> inst sub          (three-operand SUB)
;;;   inst cmp    -> inst cmp          (sets NZCV flags)
;;;   inst b :eq  -> inst b :eq        (conditional branch, same condition names)
;;;   inst b :ne  -> inst b :ne
;;;   inst li     -> inst li           (materialise integer immediate)
;;;   inst orcc   -> inst adds / cmp   (SPARC ORCC sets flags from OR;
;;;                                     on ARM64 use ADDS zero-tn,num or CMP)
;;;   inst subcc  -> inst subs         (SUB and set flags)
;;;   inst ldn    -> loadw (reg-offset) (SPARC 32/64-bit load via indexed addr)
;;;   inst stn    -> storew             (SPARC 32/64-bit store via indexed addr)
;;;
;;; Stack direction:
;;;   SPARC: stack grows upward; CSP-TN is the next free slot.
;;;   ARM64: stack grows downward; CSP-TN points one word below the last pushed
;;;          value (matching the ARM port convention already in use for this
;;;          backend).  The arithmetic is the same in both cases because all
;;;          stack adjustments here are relative moves of CSP-TN, not absolute
;;;          address computations.
;;;
;;; Pseudo-atomic / scheduling:
;;;   ARM64 does not have a branch-delay slot, so the SPARC idiom of hiding a
;;;   useful instruction in the delay slot (e.g. the MOVE inside B's delay
;;;   slot) is dropped.  Instructions appear in straightforward sequential
;;;   order.
;;;

(in-package "ARM64")


(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
  (:generator 1
    (emit-not-implemented)
    (move csp-tn ptr)))


;;; Push some values onto the stack, returning the start and number of values
;;; pushed as results.  It is assumed that the Vals are wired to the standard
;;; argument locations.  Nvals is the number of values to push.
;;;
;;; The generator cost is pseudo-random.  We could get it right by defining a
;;; bogus SC that reflects the costs of the memory-to-memory moves for each
;;; operand, but this seems unworthwhile.
;;;
(define-vop (push-values)
  (:args (vals :more t))
  (:results (start :scs (any-reg) :from :load)
            (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 20
    (emit-not-implemented)
    ;; Record the stack pointer before we push anything -- this becomes the
    ;; "start" pointer that the caller uses to find the first pushed value.
    (inst mov start csp-tn)
    ;; Bump CSP to claim space for all NVALS words at once.
    (inst add csp-tn csp-tn (* nvals vm:word-bytes))
    ;; Store each value into the freshly claimed region.
    (do ((val vals (tn-ref-across val))
         (i 0 (1+ i)))
        ((null val))
      (let ((tn (tn-ref-tn val)))
        (sc-case tn
          (descriptor-reg
           (storew tn start i))
          (control-stack
           ;; Value is on the Lisp control stack; move it through TEMP.
           (load-stack-tn temp tn)
           (storew temp start i)))))
    ;; Return the count as a tagged fixnum.
    (inst li count (fixnumize nvals))))


;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
;;;
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :type list :from (:argument 0)) list)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (emit-not-implemented)
    (let ((loop (gen-label))
          (done (gen-label)))

      (move list arg)
      (move start csp-tn)

      (emit-label loop)
      ;; Test for NIL (end of list).
      (inst cmp list null-tn)
      (inst b :eq done)
      ;; Load the CAR into TEMP, advance LIST to the CDR.
      (loadw temp list vm:cons-car-slot vm:list-pointer-type)
      (loadw list list vm:cons-cdr-slot vm:list-pointer-type)
      ;; Push TEMP onto the stack.
      (inst add csp-tn csp-tn vm:word-bytes)
      (storew temp csp-tn -1)
      ;; If the CDR is not a list pointer, signal an error; otherwise loop.
      (test-type list ndescr loop nil vm:list-pointer-type)
      (error-call vop bogus-argument-to-values-list-error list)

      (emit-label done)
      ;; Count = current CSP - start (both are tagged fixnums, so the
      ;; difference is already a tagged fixnum count of words pushed).
      (inst sub count csp-tn start))))


;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
;;;
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :target src)
         (skip :scs (any-reg zero immediate))
         (num :scs (any-reg) :target count))
  (:arg-types * positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 0)) src)
  (:temporary (:sc any-reg :from (:argument 2)) dst)
  (:temporary (:sc descriptor-reg :from (:argument 1)) temp)
  (:temporary (:sc non-descriptor-reg) k)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (emit-not-implemented)
    ;; Compute SRC = context + (skip * word-bytes), handling the three
    ;; cases for the SKIP operand's storage class.
    (sc-case skip
      (zero
       ;; No skipped args: SRC is exactly CONTEXT.
       (move src context))
      (immediate
       ;; Constant number of skipped args known at compile time.
       ;; The byte offset may exceed the 12-bit ADD immediate range,
       ;; so materialise it into SRC first if necessary.
       (let ((byte-offset (* (tn-value skip) word-bytes)))
         (cond ((typep byte-offset '(unsigned-byte 12))
                (inst add src context byte-offset))
               (t
                (inst li src byte-offset)
                (inst add src context src)))))
      (any-reg
       ;; SKIP is a tagged fixnum; shift left by 1 to convert to a byte
       ;; offset (word-bytes=8, fixnum-tag-bits=2, so net shift = 1).
       (inst add src context (shift skip :lsl 1))))

    ;; Move NUM into COUNT and branch immediately if there is nothing to copy.
    (move count num)
    (inst cbz count done)

    ;; Record the current stack top as START, then claim space.
    (inst mov start csp-tn)
    (inst mov dst csp-tn)
    (inst add csp-tn csp-tn count)

    ;; Untag COUNT into K to get the raw word count for the loop.
    (inst asr k count vm:fixnum-tag-bits)

    LOOP
    ;; Decrement K and test; branch when non-zero (more words remain).
    (inst subs k k 1)
    ;; Load word at SRC[K] and store to DST[K], scaling K by word-shift.
    (inst ldr temp (reg-offset src k :uxtx vm:word-shift))
    (inst str temp (reg-offset dst k :uxtx vm:word-shift))
    (inst b :ne loop)

    DONE))
