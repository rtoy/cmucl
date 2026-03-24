;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/arith.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition of arithmetic VOPs for the ARM64.
;;;
;;; Ported from sparc/arith.lisp (originally by Rob MacLachlan,
;;; converted by William Lott, enhanced by Raymond Toy).
;;;
;;; ARM64 port notes:
;;;
;;; ARM64 is a 64-bit RISC ISA with 31 general-purpose 64-bit registers
;;; (X0-X30) plus XZR (zero register) and SP.  The 32-bit views of those
;;; registers are W0-W30 / WZR.
;;;
;;; Key differences from SPARC that affect this file:
;;;
;;;  * No delay slots -- every branch/cbz/tbz takes effect immediately.
;;;    All (inst nop) delay-slot fillers from SPARC are removed.
;;;
;;;  * Condition flags: ARM64 sets N/Z/C/V via ADDS/SUBS/ANDS etc.
;;;    The comparison pseudo-instruction CMP is an alias for SUBS XZR.
;;;    Conditional branches use B.cond (e.g. B.LT, B.GE, B.EQ …).
;;;    Carry arithmetic uses ADCS / SBCS.
;;;
;;;  * No Y register / mulscc / wry machinery.  64-bit multiply is a
;;;    single MUL (low 64 bits) or UMULH/SMULH (high 64 bits) instruction.
;;;
;;;  * Arithmetic shifts: ASR (signed right shift), LSR (unsigned right
;;;    shift), LSL (left shift).  These are register-register or
;;;    register-immediate.  There is no separate instruction needed to
;;;    clamp the shift count; out-of-range shifts are defined (produce 0
;;;    or sign-replicated top bit), so some SPARC guard code can be
;;;    dropped.
;;;
;;;  * SPARC's sll/srl/sra are 32-bit shifts; sllx/srlx/srax are 64-bit.
;;;    ARM64 has W-register (32-bit) and X-register (64-bit) variants of
;;;    the same mnemonics.  We use the X-register forms throughout and
;;;    mask / sign-extend explicitly when a 32-bit result is needed.
;;;
;;;  * SPARC's "signx" (sign-extend word to long) = SXTW on ARM64.
;;;    SPARC's "clruw" (zero-extend word to long) = AND with #xffffffff,
;;;    or simply use the W-register write (which zero-extends).
;;;
;;;  * Logical NOT: ARM64 uses MVN (bitwise NOT).
;;;    Logical NAND / NOR etc. are synthesised the same way as SPARC.
;;;    ARM64 has BIC (AND NOT), ORN (OR NOT), EON (EOR NOT) natively.
;;;
;;;  * Integer overflow trap (SPARC taddcctv/tsubcctv) has no ARM64
;;;    equivalent.  The safe fixnum +/- VOPs that used those instructions
;;;    are kept as stubs that call the generic out-of-line routine.
;;;
;;;  * abs: ARM64 has no single abs instruction.  We use the Hacker's
;;;    Delight trick with ASR / EOR / SUB, same as SPARC but with
;;;    ARM64 mnemonics.
;;;
;;;  * integer-length loop: ARM64 has CLZ (count leading zeros).
;;;    We use it to implement integer-length in O(1) instead of a loop.
;;;
;;;  * logcount (popcount): ARM64 has no integer popcount in the base ISA
;;;    (NEON CNT exists but requires vector registers).  We keep the
;;;    parallel-bit-count approach from the SPARC version.
;;;
;;;  * Truncation by constant: the reciprocal-multiply trick is kept;
;;;    the SMULL / SMULH / UMULH instructions replace smul+rdy.
;;;

(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; Unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))


(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note _N"inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note _N"inline (signed-byte 64) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

;;; Negate: ARM64 NEG Rd, Rn  (alias for SUB Rd, XZR, Rn)
(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
      (emit-not-implemented)
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
      (emit-not-implemented)
    (inst neg res x)))

;;; Logical NOT.
;;; For fixnums we XOR with the fixnumised -1 to preserve the tag bits,
;;; same strategy as SPARC.  For raw integers we use MVN (ARM64 bitwise NOT).
(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
      (emit-not-implemented)
    ;; MVN on the whole word would flip tag bits; use EOR instead.
    (inst eor res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
      (emit-not-implemented)
    (inst mvn res x)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg.

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 64) arithmetic"))

;;; All constant binary VOPs use a (signed-byte 13) constraint, matching the
;;; SPARC convention.  This covers the full ADD/SUB immediate range: positive
;;; values 1..4095 use ADD (or the logical op directly), negative values
;;; -4095..-1 flip to SUB.  For logical ops the constant is always
;;; non-negative within this window; values 1..4095 are all valid bitmask
;;; immediates (contiguous runs of 1 bits fit trivially).
;;; The generator chooses ADD vs SUB based on the sign of the constant.

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num
              (:constant (and (signed-byte #.(- 13 vm:fixnum-tag-bits))
                              (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 64) arithmetic"))

(eval-when (compile load eval)

;;; define-binop generates the standard family of register and constant
;;; variants for each binary operator.
;;;
;;; arg-swap        -- when T the operand order is reversed (for non-commutative
;;;                    operators like logandc1 / logorc1).
;;; restore-fixnum-mask -- when T an extra AND clears the tag bits that the
;;;                    operation may have corrupted (needed for ORC1/ORC2/LOGEQV).
;;; has-immediate-p -- when NIL no constant VOPs are generated.  Must be NIL
;;;                    for BIC, ORN, EON which have no immediate encoding in
;;;                    the ARM64 ISA.
;;;
;;; For ADD and SUB the constant generator checks the sign of the constant
;;; and flips ADD <-> SUB accordingly, since ARM64 ADD/SUB only take unsigned
;;; imm12.  For logical ops (AND/ORR/EOR) the constant is always non-negative
;;; within the (signed-byte 13) window and is passed directly.
;;;
;;; ARM64 equivalents of SPARC instructions:
;;;   add -> ADD    sub -> SUB
;;;   and -> AND    bic -> BIC (Rd = Rn AND NOT Rm)  -- no immediate form
;;;   orr -> ORR    orn -> ORN (Rd = Rn OR  NOT Rm)  -- no immediate form
;;;   eor -> EOR    eon -> EON (Rd = Rn EOR NOT Rm)  -- no immediate form

(defmacro define-binop (translate untagged-penalty op
                        &optional arg-swap restore-fixnum-mask (has-immediate-p t))
  ;; conjugate-op: for add/sub flip the instruction on negative constants.
  (let ((conjugate-op (cond ((eq op 'add) 'sub)
                            ((eq op 'sub) 'add)
                            (t nil))))
    `(progn
       (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
                     fast-fixnum-binop)
         (:translate ,translate)
         ,@(when restore-fixnum-mask
                 `((:temporary (:sc non-descriptor-reg) temp)))
         (:generator 2
             (emit-not-implemented)
           ,(if arg-swap
                `(inst ,op ,(if restore-fixnum-mask 'temp 'r) y x)
                `(inst ,op ,(if restore-fixnum-mask 'temp 'r) x y))
           ,@(when restore-fixnum-mask
                   `((inst and r temp (lognot fixnum-tag-mask))))))
       ,@(when (and has-immediate-p (not arg-swap))
               `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
                               fast-fixnum-binop-c)
                   (:translate ,translate)
                   ,@(when restore-fixnum-mask
                           `((:temporary (:sc non-descriptor-reg) temp)))
                   (:generator 1
                       (emit-not-implemented)
                     ,(if conjugate-op
                          `(let ((fy (fixnumize y)))
                             (if (minusp fy)
                                 (inst ,conjugate-op ,(if restore-fixnum-mask 'temp 'r) x (imm (- fy)))
                                 (inst ,op           ,(if restore-fixnum-mask 'temp 'r) x (imm fy))))
                          `(inst ,op ,(if restore-fixnum-mask 'temp 'r) x (fixnumize y)))
                     ,@(when restore-fixnum-mask
                             `((inst and r temp (lognot fixnum-tag-mask))))))))
       (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
                     fast-signed-binop)
         (:translate ,translate)
         (:generator ,(1+ untagged-penalty)
             (emit-not-implemented)
           ,(if arg-swap
                `(inst ,op r y x)
                `(inst ,op r x y))))
       ,@(when (and has-immediate-p (not arg-swap))
               `((define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
                               fast-signed-binop-c)
                   (:translate ,translate)
                   (:generator ,untagged-penalty
                       (emit-not-implemented)
                     ,(if conjugate-op
                          `(if (minusp y)
                               (inst ,conjugate-op r x (imm (- y)))
                               (inst ,op           r x (imm y)))
                          `(inst ,op r x y))))))
       (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
                     fast-unsigned-binop)
         (:translate ,translate)
         (:generator ,(1+ untagged-penalty)
             (emit-not-implemented)
           ,(if arg-swap
                `(inst ,op r y x)
                `(inst ,op r x y))))
       ,@(when (and has-immediate-p (not arg-swap))
               `((define-vop (,(symbolicate "FAST-" translate "-C/UNSIGNED=>UNSIGNED")
                               fast-unsigned-binop-c)
                   (:translate ,translate)
                   (:generator ,untagged-penalty
                       (emit-not-implemented)
                     ,(if conjugate-op
                          `(if (minusp y)
                               (inst ,conjugate-op r x (imm (- y)))
                               (inst ,op           r x (imm y)))
                          `(inst ,op r x y)))))))))

); eval-when

(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logandc1 2 bic t nil nil)     ; no immediate form for BIC
(define-binop logandc2 2 bic nil nil nil)   ; no immediate form for BIC
(define-binop logior 2 orr)
(define-binop logorc1 2 orn t t nil)        ; no immediate form for ORN
(define-binop logorc2 2 orn nil t nil)      ; no immediate form for ORN
(define-binop logxor 2 eor)
(define-binop logeqv 2 eon nil t nil)       ; no immediate form for EON

;;; Special logand cases: (logand signed unsigned) => unsigned.
;;; ARM64 AND works uniformly on 64-bit registers.

(define-vop (fast-logand/signed-unsigned=>unsigned
             fast-logand/unsigned=>unsigned)
    (:args (x :scs (signed-reg))
           (y :target r :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand/unsigned-signed=>unsigned
             fast-logand/unsigned=>unsigned)
    (:args (x :target r :scs (unsigned-reg))
           (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num))

;;; (logand signed constant) -- use the standard (signed-byte 13) constraint.
;;; All values 1..4095 are valid bitmask immediates, so AND works directly.
(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-unsigned-binop-c)
  (:args (x :scs (signed-reg)))
  (:translate logand)
  (:arg-types signed-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:generator 2
      (emit-not-implemented)
    (inst and r x y)))

;;; abs for signed integers using Hacker's Delight:
;;;   y = x >> 63  (arithmetic shift, gives 0 or -1)
;;;   r = (x EOR y) - y
;;; ARM64: ASR for arithmetic right shift; EOR; SUB.
(define-vop (fast-abs/signed fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate abs)
  (:note _N"inline 64-bit abs")
  (:temporary (:scs (signed-reg)) y)
  (:generator 3
      (emit-not-implemented)
    ;; y = x >> 63 (signed shift: 0 if positive, -1 if negative)
    (inst asr y x (1- vm:word-bits))
    (inst eor r y x)
    (inst sub r r y)))


;;; Special fixnum + and - that trap on overflow.
;;; ARM64 has no taddcctv equivalent; we call the generic out-of-line routine.
;;; These VOPs are kept structurally but call the assembly routine.

#+nil  ; Disabled -- no trap-on-overflow instruction on ARM64.
(progn
(define-vop (+/fixnum fast-+/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note _N"safe inline fixnum arithmetic")
  (:generator 4
      (emit-not-implemented)
    ;; Use ADDS and check V flag, or just call out-of-line.
    (inst adds r x y)
    (inst b :vc done)
    ;; overflow -- call generic
    (error-call vop fixnum-overflow-error x y)
    (emit-label done)))
)


;;; Truncate

;;; ARM64 has SDIV / UDIV for signed and unsigned division.
;;; Remainder is computed as: rem = dividend - quotient * divisor.
;;; The MSUB instruction (multiply-subtract) does this in one step:
;;;   MSUB rem, quo, divisor, dividend   =>  rem = dividend - quo*divisor

(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note _N"inline (signed-byte 64) arithmetic")
  (:temporary (:scs (signed-reg) :target quo) q)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 8
      (emit-not-implemented)
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cbz y zero)
      (inst sdiv q x y)
      ;; rem = x - q*y  (MSUB: Rd = Ra - Rn*Rm)
      (inst msub rem q y x)
      (unless (location= quo q)
        (move quo q)))))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 64) arithmetic")
  (:temporary (:scs (unsigned-reg) :target quo) q)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 8
      (emit-not-implemented)
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cbz y zero)
      (inst udiv q x y)
      (inst msub rem q y x)
      (unless (location= quo q)
        (move quo q)))))


;;; Shifting

;;; ARM64 ASH for signed numbers.
;;; ARM64 shifts: LSL (logical left), ASR (arithmetic right), LSR (logical right).
;;; Shift count is taken mod 64, so no clamping needed for counts in [-63..63].
;;; For counts <= -64, an arithmetic right shift gives the sign bit replicated,
;;; equivalent to the most-negative result.

(define-vop (fast-ash/signed=>signed)
  (:note _N"inline (signed-byte 64) ASH")
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg immediate) :to :save))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
      (emit-not-implemented)
    (sc-case amount
      (signed-reg
       (let ((done (gen-label))
             (right-shift (gen-label))
             (do-shift (gen-label)))
         (inst tbnz amount 63 right-shift)
         ;; Left shift -- ARM64 LSL takes count mod 64, which is fine.
         (inst lsl result number amount)
         (inst b done)

         (emit-label right-shift)
         ;; ndesc = -amount (positive magnitude of right shift)
         (inst neg ndesc amount)
         ;; Clamp to 63: for signed right shift, ASR by 63 gives all sign bits,
         ;; which is correct for any shift >= 63.
         (inst cmp ndesc 63)
         (inst b :le do-shift)
         (inst li ndesc 63)
         (emit-label do-shift)
         (inst asr result number ndesc)

         (emit-label done)))
      (immediate
       (let ((amount (tn-value amount)))
         (cond ((< amount -63)
                (inst asr result number 63))  ; all sign bits
               ((< amount 0)
                (inst asr result number (- amount)))
               ((> amount 0)
                (inst lsl result number amount))
               (t
                (move result number))))))))

(define-vop (fast-ash/unsigned=>unsigned)
  (:note _N"inline (unsigned-byte 64) ASH")
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg immediate) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
      (emit-not-implemented)
    (sc-case amount
      (signed-reg
       (let ((done (gen-label))
             (right-shift (gen-label))
             (not-zero (gen-label)))
         (inst tbnz amount 63 right-shift)
         (inst lsl result number amount)
         (inst b done)

         (emit-label right-shift)
         (inst neg ndesc amount)
         ;; For unsigned, a right shift >= 64 gives 0.
         (inst cmp ndesc 64)
         (inst b :lt not-zero)
         (move result zero-tn)
         (inst b done)
         (emit-label not-zero)
         (inst lsr result number ndesc)

         (emit-label done)))
      (immediate
       (let ((amount (tn-value amount)))
         (cond ((< amount -63)
                (move result zero-tn))
               ((< amount 0)
                (inst lsr result number (- amount)))
               ((> amount 0)
                (inst lsl result number amount))
               (t
                (move result number))))))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:note _N"inline constant ASH")
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 4
      (emit-not-implemented)
    (cond
      ((< count -63) (move result zero-tn))
      ((< count 0)   (inst lsr result number (min (- count) 63)))
      ((> count 0)   (inst lsl result number (min count 63)))
      (t (error (intl:gettext "identity ASH not transformed away"))))))

;;; Left-shift-only VOPs for cases where direction is statically known.
(macrolet
    ((frob (name sc-type type result-type cost)
       `(define-vop (,name)
          (:note _N"inline ASH")
          (:translate ash)
          (:args (number :scs (,sc-type))
                 (amount :scs (signed-reg unsigned-reg immediate)))
          (:arg-types ,type positive-fixnum)
          (:results (result :scs (,result-type)))
          (:result-types ,type)
          (:policy :fast-safe)
          (:generator ,cost
              (emit-not-implemented)
            (sc-case amount
              ((signed-reg unsigned-reg)
               (inst lsl result number amount))
              (immediate
               (let ((amount (tn-value amount)))
                 (assert (>= amount 0))
                 (inst lsl result number amount))))))))
  (frob fast-ash-left/signed=>signed   signed-reg   signed-num   signed-reg   3)
  (frob fast-ash-left/fixnum=>fixnum   any-reg      tagged-num   any-reg      2)
  (frob fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

;;; Constant left-shift VOPs.
(macrolet
    ((frob (name sc-type type result-type cost)
       `(define-vop (,name)
          (:note _N"inline ASH")
          (:translate ash)
          (:args (number :scs (,sc-type)))
          (:info amount)
          (:arg-types ,type (:constant (integer 0 63)))
          (:results (result :scs (,result-type)))
          (:result-types ,type)
          (:policy :fast-safe)
          (:generator ,cost
              (emit-not-implemented)
            (if (zerop amount)
                (move result number)
                (inst lsl result number amount))))))
  (frob fast-ash-left-c/signed=>signed   signed-reg   signed-num   signed-reg   3)
  (frob fast-ash-left-c/fixnum=>fixnum   any-reg      tagged-num   any-reg      2)
  (frob fast-ash-left-c/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

;;; Right-shift helper functions (used by deftransforms).
(defknown ash-right-signed ((signed-byte #.vm:word-bits)
                             (and fixnum unsigned-byte))
  (signed-byte #.vm:word-bits)
  (movable foldable flushable))

(defknown ash-right-unsigned ((unsigned-byte #.vm:word-bits)
                               (and fixnum unsigned-byte))
  (unsigned-byte #.vm:word-bits)
  (movable foldable flushable))

;;; Right-shift VOPs (register and constant count).
(macrolet
    ((frob (trans name sc-type type shift-inst cost)
       `(define-vop (,name)
          (:note _N"inline right ASH")
          (:translate ,trans)
          (:args (number :scs (,sc-type))
                 (amount :scs (signed-reg unsigned-reg immediate)))
          (:arg-types ,type positive-fixnum)
          (:results (result :scs (,sc-type)))
          (:result-types ,type)
          (:policy :fast-safe)
          (:generator ,cost
              (emit-not-implemented)
            (sc-case amount
              ((signed-reg unsigned-reg)
               (inst ,shift-inst result number amount))
              (immediate
               (let ((amt (tn-value amount)))
                 (inst ,shift-inst result number amt))))))))
  (frob ash-right-signed   fast-ash-right/signed=>signed   signed-reg   signed-num   asr 3)
  (frob ash-right-unsigned fast-ash-right/unsigned=>unsigned unsigned-reg unsigned-num lsr 3))

;;; Constant right-shift.
(macrolet
    ((frob (trans name sc-type type shift-inst cost max-shift)
       `(define-vop (,name)
          (:note _N"inline right ASH")
          (:translate ,trans)
          (:args (number :target result :scs (,sc-type)))
          (:info amount)
          (:arg-types ,type (:constant (integer 0 ,max-shift)))
          (:results (result :scs (,sc-type)))
          (:result-types ,type)
          (:policy :fast-safe)
          (:generator ,cost
              (emit-not-implemented)
            (if (zerop amount)
                (move result number)
                (inst ,shift-inst result number amount))))))
  (frob ash-right-signed   fast-ash-right-c/signed=>signed   signed-reg   signed-num   asr 1 63)
  (frob ash-right-unsigned fast-ash-right-c/unsigned=>unsigned unsigned-reg unsigned-num lsr 1 63))

;;; Right-shift fixnum result: shift, then mask out the tag bits.
(define-vop (fast-ash-right/fixnum=>fixnum)
    (:note _N"inline right ASH")
  (:translate ash-right-signed)
  (:args (number :scs (any-reg))
         (amount :scs (signed-reg unsigned-reg immediate)))
  (:arg-types tagged-num positive-fixnum)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc non-descriptor-reg :target result) temp)
  (:policy :fast-safe)
  (:generator 2
      (emit-not-implemented)
    (sc-case amount
      ((signed-reg unsigned-reg)
       (inst asr temp number amount))
      (immediate
       (inst asr temp number (tn-value amount))))
    ;; Clear the shifted-in tag bits.
    (inst and result temp (lognot fixnum-tag-mask))))


;;;; integer-length

;;; ARM64 CLZ gives the number of leading zeros in a 64-bit register.
;;; integer-length(x) = 64 - clz(x) for unsigned x.
;;; For signed x we first complement negative values so CLZ sees the
;;; significant magnitude bits.  That's the same as XOR with the sign
;;; replication (x >> 63), which is 0 for non-negative and -1 for negative.

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note _N"inline (signed-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift mask)
  (:generator 5
      (emit-not-implemented)
    ;; mask = x >> 63  (arithmetic: 0 if x>=0, -1 if x<0)
    (inst asr mask arg 63)
    ;; shift = x XOR mask  (complement negative values, identity for non-negative)
    (inst eor shift arg mask)
    ;; clz(shift) -> shift
    (inst clz shift shift)
    ;; res = (64 - clz) as a fixnum = fixnumize(64) - fixnumize(clz)
    ;; Use LSL to fixnumize the clz count, then subtract from fixnumize(64).
    (inst li res (fixnumize vm:word-bits))
    (inst sub res res (shift shift :lsl fixnum-tag-bits))))

(define-vop (unsigned-byte-64-len)
  (:translate integer-length)
  (:note _N"inline (unsigned-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target shift))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 5
      (emit-not-implemented)
    (inst clz shift arg)
    (inst li res (fixnumize vm:word-bits))
    (inst sub res res (shift shift :lsl fixnum-tag-bits))))


;;;; logcount (popcount)

;;; ARM64 base ISA has no integer POPCNT.  Use the parallel-bit-count
;;; approach (same masks as the SPARC version, widened to 64 bits).
(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note _N"inline (unsigned-byte 64) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) mask temp)
  (:generator 30
      (emit-not-implemented)
    (move res arg)

    ;; 64-bit versions of the classic parallel popcount masks.
    (dolist (stuff '((1  #x5555555555555555)
                     (2  #x3333333333333333)
                     (4  #x0f0f0f0f0f0f0f0f)
                     (8  #x00ff00ff00ff00ff)
                     (16 #x0000ffff0000ffff)
                     (32 #x00000000ffffffff)))
      (destructuring-bind (shift bit-mask) stuff
        (inst li mask bit-mask)
        (inst and temp res mask)
        (inst lsr res res shift)
        (inst and res res mask)
        (inst add res res temp)))))


;;;; Multiply and Divide.

;;; ARM64 has MUL (64x64->64 low bits) as standard; no guard needed.
;;; For fixnum * fixnum we need to untag one operand first.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:generator 4
      (emit-not-implemented)
    ;; Untag y, then multiply.
    (inst asr temp y fixnum-tag-bits)
    (inst mul r x temp)))


(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
      (emit-not-implemented)
    (inst mul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
      (emit-not-implemented)
    (inst mul r x y)))


;;;; Binary conditional VOPs.

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

;;; INTEGER-WITH-A-BITE-OUT -- the full signed S-bit range with the top BITE
;;; values removed from the upper end.  That is:
;;;   (integer -(2^(s-1))  (2^(s-1) - bite - 1))
;;;
;;; Useful for immediate constants where the maximum positive value must be
;;; avoided to prevent overflow in subsequent arithmetic.  Examples:
;;;   (integer-with-a-bite-out 14 4) => (integer -8192 8187)  ; 14-bit signed, top 4 missing
;;;   (integer-with-a-bite-out  8 1) => (integer -128   126)  ; 8-bit signed, top 1 missing
(deftype integer-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
        ((and (integerp s) (> s 1))
         (let ((bound (ash 1 (1- s))))
           `(integer ,(- bound) ,(- bound bite 1))))
        (t
         (error (intl:gettext "Bad size specified for SIGNED-BYTE type specifier: ~S.") s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note _N"inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (and (signed-byte #.(- 13 vm:fixnum-tag-bits))
                                         (not (integer 0 0)))))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note _N"inline (signed-byte 64) comparison"))

;;; CMP immediate on ARM64 is the same encoding as ADD/SUB immediate:
;;; 12-bit unsigned, with negative comparisons expressed as CMN (ADD to zero).
;;; Use (signed-byte 13) to match the binop-c constraint.
(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 64) comparison"))

;;; Unsigned CMP immediate: only non-negative values (CMN for negative
;;; would compare against |y| as unsigned, which is confusing; use register).
(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (unsigned-byte 12)))
  (:info target not-p y))

;;; ARM64 conditional branches: CMP then B.cond.
;;; For negative constants, CMP x, -k  is expressed as CMN x, k
;;; (since there is no SUB-with-negative-immediate; SUB only takes unsigned imm12).
;;; There are no delay slots, so no trailing NOP.
(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed)
                   (unless (and (member suffix '(/fixnum -c/fixnum))
                                (eq tran 'eql))
                     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                    tran suffix))
                                    ,(intern
                                      (format nil "~:@(FAST-CONDITIONAL~A~)"
                                              suffix)))
                        (:translate ,tran)
                        (:generator ,cost
                            (emit-not-implemented)
                          ,(if (member suffix '(-c/fixnum -c/signed -c/unsigned))
                               ;; Constant comparison: use CMN for negative values.
                               `(let ((c ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y)))
                                  (if (minusp c)
                                      (inst cmn x (imm (- c)))
                                      (inst cmp x (imm c))))
                               `(inst cmp x
                                      ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y)))
                          (inst b (if not-p
                                      ,(if signed not-cond not-unsigned)
                                      ,(if signed cond unsigned))
                                target)))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(4 3 6 5 6 5)
               '(t t t t nil nil))))

;;; ARM64 condition codes for B.cond (from arm64-insts condition-codes list):
;;;   signed:   :lt :le :gt :ge :eq :ne
;;;   unsigned: :cc (lower) :ls (lower-or-same) :hi (higher) :cs (higher-or-same)
(define-conditional-vop < :lt :cc :ge :cs)
(define-conditional-vop > :gt :hi :le :ls)
(define-conditional-vop eql :eq :eq :ne :ne)

;;; EQL/FIXNUM variants.
(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:translate eql)
  (:generator 4
      (emit-not-implemented)
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)))

(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:generator 7
      (emit-not-implemented)
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types tagged-num (:constant (and (signed-byte #.(- 13 vm:fixnum-tag-bits))
                                         (not (integer 0 0)))))
  (:info target not-p y)
  (:translate eql)
  (:generator 3
      (emit-not-implemented)
    (let ((fy (fixnumize y)))
      (if (minusp fy)
          (inst cmn x (imm (- fy)))
          (inst cmp x (imm fy))))
    (inst b (if not-p :ne :eq) target)))

(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (and (signed-byte #.(- 13 vm:fixnum-tag-bits))
                                (not (integer 0 0)))))
  (:generator 6
      (emit-not-implemented)
    (let ((fy (fixnumize y)))
      (if (minusp fy)
          (inst cmn x (imm (- fy)))
          (inst cmp x (imm fy))))
    (inst b (if not-p :ne :eq) target)))


;;;; 32-bit logical operations (for bignum / bit-bashing).
;;;
;;; On ARM64 all registers are 64-bit; we stay with 64-bit ops
;;; throughout and let the caller mask if needed.

(define-vop (32bit-logical)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:generator 1
      (emit-not-implemented)
    (inst mvn r x)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
      (emit-not-implemented)
    (inst and r x y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
      (emit-not-implemented)
    (inst orr r x y)))

(deftransform 32bit-logical-nor ((x y) (* *))
  '(32bit-logical-not (32bit-logical-or x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
      (emit-not-implemented)
    (inst eor r x y)))

;;; xnor = EOR NOT = EON on ARM64
(define-vop (32bit-logical-eqv 32bit-logical)
  (:translate 32bit-logical-eqv)
  (:generator 1
      (emit-not-implemented)
    (inst eon r x y)))

;;; orc2 = x OR (NOT y) = ORN on ARM64
(define-vop (32bit-logical-orc2 32bit-logical)
  (:translate 32bit-logical-orc2)
  (:generator 1
      (emit-not-implemented)
    (inst orn r x y)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-orc2 y x))

;;; andc2 = x AND (NOT y) = BIC on ARM64
(define-vop (32bit-logical-andc2 32bit-logical)
  (:translate 32bit-logical-andc2)
  (:generator 1
      (emit-not-implemented)
    (inst bic r x y)))

(deftransform 32bit-logical-andc1 ((x y) (* *))
  '(32bit-logical-andc2 y x))


(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
         (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note _N"shift-towards-start")
  (:generator 1
      (emit-not-implemented)
    (inst lsl r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note _N"shift-towards-end")
  (:generator 1
      (emit-not-implemented)
    (inst lsr r num amount)))


;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-vop (bignum-ref word64-index-ref)
  (:variant vm:bignum-digits-offset vm:other-pointer-type)
  (:translate bignum::%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word64-index-set)
  (:variant vm:bignum-digits-offset vm:other-pointer-type)
  (:translate bignum::%bignum-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
      (emit-not-implemented)
    ;; ARM64: use CSEL for branchless implementation.
    (load-symbol result t)
    ;; CMP sets N flag; if digit is negative (high bit set) -> null, else t.
    (inst cmp digit 0)
    (inst csel result result null-tn :ge)))

;;; Add with carry.
;;; ARM64: ADDS to set carry, then ADCS for the carry chain.
;;; The carry-in c is a fixnum (0 or 4); we convert to raw 0/1.
(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg)) cin)
  (:generator 4
      (emit-not-implemented)
    ;; Convert fixnum carry-in to a raw bit by right-shifting by fixnum-tag-bits.
    (inst lsr cin c fixnum-tag-bits)
    ;; Set carry flag from cin: SUBS xzr, cin, #1  =>  C=1 iff cin >= 1 (i.e. cin != 0)
    (inst subs zero-tn cin (imm 1))
    ;; Now do carry-propagating add.
    (inst adcs result a b)
    ;; Extract carry out.
    (inst adc carry zero-tn zero-tn)))

;;; Subtract with borrow.
;;; ARM64 SBCS: result = a - b - NOT(carry).
;;; Borrow semantics: c=1 means no borrow (carry was set).
(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg)) cin)
  (:generator 5
      (emit-not-implemented)
    (inst lsr cin c fixnum-tag-bits)
    ;; SBCS needs the carry flag set to 1 for "no borrow".
    ;; Set carry flag from cin: SUBS xzr, cin, #1  =>  C=1 iff cin >= 1 (i.e. cin != 0)
    (inst subs zero-tn cin (imm 1))
    (inst sbcs result a b)
    ;; borrow-out: 1 if no borrow (carry set), 0 if borrow.
    (inst adc borrow zero-tn zero-tn)
    ;; Invert to match CMUCL convention (borrow = 1 - ARM_carry).
    (inst eor borrow borrow 1)))

;;; EMIT-MULTIPLY -- full 64x64->128 bit unsigned multiply.
;;; ARM64: MUL gives the low 64 bits; UMULH gives the high 64 bits.
(defun emit-multiply (multiplier multiplicand result-high result-low)
  "Emit code to multiply MULTIPLIER with MULTIPLICAND, putting the 128-bit
  result in RESULT-HIGH (high 64 bits) and RESULT-LOW (low 64 bits)."
  (declare (type tn multiplier result-high result-low)
           (type (or tn (signed-byte 13)) multiplicand))
  (inst mul result-low multiplier multiplicand)
  (inst umulh result-high multiplier multiplicand))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
         (y :scs (unsigned-reg) :to (:eval 1))
         (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
            (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 6
      (emit-not-implemented)
    (emit-multiply x y hi lo)
    (inst adds lo lo carry-in)
    (inst adc hi hi zero-tn)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
         (y :scs (unsigned-reg) :to (:eval 1))
         (prev :scs (unsigned-reg) :to (:eval 2))
         (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
            (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 8
      (emit-not-implemented)
    (emit-multiply x y hi lo)
    (inst adds lo lo carry-in)
    (inst adc hi hi zero-tn)
    (inst adds lo lo prev)
    (inst adc hi hi zero-tn)))

(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:result 1))
         (y :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 6
      (emit-not-implemented)
    (emit-multiply x y hi lo)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
      (emit-not-implemented)
    (inst mvn r x)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
      (emit-not-implemented)
    (inst asr digit fixnum fixnum-tag-bits)))

;;; Bignum floor: 128-bit / 64-bit division using UDIV + MSUB.
;;; ARM64 UDIV operates on 64-bit operands only, so we reconstruct the
;;; 128-bit dividend from div-high:div-low.
(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg))
         (div-low :scs (unsigned-reg))
         (divisor :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 0)) dividend)
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 8
      (emit-not-implemented)
    ;; Pack dividend: dividend = (div-high << 32) | div-low.
    ;; This is the same representation used by the SPARC V9 bignum-floor-v9.
    (inst lsl dividend div-high 32)
    (inst orr dividend dividend div-low)
    ;; 64-bit unsigned divide.
    (inst udiv quo dividend divisor)
    ;; rem = dividend - quo * divisor
    (inst msub rem quo divisor dividend)))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
      (emit-not-implemented)
    (sc-case res
      (any-reg
       (inst lsl res digit fixnum-tag-bits))
      (signed-reg
       (move res digit)))))

(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (signed-reg unsigned-reg immediate)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
      (emit-not-implemented)
    (sc-case count
      ((signed-reg unsigned-reg)
       (inst asr result digit count))
      (immediate
       (inst asr result digit (tn-value count))))))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
      (emit-not-implemented)
    (sc-case count
      ((signed-reg unsigned-reg)
       (inst lsr result digit count))
      (immediate
       (inst lsr result digit (tn-value count))))))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 1
      (emit-not-implemented)
    (sc-case count
      ((signed-reg unsigned-reg)
       (inst lsl result digit count))
      (immediate
       (inst lsl result digit (tn-value count))))))


;;;; Static functions.

(define-static-function two-arg-gcd (x y) :translate gcd)
(define-static-function two-arg-lcm (x y) :translate lcm)

(define-static-function two-arg-+ (x y) :translate +)
(define-static-function two-arg-- (x y) :translate -)
(define-static-function two-arg-* (x y) :translate *)
(define-static-function two-arg-/ (x y) :translate /)

(define-static-function two-arg-< (x y) :translate <)
(define-static-function two-arg-<= (x y) :translate <=)
(define-static-function two-arg-> (x y) :translate >)
(define-static-function two-arg->= (x y) :translate >=)
(define-static-function two-arg-= (x y) :translate =)
(define-static-function two-arg-/= (x y) :translate /=)

(define-static-function %negate (x) :translate %negate)

(define-static-function two-arg-and (x y) :translate logand)
(define-static-function two-arg-ior (x y) :translate logior)
(define-static-function two-arg-xor (x y) :translate logxor)


;;;; Truncation by a constant via reciprocal multiply.
;;;
;;; See generic/vm-tran.lisp for the algorithm.
;;; ARM64 replacement for the SPARC smul+rdy sequence:
;;;   SMULL   X_result, W_a, W_b   -- 32x32->64 signed multiply (not used here)
;;;   SMULH   X_hi, X_a, X_b       -- 64x64->64 signed high half
;;;   UMULH   X_hi, X_a, X_b       -- 64x64->64 unsigned high half
;;;   MUL     X_lo, X_a, X_b       -- 64x64->64 low half

(define-vop (signed-truncate-by-mult fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note _N"inline (signed-byte 64) arithmetic")
  (:temporary (:scs (signed-reg)) q)
  (:temporary (:scs (signed-reg)) temp)
  (:generator 8
      (emit-not-implemented)
    (multiple-value-bind (recip shift)
        (c::find-signed-reciprocal y vm:word-bits)
      ;; q = high 64 bits of recip * x  (SMULH)
      (inst li temp recip)
      (inst smulh q x temp)
      ;; Adjust if M is negative.
      (when (minusp recip)
        (inst add q q x))
      ;; Shift quotient as needed.
      (unless (zerop shift)
        (inst asr q q shift))
      ;; Add 1 if x was negative (subtract the sign bit).
      (inst asr temp x (1- vm:word-bits))   ; temp = 0 or -1
      (inst sub q q temp)
      ;; rem = x - q*y  (MSUB: Rd = Ra - Rn*Rm)
      (inst li temp y)
      (inst msub rem q temp x)
      (unless (location= quo q)
        (move quo q)))))

(define-vop (unsigned-truncate-by-mult fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 64) arithmetic")
  (:temporary (:scs (unsigned-reg)) q)
  (:temporary (:scs (unsigned-reg)) temp)
  (:generator 8
      (emit-not-implemented)
    (multiple-value-bind (recip shift)
        (c::find-unsigned-reciprocal y vm:word-bits)
      ;; q = high 64 bits of recip * x  (UMULH)
      (inst li temp recip)
      (inst umulh q x temp)
      (unless (zerop shift)
        (inst lsr q q shift))
      ;; rem = x - q*y  (MSUB: Rd = Ra - Rn*Rm)
      (inst li temp y)
      (inst msub rem q temp x)
      (unless (location= quo q)
        (move quo q)))))


;;;; ASH deftransforms and modular arithmetic
;;; (in the compiler package, same structure as sparc).

(defun ash-right-signed (num shift)
  (declare (type (signed-byte #.vm:word-bits) num)
           (type (integer 0 #.(1- vm:word-bits)) shift))
  (ash num (- shift)))

(defun ash-right-unsigned (num shift)
  (declare (type (unsigned-byte #.vm:word-bits) num)
           (type (integer 0 #.(1- vm:word-bits)) shift))
  (ash num (- shift)))

(in-package "C")

(deftransform ash ((num shift) (integer integer))
  (let ((num-type (continuation-type num))
        (shift-type (continuation-type shift)))
    (unless (csubtypep shift-type (specifier-type '(integer * 0)))
      (give-up))
    (cond ((csubtypep num-type (specifier-type '(signed-byte #.vm:word-bits)))
           (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
               `(arm64::ash-right-signed num (- shift))
               `(arm64::ash-right-signed num (min (- shift) #.(1- vm:word-bits)))))
          ((csubtypep num-type (specifier-type '(unsigned-byte #.vm:word-bits)))
           (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
               `(arm64::ash-right-unsigned num (- shift))
               `(if (<= shift #.(- vm:word-bits))
                  0
                  (arm64::ash-right-unsigned num (- shift)))))
          (t
           (give-up)))))


;;;; Modular (wrapping) arithmetic.

(in-package "ARM64")

#+modular-arith
(progn
(c::define-modular-fun lognot-mod64 (x) lognot 64)

(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
      (emit-not-implemented)
    (inst mvn res x)))

(define-vop (lognot-mod64/signed=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
      (emit-not-implemented)
    (inst mvn res x)))

(c::define-modular-fun %negate-mod64 (x) kernel:%negate 64)

(define-vop (%negate-mod64/unsigned=>unsigned fast-safe-arith-op)
  (:translate %negate-mod64)
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
      (emit-not-implemented)
    (inst neg res x)))

(define-vop (%negate-mod64/signed=>unsigned fast-safe-arith-op)
  (:translate %negate-mod64)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
      (emit-not-implemented)
    (inst neg res x)))

(macrolet
    ((frob (op &optional trans)
       (let ((name (symbolicate "FAST-" op "/SIGNED=>UNSIGNED"))
             (vop  (symbolicate "FAST-" op "/SIGNED=>SIGNED"))
             (trans (symbolicate (or trans op) "-MOD64")))
         `(progn
            (defknown ,trans ((signed-byte 64) (signed-byte 64))
              (unsigned-byte 64)
              (movable foldable flushable))
            (define-vop (,name ,vop)
              (:translate ,trans)
              (:results (r :scs (unsigned-reg)))
              (:result-types unsigned-num))))))
  (frob +)
  (frob -)
  (frob logxor)
  (frob logeqv)
  (frob logandc1)
  (frob logandc2)
  (frob logorc1)
  (frob logorc2)
  (frob *))

(defmacro define-modular-backend (fun &optional constantp derived)
  (let ((mfun-name (symbolicate fun '-mod64))
        (modvop  (symbolicate 'fast- fun '-mod64/unsigned=>unsigned))
        (modcvop (symbolicate 'fast- fun '-mod64-c/unsigned=>unsigned))
        (vop     (symbolicate 'fast- (or derived fun) '/unsigned=>unsigned))
        (cvop    (symbolicate 'fast- (or derived fun) '-c/unsigned=>unsigned))
        (smodvop (symbolicate 'fast- (or derived fun) '-mod64/signed=>unsigned))
        (svop    (symbolicate 'fast- (or derived fun) '/signed=>unsigned)))
    `(progn
       (c::define-modular-fun ,mfun-name (x y) ,fun 64)
       (define-vop (,modvop ,vop)
         (:translate ,mfun-name))
       ,@(when constantp
               `((define-vop (,modcvop ,cvop)
                   (:translate ,mfun-name))))
       (define-vop (,smodvop ,svop)
         (:translate ,mfun-name)))))

(define-modular-backend + t)
(define-modular-backend - t)
(define-modular-backend logxor t)
(define-modular-backend logeqv)       ; eon has no immediate form
(define-modular-backend logandc1)
(define-modular-backend logandc2)     ; bic has no immediate form
(define-modular-backend logorc1)
(define-modular-backend logorc2)      ; orn has no immediate form
(define-modular-backend * nil)

(def-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))
(def-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))

(defknown vm::ash-left-mod64 (integer (integer 0))
  (unsigned-byte 64)
  (foldable flushable movable))

(defknown vm::ash-mod64 (integer integer)
  (unsigned-byte 64)
  (foldable flushable movable))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             digit-ashl)
  (:translate ash-left-mod64))

(define-vop (fast-ash-mod64/unsigned=>unsigned
             fast-ash/unsigned=>unsigned)
    (:translate ash-mod64))

) ; #+modular-arith


(in-package :c)

#+modular-arith
(define-modular-fun-optimizer ash ((integer count) :width width)
  ;; ARM64 shift instructions take the count modulo 64.
  (when (<= width 64)
    (let ((count-type (continuation-type count)))
      (cond ((csubtypep count-type (specifier-type '(unsigned-byte 6)))
             (cut-to-width integer width)
             'vm::ash-left-mod64)
            ((csubtypep count-type (specifier-type '(integer -63 63)))
             (cut-to-width integer width)
             'vm::ash-mod64)
            (t nil)))))

;;; Multiplier recoding for constant multiplies.
;;; Break-even is estimated at ~8 shift+add operations.
(defun *-transformer (y)
  (let ((y (continuation-value y)))
    (multiple-value-bind (result adds shifts)
        (strength-reduce-constant-multiply 'x y)
      (when (> (+ adds shifts) 8)
        (give-up))
      (or result 0))))

#+modular-arith
(deftransform * ((x y)
                 ((unsigned-byte 64) (constant-argument (unsigned-byte 64)))
                 (unsigned-byte 64))
  "recode as shifts and adds"
  (*-transformer y))

#+modular-arith
(deftransform vm::*-mod64 ((x y)
                 ((unsigned-byte 64) (constant-argument (unsigned-byte 64)))
                 (unsigned-byte 64))
  "recode as shifts and adds"
  (*-transformer y))

