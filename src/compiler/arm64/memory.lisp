;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; Ported to ARM64 from the SPARC backend.
;;; **********************************************************************
;;;
;;;    This file contains the ARM64 definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Originally written by Rob MacLachlan (SPARC version by William Lott).
;;;
;;; Ported to ARM64: replaces SPARC load/store mnemonics and addressing
;;; with ARM64 equivalents:
;;;
;;;   SPARC inst LD   -> ARM64 inst LDR   (64-bit load)
;;;   SPARC inst ST   -> ARM64 inst STR   (64-bit store)
;;;   SPARC inst LDUH -> ARM64 inst LDRH  (unsigned 16-bit / word16 load)
;;;   SPARC inst LDSH -> ARM64 inst LDRSH (signed 16-bit / word16 load)
;;;   SPARC inst STH  -> ARM64 inst STRH  (16-bit / word16 store)
;;;   SPARC inst LDUB -> ARM64 inst LDRB  (unsigned 8-bit load)
;;;   SPARC inst LDSB -> ARM64 inst LDRSB (signed 8-bit load)
;;;   SPARC inst STB  -> ARM64 inst STRB  (8-bit store)
;;;   SPARC inst LDSW -> ARM64 inst LDRSW (signed 32-bit load, v9 only)
;;;   SPARC inst ADD  -> ARM64 inst ADD
;;;   SPARC inst LI   -> ARM64 inst LI    (materialise integer into register)
;;;   SPARC inst SRLN -> ARM64 inst LSR   (logical shift right)
;;;   SPARC inst MOVE -> ARM64 inst MOVE  (register move pseudo)
;;;
;;; ARM64 memory reference constructors (from arm64-insts):
;;;   (mem base offset)        -> [base, #offset]   unsigned scaled offset
;;;   (reg-offset base rm)     -> [base, rm]         register offset
;;;   (pre-index base offset)  -> [base, #offset]!   pre-index
;;;   (post-index base offset) -> [base], #offset    post-index
;;;
;;; For immediate offsets that fit in (signed-byte 9), (mem object offset)
;;; is used.  For larger offsets the value is materialised into a temporary
;;; with LI and (reg-offset object temp) is used.
;;;

(in-package "ARM64")

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the
;;; offset to be read or written is a property of the VOP used.

(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:policy :fast-safe)
  (:generator 4
    (emit-not-implemented)
    (loadw value object offset lowtag temp)))

(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:policy :fast-safe)
  (:generator 4
    (emit-not-implemented)
    (storew value object offset lowtag temp)))


;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref,
;;; where the offset is constant at compile time but varies for
;;; different uses.  We add in the standard g-vector overhead.

(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (emit-not-implemented)
    (loadw value object (+ base offset) lowtag temp)))

(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (emit-not-implemented)
    (storew value object (+ base offset) lowtag temp)))


;;;; Indexed references

;;; Define-Indexer  --  Internal
;;;
;;;    Define VOPs for indexed memory reference.
;;;
;;; SHIFT is the net left-shift needed to convert a tagged fixnum index
;;; into a byte offset, expressed as:
;;;
;;;   shift = vm:word-shift - vm:fixnum-tag-bits - log2(word64-bytes / element-bytes)
;;;         = vm:word-shift - vm:fixnum-tag-bits - n
;;;
;;; where N counts how many times smaller the element is than a word64:
;;;
;;;   word64 (n=0): (- word-shift fixnum-tag-bits)     = (- 3 2)     = +1
;;;   word32 (n=1): (- word-shift fixnum-tag-bits 1)   = (- 3 2 1)   =  0
;;;   word16 (n=2): (- word-shift fixnum-tag-bits 2)   = (- 3 2 2)   = -1
;;;   byte   (n=3): (- word-shift fixnum-tag-bits 3)   = (- 3 2 3)   = -2
;;;
;;; Positive SHIFT -> LSL; zero -> no instruction; negative -> LSR by abs(shift).
;;;
;;; Other differences from the SPARC original:
;;;   - Immediate-offset guard: (signed-byte 13) -> (signed-byte 9)
;;;     ARM64 unscaled (LDUR/STUR family) takes a 9-bit signed offset as bare
;;;     rn+imm9 with no alignment constraint.  Scaled forms (LDR/STR, LDRH/STRH
;;;     etc.) require offsets to be multiples of the access size, which cannot
;;;     be guaranteed for arbitrary slot offsets.  Therefore only the unscaled
;;;     UOP form is used throughout: for signed-9 offsets directly, and for
;;;     larger offsets via ADD to form the address followed by UOP at offset 0.

(defmacro define-indexer (name write-p uop shift)
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg zero immediate))
            ,@(when write-p
                '((value :scs (any-reg descriptor-reg) :target result))))
     (:arg-types * tagged-num ,@(when write-p '(*)))
     (:temporary (:scs (non-descriptor-reg)) temp)
     (:results (,(if write-p 'result 'value)
                :scs (any-reg descriptor-reg)))
     (:result-types *)
     (:variant-vars offset lowtag)
     (:policy :fast-safe)
     (:generator 5
       (emit-not-implemented)
       (sc-case index
         ;; Constant (or zero) index: compute the full byte offset at
         ;; compile time.  If it fits in a signed-9, emit the unscaled
         ;; LDUR/STUR form (bare rn + imm9).  Otherwise materialise into
         ;; TEMP, add to BASE, and use LDUR/STUR at offset 0.
         ((immediate zero)
          (let ((byte-offset (- (+ (if (sc-is index zero)
                                       0
                                       ;; ASH by SHIFT converts the tagged
                                       ;; fixnum value to a byte offset.
                                       (ash (tn-value index) ,shift))
                                   (ash offset vm:word-shift))
                                lowtag)))
            (etypecase byte-offset
              ;; Signed-9: use LDUR/STUR with bare rn + imm9 arguments.
              ((signed-byte 9)
               (inst ,uop value object byte-offset))
              ;; Out of range: materialise into TEMP, add to base,
              ;; then use LDUR/STUR at offset 0.
              ((or (unsigned-byte 32) (signed-byte 32))
               (inst li temp byte-offset)
               (inst add temp object temp)
               (inst ,uop value temp 0)))))
         ;; Variable index: shift the tagged fixnum to a byte displacement,
         ;; fold in the base offset, add to object, then issue LDUR/STUR at offset 0.
         (t
          ,@(cond
              ((plusp shift)
               `((inst lsl temp index ,shift)))
              ((minusp shift)
               `((inst lsr temp index ,(- shift))))
              (t nil))
          (inst add temp ,(if (zerop shift) 'index 'temp)
                (ash offset vm:word-shift))
          (inst sub temp temp lowtag)
          ;; TEMP holds the byte offset; add to object then load/store at 0.
          (inst add temp object temp)
          (inst ,uop value temp 0)))
       ,@(when write-p
           '((move result value))))))


;;; Instantiate the indexers.
;;;
;;; ARM64 mnemonics (all standard A64 ISA).
;;; Each indexer uses the unscaled form (UOP) for all accesses:
;;;   UOP       width / notes
;;;   LDUR      64-bit unsigned/signed load  (word64 ref)
;;;   STUR      64-bit store                 (word64 set)
;;;   LDUR.W    32-bit zero-extending load   (word32 unsigned ref)
;;;   LDURSW    32-bit sign-extending load   (word32 signed ref)
;;;   STUR.W    32-bit store                 (word32 set)
;;;   LDURH     16-bit zero-extending load   (word16 unsigned ref)
;;;   LDURSH    16-bit sign-extending load   (word16 signed ref)
;;;   STURH     16-bit store                 (word16 set)
;;;   LDURB     8-bit zero-extending load
;;;   LDURSB    8-bit sign-extending load
;;;   STURB     8-bit store
;;;
;;; Shift values expressed as (- vm:word-shift vm:fixnum-tag-bits n)
;;; where N = log2(word64-bytes / element-bytes):
;;;   word64 (n=0): (- word-shift fixnum-tag-bits)   = +1
;;;   word32 (n=1): (- word-shift fixnum-tag-bits 1) =  0
;;;   word16 (n=2): (- word-shift fixnum-tag-bits 2) = -1
;;;   byte   (n=3): (- word-shift fixnum-tag-bits 3) = -2

;;; 64-bit word references (word64).
;;; ARM64 LDR/STR operate on full 64-bit registers; signed vs. unsigned is a
;;; Lisp type-system distinction only — both variants emit identical instructions.
(define-indexer word64-index-ref        nil ldur
  #.(- vm:word-shift vm:fixnum-tag-bits))
(define-indexer signed-word64-index-ref nil ldur
  #.(- vm:word-shift vm:fixnum-tag-bits))
(define-indexer word64-index-set        t   stur
  #.(- vm:word-shift vm:fixnum-tag-bits))
(define-indexer signed-word64-index-set t   stur
  #.(- vm:word-shift vm:fixnum-tag-bits))

;;; Unsigned 32-bit word references (word32).
;;; LDR.W loads 32 bits and zero-extends to 64; LDRSW sign-extends.
;;; Both stores use STR.W which writes exactly 32 bits.
;;; Shift 0: 4-byte elements with 2 fixnum tag bits cancel exactly.
(define-indexer word32-index-ref        nil ldur.w
  #.(- vm:word-shift vm:fixnum-tag-bits 1))
(define-indexer signed-word32-index-ref nil ldursw
  #.(- vm:word-shift vm:fixnum-tag-bits 1))
(define-indexer word32-index-set        t   stur.w
  #.(- vm:word-shift vm:fixnum-tag-bits 1))

;;; 16-bit word16 references (renamed from halfword).
(define-indexer word16-index-ref        nil ldurh
  #.(- vm:word-shift vm:fixnum-tag-bits 2))
(define-indexer signed-word16-index-ref nil ldursh
  #.(- vm:word-shift vm:fixnum-tag-bits 2))
(define-indexer word16-index-set        t   sturh
  #.(- vm:word-shift vm:fixnum-tag-bits 2))

;;; 8-bit byte references.
(define-indexer byte-index-ref        nil ldurb
  #.(- vm:word-shift vm:fixnum-tag-bits 3))
(define-indexer signed-byte-index-ref nil ldursb
  #.(- vm:word-shift vm:fixnum-tag-bits 3))
(define-indexer byte-index-set        t   sturb
  #.(- vm:word-shift vm:fixnum-tag-bits 3))
