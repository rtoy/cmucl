;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/sap.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the ARM64 VM definition of SAP operations.
;;;
;;; Derived from the SPARC implementation by William Lott.
;;; Ported to ARM64.
;;;
(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note _N"pointer to SAP coercion")
  (:generator 1
    (emit-not-implemented)
    (loadw y x sap-pointer-slot other-pointer-type)))

;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; Move an untagged SAP to a tagged representation.
;;;
;;; Note: ARM64 uses load-acquire / store-release for heap allocation
;;; sequences where memory ordering matters, but with-fixed-allocation
;;; handles that internally; we just storew the raw pointer.
;;;
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (res :scs (descriptor-reg)))
  (:note _N"SAP to pointer coercion")
  (:generator 20
    (emit-not-implemented)
    (with-fixed-allocation (res ndescr sap-type sap-size)
      (storew sap res sap-pointer-slot other-pointer-type))))

;;;
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))


;;; Move untagged sap values.
;;;
;;; ARM64: a plain MOV Xd, Xn suffices; no delay slots to worry about.
;;;
(define-vop (sap-move)
  (:args (x :target y
            :scs (sap-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
               :load-if (not (location= x y))))
  (:note _N"SAP move")
  (:effects)
  (:affected)
  (:generator 0
    (emit-not-implemented)
    (move y x)))

;;;
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))


;;; Move untagged sap arguments/return-values.
;;;
;;; ARM64 stack slots are 8-byte aligned; storew uses STR Xn, [fp, #off].
;;;
(define-vop (move-sap-argument)
  (:args (x :target y
            :scs (sap-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note _N"SAP argument move")
  (:generator 0
    (emit-not-implemented)
    (sc-case y
      (sap-reg
       (move y x))
      (sap-stack
       (storew x fp (tn-offset y))))))

;;;
(define-move-vop move-sap-argument :move-argument
  (descriptor-reg sap-reg) (sap-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged sap to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (sap-reg) (descriptor-reg))



;;;; SAP-INT and INT-SAP

;;; On ARM64, pointers and unsigned integers are both 64-bit, so these
;;; are plain register moves — identical to the SPARC version.

(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (move int sap)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (move sap int)))



;;;; POINTER+ and POINTER-

;;; ARM64 ADD / SUB accept a 12-bit unsigned immediate (optionally
;;; shifted left by 12), which is a strict superset of SPARC's
;;; signed-byte-13 immediate range for the common case.  We widen the
;;; constant VOP's :constant type accordingly.

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 2
    (emit-not-implemented)
    (inst add res ptr offset)))

;;; ARM64 immediate ADD: 0..4095 unsigned (12-bit), no sign extension.
;;; For negative constant offsets the compiler will fall back to the
;;; register variant above, so we keep the immediate type unsigned here.
;;; If your port's assembler supports SUB-immediate for negatives, split
;;; into two VOPs (one ADD-imm, one SUB-imm).
(define-vop (pointer+-c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (unsigned-byte 12)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (inst add res ptr offset)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
         (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (emit-not-implemented)
    (inst sub res ptr1 ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

;;; ARM64 load/store instruction mapping
;;; ─────────────────────────────────────────────────────────────────────
;;;  SPARC          │ ARM64 (register offset)  │ ARM64 (imm offset)
;;; ────────────────┼──────────────────────────┼────────────────────────
;;;  ldsb  (s8)     │ LDRSB  Wt,[Xn,Xm]        │ LDURSB Wt,[Xn,#imm]   :byte
;;;  ldub  (u8)     │ LDRB   Wt,[Xn,Xm]        │ LDURB  Wt,[Xn,#imm]   :byte
;;;  ldsh  (s16)    │ LDRSH  Wt,[Xn,Xm]        │ LDURSH Wt,[Xn,#imm]   :word16
;;;  lduh  (u16)    │ LDRH   Wt,[Xn,Xm]        │ LDURH  Wt,[Xn,#imm]   :word16
;;;  ld   (u32/ptr) │ LDR    Xt,[Xn,Xm]        │ LDUR   Xt,[Xn,#imm]   :word32
;;;  stb            │ STRB   Wt,[Xn,Xm]        │ STURB  Wt,[Xn,#imm]   :byte
;;;  sth            │ STRH   Wt,[Xn,Xm]        │ STURH  Wt,[Xn,#imm]   :word16
;;;  st             │ STR    Xt,[Xn,Xm]        │ STUR   Xt,[Xn,#imm]   :word32
;;;  ldf  (f32)     │ LDR    St,[Xn,Xm]        │ LDUR   St,[Xn,#imm]   :single
;;;  stf  (f32)     │ STR    St,[Xn,Xm]        │ STUR   St,[Xn,#imm]   :single
;;;  lddf (f64)     │ LDR    Dt,[Xn,Xm]        │ LDUR   Dt,[Xn,#imm]   :double
;;;  stdf (f64)     │ STR    Dt,[Xn,Xm]        │ STUR   Dt,[Xn,#imm]   :double
;;;
;;; Key differences from SPARC:
;;;  • No load delay slots on ARM64.
;;;  • Immediate offsets: scaled by access size (byte imm 0–255 unscaled
;;;    via LDUR, or 0–4095*size via LDR/STR). The constant VOPs below
;;;    use the signed-byte-9 unscaled form (LDUR/STUR) so they cover the
;;;    same signed range as SPARC's 13-bit imm in the small-offset case.
;;;    Widen to (signed-byte 12) * element-size if your assembler uses
;;;    scaled addressing.
;;;  • Float moves: FMOV Sd,Sd for single; no separate SPARC-style
;;;    fmovs needed — just use (inst fmov result value).
;;;  • Double moves: use (move-double-reg result value) as before, or
;;;    (inst fmov result value) if your assembler handles D-regs directly.

(eval-when (compile eval)

(defmacro def-system-ref-and-set
          (ref-name set-name sc type size &optional signed)
  (let ((ref-name-c (symbolicate ref-name "-C"))
        (set-name-c (symbolicate set-name "-C")))
    `(progn
       ;; ── Register-offset ref ──────────────────────────────────────
       ;; Two-register addressing: [Xn, Xm] encoded as (reg-offset sap offset).
       (define-vop (,ref-name)
         (:translate ,ref-name)
         (:policy :fast-safe)
         (:args (sap    :scs (sap-reg))
                (offset :scs (signed-reg)))
         (:arg-types system-area-pointer signed-num)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 5
           (emit-not-implemented)
           (inst ,(ecase size
                    (:byte   (if signed 'ldrsb 'ldrb))
                    (:word16 (if signed 'ldrsh 'ldrh))
                    (:word32 (if signed 'ldrsw 'ldr.w))
                    (:single 'ldr)   ; assembler selects S-reg form
                    (:double 'ldr))  ; assembler selects D-reg form
                  result (reg-offset sap offset))))

       ;; ── Immediate-offset ref ─────────────────────────────────────
       ;; LDUR family: unscaled signed-byte-9 immediate, [Xn, #imm9].
       ;; Covers -256..255.  ldursb/ldursh sign-extend byte/halfword.
       (define-vop (,ref-name-c)
         (:translate ,ref-name)
         (:policy :fast-safe)
         (:args (sap :scs (sap-reg)))
         (:arg-types system-area-pointer (:constant (signed-byte 9)))
         (:info offset)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 4
           (emit-not-implemented)
           (inst ,(ecase size
                    (:byte   (if signed 'ldursb 'ldurb))
                    (:word16 (if signed 'ldursh 'ldurh))
                    (:word32 (if signed 'ldursw 'ldur.w))
                    (:single 'ldur)
                    (:double 'ldur))
                  result sap offset)))

       ;; ── Register-offset set ──────────────────────────────────────
       ;; Two-register addressing: [Xn, Xm] encoded as (reg-offset sap offset).
       (define-vop (,set-name)
         (:translate ,set-name)
         (:policy :fast-safe)
         (:args (sap    :scs (sap-reg))
                (offset :scs (signed-reg))
                (value  :scs (,sc) :target result))
         (:arg-types system-area-pointer signed-num ,type)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 5
           (emit-not-implemented)
           (inst ,(ecase size
                    (:byte   'strb)
                    (:word16 'strh)
                    (:word32 'str.w)
                    (:single 'str)
                    (:double 'str))
                  value (reg-offset sap offset))
           (unless (location= result value)
             ,@(case size
                 (:single
                  '((inst fmov result value)))
                 (:double
                  '((move-double-reg result value)))
                 (t
                  '((inst mov result value)))))))

       ;; ── Immediate-offset set ─────────────────────────────────────
       ;; STUR family: unscaled signed-byte-9 immediate, [Xn, #imm9].
       (define-vop (,set-name-c)
         (:translate ,set-name)
         (:policy :fast-safe)
         (:args (sap   :scs (sap-reg))
                (value :scs (,sc) :target result))
         (:arg-types system-area-pointer (:constant (signed-byte 9)) ,type)
         (:info offset)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 4
           (emit-not-implemented)
           (inst ,(ecase size
                    (:byte   'sturb)
                    (:word16 'sturh)
                    (:word32 'stur.w)
                    (:single 'stur)
                    (:double 'stur))
                  value sap offset)
           (unless (location= result value)
             ,@(case size
                 (:single
                  '((inst fmov result value)))
                 (:double
                  '((move-double-reg result value)))
                 (t
                  '((inst mov result value))))))))))

); eval-when (compile eval)

(def-system-ref-and-set sap-ref-8 %set-sap-ref-8
  unsigned-reg positive-fixnum :byte nil)
(def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
  signed-reg tagged-num :byte t)
(def-system-ref-and-set sap-ref-16 %set-sap-ref-16
  unsigned-reg positive-fixnum :word16 nil)
(def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
  signed-reg tagged-num :word16 t)
(def-system-ref-and-set sap-ref-32 %set-sap-ref-32
  unsigned-reg unsigned-num :word32 nil)
(def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
  signed-reg signed-num :word32 t)
(def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
  sap-reg system-area-pointer :word32)
(def-system-ref-and-set sap-ref-single %set-sap-ref-single
  single-reg single-float :single)
(def-system-ref-and-set sap-ref-double %set-sap-ref-double
  double-reg double-float :double)



;;; Noise to convert normal lisp data objects into SAPs.

;;; ARM64: ADD Xd, Xn, #imm  where imm = vector-data-offset*word-bytes
;;;        minus the other-pointer tag.  Same structure as SPARC; the
;;;        assembler encodes the 12-bit unsigned immediate automatically.
(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (emit-not-implemented)
    (inst add sap vector
          (- (* vector-data-offset word-bytes) other-pointer-type))))



;;; 64-bit SAP accessors.
;;;
;;; ARM64 is natively 64-bit so we can implement these as true 64-bit
;;; loads/stores rather than the two 32-bit halves needed on SPARC.
;;; If native 64-bit VOPs are not yet wired up in this backend, the
;;; SPARC-style transform fallbacks below remain correct.

;;; Native 64-bit VOPs (preferred on ARM64)
(define-vop (sap-ref-64)
  (:translate sap-ref-64)
  (:policy :fast-safe)
  (:args (sap    :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (emit-not-implemented)
    (inst ldr result (reg-offset sap offset))))

(define-vop (sap-ref-64-c)
  (:translate sap-ref-64)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 9)))
  (:info offset)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 3
    (emit-not-implemented)
    (inst ldur result sap offset)))

(define-vop (signed-sap-ref-64)
  (:translate signed-sap-ref-64)
  (:policy :fast-safe)
  (:args (sap    :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 4
    (emit-not-implemented)
    (inst ldr result (reg-offset sap offset))))   ; 64-bit LDR is always sign-preserving

(define-vop (%set-sap-ref-64)
  (:translate %set-sap-ref-64)
  (:policy :fast-safe)
  (:args (sap    :scs (sap-reg))
         (offset :scs (signed-reg))
         (value  :scs (unsigned-reg) :target result))
  (:arg-types system-area-pointer signed-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (emit-not-implemented)
    (inst str value (reg-offset sap offset))
    (unless (location= result value)
      (inst mov result value))))

(define-vop (%set-signed-sap-ref-64)
  (:translate %set-signed-sap-ref-64)
  (:policy :fast-safe)
  (:args (sap    :scs (sap-reg))
         (offset :scs (signed-reg))
         (value  :scs (signed-reg) :target result))
  (:arg-types system-area-pointer signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 4
    (emit-not-implemented)
    (inst str value (reg-offset sap offset))
    (unless (location= result value)
      (inst mov result value))))

;;; Fallback transforms — kept for compatibility if the native VOPs above
;;; are not yet recognised by the compiler's type machinery.  On a fully
;;; wired ARM64 backend these will normally be dead code.
#-arm64-native-64bit-sap
(progn
  (deftransform sap-ref-64 ((sap offset) (* *))
    '(logior (ash (sap-ref-32 sap offset) 32)
             (sap-ref-32 sap (+ offset 4))))

  (deftransform signed-sap-ref-64 ((sap offset) (* *))
    '(logior (ash (signed-sap-ref-32 sap offset) 32)
             (sap-ref-32 sap (+ 4 offset))))

  (deftransform %set-sap-ref-64 ((sap offset value) (* * *))
    '(progn
       (%set-sap-ref-32 sap offset (ash value -32))
       (%set-sap-ref-32 sap (+ offset 4) (logand value #xffffffff))))

  (deftransform %set-signed-sap-ref-64 ((sap offset value) (* * *))
    '(progn
       (%set-signed-sap-ref-32 sap offset (ash value -32))
       (%set-sap-ref-32 sap (+ 4 offset) (logand value #xffffffff)))))
