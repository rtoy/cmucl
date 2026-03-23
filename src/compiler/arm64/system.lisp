;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/system.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    ARM64 VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; SPARC conversion by William Lott and Christopher Hoover.
;;; ARM64 conversion derived from the SPARC port.
;;;
(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and result object vm:lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Grab the lowtag.
    (inst and result object lowtag-mask)
    ;; Check for various pointer types.
    (inst cmp result list-pointer-type)
    (inst b :eq done)
    (inst cmp result other-pointer-type)
    (inst b :eq other-pointer)
    (inst cmp result function-pointer-type)
    (inst b :eq function-pointer)
    (inst cmp result instance-pointer-type)
    (inst b :eq done)
    ;; Okay, it is an immediate.  If fixnum, we want zero.  Otherwise,
    ;; we want the low 8 bits.
    ;;
    ;; AArch64: TST sets flags without writing a result (ANDS Xd=XZR).
    ;; No delay slot exists on ARM64, so the fixnum zero materialisation
    ;; must be done explicitly before the branch.
    (inst tst object vm:fixnum-tag-mask)
    (inst movz result 0)            ; pre-load zero; harmless if not taken
    (inst b :eq done)
    ;; Not a fixnum: fetch the low 8 type bits.
    (inst and result object type-mask)
    (inst b done)

    FUNCTION-POINTER
    (load-type result object (- function-pointer-type))
    (inst b done)

    OTHER-POINTER
    (load-type result object (- other-pointer-type))

    DONE))


(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- vm:function-pointer-type))))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; AArch64 is little-endian: the type byte is at byte offset 0 of
    ;; the header word (i.e. the lowest-address byte), adjusted for the
    ;; function-pointer tag.  We use STURB (unscaled store byte).
    (inst sturb type function (- vm:function-pointer-type))
    (move result type)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:other-pointer-type)
    (inst lsr res res vm:type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:function-pointer-type)
    (inst lsr res res vm:type-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x   :scs (descriptor-reg) :target res)
         (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 vm:other-pointer-type)
    (inst and t1 t1 vm:type-mask)
    ;; Load DATA into t2 (untagging fixnums from any-reg/immediate, or
    ;; zero from the zero SC), then shift into the header data field and
    ;; add.  Using ADD rather than ORR avoids the bitmask-immediate
    ;; encoding restriction, and is safe here because t1 holds only the
    ;; low type-bits with the data field already cleared.
    (sc-case data
      (any-reg
       ;; DATA is a fixnum-tagged integer; remove the fixnum tag first.
       (inst asr t2 data vm:fixnum-tag-bits))
      (immediate
       (inst li t2 (tn-value data)))
      (zero
       (inst li t2 0)))
    (inst add t1 t1 (shift t2 :lsl vm:type-bits))
    (storew t1 x 0 vm:other-pointer-type)
    (move res x)))


(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    ;;
    ;; LSL by lowtag-bits then LSR by 1 gives a net left shift of
    ;; (lowtag-bits - 1), converting a tagged pointer into a positive
    ;; fixnum value.  AND with a mask cannot replicate this because the
    ;; two low fixnum-tag bits must also be cleared -- BIC/AND would
    ;; only clear the lowtag bits, leaving bits that should be zero.
    (inst lsl res ptr vm:lowtag-bits)
    (inst lsr res res 1)))

(define-vop (make-other-immediate-type)
  (:args (val  :scs (any-reg descriptor-reg))
         (type :scs (any-reg descriptor-reg immediate)
               :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (inst lsl temp val vm:type-bits)
       (inst orr res temp (tn-value type)))
      (t
       ;; TYPE is a fixnum-tagged integer; un-tag it with ASR, then
       ;; shift VAL up and OR the pieces together.
       (inst asr temp type vm:fixnum-tag-bits)
       (inst lsl res val (- vm:type-bits vm:fixnum-tag-bits))
       (inst orr res res temp)))))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move int alloc-tn)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int bsp-tn)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))


;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    ;; Read the header word, extract the word count (top bits above
    ;; type-bits), scale to bytes, then subtract the other-pointer tag
    ;; to get the byte displacement from CODE to the first instruction.
    (loadw ndescr code 0 vm:other-pointer-type)
    ;; Extract the word count and scale to bytes in one shift:
    ;; LSR by (type-bits - word-shift) = LSR by 5.
    (inst lsr ndescr ndescr (- vm:type-bits vm:word-shift))
    (inst sub ndescr ndescr vm:other-pointer-type)
    (inst add sap code ndescr)))

(define-vop (compute-function)
  (:args (code   :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    ;; Compute the byte offset from CODE to the start of the code vector,
    ;; add the caller-supplied byte OFFSET, adjust for the tag difference
    ;; between other-pointer and function-pointer, then add to CODE.
    (loadw ndescr code 0 vm:other-pointer-type)
    ;; Extract the word count and scale to bytes in one shift:
    ;; LSR by (type-bits - word-shift) = LSR by 5.
    (inst lsr ndescr ndescr (- vm:type-bits vm:word-shift))
    (inst add ndescr ndescr offset)
    (inst sub ndescr ndescr (- vm:other-pointer-type vm:function-pointer-type))
    (inst add func code ndescr)))


;;;; Other random VOPs.

(defknown unix::do-pending-interrupt () (values))
(define-vop (unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate unix::do-pending-interrupt)
  (:generator 1
    ;; AArch64 uses UDF (permanently-undefined instruction) as the trap
    ;; mechanism; the signal handler decodes the immediate from the
    ;; instruction word.  This replaces SPARC's UNIMP instruction.
    (inst udf pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst udf halt-trap)))


;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    ;; Compute the byte offset of element INDEX in the vector's data
    ;; area, accounting for the other-pointer tag.
    (let ((offset
           (- (* (+ index vector-data-offset) word-bytes) other-pointer-type)))
      ;; On AArch64 the unscaled (LDUR/STUR) immediate is a signed 9-bit
      ;; value (-256..255).  For larger offsets the loadw/storew macros
      ;; will use a register-offset form when given a TEMP argument; here
      ;; we simply assert the offset fits, matching the SPARC port's
      ;; (signed-byte 13) check for its own immediate range.
      (assert (typep offset '(signed-byte 9)))
      (inst ldur count count-vector offset)
      (inst add count count 1)
      (inst stur count count-vector offset))))


;;;; Cycle counter support.
;;;
;;; AArch64 exposes a 64-bit virtual counter via the CNTVCT_EL0 system
;;; register (accessible from EL0 when CNTKCTL_EL1.EL0VCTEN = 1, which
;;; is normally set by the OS).  This replaces the SPARC RDTICK
;;; instruction and provides a similar monotonically-increasing cycle /
;;; time-base counter.
;;;
;;; The counter is a single 64-bit value; we split it into two 32-bit
;;; halves (low, high) to match the SPARC port's two-value interface and
;;; keep WITH-CYCLE-COUNTER source-compatible.

(defknown read-cycle-counter ()
  (values (unsigned-byte 32) (unsigned-byte 32)))

(define-vop (read-cycle-counter)
  (:translate read-cycle-counter)
  (:args)
  (:policy :fast-safe)
  (:results (lo :scs (unsigned-reg))
            (hi :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) tick)
  (:generator 3
    ;; Read the virtual count register into a 64-bit temp.
    (inst mrs tick :cntvct-el0)
    ;; High 32 bits.
    (inst lsr hi tick 32)
    ;; Low 32 bits: zero-extend by masking the upper half.
    (inst and lo tick #xffffffff)))

(defun read-cycle-counter ()
  "Read the virtual instruction cycle counter available on AArch64.
The 64-bit counter is returned as two 32-bit unsigned integers.
The low 32-bit result is the first value."
  (read-cycle-counter))

(defmacro with-cycle-counter (&body body)
  "Returns the primary value of BODY as the primary value, and the
 number of CPU cycles elapsed as secondary value."
  (let ((hi0 (gensym))
        (hi1 (gensym))
        (lo0 (gensym))
        (lo1 (gensym)))
    `(multiple-value-bind (,lo0 ,hi0)
         (read-cycle-counter)
       (values (locally ,@body)
               (multiple-value-bind (,lo1 ,hi1)
                   (read-cycle-counter)
                 ;; Can't do anything about the notes about generic
                 ;; arithmetic, so silence the notes.
                 (declare (optimize (inhibit-warnings 3)))
                 (+ (ash (- ,hi1 ,hi0) 32)
                    (- ,lo1 ,lo0)))))))
