;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; arm64/insts.lisp -- Description of the ARM64 (AArch64) instruction set.
;;;
;;; Reference:
;;;   ARM Architecture Reference Manual for A-profile architecture (DDI 0487)
;;;   Encoding index: https://www.scs.stanford.edu/~zyedidia/arm64/encodingindex.html
;;;
;;; Excluded: privileged instructions, system instructions (MSR/MRS of
;;; system registers, TLBI, DC, IC, AT, ERET, SMC, HVC, DCPS*, and the
;;; barrier variants beyond non-privileged DMB/DSB/ISB).
;;;
;;; Modeled after cmucl/compiler/sparc/insts.lisp and
;;; cmucl/compiler/ppc/insts.lisp.
;;;

(in-package "ARM64")
(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
  :scheduler-p nil)



;;;; -----------------------------------------------------------------------
;;;; Registers and constants
;;;; -----------------------------------------------------------------------

;;; AArch64 has 31 general-purpose integer registers (X0-X30),
;;; a zero register (XZR/WZR, encoded as 31 in most positions),
;;; and the stack pointer (SP, also encoded as 31 in base positions).
;;; Floating-point / SIMD registers are V0-V31.

(defconstant xzr 31 "Zero/discard register encoding.")
(defconstant lr  30 "Link register (X30).")

;;; Condition codes (4-bit Cond field).
(defconstant arm64-conditions
  '#(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al :nv))

(defun condition-encoding (cond)
  (or (position cond arm64-conditions)
      (error "Unknown ARM64 condition: ~S" cond)))

;;; Shift types (2-bit field in shifted-register instructions).
(defconstant arm64-shifts '#(:lsl :lsr :asr :ror))

;;; Extend types (3-bit field in extended-register instructions).
(defconstant arm64-extends
  '#(:uxtb :uxth :uxtw :uxtx :sxtb :sxth :sxtw :sxtx))


;;;; -----------------------------------------------------------------------
;;;; Disassembler argument types
;;;; -----------------------------------------------------------------------

(disassem:define-argument-type reg
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (if (= value 31)
                   (princ "xzr" stream)
                   (format stream "x~D" value))))

(disassem:define-argument-type wreg
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (if (= value 31)
                   (princ "wzr" stream)
                   (format stream "w~D" value))))

(disassem:define-argument-type sp-reg
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (if (= value 31)
                   (princ "sp" stream)
                   (format stream "x~D" value))))

(disassem:define-argument-type fp-reg
  :printer #'(lambda (value stream dstate)
               (let ((ftype (ldb (byte 2 22)
                                 (disassem:sap-ref-int
                                  (disassem:dstate-segment-sap dstate)
                                  (disassem:dstate-cur-offs dstate)
                                  4 :little-endian))))
                 (case ftype
                   (#b00 (format stream "s~D" value))
                   (#b01 (format stream "d~D" value))
                   (t    (format stream "v~D" value))))))

(disassem:define-argument-type condition
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (princ (svref arm64-conditions value) stream)))

(disassem:define-argument-type shift-type
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (princ (svref arm64-shifts value) stream)))

(disassem:define-argument-type extend-type
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (princ (svref arm64-extends value) stream)))

(disassem:define-argument-type unsigned-imm
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (format stream "#~D" value)))

(disassem:define-argument-type signed-imm
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (format stream "#~D" value)))

(disassem:define-argument-type pc-rel-label
  :use-label #'(lambda (value dstate)
                 (+ (disassem:dstate-cur-addr dstate) (ash value 2))))

(disassem:define-argument-type cond-br-label
  :use-label #'(lambda (value dstate)
                 (+ (disassem:dstate-cur-addr dstate) (ash value 2))))


;;;; ======================================================================
;;;; Low-level word emitter
;;;; ======================================================================

(defun emit-word (segment word)
  "Emit a 32-bit little-endian instruction word to SEGMENT."
  (declare (type (unsigned-byte 32) word))
  (emit-byte segment (ldb (byte 8  0) word))
  (emit-byte segment (ldb (byte 8  8) word))
  (emit-byte segment (ldb (byte 8 16) word))
  (emit-byte segment (ldb (byte 8 24) word)))


;;;; ======================================================================
;;;; Top-level A64 encoding space (bits [28:25], [31]):
;;;;
;;;;   op0=1  op1=100x  Data Processing -- Immediate
;;;;   op0=1  op1=101x  Branches / Exceptions (non-privileged)
;;;;   op0=1  op1=x101  Data Processing -- Register
;;;;   op0=1  op1=x111  Data Processing -- Scalar FP/SIMD
;;;;   op0=1  op1=x1x0  Loads and Stores
;;;; ======================================================================


;;;; ======================================================================
;;;; DATA PROCESSING -- IMMEDIATE
;;;; ======================================================================

;;; --- PC-relative addressing ---
;;;
;;; op | immlo[1:0] | 1 0 0 0 0 | immhi[18:0] | Rd
;;;   ADR (op=0): Rd = PC + SignExtend(immhi:immlo)
;;;   ADRP (op=1): Rd = (PC & ~0xFFF) + SignExtend(immhi:immlo) << 12

(disassem:define-instruction-format (pc-rel 32
                                     :default-printer '(:name :tab rd ", " label))
  (op    :field (byte 1 31))
  (immlo :field (byte 2 29))
  (f1    :field (byte 5 24) :value #b10000)
  (immhi :field (byte 19 5))
  (rd    :field (byte 5 0) :type 'reg)
  (label :fields (list (byte 19 5) (byte 2 29))
         :use-label #'(lambda (fields dstate)
                        (let* ((hi  (first fields))
                               (lo  (second fields))
                               (raw (logior (ash hi 2) lo))
                               (off (if (logbitp 20 raw)
                                        (- raw (expt 2 21))
                                        raw)))
                          (+ (disassem:dstate-cur-addr dstate) off)))))

(define-emitter emit-pc-rel-inst 32
  (byte 1 31)   ; op
  (byte 2 29)   ; immlo
  (byte 5 24)   ; fixed #b10000
  (byte 19 5)   ; immhi
  (byte 5 0))   ; rd

(define-instruction adr (segment rd label)
  (:declare (type tn rd) (type label label))
  (:printer pc-rel ((op 0)) '("adr" :tab rd ", " label))
  (:emitter
   (emit-back-patch segment 4
     #'(lambda (segment posn)
         (let* ((off   (- (label-position label) posn))
                (immhi (ldb (byte 19 2) off))
                (immlo (ldb (byte  2 0) off)))
           (emit-pc-rel-inst segment 0 immlo #b10000 immhi (tn-offset rd)))))))

(define-instruction adrp (segment rd label)
  (:declare (type tn rd) (type label label))
  (:printer pc-rel ((op 1)) '("adrp" :tab rd ", " label))
  (:emitter
   (emit-back-patch segment 4
     #'(lambda (segment posn)
         (let* ((page-off (ash (- (logand (label-position label) (lognot #xFFF))
                                  (logand posn             (lognot #xFFF)))
                               -12))
                (immhi (ldb (byte 19 2) page-off))
                (immlo (ldb (byte  2 0) page-off)))
           (emit-pc-rel-inst segment 1 immlo #b10000 immhi (tn-offset rd)))))))


;;; --- Add/subtract immediate ---
;;;
;;; sf | op | S | 1 0 0 0 1 | sh | imm12 | Rn | Rd

;;; A shifted-imm12 bundles a 12-bit unsigned immediate with an optional
;;; logical-shift-left-12 flag, corresponding to the sh bit in the
;;; add/sub immediate encoding.
;;;
;;; Slots:
;;;   IMM   – the 12-bit unsigned immediate value.
;;;   LSL12 – when true the immediate is scaled by 2^12 (sh=1).

(defstruct (shifted-imm12
             (:constructor make-shifted-imm12
                           (imm &key lsl12)))
  (imm   0   :type (unsigned-byte 12))
  (lsl12 nil :type boolean))

(disassem:define-instruction-format (add-sub-imm 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " imm-and-shift))
  (sf    :field (byte 1 31))
  (op    :field (byte 1 30))
  (s     :field (byte 1 29))
  (f1    :field (byte 6 23) :value #b100010)
  (sh    :field (byte 1 22))
  (imm12 :field (byte 12 10))
  (rn    :field (byte 5 5)   :type 'sf-sp-reg)
  (rd    :field (byte 5 0)   :type 'sf-sp-reg)
  ;; imm-and-shift: prints "#imm, lsl #12" when sh=1, else "#imm"
  (imm-and-shift :fields (list (byte 1 22) (byte 12 10))
                 :printer #'(lambda (fields stream dstate)
                              (declare (ignore dstate))
                              (let ((sh    (first fields))
                                    (imm12 (second fields)))
                                (if (zerop sh)
                                    (format stream "#~D" imm12)
                                    (format stream "#~D, lsl #12" imm12))))))

(define-emitter emit-add-sub-imm-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 1 29)   ; s
  (byte 6 23)   ; fixed #b100010
  (byte 1 22)   ; sh
  (byte 12 10)  ; imm12
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd





;;; CMP/CMN: aliases for SUBS/ADDS with Rd = XZR.
(define-instruction-macro cmp  (rn rm/imm)
  `(inst subs  zero-tn ,rn ,rm/imm))
(define-instruction-macro cmn  (rn rm/imm)
  `(inst adds  zero-tn ,rn ,rm/imm))
(define-instruction-macro cmpw (rn rm/imm)
  `(inst subsw zero-tn ,rn ,rm/imm))
(define-instruction-macro cmnw (rn rm/imm)
  `(inst addsw zero-tn ,rn ,rm/imm))



;;; --- Logical immediate ---
;;;
;;; sf | opc[1:0] | 1 0 0 1 0 0 | N | immr | imms | Rn | Rd

(disassem:define-instruction-format (logical-imm 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " immrs))
  (sf    :field (byte 1 31))
  (opc   :field (byte 2 29))
  (f1    :field (byte 6 23) :value #b100100)
  (n     :field (byte 1 22))
  (immr  :field (byte 6 16))
  (imms  :field (byte 6 10))
  (rn    :field (byte 5 5)  :type 'reg)
  (rd    :field (byte 5 0)  :type 'reg)
  (immrs :fields (list (byte 1 22) (byte 6 16) (byte 6 10))
         :printer #'(lambda (fields stream dstate)
                      (declare (ignore dstate))
                      (let* ((mask (make-logical-imm-mask
                                    (first fields) (second fields) (third fields)))
                             (val  (handler-case
                                       (logical-immediate-value mask)
                                     (error () nil))))
                        (if val
                            (format stream "#x~X" val)
                            (format stream "N=~D,immr=~D,imms=~D"
                                    (first fields) (second fields)
                                    (third fields)))))))

(define-emitter emit-logical-imm-inst 32
  (byte 1 31)   ; sf
  (byte 2 29)   ; opc
  (byte 6 23)   ; fixed #b100100
  (byte 1 22)   ; n
  (byte 6 16)   ; immr
  (byte 6 10)   ; imms
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

;;; A logical-imm-mask bundles the three fields of the AArch64 bitmask
;;; immediate encoding used by AND/ORR/EOR/ANDS immediate instructions.
;;;
;;; Slots:
;;;   N    – 1-bit field that, together with IMMS, extends the element size.
;;;   IMMR – 6-bit rotate-right amount.
;;;   IMMS – 6-bit field encoding the number of set bits minus one.

(defstruct (logical-imm-mask
             (:constructor make-logical-imm-mask (n immr imms)))
  (n    0 :type bit)
  (immr 0 :type (unsigned-byte 6))
  (imms 0 :type (unsigned-byte 6)))

(defun encode-logical-immediate (imm &optional (width 64))
  "Encode IMM as an AArch64 bitmask immediate, returning a LOGICAL-IMM-MASK.
WIDTH is 32 or 64 (default 64).  Signals an error if IMM is not
representable as a logical immediate.

A value is encodable iff it is a non-zero, non-all-ones value whose bits
consist of a replicated element of size E (2/4/8/16/32/64) where the
element is a contiguous run of 1-bits rotated by some amount."
  (declare (type (unsigned-byte 64) imm)
           (type (member 32 64) width))
  (when (or (zerop imm)
            (= imm (ldb (byte width 0) (lognot 0))))
    (error "~S is not encodable as a logical immediate ~
            (all-zeros and all-ones are excluded)." imm))
  ;; Try each element size from smallest to largest.
  (dolist (esize '(2 4 8 16 32 64)
                 (error "~S is not encodable as a logical immediate." imm))
    (let* ((emask (ldb (byte esize 0) -1))
           (elem  (ldb (byte esize 0) imm)))
      ;; The value must be a clean replication of this element.
      (when (loop for off from 0 below width by esize
                  always (= (ldb (byte esize off) imm) elem))
        ;; elem must be a contiguous run of 1s (possibly rotated).
        ;; Find rotation: locate the LSB of the first 1-run.
        (let* (;; Normalise: rotate elem until bit 0 is 1 and top bit is 0,
               ;; giving us the canonical form 0...01...1.
               ;; Count trailing zeros to find rotation.
               (ones   (logcount elem))
               (tzeros (if (logbitp 0 elem)
                           ;; starts with 1s: count leading zeros in element
                           (- esize (integer-length elem))
                           ;; starts with 0s: count trailing zeros
                           (loop for i from 0 below esize
                                 when (logbitp i elem) return i
                                 finally (return esize))))
               ;; After rotating left by tzeros the canonical form is 0*1+
               (rotated (logand emask
                                (logior (ash elem (- esize tzeros))
                                        (ash elem tzeros))))
               ;; Canonical element must be (2^ones - 1)
               (canonical (ldb (byte ones 0) -1)))
          (when (= rotated canonical)
            ;; Encode:
            ;;   N    = 1 iff esize = 64, else 0
            ;;   imms = NOT(esize-1)[5:1] : (ones-1)   masked to 6 bits
            ;;          = ~(esize-1) masked & concatenated with (ones-1)
            ;;   immr = (esize - tzeros) mod esize
            (let* ((n    (if (= esize 64) 1 0))
                   (imms (logand #b111111
                                 (logior (logand #b111111
                                                 (lognot (1- esize)))
                                         (1- ones))))
                   (immr (mod (- esize tzeros) esize)))
              (return (make-logical-imm-mask n immr imms)))))))))

(defun logical-immediate-value (mask &optional (width 64))
  "Decode a LOGICAL-IMM-MASK back to the integer value it represents.
Useful for the disassembler immrs printer."
  (declare (type logical-imm-mask mask)
           (type (member 32 64) width))
  (let* ((n    (logical-imm-mask-n    mask))
         (immr (logical-imm-mask-immr mask))
         (imms (logical-imm-mask-imms mask))
         ;; Recover element size from N and the leading zeros of ~imms
         (esize (if (= n 1)
                    64
                    (let ((len (integer-length (logxor imms #b111111))))
                      (ash 1 (- 6 len)))))
         (ones  (1+ (logand imms (1- esize))))
         ;; Build canonical element: ones 1-bits, then rotate right by immr
         (elem  (ldb (byte esize 0)
                     (logior (ash (ldb (byte ones 0) -1) (- esize ones immr))
                             (ash (ldb (byte ones 0) -1) (- immr))))))
    ;; Replicate element across width
    (ldb (byte width 0)
         (loop with result = 0
               for off from 0 below width by esize
               do (setf result (logior result (ash elem off)))
               finally (return result)))))


;;; --- Move wide immediate ---
;;;
;;; sf | opc[1:0] | 1 0 0 1 0 1 | hw[1:0] | imm16 | Rd

(disassem:define-instruction-format (move-wide 32
                                     :default-printer
                                     '(:name :tab rd ", " imm16-hex hw))
  (sf    :field (byte 1 31))
  (opc   :field (byte 2 29))
  (f1    :field (byte 6 23) :value #b100101)
  (hw    :field (byte 2 21)
         :printer #'(lambda (value stream dstate)
                      (declare (ignore dstate))
                      (unless (zerop value)
                        (format stream ", lsl #~D" (* value 16)))))
  (imm16     :field (byte 16 5))
  (imm16-hex :field (byte 16 5)
             :printer #'(lambda (value stream dstate)
                          (declare (ignore dstate))
                          (format stream "#0x~X" value)))
  (rd    :field (byte 5 0)  :type 'sf-reg))

(define-emitter emit-move-wide-inst 32
  (byte 1 31)   ; sf
  (byte 2 29)   ; opc
  (byte 6 23)   ; fixed #b100101
  (byte 2 21)   ; hw
  (byte 16 5)   ; imm16
  (byte 5 0))   ; rd

(macrolet ((def (name sf opc &optional alias)
             `(define-instruction ,name (segment rd imm &key (shift 0))
                (:declare (type tn rd)
                          (type (unsigned-byte 16) imm)
                          (type (member ,@(if (zerop sf) '(0 16) '(0 16 32 48))) shift))
                (:printer move-wide ((sf ,sf) (opc ,opc))
                          ,@(when alias `(() :print-name ,alias)))
                (:emitter
                 ,(if (zerop sf)
                      `(unless (member shift '(0 16))
                         (error "~S: shift must be 0 or 16 for 32-bit variant, got ~D."
                                ',name shift))
                      `(unless (member shift '(0 16 32 48))
                         (error "~S: shift must be 0, 16, 32, or 48, got ~D."
                                ',name shift)))
                 (emit-move-wide-inst segment ,sf ,opc #b100101
                                      (/ shift 16) imm (tn-offset rd))))))
  (def movn  1 #b00)
  (def movz  1 #b10)
  (def movk  1 #b11)

  (def movnw 0 #b00 movn)
  (def movzw 0 #b10 movz)
  (def movkw 0 #b11 movk))



;;; --- Bitfield ---
;;;
;;; sf | opc[1:0] | 1 0 0 1 1 0 | N | immr | imms | Rn | Rd

(disassem:define-instruction-format (bitfield 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", #" immr ", #" imms))
  (sf   :field (byte 1 31))
  (opc  :field (byte 2 29))
  (f1   :field (byte 6 23) :value #b100110)
  (n    :field (byte 1 22))
  (immr :field (byte 6 16) :type 'unsigned-imm)
  (imms :field (byte 6 10) :type 'unsigned-imm)
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-bitfield-inst 32
  (byte 1 31)   ; sf
  (byte 2 29)   ; opc
  (byte 6 23)   ; fixed #b100110
  (byte 1 22)   ; n
  (byte 6 16)   ; immr
  (byte 6 10)   ; imms
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name sf opc)
             `(define-instruction ,name (segment rd rn immr imms)
                (:declare (type tn rd rn)
                          (type (unsigned-byte 6) immr imms))
                (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)))
                ;; Alias printers
                ,@(when (= opc #b00)   ; sbfm
                    `(;; ASR: imms=63 (64-bit) or 31 (32-bit)
                      (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)
                                          (imms ,(if (= sf 1) 63 31)))
                                '("ASR" :tab rd ", " rn ", #" immr))
                      ;; SXTB: immr=0 imms=7
                      (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)
                                          (immr 0) (imms 7))
                                '("SXTB" :tab rd ", " rn))
                      ;; SXTH: immr=0 imms=15
                      (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)
                                          (immr 0) (imms 15))
                                '("SXTH" :tab rd ", " rn))
                      ;; SXTW: immr=0 imms=31 (64-bit only)
                      ,@(when (= sf 1)
                          '((:printer bitfield ((sf 1) (opc #b00) (n 1)
                                                (immr 0) (imms 31))
                                      '("SXTW" :tab rd ", " rn))))))
                ,@(when (= opc #b10)   ; ubfm
                    `(;; LSR: imms=63 (64-bit) or 31 (32-bit)
                      (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)
                                          (imms ,(if (= sf 1) 63 31)))
                                '("LSR" :tab rd ", " rn ", #" immr))
                      ;; LSL: imms = 63-immr (i.e. imms+immr=63)
                      ;; Detected by immr+imms=63 and imms<63
                      ;; UXTB: immr=0 imms=7
                      (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)
                                          (immr 0) (imms 7))
                                '("UXTB" :tab rd ", " rn))
                      ;; UXTH: immr=0 imms=15
                      (:printer bitfield ((sf ,sf) (opc ,opc) (n ,sf)
                                          (immr 0) (imms 15))
                                '("UXTH" :tab rd ", " rn))))
                ,@(when (= opc #b01)   ; bfm
                    `(;; BFXIL: imms>=immr (extract and insert at bit 0)
                      ;; BFI: imms<immr (insert field)
                      ))
                (:emitter
                 (emit-bitfield-inst segment ,sf ,opc #b100110
                                     ,sf      ; N = sf for legal encodings
                                     immr imms
                                     (tn-offset rn) (tn-offset rd))))))
  (def sbfm  1 #b00)
  (def bfm   1 #b01)
  (def ubfm  1 #b10)

  (def sbfmw 0 #b00)
  (def bfmw  0 #b01)
  (def ubfmw 0 #b10))


;;; Unified shift instructions via define-instruction-macro.
;;; Dispatch on the type of SHIFT:
;;;   (integer 0 63) -> immediate encoding (bitfield alias)
;;;   tn             -> register encoding  (dp2src *V instruction)

(define-instruction-macro asr (rd rn shift)
  (if (integerp shift)
      `(inst sbfm ,rd ,rn ,shift 63)
      `(inst asrv ,rd ,rn ,shift)))

(define-instruction-macro lsr (rd rn shift)
  (if (integerp shift)
      `(inst ubfm ,rd ,rn ,shift 63)
      `(inst lsrv ,rd ,rn ,shift)))

(define-instruction-macro lsl (rd rn shift)
  (if (integerp shift)
      (let ((immr (mod (- shift) 64))
            (imms (- 63 shift)))
        `(inst ubfm ,rd ,rn ,immr ,imms))
      `(inst lslv ,rd ,rn ,shift)))

(define-instruction-macro ror (rd rn shift)
  (if (integerp shift)
      `(inst extr ,rd ,rn ,rn ,shift)
      `(inst rorv ,rd ,rn ,shift)))

(define-instruction-macro rorw (rd rn shift)
  (if (integerp shift)
      `(inst extrw ,rd ,rn ,rn ,shift)
      `(inst rorvw ,rd ,rn ,shift)))

;;; Sign/zero-extend aliases — now instruction macros over sbfm/ubfm.
(define-instruction-macro sxtb (rd rn)
  `(inst sbfm  ,rd ,rn 0  7))
(define-instruction-macro sxth (rd rn)
  `(inst sbfm  ,rd ,rn 0 15))
(define-instruction-macro sxtw (rd rn)
  `(inst sbfm  ,rd ,rn 0 31))
(define-instruction-macro uxtb (rd rn)
  `(inst ubfmw ,rd ,rn 0  7))
(define-instruction-macro uxth (rd rn)
  `(inst ubfmw ,rd ,rn 0 15))

;;; Bitfield extract/insert aliases.
;;; SBFX: signed bitfield extract — SBFM Rd, Rn, lsb, lsb+width-1
(define-instruction-macro sbfx (rd rn lsb width)
  `(inst sbfm ,rd ,rn ,lsb (+ ,lsb ,width -1)))

;;; UBFX: unsigned bitfield extract — UBFM Rd, Rn, lsb, lsb+width-1
(define-instruction-macro ubfx (rd rn lsb width)
  `(inst ubfm ,rd ,rn ,lsb (+ ,lsb ,width -1)))

;;; BFI: bitfield insert — BFM Rd, Rn, 64-lsb, width-1
(define-instruction-macro bfi (rd rn lsb width)
  `(inst bfm ,rd ,rn (mod (- ,lsb) 64) (1- ,width)))

;;; BFXIL: bitfield extract and insert low — BFM Rd, Rn, lsb, lsb+width-1
(define-instruction-macro bfxil (rd rn lsb width)
  `(inst bfm ,rd ,rn ,lsb (+ ,lsb ,width -1)))



;;; --- Extract ---
;;;
;;; sf | 0 0 | 1 0 0 1 1 1 | N | 0 | Rm | imms | Rn | Rd

(disassem:define-instruction-format (extract 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ", #" imms))
  (sf   :field (byte 1 31))
  (f1   :field (byte 8 23) :value #b10011100)
  (rm   :field (byte 5 16) :type 'sf-reg)
  (imms :field (byte 6 10) :type 'unsigned-imm)
  (rn   :field (byte 5 5)  :type 'sf-reg)
  (rd   :field (byte 5 0)  :type 'sf-reg))

(define-emitter emit-extract-inst 32
  (byte 1 31)   ; sf
  (byte 8 23)   ; fixed #b10011100
  (byte 5 16)   ; rm
  (byte 6 10)   ; imms
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(define-instruction extr (segment rd rn rm lsb)
  (:declare (type tn rd rn rm) (type (unsigned-byte 6) lsb))
  (:printer extract ((sf 1)))
  ;; ROR alias: when rn=rm, print as ROR Rd, Rn, #lsb
  (:printer extract ((sf 1) (rn rm))
            '("ROR" :tab rd ", " rn ", #" imms))
  (:emitter
   (emit-extract-inst segment 1 #b10011100
                      (tn-offset rm) lsb (tn-offset rn) (tn-offset rd))))

(define-instruction extrw (segment rd rn rm lsb)
  (:declare (type tn rd rn rm) (type (unsigned-byte 5) lsb))
  (:printer extract ((sf 0)))
  ;; ROR alias: when rn=rm
  (:printer extract ((sf 0) (rn rm))
            '("ROR" :tab rd ", " rn ", #" imms))
  (:emitter
   (emit-extract-inst segment 0 #b10011100
                      (tn-offset rm) lsb (tn-offset rn) (tn-offset rd))))



;;;; ======================================================================
;;;; BRANCHES AND EXCEPTIONS  (non-privileged)
;;;; ======================================================================

;;; --- Unconditional branch (immediate) ---
;;;
;;; op | 0 0 1 0 1 | imm26

(disassem:define-instruction-format (uncond-branch-imm 32
                                     :default-printer '(:name :tab target))
  (op    :field (byte 1 31))
  (f1    :field (byte 5 26) :value #b00101)
  (imm26 :field (byte 26 0) :type 'pc-rel-label))

(define-emitter emit-uncond-branch-imm-inst 32
  (byte 1 31)   ; op
  (byte 5 26)   ; fixed #b00101
  (byte 26 0))  ; imm26

(macrolet ((def (name op)
             `(define-instruction ,name (segment target)
                (:declare (type label target))
                (:printer uncond-branch-imm ((op ,op)))
                (:emitter
                 (emit-back-patch segment 4
                   #'(lambda (segment posn)
                       (let ((off (ash (- (label-position target) posn) -2)))
                         (emit-uncond-branch-imm-inst
                          segment ,op #b00101 (ldb (byte 26 0) off)))))))))
  (def b  0)
  (def bl 1))


;;; --- Unconditional branch (register) ---
;;;
;;; 1 1 0 1 0 1 1 | opc[3:0] | 1 1 1 1 1 | 0 0 0 0 0 0 | Rn | 0 0 0 0 0

(disassem:define-instruction-format (uncond-branch-reg 32
                                     :default-printer '(:name :tab rn))
  (f1  :field (byte 7 25) :value #b1101011)
  (opc :field (byte 4 21))
  (op2 :field (byte 5 16) :value #b11111)
  (op3 :field (byte 6 10) :value #b000000)
  (rn  :field (byte 5 5)  :type 'reg)
  (op4 :field (byte 5 0)  :value #b00000))

(define-emitter emit-uncond-branch-reg-inst 32
  (byte 7 25)   ; fixed #b1101011
  (byte 4 21)   ; opc
  (byte 5 16)   ; fixed #b11111
  (byte 6 10)   ; fixed #b000000
  (byte 5 5)    ; rn
  (byte 5 0))   ; fixed #b00000

(macrolet ((def (name opc)
             `(define-instruction ,name (segment rn)
                (:declare (type tn rn))
                (:printer uncond-branch-reg ((opc ,opc)))
                (:emitter
                 (emit-uncond-branch-reg-inst
                  segment #b1101011 ,opc #b11111 #b000000
                  (tn-offset rn) #b00000)))))
  (def br  #b0000)
  (def blr #b0001))

(define-instruction ret (segment &optional (rn nil rnp))
  (:printer uncond-branch-reg ((opc #b0010)) '("ret" :tab rn))
  (:emitter
   (emit-uncond-branch-reg-inst
    segment #b1101011 #b0010 #b11111 #b000000
    (if rnp (tn-offset rn) lr) #b00000)))


;;; --- Compare and branch (immediate) ---
;;;
;;; sf | 0 1 1 0 1 0 | op | imm19 | Rt

(disassem:define-instruction-format (compare-branch-imm 32
                                     :default-printer '(:name :tab rt ", " target))
  (sf     :field (byte 1 31))
  (f1     :field (byte 6 25) :value #b011010)
  (op     :field (byte 1 24))
  (imm19  :field (byte 19 5))
  (rt     :field (byte 5 0)  :type 'sf-reg)
  (target :field (byte 19 5)
          :use-label #'(lambda (value dstate)
                         (+ (disassem:dstate-cur-addr dstate)
                            (ash (if (logbitp 18 value)
                                     (- value (expt 2 19))
                                     value)
                                 2)))))

(define-emitter emit-compare-branch-imm-inst 32
  (byte 1 31)   ; sf
  (byte 6 25)   ; fixed #b011010
  (byte 1 24)   ; op
  (byte 19 5)   ; imm19
  (byte 5 0))   ; rt

(macrolet ((def (name sf op &optional alias)
             `(define-instruction ,name (segment rt target)
                (:declare (type tn rt) (type label target))
                (:printer compare-branch-imm ((sf ,sf) (op ,op))
                          ,@(when alias `(() :print-name ,alias)))
                (:emitter
                 (emit-back-patch segment 4
                   #'(lambda (segment posn)
                       (let ((off (ash (- (label-position target) posn) -2)))
                         (emit-compare-branch-imm-inst
                          segment ,sf #b011010 ,op
                          (ldb (byte 19 0) off) (tn-offset rt)))))))))
  (def cbz   1 0)
  (def cbnz  1 1)

  (def cbzw  0 0 cbz)
  (def cbnzw 0 1 cbnz))



;;; --- Test and branch (immediate) ---
;;;
;;; b5 | 0 1 1 0 1 1 | op | b40[4:0] | imm14 | Rt

(disassem:define-instruction-format (test-branch-imm 32
                                     :default-printer
                                     '(:name :tab rt ", #" bit-num ", " target))
  (b5    :field (byte 1 31))
  (f1    :field (byte 6 25) :value #b011011)
  (op    :field (byte 1 24))
  (b40   :field (byte 5 19))
  (imm14 :field (byte 14 5))
  (rt    :field (byte 5 0)
         :printer #'(lambda (value stream dstate)
                      (let ((b5 (ldb (byte 1 31)
                                     (disassem:sap-ref-int
                                      (disassem:dstate-segment-sap dstate)
                                      (disassem:dstate-cur-offs dstate)
                                      4 :little-endian))))
                        (if (zerop b5)
                            (format stream "w~D" value)
                            (format stream "x~D" value)))))
  (bit-num :fields (list (byte 1 31) (byte 5 19))
           :printer #'(lambda (fields stream dstate)
                        (declare (ignore dstate))
                        (format stream "~D"
                                (logior (ash (first fields) 5)
                                        (second fields)))))
  (target :field (byte 14 5)
          :use-label #'(lambda (value dstate)
                         (+ (disassem:dstate-cur-addr dstate)
                            (ash (if (logbitp 13 value)
                                     (- value (expt 2 14))
                                     value)
                                 2)))))

(define-emitter emit-test-branch-imm-inst 32
  (byte 1 31)   ; b5
  (byte 6 25)   ; fixed #b011011
  (byte 1 24)   ; op
  (byte 5 19)   ; b40
  (byte 14 5)   ; imm14
  (byte 5 0))   ; rt

(macrolet ((def (name op)
             `(define-instruction ,name (segment rt bit-pos target)
                (:declare (type tn rt)
                          (type (unsigned-byte 6) bit-pos)
                          (type label target))
                (:printer test-branch-imm ((op ,op)))
                (:emitter
                 (emit-back-patch segment 4
                   #'(lambda (segment posn)
                       (let ((off (ash (- (label-position target) posn) -2)))
                         (emit-test-branch-imm-inst
                          segment
                          (ldb (byte 1 5) bit-pos)    ; b5
                          #b011011 ,op
                          (ldb (byte 5 0) bit-pos)    ; b40
                          (ldb (byte 14 0) off)
                          (tn-offset rt)))))))))
  (def tbz  0)
  (def tbnz 1))


;;; --- Conditional branch (immediate) ---
;;;
;;; 0 1 0 1 0 1 0 0 | imm19 | 0 | cond[3:0]

(disassem:define-instruction-format (cond-branch-imm 32
                                     :default-printer '(:name :tab target))
  (f1     :field (byte 8 24) :value #b01010100)
  (imm19  :field (byte 19 5))
  (o0     :field (byte 1 4)  :value 0)
  (cond   :field (byte 4 0)  :type 'condition)
  (target :field (byte 19 5)
          :use-label #'(lambda (value dstate)
                         (+ (disassem:dstate-cur-addr dstate)
                            (ash (if (logbitp 18 value)
                                     (- value (expt 2 19))
                                     value)
                                 2)))))

(define-emitter emit-cond-branch-imm-inst 32
  (byte 8 24)   ; fixed #b01010100
  (byte 19 5)   ; imm19
  (byte 1 4)    ; fixed 0
  (byte 4 0))   ; cond

;;; Base conditional branch instruction: takes a condition keyword
;;; (e.g. :eq, :ne) and a label.  All the named b.eq, b.ne, etc. are
;;; instruction macros over this.

(define-instruction b.cond (segment cond target)
  (:declare (type (member :eq :ne :cs :cc :mi :pl :vs :vc
                          :hi :ls :ge :lt :gt :le :al :nv) cond)
            (type label target))
  (:printer cond-branch-imm ())
  (:emitter
   (emit-back-patch segment 4
     #'(lambda (segment posn)
         (let ((off (ash (- (label-position target) posn) -2)))
           (emit-cond-branch-imm-inst
            segment #b01010100
            (ldb (byte 19 0) off)
            0
            (condition-encoding cond)))))))

(macrolet ((def (name cond-key)
             `(define-instruction-macro ,name (target)
                `(inst b.cond ,,cond-key ,target))))
  (def b.eq :eq)
  (def b.ne :ne)
  (def b.cs :cs)
  (def b.cc :cc)
  (def b.mi :mi)
  (def b.pl :pl)
  (def b.vs :vs)
  (def b.vc :vc)
  (def b.hi :hi)
  (def b.ls :ls)
  (def b.ge :ge)
  (def b.lt :lt)
  (def b.gt :gt)
  (def b.le :le)
  (def b.al :al)
  ;; Synonyms
  (def b.hs :cs)
  (def b.lo :cc))


;;; --- Barriers ---
;;;
;;; 1 1 0 1 0 1 0 1 0 0 0 0 | CRm | op2 | 1 1 1 1 1

;;; Helper to convert a barrier option keyword to its 4-bit CRm encoding.
(defun barrier-option (option)
  "Convert a barrier option keyword to its 4-bit encoding.
Accepted keywords: :sy :st :ld :ish :ishst :ishld :nsh :nshst :nshld
                   :osh :oshst :oshld"
  (ecase option
    (:sy    #b1111)
    (:st    #b1110)
    (:ld    #b1101)
    (:ish   #b1011)
    (:ishst #b1010)
    (:ishld #b1001)
    (:nsh   #b0111)
    (:nshst #b0110)
    (:nshld #b0101)
    (:osh   #b0011)
    (:oshst #b0010)
    (:oshld #b0001)))

(disassem:define-instruction-format (barrier 32
                                     :default-printer '(:name :tab option))
  (f1     :field (byte 12 20) :value #b110101010000)
  (crm    :field (byte 4 8))
  (op2    :field (byte 3 5))
  (rt     :field (byte 5 0)   :value #b11111)
  (option :field (byte 4 8)
          :printer #'(lambda (value stream dstate)
                       (declare (ignore dstate))
                       (princ (case value
                                (#b1111 'sy)    (#b1110 'st)
                                (#b1101 'ld)    (#b1011 'ish)
                                (#b1010 'ishst) (#b1001 'ishld)
                                (#b0111 'nsh)   (#b0110 'nshst)
                                (#b0101 'nshld) (#b0011 'osh)
                                (#b0010 'oshst) (#b0001 'oshld)
                                (t '???))
                               stream))))

(define-emitter emit-barrier-inst 32
  (byte 12 20)  ; fixed #b110101010000
  (byte 4 8)    ; crm (option)
  (byte 3 5)    ; op2
  (byte 5 0))   ; fixed #b11111

(macrolet ((def (name op2 default-option)
             `(define-instruction ,name (segment &optional (option ,default-option))
                (:declare (type (or keyword (unsigned-byte 4)) option))
                (:printer barrier ((op2 ,op2)))
                (:emitter
                 (let ((crm (if (keywordp option)
                                (barrier-option option)
                                option)))
                   (emit-barrier-inst segment #b110101010000 crm ,op2 #b11111))))))
  (def dsb #b100 :sy)
  (def dmb #b101 :sy)
  (def isb #b110 :sy))


;;; --- Exception-generating (BRK / HLT) ---
;;;
;;; 1 1 0 1 0 1 0 0 | opc[2:0] | imm16 | 0 0 0 | LL[1:0]

(disassem:define-instruction-format (exception 32
                                     :default-printer '(:name :tab imm16))
  (f1    :field (byte 8 24) :value #b11010100)
  (opc   :field (byte 3 21))
  (imm16 :field (byte 16 5)
         :printer #'(lambda (value stream dstate)
                      (declare (ignore dstate))
                      (format stream "#0x~X" value)))
  (op2   :field (byte 3 2)  :value 0)
  (ll    :field (byte 2 0)))

(define-emitter emit-exception-inst 32
  (byte 8 24)   ; fixed #b11010100
  (byte 3 21)   ; opc
  (byte 16 5)   ; imm16
  (byte 3 2)    ; fixed 0
  (byte 2 0))   ; ll

(macrolet ((def (name opc)
             `(define-instruction ,name (segment imm)
                (:declare (type (unsigned-byte 16) imm))
                (:printer exception ((opc ,opc) (ll #b00)))
                (:emitter
                 (emit-exception-inst segment #b11010100 ,opc imm 0 #b00)))))
  (def brk #b001)
  (def hlt #b010))

;;; --- UDF (permanently undefined) ---
;;;
;;; 0000 0000 0000 0000 | imm16
;;; Always raises an Undefined Instruction exception.
;;; The 16-bit immediate is architecturally ignored but conventionally
;;; used to encode a reason code visible to a trap handler.

(disassem:define-instruction-format (udf 32
                                     :default-printer '(:name :tab imm16))
  (f1    :field (byte 16 16) :value 0)
  (imm16 :field (byte 16 0)
         :printer #'(lambda (value stream dstate)
                      (declare (ignore dstate))
                      (format stream "#0x~X" value))))

(define-emitter emit-udf-inst 32
  (byte 16 16)   ; fixed 0
  (byte 16 0))   ; imm16

(define-instruction udf (segment imm)
  (:declare (type (unsigned-byte 16) imm))
  (:printer udf ())
  (:emitter
   (emit-udf-inst segment 0 imm)))

;;; NOP
(define-instruction nop (segment)
  (:printer ((:name) (:value #xD503201F)))
  (:emitter (emit-word segment #xD503201F)))


;;;; ======================================================================
;;;; DATA PROCESSING -- REGISTER
;;;; ======================================================================

;;; Arg-types for registers that print as W or X depending on the sf bit (bit 31).

(disassem:define-argument-type sf-reg
  :printer #'(lambda (value stream dstate)
               (let ((sf (ldb (byte 1 31)
                              (disassem:sap-ref-int
                               (disassem:dstate-segment-sap dstate)
                               (disassem:dstate-cur-offs dstate)
                               4 :little-endian))))
                 (if (zerop sf)
                     (if (= value 31) (princ "wzr" stream)
                         (format stream "w~D" value))
                     (if (= value 31) (princ "xzr" stream)
                         (format stream "x~D" value))))))

;;; Always prints as a W-register regardless of sf — used for the 32-bit
;;; source operands of long-multiply instructions (SMADDL, UMADDL etc.)
(disassem:define-argument-type w-reg
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (if (= value 31)
                   (princ "wzr" stream)
                   (format stream "w~D" value))))

(disassem:define-argument-type sf-sp-reg
  :printer #'(lambda (value stream dstate)
               (let ((sf (ldb (byte 1 31)
                              (disassem:sap-ref-int
                               (disassem:dstate-segment-sap dstate)
                               (disassem:dstate-cur-offs dstate)
                               4 :little-endian))))
                 (if (zerop sf)
                     (if (= value 31) (princ "wsp" stream)
                         (format stream "w~D" value))
                     (if (= value 31) (princ "sp" stream)
                         (format stream "x~D" value))))))


;;; --- Logical (shifted register) ---
;;;
;;; sf | opc[1:0] | 0 1 0 1 0 | shift[1:0] | N | Rm | imm6 | Rn | Rd

;;; A shifted-reg-mode bundles a register with an optional shift type and
;;; amount, used by add/sub shifted-register and logical shifted-register
;;; instructions.
;;;
;;; Slots:
;;;   RM     – the register TN.
;;;   SHIFT  – shift type keyword (:lsl :lsr :asr :ror), default :lsl.
;;;            Note: add/sub only permits :lsl :lsr :asr (not :ror).
;;;   AMOUNT – 6-bit shift amount, default 0.

(defstruct (shifted-reg-mode
             (:constructor make-shifted-reg-mode
                           (rm &key (shift :lsl) (amount 0))))
  (rm     nil  :type tn)
  (shift  :lsl :type (member :lsl :lsr :asr :ror))
  (amount 0    :type (unsigned-byte 6)))

(disassem:define-instruction-format (logical-reg 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm shift-amt))
  (sf        :field (byte 1 31))
  (opc       :field (byte 2 29))
  (f1        :field (byte 5 24) :value #b01010)
  (shift     :field (byte 2 22) :type 'shift-type)
  (n         :field (byte 1 21))
  (rm        :field (byte 5 16) :type 'sf-reg)
  (imm6      :field (byte 6 10) :type 'unsigned-imm)
  (rn        :field (byte 5 5)  :type 'sf-reg)
  (rd        :field (byte 5 0)  :type 'sf-reg)
  (shift-amt :fields (list (byte 2 22) (byte 6 10))
             :printer #'(lambda (fields stream dstate)
                          (declare (ignore dstate))
                          (let ((sh  (first fields))
                                (amt (second fields)))
                            (unless (and (zerop sh) (zerop amt))
                              (format stream ", ~A #~D"
                                      (svref arm64-shifts sh) amt))))))

(define-emitter emit-logical-reg-inst 32
  (byte 1 31)   ; sf
  (byte 2 29)   ; opc
  (byte 5 24)   ; fixed #b01010
  (byte 2 22)   ; shift
  (byte 1 21)   ; n
  (byte 5 16)   ; rm
  (byte 6 10)   ; imm6 (amount)
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

;;; Unified logical instructions: dispatch on type of RM/IMM.
;;;   shifted-reg-mode  -> shifted-register encoding
;;;   logical-imm-mask  -> immediate encoding
;;; BIC/ORN/EON/BICS have no immediate form; they take shifted-reg-mode only.

(macrolet ((def (name sf opc n)
             ;; N=0 variants (AND/ORR/EOR/ANDS): accept both encodings.
             ;; N=1 variants (BIC/ORN/EON/BICS): register-only.
             (if (zerop n)
                 `(define-instruction ,name (segment rd rn rm/imm)
                    (:declare (type tn rd rn)
                              (type (or shifted-reg-mode logical-imm-mask) rm/imm))
                    (:printer logical-reg ((sf ,sf) (opc ,opc) (n 0)))
                    (:printer logical-imm ((sf ,sf) (opc ,opc)))
                    ,@(when (= opc #b11)
                        ;; TST alias printers: rd=xzr, suppress rd from output
                        `((:printer logical-reg ((sf ,sf) (opc #b11) (n 0) (rd #b11111))
                                    '("TST" :tab rn ", " rm shift-amt))
                          (:printer logical-imm ((sf ,sf) (opc #b11) (rd #b11111))
                                    '("TST" :tab rn ", " immrs))))
                    ,@(when (= opc #b01)
                        ;; MOV alias printer: ORR Rd, XZR, Rm with no shift
                        `((:printer logical-reg ((sf ,sf) (opc #b01) (n 0) (rn #b11111) (imm6 0))
                                    '("MOV" :tab rd ", " rm))))
                    (:emitter
                     (etypecase rm/imm
                       (shifted-reg-mode
                        (emit-logical-reg-inst segment ,sf ,opc #b01010
                                               (position (shifted-reg-mode-shift  rm/imm)
                                                         arm64-shifts)
                                               0
                                               (tn-offset (shifted-reg-mode-rm    rm/imm))
                                               (shifted-reg-mode-amount rm/imm)
                                               (tn-offset rn) (tn-offset rd)))
                       (logical-imm-mask
                        (emit-logical-imm-inst segment ,sf ,opc #b100100
                                               (logical-imm-mask-n    rm/imm)
                                               (logical-imm-mask-immr rm/imm)
                                               (logical-imm-mask-imms rm/imm)
                                               (tn-offset rn) (tn-offset rd))))))
                 `(define-instruction ,name (segment rd rn mode)
                    (:declare (type tn rd rn)
                              (type shifted-reg-mode mode))
                    (:printer logical-reg ((sf ,sf) (opc ,opc) (n 1)))
                    (:emitter
                     (emit-logical-reg-inst segment ,sf ,opc #b01010
                                            (position (shifted-reg-mode-shift  mode)
                                                      arm64-shifts)
                                            1
                                            (tn-offset (shifted-reg-mode-rm    mode))
                                            (shifted-reg-mode-amount mode)
                                            (tn-offset rn) (tn-offset rd)))))))
  ;; 64-bit
  (def and   1 #b00 0)
  (def bic   1 #b00 1)

  (def orr   1 #b01 0)
  (def orn   1 #b01 1)

  (def eor   1 #b10 0)
  (def eon   1 #b10 1)

  (def ands  1 #b11 0)
  (def bics  1 #b11 1)

  ;; 32-bit W-variants
  (def andw  0 #b00 0)
  (def bicw  0 #b00 1)

  (def orrw  0 #b01 0)
  (def ornw  0 #b01 1)

  (def eorw  0 #b10 0)
  (def eonw  0 #b10 1)

  (def andsw 0 #b11 0)
  (def bicsw 0 #b11 1))


;;; MOV: alias for ORR Rd, XZR, Rm (shift=LSL #0).
(define-instruction-macro mov (rd rm)
  `(inst orr ,rd zero-tn (make-shifted-reg-mode ,rm)))

;;; MVN: ORN Rd, XZR, Rm
(define-instruction mvn (segment rd mode)
  (:declare (type tn rd)
            (type shifted-reg-mode mode))
  (:printer logical-reg ((opc #b01) (n 1) (rn xzr))
            '("MVN" :tab rd ", " rm shift-amt))
  (:emitter
   (emit-logical-reg-inst segment 1 #b01 #b01010
                          (position (shifted-reg-mode-shift  mode) arm64-shifts)
                          1
                          (tn-offset (shifted-reg-mode-rm    mode))
                          (shifted-reg-mode-amount mode)
                          xzr (tn-offset rd))))

;;; TST: ANDS zero-tn, Rn, Rm/imm
;;; Accepts either a shifted-reg-mode or a logical-imm-mask.
(define-instruction-macro tst (rn rm/imm)
  `(inst ands zero-tn ,rn ,rm/imm))


;;; --- Add/subtract (shifted register) ---
;;;
;;; sf | op | S | 0 1 0 1 1 | shift[1:0] | 0 | Rm | imm6 | Rn | Rd

(disassem:define-instruction-format (add-sub-reg 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm shift-info))
  (sf         :field (byte 1 31))
  (op         :field (byte 1 30))
  (s          :field (byte 1 29))
  (f1         :field (byte 5 24) :value #b01011)
  (shift      :field (byte 2 22) :type 'shift-type)
  (f2         :field (byte 1 21) :value 0)
  (rm         :field (byte 5 16) :type 'sf-reg)
  (imm6       :field (byte 6 10))
  (rn         :field (byte 5 5)  :type 'sf-reg)
  (rd         :field (byte 5 0)  :type 'sf-reg)
  (shift-info :fields (list (byte 2 22) (byte 6 10))
              :printer #'(lambda (fields stream dstate)
                           (declare (ignore dstate))
                           (let ((sh (first fields)) (amt (second fields)))
                             (unless (and (zerop sh) (zerop amt))
                               (format stream ", ~A #~D"
                                       (svref arm64-shifts sh) amt))))))

(define-emitter emit-add-sub-reg-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 1 29)   ; s
  (byte 5 24)   ; fixed #b01011
  (byte 2 22)   ; shift
  (byte 1 21)   ; fixed 0
  (byte 5 16)   ; rm
  (byte 6 10)   ; imm6 (amount)
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd





;;; NEG/NEGS: aliases for SUB/SUBS Rd, XZR, Rm.
;;; The zero register TN is referenced via zero-tn (defined in the VM).
(define-instruction-macro neg (rd mode)
  `(inst sub ,rd zero-tn ,mode))

(define-instruction-macro negs (rd mode)
  `(inst subs ,rd zero-tn ,mode))


;;; --- Add/subtract (extended register) ---
;;;
;;; sf | op | S | 0 1 0 1 1 | 0 0 1 | Rm | option[2:0] | imm3 | Rn | Rd

;;; An extended-reg-mode value bundles the offset register and the
;;; extension/shift annotation used by add/sub extended-register
;;; instructions (ADD/SUB/ADDS/SUBS with option field).
;;;
;;; Slots:
;;;   RM     – the offset register TN.
;;;   EXTEND – extend type keyword, one of the arm64-extends keywords.
;;;            Defaults to :UXTX (plain 64-bit register extension).
;;;   SHIFT  – left-shift amount applied after extension, 0-4.
;;;            Defaults to 0.

(defstruct (extended-reg-mode
             (:constructor make-extended-reg-mode
                           (rm &key (extend :uxtx) (shift 0))))
  (rm     nil   :type tn)
  (extend :uxtx :type (member :uxtb :uxth :uxtw :uxtx
                               :sxtb :sxth :sxtw :sxtx))
  (shift  0     :type (unsigned-byte 3)))

(disassem:define-instruction-format (add-sub-ext 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ext-info))
  (sf      :field (byte 1 31))
  (op      :field (byte 1 30))
  (s       :field (byte 1 29))
  (f1      :field (byte 8 21) :value #b01011001)
  ;; rm: W-register for byte/halfword/word extends, X-register for UXTX/SXTX
  (rm      :field (byte 5 16)
           :printer #'(lambda (value stream dstate)
                        (let ((opt (ldb (byte 3 13)
                                        (disassem:sap-ref-int
                                         (disassem:dstate-segment-sap dstate)
                                         (disassem:dstate-cur-offs dstate)
                                         4 :little-endian))))
                          ;; UXTX=#b011, SXTX=#b111 use X-register; all others W
                          (if (member opt '(#b011 #b111))
                              (if (= value 31)
                                  (princ "xzr" stream)
                                  (format stream "x~D" value))
                              (if (= value 31)
                                  (princ "wzr" stream)
                                  (format stream "w~D" value))))))
  (option  :field (byte 3 13) :type 'extend-type)
  (imm3    :field (byte 3 10))
  (rn      :field (byte 5 5)  :type 'sf-sp-reg)
  (rd      :field (byte 5 0)  :type 'sf-sp-reg)
  ;; ext-info: suppress UXTX/LSL with no shift (plain register add)
  (ext-info :fields (list (byte 3 13) (byte 3 10))
            :printer #'(lambda (fields stream dstate)
                         (declare (ignore dstate))
                         (let ((ext (first fields)) (sh (second fields)))
                           (cond
                             ;; UXTX (#b011) with no shift: suppress entirely
                             ((and (= ext #b011) (zerop sh)))
                             ;; UXTX with shift: print as LSL
                             ((= ext #b011)
                              (format stream ", lsl #~D" sh))
                             ;; other extend with no shift: extend name only
                             ((zerop sh)
                              (format stream ", ~A" (svref arm64-extends ext)))
                             ;; extend with shift
                             (t
                              (format stream ", ~A #~D"
                                      (svref arm64-extends ext) sh)))))))

(define-emitter emit-add-sub-ext-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 1 29)   ; s
  (byte 8 21)   ; fixed #b01011001
  (byte 5 16)   ; rm
  (byte 3 13)   ; option (extend type)
  (byte 3 10)   ; imm3 (shift)
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

;;; Unified 64-bit add/sub: dispatch on type of RM/IMM.
;;;   tn                -> shifted-register encoding (LSL #0)
;;;   extended-reg-mode -> extended-register encoding
;;;   shifted-reg-mode  -> shifted-register encoding
;;;   shifted-imm12     -> immediate encoding
;;;   (unsigned-byte 12)-> immediate encoding (sh=0)

(macrolet ((def (name sf op s)
             (let ((alias (cond ((and (= op 1) (= s 1)) "CMP")  ; subs rd=xzr
                               ((and (= op 0) (= s 1)) "CMN")  ; adds rd=xzr
                               (t nil))))
               `(define-instruction ,name (segment rd rn rm/imm)
                  (:declare (type tn rd rn)
                            (type (or tn shifted-reg-mode shifted-imm12
                                      extended-reg-mode
                                      (unsigned-byte 12)) rm/imm))
                  (:printer add-sub-imm ((sf ,sf) (op ,op) (s ,s)))
                  (:printer add-sub-reg ((sf ,sf) (op ,op) (s ,s)))
                  (:printer add-sub-ext ((sf ,sf) (op ,op) (s ,s)))
                  ,@(when alias
                      `((:printer add-sub-imm ((sf ,sf) (op ,op) (s ,s) (rd #b11111))
                                  '(,alias :tab rn ", " imm-and-shift))
                        (:printer add-sub-reg ((sf ,sf) (op ,op) (s ,s) (rd #b11111))
                                  '(,alias :tab rn ", " rm shift-info))
                        (:printer add-sub-ext ((sf ,sf) (op ,op) (s ,s) (rd #b11111))
                                  '(,alias :tab rn ", " rm ext-info))))
                  (:emitter
                   (etypecase rm/imm
                     (tn
                      (emit-add-sub-reg-inst segment ,sf ,op ,s #b01011
                                             0 0
                                             (tn-offset rm/imm)
                                             0
                                             (tn-offset rn) (tn-offset rd)))
                     (extended-reg-mode
                      (emit-add-sub-ext-inst segment ,sf ,op ,s #b01011001
                                             (tn-offset (extended-reg-mode-rm     rm/imm))
                                             (position  (extended-reg-mode-extend rm/imm)
                                                        arm64-extends)
                                             (extended-reg-mode-shift rm/imm)
                                             (tn-offset rn) (tn-offset rd)))
                     (shifted-reg-mode
                      (emit-add-sub-reg-inst segment ,sf ,op ,s #b01011
                                             (position (shifted-reg-mode-shift  rm/imm)
                                                       arm64-shifts)
                                             0
                                             (tn-offset (shifted-reg-mode-rm    rm/imm))
                                             (shifted-reg-mode-amount rm/imm)
                                             (tn-offset rn) (tn-offset rd)))
                     (shifted-imm12
                      (emit-add-sub-imm-inst segment ,sf ,op ,s #b100010
                                             (if (shifted-imm12-lsl12 rm/imm) 1 0)
                                             (shifted-imm12-imm rm/imm)
                                             (tn-offset rn) (tn-offset rd)))
                     ((unsigned-byte 12)
                      (emit-add-sub-imm-inst segment ,sf ,op ,s #b100010
                                             0 rm/imm
                                             (tn-offset rn) (tn-offset rd)))))))
  (def add  1 0 0)
  (def adds 1 0 1)

  (def sub  1 1 0)
  (def subs 1 1 1)

  (def addw  0 0 0)
  (def addsw 0 0 1)

  (def subw  0 1 0)
  (def subsw 0 1 1))))



;;; --- Add/subtract with carry ---
;;;
;;; sf | op | S | 1 1 0 1 0 0 0 0 | Rm | 0 0 0 0 0 0 | Rn | Rd

(disassem:define-instruction-format (add-sub-carry 32
                                     :default-printer '(:name :tab rd ", " rn ", " rm))
  (sf :field (byte 1 31))
  (op :field (byte 1 30))
  (s  :field (byte 1 29))
  (f1 :field (byte 8 21) :value #b11010000)
  (rm :field (byte 5 16) :type 'sf-reg)
  (f2 :field (byte 6 10) :value 0)
  (rn :field (byte 5 5)  :type 'sf-reg)
  (rd :field (byte 5 0)  :type 'sf-reg))

(define-emitter emit-add-sub-carry-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 1 29)   ; s
  (byte 8 21)   ; fixed #b11010000
  (byte 5 16)   ; rm
  (byte 6 10)   ; fixed 0
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name sf op s alias)
             `(define-instruction ,name (segment rd rn rm)
                (:declare (type tn rd rn rm))
                (:printer add-sub-carry ((sf ,sf) (op ,op) (s ,s)))
                ,@(when alias
                    `((:printer add-sub-carry ((sf ,sf) (op ,op) (s ,s) (rn #b11111))
                                '(,alias :tab rd ", " rm))))
                (:emitter
                 (emit-add-sub-carry-inst segment ,sf ,op ,s #b11010000
                                          (tn-offset rm) 0
                                          (tn-offset rn) (tn-offset rd))))))
  (def adc  1 0 0 nil)
  (def adcs 1 0 1 nil)

  (def sbc  1 1 0 ngc)
  (def sbcs 1 1 1 ngcs)

  (def adcw  0 0 0 nil)
  (def adcsw 0 0 1 nil)

  (def sbcw  0 1 0 ngcw)
  (def sbcsw 0 1 1 ngcsw))


;;; NGC/NGCW: alias for SBC/SBCW Rd, XZR, Rm
(define-instruction-macro ngc (rd rm)
  `(inst sbc ,rd zero-tn ,rm))

(define-instruction-macro ngcw (rd rm)
  `(inst sbcw ,rd zero-tn ,rm))

(define-instruction-macro ngcs (rd rm)
  `(inst sbcs ,rd zero-tn ,rm))

(define-instruction-macro ngcsw (rd rm)
  `(inst sbcsw ,rd zero-tn ,rm))


;;; Helper to construct a 4-bit NZCV flag value from individual flag keywords.
;;; Any combination of :n :z :c :v may be supplied; unmentioned flags are 0.
;;;
;;;   bit 3 = N (negative)
;;;   bit 2 = Z (zero)
;;;   bit 1 = C (carry)
;;;   bit 0 = V (overflow)
;;;
;;; Example: (nzcv :z :c) => #b0110

(defun nzcv (&rest flags)
  (let ((result 0))
    (dolist (flag flags result)
      (setf result
            (logior result
                    (ecase flag
                      (:n #b1000)
                      (:z #b0100)
                      (:c #b0010)
                      (:v #b0001)))))))


;;; --- Conditional compare (register) ---
;;;
;;; sf | op | 1 | 1 1 0 1 0 0 1 0 | Rm | cond | 1 0 | Rn | 0 | nzcv

(disassem:define-argument-type nzcv-flags
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (format stream "#b~4,'0B" value)))

(disassem:define-instruction-format (cond-cmp-reg 32
                                     :default-printer
                                     '(:name :tab rn ", " rm ", " nzcv ", " cond))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (f1   :field (byte 9 21) :value #b111010010)
  (rm   :field (byte 5 16) :type 'reg)
  (cond :field (byte 4 12) :type 'condition)
  (f2   :field (byte 2 10) :value #b10)
  (rn   :field (byte 5 5)  :type 'reg)
  (f3   :field (byte 1 4)  :value 0)
  (nzcv :field (byte 4 0)  :type 'nzcv-flags))

(define-emitter emit-cond-cmp-reg-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 9 21)   ; fixed #b111010010
  (byte 5 16)   ; rm
  (byte 4 12)   ; cond
  (byte 2 10)   ; fixed #b10
  (byte 5 5)    ; rn
  (byte 1 4)    ; fixed 0
  (byte 4 0))   ; nzcv

;;; --- Conditional compare (immediate) ---
;;;
;;; sf | op | 1 | 1 1 0 1 0 0 1 0 | imm5 | cond | 1 1 | Rn | 0 | nzcv

(disassem:define-instruction-format (cond-cmp-imm 32
                                     :default-printer
                                     '(:name :tab rn ", #" imm5 ", " nzcv ", " cond))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (f1   :field (byte 9 21) :value #b111010010)
  (imm5 :field (byte 5 16) :type 'unsigned-imm)
  (cond :field (byte 4 12) :type 'condition)
  (f2   :field (byte 2 10) :value #b11)
  (rn   :field (byte 5 5)  :type 'reg)
  (f3   :field (byte 1 4)  :value 0)
  (nzcv :field (byte 4 0)  :type 'nzcv-flags))

(define-emitter emit-cond-cmp-imm-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 9 21)   ; fixed #b111010010
  (byte 5 16)   ; imm5
  (byte 4 12)   ; cond
  (byte 2 10)   ; fixed #b11
  (byte 5 5)    ; rn
  (byte 1 4)    ; fixed 0
  (byte 4 0))   ; nzcv

(macrolet ((def (name sf op)
             `(define-instruction ,name (segment rn rm/imm nzcv cond
                                         &key immediatep)
                (:declare (type tn rn)
                          (type (unsigned-byte 4) nzcv cond))
                (:printer cond-cmp-reg ((sf ,sf) (op ,op)))
                (:printer cond-cmp-imm ((sf ,sf) (op ,op)))
                (:emitter
                 (if (and (not immediatep) (typep rm/imm 'tn))
                     (emit-cond-cmp-reg-inst
                      segment ,sf ,op #b111010010
                      (tn-offset rm/imm) cond #b10 (tn-offset rn) 0 nzcv)
                     (emit-cond-cmp-imm-inst
                      segment ,sf ,op #b111010010
                      rm/imm cond #b11 (tn-offset rn) 0 nzcv))))))
  (def ccmp 1 1)
  (def ccmn 1 0))


;;; --- Conditional select ---
;;;
;;; sf | op | 0 | 1 1 0 1 0 1 0 0 | Rm | cond | op2[1:0] | Rn | Rd

(disassem:define-instruction-format (cond-select 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ", " cond))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (f1   :field (byte 9 21) :value #b110100100)
  (rm   :field (byte 5 16) :type 'sf-reg)
  (cond :field (byte 4 12) :type 'condition)
  (op2  :field (byte 2 10))
  (rn   :field (byte 5 5)  :type 'sf-reg)
  (rd   :field (byte 5 0)  :type 'sf-reg))

(define-emitter emit-cond-select-inst 32
  (byte 1 31)   ; sf
  (byte 1 30)   ; op
  (byte 9 21)   ; fixed #b110100100
  (byte 5 16)   ; rm
  (byte 4 12)   ; cond
  (byte 2 10)   ; op2
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name sf op op2)
             `(define-instruction ,name (segment rd rn rm cond)
                (:declare (type tn rd rn rm)
                          (type (or keyword (unsigned-byte 4)) cond))
                (:printer cond-select ((sf ,sf) (op ,op) (op2 ,op2)))
                ;; CINC alias: CSINC Rd, Rn, Rn, ~cond  (rn=rm, suppress rm)
                ,@(when (and (= op 0) (= op2 #b01))
                    `((:printer cond-select ((sf ,sf) (op 0) (op2 #b01) (rn rm))
                                '("CINC" :tab rd ", " rn ", " cond))
                      ;; CSET alias: CSINC Rd, XZR, XZR, ~cond
                      (:printer cond-select ((sf ,sf) (op 0) (op2 #b01)
                                              (rn #b11111) (rm #b11111))
                                '("CSET" :tab rd ", " cond))))
                ;; CINV alias: CSINV Rd, Rn, Rn, ~cond
                ,@(when (and (= op 1) (= op2 #b00))
                    `((:printer cond-select ((sf ,sf) (op 1) (op2 #b00) (rn rm))
                                '("CINV" :tab rd ", " rn ", " cond))
                      ;; CSETM alias: CSINV Rd, XZR, XZR, ~cond
                      (:printer cond-select ((sf ,sf) (op 1) (op2 #b00)
                                              (rn #b11111) (rm #b11111))
                                '("CSETM" :tab rd ", " cond))))
                ;; CNEG alias: CSNEG Rd, Rn, Rn, ~cond
                ,@(when (and (= op 1) (= op2 #b01))
                    '((:printer cond-select ((sf ,sf) (op 1) (op2 #b01) (rn rm))
                                '("CNEG" :tab rd ", " rn ", " cond))))
                (:emitter
                 (let ((c (if (keywordp cond) (condition-encoding cond) cond)))
                   (emit-cond-select-inst segment ,sf ,op #b110100100
                                          (tn-offset rm) c ,op2
                                          (tn-offset rn) (tn-offset rd)))))))
  (def csel  1 0 #b00)
  (def csinc 1 0 #b01)

  (def csinv 1 1 #b00)
  (def csneg 1 1 #b01)

  (def cselw  0 0 #b00)
  (def csincw 0 0 #b01)

  (def csinvw 0 1 #b00)
  (def csnegw 0 1 #b01))


(defun invert-condition (cond)
  (logxor (if (keywordp cond) (condition-encoding cond) cond) 1))

;;; CSET/CSETM: aliases for CSINC/CSINV Rd, XZR, XZR, ~cond
(define-instruction-macro cset (rd cond)
  `(inst csinc ,rd zero-tn zero-tn (invert-condition ,cond)))

(define-instruction-macro csetm (rd cond)
  `(inst csinv ,rd zero-tn zero-tn (invert-condition ,cond)))

(define-instruction-macro csetw (rd cond)
  `(inst csincw ,rd zero-tn zero-tn (invert-condition ,cond)))

(define-instruction-macro csetmw (rd cond)
  `(inst csinvw ,rd zero-tn zero-tn (invert-condition ,cond)))


;;; --- Data-processing 1-source ---
;;;
;;; sf | 1 | 0 | 1 1 0 1 0 1 1 0 | opcode2=00000 | opcode | Rn | Rd

(disassem:define-instruction-format (dp1src 32
                                     :default-printer '(:name :tab rd ", " rn))
  (sf      :field (byte 1 31))
  (f1      :field (byte 10 21) :value #b1011010110)
  (opcode2 :field (byte 5 16)  :value #b00000)
  (opcode  :field (byte 6 10))
  (rn      :field (byte 5 5)   :type 'reg)
  (rd      :field (byte 5 0)   :type 'reg))

(define-emitter emit-dp1src-inst 32
  (byte 1 31)    ; sf
  (byte 10 21)   ; fixed #b1011010110
  (byte 5 16)    ; opcode2 (fixed #b00000)
  (byte 6 10)    ; opcode
  (byte 5 5)     ; rn
  (byte 5 0))    ; rd

(macrolet ((def (name sf opcode)
             `(define-instruction ,name (segment rd rn)
                (:declare (type tn rd rn))
                (:printer dp1src ((sf ,sf) (opcode ,opcode)))
                (:emitter
                 (emit-dp1src-inst segment ,sf #b1011010110 #b00000 ,opcode
                                   (tn-offset rn) (tn-offset rd))))))
  (def rbit   1 #b000000)
  (def rev16  1 #b000001)

  (def rev32  1 #b000010)
  (def rev    1 #b000011)

  (def clz    1 #b000100)
  (def cls    1 #b000101)

  (def rbitw  0 #b000000)
  (def rev16w 0 #b000001)

  (def revw   0 #b000010)
  (def clzw   0 #b000100)
  (def clsw   0 #b000101))



;;; --- Data-processing 2-source ---
;;;
;;; sf | 0 | 0 | 1 1 0 1 0 1 1 0 | Rm | opcode | Rn | Rd

(disassem:define-instruction-format (dp2src 32
                                     :default-printer '(:name :tab rd ", " rn ", " rm))
  (sf     :field (byte 1 31))
  (f1     :field (byte 10 21) :value #b0011010110)
  (rm     :field (byte 5 16)  :type 'sf-reg)
  (opcode :field (byte 6 10))
  (rn     :field (byte 5 5)   :type 'sf-reg)
  (rd     :field (byte 5 0)   :type 'sf-reg))

(define-emitter emit-dp2src-inst 32
  (byte 1 31)    ; sf
  (byte 10 21)   ; fixed #b0011010110
  (byte 5 16)    ; rm
  (byte 6 10)    ; opcode
  (byte 5 5)     ; rn
  (byte 5 0))    ; rd

(macrolet ((def (name sf opcode &optional alias)
             `(define-instruction ,name (segment rd rn rm)
                (:declare (type tn rd rn rm))
                (:printer dp2src ((sf ,sf) (opcode ,opcode))
                          ,@(when alias `(() :print-name ,alias)))
                (:emitter
                 (emit-dp2src-inst segment ,sf #b0011010110
                                   (tn-offset rm) ,opcode
                                   (tn-offset rn) (tn-offset rd))))))
  (def udiv  1 #b000010)
  (def sdiv  1 #b000011)

  (def lslv  1 #b001000 lsl)
  (def lsrv  1 #b001001 lsr)

  (def asrv  1 #b001010 asr)
  (def rorv  1 #b001011 ror)

  (def udivw 0 #b000010)
  (def sdivw 0 #b000011)

  (def lslvw 0 #b001000 lsl)
  (def lsrvw 0 #b001001 lsr)

  (def asrvw 0 #b001010 asr)
  (def rorvw 0 #b001011 ror))



;;; --- Data-processing 3-source ---
;;;
;;; sf | op54[1:0] | 0 1 1 0 1 1 | op31[2:0] | Rm | o0 | Ra | Rn | Rd

(disassem:define-instruction-format (dp3src 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ", " ra))
  (sf   :field (byte 1 31))
  (op54 :field (byte 2 29))
  (f1   :field (byte 6 23) :value #b011011)
  (op31 :field (byte 3 21))
  (rm   :field (byte 5 16) :type 'sf-reg)
  (o0   :field (byte 1 15))
  (ra   :field (byte 5 10) :type 'sf-reg)
  (rn   :field (byte 5 5)  :type 'sf-reg)
  (rd   :field (byte 5 0)  :type 'sf-reg))

;;; Long-multiply format: rn/rm are always W-registers, rd/ra always X.
(disassem:define-instruction-format (dp3src-long 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ", " ra))
  (sf   :field (byte 1 31))
  (op54 :field (byte 2 29))
  (f1   :field (byte 6 23) :value #b011011)
  (op31 :field (byte 3 21))
  (rm   :field (byte 5 16) :type 'w-reg)
  (o0   :field (byte 1 15))
  (ra   :field (byte 5 10) :type 'reg)
  (rn   :field (byte 5 5)  :type 'w-reg)
  (rd   :field (byte 5 0)  :type 'reg))

;;; High-multiply format: no ra operand (field is XZR, suppressed).
(disassem:define-instruction-format (dp3src-high 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm))
  (sf   :field (byte 1 31))
  (op54 :field (byte 2 29))
  (f1   :field (byte 6 23) :value #b011011)
  (op31 :field (byte 3 21))
  (rm   :field (byte 5 16) :type 'reg)
  (o0   :field (byte 1 15))
  (ra   :field (byte 5 10) :value #b11111)
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-dp3src-inst 32
  (byte 1 31)   ; sf
  (byte 2 29)   ; op54
  (byte 6 23)   ; fixed #b011011
  (byte 3 21)   ; op31
  (byte 5 16)   ; rm
  (byte 1 15)   ; o0
  (byte 5 10)   ; ra
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name sf op54 op31 o0)
             `(define-instruction ,name (segment rd rn rm ra)
                (:declare (type tn rd rn rm ra))
                (:printer dp3src ((sf ,sf) (op54 ,op54) (op31 ,op31) (o0 ,o0)))
                ;; MUL alias: MADD with ra=XZR
                ,@(when (and (= op31 #b000) (= o0 0))
                    '((:printer dp3src ((sf ,sf) (op54 #b00) (op31 #b000) (o0 0)
                                        (ra #b11111))
                                '("MUL" :tab rd ", " rn ", " rm))))
                ;; MNEG alias: MSUB with ra=XZR
                ,@(when (and (= op31 #b000) (= o0 1))
                    '((:printer dp3src ((sf ,sf) (op54 #b00) (op31 #b000) (o0 1)
                                        (ra #b11111))
                                '("MNEG" :tab rd ", " rn ", " rm))))
                (:emitter
                 (emit-dp3src-inst segment ,sf ,op54 #b011011 ,op31
                                   (tn-offset rm) ,o0 (tn-offset ra)
                                   (tn-offset rn) (tn-offset rd))))))
  (def madd 1 #b00 #b000 0)
  (def msub 1 #b00 #b000 1))

(macrolet ((def (name op31 o0)
             `(define-instruction ,name (segment rd rn rm ra)
                (:declare (type tn rd rn rm ra))
                (:printer dp3src-long ((sf 1) (op54 #b00) (op31 ,op31) (o0 ,o0)))
                ;; SMULL alias: SMADDL with ra=XZR
                ,@(when (and (= op31 #b001) (= o0 0))
                    '((:printer dp3src-long ((sf 1) (op54 #b00) (op31 #b001) (o0 0)
                                             (ra #b11111))
                                '("SMULL" :tab rd ", " rn ", " rm))))
                ;; SMNEGL alias: SMSUBL with ra=XZR
                ,@(when (and (= op31 #b001) (= o0 1))
                    '((:printer dp3src-long ((sf 1) (op54 #b00) (op31 #b001) (o0 1)
                                             (ra #b11111))
                                '("SMNEGL" :tab rd ", " rn ", " rm))))
                ;; UMULL alias: UMADDL with ra=XZR
                ,@(when (and (= op31 #b101) (= o0 0))
                    '((:printer dp3src-long ((sf 1) (op54 #b00) (op31 #b101) (o0 0)
                                             (ra #b11111))
                                '("UMULL" :tab rd ", " rn ", " rm))))
                ;; UMNEGL alias: UMSUBL with ra=XZR
                ,@(when (and (= op31 #b101) (= o0 1))
                    '((:printer dp3src-long ((sf 1) (op54 #b00) (op31 #b101) (o0 1)
                                             (ra #b11111))
                                '("UMNEGL" :tab rd ", " rn ", " rm))))
                (:emitter
                 (emit-dp3src-inst segment 1 #b00 #b011011 ,op31
                                   (tn-offset rm) ,o0 (tn-offset ra)
                                   (tn-offset rn) (tn-offset rd))))))
  (def smaddl #b001 0)
  (def smsubl #b001 1)
  (def umaddl #b101 0)
  (def umsubl #b101 1))

(define-instruction smulh (segment rd rn rm)
  (:declare (type tn rd rn rm))
  (:printer dp3src-high ((sf 1) (op54 #b00) (op31 #b010) (o0 0)))
  (:emitter
   (emit-dp3src-inst segment 1 #b00 #b011011 #b010
                     (tn-offset rm) 0 xzr
                     (tn-offset rn) (tn-offset rd))))

(define-instruction umulh (segment rd rn rm)
  (:declare (type tn rd rn rm))
  (:printer dp3src-high ((sf 1) (op54 #b00) (op31 #b110) (o0 0)))
  (:emitter
   (emit-dp3src-inst segment 1 #b00 #b011011 #b110
                     (tn-offset rm) 0 xzr
                     (tn-offset rn) (tn-offset rd))))

;;; MUL/MNEG: MADD/MSUB with Ra = XZR.
(define-instruction-macro mul (rd rn rm)
  `(inst madd ,rd ,rn ,rm zero-tn))

(define-instruction-macro mneg (rd rn rm)
  `(inst msub ,rd ,rn ,rm zero-tn))

;;; SMULL/UMULL: SMADDL/UMADDL with Ra = XZR.
(define-instruction-macro smull (rd rn rm)
  `(inst smaddl ,rd ,rn ,rm zero-tn))

(define-instruction-macro umull (rd rn rm)
  `(inst umaddl ,rd ,rn ,rm zero-tn))

;;; SMNEGL/UMNEGL: SMSUBL/UMSUBL with Ra = XZR.
(define-instruction-macro smnegl (rd rn rm)
  `(inst smsubl ,rd ,rn ,rm zero-tn))

(define-instruction-macro umnegl (rd rn rm)
  `(inst umsubl ,rd ,rn ,rm zero-tn))


;;;; ======================================================================
;;;; LOADS AND STORES
;;;; ======================================================================

;;; An indexing-mode value bundles all information needed to describe one
;;; addressing operand to a load/store instruction.  Exactly one of the
;;; following addressing cases is active at a time:
;;;
;;;   Unsigned scaled offset  – [base, #imm12]          offset >= 0, aligned.
;;;   Unscaled signed offset  – [base, #imm9]           small signed byte offset.
;;;   Pre-index               – [base, #imm9]!          PRE-INDEX-P = T
;;;   Post-index              – [base], #imm9           POST-INDEX-P = T
;;;   Register offset         – [base, index, extend #shift]  INDEX is a TN.
;;;
;;; Slots:
;;;   BASE         – the base address register TN.
;;;   OFFSET       – byte offset (signed-byte 9) or scaled units
;;;                  (unsigned-byte 12); ignored when INDEX is supplied.
;;;   INDEX        – index register TN, or NIL for immediate addressing.
;;;   EXTEND       – extend/shift keyword for register-offset mode
;;;                  (one of the arm64-extends keywords, default :UXTX).
;;;   SHIFT        – shift amount for register-offset mode (0 or log2(size)).
;;;   PRE-INDEX-P  – T for pre-indexed ([base, #imm]!) addressing.
;;;   POST-INDEX-P – T for post-indexed ([base], #imm) addressing.

(defstruct (indexing-mode
             (:constructor make-indexing-mode
                           (base &key (offset 0) index
                                      (extend :uxtx) (shift 0)
                                      pre-index-p post-index-p)))
  (base         nil  :type tn)
  (offset       0    :type (or (signed-byte 9) (unsigned-byte 12)))
  (index        nil  :type (or null tn))
  (extend       :uxtx :type (member :uxtb :uxth :uxtw :uxtx
                                    :sxtb :sxth :sxtw :sxtx))
  (shift        0    :type (unsigned-byte 3))
  (pre-index-p  nil  :type boolean)
  (post-index-p nil  :type boolean))


;;; Arg-type for the Rt field in load/store instructions.
;;; For integer (V=0): prints xzr or xN.
;;; For FP/SIMD (V=1): prints sN (32-bit), dN (64-bit), or qN (128-bit)
;;; based on the size field (bits [31:30]) and opc field (bits [23:22]).

(disassem:define-argument-type load-store-rt
  :printer #'(lambda (value stream dstate)
               (let* ((word (disassem:sap-ref-int
                             (disassem:dstate-segment-sap dstate)
                             (disassem:dstate-cur-offs dstate)
                             4 :little-endian))
                      (v    (ldb (byte 1 26) word))
                      (size (ldb (byte 2 30) word))
                      (opc  (ldb (byte 2 22) word)))
                 (if (zerop v)
                     ;; Integer register
                     (if (= value 31)
                         (princ "xzr" stream)
                         (format stream "x~D" value))
                     ;; FP/SIMD register: size + opc determine width
                     (let ((prefix (cond ((= size #b10) "s")    ; 32-bit
                                         ((= size #b11) "d")    ; 64-bit
                                         ;; size=#b00, opc=#b10 or #b11 -> 128-bit
                                         ((and (= size #b00)
                                               (or (= opc #b10)
                                                   (= opc #b11))) "q")
                                         (t "v"))))             ; fallback
                       (format stream "~A~D" prefix value))))))


;;; --- Load/store register (unsigned offset) ---
;;;
;;; size | 1 1 1 | V | 0 1 | opc | imm12 | Rn | Rt

(disassem:define-instruction-format (load-store-uoff 32
                                     :default-printer
                                     '(:name :tab rt ", [" rn ", #" byte-offset "]"))
  (size        :field (byte 2 30))
  (f1          :field (byte 3 27) :value #b111)
  (v           :field (byte 1 26))
  (f2          :field (byte 2 24) :value #b01)
  (opc         :field (byte 2 22))
  (imm12       :field (byte 12 10))
  (rn          :field (byte 5 5)   :type 'sp-reg)
  (rt          :field (byte 5 0)   :type 'load-store-rt)
  ;; byte-offset = imm12 << size
  (byte-offset :fields (list (byte 2 30) (byte 12 10))
               :printer #'(lambda (fields stream dstate)
                            (declare (ignore dstate))
                            (let ((size  (first fields))
                                  (imm12 (second fields)))
                              (format stream "#~D" (ash imm12 size))))))

(define-emitter emit-load-store-uoff-inst 32
  (byte 2 30)   ; size
  (byte 3 27)   ; fixed #b111
  (byte 1 26)   ; v
  (byte 2 24)   ; fixed #b01
  (byte 2 22)   ; opc
  (byte 12 10)  ; imm12
  (byte 5 5)    ; rn
  (byte 5 0))   ; rt

;;; --- Load/store register (unscaled immediate) ---
;;;
;;; size | 1 1 1 | V | 0 0 | opc | 0 | imm9 | 0 0 | Rn | Rt

(disassem:define-instruction-format (load-store-unscaled 32
                                     :default-printer
                                     '(:name :tab rt ", [" rn ", #" imm9 "]"))
  (size :field (byte 2 30))
  (f1   :field (byte 3 27) :value #b111)
  (v    :field (byte 1 26))
  (f2   :field (byte 2 24) :value #b00)
  (opc  :field (byte 2 22))
  (f3   :field (byte 1 21) :value 0)
  (imm9 :field (byte 9 12) :type 'signed-imm)
  (f4   :field (byte 2 10) :value #b00)
  (rn   :field (byte 5 5)  :type 'sp-reg)
  (rt   :field (byte 5 0)  :type 'load-store-rt))

(define-emitter emit-load-store-unscaled-inst 32
  (byte 2 30)   ; size
  (byte 3 27)   ; fixed #b111
  (byte 1 26)   ; v
  (byte 2 24)   ; fixed #b00
  (byte 2 22)   ; opc
  (byte 1 21)   ; fixed 0
  (byte 9 12)   ; imm9
  (byte 2 10)   ; fixed #b00
  (byte 5 5)    ; rn
  (byte 5 0))   ; rt

;;; --- Load/store register (pre/post-index) ---
;;;
;;; size | 1 1 1 | V | 0 0 | opc | 0 | imm9 | idx[1:0] | Rn | Rt
;;;   idx=#b01 post-index: [Rn], #imm
;;;   idx=#b11 pre-index:  [Rn, #imm]!

(disassem:define-instruction-format (load-store-idx 32
                                     :default-printer
                                     '(:name :tab rt ", " addr-and-imm))
  (size        :field (byte 2 30))
  (f1          :field (byte 3 27) :value #b111)
  (v           :field (byte 1 26))
  (f2          :field (byte 2 24) :value #b00)
  (opc         :field (byte 2 22))
  (f3          :field (byte 1 21) :value 0)
  (imm9        :field (byte 9 12) :type 'signed-imm)
  (idx         :field (byte 2 10))
  (rn          :field (byte 5 5)  :type 'sp-reg)
  (rt          :field (byte 5 0)  :type 'load-store-rt)
  ;; addr-and-imm prints the full address syntax depending on idx:
  ;;   pre-index  (idx=#b11): [Rn, #imm]!
  ;;   post-index (idx=#b01): [Rn], #imm
  (addr-and-imm :fields (list (byte 2 10) (byte 5 5) (byte 9 12))
                :printer #'(lambda (fields stream dstate)
                             (declare (ignore dstate))
                             (let* ((idx  (first fields))
                                    (rn   (second fields))
                                    (imm9 (third fields))
                                    (imm  (if (logbitp 8 imm9)
                                              (- imm9 512)
                                              imm9))
                                    (rn-str (if (= rn 31) "sp"
                                                (format nil "x~D" rn))))
                               (if (= idx #b11)
                                   ;; pre-index
                                   (format stream "[~A, #~D]!" rn-str imm)
                                   ;; post-index
                                   (format stream "[~A], #~D" rn-str imm))))))

(define-emitter emit-load-store-idx-inst 32
  (byte 2 30)   ; size
  (byte 3 27)   ; fixed #b111
  (byte 1 26)   ; v
  (byte 2 24)   ; fixed #b00
  (byte 2 22)   ; opc
  (byte 1 21)   ; fixed 0
  (byte 9 12)   ; imm9
  (byte 2 10)   ; idx  (01=post, 11=pre)
  (byte 5 5)    ; rn
  (byte 5 0))   ; rt

;;; --- Load/store register (register offset) ---
;;;
;;; size | 1 1 1 | V | 0 0 | opc | 1 | Rm | option | S | 1 0 | Rn | Rt

(disassem:define-instruction-format (load-store-reg-off 32
                                     :default-printer
                                     '(:name :tab rt ", [" rn ", " rm ext-info "]"))
  (size     :field (byte 2 30))
  (f1       :field (byte 3 27) :value #b111)
  (v        :field (byte 1 26))
  (f2       :field (byte 2 24) :value #b00)
  (opc      :field (byte 2 22))
  (f3       :field (byte 1 21) :value 1)
  (rm       :field (byte 5 16) :type 'reg)
  (option   :field (byte 3 13))
  (s        :field (byte 1 12))
  (f4       :field (byte 2 10) :value #b10)
  (rn       :field (byte 5 5)  :type 'sp-reg)
  (rt       :field (byte 5 0)  :type 'load-store-rt)
  ;; ext-info fields: size, option, s
  ;; Prints nothing for default UXTX/LSL with s=0.
  ;; For shift: amount = size (log2 of access size in bytes).
  (ext-info :fields (list (byte 2 30) (byte 3 13) (byte 1 12))
            :printer #'(lambda (fields stream dstate)
                         (declare (ignore dstate))
                         (let ((size   (first fields))
                               (opt    (second fields))
                               (s      (third fields)))
                           (cond
                             ;; UXTX/LSL with no shift: omit entirely
                             ((and (= opt #b011) (zerop s)))
                             ;; UXTX/LSL with shift: just show lsl #amount
                             ((= opt #b011)
                              (format stream ", lsl #~D" size))
                             ;; Any other extend with no shift: show extend only
                             ((zerop s)
                              (format stream ", ~A" (svref arm64-extends opt)))
                             ;; Extend with shift
                             (t
                              (format stream ", ~A #~D"
                                      (svref arm64-extends opt)
                                      size)))))))

(defun assert-integer-reg (rt inst-name)
  "Signal an error if RT is not an integer register SC."
  (unless (sc-is rt descriptor-reg any-reg unsigned-reg signed-reg)
    (error "~S requires an integer register, not ~S (SC ~S)."
           inst-name rt (sc-name (tn-sc rt)))))

(defun check-scaled-offset (offset size inst-name)
  "Signal an error if OFFSET is not a valid scaled immediate for load/store.
SIZE is the log2 of the access size in bytes (0=byte,1=half,2=word,3=dword)."
  (let ((scale (ash 1 size)))
    (unless (and (>= offset 0)
                 (zerop (mod offset scale))
                 (<= (/ offset scale) #xFFF))
      (error "~S: offset ~D is not a valid scaled immediate ~
              (must be 0..~D, aligned to ~D bytes)."
             inst-name offset (* #xFFF scale) scale))))

(defun check-index-offset (offset inst-name)
  "Signal an error if OFFSET does not fit in a signed 9-bit field [-256,255]
for pre/post-index addressing."
  (unless (typep offset '(signed-byte 9))
    (error "~S: pre/post-index offset ~D is out of range [-256, 255]."
           inst-name offset)))

(defun check-unscaled-offset (offset inst-name)
  "Signal an error if OFFSET does not fit in a signed 9-bit field [-256,255]."
  (unless (typep offset '(signed-byte 9))
    (error "~S: unscaled offset ~D is out of range [-256, 255]."
           inst-name offset)))

(macrolet ((def (name size v opc)
             `(define-instruction ,name (segment rt mode)
                (:declare (type tn rt)
                          (type indexing-mode mode))
                (:printer load-store-uoff
                          ((size ,size) (v ,v) (opc ,opc)))
                (:printer load-store-idx
                          ((size ,size) (v ,v) (opc ,opc)))
                (:printer load-store-reg-off
                          ((size ,size) (v ,v) (opc ,opc)))
                (:emitter
                 ,@(when (zerop v)
                     `((assert-integer-reg rt ',name)))
                 (let ((base        (indexing-mode-base        mode))
                       (offset       (indexing-mode-offset       mode))
                       (index        (indexing-mode-index        mode))
                       (extend       (indexing-mode-extend       mode))
                       (shift        (indexing-mode-shift        mode))
                       (pre-index-p  (indexing-mode-pre-index-p  mode))
                       (post-index-p (indexing-mode-post-index-p mode)))
                   (cond
                     (index
                      (emit-load-store-reg-off-inst
                       segment ,size #b111 ,v #b00 ,opc 1
                       (tn-offset index)
                       (or (position extend arm64-extends) #b011)
                       (if (> shift 0) 1 0)
                       #b10
                       (tn-offset base) (tn-offset rt)))
                     (pre-index-p
                      (check-index-offset offset ',name)
                      (emit-load-store-idx-inst
                       segment ,size #b111 ,v #b00 ,opc 0
                       (ldb (byte 9 0) offset) #b11
                       (tn-offset base) (tn-offset rt)))
                     (post-index-p
                      (check-index-offset offset ',name)
                      (emit-load-store-idx-inst
                       segment ,size #b111 ,v #b00 ,opc 0
                       (ldb (byte 9 0) offset) #b01
                       (tn-offset base) (tn-offset rt)))
                     (t
                      (check-scaled-offset offset ,size ',name)
                      (emit-load-store-uoff-inst
                       segment ,size #b111 ,v #b01 ,opc
                       (/ offset (ash 1 ,size))
                       (tn-offset base) (tn-offset rt)))))))))
  (def ldrw   #b10 0 #b01)
  (def strw   #b10 0 #b00)

  (def ldrsw  #b10 0 #b10)
  (def ldrh   #b01 0 #b01)
  (def strh   #b01 0 #b00)

  (def ldrshx #b01 0 #b10)
  (def ldrshw #b01 0 #b11)

  (def ldrb   #b00 0 #b01)
  (def strb   #b00 0 #b00)

  (def ldrbx  #b00 0 #b10)
  (def ldrbw  #b00 0 #b11))

;;; --- Unscaled load/store (LDUR/STUR) ---
;;; Signed 9-bit byte offset [-256,255], no alignment requirement.

(macrolet ((def (name size v opc)
             `(define-instruction ,name (segment rt mode)
                (:declare (type tn rt)
                          (type indexing-mode mode))
                (:printer load-store-unscaled
                          ((size ,size) (v ,v) (opc ,opc)))
                (:emitter
                 ,@(when (zerop v)
                     `((assert-integer-reg rt ',name)))
                 (let ((base   (indexing-mode-base   mode))
                       (offset (indexing-mode-offset mode)))
                   (check-unscaled-offset offset ',name)
                   (emit-load-store-unscaled-inst
                    segment ,size #b111 ,v #b00 ,opc 0
                    (ldb (byte 9 0) offset) #b00
                    (tn-offset base) (tn-offset rt)))))))
  (def ldurw   #b10 0 #b01)
  (def sturw   #b10 0 #b00)

  (def ldursw  #b10 0 #b10)
  (def ldurh   #b01 0 #b01)
  (def sturh   #b01 0 #b00)

  (def ldurshx #b01 0 #b10)
  (def ldurshw #b01 0 #b11)

  (def ldurb   #b00 0 #b01)
  (def sturb   #b00 0 #b00)

  (def ldurbx  #b00 0 #b10)
  (def ldurbw  #b00 0 #b11))

;;; --- FP load/store (single and double) ---
;;;
;;; ldr/str with an FP register: size and opc are derived at emit time
;;; from the SC of RT via sc-case.  single-reg -> 32-bit (size=#b10),
;;; double-reg -> 64-bit (size=#b11).  V is always 1.

(define-instruction ldr (segment rt mode)
  (:declare (type tn rt)
            (type indexing-mode mode))
  (:printer load-store-uoff    ((opc #b01)))
  (:printer load-store-idx     ((opc #b01)))
  (:printer load-store-reg-off ((opc #b01)))
  (:emitter
   (multiple-value-bind (size v opc)
       (sc-case rt
         (single-reg     (values #b10 1 #b01))
         (double-reg     (values #b11 1 #b01))
         (descriptor-reg (values #b11 0 #b01))
         (any-reg        (values #b11 0 #b01))
         (unsigned-reg   (values #b11 0 #b01))
         (signed-reg     (values #b11 0 #b01)))
     (let ((base        (indexing-mode-base        mode))
           (offset       (indexing-mode-offset       mode))
           (index        (indexing-mode-index        mode))
           (extend       (indexing-mode-extend       mode))
           (shift        (indexing-mode-shift        mode))
           (pre-index-p  (indexing-mode-pre-index-p  mode))
           (post-index-p (indexing-mode-post-index-p mode)))
       (cond
         (index
          (emit-load-store-reg-off-inst
           segment size #b111 v #b00 opc 1
           (tn-offset index)
           (or (position extend arm64-extends) #b011)
           (if (> shift 0) 1 0)
           #b10
           (tn-offset base) (tn-offset rt)))
         (pre-index-p
          (check-index-offset offset 'ldr)
          (emit-load-store-idx-inst
           segment size #b111 v #b00 opc 0
           (ldb (byte 9 0) offset) #b11
           (tn-offset base) (tn-offset rt)))
         (post-index-p
          (check-index-offset offset 'ldr)
          (emit-load-store-idx-inst
           segment size #b111 v #b00 opc 0
           (ldb (byte 9 0) offset) #b01
           (tn-offset base) (tn-offset rt)))
         (t
          (check-scaled-offset offset size 'ldr)
          (emit-load-store-uoff-inst
           segment size #b111 v #b01 opc
           (/ offset (ash 1 size))
           (tn-offset base) (tn-offset rt))))))))

(define-instruction str (segment rt mode)
  (:declare (type tn rt)
            (type indexing-mode mode))
  (:printer load-store-uoff    ((opc #b00)))
  (:printer load-store-idx     ((opc #b00)))
  (:printer load-store-reg-off ((opc #b00)))
  (:emitter
   (multiple-value-bind (size v opc)
       (sc-case rt
         (single-reg     (values #b10 1 #b00))
         (double-reg     (values #b11 1 #b00))
         (descriptor-reg (values #b11 0 #b00))
         (any-reg        (values #b11 0 #b00))
         (unsigned-reg   (values #b11 0 #b00))
         (signed-reg     (values #b11 0 #b00)))
     (let ((base        (indexing-mode-base        mode))
           (offset       (indexing-mode-offset       mode))
           (index        (indexing-mode-index        mode))
           (extend       (indexing-mode-extend       mode))
           (shift        (indexing-mode-shift        mode))
           (pre-index-p  (indexing-mode-pre-index-p  mode))
           (post-index-p (indexing-mode-post-index-p mode)))
       (cond
         (index
          (emit-load-store-reg-off-inst
           segment size #b111 v #b00 opc 1
           (tn-offset index)
           (or (position extend arm64-extends) #b011)
           (if (> shift 0) 1 0)
           #b10
           (tn-offset base) (tn-offset rt)))
         (pre-index-p
          (check-index-offset offset 'str)
          (emit-load-store-idx-inst
           segment size #b111 v #b00 opc 0
           (ldb (byte 9 0) offset) #b11
           (tn-offset base) (tn-offset rt)))
         (post-index-p
          (check-index-offset offset 'str)
          (emit-load-store-idx-inst
           segment size #b111 v #b00 opc 0
           (ldb (byte 9 0) offset) #b01
           (tn-offset base) (tn-offset rt)))
         (t
          (check-scaled-offset offset size 'str)
          (emit-load-store-uoff-inst
           segment size #b111 v #b01 opc
           (/ offset (ash 1 size))
           (tn-offset base) (tn-offset rt))))))))

;;; FP unscaled load/store (LDUR/STUR for single/double registers).

(define-instruction ldur (segment rt mode)
  (:declare (type tn rt)
            (type indexing-mode mode))
  (:printer load-store-unscaled ((opc #b01)))
  (:emitter
   (multiple-value-bind (size v opc)
       (sc-case rt
         (single-reg   (values #b10 1 #b01))
         (double-reg   (values #b11 1 #b01))
         (descriptor-reg (values #b11 0 #b01))
         (any-reg      (values #b11 0 #b01))
         (unsigned-reg (values #b11 0 #b01))
         (signed-reg   (values #b11 0 #b01)))
     (let ((base   (indexing-mode-base   mode))
           (offset (indexing-mode-offset mode)))
       (check-unscaled-offset offset 'ldur)
       (emit-load-store-unscaled-inst
        segment size #b111 v #b00 opc 0
        (ldb (byte 9 0) offset) #b00
        (tn-offset base) (tn-offset rt))))))

(define-instruction stur (segment rt mode)
  (:declare (type tn rt)
            (type indexing-mode mode))
  (:printer load-store-unscaled ((opc #b00)))
  (:emitter
   (multiple-value-bind (size v opc)
       (sc-case rt
         (single-reg   (values #b10 1 #b00))
         (double-reg   (values #b11 1 #b00))
         (descriptor-reg (values #b11 0 #b00))
         (any-reg      (values #b11 0 #b00))
         (unsigned-reg (values #b11 0 #b00))
         (signed-reg   (values #b11 0 #b00)))
     (let ((base   (indexing-mode-base   mode))
           (offset (indexing-mode-offset mode)))
       (check-unscaled-offset offset 'stur)
       (emit-load-store-unscaled-inst
        segment size #b111 v #b00 opc 0
        (ldb (byte 9 0) offset) #b00
        (tn-offset base) (tn-offset rt))))))


;;; --- Load/store pair ---
;;;
;;; opc | 1 0 1 | V | mode[1] | mode[0] | L | imm7 | Rt2 | Rn | Rt1

(disassem:define-instruction-format (load-store-pair 32
                                     :default-printer
                                     '(:name :tab rt1 ", " rt2 ", " addr-imm))
  (opc      :field (byte 2 30))
  (f1       :field (byte 3 27) :value #b101)
  (v        :field (byte 1 26))
  (mode     :field (byte 3 23))          ; 001=post-index 010=signed-offset 011=pre-index
  (l        :field (byte 1 22))
  (imm7     :field (byte 7 15))
  (rt2      :field (byte 5 10)
            :printer #'(lambda (value stream dstate)
                         (let* ((word (disassem:sap-ref-int
                                       (disassem:dstate-segment-sap dstate)
                                       (disassem:dstate-cur-offs dstate)
                                       4 :little-endian))
                                (v   (ldb (byte 1 26) word))
                                (opc (ldb (byte 2 30) word)))
                           (cond ((zerop v)
                                  (if (= opc #b00)
                                      (format stream "w~D" value)
                                      (format stream "x~D" value)))
                                 (t
                                  (if (= opc #b01)
                                      (format stream "d~D" value)
                                      (format stream "s~D" value)))))))
  (rn       :field (byte 5 5)   :type 'sp-reg)
  (rt1      :field (byte 5 0)
            :printer #'(lambda (value stream dstate)
                         (let* ((word (disassem:sap-ref-int
                                       (disassem:dstate-segment-sap dstate)
                                       (disassem:dstate-cur-offs dstate)
                                       4 :little-endian))
                                (v   (ldb (byte 1 26) word))
                                (opc (ldb (byte 2 30) word)))
                           (cond ((zerop v)
                                  (if (= opc #b00)
                                      (format stream "w~D" value)
                                      (format stream "x~D" value)))
                                 (t
                                  (if (= opc #b01)
                                      (format stream "d~D" value)
                                      (format stream "s~D" value)))))))
  ;; addr-imm: reconstructs byte offset and prints correct addressing syntax
  (addr-imm :fields (list (byte 3 23) (byte 2 30) (byte 7 15) (byte 5 5))
            :printer #'(lambda (fields stream dstate)
                         (declare (ignore dstate))
                         (let* ((mode  (first fields))
                                (opc   (second fields))
                                (imm7  (third fields))
                                (rn    (fourth fields))
                                ;; byte offset = imm7 * scale
                                (scale (if (= opc #b10) 8 4))
                                (imm   (* (if (logbitp 6 imm7)
                                             (- imm7 128)
                                             imm7)
                                          scale))
                                (rn-str (if (= rn 31) "sp"
                                            (format nil "x~D" rn))))
                           (ecase mode
                             (#b010  ; signed offset
                              (format stream "[~A, #~D]" rn-str imm))
                             (#b011  ; pre-index
                              (format stream "[~A, #~D]!" rn-str imm))
                             (#b001  ; post-index
                              (format stream "[~A], #~D" rn-str imm)))))))

(define-emitter emit-load-store-pair-inst 32
  (byte 2 30)   ; opc
  (byte 3 27)   ; fixed #b101
  (byte 1 26)   ; v
  (byte 3 23)   ; mode: 001=post-index 010=signed-offset 011=pre-index
  (byte 1 22)   ; l
  (byte 7 15)   ; imm7
  (byte 5 10)   ; rt2
  (byte 5 5)    ; rn
  (byte 5 0))   ; rt1

(macrolet ((def (name opc v l scale-bits)
             `(define-instruction ,name (segment rt1 rt2 mode)
                (:declare (type tn rt1 rt2)
                          (type indexing-mode mode))
                (:printer load-store-pair ((opc ,opc) (v ,v) (l ,l)))
                (:emitter
                 (assert-pair-indexing-mode mode ',name)
                 (let* ((base        (indexing-mode-base        mode))
                        (offset      (indexing-mode-offset      mode))
                        (pre-index-p (indexing-mode-pre-index-p mode))
                        (post-index-p (indexing-mode-post-index-p mode))
                        (imm7 (ash offset ,(- scale-bits)))
                        (mcode (cond (pre-index-p  #b011)
                                     (post-index-p #b001)
                                     (t            #b010))))
                   (emit-load-store-pair-inst
                    segment ,opc #b101 ,v
                    mcode
                    ,l
                    (ldb (byte 7 0) imm7)
                    (tn-offset rt2) (tn-offset base) (tn-offset rt1)))))))
  (def ldpw  #b00 0 1 2)
  (def stpw  #b00 0 0 2)

  (def ldpsw #b01 0 1 2))

(defun assert-pair-indexing-mode (mode inst-name)
  "Signal an error if MODE has register-offset fields set, which are
not valid for load/store pair instructions."
  (when (indexing-mode-index mode)
    (error "~S: register index not valid in pair addressing mode." inst-name))
  (unless (eq (indexing-mode-extend mode) :uxtx)
    (error "~S: extend not valid in pair addressing mode." inst-name))
  (unless (zerop (indexing-mode-shift mode))
    (error "~S: shift not valid in pair addressing mode." inst-name)))

;;; Unified LDP/STP: dispatch on rt1 SC to determine opc, v, and scale.
;;;   integer (64-bit): opc=#b10 v=0 scale=3
;;;   single-reg:       opc=#b00 v=1 scale=2
;;;   double-reg:       opc=#b01 v=1 scale=3

(define-instruction ldp (segment rt1 rt2 mode)
  (:declare (type tn rt1 rt2)
            (type indexing-mode mode))
  (:printer load-store-pair ((l 1)))
  (:emitter
   (assert-pair-indexing-mode mode 'ldp)
   (multiple-value-bind (opc v scale)
       (sc-case rt1
         (single-reg     (values #b00 1 2))
         (double-reg     (values #b01 1 3))
         (descriptor-reg (values #b10 0 3))
         (any-reg        (values #b10 0 3))
         (unsigned-reg   (values #b10 0 3))
         (signed-reg     (values #b10 0 3)))
     (let* ((base         (indexing-mode-base         mode))
            (offset       (indexing-mode-offset       mode))
            (pre-index-p  (indexing-mode-pre-index-p  mode))
            (post-index-p (indexing-mode-post-index-p mode))
            (imm7  (ash offset (- scale)))
            (mcode (cond (pre-index-p  #b011)
                         (post-index-p #b001)
                         (t            #b010))))
       (emit-load-store-pair-inst
        segment opc #b101 v
        mcode 1
        (ldb (byte 7 0) imm7)
        (tn-offset rt2) (tn-offset base) (tn-offset rt1))))))

(define-instruction stp (segment rt1 rt2 mode)
  (:declare (type tn rt1 rt2)
            (type indexing-mode mode))
  (:printer load-store-pair ((l 0)))
  (:emitter
   (assert-pair-indexing-mode mode 'stp)
   (multiple-value-bind (opc v scale)
       (sc-case rt1
         (single-reg     (values #b00 1 2))
         (double-reg     (values #b01 1 3))
         (descriptor-reg (values #b10 0 3))
         (any-reg        (values #b10 0 3))
         (unsigned-reg   (values #b10 0 3))
         (signed-reg     (values #b10 0 3)))
     (let* ((base         (indexing-mode-base         mode))
            (offset       (indexing-mode-offset       mode))
            (pre-index-p  (indexing-mode-pre-index-p  mode))
            (post-index-p (indexing-mode-post-index-p mode))
            (imm7  (ash offset (- scale)))
            (mcode (cond (pre-index-p  #b011)
                         (post-index-p #b001)
                         (t            #b010))))
       (emit-load-store-pair-inst
        segment opc #b101 v
        mcode 0
        (ldb (byte 7 0) imm7)
        (tn-offset rt2) (tn-offset base) (tn-offset rt1))))))



;;; --- Load register literal ---
;;;
;;; opc | 0 1 1 | V | imm19 | Rt

(disassem:define-instruction-format (load-lit 32
                                     :default-printer '(:name :tab rt ", " label))
  (opc   :field (byte 2 30))
  (f1    :field (byte 3 27) :value #b011)
  (v     :field (byte 1 26))
  (imm19 :field (byte 19 5))
  (rt    :field (byte 5 0) :type 'reg)
  (label :field (byte 19 5)
         :use-label #'(lambda (value dstate)
                        (+ (disassem:dstate-cur-addr dstate)
                           (ash (if (logbitp 18 value)
                                    (- value (expt 2 19))
                                    value)
                                2)))))

(define-emitter emit-load-lit-inst 32
  (byte 2 30)   ; opc
  (byte 3 27)   ; fixed #b011
  (byte 1 26)   ; v
  (byte 19 5)   ; imm19
  (byte 5 0))   ; rt

(macrolet ((def (name opc v)
             `(define-instruction ,name (segment rt label)
                (:declare (type tn rt) (type label label))
                (:printer load-lit ((opc ,opc) (v ,v)))
                (:emitter
                 (emit-back-patch segment 4
                   #'(lambda (segment posn)
                       (let ((off (ash (- (label-position label) posn) -2)))
                         (emit-load-lit-inst segment ,opc #b011 ,v
                                             (ldb (byte 19 0) off)
                                             (tn-offset rt)))))))))
  (def ldrw-lit  #b00 0)
  (def ldrsw-lit #b10 0))

;;; Unified LDR-LIT: dispatches on rt SC for integer vs FP.
;;;   integer (64-bit): opc=#b01 v=0
;;;   single-reg:       opc=#b00 v=1
;;;   double-reg:       opc=#b01 v=1

(define-instruction ldr-lit (segment rt label)
  (:declare (type tn rt) (type label label))
  (:printer load-lit ((v 0) (opc #b01)))
  (:emitter
   (multiple-value-bind (opc v)
       (sc-case rt
         (single-reg     (values #b00 1))
         (double-reg     (values #b01 1))
         (descriptor-reg (values #b01 0))
         (any-reg        (values #b01 0))
         (unsigned-reg   (values #b01 0))
         (signed-reg     (values #b01 0)))
     (emit-back-patch segment 4
       #'(lambda (segment posn)
           (let ((off (ash (- (label-position label) posn) -2)))
             (emit-load-lit-inst segment opc #b011 v
                                 (ldb (byte 19 0) off)
                                 (tn-offset rt))))))))


;;; --- Load/store exclusive ---
;;;
;;; size | 0 0 1 0 0 0 | o2 | L | o1 | Rs | o0 | Rt2 | Rn | Rt

(disassem:define-instruction-format (load-store-excl 32
                                     :default-printer '(:name :tab rt ", [" rn "]"))
  (size :field (byte 2 30))
  (f1   :field (byte 6 24) :value #b001000)
  (o2   :field (byte 1 23))
  (l    :field (byte 1 22))
  (o1   :field (byte 1 21))
  (rs   :field (byte 5 16)
        :printer #'(lambda (value stream dstate)
                     (declare (ignore dstate))
                     (format stream "w~D" value)))
  (o0   :field (byte 1 15))
  (rt2  :field (byte 5 10))
  (rn   :field (byte 5 5)  :type 'sp-reg)
  (rt   :field (byte 5 0)
        :printer #'(lambda (value stream dstate)
                     (let ((size (ldb (byte 2 30)
                                      (disassem:sap-ref-int
                                       (disassem:dstate-segment-sap dstate)
                                       (disassem:dstate-cur-offs dstate)
                                       4 :little-endian))))
                       (if (= size #b11)
                           (format stream "x~D" value)
                           (format stream "w~D" value))))))

(define-emitter emit-load-store-excl-inst 32
  (byte 2 30)   ; size
  (byte 6 24)   ; fixed #b001000
  (byte 1 23)   ; o2
  (byte 1 22)   ; l
  (byte 1 21)   ; o1
  (byte 5 16)   ; rs
  (byte 1 15)   ; o0
  (byte 5 10)   ; rt2 (pass #b11111 when unused)
  (byte 5 5)    ; rn
  (byte 5 0))   ; rt

;;; Exclusive stores: Rs receives the status result, L=0.
(macrolet ((def (name size o2 o1 o0)
             `(define-instruction ,name (segment rs rt rn)
                (:declare (type tn rs rt rn))
                (:printer load-store-excl
                          ((size ,size) (o2 ,o2) (l 0) (o1 ,o1) (o0 ,o0))
                          '(:name :tab rs ", " rt ", [" rn "]"))
                (:emitter
                 (when (location= rs rt)
                   (error "~S: status register RS must not be the same as RT." ',name))
                 (when (location= rs rn)
                   (error "~S: status register RS must not be the same as RN." ',name))
                 (when (= (tn-offset rs) 31)
                   (error "~S: status register RS must not be XZR/WZR." ',name))
                 (emit-load-store-excl-inst
                  segment ,size #b001000 ,o2 0 ,o1
                  (tn-offset rs) ,o0 #b11111
                  (tn-offset rn) (tn-offset rt))))))
  (def stxr   #b11 0 0 0)
  (def stlxr  #b11 0 0 1)

  (def stxrw  #b10 0 0 0)
  (def stlxrw #b10 0 0 1))


;;; Exclusive/acquire loads: no Rs output.
(macrolet ((def (name size o2 l o1 o0)
             `(define-instruction ,name (segment rt rn)
                (:declare (type tn rt rn))
                (:printer load-store-excl
                          ((size ,size) (o2 ,o2) (l ,l) (o1 ,o1) (o0 ,o0)))
                (:emitter
                 (emit-load-store-excl-inst
                  segment ,size #b001000 ,o2 ,l ,o1
                  #b11111 ,o0 #b11111
                  (tn-offset rn) (tn-offset rt))))))
  (def ldxr   #b11 0 1 0 0)
  (def ldaxr  #b11 0 1 0 1)

  (def stlr   #b11 1 0 0 1)
  (def ldar   #b11 1 1 0 1)

  (def ldxrw  #b10 0 1 0 0)
  (def ldaxrw #b10 0 1 0 1)

  (def stlrw  #b10 1 0 0 1)
  (def ldarw  #b10 1 1 0 1))



;;;; ======================================================================
;;;; DATA PROCESSING -- SCALAR FP/SIMD
;;;; ======================================================================

;;; --- FP move immediate ---
;;;
;;; 0 0 0 1 1 1 1 0 | ftype | 1 | imm8 | 1 0 0 0 0 | 0 0 0 0 0 | Rd
;;;
;;; The 8-bit immediate encodes a small FP constant using the AArch64
;;; VFPExpandImm scheme:
;;;   bits: a bcdefgh
;;;   single: sign=a  exp=NOT(b):bb:bcde  mantissa=fgh:0...0
;;;   double: sign=a  exp=NOT(b):bb:bbbb:bcde  mantissa=fgh:0...0
;;;
;;; Only values exactly representable in this 8-bit scheme are legal.

(disassem:define-instruction-format (fp-imm 32
                                     :default-printer '(:name :tab rd ", " imm8))
  (f1    :field (byte 8 24) :value #b00011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 1 21) :value 1)
  (imm8  :field (byte 8 13)
         :printer #'(lambda (value stream dstate)
                      (declare (ignore dstate))
                      (format stream "#~S" (fp-imm8-decode value))))
  (f3    :field (byte 5 8)  :value #b10000)
  (f4    :field (byte 3 5)  :value 0)
  (rd    :field (byte 5 0)  :type 'fp-reg))

(define-emitter emit-fp-imm-inst 32
  (byte 8 24)   ; fixed #b00011110
  (byte 2 22)   ; ftype
  (byte 1 21)   ; fixed 1
  (byte 8 13)   ; imm8
  (byte 5 8)    ; fixed #b10000
  (byte 3 5)    ; fixed 0
  (byte 5 0))   ; rd

(defun fp-imm8-decode (imm8)
  "Expand an 8-bit AArch64 VFPExpandImm value to a Lisp single-float."
  (declare (type (unsigned-byte 8) imm8))
  (let* ((a    (ldb (byte 1 7) imm8))
         (b    (ldb (byte 1 6) imm8))
         (cde  (ldb (byte 3 3) imm8))
         (fgh  (ldb (byte 3 0) imm8))
         ;; single-precision: 1 sign, 8 exp, 23 mantissa
         (sign (ash a 31))
         (exp  (ash (logior (ash (logxor b 1) 6)
                            (ash b 5)
                            (ash b 4)
                            (ash b 3)
                            (ash b 2)
                            (ash b 1)
                            cde)
                    23))
         (mant (ash fgh 20))
         (bits (logior sign exp mant)))
    (kernel:make-single-float bits)))

(defun fp-imm8-encode (float)
  "Encode a float as an 8-bit AArch64 VFPExpandImm value.
Accepts single-float or double-float.  If a double-float is supplied it is
converted to single-float and an error is signalled if the conversion is
not exact.  Signals an error if the value is not representable in the
8-bit VFPExpandImm scheme."
  (let* ((single (if (typep float 'double-float)
                     (let ((s (coerce float 'single-float)))
                       (unless (= (coerce s 'double-float) float)
                         (error "~S cannot be exactly represented as a ~
                                 single-float." float))
                       s)
                     (coerce float 'single-float)))
         (bits  (kernel:single-float-bits single))
         (sign  (ldb (byte 1 31) bits))
         (exp   (ldb (byte 8 23) bits))
         (mant  (ldb (byte 23 0) bits))
         (b     (ldb (byte 1 6) exp))
         (cde   (ldb (byte 3 3) exp))  ; bits [5:3] of exp
         (fgh   (ldb (byte 3 20) mant)))
    ;; Validate: bit 7 of exp must be NOT(b), bits [6:1] must all equal b,
    ;; and the lower 20 mantissa bits must be zero.
    (unless (= (logxor b 1) (ldb (byte 1 7) exp))
      (error "~S is not representable as an 8-bit FP immediate." float))
    (let ((exp6 (ldb (byte 6 1) exp)))
      (unless (or (and (= b 0) (= exp6 #b000000))
                  (and (= b 1) (= exp6 #b111111)))
        (error "~S is not representable as an 8-bit FP immediate." float)))
    (unless (zerop (ldb (byte 20 0) mant))
      (error "~S is not representable as an 8-bit FP immediate." float))
    (logior (ash sign 7)
            (ash b 6)
            (ash cde 3)
            fgh)))

;;; --- FP compare ---
;;;
;;; 0 0 1 1 1 1 0 | ftype | 0 1 | Rm | op=00 | 1 0 0 0 0 | Rn | opc

(disassem:define-instruction-format (fp-compare 32
                                     :default-printer '(:name :tab rn ", " rm))
  (f1    :field (byte 7 25) :value #b0011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 2 20) :value #b01)
  (rm    :field (byte 5 16) :type 'fp-reg)
  (op    :field (byte 2 14) :value 0)
  (f3    :field (byte 5 9)  :value #b10000)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (opc   :field (byte 5 0)))

;;; fp-compare-zero: opc bit 3 set means compare with #0.0, rm field is 0.
(disassem:define-instruction-format (fp-compare-zero 32
                                     :default-printer '(:name :tab rn ", #0.0"))
  (f1    :field (byte 7 25) :value #b0011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 2 20) :value #b01)
  (rm    :field (byte 5 16) :value 0)
  (op    :field (byte 2 14) :value 0)
  (f3    :field (byte 5 9)  :value #b10000)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (opc   :field (byte 5 0)))

(define-emitter emit-fp-compare-inst 32
  (byte 7 25)   ; fixed #b0011110
  (byte 2 22)   ; ftype
  (byte 2 20)   ; fixed #b01
  (byte 5 16)   ; rm
  (byte 2 14)   ; op (fixed 0)
  (byte 5 9)    ; fixed #b10000
  (byte 5 5)    ; rn
  (byte 5 0))   ; opc

(macrolet ((def (name opc zero-opc)
             `(define-instruction ,name (segment rn &optional rm)
                (:declare (type tn rn)
                          (type (or tn null) rm))
                (:printer fp-compare ((opc ,opc)))
                (:printer fp-compare-zero ((opc ,zero-opc)))
                (:emitter
                 (let ((ftype (sc-case rn
                                (single-reg #b00)
                                (double-reg #b01))))
                   (emit-fp-compare-inst
                    segment #b0011110 ftype #b01
                    (if rm (tn-offset rm) 0)
                    0 #b10000 (tn-offset rn)
                    (if rm ,opc ,zero-opc)))))))
  (def fcmp  #b00000 #b01000)
  (def fcmpe #b10000 #b11000))


;;; --- FP data-processing 1-source ---
;;;
;;; 0 0 1 1 1 1 0 | ftype | 1 | opcode[5:0] | 1 0 0 0 0 | Rn | Rd

(disassem:define-instruction-format (fp-dp1 32
                                     :default-printer '(:name :tab rd ", " rn))
  (f1    :field (byte 7 25) :value #b0011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 1 21) :value 1)
  (opc   :field (byte 6 15))
  (f3    :field (byte 5 10) :value #b10000)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (rd    :field (byte 5 0)  :type 'fp-reg))

(define-emitter emit-fp-dp1-inst 32
  (byte 7 25)   ; fixed #b0011110
  (byte 2 22)   ; ftype
  (byte 1 21)   ; fixed 1
  (byte 6 15)   ; opc
  (byte 5 10)   ; fixed #b10000
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name opc)
             `(define-instruction ,name (segment rd rn)
                (:declare (type tn rd rn))
                (:printer fp-dp1 ((opc ,opc)))
                (:emitter
                 (let ((ftype (sc-case rd
                                (single-reg #b00)
                                (double-reg #b01))))
                   (emit-fp-dp1-inst segment #b0011110 ftype 1 ,opc #b10000
                                     (tn-offset rn) (tn-offset rd)))))))
  (def fabs   #b000001)
  (def fneg   #b000010)
  (def fsqrt  #b000011)
  (def frintn #b001000)
  (def frintp #b001001)

  (def frintm #b001010)
  (def frintz #b001011)

  (def frinta #b001100)
  (def frintx #b001110)

  (def frinti #b001111))

;;; fcvt is kept separate: rd and rn are different precisions.
(define-instruction fcvt (segment rd rn)
  (:declare (type tn rd rn))
  (:printer fp-dp1 ((opc #b000101)))   ; S->D: ftype=#b00, opc=#b000101
  (:printer fp-dp1 ((opc #b000100)))   ; D->S: ftype=#b01, opc=#b000100
  (:emitter
   ;; ftype encodes the *source* precision; opc[1:0] encodes the *destination*.
   (multiple-value-bind (ftype opc)
       (sc-case rn
         (single-reg (values #b00 #b000101))   ; S->D
         (double-reg (values #b01 #b000100)))  ; D->S
     (emit-fp-dp1-inst segment #b0011110 ftype 1 opc #b10000
                       (tn-offset rn) (tn-offset rd)))))


;;; --- FP data-processing 2-source ---
;;;
;;; 0 0 1 1 1 1 0 | ftype | 1 | Rm | opc[3:0] | 1 0 | Rn | Rd

(disassem:define-instruction-format (fp-dp2 32
                                     :default-printer '(:name :tab rd ", " rn ", " rm))
  (f1    :field (byte 7 25) :value #b0011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 1 21) :value 1)
  (rm    :field (byte 5 16) :type 'fp-reg)
  (opc   :field (byte 4 12))
  (f3    :field (byte 2 10) :value #b10)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (rd    :field (byte 5 0)  :type 'fp-reg))

(define-emitter emit-fp-dp2-inst 32
  (byte 7 25)   ; fixed #b0011110
  (byte 2 22)   ; ftype
  (byte 1 21)   ; fixed 1
  (byte 5 16)   ; rm
  (byte 4 12)   ; opc
  (byte 2 10)   ; fixed #b10
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name opc)
             `(define-instruction ,name (segment rd rn rm)
                (:declare (type tn rd rn rm))
                (:printer fp-dp2 ((opc ,opc)))
                (:emitter
                 (let ((ftype (sc-case rd
                                (single-reg #b00)
                                (double-reg #b01))))
                   (emit-fp-dp2-inst segment #b0011110 ftype 1
                                     (tn-offset rm) ,opc #b10
                                     (tn-offset rn) (tn-offset rd)))))))
  (def fmul   #b0000)
  (def fdiv   #b0001)

  (def fadd   #b0010)
  (def fsub   #b0011)

  (def fmax   #b0100)
  (def fmin   #b0101)

  (def fmaxnm #b0110)
  (def fminnm #b0111)

  (def fnmul  #b1000))


;;; --- FP data-processing 3-source ---
;;;
;;; 0 0 1 1 1 1 1 | ftype | o1 | Rm | o0 | Ra | Rn | Rd

(disassem:define-instruction-format (fp-dp3 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ", " ra))
  (f1    :field (byte 7 25) :value #b0011111)
  (ftype :field (byte 2 22))
  (o1    :field (byte 1 21))
  (rm    :field (byte 5 16) :type 'fp-reg)
  (o0    :field (byte 1 15))
  (ra    :field (byte 5 10) :type 'fp-reg)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (rd    :field (byte 5 0)  :type 'fp-reg))

(define-emitter emit-fp-dp3-inst 32
  (byte 7 25)   ; fixed #b0011111
  (byte 2 22)   ; ftype
  (byte 1 21)   ; o1
  (byte 5 16)   ; rm
  (byte 1 15)   ; o0
  (byte 5 10)   ; ra
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def (name o1 o0)
             `(define-instruction ,name (segment rd rn rm ra)
                (:declare (type tn rd rn rm ra))
                (:printer fp-dp3 ((o1 ,o1) (o0 ,o0)))
                (:emitter
                 (let ((ftype (sc-case rd
                                (single-reg #b00)
                                (double-reg #b01))))
                   (emit-fp-dp3-inst segment #b0011111 ftype ,o1
                                     (tn-offset rm) ,o0 (tn-offset ra)
                                     (tn-offset rn) (tn-offset rd)))))))
  (def fmadd  0 0)
  (def fmsub  0 1)

  (def fnmadd 1 0)
  (def fnmsub 1 1))



;;; --- FP <-> integer conversion ---
;;;
;;; sf | 0 | 0 | 1 1 1 1 0 | ftype | 1 | rmode[1:0] | opcode[2:0] | 000000 | Rn | Rd

;;; FP -> integer: rd is integer (sf-reg), rn is FP (fp-reg)
(disassem:define-instruction-format (fp->int-conv 32
                                     :default-printer '(:name :tab rd ", " rn))
  (sf    :field (byte 1 31))
  (f1    :field (byte 6 25) :value #b011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 1 21) :value 1)
  (rmode :field (byte 2 19))
  (opc   :field (byte 3 16))
  (f3    :field (byte 6 10) :value 0)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (rd    :field (byte 5 0)  :type 'sf-reg))

;;; integer -> FP: rd is FP (fp-reg), rn is integer (sf-reg)
(disassem:define-instruction-format (int->fp-conv 32
                                     :default-printer '(:name :tab rd ", " rn))
  (sf    :field (byte 1 31))
  (f1    :field (byte 6 25) :value #b011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 1 21) :value 1)
  (rmode :field (byte 2 19))
  (opc   :field (byte 3 16))
  (f3    :field (byte 6 10) :value 0)
  (rn    :field (byte 5 5)  :type 'sf-reg)
  (rd    :field (byte 5 0)  :type 'fp-reg))

(define-emitter emit-fp-int-conv-inst 32
  (byte 1 31)   ; sf
  (byte 6 25)   ; fixed #b011110
  (byte 2 22)   ; ftype
  (byte 1 21)   ; fixed 1
  (byte 2 19)   ; rmode
  (byte 3 16)   ; opc
  (byte 6 10)   ; fixed 0
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(macrolet ((def-fp->int (name rmode opc)
             ;; FP -> integer: rd is integer (sf from rd SC),
             ;;                rn is FP     (ftype from rn SC).
             `(define-instruction ,name (segment rd rn)
                (:declare (type tn rd rn))
                (:printer fp->int-conv ((rmode ,rmode) (opc ,opc)))
                (:emitter
                 (let ((sf    (sc-case rd
                                (descriptor-reg 1)
                                (any-reg        1)
                                (unsigned-reg   1)
                                (signed-reg     1)))
                       (ftype (sc-case rn
                                (single-reg #b00)
                                (double-reg #b01))))
                   (emit-fp-int-conv-inst segment sf #b011110 ftype 1
                                          ,rmode ,opc 0
                                          (tn-offset rn) (tn-offset rd))))))
           (def-int->fp (name opc)
             ;; integer -> FP: rd is FP     (ftype from rd SC),
             ;;                rn is integer (sf from rn SC).
             `(define-instruction ,name (segment rd rn)
                (:declare (type tn rd rn))
                (:printer int->fp-conv ((rmode #b00) (opc ,opc)))
                (:emitter
                 (let ((sf    (sc-case rn
                                (descriptor-reg 1)
                                (any-reg        1)
                                (unsigned-reg   1)
                                (signed-reg     1)))
                       (ftype (sc-case rd
                                (single-reg #b00)
                                (double-reg #b01))))
                   (emit-fp-int-conv-inst segment sf #b011110 ftype 1
                                          #b00 ,opc 0
                                          (tn-offset rn) (tn-offset rd)))))))
  ;; FP -> integer (rmode selects rounding: 00=nearest 01=+inf 10=-inf 11=zero)
  (def-fp->int fcvtns #b00 #b000)
  (def-fp->int fcvtnu #b00 #b001)
  (def-fp->int fcvtps #b01 #b000)
  (def-fp->int fcvtpu #b01 #b001)
  (def-fp->int fcvtms #b10 #b000)
  (def-fp->int fcvtmu #b10 #b001)
  (def-fp->int fcvtzs #b11 #b000)
  (def-fp->int fcvtzu #b11 #b001)
  ;; integer -> FP
  (def-int->fp scvtf #b010)
  (def-int->fp ucvtf #b011))

;;; Unified FMOV: dispatches on the type of RN.
;;;   float         -> fp-imm encoding (8-bit VFPExpandImm)
;;;   FP  <- FP     -> fp-dp1 encoding, opc=#b000000
;;;   FP  <- int    -> fp-int-conv encoding, opc=#b111
;;;   int <- FP     -> fp-int-conv encoding, opc=#b110

(define-instruction fmov (segment rd rn)
  (:declare (type tn rd)
            (type (or tn single-float double-float) rn))
  (:printer fp-imm ())
  (:printer fp-dp1 ((opc #b000000)))
  (:printer fp->int-conv ((rmode #b00) (opc #b110)))
  (:printer int->fp-conv ((rmode #b00) (opc #b111)))
  (:emitter
   (if (typep rn '(or single-float double-float))
       ;; Immediate form: FMOV Rd, #float
       (let ((ftype (sc-case rd
                      (single-reg #b00)
                      (double-reg #b01))))
         (emit-fp-imm-inst segment #b00011110 ftype 1
                           (fp-imm8-encode rn) #b10000 0
                           (tn-offset rd)))
       ;; Register forms
       (sc-case rd
         (single-reg
          (sc-case rn
            (single-reg                          ; S <- S (fp-dp1)
             (emit-fp-dp1-inst segment #b0011110 #b00 1 #b000000 #b10000
                               (tn-offset rn) (tn-offset rd)))
            ((unsigned-reg signed-reg)           ; S <- W (fp-int-conv)
             (emit-fp-int-conv-inst segment 0 #b011110 #b00 1 #b00 #b111 0
                                    (tn-offset rn) (tn-offset rd)))))
         (double-reg
          (sc-case rn
            (double-reg                          ; D <- D (fp-dp1)
             (emit-fp-dp1-inst segment #b0011110 #b01 1 #b000000 #b10000
                               (tn-offset rn) (tn-offset rd)))
            ((unsigned-reg signed-reg)           ; D <- X (fp-int-conv)
             (emit-fp-int-conv-inst segment 1 #b011110 #b01 1 #b00 #b111 0
                                    (tn-offset rn) (tn-offset rd)))))
         ((unsigned-reg signed-reg)
          (sc-case rn
            (single-reg                          ; W <- S (fp-int-conv)
             (emit-fp-int-conv-inst segment 0 #b011110 #b00 1 #b00 #b110 0
                                    (tn-offset rn) (tn-offset rd)))
            (double-reg                          ; X <- D (fp-int-conv)
             (emit-fp-int-conv-inst segment 1 #b011110 #b01 1 #b00 #b110 0
                                    (tn-offset rn) (tn-offset rd)))))))))



;;; --- FP conditional select ---
;;;
;;; 0 0 1 1 1 1 0 | ftype | 1 | Rm | cond | 1 1 | Rn | Rd

(disassem:define-instruction-format (fp-csel 32
                                     :default-printer
                                     '(:name :tab rd ", " rn ", " rm ", " cond))
  (f1    :field (byte 7 25) :value #b0011110)
  (ftype :field (byte 2 22))
  (f2    :field (byte 1 21) :value 1)
  (rm    :field (byte 5 16) :type 'fp-reg)
  (cond  :field (byte 4 12) :type 'condition)
  (f3    :field (byte 2 10) :value #b11)
  (rn    :field (byte 5 5)  :type 'fp-reg)
  (rd    :field (byte 5 0)  :type 'fp-reg))

(define-emitter emit-fp-csel-inst 32
  (byte 7 25)   ; fixed #b0011110
  (byte 2 22)   ; ftype
  (byte 1 21)   ; fixed 1
  (byte 5 16)   ; rm
  (byte 4 12)   ; cond
  (byte 2 10)   ; fixed #b11
  (byte 5 5)    ; rn
  (byte 5 0))   ; rd

(define-instruction fcsel (segment rd rn rm cond)
  (:declare (type tn rd rn rm)
            (type (or keyword (unsigned-byte 4)) cond))
  (:printer fp-csel ())
  (:emitter
   (let ((ftype (sc-case rd
                  (single-reg #b00)
                  (double-reg #b01)))
         (c (if (keywordp cond) (condition-encoding cond) cond)))
     (emit-fp-csel-inst segment #b0011110 ftype 1
                        (tn-offset rm) c #b11
                        (tn-offset rn) (tn-offset rd)))))


;;;; ======================================================================
;;;; PSEUDO-INSTRUCTIONS
;;;; ======================================================================

(define-instruction word (segment value)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) value))
  (:emitter (emit-word segment (ldb (byte 32 0) value))))

(define-instruction dword (segment value)
  (:declare (type (or (unsigned-byte 64) (signed-byte 64)) value))
  (:emitter
   (emit-word segment (ldb (byte 32  0) value))
   (emit-word segment (ldb (byte 32 32) value))))

(define-instruction align (segment alignment)
  (:declare (type (unsigned-byte 7) alignment))
  (:emitter (emit-alignment segment alignment #xD503201F)))


