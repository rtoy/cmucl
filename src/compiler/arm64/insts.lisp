;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/insts.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the AArch64 (ARM64) A64 architecture.
;;;
;;; Written by Raymond Toy
;;;
;;; Reference: ARM Architecture Reference Manual, ARMv8-A, ARM DDI 0487
;;; Encoding index: https://www.scs.stanford.edu/~zyedidia/arm64/encodingindex.html
;;;
;;; Top-level A64 encoding groups (op0[31], op1[30:28:27:26:25]):
;;;
;;;   op1 = 100x  Data Processing -- Immediate
;;;   op1 = 101x  Branches, Exception Generating and System instructions
;;;   op1 = x101  Data Processing -- Register
;;;   op1 = x111  Data Processing -- Scalar FP and Advanced SIMD
;;;   op1 = x1x0  Loads and Stores
;;;
;;; SME and SVE encodings are not included.
;;;
(in-package "ARM64")

(use-package "NEW-ASSEM")

(def-assembler-params
  :scheduler-p nil)


;;;; Constants, types, conversion functions, some disassembler stuff.

;; In AArch64, the PC is not directly readable as a general-purpose
;; register; there is no equivalent of ARMv7's +8 bias.  The PC is
;; only accessible via ADR/ADRP or branch-and-link.
(defconstant pc-read-offset 0)

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
         (tn-offset tn)
         (error (intl:gettext "~S isn't a register.") tn)))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error (intl:gettext "~S isn't a floating-point register.") tn))
  (tn-offset tn))

(disassem:set-disassem-params :instruction-alignment 32
                              :opcode-column-width 12)


;;; Symbols used for disassembly printing.

(defparameter reg-symbols
  (map 'vector #'make-symbol *register-names*)
  "The Lisp names for the ARM64 integer registers.")

;; AArch64 has 31 general-purpose registers X0-X30 plus the
;; zero-register XZR (encoded as 31 in most contexts) and SP (also
;; encoded as 31 in stack-pointer contexts).
(defparameter arm64-reg-symbols
  #(x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30 xzr)
  "The AArch64 64-bit names for integer registers.")

(defparameter arm64-wreg-symbols
  #(w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30 wzr)
  "The AArch64 32-bit names for integer registers.")

(defvar *use-lisp-register-names* t)

(defun get-reg-name (index &optional (sf 1))
  "Return the register name for INDEX.  SF=1 (default) gives 64-bit Xn names,
  SF=0 gives 32-bit Wn names."
  (if *use-lisp-register-names*
      (aref reg-symbols index)
      (if (zerop sf)
          (aref arm64-wreg-symbols index)
          (aref arm64-reg-symbols index))))

(eval-when (compile load eval)
(defun reg-arg-printer (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (get-reg-name value)))
    (princ regname stream)
    (disassem:maybe-note-associated-storage-ref value
                                               'registers
                                               regname
                                               dstate)))
) ; eval-when

(disassem:define-argument-type reg
  :printer #'reg-arg-printer)

;; 32-bit Wn register printer.
(disassem:define-argument-type wreg
  :printer #'(lambda (value stream dstate)
               (declare (stream stream) (fixnum value))
               (let ((regname (get-reg-name value 0)))
                 (princ regname stream)
                 (disassem:maybe-note-associated-storage-ref
                  value 'registers regname dstate))))

;; We need separate types for single and double float because the
;; disassembler needs to print S<n> vs D<n>.
(disassem:define-argument-type fp-reg-single
  :printer #'(lambda (value stream dstate)
               (declare (stream stream) (fixnum value))
               (princ 'S stream)
               (princ value stream)
               (disassem:maybe-note-associated-storage-ref
                value
                'float-registers
                (symbolicate "S" (format nil "~D" value))
                dstate)))

(disassem:define-argument-type fp-reg-double
  :printer #'(lambda (value stream dstate)
               (declare (stream stream) (fixnum value))
               (princ 'D stream)
               (princ value stream)
               (disassem:maybe-note-associated-storage-ref
                value
                'float-registers
                (symbolicate "D" (format nil "~D" value))
                dstate)))


;;; Condition codes.
;;;
;;; AArch64 uses the same four condition flags (N, Z, C, V) as ARMv7
;;; but the encoding is identical.  Table C1-1 in the ARM ARM.

(defconstant condition-codes
  '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al :nv))

(deftype condition-code ()
  `(member ,@condition-codes))

(defconstant condition-code-name-vec
  (coerce condition-codes 'vector))

(defconstant condition-always #b1110)   ; :al

(disassem:define-argument-type condition-code
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (unless (= value condition-always)
                 (princ (aref condition-code-name-vec value) stream))))

(defun condition-code-encoding (c)
  (let ((position (position c condition-codes)))
    (or position
        (error "Unknown condition code ~S" c))))


;;; Shift types used in Data Processing -- Register and Load/Store.
;;; Table C3-1: LSL=0, LSR=1, ASR=2, ROR=3.

(defconstant shift-types '(:lsl :lsr :asr :ror))

(deftype shift-type ()
  `(member ,@shift-types))

(disassem:define-argument-type shift-type
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (princ (elt shift-types value) stream)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun shift-type-encoding (shift-type)
  (or (position shift-type shift-types)
      (error "Unknown shift type: ~S" shift-type)))
) ; eval-when


;;; Extend types used in extended-register instructions.
;;; Table C3-2: UXTB=0 UXTH=1 UXTW=2 UXTX/LSL=3 SXTB=4 SXTH=5 SXTW=6 SXTX=7

(defconstant extend-types
  '(:uxtb :uxth :uxtw :uxtx :sxtb :sxth :sxtw :sxtx))

(disassem:define-argument-type extend-type
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate))
               (princ (elt extend-types value) stream)))

(defun extend-type-encoding (extend-type)
  (or (position extend-type extend-types)
      (error "Unknown extend type: ~S" extend-type)))


;;; Immediate printing helpers.

(defun print-immed (value stream)
  (format stream "#~S" value))

(eval-when (compile load eval)
(defun signed-immed-printer (value stream dstate)
  "Print a sign-extended immediate in the assembler style."
  (declare (stream stream) (ignore dstate))
  (print-immed value stream))
) ; eval-when


;;; Relative-label argument type (for branch instructions).
;;; AArch64 branch offsets are PC-relative with no read-ahead bias.

(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
                 (declare (type integer value)
                          (type disassem:disassem-state dstate))
                 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

;; 19-bit relative label (conditional branches, CBZ/CBNZ, LDR-literal).
(disassem:define-argument-type relative-label-19
  :sign-extend t
  :use-label #'(lambda (value dstate)
                 (declare (type (signed-byte 19) value)
                          (type disassem:disassem-state dstate))
                 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

;; 14-bit relative label (TBZ/TBNZ).
(disassem:define-argument-type relative-label-14
  :sign-extend t
  :use-label #'(lambda (value dstate)
                 (declare (type (signed-byte 14) value)
                          (type disassem:disassem-state dstate))
                 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))


;;;; Primitive emitters.

(define-emitter emit-word 32
  (byte 32 0))

(define-emitter emit-short 16
  (byte 16 0))


;;;; -----------------------------------------------------------------------
;;;; Instruction formats.
;;;;
;;;; Each format corresponds to a leaf encoding class from the A64
;;;; encoding index.  All A64 instructions are exactly 32 bits.
;;;;
;;;; Naming conventions follow the encoding index section headings,
;;;; simplified and lowercased with hyphens.
;;;; -----------------------------------------------------------------------


;;;; Base format -- common to all A64 instructions.
;;;;
;;;; Bits [31] and [28:25] select the major group; they are named
;;;; op0/op1 at the top level but differ inside sub-groups, so we only
;;;; capture the fixed bit positions that appear in every instruction.

(disassem:define-instruction-format
    (format-base 32)
  (op0 :field (byte 1 31))     ; selects 32 vs 64-bit in most groups (sf bit)
  (op1 :field (byte 7 25)))    ; major group select


;;;; -----------------------------------------------------------------------
;;;; Data Processing -- Immediate  (op1 = 100x, bits [28:26:25] = 100)
;;;;
;;;; Section C4.1.2 of the ARM ARM.
;;;; -----------------------------------------------------------------------

;;; PC-rel addressing: ADR / ADRP
;;;
;;; Encoding (C6.2.10/C6.2.11):
;;;   [31]    op       (0=ADR, 1=ADRP)
;;;   [30:29] immlo    (low 2 bits of immediate)
;;;   [28:24] 10000
;;;   [23:5]  immhi    (high 21 bits of immediate)
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-pc-rel 32
     :default-printer '(:name :tab rd ", " imm))
  (op    :field (byte 1 31))
  (immlo :field (byte 2 29))
  (op1   :field (byte 5 24))
  (immhi :field (byte 19 5))
  (imm   :fields (list (byte 19 5) (byte 2 29))
         :printer #'(lambda (vlist stream dstate)
                      (declare (ignore dstate))
                      (let* ((raw (logior (ash (first vlist) 2)
                                          (second vlist)))
                             ;; sign-extend from bit 20 (21-bit signed value)
                             (imm (if (logbitp 20 raw)
                                      (- raw (ash 1 21))
                                      raw)))
                        (print-immed imm stream))))
  (rd    :field (byte 5 0) :type 'reg))

(define-emitter emit-format-pc-rel 32
  (byte 1 31) (byte 2 29) (byte 5 24) (byte 19 5) (byte 5 0))


;;; Add/Sub (immediate)
;;;
;;; Encoding (C6.2.4 / C6.2.196):
;;;   [31]    sf         (0=32-bit, 1=64-bit)
;;;   [30]    op         (0=ADD, 1=SUB)
;;;   [29]    S          (0=no flags, 1=set flags)
;;;   [28:24] 10001
;;;   [23:22] shift      (00=LSL#0, 01=LSL#12)
;;;   [23]    0      (reserved)
;;;   [22]    shift  (0=LSL#0, 1=LSL#12)
;;;   [21:10] imm12
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-add-sub-imm 32
     :default-printer '(:name :tab rd ", " rn ", " imm12
                        (:unless (shift :constant 0)
                          ", lsl #12")))
  (sf    :field (byte 1 31))
  (op    :field (byte 1 30))
  (s     :field (byte 1 29))
  (op1   :field (byte 5 24))
  (res0  :field (byte 1 23) :value 0)
  (shift :field (byte 1 22))
  (imm12 :field (byte 12 10))
  (rn    :field (byte 5 5)  :type 'reg)
  (rd    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-add-sub-imm 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 1 23)
  (byte 1 22) (byte 12 10) (byte 5 5) (byte 5 0))


;;; Logical (immediate)
;;;
;;; Encoding (C6.2.113 / C6.2.114 / C6.2.115 / C6.2.116):
;;;   [31]    sf
;;;   [30:29] opc     (00=AND, 01=ORR, 10=EOR, 11=ANDS)
;;;   [28:23] 100100
;;;   [22]    N       (part of bitmask immediate)
;;;   [21:16] immr    (part of bitmask immediate)
;;;   [15:10] imms    (part of bitmask immediate)
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-logic-imm 32
     :default-printer '(:name :tab rd ", " rn ", " bimm))
  (sf   :field (byte 1 31))
  (opc  :field (byte 2 29))
  (op1  :field (byte 6 23) :value #b100100)
  (bimm :fields (list (byte 1 22) (byte 6 16) (byte 6 10))
        :printer #'(lambda (vlist stream dstate)
                     (declare (ignore dstate))
                     (destructuring-bind (n immr imms) vlist
                       (format stream "#x~X" (decode-bit-mask n immr imms)))))
  (rn   :field (byte 5 5) :type 'reg)
  (rd   :field (byte 5 0) :type 'reg))

(define-emitter emit-format-logic-imm 32
  (byte 1 31) (byte 2 29) (byte 6 23) (byte 1 22) (byte 6 16)
  (byte 6 10) (byte 5 5) (byte 5 0))


;;; Move wide (immediate): MOVN / MOVZ / MOVK
;;;
;;; Encoding (C6.2.131 / C6.2.135 / C6.2.130):
;;;   [31]    sf
;;;   [30:29] opc     (00=MOVN, 10=MOVZ, 11=MOVK)
;;;   [28:23] 100101
;;;   [22:21] hw      (shift = hw*16)
;;;   [20:5]  imm16
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-move-wide 32
     :default-printer '(:name :tab rd ", #" imm16
                        (:unless (hw :constant 0)
                          ", lsl #" hw)))
  (sf    :field (byte 1 31))
  (opc   :field (byte 2 29))
  (op1   :field (byte 6 23) :value #b100101)
  (hw    :field (byte 2 21)
         :printer #'(lambda (value stream dstate)
                      (declare (ignore dstate))
                      (princ (* value 16) stream)))
  (imm16 :field (byte 16 5))
  (rd    :field (byte 5 0) :type 'reg))

(define-emitter emit-format-move-wide 32
  (byte 1 31) (byte 2 29) (byte 6 23) (byte 2 21) (byte 16 5) (byte 5 0))


;;; Bitfield: SBFM / BFM / UBFM
;;;
;;; Encoding (C6.2.26 / C6.2.28 / C6.2.238):
;;;   [31]    sf
;;;   [30:29] opc     (00=SBFM, 01=BFM, 10=UBFM)
;;;   [28:23] 100110
;;;   [22]    N
;;;   [21:16] immr
;;;   [15:10] imms
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-bitfield 32
     :default-printer '(:name :tab rd ", " rn ", " immr ", " imms))
  (sf   :field (byte 1 31))
  (opc  :field (byte 2 29))
  (op1  :field (byte 6 23) :value #b100110)
  (n    :field (byte 1 22))
  (immr :field (byte 6 16))
  (imms :field (byte 6 10))
  (rn   :field (byte 5 5) :type 'reg)
  (rd   :field (byte 5 0) :type 'reg))

(define-emitter emit-format-bitfield 32
  (byte 1 31) (byte 2 29) (byte 6 23) (byte 1 22) (byte 6 16)
  (byte 6 10) (byte 5 5) (byte 5 0))


;;; Extract: EXTR
;;;
;;; Encoding (C6.2.73):
;;;   [31]    sf
;;;   [30:29] op21    (00)
;;;   [28:23] 100111
;;;   [22]    N
;;;   [21]    o0      (0)
;;;   [20:16] Rm
;;;   [15:10] imms
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-extract 32
     :default-printer '(:name :tab rd ", " rn ", " rm ", " imms))
  (sf   :field (byte 1 31))
  (op21 :field (byte 2 29) :value 0)
  (op1  :field (byte 6 23) :value #b100111)
  (n    :field (byte 1 22))
  (o0   :field (byte 1 21) :value 0)
  (rm   :field (byte 5 16) :type 'reg)
  (imms :field (byte 6 10))
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-extract 32
  (byte 1 31) (byte 2 29) (byte 6 23) (byte 1 22) (byte 1 21)
  (byte 5 16) (byte 6 10) (byte 5 5) (byte 5 0))


;;;; -----------------------------------------------------------------------
;;;; Branches, Exception Generating and System instructions
;;;; (op1 = 101x, bits [29:28:26:25] = 1010 or 1011)
;;;;
;;;; Section C4.1.3 of the ARM ARM.
;;;; -----------------------------------------------------------------------

;;; Unconditional branch (immediate): B / BL
;;;
;;; Encoding (C6.2.33 / C6.2.35):
;;;   [31]    op      (0=B, 1=BL)
;;;   [30:26] 00101
;;;   [25:0]  imm26

(disassem:define-instruction-format
    (branch-imm 32
     :default-printer '(:name :tab imm26))
  (op    :field (byte 1 31))
  (op1   :field (byte 5 26) :value #b00101)
  (imm26 :field (byte 26 0) :type 'relative-label))

(define-emitter emit-branch-imm 32
  (byte 1 31) (byte 5 26) (byte 26 0))


;;; Compare and branch (immediate): CBZ / CBNZ
;;;
;;; Encoding (C6.2.43 / C6.2.44):
;;;   [31]    sf
;;;   [30:25] 011010
;;;   [24]    op      (0=CBZ, 1=CBNZ)
;;;   [23:5]  imm19
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-compare-branch 32
     :default-printer '(:name :tab rt ", " imm19))
  (sf    :field (byte 1 31))
  (op1   :field (byte 6 25) :value #b011010)
  (op    :field (byte 1 24))
  (imm19 :field (byte 19 5) :type 'relative-label-19)
  (rt    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-compare-branch 32
  (byte 1 31) (byte 6 25) (byte 1 24) (byte 19 5) (byte 5 0))


;;; Test and branch (immediate): TBZ / TBNZ
;;;
;;; Encoding (C6.2.228 / C6.2.229):
;;;   [31]    b5      (MSB of bit-number)
;;;   [30:25] 011011
;;;   [24]    op      (0=TBZ, 1=TBNZ)
;;;   [23:19] b40     (low 5 bits of bit-number)
;;;   [18:5]  imm14
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-test-branch 32
     :default-printer '(:name :tab rt ", " b40 ", " imm14))
  (b5    :field (byte 1 31))
  (op1   :field (byte 6 25) :value #b011011)
  (op    :field (byte 1 24))
  (b40   :field (byte 5 19))
  (imm14 :field (byte 14 5) :type 'relative-label-14)
  (rt    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-test-branch 32
  (byte 1 31) (byte 6 25) (byte 1 24) (byte 5 19) (byte 14 5) (byte 5 0))


;;; Conditional branch (immediate): B.cond
;;;
;;; Encoding (C6.2.34):
;;;   [31:25] 0101010
;;;   [24]    o1      (0)
;;;   [23:5]  imm19
;;;   [4]     o0      (0)
;;;   [3:0]   cond

(disassem:define-instruction-format
    (format-cond-branch 32
     :default-printer '(:name cond :tab imm19))
  (op1   :field (byte 7 25) :value #b0101010)
  (o1    :field (byte 1 24) :value 0)
  (imm19 :field (byte 19 5) :type 'relative-label-19)
  (o0    :field (byte 1 4)  :value 0)
  (cond  :field (byte 4 0)  :type 'condition-code))

(define-emitter emit-format-cond-branch 32
  (byte 7 25) (byte 1 24) (byte 19 5) (byte 1 4) (byte 4 0))


;;; Unconditional branch (register): BR / BLR / RET / ERET / DRPS
;;;
;;; Encoding (C6.2.37 / C6.2.36 / C6.2.186):
;;;   [31:25] 1101011
;;;   [24:21] opc
;;;   [20:16] op2     (#b11111)
;;;   [15:10] op3     (#b000000)
;;;   [9:5]   Rn
;;;   [4:0]   op4     (#b00000)

(disassem:define-instruction-format
    (branch-reg 32
     :default-printer '(:name :tab rn))
  (op1 :field (byte 7 25) :value #b1101011)
  (opc :field (byte 4 21))
  (op2 :field (byte 5 16) :value #b11111)
  (op3 :field (byte 6 10) :value #b000000)
  (rn  :field (byte 5 5)  :type 'reg)
  (op4 :field (byte 5 0)  :value #b00000))

(define-emitter emit-branch-reg 32
  (byte 7 25) (byte 4 21) (byte 5 16) (byte 6 10) (byte 5 5) (byte 5 0))


;;; Exception generation: SVC / HVC / SMC / BRK / HLT / DCPS1/2/3
;;;
;;; Encoding (C6.2.211 etc.):
;;;   [31:24] 11010100
;;;   [23:21] opc
;;;   [20:5]  imm16
;;;   [4:2]   op2     (#b000)
;;;   [1:0]   LL

(disassem:define-instruction-format
    (format-exception 32
     :default-printer '(:name :tab imm16))
  (op1   :field (byte 8 24) :value #b11010100)
  (opc   :field (byte 3 21))
  (imm16 :field (byte 16 5))
  (op2   :field (byte 3 2)  :value #b000)
  (ll    :field (byte 2 0)))

(define-emitter emit-format-exception 32
  (byte 8 24) (byte 3 21) (byte 16 5) (byte 3 2) (byte 2 0))


;;; System instructions: MSR (immediate), HINT, CLREX, DSB, DMB, ISB,
;;;                      SYS, SYSL, MSR (register), MRS
;;;
;;; These all share op1[31:22] = 1101010100.

(defconstant +sysreg-keyword-map+
  '((:fpcr      . #b1101101000100000)   ; op0=3 op1=3 CRn=4 CRm=4 op2=0
    (:fpsr      . #b1101101000100001)   ; op0=3 op1=3 CRn=4 CRm=4 op2=1
    (:nzcv      . #b1101101000010000)   ; op0=3 op1=3 CRn=4 CRm=2 op2=0
    (:tpidr-el0 . #b1101111010000010)   ; op0=3 op1=3 CRn=13 CRm=0 op2=2
    (:ctr-el0   . #b1100000000000001))) ; op0=3 op1=0 CRn=0 CRm=0 op2=1

(defun encode-sysreg (sysreg)
  "Return the 16-bit op0:op1f:CRn:CRm:op2 encoding for SYSREG.
  SYSREG may be a keyword (looked up in +sysreg-keyword-map+) or a
  raw (unsigned-byte 16) value."
  (etypecase sysreg
    ((unsigned-byte 16) sysreg)
    (keyword
     (or (cdr (assoc sysreg +sysreg-keyword-map+))
         (error "Unknown system register keyword ~S." sysreg)))))

(defun decode-sysreg (op0 op1f crn crm op2)
  "Reconstruct the packed sysreg encoding and look it up in the keyword map.
  Returns the keyword name if known, otherwise S<op0>_<op1>_C<n>_C<m>_<op2>."
  (let* ((enc (logior (ash op0 14) (ash op1f 11) (ash crn 7) (ash crm 3) op2))
         (entry (rassoc enc +sysreg-keyword-map+)))
    (if entry
        (symbol-name (car entry))
        (format nil "S~D_~D_C~D_C~D_~D" op0 op1f crn crm op2))))

(disassem:define-instruction-format
    (format-system 32
     :default-printer '(:name :tab rt))
  (op1    :field (byte 10 22) :value #b1101010100)
  (l      :field (byte 1 21))     ; 0=write, 1=read
  (sysreg :field (byte 15 5)
          :printer #'(lambda (value stream dstate)
                       (declare (ignore dstate))
                       (let* ((op0  (ldb (byte 2 13) value))
                              (op1f (ldb (byte 3 10) value))
                              (crn  (ldb (byte 4  6) value))
                              (crm  (ldb (byte 4  2) value))
                              (op2  (ldb (byte 3  0) value)))
                         (write-string (decode-sysreg op0 op1f crn crm op2)
                                       stream))))
  (rt     :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-system 32
  (byte 10 22) (byte 1 21) (byte 2 19) (byte 3 16) (byte 4 12)
  (byte 4 8) (byte 3 5) (byte 5 0))


;;;; -----------------------------------------------------------------------
;;;; Loads and Stores  (op1 = x1x0)
;;;;
;;;; Section C4.1.4 of the ARM ARM.
;;;; -----------------------------------------------------------------------

;;; Load/Store register (unsigned offset)
;;;
;;; Encoding (C6.2.100 etc.):
;;;   [31:30] size
;;;   [29:27] 111
;;;   [26]    V       (0=integer, 1=FP/SIMD)
;;;   [25:24] 01
;;;   [23:22] opc
;;;   [21:10] imm12   (unsigned, scaled by access size)
;;;   [9:5]   Rn
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-ldst-uoffset 32
     :default-printer '(:name :tab rt ", [" rn ", #" imm12 "]"))
  (size  :field (byte 2 30))
  (op1   :field (byte 3 27))
  (v     :field (byte 1 26))
  (op2   :field (byte 2 24))
  (opc   :field (byte 2 22))
  (imm12 :field (byte 12 10))
  (rn    :field (byte 5 5)  :type 'reg)
  (rt    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-ldst-uoffset 32
  (byte 2 30) (byte 3 27) (byte 1 26) (byte 2 24) (byte 2 22)
  (byte 12 10) (byte 5 5) (byte 5 0))


;;; Load/Store register (unscaled immediate / pre-index / post-index)
;;;
;;; Encoding (C6.2.101 / C6.2.97 / C6.2.99):
;;;   [31:30] size
;;;   [29:27] 111
;;;   [26]    V
;;;   [25:24] 00
;;;   [23:22] opc
;;;   [21]    (0)
;;;   [20:12] imm9    (signed 9-bit)
;;;   [11:10] type    (00=unscaled, 01=post-index, 10=unprivileged, 11=pre-index)
;;;   [9:5]   Rn
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-ldst-imm9 32
     :default-printer '(:name :tab rt
                        (:cond ((type :constant #b01)
                                ", [" rn "], #" imm9)
                               ((type :constant #b11)
                                ", [" rn ", #" imm9 "]!")
                               (t
                                ", [" rn ", #" imm9 "]"))))
  (size  :field (byte 2 30))
  (op1   :field (byte 3 27))
  (v     :field (byte 1 26))
  (op2   :field (byte 2 24))
  (opc   :field (byte 2 22))
  (z     :field (byte 1 21))
  (imm9  :field (byte 9 12) :sign-extend t)
  (type  :field (byte 2 10))
  (rn    :field (byte 5 5)  :type 'reg)
  (rt    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-ldst-imm9 32
  (byte 2 30) (byte 3 27) (byte 1 26) (byte 2 24) (byte 2 22)
  (byte 1 21) (byte 9 12) (byte 2 10) (byte 5 5) (byte 5 0))


;;; Load/Store register (register offset)
;;;
;;; Encoding (C6.2.102):
;;;   [31:30] size
;;;   [29:27] 111
;;;   [26]    V
;;;   [25:24] 00
;;;   [23:22] opc
;;;   [21]    1
;;;   [20:16] Rm
;;;   [15:13] option  (extend type)
;;;   [12]    S       (shift amount: 0 or log2(access-size))
;;;   [11:10] 10
;;;   [9:5]   Rn
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-ldst-reg 32
     :default-printer '(:name :tab rt ", [" rn ", " rm
                        (:unless (option :constant #b011)
                          ", " option)
                        (:unless (s :constant 0)
                          " lsl")
                        "]"))
  (size   :field (byte 2 30))
  (op1    :field (byte 3 27))
  (v      :field (byte 1 26))
  (op2    :field (byte 2 24))
  (opc    :field (byte 2 22))
  (one    :field (byte 1 21) :value 1)
  (rm     :field (byte 5 16)
          :printer #'(lambda (value stream dstate)
                       (let* ((word   (disassem::sap-ref-int
                                       (disassem:dstate-segment-sap dstate)
                                       (disassem:dstate-cur-offs dstate)
                                       4 :little-endian))
                              (option (ldb (byte 3 13) word)))
                         (princ (get-reg-name value (if (< option 6) 0 1))
                                stream))))
  (option :field (byte 3 13) :type 'extend-type)
  (s      :field (byte 1 12))
  (op3    :field (byte 2 10) :value #b10)
  (rn     :field (byte 5 5)  :type 'reg)
  (rt     :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-ldst-reg 32
  (byte 2 30) (byte 3 27) (byte 1 26) (byte 2 24) (byte 2 22)
  (byte 1 21) (byte 5 16) (byte 3 13) (byte 1 12) (byte 2 10)
  (byte 5 5) (byte 5 0))


;;; Load/Store pair (offset / pre-index / post-index)
;;;
;;; Encoding (C6.2.103 / C6.2.105 / C6.2.104):
;;;   [31:30] opc
;;;   [29:27] 101
;;;   [26]    Vr      (0=integer, 1=SIMD/FP)
;;;   [25]    0       (always 0; disambiguates from DP-reg where bit 25=1)
;;;   [24:23] index   (01=post-index, 10=offset, 11=pre-index)
;;;   [22]    L       (0=store, 1=load)
;;;   [21:15] imm7    (signed, scaled by access size)
;;;   [14:10] Rt2
;;;   [9:5]   Rn
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-ldst-pair 32
     :default-printer '(:name :tab rt ", " rt2
                        (:cond ((index :constant #b01)
                                ", [" rn "], #" imm7)
                               ((index :constant #b11)
                                ", [" rn ", #" imm7 "]!")
                               (t
                                ", [" rn ", #" imm7 "]"))))
  (opc   :field (byte 2 30))
  (op1   :field (byte 3 27) :value #b101)
  (vr    :field (byte 1 26))
  (op2   :field (byte 1 25) :value 0)
  (index :field (byte 2 23))
  (l     :field (byte 1 22))
  (imm7  :field (byte 7 15) :sign-extend t)
  (rt2   :field (byte 5 10) :type 'reg)
  (rn    :field (byte 5 5)  :type 'reg)
  (rt    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-ldst-pair 32
  (byte 2 30) (byte 3 27) (byte 1 26) (byte 1 25) (byte 2 23) (byte 1 22)
  (byte 7 15) (byte 5 10) (byte 5 5) (byte 5 0))


;;; Load register (literal)
;;;
;;; Encoding (C6.2.91):
;;;   [31:30] opc
;;;   [29:27] 011
;;;   [26]    V
;;;   [25:24] 00
;;;   [23:5]  imm19
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-ldr-literal 32
     :default-printer '(:name :tab rt ", " imm19))
  (opc   :field (byte 2 30))
  (op1   :field (byte 3 27) :value #b011)
  (v     :field (byte 1 26))
  (op2   :field (byte 2 24) :value #b00)
  (imm19 :field (byte 19 5) :type 'relative-label-19)
  (rt    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-ldr-literal 32
  (byte 2 30) (byte 3 27) (byte 1 26) (byte 2 24) (byte 19 5) (byte 5 0))


;;; Load/Store exclusive
;;;
;;; Encoding (C6.2.92 etc.):
;;;   [31:30] size
;;;   [29:24] 001000
;;;   [23]    o2
;;;   [22]    L
;;;   [21]    o1
;;;   [20:16] Rs
;;;   [15]    o0
;;;   [14:10] Rt2
;;;   [9:5]   Rn
;;;   [4:0]   Rt

(disassem:define-instruction-format
    (format-ldst-exclusive 32
     :default-printer '(:name :tab rt ", [" rn "]"))
  (size :field (byte 2 30))
  (op1  :field (byte 6 24) :value #b001000)
  (o2   :field (byte 1 23))
  (l    :field (byte 1 22))
  (o1   :field (byte 1 21))
  (rs   :field (byte 5 16) :type 'reg)
  (o0   :field (byte 1 15))
  (rt2  :field (byte 5 10) :type 'reg)
  (rn   :field (byte 5 5)  :type 'reg)
  (rt   :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-ldst-exclusive 32
  (byte 2 30) (byte 6 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 5 16) (byte 1 15) (byte 5 10) (byte 5 5) (byte 5 0))


;;;; -----------------------------------------------------------------------
;;;; Data Processing -- Register  (op1 = x101)
;;;;
;;;; Section C4.1.5 of the ARM ARM.
;;;; -----------------------------------------------------------------------

;;; Shifted-register operand for logical and add/sub (shifted register)
;;; instructions.  Use SHIFT to construct one.

(defstruct shifted-reg
  ;; The register to be shifted.
  reg
  ;; Shift type: :lsl :lsr :asr :ror
  shift-type
  ;; Shift amount (0-63)
  shift-amount)

(defun shift (reg shift-type &optional (amount 0))
  "Construct a shifted-register operand.
  Reg is the register to shift.  Shift-type is one of :lsl :lsr :asr :ror.
  Amount is the immediate shift count (0-63), defaulting to 0."
  (declare (type tn reg)
           (type shift-type shift-type)
           (type (unsigned-byte 6) amount))
  (make-shifted-reg :reg reg :shift-type shift-type :shift-amount amount))

(defstruct shifted-imm
  ;; The 12-bit unsigned immediate value.
  (value 0 :type (unsigned-byte 12))
  ;; Shift: 0 = LSL #0, 1 = LSL #12.
  (shift 0 :type (integer 0 1)))

(defun imm (value &optional (shift 0))
  "Construct a shifted immediate operand for add/sub instructions.
  Value is an unsigned 12-bit immediate.  Shift is 0 (LSL #0, default)
  or 12 (LSL #12)."
  (declare (type (unsigned-byte 12) value)
           (type (member 0 12) shift))
  (make-shifted-imm :value value :shift (/ shift 12)))

(defstruct bitmask-imm
  ;; The three encoding fields for an AArch64 logical immediate.
  (n    0 :type (unsigned-byte 1))
  (immr 0 :type (unsigned-byte 6))
  (imms 0 :type (unsigned-byte 6)))

;; Bitmask immediate encoding algorithm.
;; References:
;;   https://kddnewton.com/2022/08/11/aarch64-bitmask-immediates.html
;;   https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/AArch64/MCTargetDesc/AArch64AddressingModes.h
;;
;; Examples (64-bit):
;;   (encode-bit-mask #x00000000000000ff 1) => N=1, immr=56, imms=7
;;     -- low 8 bits set
;;   (encode-bit-mask #x0101010101010101 1) => N=0, immr=0,  imms=0
;;     -- every 8th bit (esize=8, 1 one)
;;   (encode-bit-mask #xaaaaaaaaaaaaaaaa 1) => N=0, immr=1,  imms=0
;;     -- alternating bits (esize=2, rotated by 1)
;;   (encode-bit-mask -1 1) => NIL
;;     -- all-ones not encodable
;;   (encode-bit-mask -256 1) => N=1, immr=8, imms=55
;;     -- #xffffffffffffff00: 56 ones rotated by 8
(defun encode-bit-mask (value sf)
  "Attempt to encode VALUE as an AArch64 bitmask immediate for a
  register of size 2^(SF+5) bits (SF=0 => 32-bit, SF=1 => 64-bit).
  Returns (values N IMMR IMMS) on success, or NIL on failure.
  Negative values are treated as their two's complement bit pattern."
  (declare (type (or (unsigned-byte 64) (signed-byte 64)) value)
           (type bit sf))
  (let* ((reg-size (if (zerop sf) 32 64))
         (imm      (ldb (byte reg-size 0) value))   ; normalize to unsigned
         (all-ones (ldb (byte reg-size 0) -1)))
    ;; All-zeros and all-ones cannot be encoded.
    (when (or (zerop imm) (= imm all-ones))
      (return-from encode-bit-mask nil))
    (labels (;; Count trailing zeros -- undefined for n=0.
             (ctz (n)
               (1- (integer-length (logand n (- n)))))
             ;; Count trailing ones.
             (cto (n)
               (1- (integer-length (logand (lognot n) (- (lognot n))))))
             ;; Count leading ones in a w-bit value.
             (clo (n w)
               (- w (integer-length (logand (lognot n) (1- (ash 1 w))))))
             ;; True if n is a contiguous run of 1s at bit 0, e.g. #b0001111.
             (is-mask (n)
               (and (not (zerop n))
                    (zerop (logand n (1+ n)))))
             ;; True if n is a contiguous run of 1s anywhere, e.g. #b0111100.
             ;; Fills trailing zeros to get a mask, then checks it's a pure mask.
             (is-shifted-mask (n)
               (and (not (zerop n))
                    (is-mask (logior n (1- n))))))
      ;; Find element size by halving: if upper and lower halves match,
      ;; the pattern tiles and we can use a smaller element size.
      (let ((size reg-size))
        (loop
          (let* ((half (ash size -1))
                 (mask (1- (ash 1 half))))
            (when (/= (logand imm mask)
                      (logand (ash imm (- half)) mask))
              (return))
            (setf size half))
          (when (<= size 2) (return)))
        ;; Isolate the pattern to SIZE bits.
        (let* ((mask (ldb (byte size 0) -1))
               (imm  (logand imm mask))
               left-rotations trailing-ones)
          (if (is-shifted-mask imm)
            ;; Unrotated or simple left-shift case.
            (let ((tz (ctz imm)))
              (setf left-rotations tz
                    trailing-ones  (cto (ash imm (- tz)))))
            ;; Split pattern wrapping around (e.g. #b1001): invert within
            ;; size bits and verify that gives a valid shifted mask.
            (let ((inv (logand (lognot imm) mask)))
              (unless (is-shifted-mask inv)
                (return-from encode-bit-mask nil))
              (let ((leading-ones (clo imm size)))
                (setf left-rotations (- size leading-ones)
                      trailing-ones  (+ leading-ones (cto imm))))))
          ;; immr = right rotations = (size - left-rotations) mod size
          (let* ((immr (logand (- size left-rotations) (1- size)))
                 ;; imms = NOT(size-1) shifted left 1, OR ones-1.
                 ;; The left shift places size-encoding bits correctly
                 ;; within the 6-bit imms field.
                 (imms (logior (logand (ash (lognot (1- size)) 1) #x3f)
                               (1- trailing-ones)))
                 ;; N = 1 iff size = 64 (bit 6 of pre-masked imms is 0).
                 (n    (logxor (logand (ash imms -6) 1) 1)))
            (values n
                    (logand immr #x3f)
                    (logand imms #x3f))))))))


(defun mask (value &optional (sf 1))
  "Construct a bitmask immediate operand for logical instructions.
  Value is the integer to encode; SF selects 64-bit (1, default) or 32-bit (0).
  Signals an error if value cannot be encoded as an AArch64 bitmask immediate."
  (declare (type (or (signed-byte 64) (unsigned-byte 64)) value)
           (type bit sf))
  (multiple-value-bind (n immr imms)
      (encode-bit-mask value sf)
    (unless n
      (error "Cannot encode ~S as an AArch64 bitmask immediate." value))
    (make-bitmask-imm :n n :immr immr :imms imms)))

(defstruct extended-reg
  ;; The register to be extended.
  reg
  ;; Extension type: :uxtb :uxth :uxtw :uxtx :sxtb :sxth :sxtw :sxtx
  extend-type
  ;; Optional left shift applied after extension (0-4).
  (shift 0 :type (integer 0 4))
  ;; T if rm is a W register (derived from extend-type).
  (word-reg nil :type boolean))

(defun extend (reg extend-type &optional (shift 0))
  "Construct an extended-register operand for add/sub instructions.
  Reg is the register to extend.  Extend-type is one of:
    :uxtb :uxth :uxtw :uxtx  -- zero-extend byte/halfword/word/doubleword
    :sxtb :sxth :sxtw :sxtx  -- sign-extend byte/halfword/word/doubleword
  Shift is an optional left shift of 0-4 applied after extension.
  The word-reg slot is derived automatically: W register for uxtb/uxth/uxtw/sxtb/sxth/sxtw,
  X register for uxtx/sxtx."
  (declare (type tn reg)
           (type (member :uxtb :uxth :uxtw :uxtx :sxtb :sxth :sxtw :sxtx) extend-type)
           (type (integer 0 4) shift))
  (make-extended-reg :reg reg :extend-type extend-type :shift shift
                     :word-reg (not (null (member extend-type
                                                   '(:uxtb :uxth :uxtw
                                                     :sxtb :sxth :sxtw))))))

(defstruct memory-ref
  ;; Base register.
  (base (required-argument) :type tn)
  ;; Byte offset -- semantics depend on mode:
  ;;   :offset     unsigned, scaled by access size at emit time
  ;;   :pre        signed 9-bit, stored unscaled in imm9
  ;;   :post       signed 9-bit, stored unscaled in imm9
  ;;   :reg-offset register offset via extended-reg
  (offset 0 :type integer)
  ;; Register offset operand (only for :reg-offset mode).
  (rm nil :type (or null extended-reg))
  ;; Addressing mode.
  (mode :offset :type (member :offset :pre :post :reg-offset)))

(defun mem (base &optional (offset 0))
  "Unsigned-offset memory reference: [base, #offset].
  Offset is a byte offset; must be a non-negative multiple of the
  access size of the instruction it is used with."
  (declare (type tn base) (type (integer 0) offset))
  (make-memory-ref :base base :offset offset :mode :offset))

(defun pre-index (base offset)
  "Pre-index memory reference: [base, #offset]!
  Offset is a signed byte offset in the range -256..255."
  (declare (type tn base) (type (signed-byte 9) offset))
  (make-memory-ref :base base :offset offset :mode :pre))

(defun post-index (base offset)
  "Post-index memory reference: [base], #offset
  Offset is a signed byte offset in the range -256..255."
  (declare (type tn base) (type (signed-byte 9) offset))
  (make-memory-ref :base base :offset offset :mode :post))

(defun reg-offset (base rm &optional (extend-type :uxtx) (shift 0))
  "Register-offset memory reference: [base, rm, extend #shift]
  Rm is the offset register, extended and optionally shifted.
  Extend-type defaults to :uxtx (no extension, X register).
  Use :uxtw or :sxtw to treat rm as a W register.
  Shift is 0 or the log2 of the access size."
  (declare (type tn base rm)
           (type (member :uxtb :uxth :uxtw :uxtx :sxtb :sxth :sxtw :sxtx) extend-type)
           (type (integer 0 4) shift))
  (make-memory-ref :base base :mode :reg-offset
                   :rm (extend rm extend-type shift)))

(defun nzcv (&rest flags)
  "Return a 4-bit integer encoding the NZCV flags, suitable for use as
  the nzcv argument to CCMP/CCMN.  Also usable with MSR NZCV after
  shifting left by 28.  Valid flags are :n (bit 3), :z (bit 2),
  :c (bit 1), :v (bit 0).

  Examples:
    (nzcv :z :c)       ; #b0110 = 6
    (nzcv :n :z :c :v) ; #b1111 = 15
    (nzcv)             ; #b0000 = 0"
  (loop for flag in flags
        sum (ecase flag
              (:n 8)
              (:z 4)
              (:c 2)
              (:v 1))))

;;; Logical (shifted register)
;;;
;;; Encoding (C6.2.113 etc.):
;;;   [31]    sf
;;;   [30:29] opc     (00=AND, 01=ORR, 10=EOR, 11=ANDS)
;;;   [28:24] 01010
;;;   [23:22] shift
;;;   [21]    N       (0=normal, 1=invert Rm)
;;;   [20:16] Rm
;;;   [15:10] imm6
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-logic-reg 32
     :default-printer '(:name :tab rd ", " rn ", " rm
                        (:unless (:and (shift :constant 0) (imm6 :constant 0))
                          ", " shift " #" imm6)))
  (sf   :field (byte 1 31))
  (opc  :field (byte 2 29))
  (op1  :field (byte 5 24))
  (shift :field (byte 2 22) :type 'shift-type)
  (n    :field (byte 1 21))
  (rm   :field (byte 5 16) :type 'reg)
  (imm6 :field (byte 6 10))
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-logic-reg 32
  (byte 1 31) (byte 2 29) (byte 5 24) (byte 2 22) (byte 1 21)
  (byte 5 16) (byte 6 10) (byte 5 5) (byte 5 0))


;;; Add/Sub (shifted register)
;;;
;;; Encoding (C6.2.5 / C6.2.197):
;;;   [31]    sf
;;;   [30]    op
;;;   [29]    S
;;;   [28:24] 01011
;;;   [23:22] shift
;;;   [21]    0
;;;   [20:16] Rm
;;;   [15:10] imm6
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-add-sub-shift 32
     :default-printer '(:name :tab rd ", " rn ", " rm
                        (:unless (:and (shift :constant 0) (imm6 :constant 0))
                          ", " shift " #" imm6)))
  (sf    :field (byte 1 31))
  (op    :field (byte 1 30))
  (s     :field (byte 1 29))
  (op1   :field (byte 5 24))
  (shift :field (byte 2 22) :type 'shift-type)
  (zero  :field (byte 1 21) :value 0)
  (rm    :field (byte 5 16) :type 'reg)
  (imm6  :field (byte 6 10))
  (rn    :field (byte 5 5)  :type 'reg)
  (rd    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-add-sub-shift 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 2 22) (byte 1 21)
  (byte 5 16) (byte 6 10) (byte 5 5) (byte 5 0))


;;; Add/Sub (extended register)
;;;
;;; Encoding (C6.2.3 / C6.2.195):
;;;   [31]    sf
;;;   [30]    op
;;;   [29]    S
;;;   [28:24] 01011
;;;   [23:22] opt     (00)
;;;   [21]    1
;;;   [20:16] Rm
;;;   [15:13] option  (extend type)
;;;   [12:10] imm3    (shift amount 0-4)
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-add-sub-ext 32
     :default-printer '(:name :tab rd ", " rn ", " rm ", " option
                        (:unless (imm3 :constant 0)
                          " #" imm3)))
  (sf     :field (byte 1 31))
  (op     :field (byte 1 30))
  (s      :field (byte 1 29))
  (op1    :field (byte 5 24))
  (opt    :field (byte 2 22) :value 0)
  (one    :field (byte 1 21) :value 1)
  ;; rm prints as W register for uxtb/uxth/uxtw/sxtb/sxth/sxtw (option < 6),
  ;; X register for uxtx/sxtx (option >= 6).
  (rm     :field (byte 5 16)
          :printer #'(lambda (value stream dstate)
                       (let* ((word   (disassem::sap-ref-int
                                       (disassem:dstate-segment-sap dstate)
                                       (disassem:dstate-cur-offs dstate)
                                       4 :little-endian))
                              (option (ldb (byte 3 13) word)))
                         (princ (get-reg-name value (if (< option 6) 0 1))
                                stream))))
  (option :field (byte 3 13) :type 'extend-type)
  (imm3   :field (byte 3 10))
  (rn     :field (byte 5 5)  :type 'reg)
  (rd     :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-add-sub-ext 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 2 22) (byte 1 21)
  (byte 5 16) (byte 3 13) (byte 3 10) (byte 5 5) (byte 5 0))


;;; Data processing (2 sources): UDIV, SDIV, LSLV, LSRV, ASRV, RORV,
;;;                               CRC32*, PACGA, SUBP
;;;
;;; Encoding (C6.2.64 etc.):
;;;   [31]    sf
;;;   [30]    0
;;;   [29]    S       (0 for most; not used)
;;;   [28:21] 11010110
;;;   [20:16] Rm
;;;   [15:10] opcode
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-dp-2src 32
     :default-printer '(:name :tab rd ", " rn ", " rm))
  (sf    :field (byte 1 31))
  (zero  :field (byte 1 30) :value 0)
  (s     :field (byte 1 29) :value 0)
  (op1   :field (byte 8 21) :value #b11010110)
  (rm    :field (byte 5 16) :type 'reg)
  (opc   :field (byte 6 10))
  (rn    :field (byte 5 5)  :type 'reg)
  (rd    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-dp-2src 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 8 21) (byte 5 16)
  (byte 6 10) (byte 5 5) (byte 5 0))


;;; Data processing (1 source): RBIT, REV16, REV, CLZ, CLS, REV32,
;;;                              PACIZA, PACDZA, AUTIZA, AUTDZA, XPACI, XPACD
;;;
;;; Encoding (C6.2.62):
;;;   [31]    sf
;;;   [30]    1
;;;   [29]    S       (0)
;;;   [28:21] 11010110
;;;   [20:16] opcode2 (#b00000)
;;;   [15:10] opcode
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-dp-1src 32
     :default-printer '(:name :tab rd ", " rn))
  (sf     :field (byte 1 31))
  (one    :field (byte 1 30) :value 1)
  (s      :field (byte 1 29) :value 0)
  (op1    :field (byte 8 21) :value #b11010110)
  (opcode2 :field (byte 5 16) :value 0)
  (opc    :field (byte 6 10))
  (rn     :field (byte 5 5)  :type 'reg)
  (rd     :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-dp-1src 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 8 21) (byte 5 16)
  (byte 6 10) (byte 5 5) (byte 5 0))


;;; Add/Sub with carry: ADC / ADCS / SBC / SBCS
;;;
;;; Encoding (C6.2.2 / C6.2.194):
;;;   [31]    sf
;;;   [30]    op
;;;   [29]    S
;;;   [28:21] 11010000
;;;   [20:16] Rm
;;;   [15:10] 000000
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-adc 32
     :default-printer '(:name :tab rd ", " rn ", " rm))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (s    :field (byte 1 29))
  (op1  :field (byte 8 21) :value #b11010000)
  (rm   :field (byte 5 16) :type 'reg)
  (zero :field (byte 6 10) :value 0)
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-adc 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 8 21) (byte 5 16)
  (byte 6 10) (byte 5 5) (byte 5 0))


;;; Rotate right into flags / Evaluate into flags (ARMv8.4)
;;; Conditional compare (register / immediate): CCMN / CCMP
;;;
;;; Encoding (C6.2.48 / C6.2.49):
;;;   [31]    sf
;;;   [30]    op
;;;   [29]    1
;;;   [28:21] 11010010  (imm) or 11010010 (reg)
;;;   [20:16] Rm or imm5
;;;   [15:12] cond
;;;   [11:10] 10 (imm) or 00 (reg)
;;;   [9:5]   Rn
;;;   [4]     o3      (0)
;;;   [3:0]   nzcv

(disassem:define-instruction-format
    (format-cond-cmp-imm 32
     :default-printer '(:name :tab rn ", #" imm5 ", #" nzcv ", " cond))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (one  :field (byte 1 29) :value 1)
  (op1  :field (byte 8 21) :value #b11010010)
  (imm5 :field (byte 5 16))
  (cond :field (byte 4 12) :type 'condition-code)
  (op2  :field (byte 2 10) :value #b10)
  (rn   :field (byte 5 5)  :type 'reg)
  (o3   :field (byte 1 4)  :value 0)
  (nzcv :field (byte 4 0)))

(define-emitter emit-format-cond-cmp-imm 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 8 21) (byte 5 16)
  (byte 4 12) (byte 2 10) (byte 5 5) (byte 1 4) (byte 4 0))

(disassem:define-instruction-format
    (format-cond-cmp-reg 32
     :default-printer '(:name :tab rn ", " rm ", #" nzcv ", " cond))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (one  :field (byte 1 29) :value 1)
  (op1  :field (byte 8 21) :value #b11010010)
  (rm   :field (byte 5 16) :type 'reg)
  (cond :field (byte 4 12) :type 'condition-code)
  (op2  :field (byte 2 10) :value #b00)
  (rn   :field (byte 5 5)  :type 'reg)
  (o3   :field (byte 1 4)  :value 0)
  (nzcv :field (byte 4 0)))

(define-emitter emit-format-cond-cmp-reg 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 8 21) (byte 5 16)
  (byte 4 12) (byte 2 10) (byte 5 5) (byte 1 4) (byte 4 0))


;;; Conditional select: CSEL / CSINC / CSINV / CSNEG
;;;
;;; Encoding (C6.2.56 etc.):
;;;   [31]    sf
;;;   [30]    op
;;;   [29]    S       (0)
;;;   [28:21] 11010100
;;;   [20:16] Rm
;;;   [15:12] cond
;;;   [11:10] op2
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-cond-select 32
     :default-printer '(:name :tab rd ", " rn ", " rm ", " cond))
  (sf   :field (byte 1 31))
  (op   :field (byte 1 30))
  (s    :field (byte 1 29) :value 0)
  (op1  :field (byte 8 21) :value #b11010100)
  (rm   :field (byte 5 16) :type 'reg)
  (cond :field (byte 4 12) :type 'condition-code)
  (op2  :field (byte 2 10))
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-cond-select 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 8 21) (byte 5 16)
  (byte 4 12) (byte 2 10) (byte 5 5) (byte 5 0))


;;; Data processing (3 sources): MADD / MSUB / SMADDL / SMSUBL / SMULH /
;;;                               UMADDL / UMSUBL / UMULH
;;;
;;; Encoding (C6.2.120 etc.):
;;;   [31]    sf
;;;   [30:29] op54
;;;   [28:24] 11011
;;;   [23:21] op31
;;;   [20:16] Rm
;;;   [15]    o0
;;;   [14:10] Ra
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-dp-3src 32
     :default-printer '(:name :tab rd ", " rn ", " rm ", " ra))
  (sf   :field (byte 1 31))
  (op54 :field (byte 2 29))
  (op1  :field (byte 5 24))
  (op31 :field (byte 3 21))
  (rm   :field (byte 5 16) :type 'reg)
  (o0   :field (byte 1 15))
  (ra   :field (byte 5 10) :type 'reg)
  (rn   :field (byte 5 5)  :type 'reg)
  (rd   :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-dp-3src 32
  (byte 1 31) (byte 2 29) (byte 5 24) (byte 3 21) (byte 5 16)
  (byte 1 15) (byte 5 10) (byte 5 5) (byte 5 0))


;;;; -----------------------------------------------------------------------
;;;; Data Processing -- Scalar Floating-Point and Advanced SIMD  (op1 = x111)
;;;;
;;;; Section C4.1.6 of the ARM ARM.
;;;; Only the scalar FP instructions (V=0 or SIMD=0 path) are described
;;;; here in detail; Advanced SIMD vector formats follow the same pattern.
;;;; -----------------------------------------------------------------------

;;; Floating-point data-processing (1 source): FMOV / FABS / FNEG / FSQRT /
;;;   FCVT / FRINTN / FRINTP / FRINTM / FRINTZ / FRINTA / FRINTX / FRINTI
;;;
;;; Encoding (C6.2.79 etc.):
;;;   [31]    M       (0)
;;;   [30]    0
;;;   [29]    S       (0)
;;;   [28:24] 11110
;;;   [23:22] type    (00=S, 01=D, 11=H)
;;;   [21]    1
;;;   [20:15] opcode
;;;   [14:10] 10000
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-fp-dp1 32
     :default-printer '(:name :tab rd ", " rn))
  (m    :field (byte 1 31) :value 0)
  (zero :field (byte 1 30) :value 0)
  (s    :field (byte 1 29) :value 0)
  (op1  :field (byte 5 24))
  (type :field (byte 2 22))
  (one  :field (byte 1 21) :value 1)
  (opc  :field (byte 6 15))
  (op2  :field (byte 5 10) :value #b10000)
  (rn   :field (byte 5 5)  :type 'fp-reg-single)
  (rd   :field (byte 5 0)  :type 'fp-reg-single))

(define-emitter emit-format-fp-dp1 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 2 22)
  (byte 1 21) (byte 6 15) (byte 5 10) (byte 5 5) (byte 5 0))


;;; Floating-point data-processing (2 sources): FMUL / FDIV / FADD / FSUB /
;;;   FMAX / FMIN / FMAXNM / FMINNM / FNMUL
;;;
;;; Encoding (C6.2.80):
;;;   [31]    M       (0)
;;;   [30]    0
;;;   [29]    S       (0)
;;;   [28:24] 11110
;;;   [23:22] type
;;;   [21]    1
;;;   [20:16] Rm
;;;   [15:12] opcode
;;;   [11:10] 10
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-fp-dp2 32
     :default-printer '(:name :tab rd ", " rn ", " rm))
  (m    :field (byte 1 31) :value 0)
  (zero :field (byte 1 30) :value 0)
  (s    :field (byte 1 29) :value 0)
  (op1  :field (byte 5 24))
  (type :field (byte 2 22))
  (one  :field (byte 1 21) :value 1)
  (rm   :field (byte 5 16) :type 'fp-reg-single)
  (opc  :field (byte 4 12))
  (op2  :field (byte 2 10) :value #b10)
  (rn   :field (byte 5 5)  :type 'fp-reg-single)
  (rd   :field (byte 5 0)  :type 'fp-reg-single))

(define-emitter emit-format-fp-dp2 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 2 22)
  (byte 1 21) (byte 5 16) (byte 4 12) (byte 2 10) (byte 5 5) (byte 5 0))


;;; Floating-point data-processing (3 sources): FMADD / FMSUB / FNMADD / FNMSUB
;;;
;;; Encoding (C6.2.81):
;;;   [31]    M       (0)
;;;   [30]    0
;;;   [29]    S       (0)
;;;   [28:24] 11111
;;;   [23:22] type
;;;   [21]    o1
;;;   [20:16] Rm
;;;   [15]    o0
;;;   [14:10] Ra
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-fp-dp3 32
     :default-printer '(:name :tab rd ", " rn ", " rm ", " ra))
  (m    :field (byte 1 31) :value 0)
  (zero :field (byte 1 30) :value 0)
  (s    :field (byte 1 29) :value 0)
  (op1  :field (byte 5 24))
  (type :field (byte 2 22))
  (o1   :field (byte 1 21))
  (rm   :field (byte 5 16) :type 'fp-reg-single)
  (o0   :field (byte 1 15))
  (ra   :field (byte 5 10) :type 'fp-reg-single)
  (rn   :field (byte 5 5)  :type 'fp-reg-single)
  (rd   :field (byte 5 0)  :type 'fp-reg-single))

(define-emitter emit-format-fp-dp3 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 2 22)
  (byte 1 21) (byte 5 16) (byte 1 15) (byte 5 10) (byte 5 5) (byte 5 0))


;;; Floating-point move immediate: FMOV Sd/Dd, #imm
;;;
;;; Encoding (C6.2.120):
;;;   [31]    M=0
;;;   [30:29] 00
;;;   [28:24] 11110
;;;   [23:22] type    (00=single, 01=double)
;;;   [21]    1
;;;   [20:13] imm8
;;;   [12:10] 100
;;;   [9:5]   00000
;;;   [4:0]   Rd

(eval-when (compile load eval)
(defun decode-fp-immediate (imm8)
  "Decode an 8-bit VFP immediate back to a single-float."
  (let* ((sign  (ldb (byte 1 7) imm8))
         (exp3  (logior (ash (logxor (ldb (byte 1 6) imm8) 1) 2)
                        (ldb (byte 2 4) imm8)))
         (mant4 (ldb (byte 4 0) imm8))
         (uexp  (- exp3 3))
         (bits  (logior (ash sign 31)
                        (ash (+ uexp 127) 23)
                        (ash mant4 19))))
    (make-single-float bits)))
) ; eval-when

(disassem:define-instruction-format
    (format-fp-imm 32
     :default-printer '(:name :tab rd ", #" imm8))
  (m    :field (byte 1 31) :value 0)
  (op0  :field (byte 2 29) :value 0)
  (op1  :field (byte 5 24))
  (type :field (byte 2 22))
  (one  :field (byte 1 21) :value 1)
  (imm8 :field (byte 8 13)
        :printer #'(lambda (value stream dstate)
                     (declare (ignore dstate))
                     (format stream "#~S" (decode-fp-immediate value))))
  (op2  :field (byte 3 10) :value #b100)
  (op3  :field (byte 5 5)  :value 0)
  (rd   :field (byte 5 0)  :type 'fp-reg-single))

(define-emitter emit-format-fp-imm 32
  (byte 1 31) (byte 2 29) (byte 5 24) (byte 2 22) (byte 1 21)
  (byte 8 13) (byte 3 10) (byte 5 5) (byte 5 0))



;;;
;;; Encoding (C6.2.75):
;;;   [31]    M / 0
;;;   [30:29] 00
;;;   [28:24] 11110
;;;   [23:22] type
;;;   [21]    1
;;;   [20:16] Rm  (or 00000 for zero-compare)
;;;   [15:14] op
;;;   [13:10] 1000
;;;   [9:5]   Rn
;;;   [4]     opc2[4]
;;;   [3:0]   opc2[3:0]

(disassem:define-instruction-format
    (format-fp-cmp 32
     :default-printer '(:name :tab rn ", "
                        (:cond ((opc2 :constant #b01000) "#0.0")
                               ((opc2 :constant #b11000) "#0.0")
                               (t rm))))
  (m    :field (byte 1 31) :value 0)
  (op0  :field (byte 2 29) :value 0)
  (op1  :field (byte 5 24))
  (type :field (byte 2 22))
  (one  :field (byte 1 21) :value 1)
  (rm   :field (byte 5 16) :type 'fp-reg-single)
  (op   :field (byte 2 14))
  (op2  :field (byte 4 10) :value #b1000)
  (rn   :field (byte 5 5)  :type 'fp-reg-single)
  (opc2 :field (byte 5 0)))

(define-emitter emit-format-fp-cmp 32
  (byte 1 31) (byte 2 29) (byte 5 24) (byte 2 22) (byte 1 21)
  (byte 5 16) (byte 2 14) (byte 4 10) (byte 5 5) (byte 5 0))


;;; Floating-point conditional select: FCSEL
;;;
;;; Encoding (C6.2.77):
;;;   [31:30] 00
;;;   [29]    S       (0)
;;;   [28:24] 11110
;;;   [23:22] type
;;;   [21]    1
;;;   [20:16] Rm
;;;   [15:12] cond
;;;   [11:10] 11
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-fp-csel 32
     :default-printer '(:name :tab rd ", " rn ", " rm ", " cond))
  (op0  :field (byte 2 30) :value 0)
  (s    :field (byte 1 29) :value 0)
  (op1  :field (byte 5 24))
  (type :field (byte 2 22))
  (one  :field (byte 1 21) :value 1)
  (rm   :field (byte 5 16) :type 'fp-reg-single)
  (cond :field (byte 4 12) :type 'condition-code)
  (op2  :field (byte 2 10) :value #b11)
  (rn   :field (byte 5 5)  :type 'fp-reg-single)
  (rd   :field (byte 5 0)  :type 'fp-reg-single))

(define-emitter emit-format-fp-csel 32
  (byte 2 30) (byte 1 29) (byte 5 24) (byte 2 22) (byte 1 21)
  (byte 5 16) (byte 4 12) (byte 2 10) (byte 5 5) (byte 5 0))


;;; Conversion between FP and integer: FCVTNS / FCVTNU / SCVTF / UCVTF etc.
;;;
;;; Encoding (C6.2.84 etc.):
;;;   [31]    sf
;;;   [30]    0
;;;   [29]    S       (0)
;;;   [28:24] 11110
;;;   [23:22] type
;;;   [21]    1
;;;   [20:19] rmode
;;;   [18:16] opcode
;;;   [15:10] 000000
;;;   [9:5]   Rn
;;;   [4:0]   Rd

(disassem:define-instruction-format
    (format-fp-int-cvt 32
     :default-printer '(:name :tab rd ", " rn))
  (sf    :field (byte 1 31))
  (zero  :field (byte 1 30) :value 0)
  (s     :field (byte 1 29) :value 0)
  (op1   :field (byte 5 24))
  (type  :field (byte 2 22))
  (one   :field (byte 1 21) :value 1)
  (rmode :field (byte 2 19))
  (opc   :field (byte 3 16))
  (zero2 :field (byte 6 10) :value 0)
  (rn    :field (byte 5 5)  :type 'reg)
  (rd    :field (byte 5 0)  :type 'reg))

(define-emitter emit-format-fp-int-cvt 32
  (byte 1 31) (byte 1 30) (byte 1 29) (byte 5 24) (byte 2 22)
  (byte 1 21) (byte 2 19) (byte 3 16) (byte 6 10) (byte 5 5) (byte 5 0))


;;;; -----------------------------------------------------------------------
;;;; Miscellaneous system / undefined.
;;;; -----------------------------------------------------------------------

;;; UDF -- permanently undefined instruction.
;;;
;;; Encoding (C6.2.232): all bits [15:0] are the imm16, [31:16] = 0000000000000000.

(disassem:define-instruction-format
    (format-udf 32
     :default-printer '(:name :tab imm16))
  (op0   :field (byte 16 16) :value 0)
  (imm16 :field (byte 16 0)))

(define-emitter emit-format-udf 32
  (byte 16 16) (byte 16 0))


;;;; -----------------------------------------------------------------------
;;;; Bitmask immediate encoding/decoding helpers.
;;;;
;;;; AArch64 logical immediates are encoded as (N, immr, imms) triples.
;;;; See the ARM ARM section C2.2.5 for the algorithm.
;;;; -----------------------------------------------------------------------

(eval-when (compile load eval)
(defun decode-bit-mask (n immr imms)
  "Decode an AArch64 bitmask immediate (N, IMMR, IMMS) back to an integer.
  Returns the decoded value as an unsigned integer."
  ;; Determine element size from N and the high bits of imms.
  (let* ((esize (if (= n 1)
                    64
                    ;; Find highest zero bit in imms[5:1] to get esize.
                    (loop for len from 5 downto 1
                          when (zerop (ldb (byte 1 len) imms))
                          return (ash 1 len)
                          finally (return 2))))
         ;; Number of set bits = low bits of imms + 1, masked to esize.
         (ones  (1+ (ldb (byte (integer-length (1- esize)) 0) imms)))
         ;; Build a run of ONES 1-bits.
         (base  (1- (ash 1 ones)))
         ;; Rotate right by immr within esize bits.
         (rotated (logior (ash base (- esize immr))
                          (ash base (- immr))))
         (element (ldb (byte esize 0) rotated))
         ;; Replicate element across 64 bits.
         (result 0))
    (loop for pos from 0 below 64 by esize
          do (setf result (logior result (ash element pos))))
    result))
) ; eval-when

(defun encode-fp-immediate (value)
  "Encode a floating-point value as an 8-bit VFP immediate, or return NIL
  if VALUE cannot be represented.  A double-float is first narrowed to
  single-float; if the round-trip is not exact, NIL is returned."
  (let ((single (etypecase value
                  (single-float value)
                  (double-float
                   (let ((s (float value 1.0f0)))
                     (unless (= (float s 1.0d0) value)
                       (return-from encode-fp-immediate nil))
                     s)))))
    (let* ((bits  (single-float-bits single))
           (sign  (ldb (byte 1 31) bits))
           (exp   (ldb (byte 8 23) bits))
           (mant  (ldb (byte 23 0) bits)))
      ;; Low 19 mantissa bits must be zero; only top 4 are encodable.
      (unless (zerop (ldb (byte 19 0) mant))
        (return-from encode-fp-immediate nil))
      (let ((mant4 (ldb (byte 4 19) mant))
            (uexp  (- exp 127)))
        ;; Unbiased exponent must be in [-3, 4].
        (unless (<= -3 uexp 4)
          (return-from encode-fp-immediate nil))
        ;; Pack: sign(1) | NOT(exp3[2])(1) | exp3[1:0](2) | mant4(4)
        (let ((exp3 (ldb (byte 3 0) (+ uexp 3))))
          (logior (ash sign 7)
                  (ash (logxor (ldb (byte 1 2) exp3) 1) 6)
                  (ash (ldb (byte 2 0) exp3) 4)
                  mant4))))))


;;;; -----------------------------------------------------------------------
;;;; Instruction emitters.
;;;; -----------------------------------------------------------------------


;;;; PC-relative addressing.

(defun emit-adr-or-adrp (segment op rd label)
  "Emit ADR (op=0) or ADRP (op=1) RD, <label> using a back-patch."
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (let* ((target (label-position label))
              (delta  (if (zerop op)
                          (- target posn)
                          ;; ADRP: page-relative, 4K pages
                          (ash (- (ash target -12) (ash posn -12)) 12)))
              (immhi  (ldb (byte 19 2) delta))
              (immlo  (ldb (byte 2 0)  delta)))
         (emit-format-pc-rel segment op immlo #b10000 immhi rd)))))

(define-instruction adr (segment rd label)
  (:declare (type tn rd) (type label label))
  (:printer format-pc-rel ((op 0) (op1 #b10000)))
  (:attributes branch)
  (:emitter
   (emit-adr-or-adrp segment 0 (reg-tn-encoding rd) label)))

(define-instruction adrp (segment rd label)
  (:declare (type tn rd) (type label label))
  (:printer format-pc-rel ((op 1) (op1 #b10000)))
  (:attributes branch)
  (:emitter
   (emit-adr-or-adrp segment 1 (reg-tn-encoding rd) label)))


;;;; Add/Sub -- combined immediate and shifted-register forms.
;;
;; The third argument src accepts:
;;   (unsigned-byte 12)  -- bare immediate, LSL #0 implicit
;;   shifted-imm         -- immediate with explicit shift, constructed with IMM:
;;                            (imm value)      -- LSL #0 (default)
;;                            (imm value 12)   -- LSL #12
;;   tn                  -- register, no shift
;;   shifted-reg         -- register with explicit shift, constructed with SHIFT:
;;                            (shift reg :lsl amount)
;;                            (shift reg :lsr amount)
;;                            (shift reg :asr amount)
;;   extended-reg        -- extended register (64-bit only), constructed with EXTEND:
;;                            (extend reg :uxtw)     -- zero-extend W register
;;                            (extend reg :sxtw)     -- sign-extend W register
;;                            (extend reg :uxtx 3)   -- X register, LSL #3
;;
;; Examples:
;;   (inst add  x0 x1 100)                  ; ADD X0, X1, #100
;;   (inst add  x0 x1 (imm 1 12))           ; ADD X0, X1, #1, LSL #12  (= #4096)
;;   (inst sub  x0 x1 x2)                   ; SUB X0, X1, X2
;;   (inst add  x0 x1 (shift x2 :lsl 3))   ; ADD X0, X1, X2, LSL #3
;;   (inst adds x0 x1 255)                  ; ADDS X0, X1, #255  (sets flags)
;;   (inst add  x0 x1 (extend w2 :uxtw))   ; ADD X0, X1, W2, UXTW
;;   (inst add  sp sp (extend x0 :uxtx 3)) ; ADD SP, SP, X0, LSL #3
;;   (inst cmp  x0 1)                       ; CMP X0, #1
;;   (inst cmp  x0 x1)                      ; CMP X0, X1

(macrolet
    ((def (name sf op s neg-name has-ext)
       `(define-instruction ,name (segment rd rn src)
          (:declare (type tn rd rn)
                    (type (or (unsigned-byte 12) shifted-imm tn shifted-reg
                              ,@(when has-ext '(extended-reg))) src))
          (:printer format-add-sub-imm
                    ((sf ,sf) (op ,op) (s ,s) (op1 #b10001)))
          (:printer format-add-sub-shift
                    ((sf ,sf) (op ,op) (s ,s) (op1 #b01011)))
          ,@(when neg-name
              `((:printer format-add-sub-shift
                          ((sf ,sf) (op ,op) (s ,s) (op1 #b01011) (rn 31))
                          '(:name :tab rd ", " rm)
                          :print-name ',neg-name)))
          ,@(when has-ext
              `((:printer format-add-sub-ext
                          ((sf ,sf) (op ,op) (s ,s) (op1 #b01011)))))
          (:emitter
           (etypecase src
             ((unsigned-byte 12)
              (emit-format-add-sub-imm segment ,sf ,op ,s #b10001
                                        0 0 src
                                        (reg-tn-encoding rn)
                                        (reg-tn-encoding rd)))
             (shifted-imm
              (emit-format-add-sub-imm segment ,sf ,op ,s #b10001
                                        0 (shifted-imm-shift src)
                                        (shifted-imm-value src)
                                        (reg-tn-encoding rn)
                                        (reg-tn-encoding rd)))
             (tn
              (emit-format-add-sub-shift segment ,sf ,op ,s #b01011
                                          0 0
                                          (reg-tn-encoding src)
                                          0
                                          (reg-tn-encoding rn)
                                          (reg-tn-encoding rd)))
             (shifted-reg
              (emit-format-add-sub-shift segment ,sf ,op ,s #b01011
                                          (shift-type-encoding
                                           (shifted-reg-shift-type src))
                                          0
                                          (reg-tn-encoding
                                           (shifted-reg-reg src))
                                          (shifted-reg-shift-amount src)
                                          (reg-tn-encoding rn)
                                          (reg-tn-encoding rd)))
             ,@(when has-ext
                 `((extended-reg
                    (emit-format-add-sub-ext segment ,sf ,op ,s #b01011
                                              0 1
                                              (reg-tn-encoding
                                               (extended-reg-reg src))
                                              (extend-type-encoding
                                               (extended-reg-extend-type src))
                                              (extended-reg-shift src)
                                              (reg-tn-encoding rn)
                                              (reg-tn-encoding rd))))))))))
  ;;         name    sf  op  s    neg-name  has-ext
  (def add    1  0  0   nil   t)
  (def adds   1  0  1   nil   t)
  (def sub    1  1  0   neg   t)    ; rn=31 -> NEG
  (def subs   1  1  1   negs  t)    ; rn=31 -> NEGS
  (def add.w  0  0  0   nil   nil)
  (def adds.w 0  0  1   nil   nil)
  (def sub.w  0  1  0   neg.w nil)  ; rn=31 -> NEG.W
  (def subs.w 0  1  1   negs.w nil))

;; NEG Xd, src  =  SUB Xd, XZR, src
(define-instruction-macro neg (rd src)
  `(inst sub ,rd null-tn ,src))

(define-instruction-macro neg.w (rd src)
  `(inst sub.w ,rd null-tn ,src))

;; CMP is SUBS with Rd = XZR.
;; CMN is ADDS with Rd = XZR.
(macrolet
    ((def (name sf op)
       `(define-instruction ,name (segment rn src)
          (:declare (type tn rn)
                    (type (or (unsigned-byte 12) shifted-imm tn shifted-reg) src))
          (:printer format-add-sub-imm
                    ((sf ,sf) (op ,op) (s 1) (op1 #b10001) (rd 31))
                    '(:name :tab rn ", #" imm12))
          (:printer format-add-sub-shift
                    ((sf ,sf) (op ,op) (s 1) (op1 #b01011) (rd 31))
                    '(:name :tab rn ", " rm))
          (:emitter
           (etypecase src
             ((unsigned-byte 12)
              (emit-format-add-sub-imm segment ,sf ,op 1 #b10001
                                        0 0 src (reg-tn-encoding rn) 31))
             (shifted-imm
              (emit-format-add-sub-imm segment ,sf ,op 1 #b10001
                                        0 (shifted-imm-shift src)
                                        (shifted-imm-value src)
                                        (reg-tn-encoding rn) 31))
             (tn
              (emit-format-add-sub-shift segment ,sf ,op 1 #b01011
                                          0 0 (reg-tn-encoding src) 0
                                          (reg-tn-encoding rn) 31))
             (shifted-reg
              (emit-format-add-sub-shift segment ,sf ,op 1 #b01011
                                          (shift-type-encoding
                                           (shifted-reg-shift-type src))
                                          0
                                          (reg-tn-encoding (shifted-reg-reg src))
                                          (shifted-reg-shift-amount src)
                                          (reg-tn-encoding rn) 31)))))))
  (def cmp   1 1)
  (def cmp.w 0 1)
  (def cmn   1 0)
  (def cmn.w 0 0))


;;;; Move wide (immediate): MOVZ / MOVN / MOVK.
;;
;; MOVZ zeroes the register and moves the immediate into the selected halfword.
;; MOVN moves the bitwise inverse of the immediate.
;; MOVK keeps the other halfwords and inserts the immediate into one halfword.
;;
;; Examples:
;;   (inst movz x0 #xABCD)             ; MOVZ X0, #0xABCD
;;   (inst movz x0 #xABCD :lsl 16)     ; MOVZ X0, #0xABCD, LSL #16  (= #xABCD0000)
;;   (inst movk x0 #x1234 :lsl 32)     ; MOVK X0, #0x1234, LSL #32
;;   (inst movn x0 0)                  ; MOVN X0, #0                 (= -1)
;;   ;; Load #xDEADBEEF12345678 into X0:
;;   (inst movz x0 #x5678)
;;   (inst movk x0 #x1234 :lsl 16)
;;   (inst movk x0 #xBEEF :lsl 32)
;;   (inst movk x0 #xDEAD :lsl 48)

(macrolet
    ((def (name opc sf fixup-kind)
       `(define-instruction ,name (segment rd src &key (lsl 0))
          (:declare (type tn rd)
                    (type (or (unsigned-byte 16) fixup) src)
                    (type (member 0 16 ,@(unless (zerop sf) '(32 48))) lsl))
          (:printer format-move-wide
                    ((opc ,opc) (op1 #b100101) (sf ,sf)))
          (:emitter
           (let ((hw (/ lsl 16)))
             (etypecase src
               ((unsigned-byte 16)
                (emit-format-move-wide segment ,sf ,opc #b100101 hw src
                                       (reg-tn-encoding rd)))
               (fixup
                (note-fixup segment ,fixup-kind src)
                (emit-format-move-wide segment ,sf ,opc #b100101 hw 0
                                       (reg-tn-encoding rd)))))))))
  (def movn   0 1 :movn)   ; Move with NOT,  64-bit
  (def movz   2 1 :movz)   ; Move with zero, 64-bit
  (def movk   3 1 :movk)   ; Move with keep, 64-bit
  (def movn.w 0 0 :movn)   ; Move with NOT,  32-bit
  (def movz.w 2 0 :movz)   ; Move with zero, 32-bit
  (def movk.w 3 0 :movk))  ; Move with keep, 32-bit


;;;; Logical -- combined immediate and shifted-register forms.
;;
;; The src argument accepts:
;;   bitmask-imm  -- logical bitmask immediate (use MASK to construct)
;;   tn           -- register, no shift
;;   shifted-reg  -- register with explicit shift type and amount
;;
;; Examples:
;;   (inst and  x0 x1 (mask #xff))          ; AND X0, X1, #0xFF
;;   (inst and  x0 x1 x2)                   ; AND X0, X1, X2
;;   (inst and  x0 x1 (shift x2 :lsl 3))   ; AND X0, X1, X2, LSL #3
;;   (inst ands x0 x1 (mask #xff))          ; ANDS X0, X1, #0xFF  (sets flags)
;;   (inst tst  x0 (mask #xff))             ; TST X0, #0xFF        (= ANDS XZR, X0, #0xFF)
;;   (inst tst  x0 x1)                      ; TST X0, X1

(macrolet
    ((def (name sf opc invertp tst-name mov-name)
       `(define-instruction ,name (segment rd rn src)
          (:declare (type tn rd rn)
                    (type (or ,@(unless invertp '(bitmask-imm)) tn shifted-reg) src))
          ,@(unless invertp
              `((:printer format-logic-imm
                          ((sf ,sf) (opc ,opc) (op1 #b100100)))))
          ,@(when tst-name
              `((:printer format-logic-imm
                          ((sf ,sf) (opc ,opc) (op1 #b100100) (rd 31))
                          '(:name :tab rn ", " bimm)
                          :print-name ',tst-name)))
          (:printer format-logic-reg
                    ((sf ,sf) (opc ,opc) (op1 #b01010) (n ,(if invertp 1 0))))
          ,@(when tst-name
              `((:printer format-logic-reg
                          ((sf ,sf) (opc ,opc) (op1 #b01010) (n 0) (rd 31))
                          '(:name :tab rn ", " rm)
                          :print-name ',tst-name)))
          ,@(when mov-name
              `((:printer format-logic-reg
                          ((sf ,sf) (opc ,opc) (op1 #b01010)
                           (n ,(if invertp 1 0)) (rn 31))
                          '(:name :tab rd ", " rm)
                          :print-name ',mov-name)))
          (:emitter
           (etypecase src
             ,@(unless invertp
                 `((bitmask-imm
                    (emit-format-logic-imm segment ,sf ,opc #b100100
                                            (bitmask-imm-n src)
                                            (bitmask-imm-immr src)
                                            (bitmask-imm-imms src)
                                            (reg-tn-encoding rn)
                                            (reg-tn-encoding rd)))))
             (tn
              (emit-format-logic-reg segment ,sf ,opc #b01010
                                      0 ,(if invertp 1 0)
                                      (reg-tn-encoding src)
                                      0
                                      (reg-tn-encoding rn)
                                      (reg-tn-encoding rd)))
             (shifted-reg
              (emit-format-logic-reg segment ,sf ,opc #b01010
                                      (shift-type-encoding
                                       (shifted-reg-shift-type src))
                                      ,(if invertp 1 0)
                                      (reg-tn-encoding
                                       (shifted-reg-reg src))
                                      (shifted-reg-shift-amount src)
                                      (reg-tn-encoding rn)
                                      (reg-tn-encoding rd))))))))
  ;;          name    sf   opc    invertp  tst-name  mov-name
  (def and    1 #b00 nil   nil    nil)
  (def orr    1 #b01 nil   nil    mov)     ; rn=31 -> MOV
  (def eor    1 #b10 nil   nil    nil)
  (def ands   1 #b11 nil   tst    nil)     ; rd=31 -> TST
  (def bic    1 #b00 t     nil    nil)     ; AND NOT  (no immediate form)
  (def orn    1 #b01 t     nil    mvn)     ; rn=31 -> MVN
  (def eon    1 #b10 t     nil    nil)     ; EOR NOT  (no immediate form)
  (def bics   1 #b11 t     nil    nil)     ; BICS     (no immediate form)
  ;; 32-bit variants
  (def and.w  0 #b00 nil   nil    nil)
  (def orr.w  0 #b01 nil   nil    mov.w)  ; rn=31 -> MOV.W
  (def eor.w  0 #b10 nil   nil    nil)
  (def ands.w 0 #b11 nil   tst.w  nil)    ; rd=31 -> TST.W
  (def bic.w  0 #b00 t     nil    nil)
  (def orn.w  0 #b01 t     nil    mvn.w)  ; rn=31 -> MVN.W
  (def eon.w  0 #b10 t     nil    nil)
  (def bics.w 0 #b11 t     nil    nil))

;; TST Xn/Wn, src  =  ANDS XZR/WZR, Xn/Wn, src
;; TST / TST.W -- aliases for ANDS / ANDS.W with Rd = XZR/WZR.
(define-instruction-macro tst (rn src)
  `(inst ands null-tn ,rn ,src))

(define-instruction-macro tst.w (rn src)
  `(inst ands.w null-tn ,rn ,src))

;; MOV Xd, src  =  ORR Xd, XZR, src
(define-instruction-macro mov (rd src)
  `(inst orr ,rd null-tn ,src))

;; MVN Xd, src  =  ORN Xd, XZR, src  -- bitwise NOT.
(define-instruction-macro mvn (rd src)
  `(inst orn ,rd null-tn ,src))


;;;; Bitfield instructions.
;;
;; SBFM (Signed Bitfield Move): copies a bitfield and sign-extends it.
;; UBFM (Unsigned Bitfield Move): copies a bitfield and zero-extends it.
;; BFM  (Bitfield Move): inserts a bitfield into the destination.
;;
;; The immr and imms fields encode the bitfield:
;;   immr = right-rotation amount
;;   imms = bitfield width - 1  (for extraction)
;;
;; Aliases are generated automatically for common cases:
;;   SXTB Xd, Xn  =  SBFM Xd, Xn, #0, #7
;;   SXTH Xd, Xn  =  SBFM Xd, Xn, #0, #15
;;   SXTW Xd, Xn  =  SBFM Xd, Xn, #0, #31
;;   ASR  Xd, Xn, #shift  =  SBFM Xd, Xn, #shift, #63
;;
;; Examples:
;;   (inst sbfm x0 x1 0 7)    ; SBFM X0, X1, #0, #7   -- sign-extend byte  (= SXTB)
;;   (inst sbfm x0 x1 0 31)   ; SBFM X0, X1, #0, #31  -- sign-extend word  (= SXTW)
;;   (inst sbfm x0 x1 8 63)   ; SBFM X0, X1, #8, #63  -- ASR by 8
;;   (inst sbfm x0 x1 4 7)    ; SBFM X0, X1, #4, #7   -- extract and sign-extend 4-bit field
;;
;; SBFM (Signed Bitfield Move): copies a bitfield and sign-extends it.
;; UBFM (Unsigned Bitfield Move): copies a bitfield and zero-extends it.
;; BFM  (Bitfield Move): inserts a bitfield into the destination.
;;
;; The immr and imms fields encode the bitfield:
;;   immr = right-rotation amount
;;   imms = bitfield width - 1  (for extraction)
;;
;; Aliases are generated automatically for common cases:
;;   SXTB Xd, Xn        =  SBFM Xd, Xn, #0, #7
;;   SXTH Xd, Xn        =  SBFM Xd, Xn, #0, #15
;;   SXTW Xd, Xn        =  SBFM Xd, Xn, #0, #31
;;   ASR  Xd, Xn, #shift  =  SBFM Xd, Xn, #shift, #63
;;
;; Examples:
;;   (inst sbfm x0 x1 0  7)  ; SBFM X0, X1, #0, #7   -- sign-extend byte  (= SXTB)
;;   (inst sbfm x0 x1 0 31)  ; SBFM X0, X1, #0, #31  -- sign-extend word  (= SXTW)
;;   (inst sbfm x0 x1 8 63)  ; SBFM X0, X1, #8, #63  -- ASR #8
;;   (inst sbfm x0 x1 4  7)  ; SBFM X0, X1, #4, #7   -- extract and sign-extend 4-bit field

(macrolet
    ((def (name sf opc aliases imms-aliases)
       `(define-instruction ,name (segment rd rn immr imms)
          (:declare (type tn rd rn)
                    (type (unsigned-byte 6) immr imms))
          (:printer format-bitfield
                    ((sf ,sf) (opc ,opc) (op1 #b100110)))
          ,@(mapcar (lambda (alias)
                      (destructuring-bind (aname immr-val imms-val) alias
                        `(:printer format-bitfield
                                   ((sf ,sf) (opc ,opc) (op1 #b100110)
                                    (n ,sf) (immr ,immr-val) (imms ,imms-val))
                                   '(:name :tab rd ", " rn)
                                   :print-name ',aname)))
                    aliases)
          ,@(mapcar (lambda (alias)
                      ;; imms-only alias: matches any immr, prints immr as shift amount
                      (destructuring-bind (aname imms-val) alias
                        `(:printer format-bitfield
                                   ((sf ,sf) (opc ,opc) (op1 #b100110)
                                    (n ,sf) (imms ,imms-val))
                                   '(:name :tab rd ", " rn ", " immr)
                                   :print-name ',aname)))
                    imms-aliases)
          (:emitter
           (emit-format-bitfield segment ,sf ,opc #b100110
                                  ,sf  ; N = sf for legal encodings
                                  immr imms
                                  (reg-tn-encoding rn)
                                  (reg-tn-encoding rd))))))
  ;;           name   sf   opc    fixed-aliases           imms-only-aliases
  (def sbfm    1 #b00
               ((sxtb 0  7) (sxth 0 15) (sxtw 0 31))
               ())
  (def bfm     1 #b01
               ()
               ())
  (def ubfm    1 #b10
               ((uxtb 0  7) (uxth 0 15))
               ())
  (def sbfm.w  0 #b00
               ((sxtb.w 0  7) (sxth.w 0 15))
               ())
  (def bfm.w   0 #b01
               ()
               ())
  (def ubfm.w  0 #b10
               ((uxtb.w 0  7) (uxth.w 0 15))
               ()))

;; Convenient aliases.
(define-instruction-macro sxtb (rd rn)
  `(inst sbfm ,rd ,rn 0 7))

(define-instruction-macro sxth (rd rn)
  `(inst sbfm ,rd ,rn 0 15))

(define-instruction-macro sxtw (rd rn)
  `(inst sbfm ,rd ,rn 0 31))

(define-instruction-macro uxtb (rd rn)
  `(inst ubfm.w ,rd ,rn 0 7))

(define-instruction-macro uxth (rd rn)
  `(inst ubfm.w ,rd ,rn 0 15))

(define-instruction-macro uxtw (rd rn)
  `(inst ubfm ,rd ,rn 0 31))

;;; EXTR -- extract register (used by ROR immediate alias).
(macrolet
    ((def (name sf)
       `(define-instruction ,name (segment rd rn rm imms)
          (:declare (type tn rd rn rm)
                    (type (unsigned-byte ,(if (zerop sf) 5 6)) imms))
          (:printer format-extract ((sf ,sf) (op21 0) (op1 #b100111) (o0 0)))
          (:emitter
           (emit-format-extract segment ,sf 0 #b100111 ,sf 0
                                 (reg-tn-encoding rm)
                                 imms
                                 (reg-tn-encoding rn)
                                 (reg-tn-encoding rd))))))
  (def extr   1)   ; 64-bit
  (def extr.w 0))  ; 32-bit

;;; ASR / LSR / LSL / ROR -- aliases dispatching at macro-expansion time.
;;
;; If shift is an integer literal the immediate encoding is used;
;; otherwise the register (variable-shift) instruction is emitted.
;;
;;   asr / asr.w  -> sbfm / sbfm.w  (imm)  or  asrv / asrv.w  (reg)
;;   lsr / lsr.w  -> ubfm / ubfm.w  (imm)  or  lsrv / lsrv.w  (reg)
;;   lsl / lsl.w  -> ubfm / ubfm.w  (imm)  or  lslv / lslv.w  (reg)
;;   ror / ror.w  -> extr            (imm)  or  rorv / rorv.w  (reg)

;;; Shift instructions as real instructions dispatching on shift type.
;;
;; Immediate shift -> bitfield/extract instruction
;; Register shift  -> variable-shift instruction (asrv/lsrv/lslv/rorv)
;;
;; asr  imm: SBFM Rd, Rn, shift, {63|31}
;; lsr  imm: UBFM Rd, Rn, shift, {63|31}
;; lsl  imm: UBFM Rd, Rn, {64|32}-shift, {63|31}-shift
;; ror  imm: EXTR Rd, Rn, Rn, shift

(defun emit-shift-reg (segment sf opc rd rn rm)
  "Emit a variable-shift instruction (ASRV/LSRV/LSLV/RORV)."
  (emit-format-dp-2src segment sf 0 0 #b11010110
                        (reg-tn-encoding rm) opc
                        (reg-tn-encoding rn)
                        (reg-tn-encoding rd)))

(defun emit-shift-imm (segment sf bf-opc rd rn immr imms)
  "Emit a bitfield-based immediate shift (ASR/LSR/LSL)."
  (emit-format-bitfield segment sf bf-opc #b100110
                         sf immr imms
                         (reg-tn-encoding rn)
                         (reg-tn-encoding rd)))

;;; ASR -- Arithmetic Shift Right
;;; Immediate: SBFM Rd, Rn, shift, {63|31}
;;; Register:  ASRV Rd, Rn, Rm

(macrolet
    ((def (name sf imms-fixed)
       `(define-instruction ,name (segment rd rn shift)
          (:declare (type tn rd rn)
                    (type (or tn (integer 0 63)) shift))
          (:printer format-bitfield
                    ((sf ,sf) (opc #b00) (op1 #b100110) (n ,sf) (imms ,imms-fixed))
                    '(:name :tab rd ", " rn ", " immr)
                    :print-name ',name)
          (:printer format-dp-2src
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11010110) (opc #b001010))
                    '(:name :tab rd ", " rn ", " rm)
                    :print-name ',name)
          (:emitter
           (etypecase shift
             (integer (emit-shift-imm segment ,sf #b00 rd rn shift ,imms-fixed))
             (tn      (emit-shift-reg segment ,sf #b001010 rd rn shift)))))))
  (def asr   1 63)
  (def asr.w 0 31))

;;; LSR -- Logical Shift Right
;;; Immediate: UBFM Rd, Rn, shift, {63|31}
;;; Register:  LSRV Rd, Rn, Rm

(macrolet
    ((def (name sf imms-fixed)
       `(define-instruction ,name (segment rd rn shift)
          (:declare (type tn rd rn)
                    (type (or tn (integer 0 63)) shift))
          (:printer format-bitfield
                    ((sf ,sf) (opc #b10) (op1 #b100110) (n ,sf) (imms ,imms-fixed))
                    '(:name :tab rd ", " rn ", " immr)
                    :print-name ',name)
          (:printer format-dp-2src
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11010110) (opc #b001001))
                    '(:name :tab rd ", " rn ", " rm)
                    :print-name ',name)
          (:emitter
           (etypecase shift
             (integer (emit-shift-imm segment ,sf #b10 rd rn shift ,imms-fixed))
             (tn      (emit-shift-reg segment ,sf #b001001 rd rn shift)))))))
  (def lsr   1 63)
  (def lsr.w 0 31))

;;; LSL -- Logical Shift Left
;;; Immediate: UBFM Rd, Rn, {64|32}-shift, {63|31}-shift
;;; Register:  LSLV Rd, Rn, Rm

(macrolet
    ((def (name sf reg-size)
       `(define-instruction ,name (segment rd rn shift)
          (:declare (type tn rd rn)
                    (type (or tn (integer 0 63)) shift))
          (:printer format-bitfield
                    ((sf ,sf) (opc #b10) (op1 #b100110) (n ,sf))
                    '(:name :tab rd ", " rn ", " immr)
                    :print-name ',name)
          (:printer format-dp-2src
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11010110) (opc #b001000))
                    '(:name :tab rd ", " rn ", " rm)
                    :print-name ',name)
          (:emitter
           (etypecase shift
             (integer (emit-shift-imm segment ,sf #b10 rd rn
                                       (- ,reg-size shift)
                                       (- ,(1- reg-size) shift)))
             (tn      (emit-shift-reg segment ,sf #b001000 rd rn shift)))))))
  (def lsl   1 64)
  (def lsl.w 0 32))

;;; ROR -- Rotate Right
;;; Immediate: EXTR Rd, Rn, Rn, shift  (disassembles as EXTR -- same encoding)
;;; Register:  RORV Rd, Rn, Rm
;;;
;;; The immediate form has no format-extract printer because ROR is
;;; indistinguishable from EXTR without a rm=rn specializer constraint,
;;; which the CMUCL disassembler DSL does not support.
;;; Future improvement: add a note printer (as Sparc does) so that
;;; "EXTR Rd, Rn, Rn, #shift" also prints "; ROR Rd, Rn, #shift".

(macrolet
    ((def (name sf)
       `(define-instruction ,name (segment rd rn shift)
          (:declare (type tn rd rn)
                    (type (or tn (integer 0 63)) shift))
          ;; No format-extract printer -- see comment above.
          (:printer format-dp-2src
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11010110) (opc #b001011))
                    '(:name :tab rd ", " rn ", " rm)
                    :print-name ',name)
          (:emitter
           (etypecase shift
             (integer
              (emit-format-extract segment ,sf 0 #b100111 ,sf 0
                                    (reg-tn-encoding rn) shift
                                    (reg-tn-encoding rn)
                                    (reg-tn-encoding rd)))
             (tn
              (emit-shift-reg segment ,sf #b001011 rd rn shift)))))))
  (def ror   1)
  (def ror.w 0))


;;;; Branches.

(defun emit-relative-branch (segment op target)
  "Emit a B (op=0) or BL (op=1) to TARGET using a back-patch."
  (emit-back-patch segment 4
    #'(lambda (segment posn)
        (let ((delta (- (label-position target) posn)))
          (assert (zerop (ldb (byte 2 0) delta)))
          (emit-branch-imm segment op #b00101
                           (ldb (byte 26 0) (ash delta -2)))))))

(define-instruction b (segment cond-or-target &optional (target nil targetp))
  (:declare (type (or condition-code label) cond-or-target)
            (type (or null label) target))
  ;; Conditional form printer
  (:printer format-cond-branch ((op1 #b0101010) (o1 0) (o0 0)))
  ;; Unconditional form printer
  (:printer branch-imm ((op 0) (op1 #b00101)))
  (:attributes branch)
  (:emitter
   (if targetp
     ;; (inst b :eq label) -- conditional branch
     (emit-cond-branch segment
                       (condition-code-encoding cond-or-target)
                       target)
     ;; (inst b label) -- unconditional branch
     (emit-relative-branch segment 0 cond-or-target))))

;; B.cond aliases -- (inst b.eq label) expands to (inst b :eq label)
(macrolet
    ((def (name cond)
       `(define-instruction-macro ,name (target)
          `(inst b ,,cond ,target))))
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
  (def b.al :al))

(define-instruction bl (segment target)
  (:declare (type label target))
  (:printer branch-imm ((op 1) (op1 #b00101)))
  (:attributes branch)
  (:emitter
   (emit-relative-branch segment 1 target)))

(define-instruction br (segment rn)
  (:declare (type tn rn))
  (:printer branch-reg ((opc #b0000)))
  (:attributes branch)
  (:emitter
   (emit-branch-reg segment #b1101011 #b0000 #b11111 #b000000
                    (reg-tn-encoding rn) #b00000)))

(define-instruction blr (segment rn)
  (:declare (type tn rn))
  (:printer branch-reg ((opc #b0001)))
  (:attributes branch)
  (:emitter
   (emit-branch-reg segment #b1101011 #b0001 #b11111 #b000000
                    (reg-tn-encoding rn) #b00000)))

(define-instruction ret (segment &optional (rn 30))
  (:declare (type (or tn (integer 0 31)) rn))
  (:printer branch-reg ((opc #b0010))
            '(:name (:unless (rn :constant 30) :tab rn)))
  (:attributes branch)
  (:emitter
   (emit-branch-reg segment #b1101011 #b0010 #b11111 #b000000
                    (if (typep rn 'tn) (reg-tn-encoding rn) rn)
                    #b00000)))


;;; Conditional branch.

(defun emit-cond-branch (segment cond-code target)
  (emit-back-patch segment 4
    #'(lambda (segment posn)
        (let ((delta (- (label-position target) posn)))
          (assert (zerop (ldb (byte 2 0) delta)))
          (emit-format-cond-branch segment #b0101010 0
                                   (ldb (byte 19 0) (ash delta -2))
                                   0 cond-code)))))


;;; Compare and branch.

(macrolet
    ((def (name sf op)
       `(define-instruction ,name (segment rt target)
          (:declare (type tn rt) (type label target))
          (:printer format-compare-branch
                    ((sf ,sf) (op1 #b011010) (op ,op)
                     ,@(when (zerop sf) '((rt nil :type 'wreg)))))
          (:attributes branch)
          (:emitter
           (emit-back-patch segment 4
             #'(lambda (segment posn)
                 (let ((delta (- (label-position target) posn)))
                   (assert (zerop (ldb (byte 2 0) delta)))
                   (emit-format-compare-branch
                    segment ,sf #b011010 ,op
                    (ldb (byte 19 0) (ash delta -2))
                    (reg-tn-encoding rt)))))))))
  (def cbz   1 0)
  (def cbnz  1 1)
  (def cbz.w  0 0)
  (def cbnz.w 0 1))


;;; Test and branch.

(macrolet
    ((def (name op sf)
       `(define-instruction ,name (segment rt bit-num target)
          (:declare (type tn rt)
                    (type (unsigned-byte ,(if (zerop sf) 5 6)) bit-num)
                    (type label target))
          (:printer format-test-branch
                    ((op1 #b011011) (op ,op) (b5 ,sf)
                     ,@(when (zerop sf) '((rt nil :type 'wreg)))))
          (:attributes branch)
          (:emitter
           (emit-back-patch segment 4
             #'(lambda (segment posn)
                 (let ((delta (- (label-position target) posn)))
                   (assert (zerop (ldb (byte 2 0) delta)))
                   (emit-format-test-branch
                    segment
                    ,sf
                    #b011011
                    ,op
                    (ldb (byte 5 0) bit-num)
                    (ldb (byte 14 0) (ash delta -2))
                    (reg-tn-encoding rt)))))))))
  (def tbz    0 1)   ; 64-bit, b5=1, bits 32-63
  (def tbnz   1 1)   ; 64-bit, b5=1, bits 32-63
  (def tbz.w  0 0)   ; 32-bit, b5=0, bits 0-31
  (def tbnz.w 1 0))  ; 32-bit, b5=0, bits 0-31


;;;; Exception generation.

(macrolet
    ((def (name opc ll)
       `(define-instruction ,name (segment imm16)
          (:declare (type (unsigned-byte 16) imm16))
          (:printer format-exception
                    ((op1 #b11010100) (opc ,opc) (op2 #b000) (ll ,ll)))
          (:emitter
           (emit-format-exception segment #b11010100 ,opc imm16 #b000 ,ll)))))
  (def svc 0 #b01)   ; Supervisor call
  (def hvc 0 #b10)   ; Hypervisor call
  (def smc 0 #b11)   ; Secure monitor call
  (def brk 1 #b00)   ; Breakpoint
  (def hlt 2 #b00))  ; Halt (debug)

;; UDF -- permanently undefined.
(define-instruction udf (segment imm16)
  (:declare (type (unsigned-byte 16) imm16))
  (:printer format-udf ((op0 0)))
  (:emitter
   (emit-format-udf segment 0 imm16)))


;;;; System instructions: NOP, MSR, MRS, DMB, DSB, ISB.

;; HINT instructions share the encoding with NOP / WFI / WFE / SEV / SEVL.
(defun emit-hint (segment crm op2)
  (emit-format-system segment #b1101010100 0 #b00 #b011 #b0010 crm op2 31))

(define-instruction nop (segment)
  (:printer format-system
            ((op1 #b1101010100) (l 0) (sysreg #x1900) (rt 31))
            '(:name))
  (:emitter
   (emit-hint segment 0 0)))

(define-instruction wfe (segment)
  (:printer format-system
            ((op1 #b1101010100) (l 0) (sysreg #x1902) (rt 31))
            '(:name))
  (:emitter
   (emit-hint segment 0 2)))

(define-instruction wfi (segment)
  (:printer format-system
            ((op1 #b1101010100) (l 0) (sysreg #x1901) (rt 31))
            '(:name))
  (:emitter
   (emit-hint segment 0 1)))

(define-instruction sev (segment)
  (:emitter (emit-hint segment 0 4)))

(define-instruction sevl (segment)
  (:emitter (emit-hint segment 0 5)))

;; Barrier instructions.  crm is a variable 4-bit field (barrier type/domain);
;; we constrain op1/l/rt and the fixed bits of sysreg around crm.
(macrolet
    ((def (name op2)
       `(define-instruction ,name (segment &optional (crm #b1111))
          (:declare (type (unsigned-byte 4) crm))
          (:printer format-system
                    ((op1 #b1101010100) (l 0) (rt 31)))
          (:emitter
           (emit-format-system segment #b1101010100 0 #b00 #b011 #b0011
                               crm ,op2 31)))))
  (def dsb 4)
  (def dmb 5)
  (def isb 6))

;; System register keyword map: keyword -> 16-bit op0:op1f:CRn:CRm:op2 encoding.
;; MRS -- read system register into Xt.
;; MSR -- write Xt to system register.
;;
;; The sysreg argument accepts a keyword from +sysreg-keyword-map+
;; or a raw 16-bit encoding (op0:op1f:CRn:CRm:op2).
;;
;; Examples:
;;   (inst mrs x0 :fpcr)               ; MRS X0, FPCR  -- read FP control register
;;   (inst mrs x1 :fpsr)               ; MRS X1, FPSR  -- read FP status register
;;   (inst msr :fpcr x0)               ; MSR FPCR, X0  -- write FP control register
;;   (inst msr :nzcv x2)               ; MSR NZCV, X2  -- write condition flags
;;   ;; Clear FPSR:
;;   (inst movz x0 0)
;;   (inst msr :fpsr x0)
;;   ;; Set Z and C flags via NZCV:
;;   (inst movz x0 (ash (nzcv :z :c) 28))
;;   (inst msr :nzcv x0)
(define-instruction mrs (segment rt sysreg)
  (:declare (type tn rt)
            (type (or (unsigned-byte 16) keyword) sysreg))
  (:printer format-system ((op1 #b1101010100) (l 1))
            '(:name :tab rt ", " sysreg))
  (:emitter
   (let* ((enc  (encode-sysreg sysreg))
          (op0  (ldb (byte 2 14) enc))
          (op1f (ldb (byte 3 11) enc))
          (crn  (ldb (byte 4  7) enc))
          (crm  (ldb (byte 4  3) enc))
          (op2  (ldb (byte 3  0) enc)))
     (emit-format-system segment #b1101010100 1 op0 op1f crn crm op2
                         (reg-tn-encoding rt)))))

;; MSR -- write Xt to system register.
(define-instruction msr (segment sysreg rt)
  (:declare (type (or (unsigned-byte 16) keyword) sysreg)
            (type tn rt))
  (:printer format-system ((op1 #b1101010100) (l 0))
            '(:name :tab sysreg ", " rt))
  (:emitter
   (let* ((enc  (encode-sysreg sysreg))
          (op0  (ldb (byte 2 14) enc))
          (op1f (ldb (byte 3 11) enc))
          (crn  (ldb (byte 4  7) enc))
          (crm  (ldb (byte 4  3) enc))
          (op2  (ldb (byte 3  0) enc)))
     (emit-format-system segment #b1101010100 0 op0 op1f crn crm op2
                         (reg-tn-encoding rt)))))



;;;; Load/Store -- memory-ref addressing.
;;
;; The second argument is always a MEMORY-REF capturing base register,
;; byte offset, and addressing mode.  Use the constructors:
;;
;;   (mem rn)            -> [Xn]           unsigned offset 0
;;   (mem rn offset)     -> [Xn, #offset]  unsigned byte offset (multiple of access size)
;;   (pre-index rn n)    -> [Xn, #n]!      pre-index, signed byte offset -256..255
;;   (post-index rn n)   -> [Xn], #n       post-index, signed byte offset -256..255
;;
;; All offsets are raw byte offsets.  For the unsigned form the emitter
;; asserts divisibility by the access size and divides to produce imm12.
;;
;; Examples:
;;   (inst ldr  x0 (mem x1))              ; LDR X0, [X1]
;;   (inst ldr  x0 (mem x1 16))           ; LDR X0, [X1, #16]
;;   (inst ldr  x0 (pre-index x1 16))     ; LDR X0, [X1, #16]!
;;   (inst ldr  x0 (post-index x1 -8))    ; LDR X0, [X1], #-8
;;   (inst ldr  x0 (reg-offset x1 x2))             ; LDR X0, [X1, X2]
;;   (inst ldr  x0 (reg-offset x1 x2 :uxtx 3))     ; LDR X0, [X1, X2, LSL #3]
;;   (inst ldr  x0 (reg-offset x1 x2 :uxtw))       ; LDR X0, [X1, W2, UXTW]
;;   (inst ldr  x0 (reg-offset x1 x2 :sxtw 2))     ; LDR X0, [X1, W2, SXTW #2]
;;   (inst ldr  x0 label)                          ; LDR X0, label  (literal)
;;   (inst ldr.w x0 label)                         ; LDR W0, label  (literal)
;;   (inst ldrb w0 (mem x1 3))            ; LDRB W0, [X1, #3]
;;   (inst str  x0 (mem x1 16))           ; STR X0, [X1, #16]

;;; Byte, halfword and sign-extending load/store.

(defun emit-ldst-modes (segment mem size v opc access-size rt-enc)
  "Emit a load/store instruction dispatching on MEM's addressing mode.
  SIZE, V, OPC are the encoding fields; ACCESS-SIZE is the byte width
  for scaling the unsigned offset; RT-ENC is the already-encoded
  destination/source register number."
  (let ((rn     (memory-ref-base mem))
        (offset (memory-ref-offset mem)))
    (ecase (memory-ref-mode mem)
      (:offset
       (assert (zerop (mod offset access-size))
               (offset)
               "Byte offset ~D is not a multiple of access size ~D."
               offset access-size)
       (emit-format-ldst-uoffset segment size #b111 v #b01 opc
                                  (/ offset access-size)
                                  (reg-tn-encoding rn) rt-enc))
      (:pre
       (emit-format-ldst-imm9 segment size #b111 v #b00 opc
                               0 (ldb (byte 9 0) offset) #b11
                               (reg-tn-encoding rn) rt-enc))
      (:post
       (emit-format-ldst-imm9 segment size #b111 v #b00 opc
                               0 (ldb (byte 9 0) offset) #b01
                               (reg-tn-encoding rn) rt-enc))
      (:reg-offset
       (let* ((ext (memory-ref-rm mem))
              (rm  (extended-reg-reg ext))
              (s   (if (zerop (extended-reg-shift ext)) 0 1)))
         (emit-format-ldst-reg segment size #b111 v #b00 opc
                                1
                                (reg-tn-encoding rm)
                                (extend-type-encoding
                                 (extended-reg-extend-type ext))
                                s #b10
                                (reg-tn-encoding rn) rt-enc))))))

(macrolet
    ((def (name size opc access-size)
       `(define-instruction ,name (segment rt mem)
          (:declare (type tn rt)
                    (type memory-ref mem))
          (:printer format-ldst-uoffset
                    ((size ,size) (op1 #b111) (v 0) (op2 #b01) (opc ,opc)))
          (:printer format-ldst-imm9
                    ((z 0) (size ,size) (op1 #b111) (v 0) (op2 #b00) (opc ,opc)
                     (type #b11)))
          (:printer format-ldst-imm9
                    ((z 0) (size ,size) (op1 #b111) (v 0) (op2 #b00) (opc ,opc)
                     (type #b01)))
          (:printer format-ldst-reg
                    ((size ,size) (op1 #b111) (v 0) (op2 #b00) (opc ,opc)
                     (one 1) (op3 #b10)))
          (:emitter
           (emit-ldst-modes segment mem ,size 0 ,opc ,access-size
                             (reg-tn-encoding rt))))))
  ;;          name      size    opc    access-size
  (def strb    #b00 #b00 1)
  (def ldrb    #b00 #b01 1)
  (def ldrsb   #b00 #b10 1)   ; sign-extend byte -> X
  (def ldrsb.w #b00 #b11 1)   ; sign-extend byte -> W
  (def strh    #b01 #b00 2)
  (def ldrh    #b01 #b01 2)
  (def ldrsh   #b01 #b10 2)   ; sign-extend halfword -> X
  (def ldrsh.w #b01 #b11 2)   ; sign-extend halfword -> W
  (def ldrsw   #b10 #b10 4))  ; sign-extend word -> X

;;; 32-bit and 64-bit load/store -- integer and FP registers.
;;
;; Dispatches on sc-case rt:
;;   single-reg -> FP single (S register, size=#b10, v=1)
;;   double-reg -> FP double (D register, size=#b11, v=1)
;;   otherwise  -> integer register (v=0)

(macrolet
    ((def (name int-size int-opc int-access-size lit-opc wreg-p)
       `(define-instruction ,name (segment rt mem)
          (:declare (type tn rt)
                    (type (or memory-ref ,@(when lit-opc '(label))) mem))
          ;; Integer register printers (v=0)
          (:printer format-ldst-uoffset
                    ((size ,int-size) (op1 #b111) (v 0) (op2 #b01) (opc ,int-opc)
                     ,@(when wreg-p '((rt nil :type 'wreg)))))
          (:printer format-ldst-imm9
                    ((z 0) (size ,int-size) (op1 #b111) (v 0) (op2 #b00) (opc ,int-opc)
                     (type #b11) ,@(when wreg-p '((rt nil :type 'wreg)))))
          (:printer format-ldst-imm9
                    ((z 0) (size ,int-size) (op1 #b111) (v 0) (op2 #b00) (opc ,int-opc)
                     (type #b01) ,@(when wreg-p '((rt nil :type 'wreg)))))
          (:printer format-ldst-reg
                    ((size ,int-size) (op1 #b111) (v 0) (op2 #b00) (opc ,int-opc)
                     (one 1) (op3 #b10) ,@(when wreg-p '((rt nil :type 'wreg)))))
          ,@(when lit-opc
              `((:printer format-ldr-literal
                          ((opc ,lit-opc) (op1 #b011) (v 0) (op2 #b00)
                           ,@(when wreg-p '((rt nil :type 'wreg)))))))
          ;; FP single-precision printers (v=1, size=#b10)
          ,@(unless wreg-p
              `((:printer format-ldst-uoffset
                          ((size #b10) (op1 #b111) (v 1) (op2 #b01) (opc ,int-opc)
                           (rt nil :type 'fp-reg-single)))
                (:printer format-ldst-imm9
                          ((z 0) (size #b10) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (type #b11) (rt nil :type 'fp-reg-single)))
                (:printer format-ldst-imm9
                          ((z 0) (size #b10) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (type #b01) (rt nil :type 'fp-reg-single)))
                (:printer format-ldst-reg
                          ((size #b10) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (one 1) (op3 #b10) (rt nil :type 'fp-reg-single)))
                ;; FP double-precision printers (v=1, size=#b11)
                (:printer format-ldst-uoffset
                          ((size #b11) (op1 #b111) (v 1) (op2 #b01) (opc ,int-opc)
                           (rt nil :type 'fp-reg-double)))
                (:printer format-ldst-imm9
                          ((z 0) (size #b11) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (type #b11) (rt nil :type 'fp-reg-double)))
                (:printer format-ldst-imm9
                          ((z 0) (size #b11) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (type #b01) (rt nil :type 'fp-reg-double)))
                (:printer format-ldst-reg
                          ((size #b11) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (one 1) (op3 #b10) (rt nil :type 'fp-reg-double)))
                ;; Literal loads for FP (only for ldr, not str)
                ,@(when lit-opc
                    `((:printer format-ldr-literal
                                ((opc #b01) (op1 #b011) (v 1) (op2 #b00)
                                 (rt nil :type 'fp-reg-single)))
                      (:printer format-ldr-literal
                                ((opc #b11) (op1 #b011) (v 1) (op2 #b00)
                                 (rt nil :type 'fp-reg-double)))))
          (:emitter
           ,@(if wreg-p
               ;; .w variant: integer only -- reject FP registers
               `((assert (not (or (sc-is rt single-reg) (sc-is rt double-reg)))
                         (rt) "~A does not accept FP registers." ',name)
                 (etypecase mem
                   (label
                    (emit-ldr-literal segment (reg-tn-encoding rt) ,lit-opc 0 mem))
                   (memory-ref
                    (emit-ldst-modes segment mem ,int-size 0 ,int-opc ,int-access-size
                                     (reg-tn-encoding rt)))))
               ;; Full variant: dispatch on register type
               `((etypecase mem
                   ,@(when lit-opc
                       `((label
                          (sc-case rt
                            (single-reg
                             (emit-ldr-literal segment (fp-reg-tn-encoding rt)
                                               #b01 1 mem))
                            (double-reg
                             (emit-ldr-literal segment (fp-reg-tn-encoding rt)
                                               #b11 1 mem))
                            (t
                             (emit-ldr-literal segment (reg-tn-encoding rt)
                                               ,lit-opc 0 mem))))))
                   (memory-ref
                    (sc-case rt
                      (single-reg
                       (emit-ldst-modes segment mem #b10 1 ,int-opc 4
                                        (fp-reg-tn-encoding rt)))
                      (double-reg
                       (emit-ldst-modes segment mem #b11 1 ,int-opc 8
                                        (fp-reg-tn-encoding rt)))
                      (t
                       (emit-ldst-modes segment mem ,int-size 0 ,int-opc ,int-access-size
                                        (reg-tn-encoding rt))))))))))))))
  ;;         name   int-size  int-opc  int-access  lit-opc  wreg-p
  (def str.w #b10   #b00      4        nil          t)    ; 32-bit integer store only
  (def ldr.w #b10   #b01      4        #b01         t)    ; 32-bit integer load
  (def str   #b11   #b00      8        nil          nil)  ; 64-bit / FP store
  (def ldr   #b11   #b01      8        #b10         nil)) ; 64-bit / FP load

;; Unscaled offset (LDUR/STUR): type field = #b00, imm9 is unscaled and signed.
;; Use these when the byte offset is not a multiple of the access size,
;; or when the offset is negative and small (-256..255).
;;
;; Examples:
;;   (inst ldur    x0 x1 -8)   ; LDUR X0, [X1, #-8]   -- 64-bit, negative offset
;;   (inst ldur    x0 x1 1)    ; LDUR X0, [X1, #1]    -- 64-bit, unaligned offset
;;   (inst ldur.w  w0 x1 -4)   ; LDUR W0, [X1, #-4]   -- 32-bit
;;   (inst ldurb   w0 x1 3)    ; LDURB W0, [X1, #3]   -- byte, unaligned
;;   (inst ldurh   w0 x1 -2)   ; LDURH W0, [X1, #-2]  -- halfword, negative
;;   (inst ldursw  x0 x1 -4)   ; LDURSW X0, [X1, #-4] -- sign-extend word -> X
;;   (inst stur    x0 x1 -8)   ; STUR X0, [X1, #-8]   -- 64-bit store
;;; Byte, halfword and sign-extending unscaled load/store.

(macrolet
    ((def (name size opc)
       `(define-instruction ,name (segment rt rn imm9)
          (:declare (type tn rt rn)
                    (type (signed-byte 9) imm9))
          (:printer format-ldst-imm9
                    ((z 0) (size ,size) (op1 #b111) (v 0) (op2 #b00) (opc ,opc)
                     (type #b00)))
          (:emitter
           (emit-format-ldst-imm9 segment ,size #b111 0 #b00 ,opc
                                   0 (ldb (byte 9 0) imm9)
                                   #b00
                                   (reg-tn-encoding rn)
                                   (reg-tn-encoding rt))))))
  (def sturb    #b00 #b00)
  (def ldurb    #b00 #b01)
  (def ldursb   #b00 #b10)   ; sign-extend byte -> X
  (def ldursb.w #b00 #b11)   ; sign-extend byte -> W
  (def sturh    #b01 #b00)
  (def ldurh    #b01 #b01)
  (def ldursh   #b01 #b10)   ; sign-extend halfword -> X
  (def ldursh.w #b01 #b11)   ; sign-extend halfword -> W
  (def ldursw   #b10 #b10))  ; sign-extend word -> X

;;; 32-bit and 64-bit unscaled load/store -- integer only (.w) and
;;; integer+FP (full).

(macrolet
    ((def (name int-size int-opc wreg-p)
       `(define-instruction ,name (segment rt rn imm9)
          (:declare (type tn rt rn)
                    (type (signed-byte 9) imm9))
          ;; Integer register printer (v=0)
          (:printer format-ldst-imm9
                    ((z 0) (size ,int-size) (op1 #b111) (v 0) (op2 #b00) (opc ,int-opc)
                     (type #b00) ,@(when wreg-p '((rt nil :type 'wreg)))))
          ;; FP single-precision printer (v=1, size=#b10)
          ,@(unless wreg-p
              `((:printer format-ldst-imm9
                          ((z 0) (size #b10) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (type #b00) (rt nil :type 'fp-reg-single)))
                ;; FP double-precision printer (v=1, size=#b11)
                (:printer format-ldst-imm9
                          ((size #b11) (op1 #b111) (v 1) (op2 #b00) (opc ,int-opc)
                           (type #b00) (rt nil :type 'fp-reg-double)))))
          (:emitter
           ,@(if wreg-p
               ;; .w variant: integer only
               `((assert (not (or (sc-is rt single-reg) (sc-is rt double-reg)))
                         (rt) "~A does not accept FP registers." ',name)
                 (emit-format-ldst-imm9 segment ,int-size #b111 0 #b00 ,int-opc
                                         0 (ldb (byte 9 0) imm9) #b00
                                         (reg-tn-encoding rn)
                                         (reg-tn-encoding rt)))
               ;; Full variant: dispatch on register type
               `((sc-case rt
                   (single-reg
                    (emit-format-ldst-imm9 segment #b10 #b111 1 #b00 ,int-opc
                                            0 (ldb (byte 9 0) imm9) #b00
                                            (reg-tn-encoding rn)
                                            (fp-reg-tn-encoding rt)))
                   (double-reg
                    (emit-format-ldst-imm9 segment #b11 #b111 1 #b00 ,int-opc
                                            0 (ldb (byte 9 0) imm9) #b00
                                            (reg-tn-encoding rn)
                                            (fp-reg-tn-encoding rt)))
                   (t
                    (emit-format-ldst-imm9 segment ,int-size #b111 0 #b00 ,int-opc
                                            0 (ldb (byte 9 0) imm9) #b00
                                            (reg-tn-encoding rn)
                                            (reg-tn-encoding rt))))))))))
  ;;         name     int-size  int-opc  wreg-p
  (def stur.w  #b10   #b00      t)    ; 32-bit integer store
  (def ldur.w  #b10   #b01      t)    ; 32-bit integer load
  (def stur    #b11   #b00      nil)  ; 64-bit / FP store
  (def ldur    #b11   #b01      nil)) ; 64-bit / FP load


;;;; Load/Store pair.
;;
;; Load or store two registers simultaneously.  The offset is a signed
;; byte offset scaled by the access size (8 bytes for 64-bit, 4 for 32-bit).
;; Addressing modes are specified via memory-ref constructors:
;;   (mem rn offset)      -> [Xn, #offset]   signed scaled offset (default)
;;   (pre-index rn off)   -> [Xn, #off]!     pre-index
;;   (post-index rn off)  -> [Xn], #off      post-index
;;
;; Examples:
;;   (inst ldp   x0 x1 (mem x2))           ; LDP X0, X1, [X2]
;;   (inst ldp   x0 x1 (mem x2 16))        ; LDP X0, X1, [X2, #16]
;;   (inst stp   x0 x1 (pre-index x2 -16)) ; STP X0, X1, [X2, #-16]!
;;   (inst ldp   x0 x1 (post-index x2 16)) ; LDP X0, X1, [X2], #16
;;   (inst ldp.w w0 w1 (mem x2 8))         ; LDP W0, W1, [X2, #8]
;;   (inst stp   x0 x1 (mem sp 0))         ; STP X0, X1, [SP]

;; opc: 00=32-bit int / FP single (vr=1), 01=FP double (vr=1), 10=64-bit int
;; index: #b01=post-index, #b11=pre-index, #b10=offset
(macrolet
    ((def (name int-opc l wreg-p)
       `(define-instruction ,name (segment rt rt2 rn imm7
                                   &optional (idx-type :offset))
          (:declare (type tn rt rt2 rn)
                    (type (signed-byte 7) imm7)
                    (type (member :pre :post :offset) idx-type))
          ;; Integer register printer
          (:printer format-ldst-pair
                    ((opc ,int-opc) (op1 #b101) (vr 0) (op2 0) (l ,l)
                     ,@(when wreg-p '((rt  nil :type 'wreg)
                                      (rt2 nil :type 'wreg)))))
          ;; FP single-precision printer (vr=1, opc=#b00)
          ,@(unless wreg-p
              `((:printer format-ldst-pair
                          ((opc #b00) (op1 #b101) (vr 1) (op2 0) (l ,l)
                           (rt  nil :type 'fp-reg-single)
                           (rt2 nil :type 'fp-reg-single)))
                ;; FP double-precision printer (vr=1, opc=#b01)
                (:printer format-ldst-pair
                          ((opc #b01) (op1 #b101) (vr 1) (op2 0) (l ,l)
                           (rt  nil :type 'fp-reg-double)
                           (rt2 nil :type 'fp-reg-double)))))
          (:emitter
           (let ((index (ecase idx-type
                          (:post   #b01)
                          (:pre    #b11)
                          (:offset #b10))))
             ,@(if wreg-p
                 ;; .w variant: integer only
                 `((assert (not (or (sc-is rt single-reg) (sc-is rt double-reg)))
                           (rt) "~A does not accept FP registers." ',name)
                   (emit-format-ldst-pair segment ,int-opc #b101 0 0 index ,l
                                          (ldb (byte 7 0) imm7)
                                          (reg-tn-encoding rt2)
                                          (reg-tn-encoding rn)
                                          (reg-tn-encoding rt)))
                 ;; Full variant: dispatch on register type
                 `((sc-case rt
                     (single-reg
                      (emit-format-ldst-pair segment #b00 #b101 1 0 index ,l
                                             (ldb (byte 7 0) imm7)
                                             (fp-reg-tn-encoding rt2)
                                             (reg-tn-encoding rn)
                                             (fp-reg-tn-encoding rt)))
                     (double-reg
                      (emit-format-ldst-pair segment #b01 #b101 1 0 index ,l
                                             (ldb (byte 7 0) imm7)
                                             (fp-reg-tn-encoding rt2)
                                             (reg-tn-encoding rn)
                                             (fp-reg-tn-encoding rt)))
                     (t
                      (emit-format-ldst-pair segment ,int-opc #b101 0 0 index ,l
                                             (ldb (byte 7 0) imm7)
                                             (reg-tn-encoding rt2)
                                             (reg-tn-encoding rn)
                                             (reg-tn-encoding rt)))))))))))
  ;;         name    int-opc  l   wreg-p
  (def stp.w  #b00   0   t)    ; store pair 32-bit integer only
  (def ldp.w  #b00   1   t)    ; load  pair 32-bit integer only
  (def stp    #b10   0   nil)  ; store pair 64-bit int / FP single / FP double
  (def ldp    #b10   1   nil)) ; load  pair 64-bit int / FP single / FP double


;;;; Load register (literal).

(defun emit-ldr-literal (segment rt opc v label)
  (emit-back-patch segment 4
    #'(lambda (segment posn)
        (let ((delta (- (label-position label) posn)))
          (assert (zerop (ldb (byte 2 0) delta)))
          (emit-format-ldr-literal segment opc #b011 v #b00
                                   (ldb (byte 19 0) (ash delta -2))
                                   rt)))))



;;;; Data Processing -- Register.

;;; Add/Sub (shifted register).

;; NEG Xd, Xm  =  SUB Xd, XZR, Xm


;;; Data Processing (2 sources): UDIV, SDIV, LSL/LSR/ASR/ROR (variable).

(macrolet
    ((def (name sf opc)
       `(define-instruction ,name (segment rd rn rm)
          (:declare (type tn rd rn rm))
          (:printer format-dp-2src
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11010110) (opc ,opc))
                    '(:name :tab rd ", " rn ", " rm))
          (:emitter
           (emit-format-dp-2src segment ,sf 0 0 #b11010110
                                 (reg-tn-encoding rm)
                                 ,opc
                                 (reg-tn-encoding rn)
                                 (reg-tn-encoding rd))))))
  (def udiv   1 #b000010)
  (def sdiv   1 #b000011)
  (def udiv.w  0 #b000010)
  (def sdiv.w  0 #b000011))

;; asrv/lsrv/lslv/rorv have no printer -- asr/lsr/lsl/ror own those bit patterns.
(macrolet
    ((def (name sf opc)
       `(define-instruction ,name (segment rd rn rm)
          (:declare (type tn rd rn rm))
          (:emitter
           (emit-format-dp-2src segment ,sf 0 0 #b11010110
                                 (reg-tn-encoding rm)
                                 ,opc
                                 (reg-tn-encoding rn)
                                 (reg-tn-encoding rd))))))
  (def asrv   1 #b001010)
  (def lsrv   1 #b001001)
  (def lslv   1 #b001000)
  (def rorv   1 #b001011)
  (def asrv.w  0 #b001010)
  (def lsrv.w  0 #b001001)
  (def lslv.w  0 #b001000)
  (def rorv.w  0 #b001011))


;;; Data Processing (1 source): RBIT, REV, CLZ, CLS.

(macrolet
    ((def (name sf opc)
       `(define-instruction ,name (segment rd rn)
          (:declare (type tn rd rn))
          (:printer format-dp-1src
                    ((sf ,sf) (one 1) (s 0) (op1 #b11010110) (opcode2 0)
                     (opc ,opc)))
          (:emitter
           (emit-format-dp-1src segment ,sf 1 0 #b11010110
                                 0 ,opc
                                 (reg-tn-encoding rn)
                                 (reg-tn-encoding rd))))))
  (def rbit   1 #b000000)
  (def rev16  1 #b000001)
  (def rev    1 #b000011)   ; REV64 in earlier ARM ARM
  (def clz    1 #b000100)
  (def cls    1 #b000101)
  (def rbit.w  0 #b000000)
  (def rev16.w 0 #b000001)
  (def rev.w   0 #b000010)   ; REV32
  (def clz.w   0 #b000100)
  (def cls.w   0 #b000101))


;;; Add/Sub with carry.

(macrolet
    ((def (name sf op s)
       `(define-instruction ,name (segment rd rn rm)
          (:declare (type tn rd rn rm))
          (:printer format-adc
                    ((sf ,sf) (op ,op) (s ,s) (op1 #b11010000)))
          (:emitter
           (emit-format-adc segment ,sf ,op ,s #b11010000
                             (reg-tn-encoding rm)
                             0
                             (reg-tn-encoding rn)
                             (reg-tn-encoding rd))))))
  (def adc  1 0 0)
  (def adcs 1 0 1)
  (def sbc  1 1 0)
  (def sbcs 1 1 1)
  (def adc.w  0 0 0)
  (def adcs.w 0 0 1)
  (def sbc.w  0 1 0)
  (def sbcs.w 0 1 1))

;; NGC Xd, Xm  =  SBC Xd, XZR, Xm
(define-instruction-macro ngc (rd rm)
  `(inst sbc ,rd null-tn ,rm))


;;; Conditional select.

(macrolet
    ((def (name sf op op2)
       `(define-instruction ,name (segment rd rn rm cond)
          (:declare (type tn rd rn rm) (type condition-code cond))
          (:printer format-cond-select
                    ((sf ,sf) (op ,op) (s 0) (op1 #b11010100) (op2 ,op2)))
          (:emitter
           (emit-format-cond-select segment ,sf ,op 0 #b11010100
                                     (reg-tn-encoding rm)
                                     (condition-code-encoding cond)
                                     ,op2
                                     (reg-tn-encoding rn)
                                     (reg-tn-encoding rd))))))
  (def csel    1 0 #b00)
  (def csinc   1 0 #b01)   ; CINC / CSET aliases use this
  (def csinv   1 1 #b00)   ; CINV / CSETM aliases use this
  (def csneg   1 1 #b01)
  ;; 32-bit variants
  (def csel.w  0 0 #b00)
  (def csinc.w 0 0 #b01)
  (def csinv.w 0 1 #b00)
  (def csneg.w 0 1 #b01))

;; CSET Xd, cond  =  CSINC Xd, XZR, XZR, invert(cond)
(defun invert-condition (cond)
  (let ((enc (condition-code-encoding cond)))
    (aref condition-code-name-vec (logxor enc 1))))

(define-instruction-macro cset (rd cond)
  `(inst csinc ,rd null-tn null-tn (invert-condition ,cond)))

(define-instruction-macro cset.w (rd cond)
  `(inst csinc.w ,rd null-tn null-tn (invert-condition ,cond)))

(define-instruction-macro cinc (rd rn cond)
  `(inst csinc ,rd ,rn ,rn (invert-condition ,cond)))

(define-instruction-macro cinc.w (rd rn cond)
  `(inst csinc.w ,rd ,rn ,rn (invert-condition ,cond)))


;;; Conditional compare.

(macrolet
    ((def (name sf op)
       `(define-instruction ,name (segment rn rm-or-imm nzcv cond)
          (:declare (type tn rn)
                    (type (or tn (unsigned-byte 5)) rm-or-imm)
                    (type (unsigned-byte 4) nzcv)
                    (type condition-code cond))
          (:printer format-cond-cmp-imm
                    ((op ,op) (one 1) (op1 #b11010010) (op2 #b10) (sf ,sf)
                     ,@(when (zerop sf) '((rn nil :type 'wreg)))))
          (:printer format-cond-cmp-reg
                    ((op ,op) (one 1) (op1 #b11010010) (op2 #b00) (sf ,sf)
                     ,@(when (zerop sf) '((rn nil :type 'wreg)
                                          (rm nil :type 'wreg)))))
          (:emitter
           (etypecase rm-or-imm
             (tn
              (emit-format-cond-cmp-reg segment ,sf ,op 1 #b11010010
                                         (reg-tn-encoding rm-or-imm)
                                         (condition-code-encoding cond)
                                         #b00
                                         (reg-tn-encoding rn)
                                         0 nzcv))
             (integer
              (emit-format-cond-cmp-imm segment ,sf ,op 1 #b11010010
                                         rm-or-imm
                                         (condition-code-encoding cond)
                                         #b10
                                         (reg-tn-encoding rn)
                                         0 nzcv)))))))
  (def ccmp   1 1)   ; conditional compare,          64-bit
  (def ccmp.w 0 1)   ; conditional compare,          32-bit
  (def ccmn   1 0)   ; conditional compare negative, 64-bit
  (def ccmn.w 0 0))  ; conditional compare negative, 32-bit


;;; Multiply / Multiply-accumulate (3-source).

;; MADD  Xd, Xn, Xm, Xa  =>  Xd = Xa + Xn*Xm
;; MSUB  Xd, Xn, Xm, Xa  =>  Xd = Xa - Xn*Xm
;; MUL   Xd, Xn, Xm      =   MADD Xd, Xn, Xm, XZR  (ra=31)
;; MNEG  Xd, Xn, Xm      =   MSUB Xd, Xn, Xm, XZR  (ra=31)

(macrolet
    ((def (name sf op54 op31 o0 mul-name)
       `(define-instruction ,name (segment rd rn rm ra)
          (:declare (type tn rd rn rm ra))
          (:printer format-dp-3src
                    ((sf ,sf) (op54 ,op54) (op1 #b11011)
                     (op31 ,op31) (o0 ,o0)))
          ,@(when mul-name
              `((:printer format-dp-3src
                          ((sf ,sf) (op54 ,op54) (op1 #b11011)
                           (op31 ,op31) (o0 ,o0) (ra 31))
                          '(:name :tab rd ", " rn ", " rm)
                          :print-name ',mul-name)))
          (:emitter
           (emit-format-dp-3src segment ,sf ,op54 #b11011 ,op31
                                 (reg-tn-encoding rm)
                                 ,o0
                                 (reg-tn-encoding ra)
                                 (reg-tn-encoding rn)
                                 (reg-tn-encoding rd))))))
  ;;           name    sf  op54  op31   o0  mul-name
  (def madd    1  0  #b000  0   mul)     ; ra=31 -> MUL
  (def msub    1  0  #b000  1   mneg)    ; ra=31 -> MNEG
  (def smaddl  1  0  #b001  0   smull)   ; ra=31 -> SMULL
  (def smsubl  1  0  #b001  1   smnegl)  ; ra=31 -> SMNEGL
  (def umaddl  1  0  #b101  0   umull)   ; ra=31 -> UMULL
  (def umsubl  1  0  #b101  1   umnegl)  ; ra=31 -> UMNEGL
  (def madd.w  0  0  #b000  0   mul.w)   ; ra=31 -> MUL.W
  (def msub.w  0  0  #b000  1   mneg.w)) ; ra=31 -> MNEG.W

;; MUL / MNEG -- aliases for MADD/MSUB with Ra = XZR.
(define-instruction-macro mul (rd rn rm)
  `(inst madd ,rd ,rn ,rm null-tn))

(define-instruction-macro mneg (rd rn rm)
  `(inst msub ,rd ,rn ,rm null-tn))

(define-instruction-macro mul.w (rd rn rm)
  `(inst madd.w ,rd ,rn ,rm null-tn))

(define-instruction-macro mneg.w (rd rn rm)
  `(inst msub.w ,rd ,rn ,rm null-tn))

;; SMULH Xd, Xn, Xm  -- high 64 bits of signed 64x64.
(define-instruction smulh (segment rd rn rm)
  (:declare (type tn rd rn rm))
  (:printer format-dp-3src
            ((sf 1) (op54 0) (op1 #b11011) (op31 #b010) (o0 0)))
  (:emitter
   (emit-format-dp-3src segment 1 0 #b11011 #b010
                         (reg-tn-encoding rm)
                         0 31
                         (reg-tn-encoding rn)
                         (reg-tn-encoding rd))))

(define-instruction umulh (segment rd rn rm)
  (:declare (type tn rd rn rm))
  (:printer format-dp-3src
            ((sf 1) (op54 0) (op1 #b11011) (op31 #b110) (o0 0)))
  (:emitter
   (emit-format-dp-3src segment 1 0 #b11011 #b110
                         (reg-tn-encoding rm)
                         0 31
                         (reg-tn-encoding rn)
                         (reg-tn-encoding rd))))


;;;; Floating-point instructions.

;; FP type field: 00=single, 01=double, 11=half.
(defconstant fp-type-single  #b00)
(defconstant fp-type-double  #b01)
(defconstant fp-type-half    #b11)

;;; FP unary (1-source).

(macrolet
    ((def (name opc)
       `(define-instruction ,name (segment rd rn)
          (:declare (type tn rd rn))
          (:printer format-fp-dp1
                    ((m 0) (zero 0) (s 0) (op1 #b11110)
                     (type 0) (one 1) (opc ,opc) (op2 #b10000)))
          (:printer format-fp-dp1
                    ((m 0) (zero 0) (s 0) (op1 #b11110)
                     (type 1) (one 1) (opc ,opc) (op2 #b10000)
                     (rn nil :type 'fp-reg-double)
                     (rd nil :type 'fp-reg-double)))
          (:emitter
           (sc-case rd
             (single-reg
              (emit-format-fp-dp1 segment 0 0 0 #b11110
                                   fp-type-single 1 ,opc #b10000
                                   (fp-reg-tn-encoding rn)
                                   (fp-reg-tn-encoding rd)))
             (double-reg
              (emit-format-fp-dp1 segment 0 0 0 #b11110
                                   fp-type-double 1 ,opc #b10000
                                   (fp-reg-tn-encoding rn)
                                   (fp-reg-tn-encoding rd))))))))
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

;; FCVT -- convert between FP precisions.
;; The type field encodes the source precision; opc encodes the destination:
;;   opc #b000100 = to-single, #b000101 = to-double.
(define-instruction fcvt (segment rd rn)
  (:declare (type tn rd rn))
  ;; single -> double  (type=0 src=single, opc=to-double)
  (:printer format-fp-dp1
            ((m 0) (zero 0) (s 0) (op1 #b11110)
             (type 0) (one 1) (opc #b000101) (op2 #b10000)
             (rd nil :type 'fp-reg-double)))
  ;; double -> single  (type=1 src=double, opc=to-single)
  (:printer format-fp-dp1
            ((m 0) (zero 0) (s 0) (op1 #b11110)
             (type 1) (one 1) (opc #b000100) (op2 #b10000)
             (rn nil :type 'fp-reg-double)
             (rd nil :type 'fp-reg-single)))
  (:emitter
   (sc-case rn
     (single-reg
      (sc-case rd
        (double-reg
         (emit-format-fp-dp1 segment 0 0 0 #b11110
                              fp-type-single 1 #b000101 #b10000
                              (fp-reg-tn-encoding rn)
                              (fp-reg-tn-encoding rd)))))
     (double-reg
      (sc-case rd
        (single-reg
         (emit-format-fp-dp1 segment 0 0 0 #b11110
                              fp-type-double 1 #b000100 #b10000
                              (fp-reg-tn-encoding rn)
                              (fp-reg-tn-encoding rd))))))))


;;; FP binary (2-source).

(macrolet
    ((def (name opc)
       `(define-instruction ,name (segment rd rn rm)
          (:declare (type tn rd rn rm))
          (:printer format-fp-dp2
                    ((m 0) (zero 0) (s 0) (op1 #b11110)
                     (type 0) (one 1) (opc ,opc) (op2 #b10)))
          (:printer format-fp-dp2
                    ((m 0) (zero 0) (s 0) (op1 #b11110)
                     (type 1) (one 1) (opc ,opc) (op2 #b10)
                     (rm nil :type 'fp-reg-double)
                     (rn nil :type 'fp-reg-double)
                     (rd nil :type 'fp-reg-double)))
          (:emitter
           (sc-case rd
             (single-reg
              (emit-format-fp-dp2 segment 0 0 0 #b11110
                                   fp-type-single 1
                                   (fp-reg-tn-encoding rm)
                                   ,opc #b10
                                   (fp-reg-tn-encoding rn)
                                   (fp-reg-tn-encoding rd)))
             (double-reg
              (emit-format-fp-dp2 segment 0 0 0 #b11110
                                   fp-type-double 1
                                   (fp-reg-tn-encoding rm)
                                   ,opc #b10
                                   (fp-reg-tn-encoding rn)
                                   (fp-reg-tn-encoding rd))))))))
  (def fmul   #b0000)
  (def fdiv   #b0001)
  (def fadd   #b0010)
  (def fsub   #b0011)
  (def fmax   #b0100)
  (def fmin   #b0101)
  (def fmaxnm #b0110)
  (def fminnm #b0111)
  (def fnmul  #b1000))


;;; FP fused multiply-accumulate (3-source).

(macrolet
    ((def (name o1 o0)
       `(define-instruction ,name (segment rd rn rm ra)
          (:declare (type tn rd rn rm ra))
          (:printer format-fp-dp3
                    ((m 0) (zero 0) (s 0) (op1 #b11111)
                     (type 0) (o1 ,o1) (o0 ,o0)))
          (:printer format-fp-dp3
                    ((m 0) (zero 0) (s 0) (op1 #b11111)
                     (type 1) (o1 ,o1) (o0 ,o0)
                     (rm nil :type 'fp-reg-double)
                     (ra nil :type 'fp-reg-double)
                     (rn nil :type 'fp-reg-double)
                     (rd nil :type 'fp-reg-double)))
          (:emitter
           (sc-case rd
             (single-reg
              (emit-format-fp-dp3 segment 0 0 0 #b11111
                                   fp-type-single ,o1
                                   (fp-reg-tn-encoding rm)
                                   ,o0
                                   (fp-reg-tn-encoding ra)
                                   (fp-reg-tn-encoding rn)
                                   (fp-reg-tn-encoding rd)))
             (double-reg
              (emit-format-fp-dp3 segment 0 0 0 #b11111
                                   fp-type-double ,o1
                                   (fp-reg-tn-encoding rm)
                                   ,o0
                                   (fp-reg-tn-encoding ra)
                                   (fp-reg-tn-encoding rn)
                                   (fp-reg-tn-encoding rd))))))))
  (def fmadd  0 0)
  (def fmsub  0 1)
  (def fnmadd 1 0)
  (def fnmsub 1 1))


;;; FP compare.

(macrolet
    ((def (name opc2)
       `(define-instruction ,name (segment rn rm-or-zero)
          (:declare (type tn rn)
                    (type (or tn (member 0.0 0.0d0)) rm-or-zero))
          ;; single, register compare
          (:printer format-fp-cmp
                    ((m 0) (op0 0) (op1 #b11110) (type 0) (one 1)
                     (op #b00) (op2 #b1000) (opc2 ,opc2)))
          ;; double, register compare
          (:printer format-fp-cmp
                    ((m 0) (op0 0) (op1 #b11110) (type 1) (one 1)
                     (op #b00) (op2 #b1000) (opc2 ,opc2)
                     (rm nil :type 'fp-reg-double)
                     (rn nil :type 'fp-reg-double)))
          ;; single, zero compare
          (:printer format-fp-cmp
                    ((m 0) (op0 0) (op1 #b11110) (type 0) (one 1)
                     (op #b00) (op2 #b1000) (opc2 ,(logior opc2 #b01000))))
          ;; double, zero compare
          (:printer format-fp-cmp
                    ((m 0) (op0 0) (op1 #b11110) (type 1) (one 1)
                     (op #b00) (op2 #b1000) (opc2 ,(logior opc2 #b01000))
                     (rm nil :type 'fp-reg-double)
                     (rn nil :type 'fp-reg-double)))
          (:emitter
           (let* ((zerop (not (typep rm-or-zero 'tn)))
                  (rm    (if zerop 0 (fp-reg-tn-encoding rm-or-zero)))
                  (opc   (if zerop (logior ,opc2 #b01000) ,opc2)))
             (sc-case rn
               (single-reg
                (emit-format-fp-cmp segment 0 0 #b11110 fp-type-single 1
                                     rm #b00 #b1000
                                     (fp-reg-tn-encoding rn)
                                     opc))
               (double-reg
                (emit-format-fp-cmp segment 0 0 #b11110 fp-type-double 1
                                     rm #b00 #b1000
                                     (fp-reg-tn-encoding rn)
                                     opc))))))))
  (def fcmp  #b00000)
  (def fcmpe #b10000))   ; also signals invalid-op on QNaN


;;; FP conditional select.

(define-instruction fcsel (segment rd rn rm cond)
  (:declare (type tn rd rn rm) (type condition-code cond))
  (:printer format-fp-csel
            ((op0 0) (s 0) (op1 #b11110) (type 0) (one 1) (op2 #b11)))
  (:printer format-fp-csel
            ((op0 0) (s 0) (op1 #b11110) (type 1) (one 1) (op2 #b11)
             (rm nil :type 'fp-reg-double)
             (rn nil :type 'fp-reg-double)
             (rd nil :type 'fp-reg-double)))
  (:emitter
   (sc-case rd
     (single-reg
      (emit-format-fp-csel segment 0 0 #b11110 fp-type-single 1
                            (fp-reg-tn-encoding rm)
                            (condition-code-encoding cond)
                            #b11
                            (fp-reg-tn-encoding rn)
                            (fp-reg-tn-encoding rd)))
     (double-reg
      (emit-format-fp-csel segment 0 0 #b11110 fp-type-double 1
                            (fp-reg-tn-encoding rm)
                            (condition-code-encoding cond)
                            #b11
                            (fp-reg-tn-encoding rn)
                            (fp-reg-tn-encoding rd))))))


;;; FP <-> integer conversion.
;;;
;;; rmode: 00=nearest, 01=+inf, 10=-inf, 11=zero
;;; opc:   000=FCVT*W, 001=FCVT*X, 010=SCVTF, 011=UCVTF, 100=FCVTAS*, 101=FCVTAU*

;; FCVTZS / FCVTZU: FP -> signed/unsigned integer, round toward zero.
;; fcvtzs / fcvtzu     : result in X register (sf=1)
;; fcvtzs.w / fcvtzu.w : result in W register (sf=0)
(macrolet
    ((def (name sf opc)
       `(define-instruction ,name (segment rd rn)
          (:declare (type tn rd rn))
          ;; single -> integer
          (:printer format-fp-int-cvt
                    ((zero 0) (s 0) (op1 #b11110)
                     (sf ,sf) (type 0) (one 1) (rmode #b11) (opc ,opc) (zero2 0)))
          ;; double -> integer
          (:printer format-fp-int-cvt
                    ((zero 0) (s 0) (op1 #b11110)
                     (sf ,sf) (type 1) (one 1) (rmode #b11) (opc ,opc) (zero2 0)
                     (rn nil :type 'fp-reg-double)))
          (:emitter
           (let ((fp-type (sc-case rn
                            (single-reg fp-type-single)
                            (double-reg fp-type-double))))
             (emit-format-fp-int-cvt segment ,sf 0 0 #b11110
                                      fp-type 1 #b11 ,opc 0
                                      (fp-reg-tn-encoding rn)
                                      (reg-tn-encoding rd)))))))
  (def fcvtzs   1 #b000)   ; signed,   X result
  (def fcvtzs.w 0 #b000)   ; signed,   W result
  (def fcvtzu   1 #b001)   ; unsigned, X result
  (def fcvtzu.w 0 #b001))  ; unsigned, W result

;; SCVTF / UCVTF: integer -> FP.
;; scvtf / ucvtf     : 64-bit integer source (X register, sf=1)
;; scvtf.w / ucvtf.w : 32-bit integer source (W register, sf=0)
(macrolet
    ((def (name sf opc)
       `(define-instruction ,name (segment rd rn)
          (:declare (type tn rd rn))
          (:printer format-fp-int-cvt
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11110)
                     (type fp-type-single) (one 1) (rmode 0) (opc ,opc) (zero2 0)))
          (:printer format-fp-int-cvt
                    ((sf ,sf) (zero 0) (s 0) (op1 #b11110)
                     (type fp-type-double) (one 1) (rmode 0) (opc ,opc) (zero2 0)))
          (:emitter
           (sc-case rd
             (single-reg
              (emit-format-fp-int-cvt segment ,sf 0 0 #b11110
                                       fp-type-single 1 0 ,opc 0
                                       (reg-tn-encoding rn)
                                       (fp-reg-tn-encoding rd)))
             (double-reg
              (emit-format-fp-int-cvt segment ,sf 0 0 #b11110
                                       fp-type-double 1 0 ,opc 0
                                       (reg-tn-encoding rn)
                                       (fp-reg-tn-encoding rd))))))))
  (def scvtf   1 #b010)   ; signed,   X source
  (def scvtf.w 0 #b010)   ; signed,   W source
  (def ucvtf   1 #b011)   ; unsigned, X source
  (def ucvtf.w 0 #b011))  ; unsigned, W source

;; FMOV -- move between FP registers, between FP and integer registers,
;; or load a floating-point immediate.
;;
;; Dispatch on rd and rn types:
;;   single-reg <- single-reg       : FP copy  (format-fp-dp1,     type=S)
;;   double-reg <- double-reg       : FP copy  (format-fp-dp1,     type=D)
;;   single-reg <- registers        : W->S     (format-fp-int-cvt, sf=0, opc=#b110)
;;   registers  <- single-reg       : S->W     (format-fp-int-cvt, sf=0, opc=#b111)
;;   double-reg <- registers        : X->D     (format-fp-int-cvt, sf=1, opc=#b110)
;;   registers  <- double-reg       : D->X     (format-fp-int-cvt, sf=1, opc=#b111)
;;   single-reg <- single/double-float : imm   (format-fp-imm,     type=S)
;;   double-reg <- single/double-float : imm   (format-fp-imm,     type=D)

(define-instruction fmov (segment rd rn)
  (:declare (type tn rd)
            (type (or tn single-float double-float) rn))
  ;; FP->FP single
  (:printer format-fp-dp1
            ((m 0) (zero 0) (s 0) (op1 #b11110)
             (type 0) (one 1) (opc #b000000) (op2 #b10000)))
  ;; FP->FP double
  (:printer format-fp-dp1
            ((m 0) (zero 0) (s 0) (op1 #b11110)
             (type 1) (one 1) (opc #b000000) (op2 #b10000)
             (rn nil :type 'fp-reg-double)
             (rd nil :type 'fp-reg-double)))
  ;; W->S
  (:printer format-fp-int-cvt
            ((op1 #b11110) (sf 0) (type fp-type-single) (rmode 0) (opc #b110)))
  ;; S->W
  (:printer format-fp-int-cvt
            ((op1 #b11110) (sf 0) (type fp-type-single) (rmode 0) (opc #b111)))
  ;; X->D
  (:printer format-fp-int-cvt
            ((op1 #b11110) (sf 1) (type fp-type-double) (rmode 0) (opc #b110)))
  ;; D->X
  (:printer format-fp-int-cvt
            ((op1 #b11110) (sf 1) (type fp-type-double) (rmode 0) (opc #b111)))
  ;; immediate -> single
  (:printer format-fp-imm ((op1 #b11110) (type fp-type-single)))
  ;; immediate -> double
  (:printer format-fp-imm
            ((op1 #b11110) (type fp-type-double)
             (rd nil :type 'fp-reg-double)))
  (:emitter
   (if (typep rn '(or single-float double-float))
     (let ((imm8 (encode-fp-immediate rn)))
       (unless imm8
         (error "Cannot encode ~S as an FP immediate." rn))
       (sc-case rd
         (single-reg
          (emit-format-fp-imm segment 0 0 #b11110 fp-type-single
                               1 imm8 #b100 0 (fp-reg-tn-encoding rd)))
         (double-reg
          (emit-format-fp-imm segment 0 0 #b11110 fp-type-double
                               1 imm8 #b100 0 (fp-reg-tn-encoding rd)))))
     (sc-case rd
       (single-reg
        (sc-case rn
          (single-reg                          ; S <- S
           (emit-format-fp-dp1 segment 0 0 0 #b11110
                                fp-type-single 1 #b000000 #b10000
                                (fp-reg-tn-encoding rn)
                                (fp-reg-tn-encoding rd)))
          (non-descriptor-reg                             ; S <- W
           (emit-format-fp-int-cvt segment 0 0 0 #b11110
                                    fp-type-single 1 0 #b110 0
                                    (reg-tn-encoding rn)
                                    (fp-reg-tn-encoding rd)))))
       (double-reg
        (sc-case rn
          (double-reg                          ; D <- D
           (emit-format-fp-dp1 segment 0 0 0 #b11110
                                fp-type-double 1 #b000000 #b10000
                                (fp-reg-tn-encoding rn)
                                (fp-reg-tn-encoding rd)))
          (non-descriptor-reg                             ; D <- X
           (emit-format-fp-int-cvt segment 1 0 0 #b11110
                                    fp-type-double 1 0 #b110 0
                                    (reg-tn-encoding rn)
                                    (fp-reg-tn-encoding rd)))))
       (non-descriptor-reg
        (sc-case rn
          (single-reg                          ; W <- S
           (emit-format-fp-int-cvt segment 0 0 0 #b11110
                                    fp-type-single 1 0 #b111 0
                                    (fp-reg-tn-encoding rn)
                                    (reg-tn-encoding rd)))
          (double-reg                          ; X <- D
           (emit-format-fp-int-cvt segment 1 0 0 #b11110
                                    fp-type-double 1 0 #b111 0
                                    (fp-reg-tn-encoding rn)
                                    (reg-tn-encoding rd)))))))))


;;;; Instructions for dumping data and header objects.

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:delay 0)
  (:emitter
   (emit-word segment word)))

(define-instruction short (segment short)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) short))
  :pinned
  (:delay 0)
  (:emitter
   (emit-short segment short)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))

(define-emitter emit-header-object 32
  (byte 24 8) (byte 8 0))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
                  (logior type
                          (ash (+ posn (component-header-length))
                               (- type-bits word-shift)))))))

(define-instruction function-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment function-header-type)))

(define-instruction lra-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-header-type)))


;;;; Instructions for converting between code objects, functions, and lras.

(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (declare (ignore segment posn delta-if-after))
       nil)
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
         (assemble (segment vop)
           ;; Load the 64-bit delta using MOVZ + MOVK sequence.
           (inst movz temp (ldb (byte 16  0) delta))
           (inst movk temp (ldb (byte 16 16) delta) :lsl 16)
           (inst movk temp (ldb (byte 16 32) delta) :lsl 32)
           (inst movk temp (ldb (byte 16 48) delta) :lsl 48)
           (inst add dst src temp))))))

;; code = fn - fn-ptr-type - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- other-pointer-type
                             function-pointer-type
                             (label-position label posn delta-if-after)
                             (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- (+ (label-position label posn delta-if-after)
                                (component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (+ (label-position label posn delta-if-after)
                             (component-header-length))))))


;;;; Pseudo-instruction macros.

;; LI (load immediate) -- chooses the shortest encoding.
(defun %li (reg value)
  (etypecase value
    ((or (signed-byte 64) (unsigned-byte 64))
     (let ((hw0 (ldb (byte 16  0) value))
           (hw1 (ldb (byte 16 16) value))
           (hw2 (ldb (byte 16 32) value))
           (hw3 (ldb (byte 16 48) value)))
       (cond
         ;; Small non-negative value: single MOVZ.
         ((and (zerop hw1) (zerop hw2) (zerop hw3))
          (inst movz reg hw0))
         ;; All-ones (negative -1): MOVN #0.
         ((= value -1)
          (inst movn reg 0))
         ;; General case: MOVZ then MOVK for non-zero halfwords.
         (t
          (inst movz reg hw0)
          (unless (zerop hw1) (inst movk reg hw1 :lsl 16))
          (unless (zerop hw2) (inst movk reg hw2 :lsl 32))
          (unless (zerop hw3) (inst movk reg hw3 :lsl 48))))))
    (fixup
     (inst movz reg value)
     (inst movk reg value :lsl 16))))

(define-instruction-macro li (reg value)
  `(%li ,reg ,value))
