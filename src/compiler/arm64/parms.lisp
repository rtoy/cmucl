;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/parms.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the ARM64 (AArch64) architecture.  This file is
;;; separate from other stuff so that it can be compiled and loaded
;;; earlier.
;;;
;;; Written by Rob MacLachlan (original SPARC version).
;;; Converted to ARM64 by [ARM64 port contributors].
;;; Derived from the SPARC and PPC ports.
;;;

(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")
(use-package "C")


;;;; Compiler constants.

(eval-when (compile eval load)

(setf (backend-name *target-backend*) "ARM64")
(setf (backend-version *target-backend*) "ARM64/AArch64")
(setf (backend-fasl-file-type *target-backend*) "arm64f")
(setf (backend-fasl-file-implementation *target-backend*)
      arm64-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) byte-fasl-file-version)
(setf (backend-register-save-penalty *target-backend*) 3)
;; AArch64 is a little-endian architecture by default (BE8 mode exists
;; but is uncommon and not targeted here).
(setf (backend-byte-order *target-backend*) :little-endian)
(setf (backend-page-size *target-backend*) 4096)

;;; Foreign linkage space.  The start address must match arm64-validate.h
;;; and the entry size must agree with arm64-arch.c.  Each entry is a
;;; ADRP + ADD + BR sequence (3 instructions = 12 bytes), rounded up to
;;; 16 bytes for alignment.
(setf (c::backend-foreign-linkage-space-start *target-backend*)
      #x0f800000
      (c::backend-foreign-linkage-entry-size *target-backend*)
      16)

); eval-when

(pushnew :new-assembler *features*)


;;;; Machine Architecture parameters:

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(word-bits byte-bits char-bits word-shift word-bytes char-bytes
	  fixnum-tag-bits fixnum-tag-mask positive-fixnum-bits

	  float-sign-shift

	  single-float-bytes
	  single-float-bias single-float-exponent-byte
	  single-float-significand-byte single-float-normal-exponent-min
	  single-float-normal-exponent-max single-float-hidden-bit
	  single-float-trapping-nan-bit single-float-digits

	  double-float-bytes
	  double-float-bias double-float-exponent-byte
	  double-float-significand-byte double-float-normal-exponent-min
	  double-float-normal-exponent-max double-float-hidden-bit
	  double-float-trapping-nan-bit double-float-digits

	  float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit))

#+double-double
(export '(double-double-float-digits))
) ; eval-when


(eval-when (compile load eval)

;;; AArch64 is a 64-bit architecture: one Lisp descriptor occupies one
;;; 64-bit word.
(defconstant word-bits 64
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant char-bits #-unicode 8 #+unicode 16
  "Number of bits needed to represent a character")

(defconstant char-bytes (truncate char-bits byte-bits)
  "Number of bytes needed to represent a character")

;;; word-shift = log2(word-bytes) = log2(8) = 3
(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  "Number of bytes in a word.")

;;; AArch64 uses the same 3-bit lowtag scheme as SPARC/PPC.
(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  "Mask to extract the low tag bits from a pointer.")

(defconstant lowtag-limit (ash 1 lowtag-bits)
  "Exclusive upper bound on the value of the low tag bits from a pointer.")

(defconstant fixnum-tag-bits (1- lowtag-bits)
  "Number of tag bits used for a fixnum")

(defconstant fixnum-tag-mask (1- (ash 1 fixnum-tag-bits))
  "Mask to get the fixnum tag")

(defconstant positive-fixnum-bits (- 32 fixnum-tag-bits 1)
  "Maximum number of bits in a positive fixnum.
  Although the machine word is 64 bits, fixnums are kept within a
  signed 32-bit range for compatibility with the rest of CMU CL.")

;;; Float layout constants.  IEEE 754 applies uniformly across
;;; architectures; only the sign-bit position changes with word width.
;;; For AArch64 (64-bit word) we use bit 63 as the float sign bit, but
;;; for the 32-bit single-float representation the sign is still bit 31
;;; within the 32-bit encoding word.
(defconstant float-sign-shift 31)

;;; Single-float (32-bit IEEE 754 binary32).  Identical to SPARC/PPC.
(defconstant single-float-bytes 4)
(defconstant single-float-bias 126)
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

;;; Double-float (64-bit IEEE 754 binary64).  Identical to SPARC/PPC.
(defconstant double-float-bytes 8)
(defconstant double-float-bias 1022)
(defconstant double-float-exponent-byte (byte 11 20))
(defconstant double-float-significand-byte (byte 20 0))
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))
(defconstant double-float-trapping-nan-bit (ash 1 19))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

;;; double-float-digits = 20 + 32 + 1 = 53 (IEEE 754 binary64).
;;; The literal 32 matches the SPARC formula; word-bits cannot be used
;;; here because on ARM64 word-bits is 64, which would give the wrong answer.
(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) 32 1))

#+double-double
(defconstant double-double-float-digits
  (* 2 double-float-digits))

;;; AArch64 FPCR/FPSR trap and rounding-mode bits.
;;;
;;; FPCR layout (Floating-Point Control Register):
;;;   [26]   IDE  – Input Denormal exception trap enable
;;;   [25]   IXE  – Inexact trap enable
;;;   [24]   UFE  – Underflow trap enable
;;;   [23]   OFE  – Overflow trap enable
;;;   [22]   DZE  – Divide-by-Zero trap enable
;;;   [21]   IOE  – Invalid Operation trap enable
;;;   [23:22] RMode – Rounding mode
;;;
;;; FPSR layout (Floating-Point Status Register):
;;;   [4]    QC   – Cumulative saturation (SIMD)
;;;   [3]    IDC  – Input Denormal cumulative
;;;   [2:0]  –    (reserved)
;;;   Bits 0-4 of the low byte carry the IEEE exception sticky flags:
;;;   [4]    IXC  – Inexact
;;;   [3]    UFC  – Underflow
;;;   [2]    OFC  – Overflow
;;;   [1]    DZC  – Divide-by-Zero
;;;   [0]    IOC  – Invalid Operation
;;;
;;; The trap-enable bits in FPCR use the same bit positions as FPSR
;;; sticky flags, offset by 8.  We follow the SPARC convention and
;;; name the FPCR trap-enable fields here; the runtime uses them when
;;; installing/querying the floating-point environment.

;;; Exception / trap-enable bit positions within their respective
;;; FPCR byte (bits 8..13) and FPSR byte (bits 0..4).
(defconstant float-inexact-trap-bit        (ash 1 4))  ; IXE / IXC
(defconstant float-underflow-trap-bit      (ash 1 3))  ; UFE / UFC
(defconstant float-overflow-trap-bit       (ash 1 2))  ; OFE / OFC
(defconstant float-divide-by-zero-trap-bit (ash 1 1))  ; DZE / DZC
(defconstant float-invalid-trap-bit        (ash 1 0))  ; IOE / IOC
(defconstant float-imprecise-trap-bit float-inexact-trap-bit)

;;; Rounding mode encoding in FPCR bits [23:22].
(defconstant float-round-to-nearest  0)   ; RN  (ties to even)
(defconstant float-round-to-positive 1)   ; RP
(defconstant float-round-to-negative 2)   ; RM
(defconstant float-round-to-zero     3)   ; RZ

;;; Byte descriptors for fields within the 32-bit FPCR value.
(defconstant float-rounding-mode   (byte 2 22))  ; RMode field
(defconstant float-traps-byte      (byte 6  8))  ; trap-enable bits 8..13
(defconstant float-exceptions-byte (byte 6  0))  ; FPSR sticky bits 0..5
(defconstant float-sticky-bits     (byte 6  0))  ; alias for exceptions

;;; Flush-to-zero / fast mode bit in FPCR.
;;; When set, AArch64 flushes denormal inputs/outputs to zero
;;; (equivalent to the SPARC EFM bit).
(defconstant float-fast-bit (ash 1 24))    ; FZ bit in FPCR

); eval-when

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.
;;; AArch64 does not have SPARC-style register windows, so no window-
;;; spill area is needed.  We reserve one word (8 bytes) as a
;;; red-zone / alignment pad to match common ABI expectations.
(defconstant number-stack-displacement
  (* 1 vm:word-bytes))


;;;; Description of the target address space.

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
	  target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))
)

;;; Where to put the different spaces.  Must match the C code (arm64-validate.h)!
;;;
;;; AArch64 virtual address space is 48 bits (256 TiB) with the low
;;; half available to user-space processes.  We place the Lisp spaces
;;; in the first gigabyte, mirroring the SPARC layout but noting that
;;; on AArch64 the upper 16 bits of a 64-bit address must match bit 47
;;; (tagged-address extension); user-space addresses are therefore in
;;; the range 0x0000_0000_0000_0000 – 0x0000_7FFF_FFFF_FFFF.
(defconstant target-read-only-space-start #x10000000)
(defconstant target-static-space-start    #x28000000)
(defconstant target-dynamic-space-start   #x40000000)

(defconstant target-foreign-linkage-space-start
  (c:backend-foreign-linkage-space-start *target-backend*))
(defconstant target-foreign-linkage-entry-size
  (c:backend-foreign-linkage-entry-size *target-backend*))


;;;; Other random constants.

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(halt-trap pending-interrupt-trap error-trap cerror-trap
	  breakpoint-trap function-end-breakpoint-trap
	  after-breakpoint-trap allocation-trap
	  pseudo-atomic-trap
	  object-not-list-trap object-not-instance-trap
	  trace-table-normal trace-table-call-site
	  trace-table-function-prologue trace-table-function-epilogue))

#+heap-overflow-check
(export '(dynamic-space-overflow-error-trap
	  dynamic-space-overflow-warning-trap))
)

;;; Trap codes are encoded as the immediate operand of the UDF (Undefined
;;; instruction) used to signal synchronous traps to the runtime.  The
;;; encoding must match arm64-arch.c.
;;;
;;; We start at 8 (same as SPARC) to leave room for low values that may
;;; be used by the OS or debugger.
(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  after-breakpoint
  #+heap-overflow-check
  dynamic-space-overflow-warning
  #+heap-overflow-check
  dynamic-space-overflow-error
  )

;; Make sure this starts AFTER the last element of the above enum!
(defenum (:prefix object-not- :suffix -trap :start 16)
  list
  instance)

(defenum (:prefix trace-table-)
  normal
  call-site
  function-prologue
  function-epilogue)


;;;; Static symbols.

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(static-symbols static-functions))
)

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defparameter static-symbols
  '(t

    ;; The C startup code must fill these in.
    lisp::lisp-environment-list
    lisp::lisp-command-line-list
    ext::*batch-mode*
    lisp::*initial-fdefn-objects*

    ;; Functions that the C code needs to call
    lisp::%initial-function
    lisp::maybe-gc
    kernel::internal-error
    #+stack-checking kernel::yellow-zone-hit
    #+stack-checking kernel::red-zone-hit
    #+heap-overflow-check kernel::dynamic-space-overflow-warning-hit
    #+heap-overflow-check kernel::dynamic-space-overflow-error-hit
    di::handle-breakpoint
    lisp::fdefinition-object

    ;; Free Pointers.
    lisp::*read-only-space-free-pointer*
    lisp::*static-space-free-pointer*
    lisp::*initial-dynamic-space-free-pointer*

    ;; Things needed for non-local-exit.
    lisp::*current-catch-block*
    lisp::*current-unwind-protect-block*
    *eval-stack-top*

    ;; Interrupt Handling
    lisp::*free-interrupt-context-index*
    unix::*interrupts-enabled*
    unix::*interrupt-pending*

    ;; Foreign linkage stuff
    #+linkage-table
    lisp::*linkage-table-data*

    ;;
    lisp::*cmucl-lib*
    lisp::*cmucl-core-path*

    ;; Gencgc
    #+gencgc
    *current-region-free-pointer*
    ;; current-region-end-addr is a 64-bit physical address; when read
    ;; back from Lisp it is a fixnum shifted left by fixnum-tag-bits.
    #+gencgc
    *current-region-end-addr*

    #+gencgc
    *scavenge-read-only-space*

    ;; Types of weak hash tables
    :key
    :value
    :key-and-value
    :key-or-value

    ;; FP constants
    *fp-constant-0d0*
    *fp-constant-0f0*

    lisp::*unidata-path*
    lisp::*lisp-implementation-version*

    ;; Some spare static symbols.  Useful for adding another static
    ;; symbol without having to do a cross-compile.  Just rename one
    ;; of these to the desired name.
    spare-8
    spare-7
    spare-6
    spare-5
    spare-4
    spare-3
    spare-2
    spare-1
    spare-0
    ))

(defparameter static-functions
  '(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor
    two-arg-gcd two-arg-lcm
    ))


;;;; Assembler parameters:

;;; The number of bits per element in the assembler's code vector.
;;; AArch64 instructions are always 32 bits wide (4 bytes), but the
;;; assembler code-vector element unit is still 8-bit bytes, matching
;;; every other CMU CL port.
(defparameter *assembly-unit-length* 8)


(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(pseudo-atomic-trap allocation-trap
	  pseudo-atomic-value pseudo-atomic-interrupted-value))
)

;;;; Pseudo-atomic trap number.
;;;;
;;;; On AArch64 we encode synchronous traps with UDF (permanently
;;;; undefined instruction), whose 16-bit immediate is the trap code.
;;;; There is no software-trap instruction analogous to SPARC's TRAP,
;;;; so pseudo-atomic-trap is an alias for the pending-interrupt-trap
;;;; UDF code.  The value must match arm64-arch.c.
(defconstant pseudo-atomic-trap pending-interrupt-trap)

;;;; Allocation trap number.
;;;;
;;;; This is the UDF immediate used when inline allocation overflows
;;;; the current region.  Must match arm64-arch.c.
(defconstant allocation-trap
  ;; allocation-trap is encoded as the UDF immediate in macros.lisp.
  ;; Re-export the value used there so C code and Lisp agree.
  ;;
  ;; The numeric value 31 (same as SPARC) is chosen to be distinct from
  ;; all values in the halt..after-breakpoint range (8..14) and the
  ;; object-not-*-trap range (16..17).
  31)

;;;; Pseudo-atomic flag
;;;;
;;;; This value is ORed into ALLOC-TN (X14) to mark a pseudo-atomic
;;;; section, matching the SPARC and PPC convention of using the bit
;;;; just below the lowtag field.
(defconstant pseudo-atomic-value (ash 1 (1- vm::lowtag-bits)))

;;;; Pseudo-atomic-interrupted-mask
;;;;
;;;; AArch64 (like SPARC) signals a pseudo-atomic interrupt via the
;;;; least-significant bit of ALLOC-TN.
(defconstant pseudo-atomic-interrupted-value 1)
