;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/parms.lisp,v 1.37 2002/10/25 21:49:08 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the SPARC.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted to MIPS by William Lott.
;;;

(in-package "SPARC")
(use-package "C")


;;;; Compiler constants.

(eval-when (compile eval load)

#+svr4
(adjoin :svr4 (backend-features *target-backend*))
#+svr4
(adjoin :solaris (backend-features *target-backend*))
(setf (backend-name *target-backend*) "SPARC")
#+svr4
(setf (backend-version *target-backend*) "SPARCstation/Solaris 2")
#-svr4
(setf (backend-version *target-backend*) "SPARCstation/Sun 4")
(setf (backend-fasl-file-type *target-backend*) "sparcf")
(setf (backend-fasl-file-implementation *target-backend*)
      sparc-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) #x18d)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) :big-endian)
(setf (backend-page-size *target-backend*)
      #+mach 4096 #+sunos 8192)

); eval-when

(pushnew :new-assembler *features*)


;;;; Machine Architecture parameters:

(export '(word-bits byte-bits word-shift word-bytes 
	  fixnum-tag-bits fixnum-tag-mask positive-fixnum-bits

	  float-sign-shift
	  
	  single-float-bias single-float-exponent-byte
	  single-float-significand-byte single-float-normal-exponent-min
	  single-float-normal-exponent-max single-float-hidden-bit
	  single-float-trapping-nan-bit single-float-digits

	  double-float-bias double-float-exponent-byte
	  double-float-significand-byte double-float-normal-exponent-min
	  double-float-normal-exponent-max double-float-hidden-bit
	  double-float-trapping-nan-bit double-float-digits

	  long-float-bias long-float-exponent-byte
	  long-float-significand-byte long-float-normal-exponent-min
	  long-float-normal-exponent-max long-float-hidden-bit
	  long-float-trapping-nan-bit long-float-digits

	  float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit))

	  

(eval-when (compile load eval)

(defconstant word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  "Number of bytes in a word.")

(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  "Mask to extract the low tag bits from a pointer.")
  
(defconstant lowtag-limit (ash 1 lowtag-bits)
  "Exclusive upper bound on the value of the low tag bits from a
  pointer.")

(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant fixnum-tag-bits (1- lowtag-bits)
  "Number of tag bits used for a fixnum")

(defconstant fixnum-tag-mask (1- (ash 1 fixnum-tag-bits))
  "Mask to get the fixnum tag")

(defconstant positive-fixnum-bits (- word-bits fixnum-tag-bits 1)
  "Maximum number of bits in a positive fixnum")

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant double-float-bias 1022)
(defconstant double-float-exponent-byte (byte 11 20))
(defconstant double-float-significand-byte (byte 20 0))
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))
(defconstant double-float-trapping-nan-bit (ash 1 19))

;;; X These values are for the x86 80 bit format and are no doubt
;;; incorrect for the sparc.
(defconstant long-float-bias 16382)
(defconstant long-float-exponent-byte (byte 15 0))
(defconstant long-float-significand-byte (byte 31 0))
(defconstant long-float-normal-exponent-min 1)
(defconstant long-float-normal-exponent-max #x7FFE)
(defconstant long-float-hidden-bit (ash 1 31))
(defconstant long-float-trapping-nan-bit (ash 1 30))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) word-bits 1))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant float-rounding-mode (byte 2 30))	  ; RD 
(defconstant float-sticky-bits (byte 5 5))	  ; aexc
(defconstant float-traps-byte (byte 5 23))	  ; TEM
(defconstant float-exceptions-byte (byte 5 0))	  ; cexc

;;; According to the SPARC doc (as opposed to FPU doc), the fast mode
;;; bit (EFM) is "reserved", and should always be zero.  However, for
;;; sparc-V8 and sparc-V9, it appears to work, causing denormals to
;;; be truncated to 0 silently.
(defconstant float-fast-bit (ash 1 22))

); eval-when

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture for a place to spill register windows.
;;; 
(defconstant number-stack-displacement
  (* 16 vm:word-bytes))


;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
	  target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))

;;; Where to put the different spaces.  Must match the C code!
;;; 
(defparameter target-read-only-space-start #x10000000)
(defparameter target-static-space-start    #x28000000)
(defparameter target-dynamic-space-start   #x40000000)

;; This better match the value in sparc-validate.h!
(defparameter target-foreign-linkage-space-start #x0f800000)
;; This better agree with what sparc-arch.c thinks it is!  Right now,
;; it's 4 instructions, so 16 bytes.
(defconstant target-foreign-linkage-entry-size 16)


;;;; Other random constants.

(export '(halt-trap pending-interrupt-trap error-trap cerror-trap
	  breakpoint-trap function-end-breakpoint-trap
	  after-breakpoint-trap pseudo-atomic-trap
	  object-not-list-trap object-not-instance-trap
	  trace-table-normal trace-table-call-site
	  trace-table-function-prologue trace-table-function-epilogue))

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  after-breakpoint)

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

(export '(static-symbols static-functions))

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
    ))

(defparameter static-functions
  '(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor
    two-arg-gcd two-arg-lcm
    ))



;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)


;;;; Pseudo-atomic trap number.
;;;;
;;;; This should be any valid trap number. According to the Sparc
;;;; Compliance Definition 2.4.1, only traps 16-31 are allowed for
;;;; user applications.  All others are reserved.  It's ok if this
;;;; number matches any of the other trap enums above because those
;;;; are only used in an illtrap instruction, not the trap
;;;; instruction.  This needs to be coordinated with the C code.
(defconstant pseudo-atomic-trap 16)
