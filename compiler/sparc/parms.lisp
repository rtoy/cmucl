;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/parms.lisp,v 1.1 1990/11/30 17:04:57 wlott Exp $
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

(export '(word-bits byte-bits word-shift word-bytes))

(export '(float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit single-float-trapping-nan-bit
	  double-float-trapping-nan-bit *fixup-values*))

(export '(check-cons check-symbol check-fixnum check-unsigned-byte-32
	  check-signed-byte-32 check-function check-function-or-symbol))


;;;; Compiler constants.

(eval-when (compile eval load)

(setf (backend-name *target-backend*) "SPARC")
(setf (backend-version *target-backend*) "SPARCstation/Mach 0.0")
(setf (backend-fasl-file-type *target-backend*) "sparcf")
(setf (backend-fasl-file-implementation *target-backend*)
      sparc-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) 1)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) :big-endian)

); eval-when


;;;; Machine Architecture parameters:

(eval-when (compile load eval)

(defconstant word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  "Number of bytes in a word.")


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

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

); eval-when


;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start))

;;; Where to put the different spaces.
;;; 
(defparameter target-read-only-space-start #x01000000)
(defparameter target-static-space-start    #x04000000)
(defparameter target-dynamic-space-start   #x06000000)



;;;; Other random constants.

(export '(halt-trap pending-interrupt-trap error-trap cerror-trap))

(defconstant halt-trap 8)
(defconstant pending-interrupt-trap 9)
(defconstant error-trap 10)
(defconstant cerror-trap 11)


;;;; Static symbols.

(export '(static-symbols exported-static-symbols))

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The exported static symbols are a subset of the static symbols that get
;;; exported to the C header file.
;;;
(defparameter static-symbols
  '(t

    ;; The C startup code must fill these in.
    lisp::lisp-environment-list
    lisp::lisp-command-line-list

    ;; Functions that the C code needs to call
    lisp::%initial-function
    lisp::maybe-gc
    kernel::internal-error

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
    lisp::*pseudo-atomic-atomic*
    lisp::*pseudo-atomic-interrupted*
    mach::*interrupts-enabled*
    mach::*interrupt-pending*

    ;; Static functions.
    length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor
    two-arg-gcd two-arg-lcm
    ))

(defparameter exported-static-symbols
  (subseq static-symbols 0 (position 'length static-symbols)))



;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)
