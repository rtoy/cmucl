;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/rt/params.lisp,v 1.3 1991/04/12 22:48:32 wlott Exp $
;;;
;;; This file contains some parameterizations of various VM attributes for the
;;; IBM RT.  This file is separate from other stuff, so we can compile and
;;; load it earlier.
;;;
;;; Written by Rob MacLachlan
;;; Converted to MIPS by William Lott.
;;; Converted to IBM RT by William Lott and Bill Chiles.
;;;

(in-package "RT")
(use-package "C")

(export '(word-bits byte-bits word-shift word-bytes float-sign-shift

	  single-float-bias single-float-exponent-byte
	  single-float-significand-byte single-float-normal-exponent-min
	  single-float-normal-exponent-max single-float-hidden-bit
	  single-float-trapping-nan-bit single-float-digits

	  double-float-bias double-float-exponent-byte
	  double-float-significand-byte double-float-normal-exponent-min
	  double-float-normal-exponent-max double-float-hidden-bit
	  double-float-trapping-nan-bit double-float-digits

	  float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit

))



;;;; Compiler constants.

(eval-when (compile eval load)

(setf (backend-name *target-backend*) "RT")
(setf (backend-version *target-backend*) "IBM RT/Mach 0.0")
(setf (backend-fasl-file-type *target-backend*) "rtf")
(setf (backend-fasl-file-implementation *target-backend*)
      rt-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) 1)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) :big-endian)

) ;eval-when



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

(defparameter target-most-positive-fixnum (1- (ash 1 29))
  "most-positive-fixnum in the target architecture.")

(defparameter target-most-negative-fixnum (ash -1 29)
  "most-negative-fixnum in the target architecture.")

(defconstant float-sign-shift 31)

;;; The exponent min/max values are wrong, I think.  The denorm, infinity, etc.
;;; info must go in there somewhere.

(defconstant single-float-bias 126)
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 0)
(defconstant single-float-normal-exponent-max 255)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant double-float-exponent-byte (byte 11 20))
(defconstant double-float-significand-byte (byte 20 0))
(defconstant double-float-normal-exponent-min 0)
(defconstant double-float-normal-exponent-max #x7FF)
(defconstant double-float-hidden-bit (ash 1 20))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

); eval-when




;;;; Status register formats, goes in parms.

;;; We squeeze the two 68881 float status registers into a one-word result for
;;; FLOATING-POINT-MODES.  We also canonicalize the exceptions so that the
;;; "portable" float trap code will work.
;;;

(eval-when (compile eval load)

;;; The actual positions of the info in the mc68881 FPCR and FPSR.
;;;
(defconstant mc68881-fpcr-rounding-mode-byte (byte 2 4))
(defconstant mc68881-fpcr-rounding-precision-byte (byte 2 6))
(defconstant mc68881-fpcr-traps-byte (byte 8 8))
(defconstant mc68881-fpsr-accrued-exceptions-byte (byte 5 3))
(defconstant mc68881-fpsr-current-exceptions-byte (byte 8 8))
(defconstant mc68881-fpsr-condition-code-byte (byte 4 24))

;;; Amount to shift by the get the condition code, - 16.
;;;
(defconstant mc68881-fpsr-condition-code-shift-16 8)

;;; The condition code bits.
;;;
(defconstant mc68881-nan-condition (ash 1 0))
(defconstant mc68881-infinity-condition (ash 1 1))
(defconstant mc68881-zero-condition (ash 1 2))
(defconstant mc68881-negative-condition (ash 1 3))

;;; Masks that map the extended set of exceptions implemented by the 68881 to
;;; the IEEE exceptions.  This extended format is used for the enabled traps
;;; and the current exceptions.
;;;
(defconstant mc68881-invalid-exception (ash #b111 5))
(defconstant mc68881-overflow-exception (ash 1 4))
(defconstant mc68881-underflow-exception (ash 1 3))
(defconstant mc68881-divide-zero-exception (ash 1 2))
(defconstant mc68881-inexact-exception (ash #b11 0))

;;; Encoding of float exceptions in the FLOATING-POINT-MODES result.  This is
;;; also the encoding used in the mc68881 accrued exceptions.
;;;
(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-negative 2)
(defconstant float-round-to-positive 3)

;;; Positions of bits in the FLOATING-POINT-MODES result.
;;;
(defconstant float-rounding-mode (byte 2 0))
(defconstant float-sticky-bits (byte 5 2))
(defconstant float-traps-byte (byte 5 7))
(defconstant float-exceptions-byte (byte 5 12))
(defconstant float-fast-bit 0)

); eval-when


;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start))

;;; Where to put the different spaces.
;;; 
(defparameter target-read-only-space-start #x00100000)
(defparameter target-static-space-start    #x04000000)
(defparameter target-dynamic-space-start   #x06000000)



;;;; Other non-type constants.

(export '(halt-trap pending-interrupt-trap error-trap cerror-trap
	  breakpoint-trap))

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint)



;;;; Static symbols.

(export '(static-symbols exported-static-symbols))


;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The exported static symbols are a subset of the static symbols that get
;;; exported to the C header file.  NOTE: EXPORTED-STATIC-SYMBOLS IS DEFINED
;;; AS A FUNCTION OF THE ORDERING OF THIS LIST.
;;;
(defparameter static-symbols
  '(t

    ;; Random stuff needed for initialization.
    lisp::lisp-environment-list
    lisp::lisp-command-line-list

    ;; Functions that C needs to call.
    lisp::%initial-function
    lisp::maybe-gc
    kernel::internal-error
    di::handle-breakpoint

    ;; Free Pointers
    lisp::*read-only-space-free-pointer*
    lisp::*static-space-free-pointer*
    lisp::*initial-dynamic-space-free-pointer*
    rt::*allocation-pointer*
    rt::*binding-stack-pointer*

    ;; Things needed for non-local-exit.
    lisp::*current-catch-block*
    lisp::*current-unwind-protect-block*
    *eval-stack-top*

    ;; Interrupt Handling
    lisp::*pseudo-atomic-atomic*
    lisp::*pseudo-atomic-interrupted*
    mach::*interrupts-enabled*
    mach::*interrupt-pending*
    lisp::*free-interrupt-context-index*

    ;; Static functions.
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    %negate two-arg-and two-arg-ior two-arg-xor
    length two-arg-gcd two-arg-lcm truncate
    ))

(defparameter exported-static-symbols
  (subseq static-symbols 0 (1+ (position 'lisp::*free-interrupt-context-index*
					 static-symbols))))



;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)
