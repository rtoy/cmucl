;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/parms.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for ARM.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Converted from SPARC to ARM.
;;;

(in-package "ARM")
;;(intl:textdomain "cmucl-sparc-vm")
(use-package "C")


;;;; Compiler constants.

(eval-when (compile eval load)

(adjoin :linux (backend-features *target-backend*))
(setf (backend-name *target-backend*) "ARM")
;; FIXME:  Choose a better version name?
(setf (backend-version *target-backend*)
      "ARMv7/Linux")
;; FIXME:  Is armf ok?
(setf (backend-fasl-file-type *target-backend*) "armf")
(setf (backend-fasl-file-implementation *target-backend*)
      arm-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) byte-fasl-file-version)
(setf (backend-register-save-penalty *target-backend*) 3)

;; Only supporting little-endian ARM for now.
(setf (backend-byte-order *target-backend*) :little-endian)
;; FIXME:  What is the page size on ARM?
(setf (backend-page-size *target-backend*) 4096)

(setf (c::backend-foreign-linkage-space-start *target-backend*)
      #x0f000000
      (c::backend-foreign-linkage-entry-size *target-backend*)
      ;; FIXME: Update this when we figure out how to do
      ;; linkage-tables on arm.
      16)
); eval-when

(pushnew :new-assembler *features*)


;;;; Machine Architecture parameters:

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

	  long-float-bias long-float-exponent-byte
	  long-float-significand-byte long-float-normal-exponent-min
	  long-float-normal-exponent-max long-float-hidden-bit
	  long-float-trapping-nan-bit long-float-digits

	  float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit))

#+double-double
(export '(double-double-float-digits))
	  

(eval-when (compile load eval)

(defconstant word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant char-bits #-unicode 8 #+unicode 16
  "Number of bits needed to represent a character")

(defconstant char-bytes (truncate char-bits byte-bits)
  "Number of bytes needed to represent a character")

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

(defconstant fixnum-tag-bits (1- lowtag-bits)
  "Number of tag bits used for a fixnum")

(defconstant fixnum-tag-mask (1- (ash 1 fixnum-tag-bits))
  "Mask to get the fixnum tag")

(defconstant positive-fixnum-bits (- word-bits fixnum-tag-bits 1)
  "Maximum number of bits in a positive fixnum")

;; FIXME: All of the IEEE-754 float parms should probably be split
;; into a separate file to be shared by all platforms using standard
;; IEEE-754 single and double precision floats.
(defconstant float-sign-shift 31)

(defconstant single-float-bytes 4)	; Bytes to hold a single-float
(defconstant single-float-bias 126)
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant double-float-bytes 8)	; Bytes to hold a double-float
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

(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) word-bits 1))

#+double-double
(defconstant double-double-float-digits
  (* 2 double-float-digits))

;; ARM specific information
;; See B6.1.39: FPSCR, Floating-point Status and Control Regiser, PMSA
(defconstant float-inexact-trap-bit (ash 1 12))
(defconstant float-underflow-trap-bit (ash 1 11))
(defconstant float-overflow-trap-bit (ash 1 10))
(defconstant float-divide-by-zero-trap-bit (ash 1 9))
(defconstant float-invalid-trap-bit (ash 1 8))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-positive 1)
(defconstant float-round-to-negative 2)
(defconstant float-round-to-zero 3)

(defconstant float-rounding-mode (byte 2 22))	  ; RMode
(defconstant float-traps-byte (byte 5 8))	  ; Trap enable bits
;; There doesn't appear to be separate accrued (sticky) and current
;; exceptions.
(defconstant float-sticky-bits (byte 5 0))	  ; 
(defconstant float-exceptions-byte (byte 5 0))	  ; 

;; Flush-to-zero bit
(defconstant float-fast-bit (ash 1 24))

); eval-when

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  I
;;; (rtoy) think the architecture doesn't require any, but we need
;;; space for arguments to C routines.  See c-call.lisp
;;; 
(defconstant number-stack-displacement
  (* 4 vm:word-bytes))


;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
	  target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))

;;; Where to put the different spaces.  Must match the C code!
;;; 
(defconstant target-read-only-space-start #x10000000)
(defconstant target-static-space-start    #x30000000)
(defconstant target-dynamic-space-start   #x40000000)

(defconstant target-foreign-linkage-space-start
  (c:backend-foreign-linkage-space-start *target-backend*))
(defconstant target-foreign-linkage-entry-size
  (c:backend-foreign-linkage-entry-size *target-backend*))


;;;; Other random constants.

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
    lisp::*pseudo-atomic-atomic*
    lisp::*pseudo-atomic-interrupted*

    ;; Foreign linkage stuff
    #+linkage-table
    lisp::*linkage-table-data*

    ;;
    lisp::*cmucl-lib*
    lisp::*cmucl-core-path*

    *binding-stack-pointer*

    *allocation-pointer*
    *current-region-free-pointer*
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
    
    ;; Some spare static symbols.  Useful for adding another static
    ;; symbol without having to do a cross-compile.  Just rename one
    ;; of these to the desired name.
    spare-9
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
  ;; FIXME: Correct this list.  This is based on the sparc port which
  ;; has a different list from x86
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
