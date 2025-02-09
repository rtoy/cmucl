;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: src/compiler/ppc/parms.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the PPC.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted to MIPS by William Lott.
;;;

(in-package "PPC")


;;;; Compiler constants.

(eval-when (:compile-toplevel :execute :load-toplevel)

(setf (backend-name *target-backend*) "PPC")
(setf (backend-version *target-backend*) "PowerPC")
(setf (backend-fasl-file-type *target-backend*) "ppcf")
(setf (backend-fasl-file-implementation *target-backend*)
      ppc-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) byte-fasl-file-version)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) :big-endian)
(setf (backend-page-size *target-backend*) 4096)

(setf (c::backend-foreign-linkage-space-start *target-backend*)
      #x17000000
      (c::backend-foreign-linkage-entry-size *target-backend*)
      32)

); eval-when

(pushnew :new-assembler *features*)


;;;; Machine Architecture parameters:

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(word-bits byte-bits char-bits word-shift word-bytes char-bytes
	  float-sign-shift

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
	  float-invalid-op-1-byte
	  float-invalid-op-2-byte
	  float-exceptions-summary-byte))

#+double-double
(export '(double-double-float-digits))
) ; eval-when


	  

(eval-when (:compile-toplevel :execute :load-toplevel)

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

#+double-double
(defconstant double-double-float-digits
  (* 2 double-float-digits))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant float-rounding-mode (byte 2 0))	  ; RD
;; Accrued exceptions.
(defconstant float-sticky-bits (byte 5 25))
;; Exception enable
(defconstant float-traps-byte (byte 5 3))
;; Current exceptions.  It seems that PPC doesn't actually have a
;; separate field for current exceptions, so we make this the same as
;; the (sticky) accrued exceptions.
(defconstant float-exceptions-byte (byte 5 25))
;; Sticky invalid operation bits that indicates what kind of invalid
;; exception occurred, like SNaN, inf - inf, inf / inf, 0/0, or an
;; invalid compare.
(defconstant float-invalid-op-1-byte (byte 6 19))
;; Sticky invalid operation bits that indicates some kind of invalid
;; operation such as some software request, invalid square root, or
;; invalid integer convert.
(defconstant float-invalid-op-2-byte (byte 3 8))
;; FP exception summary bit. This is set if an FP instruction causes
;; any of the FP exception bits to transition from 0 to 1 (meaning
;; that exception occurred.)
(defconstant float-exceptions-summary-byte (byte 1 31))
(defconstant float-fast-bit (ash 1 2))	; Non-IEEE mode

); eval-when

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture, mostly (?) to make C backtrace
;;; work.
;;; 
(defconstant number-stack-displacement
  (* #-darwin 2
     #+darwin 8 vm:word-bytes))


;;;; Description of the target address space.
(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
          target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))
)


;;; Where to put the different spaces.
;;; 
(defparameter target-read-only-space-start #x04000000)
(defparameter target-static-space-start    #x10000000)
(defparameter target-dynamic-space-start   #x40000000)
;; We're sticking this at the end of the static space for now for the
;; first linkage-table build because it's mapped with the
;; static-space.  We can move it later once linkage-table is built.
(defparameter target-foreign-linkage-space-start #x17000000)
(defconstant target-foreign-linkage-entry-size 32) ;In bytes.  Duh.



;;;; Other random constants.

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(halt-trap pending-interrupt-trap error-trap cerror-trap
	  breakpoint-trap function-end-breakpoint-trap
	  after-breakpoint-trap
	  object-not-list-trap object-not-instance-trap
	  trace-table-normal trace-table-call-site
	  trace-table-function-prologue trace-table-function-epilogue))
)


(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  after-breakpoint)

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

    ;; Make the ..slot-unbound.. symbol static to optimise the
    ;; common slot unbound check.
    pcl::..slot-unbound..

    ;; These are filled in the C run-time.
    lisp::*cmucl-lib*
    lisp::*cmucl-core-path*
      
    ;; Foreign linkage stuff
    #+linkage-table
    lisp::*linkage-table-data*

    #+gencgc
    *current-region-end-addr*
    #+gencgc
    *scavenge-read-only-space*

    ;; Weak hash table support
    :key
    :value
    :key-and-value
    :key-or-value
    
    lisp::*lisp-implementation-version*
    ;; Spare symbols.  Rename these when you need to add some static
    ;; symbols and don't want to do a cross-compile.
    spare-8
    spare-7
    spare-6
    spare-5
    spare-4
    spare-3
    spare-2
    spare-1
    spare-0

    #|kernel::*current-thread*|#
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
