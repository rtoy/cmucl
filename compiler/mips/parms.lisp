;;; -*- Package: VM; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/parms.lisp,v 1.19 1990/02/26 18:54:05 wlott Exp $
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the MIPS.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted to MIPS by William Lott.
;;;

(in-package "VM")

(export '(sc-number-limit most-positive-cost word-bits byte-bits word-shift
	  word-bytes target-byte-order target-read-only-space-start
	  target-static-space-start target-dynamic-space-start
	  target-control-stack-start target-binding-stack-start
	  target-heap-address-space lowtag-bits lowtag-mask
	  lowtag-limit type-bits type-mask pad-data-block even-fixnum-type
	  function-pointer-type other-immediate-0-type other-immediate-1-type
	  list-pointer-type odd-fixnum-type structure-pointer-type
	  other-pointer-type bignum-type ratio-type single-float-type
	  double-float-type complex-type simple-array-type simple-string-type
	  simple-bit-vector-type simple-vector-type
	  simple-array-unsigned-byte-2-type
	  simple-array-unsigned-byte-4-type
	  simple-array-unsigned-byte-8-type
	  simple-array-unsigned-byte-16-type
	  simple-array-unsigned-byte-32-type simple-array-single-float-type
	  simple-array-double-float-type complex-string-type
	  complex-bit-vector-type complex-vector-type complex-array-type
	  code-header-type function-header-type
	  closure-function-header-type return-pc-header-type
	  closure-header-type value-cell-header-type symbol-header-type
	  character-type SAP-type unbound-marker-type atomic-flag
	  interrupted-flag pending-interrupt-trap error-trap cerror-trap
	  fixnum static-symbols static-symbol-offset offset-static-symbol
	  static-symbol-p
	  *assembly-unit-length* target-fasl-code-format vm-version))
	  

(eval-when (compile load eval)


;;;; Compiler constants.

;;; Maximum number of SCs allowed.
;;;
(defconstant sc-number-limit 20)

;;; The inclusive upper bound on a cost.  We want to write cost frobbing
;;; code so that it is portable, but works on fixnums.  This constant
;;; should be defined so that adding two costs cannot result in fixnum
;;; overflow.
;;;
(defconstant most-positive-cost (1- (expt 2 20)))



;;;; Machine Architecture parameters:

(defconstant word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  "Number of bytes in a word.")

(defconstant target-byte-order :little-endian
  "The byte order of the target machine.  Should either be :big-endian
  which has the MSB first (RT) or :little-endian which has the MSB last
  (VAX).")



;;;; Description of the target address space.

;;; Where to put the different spaces and stacks.
;;; 
(defconstant target-read-only-space-start #x20000000)
(defconstant target-static-space-start #x30000000)
(defconstant target-dynamic-space-start #x40000000)
(defconstant target-control-stack-start #x50000000)
(defconstant target-binding-stack-start #x60000000)

;;; How much memory to validate for lisp.
;;; 
(defconstant target-heap-address-space
  '((#x40000000 . #x4000) ; Dynamic space
    (#x50000000 . #x4000) ; Control stack
    (#x60000000 . #x4000))) ; Binding stack



;;;; Type definitions:

(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  "Mask to extract the low tag bits from a pointer.")
  
(defconstant lowtag-limit (ash 1 lowtag-bits)
  "Exclusive upper bound on the value of the low tag bits from a
  pointer.")

(defconstant type-bits 8
  "Number of bits used in the header word of a data block for typeing.")

(defconstant type-mask (1- (ash 1 type-bits))
  "Mask to extract the type from a header word.")

(defmacro pad-data-block (words)
  `(logandc2 (+ (ash ,words vm:word-shift) lowtag-mask) lowtag-mask))


(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
		   &rest identifiers)
  (let ((results nil)
	(index 0)
	(start (eval start))
	(step (eval step)))
    (dolist (id identifiers)
      (when id
	(multiple-value-bind
	    (root docs)
	    (if (consp id)
		(values (car id) (cdr id))
		(values id nil))
	  (push `(defconstant ,(intern (concatenate 'simple-string
						    (string prefix)
						    (string root)
						    (string suffix)))
		   ,(+ start (* step index))
		   ,@docs)
		results)))
      (incf index))
    `(eval-when (compile load eval)
       ,@(nreverse results))))

;;; The main types.  These types are represented by the low three bits of the
;;; pointer or immeditate object.
;;; 
(defenum (:suffix -type)
  even-fixnum
  function-pointer
  other-immediate-0
  list-pointer
  odd-fixnum
  structure-pointer
  other-immediate-1
  other-pointer)

;;; The heap types.  Each of these types is in the header of objects in
;;; the heap.
;;; 
(defenum (:suffix -type
	  :start (+ (ash 1 lowtag-bits) other-immediate-0-type)
	  :step (ash 1 (1- lowtag-bits)))
  bignum
  ratio
  single-float
  double-float
  complex
  
  simple-array
  simple-string
  simple-bit-vector
  simple-vector
  simple-array-unsigned-byte-2
  simple-array-unsigned-byte-4
  simple-array-unsigned-byte-8
  simple-array-unsigned-byte-16
  simple-array-unsigned-byte-32
  simple-array-single-float
  simple-array-double-float
  complex-string
  complex-bit-vector
  complex-vector
  complex-array
  
  code-header
  function-header
  closure-function-header
  return-pc-header
  closure-header
  value-cell-header
  symbol-header
  base-character
  sap
  unbound-marker)


;;;; Other non-type constants.

(defenum (:suffix -flag)
  atomic
  interrupted)

(defenum (:suffix -trap :start 8)
  pending-interrupt
  error
  cerror)


;;;; Static symbols.

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;

(defparameter static-symbols
  '(t

    ;; Random stuff needed for initialization.
    lisp::lisp-environment-list
    lisp::lisp-command-line-list
    lisp::*static-symbols*
    lisp::*lisp-initialization-functions*
    lisp::%initial-function

    ;; Values needed for interfacing C and LISP.
    lisp::*foreign-function-call-active*
    lisp::*saved-global-pointer*
    lisp::*saved-control-stack-pointer*
    lisp::*saved-binding-stack-pointer*
    lisp::*saved-allocation-pointer*
    lisp::*saved-flags-register*))

(defun static-symbol-p (symbol)
  (member symbol static-symbols))

(defun static-symbol-offset (symbol)
  "Returns the byte offset of the static symbol Symbol."
  (let ((posn (position symbol static-symbols)))
    (unless posn (error "~S is not a static symbol." symbol))
    (+ (* posn (pad-data-block symbol-size))
       (pad-data-block (1- symbol-size))
       other-pointer-type
       (- list-pointer-type))))

(defun offset-static-symbol (offset)
  "Given a byte offset, Offset, returns the appropriate static symbol."
  (multiple-value-bind
      (n rem)
      (truncate (+ offset list-pointer-type (- other-pointer-type)
		   (- (pad-data-block (1- symbol-size))))
		(pad-data-block symbol-size))
    (unless (and (zerop rem) (<= 0 n (1- (length static-symbols))))
      (error "Byte offset, ~D, is not correct." offset))
    (elt static-symbols n)))


;;;; Handy routine for making fixnums:

(defun fixnum (num)
  "Make a fixnum out of NUM.  (i.e. shift by two bits if it will fit.)"
  (if (<= #x-20000000 num #x1fffffff)
      (ash num 2)
      (error "~D is too big for a fixnum." num)))



;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)


;;;; Other parameters:

;;; The number representing the fasl-code format emit code in.
;;;
(defparameter target-fasl-code-format 7)

;;; The version string for the implementation dependent code.
;;;
(defparameter vm-version "DECstation 3100/Mach 0.0")




); Eval-When (Compile Load Eval)
