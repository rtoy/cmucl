;;; -*- Package: VM; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/parms.lisp,v 1.3 1990/02/14 21:10:37 wlott Exp $
;;;
;;;    This file contains some parameterizations of various VM attributes for
;;; the MIPS.  This file is separate from other stuff so that it can be compiled
;;; and loaded earlier.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted to MIPS by William Lott.
;;;

(in-package "VM")


(export '(sc-number-limit most-positive-cost word-bits byte-bits word-shift
	  word-bytes target-byte-order register-names lowtag-bits
	  lowtag-mask type-bits type-mask even-fixnum-type
	  function-pointer-type other-immediate-type list-pointer-type
	  odd-fixnum-type structure-pointer-type other-pointer-type
	  bignum-type ratio-type single-float-type double-float-type
	  complex-type simple-array-type simple-string-type
	  simple-bit-vector-type simple-vector-type
	  simple-array-unsigned-byte-2-type
	  simple-array-unsigned-byte-4-type
	  simple-array-unsigned-byte-8-type
	  simple-array-unsigned-byte-16-type
	  simple-array-unsigned-byte-32-type simple-array-single-float-type
	  simple-array-double-float-type complex-string-type
	  complex-bit-vector-type complex-vector-type complex-array-type
	  code-header-type function-header-type return-pc-header-type
	  closure-header-type symbol-header-type character-type SAP-type
	  unbound-marker-type fixnum *assembly-unit-length*
	  convert-byte-list target-fasl-code-format vm-version))
	  

(eval-when (compile load eval)


;;;; Compiler constants.

;;; Maximum number of SCs allowed.
;;;
(defconstant sc-number-limit 20)

;;; The inclusive upper bound on a cost.  We want to write cost frobbing
;;; code so that it is portable, but works on fixnums.  This constant should be
;;; defined so that adding two costs cannot result in fixnum overflow.
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
  "The byte order of the target machine.  :big-endian has the MSB first (RT)
  and :little-endian has the MSB last (VAX).")

(defconstant register-names
  '#(ZERO NL0 NL1 NL2 NL3 NL4 NL5 NL6/NARGS L0 L1 L2 L3 L4 L5 L6 L7
	  A0 A1 A2 A3 A4 A5 ENV LRA BSP CSP K0 K1 NULL NSP CODE CONT))


;;;; Type definitions:

(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  "Mask to extract the lowtag bits from a pointer.")

(defconstant type-bits 8
  "Number of bits used in the header word of a data block for typeing.")

(defconstant type-mask (1- (ash 1 type-bits))
  "Mask to extract the type from a header word.")


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

(defenum (:suffix -type)
  even-fixnum
  function-pointer
  other-immediate
  list-pointer
  odd-fixnum
  structure-pointer
  unused
  other-pointer)

;;; The number types.  Each of these types is in the header of non-fixnum
;;; numbers on the heap.

(defenum (:suffix -type
	  :start (+ (ash 1 lowtag-bits) other-immediate-type)
	  :step (ash 1 lowtag-bits))
  bignum
  ratio
  single-float
  double-float
  complex)

;;; The array types.

(defenum (:suffix -type
	  :start (+ (ash 8 lowtag-bits) other-immediate-type)
	  :step (ash 1 lowtag-bits))
  ;; The simple arrays:
    simple-array
    ;; The vectors:
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
  complex-array)

;;; The other random types:

(defenum (:suffix -type
	  :start (+ (ash 24 lowtag-bits) other-immediate-type)
	  :step (ash 1 lowtag-bits))
  code-header
  function-header
  return-pc-header
  closure-header
  symbol-header
  character
  SAP
  unbound-marker)

;;; Handy routine for making fixnums.

(defun fixnum (num)
  "Make a fixnum out of NUM.  (i.e. shift by two bits if it will fit.)"
  (if (<= #x-20000000 num #x1fffffff)
      (ash num 2)
      (error "~D is too big for a fixnum." num)))



;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

;;; Convert-Byte-List  --  Internal
;;; 
;;; Convert-Byte-List take a list of byte specifiers that define a field
;;; and returns a list of byte specifiers that can be used to take apart
;;; the value to be placed in that field.  This is somewhat architecture
;;; dependent because of differences in byte ordering conventions.

(defun convert-byte-list (byte-list)
  (let ((offset (byte-position (first byte-list))))
    (mapcar #'(lambda (byte)
		(byte (byte-size byte) (- (byte-position byte) offset)))
	    byte-list)))


;;;; Other parameters:

;;; The number representing the fasl-code format emit code in.
;;;
(defparameter target-fasl-code-format 7)

;;; The version string for the implementation dependent code.
;;;
(defparameter vm-version "DECstation 3100/Mach 0.0")




); Eval-When (Compile Load Eval)
