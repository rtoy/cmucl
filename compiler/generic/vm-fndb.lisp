;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-fndb.lisp,v 1.9 1990/05/27 16:51:48 ch Exp $
;;;
;;; This file defines the machine specific function signatures.
;;;
;;; Written by William Lott.
;;;
(in-package "C")

(import '(lisp::%raw-bits lisp::simple-array-p))



;;;; Internal type predicates:
;;;
;;;    Simple typep uses that don't have any standard predicate are translated
;;; into non-standard unary predicates.

(defknown (fixnump bignump ratiop short-float-p single-float-p double-float-p
	   long-float-p base-char-p %string-char-p %standard-char-p structurep
	   array-header-p simple-array-p simple-array-unsigned-byte-2-p
	   simple-array-unsigned-byte-4-p simple-array-unsigned-byte-8-p
	   simple-array-unsigned-byte-16-p simple-array-unsigned-byte-32-p
	   simple-array-single-float-p simple-array-double-float-p
	   system-area-pointer-p realp unsigned-byte-32-p signed-byte-32-p)
  (t) boolean (movable foldable flushable))

;;; Introduce these predicates into the old compiler.  This is necessary
;;; 'cause they are marked as foldable.
;;; 
#-new-compiler
(macrolet ((frob (name type)
	     `(defun ,name (thing)
		(typep thing ',type))))
  (frob simple-array-unsigned-byte-2-p (simple-array (unsigned-byte 2) (*)))
  (frob simple-array-unsigned-byte-4-p (simple-array (unsigned-byte 4) (*)))
  (frob simple-array-unsigned-byte-8-p (simple-array (unsigned-byte 8) (*)))
  (frob simple-array-unsigned-byte-16-p (simple-array (unsigned-byte 16) (*)))
  (frob simple-array-unsigned-byte-32-p (simple-array (unsigned-byte 32) (*)))
  (frob simple-array-single-float-p (simple-array single-float (*)))
  (frob simple-array-double-float-p (simple-array double-float (*)))
  (frob system-area-pointer-p system-area-pointer)
  (frob realp real)
  (frob unsigned-byte-32-p (unsigned-byte 32))
  (frob signed-byte-32-p (signed-byte 32))) 


;;;; Miscellaneous "sub-primitives":

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))


(defknown %raw-bits (t fixnum) (unsigned-byte 32)
  (foldable flushable))
(defknown ((setf %raw-bits)) (t fixnum (unsigned-byte 32)) (unsigned-byte 32)
  (unsafe))


(defknown dynamic-space-free-pointer ()
  (system-area-pointer)
  (foldable flushable movable))


;;;; 32bit logical operations

(defknown merge-bits ((unsigned-byte 5) (unsigned-byte 32) (unsigned-byte 32))
  (unsigned-byte 32)
  (foldable flushable movable))

(defknown 32bit-logical-not ((unsigned-byte 32)) (unsigned-byte 32)
  (foldable flushable movable))

(defknown (32bit-logical-and 32bit-logical-or 32bit-logical-xor
			     32bit-logical-nor)
	  ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)
  (foldable flushable movable))



;;;; Bignum operations.

(defknown bignum::%allocate-bignum (bignum-index) bignum-type
  (flushable))

(defknown bignum::%bignum-length (bignum-type) bignum-index
  (foldable flushable movable))

(defknown bignum::%bignum-set-length (bignum-type bignum-index) bignum-index
  (unsafe))

(defknown bignum::%bignum-ref (bignum-type bignum-index) bignum-element-type
  (flushable))

(defknown bignum::%bignum-set (bignum-type bignum-index bignum-element-type)
  bignum-element-type
  (unsafe))

(defknown bignum::%digit-0-or-plusp (bignum-element-type) boolean
  (foldable flushable movable))

(defknown (bignum::%add-with-carry bignum::%subtract-with-borrow)
	  (bignum-element-type bignum-element-type (mod 2))
  (values bignum-element-type (mod 2))
  (foldable flushable movable))

(defknown bignum::%multiply (bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown bignum::%lognot (bignum-element-type) bignum-element-type
  (foldable flushable movable))

(defknown (bignum::%logand bignum::%logior bignum::%logxor)
	  (bignum-element-type bignum-element-type)
  bignum-element-type
  (foldable flushable movable))

(defknown bignum::%fixnum-to-digit (fixnum) bignum-element-type
  (foldable flushable movable))

(defknown bignum::%floor
	  (bignum-element-type bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown bignum::%fixnum-digit-with-correct-sign
	  (bignum-element-type)
  fixnum
  (foldable flushable movable))

(defknown (bignum::%signed-digit-to-single-float bignum::%digit-to-single-float)
	  (bignum-element-type)
  (single-float)
  (foldable flushable movable))

(defknown (bignum::%signed-digit-to-double-float bignum::%digit-to-double-float)
	  (bignum-element-type)
  (double-float)
  (foldable flushable movable))

(defknown (bignum::%ashl bignum::%ashr)
	  (bignum-element-type (mod 32)) bignum-element-type
  (foldable flushable movable))



;;;; Bit-bashing routines.

(defknown copy-to-system-area
	  ((simple-unboxed-array (*)) index system-area-pointer index index)
  null
  ())

(defknown copy-from-system-area
	  (system-area-pointer index (simple-unboxed-array (*)) index index)
  null
  ())

(defknown system-area-copy
	  (system-area-pointer index system-area-pointer index index)
  null
  ())

(defknown bit-bash-copy
	  ((simple-unboxed-array (*)) index
	   (simple-unboxed-array (*)) index index)
  null
  ())
