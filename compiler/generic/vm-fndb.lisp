;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-fndb.lisp,v 1.33 1991/01/08 18:06:44 wlott Exp $
;;;
;;; This file defines the machine specific function signatures.
;;;
;;; Written by William Lott.
;;;
(in-package "C")

(import '(lisp::%raw-bits lisp::simple-array-p))

(in-package "KERNEL")
(export '(funcallable-instance-p %set-funcallable-instance-info))

(in-package "C")


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
	   system-area-pointer-p realp unsigned-byte-32-p signed-byte-32-p
	   weak-pointer-p funcallable-instance-p)
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
  (frob signed-byte-32-p (signed-byte 32))
  (frob weak-pointer-p weak-pointer))


;;;; Miscellaneous "sub-primitives":

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))

(defknown %sxhash-simple-string (simple-string) index
  (foldable flushable))

(defknown %sxhash-simple-substring (simple-string index) index
  (foldable flushable))


(defknown %closure-index-ref (function index) t
  (flushable))


(defknown %make-funcallable-instance (index function) function (unsafe))

(defknown %set-funcallable-instance-info (function index t) t (unsafe))


(defknown vector-sap ((simple-unboxed-array (*))) system-area-pointer
  (flushable))


(defknown get-lowtag (t) (unsigned-byte #.vm:lowtag-bits)
  (flushable movable))
(defknown get-type (t) (unsigned-byte #.vm:type-bits)
  (flushable movable))

(defknown (get-header-data get-closure-length) (t) (unsigned-byte 24)
  (flushable))
(defknown set-header-data (t (unsigned-byte 24)) t
  (unsafe))


(defknown make-structure (structure-index) structure
  (unsafe))
(defknown structure-type (structure) t
  (foldable flushable))
(defknown structure-length (structure) structure-index
  (foldable flushable))
(defknown structure-ref (structure structure-index) t
  (flushable))
(defknown structure-set (structure structure-index t) t
  (unsafe))



(defknown %raw-bits (t fixnum) (unsigned-byte 32)
  (foldable flushable))
(defknown (%set-raw-bits) (t fixnum (unsigned-byte 32)) (unsigned-byte 32)
  (unsafe))


(defknown allocate-vector ((unsigned-byte 8) index index) (simple-array * (*))
  (flushable movable))

(defknown make-array-header ((unsigned-byte 8) (unsigned-byte 24)) array
  (flushable movable))


(defknown %make-weak-pointer (t boolean) weak-pointer
  (flushable))
(defknown %make-complex (real real) complex
  (flushable movable))
(defknown %make-ratio (rational rational) ratio
  (flushable movable))


(defknown (dynamic-space-free-pointer binding-stack-pointer-sap
				      control-stack-pointer-sap)  ()
  system-area-pointer
  (flushable))


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

(defknown %allocate-bignum (bignum-index) bignum-type
  (flushable))

(defknown %bignum-length (bignum-type) bignum-index
  (foldable flushable movable))

(defknown %bignum-set-length (bignum-type bignum-index) bignum-type
  (unsafe))

(defknown %bignum-ref (bignum-type bignum-index) bignum-element-type
  (flushable))

(defknown %bignum-set (bignum-type bignum-index bignum-element-type)
  bignum-element-type
  (unsafe))

(defknown %digit-0-or-plusp (bignum-element-type) boolean
  (foldable flushable movable))

(defknown (%add-with-carry %subtract-with-borrow)
	  (bignum-element-type bignum-element-type (mod 2))
  (values bignum-element-type (mod 2))
  (foldable flushable movable))

(defknown %multiply-and-add
	  (bignum-element-type bignum-element-type bignum-element-type
			       &optional bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %multiply (bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %lognot (bignum-element-type) bignum-element-type
  (foldable flushable movable))

(defknown (%logand %logior %logxor) (bignum-element-type bignum-element-type)
  bignum-element-type
  (foldable flushable movable))

(defknown %fixnum-to-digit (fixnum) bignum-element-type
  (foldable flushable movable))

(defknown %floor (bignum-element-type bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %fixnum-digit-with-correct-sign (bignum-element-type)
  (signed-byte #.vm:word-bits)
  (foldable flushable movable))

(defknown (%ashl %ashr %digit-logical-shift-right)
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



;;;; Automatic defknowns for primitive objects.

(vm:define-for-each-primitive-object (obj)
  (collect ((forms))
    (let* ((options (vm:primitive-object-options obj))
	   (obj-type (getf options :type t)))
      (dolist (slot (vm:primitive-object-slots obj))
	(let* ((name (vm:slot-name slot))
	       (slot-opts (vm:slot-options slot))
	       (slot-type (getf slot-opts :type t))
	       (ref-trans (getf slot-opts :ref-trans))
	       (ref-known (getf slot-opts :ref-known))
	       (set-trans (getf slot-opts :set-trans))
	       (set-known (getf slot-opts :set-known)))
	  (when ref-known
	    (if ref-trans
		(forms `(defknown (,ref-trans) (,obj-type) ,slot-type
			  ,ref-known))
		(error "Can't spec a :ref-known with no :ref-trans. ~S in ~S"
		       name (vm:primitive-object-name obj))))
	  (when set-known
	    (if set-trans
		(forms `(defknown (,set-trans)
				  ,(if (and (listp set-trans)
					    (= (length set-trans) 2)
					    (eq (car set-trans) 'setf))
				       (list slot-type obj-type)
				       (list obj-type slot-type))
			  ,slot-type ,set-known))
		(error "Can't spec a :set-known with no :set-trans. ~S in ~S"
		       name (vm:primitive-object-name obj)))))))
    (when (forms)
      `(progn
	 ,@(forms)))))
