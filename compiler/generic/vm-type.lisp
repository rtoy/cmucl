;;; -*- Package: KERNEL; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-type.lisp,v 1.16 1990/08/25 18:46:40 wlott Exp $
;;;
;;;    This file contains implementation-dependent parts of the type support
;;; code.  This is stuff which deals with the mapping from types defined in
;;; Common Lisp to types actually supported by an implementation.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "KERNEL")


;;;; Implementation dependent deftypes:

;;; Make double-float a synonym for long-float, single-float for Short-Float.
;;; This is be expanded before the translator gets a chance, so we will get
;;; precedence.
;;;
;;; ### Bootstrap hack: if we frob these types in the old compiler environment,
;;; subtypep will break.
;;; 
(compiler-let ((lisp::*bootstrap-deftype* t))

(deftype long-float (&optional low high)
  `(double-float ,low ,high))
;;;
(deftype short-float (&optional low high)
  `(single-float ,low ,high))

); compiler-let
;;; Compiled-function is the same as function in this implementation.
;;;
(deftype compiled-function () 'function)

;;; Character is the same as base-character.
;;; ### Bootstrap hack: base characters don't exist in the old compiler,
;;; so leave characters alone.  Also, make string-char look like base-char.
(compiler-let ((lisp::*bootstrap-deftype* t))
  (setf (info type builtin 'character) nil)
  (deftype character () 'base-character)
  (deftype string-char () 'base-character))

;;;
;;; An index into an integer.
(deftype bit-index () `(integer 0 ,most-positive-fixnum))
;;;
;;; Offset argument to Ash (a signed bit index).
(deftype ash-index () 'fixnum)
;;;
;;; A lexical environment for macroexpansion.
(deftype lexical-environment () 'list)
;;;
;;; Worst case values for float attributes.
;;; ### long-float exponent range seems to be this, but I don't know why.
;;; Perhaps IEEE double uses some of the negative exponents for NAN, etc?
;;;
(deftype float-exponent () '(integer -1021 1024))
(deftype float-digits () '(unsigned-byte 6))
(deftype float-radix () '(integer 2 2))
;;;
;;; A code for Boole.
(deftype boole-code () '(unsigned-byte 4))
;;;
;;; A byte-specifier.
(deftype byte-specifier () 'cons)
;;;
;;; Result of Char-Int...
(deftype char-int () 'char-code)
;;;
;;; Pathname pieces, as returned by the PATHNAME-xxx functions.
(deftype pathname-host () '(or simple-string null)); Host not really supported...
(deftype pathname-device () '(or simple-string (member :absolute nil)))
(deftype pathname-directory () '(or simple-vector null))
(deftype pathname-name () '(or simple-string null))
(deftype pathname-type () '(or simple-string null))
(deftype pathname-version () '(or simple-string (member nil :newest)))
;;;
;;; Internal time format.  Not a fixnum (blag...)
(deftype internal-time () 'unsigned-byte)

(deftype bignum-element-type () `(unsigned-byte ,vm:word-bits))
(deftype bignum-type () 'bignum)
(deftype bignum-index () 'index)


;;;; Hooks into type system:

;;; The kinds of specialised array that actually exist in this implementation.
;;;
(defparameter specialized-array-element-types
  '(bit (unsigned-byte 2) (unsigned-byte 4) (unsigned-byte 8) (unsigned-byte 16)
	(unsigned-byte 32) base-character single-float double-float))

(deftype unboxed-array (&optional dims)
  (collect ((types (list 'or)))
    (dolist (type specialized-array-element-types)
      (when (subtypep type '(or integer character))
	(types `(array ,type ,dims))))
    (types)))

(deftype simple-unboxed-array (&optional dims)
  (collect ((types (list 'or)))
    (dolist (type specialized-array-element-types)
      (when (subtypep type '(or integer character))
	(types `(simple-array ,type ,dims))))
    (types)))


;;; Float-Format-Name  --  Internal
;;;
;;;    Return the symbol that describes the format of Float.
;;;
(proclaim '(function float-format-name (float) symbol))
(defun float-format-name (x)
  (etypecase x
    (single-float 'single-float)
    (double-float 'double-float)))

;;; Specialize-Array-Type  --  Internal
;;;
;;;    This function is called when the type code wants to find out how an
;;; array will actually be implemented.  We set the Specialized-Element-Type to
;;; correspond to the actual specialization used in this implementation.
;;;
(proclaim '(function specialize-array-type (array-type) array-type))
(defun specialize-array-type (type)
  (let ((eltype (array-type-element-type type)))

    (setf (array-type-specialized-element-type type)
	  (if (eq eltype *wild-type*)
	      *wild-type*
	      (dolist (stype-name specialized-array-element-types
				  (specifier-type 't))
		(let ((stype (specifier-type stype-name)))
		  (when (csubtypep eltype stype)
		    (return stype))))))

    type))


;;; Contaning-Integer-Type  --  Interface
;;;
;;; Return the most specific integer type that can be quickly checked that
;;; includes the given type.
;;; 
(defun containing-integer-type (subtype)
  (dolist (type '(fixnum
		  (signed-byte 32)
		  (unsigned-byte 32)
		  integer)
		(error "~S isn't an integer type?" subtype))
    (when (csubtypep subtype (specifier-type type))
      (return type))))


;;; Hairy-Type-Check-Template  --  Interface
;;;
;;;    If Type has a CHECK-xxx template, but doesn't have a corresponding
;;; primitive-type, then return the template's name.  Otherwise, return NIL.
;;;
(defun hairy-type-check-template (type)
  (declare (type ctype type))
  (typecase type
    (named-type
     (case (named-type-name type)
       (cons 'c::check-cons)
       (symbol 'c::check-symbol)
       (t nil)))
    (numeric-type
     (cond ((type= type (specifier-type 'fixnum))
	    'c::check-fixnum)
	   ((type= type (specifier-type '(signed-byte 32)))
	    'c::check-signed-byte-32)
	   ((type= type (specifier-type '(unsigned-byte 32)))
	    'c::check-unsigned-byte-32)
	   (t nil)))
    (union-type
     (if (type= type (specifier-type '(or function symbol)))
	 'c::check-function-or-symbol
	 nil))
    (function-type
     'c::check-function)
    (t
     nil)))
