;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/float-tran.lisp,v 1.3 1990/09/28 06:41:54 ram Exp $
;;;
;;; This file contains floating-point specific transforms, and may be somewhat
;;; implementation dependent in its assumptions of what the formats are.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "C")


;;;; Coercions:

#-new-compiler
(progn
  (defun %single-float (x) (coerce x 'single-float))
  (defun %double-float (x) (coerce x 'double-float)))

(defknown %single-float (real) single-float (movable foldable flushable))
(defknown %double-float (real) double-float (movable foldable flushable))

(deftransform float ((n &optional f) (* &optional single-float))
  '(%single-float n))

(deftransform float ((n f) (* double-float))
  '(%double-float n))

(deftransform %single-float ((n) (single-float))
  'n)

(deftransform %double-float ((n) (double-float))
  'n)

(deftransform coerce ((n type)
		      (* (constant-argument
			  (member float short-float single-float))))
  '(%single-float n))

(deftransform coerce ((n type)
		      (* (constant-argument
			  (member double-float long-float))))
  '(%double-float n))



;;;; Float accessors:

(defknown make-single-float ((signed-byte 32)) single-float
  (movable foldable flushable))

(defknown make-double-float ((signed-byte 32) (unsigned-byte 32)) double-float
  (movable foldable flushable))

(defknown single-float-bits (single-float) (signed-byte 32)
  (movable foldable flushable))

(defknown double-float-high-bits (double-float) (signed-byte 32)
  (movable foldable flushable))

(defknown double-float-low-bits (double-float) (unsigned-byte 32)
  (movable foldable flushable))


(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))
(defun single-float-bits (x) (single-float-bits x))
(defun double-float-high-bits (x) (double-float-high-bits x))
(defun double-float-low-bits (x) (double-float-low-bits x))

(def-source-transform float-sign (float1 &optional (float2 nil f2-p))
  (let ((n-f1 (gensym)))
    (if f2-p
	`(* (float-sign ,float1) (abs ,float2))
	`(let ((,n-f1 ,float1))
	   (declare (float ,n-f1))
	   (if (minusp (if (typep ,n-f1 'single-float)
			   (single-float-bits ,n-f1)
			   (double-float-high-bits ,n-f1)))
	       (float -1 ,n-f1)
	       (float 1 ,n-f1))))))


;;;; DECODE-FLOAT, INTEGER-DECODE-FLOAT, SCALE-FLOAT:
;;;
;;;    Convert these operations to format specific versions when the format is
;;; known.
;;;

(defconstant single-float-digits
  (1+ (byte-size single-float-significand-byte)))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte)
			vm:word-bits
			1))

(deftype single-float-exponent ()
  `(integer ,(- single-float-normal-exponent-min single-float-bias
		single-float-digits)
	    ,(- single-float-normal-exponent-max single-float-bias)))

(deftype double-float-exponent ()
  `(integer ,(- double-float-normal-exponent-min double-float-bias
		double-float-digits)
	    ,(- double-float-normal-exponent-max double-float-bias)))


(deftype single-float-int-exponent ()
  `(integer ,(- single-float-normal-exponent-min single-float-bias
		(* single-float-digits 2))
	    ,(- single-float-normal-exponent-max single-float-bias
		single-float-digits)))

(deftype double-float-int-exponent ()
  `(integer ,(- double-float-normal-exponent-min double-float-bias
		(* double-float-digits 2))
	    ,(- double-float-normal-exponent-max double-float-bias
		double-float-digits)))

(deftype single-float-significand ()
  `(integer 0 (,(ash 1 single-float-digits))))

(deftype double-float-significand ()
  `(integer 0 (,(ash 1 double-float-digits))))

(defknown decode-single-float (single-float)
  (values single-float single-float-exponent (single-float -1f0 1f0))
  (movable foldable flushable))

(defknown decode-double-float (double-float)
  (values double-float double-float-exponent (double-float -1d0 1d0))
  (movable foldable flushable))

(defknown integer-decode-single-float (single-float)
  (values single-float-significand single-float-int-exponent (integer -1 1))
  (movable foldable flushable))

(defknown integer-decode-double-float (double-float)
  (values double-float-significand double-float-int-exponent (integer -1 1))
  (movable foldable flushable)))

(defknown scale-single-float (single-float fixnum) single-float
  (movable foldable flushable))

(defknown scale-double-float (double-float fixnum) double-float
  (movable foldable flushable))

(deftransform decode-float ((x) (single-float))
  '(decode-single-float x))

(deftransform decode-float ((x) (double-float))
  '(decode-double-float x))

(deftransform integer-decode-float ((x) (single-float))
  '(integer-decode-single-float x))

(deftransform integer-decode-float ((x) (double-float))
  '(integer-decode-double-float x))

(deftransform scale-float ((f ex) (single-float *))
  '(scale-single-float f ex))

(deftransform scale-float ((f ex) (double-float *))
  '(scale-double-float f ex))


;;;; Misc transforms:

;;; Prevent zerop, plusp, minusp from losing horribly.  We can't in general do
;;; float contagion on args to comparison, since Common Lisp semantics says we
;;; are supposed to compare as rationals.  But I think this is harmless, since
;;; 0 will have a precise representation in any float format.
;;;
(macrolet ((frob (op)
	     `(deftransform ,op ((x y) (float (constant-argument (member 0))))
		`(,',op x (float 0 x)))))
  (frob <)
  (frob >)
  (frob =))


(defknown %unary-truncate (real) integer (movable foldable flushable))

;;; Not strictly a float function, but primarily useful on floats:
;;;
(deftransform truncate ((x &optional by)
			(* &optional (constant-argument (member 1))))
  '(let ((res (%unary-truncate x)))
     (values res (- x res))))
