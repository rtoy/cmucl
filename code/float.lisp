;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/float.lisp,v 1.4 1990/10/14 19:07:15 ram Exp $
;;;
;;;    This file contains the definitions of float specific number support
;;; (other than irrational stuff, which is in irrat.)  There is code in here
;;; that assumes there are only two float formats: IEEE single and double.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "KERNEL")

(in-package "LISP")
(export '(least-positive-normalized-short-float
	  least-positive-normalized-single-float
	  least-positive-normalized-double-float
	  least-positive-normalized-long-float))

(in-package "EXTENSIONS")
(export '(single-float-positive-infinity short-float-positive-infinity
	  double-float-positive-infinity long-float-positive-infinity
	  single-float-negative-infinity short-float-negative-infinity
	  double-float-negative-infinity long-float-negative-infinity
	  set-floating-point-modes float-denormalized-p float-nan-p
	  float-trapping-nan-p float-infinity-p))

(in-package "KERNEL")


;;;; Utilities:

;;; SINGLE-FROM-BITS, DOUBLE-FROM-BITS  --  Internal
;;;
;;;    These functions let us create floats from bits with the significand
;;; uniformly represented as an integer.  This is less efficient for double
;;; floats, but is more convenient when making special values, etc.
;;;
(defun single-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 24) sig)
	   (type (unsigned-byte 8) exp))
  (make-single-float
   (dpb exp single-float-exponent-byte
	(dpb sig single-float-significand-byte
	     (if (zerop sign) 0 -1)))))
;;;
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
	   (type (unsigned-byte 11) exp))
  (make-double-float (dpb exp double-float-exponent-byte
			  (dpb (ash sig -32) double-float-significand-byte
			       (if (zerop sign) 0 -1)))
		     (ldb (byte 32 0) sig)))
					

;;;; Float parameters:

(defconstant least-positive-single-float (single-from-bits 0 0 1))
(defconstant least-positive-short-float least-positive-single-float)
(defconstant least-negative-single-float (single-from-bits 1 0 1))
(defconstant least-negative-short-float least-negative-single-float)
(defconstant least-positive-double-float (double-from-bits 0 0 1))
(defconstant least-positive-long-float least-positive-double-float)
(defconstant least-negative-double-float (double-from-bits 1 0 1))
(defconstant least-negative-long-float least-negative-double-float)

(defconstant least-positive-normalized-single-float
  (single-from-bits 0 single-float-normal-exponent-min 0))
(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)
(defconstant least-negative-normalized-single-float
  (single-from-bits 1 single-float-normal-exponent-min 0))
(defconstant least-negative-normalized-short-float
  least-negative-normalized-single-float)
(defconstant least-positive-normalized-double-float
  (double-from-bits 0 double-float-normal-exponent-min 0))
(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)
(defconstant least-negative-normalized-double-float
  (double-from-bits 1 double-float-normal-exponent-min 0))
(defconstant least-negative-normalized-long-float
  least-negative-normalized-double-float)

(defconstant most-positive-single-float
  (single-from-bits 0 single-float-normal-exponent-max
		    (ldb single-float-significand-byte -1)))
(defconstant most-positive-short-float most-positive-single-float)
(defconstant most-negative-single-float
  (single-from-bits 1 single-float-normal-exponent-max
		    (ldb single-float-significand-byte -1)))
(defconstant most-negative-short-float most-negative-single-float)
(defconstant most-positive-double-float
  (double-from-bits 0 double-float-normal-exponent-max
		    (ldb double-float-significand-byte -1)))
(defconstant most-positive-long-float most-positive-double-float)
(defconstant most-negative-double-float
  (double-from-bits 1 double-float-normal-exponent-max
		    (ldb double-float-significand-byte -1)))
(defconstant most-negative-long-float most-negative-double-float)

(defconstant single-float-positive-infinity
  (single-from-bits 0 (1+ single-float-normal-exponent-max) 0))
(defconstant short-float-positive-infinity single-float-positive-infinity)
(defconstant single-float-negative-infinity
  (single-from-bits 1 (1+ single-float-normal-exponent-max) 0))
(defconstant short-float-negative-infinity single-float-negative-infinity)
(defconstant double-float-positive-infinity
  (double-from-bits 0 (1+ double-float-normal-exponent-max) 0))
(defconstant long-float-positive-infinity double-float-positive-infinity)
(defconstant double-float-negative-infinity
  (double-from-bits 1 (1+ double-float-normal-exponent-max) 0))
(defconstant long-float-negative-infinity double-float-negative-infinity)

(defconstant single-float-epsilon
  (single-from-bits 0 (- single-float-bias (1- single-float-digits)) 1))
(defconstant short-float-epsilon single-float-epsilon)
(defconstant single-float-negative-epsilon
  (single-from-bits 0 (- single-float-bias single-float-digits) 1))
(defconstant short-float-negative-epsilon single-float-negative-epsilon)
(defconstant double-float-epsilon
  (double-from-bits 0 (- double-float-bias (1- double-float-digits)) 1))
(defconstant long-float-epsilon double-float-epsilon)
(defconstant double-float-negative-epsilon
  (double-from-bits 0 (- double-float-bias double-float-digits) 1))
(defconstant long-float-negative-epsilon double-float-negative-epsilon)


;;;; Float predicates and environment query:

(proclaim '(maybe-inline float-denormalized-p float-infinity-p float-nan-p
			 float-trapping-nan-p))

;;; FLOAT-DENORMALIZED-P  --  Public
;;;
(defun float-denormalized-p (x)
  "Return true if the float X is denormalized."
  (number-dispatch ((x float))
    ((single-float)
     (and (zerop (ldb single-float-exponent-byte (single-float-bits x)))
	  (not (zerop x))))
    ((double-float)
     (and (zerop (ldb double-float-exponent-byte (double-float-high-bits x)))
	  (not (zerop x))))))

(macrolet ((frob (name doc single double)
	     `(defun ,name (x)
		,doc
		(number-dispatch ((x float))
		  ((single-float)
		   (let ((bits (single-float-bits x)))
		     (and (> (ldb single-float-exponent-byte bits)
			     single-float-normal-exponent-max)
			  ,single)))
		  ((double-float)
		   (let ((hi (double-float-high-bits x))
			 (lo (double-float-low-bits x)))
		     (and (> (ldb double-float-exponent-byte hi)
			     double-float-normal-exponent-max)
			  ,double)))))))

  (frob float-infinity-p "Return true if the float X is an infinity (+ or -)."
    (zerop (ldb single-float-significand-byte bits))
    (and (zerop (ldb double-float-significand-byte hi))
	 (zerop lo)))

  (frob float-nan-p "Return true if the float X is a NaN (Not a Number)."
    (not (zerop (ldb single-float-significand-byte bits)))
    (or (not (zerop (ldb double-float-significand-byte hi)))
	(not (zerop lo))))

  (frob float-trapping-nan-p
    "Return true if the float X is a trapping NaN (Not a Number)."
    (not (zerop (logand (ldb single-float-significand-byte bits)
			single-float-trapping-nan-bit)))
    (progn
      lo; ignore
      (not (zerop (logand (ldb double-float-significand-byte hi)
			  double-float-trapping-nan-bit))))))


;;; FLOAT-PRECISION  --  Public
;;;
;;;    If denormalized, use a subfunction from INTEGER-DECODE-FLOAT to find the
;;; actual exponent (and hence how denormalized it is), otherwise we just
;;; return the number of digits or 0.
;;;
(proclaim '(maybe-inline float-precision))
(defun float-precision (f)
  "Returns a non-negative number of significant digits in it's float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (macrolet ((frob (digits bias decode)
	       `(cond ((zerop f) 0)
		      ((float-denormalized-p f)
		       (multiple-value-bind (ignore exp)
					    (,decode f)
			 (declare (ignore ignore))
			 (truly-the fixnum
				    (+ ,digits (1- ,digits) ,bias exp))))
		      (t
		       ,digits))))
    (number-dispatch ((f float))
      ((single-float)
       (frob single-float-digits single-float-bias
	 integer-decode-single-denorm))
      ((double-float)
       (frob double-float-digits double-float-bias
	 integer-decode-double-denorm)))))


(defun float-sign (float1 &optional (float2 (float 1 float1)))
  "Returns a floating-point number that has the same sign as
   float1 and, if float2 is given, has the same absolute value
   as float2."
  (declare (float float1 float2))
  (float-sign float1 float2))

(defun float-format-digits (format)
  (ecase format
    ((short-float single-float) single-float-digits)
    ((double-float long-float) double-float-digits)))

(proclaim '(inline float-digits float-radix))

(defun float-digits (f)
  "Returns a non-negative number of radix-b digits used in the
   representation of it's argument.  See Common Lisp: The Language
   by Guy Steele for more details."
  (number-dispatch ((f float))
    ((single-float) single-float-digits)
    ((double-float) double-float-digits)))

(defun float-radix (f)
  "Returns (as an integer) the radix b of its floating-point
   argument."
  (declare (ignore f))
  2)



;;;; INTEGER-DECODE-FLOAT and DECODE-FLOAT:

(proclaim '(maybe-inline integer-decode-single-float
			 integer-decode-double-float))

;;; INTEGER-DECODE-SINGLE-DENORM  --  Internal
;;;
;;;    Handle the denormalized case of INTEGER-DECODE-FLOAT for SINGLE-FLOAT.
;;;
(defun integer-decode-single-denorm (x)
  (declare (type single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (sig (ash (ldb single-float-significand-byte bits) 1))
	 (extra-bias 0))
    (declare (type (unsigned-byte 24) sig)
	     (type (integer 0 23) extra-bias))
    (loop
      (unless (zerop (logand sig single-float-hidden-bit))
	(return))
      (setq sig (ash sig 1))
      (incf extra-bias))
    (values sig
	    (- (- single-float-bias) single-float-digits extra-bias)
	    (if (minusp (float-sign x)) -1 1))))


;;; INTEGER-DECODE-SINGLE-FLOAT  --  Internal
;;;
;;;    Handle the single-float case of INTEGER-DECODE-FLOAT.  If an infinity or
;;; NAN, error.  If a denorm, call i-d-s-DENORM to handle it.
;;;
(defun integer-decode-single-float (x)
  (declare (single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (exp (ldb single-float-exponent-byte bits))
	 (sig (ldb single-float-significand-byte bits))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- exp single-float-bias single-float-digits)))
    (declare (fixnum biased))
    (assert (<= exp single-float-normal-exponent-max) ()
	    "Can't decode NAN or infinity: ~S." x)
    (cond ((and (zerop exp) (zerop sig))
	   (values 0 biased sign))
	  ((< exp single-float-normal-exponent-min)
	   (integer-decode-single-denorm x))
	  (t
	   (values (logior sig single-float-hidden-bit) biased sign)))))


;;; INTEGER-DECODE-DOUBLE-DENORM  --  Internal
;;;
;;;    Like INTEGER-DECODE-SINGLE-DENORM, only doubly so.
;;;
(defun integer-decode-double-denorm (x)
  (declare (type double-float x))
  (let* ((high-bits (double-float-high-bits (abs x)))
	 (sig-high (ldb double-float-significand-byte high-bits))
	 (low-bits (double-float-low-bits x))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- (- double-float-bias) double-float-digits)))
    (if (zerop sig-high)
	(let ((sig low-bits)
	      (extra-bias (- double-float-digits 33))
	      (bit (ash 1 31)))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig bit)) (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (ash sig (- double-float-digits 32))
		  (truly-the fixnum (- biased extra-bias))
		  sign))
	(let ((sig (ash sig-high 1))
	      (extra-bias 0))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig double-float-hidden-bit))
	      (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (logior (ash sig 32) (ash low-bits (1- extra-bias)))
		  (truly-the fixnum (- biased extra-bias))
		  sign)))))


;;; INTEGER-DECODE-DOUBLE-FLOAT  --  Internal
;;;
;;;    Like INTEGER-DECODE-SINGLE-FLOAT, only doubly so.
;;;
(defun integer-decode-double-float (x)
  (declare (double-float x))
  (let* ((abs (abs x))
	 (hi (double-float-high-bits abs))
	 (lo (double-float-low-bits abs))
	 (exp (ldb double-float-exponent-byte hi))
	 (sig (ldb double-float-significand-byte hi))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- exp double-float-bias double-float-digits)))
    (declare (fixnum biased))
    (assert (<= exp double-float-normal-exponent-max) ()
	    "Can't decode NAN or infinity: ~S." x)
    (cond ((and (zerop exp) (zerop sig) (zerop lo))
	   (values 0 biased sign))
	  ((< exp double-float-normal-exponent-min)
	   (integer-decode-double-denorm x))
	  (t
	   (values
	    (logior (ash (logior (ldb double-float-significand-byte hi)
				 double-float-hidden-bit)
			 32)
		    lo)
	    biased sign)))))


;;; INTEGER-DECODE-FLOAT  --  Public
;;;
;;;    Dispatch to the correct type-specific i-d-f function.
;;;
(defun integer-decode-float (x)
  "Returns three values:
   1) an integer representation of the significand.
   2) the exponent for the power of 2 that the significand must be multiplied
      by to get the actual value.  This differs from the DECODE-FLOAT exponent
      by FLOAT-DIGITS, since the significand has been scaled to have all its
      digits before the radix point.
   3) -1 or 1 (i.e. the sign of the argument.)"
  (number-dispatch ((x float))
    ((single-float)
     (integer-decode-single-float x))
    ((double-float)
     (integer-decode-double-float x))))


(proclaim '(maybe-inline decode-single-float decode-double-float))

;;; DECODE-SINGLE-DENORM  --  Internal
;;;
;;;    Handle the denormalized case of DECODE-SINGLE-FLOAT.  We call
;;; INTEGER-DECODE-SINGLE-DENORM and then make the result into a float.
;;;
(defun decode-single-denorm (x)
  (declare (type single-float x))
  (multiple-value-bind (sig exp sign)
		       (integer-decode-single-denorm x)
    (values (make-single-float
	     (dpb sig single-float-significand-byte
		  (dpb single-float-bias single-float-exponent-byte 0)))
	    (truly-the fixnum (+ exp single-float-digits))
	    (float sign x))))


;;; DECODE-SINGLE-FLOAT  --  Internal
;;;
;;;    Handle the single-float case of DECODE-FLOAT.  If an infinity or NAN,
;;; error.  If a denorm, call d-s-DENORM to handle it.
;;;
(defun decode-single-float (x)
  (declare (single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (exp (ldb single-float-exponent-byte bits))
	 (sign (float-sign x))
	 (biased (truly-the single-float-exponent (- exp single-float-bias))))
    (assert (<= exp single-float-normal-exponent-max) ()
	    "Can't decode NAN or infinity: ~S." x)
    (cond ((zerop x)
	   (values 0.0f0 biased sign))
	  ((< exp single-float-normal-exponent-min)
	   (decode-single-denorm x))
	  (t
	   (values (make-single-float
		    (dpb single-float-bias single-float-exponent-byte bits))
		   biased sign)))))


;;; DECODE-DOUBLE-DENORM  --  Internal
;;;
;;;    Like DECODE-SINGLE-DENORM, only doubly so.
;;; 
(defun decode-double-denorm (x)
  (declare (double-float x))
  (multiple-value-bind (sig exp sign)
		       (integer-decode-double-denorm x)
    (values (make-double-float
	     (dpb (logand (ash sig -32) (lognot double-float-hidden-bit))
		  double-float-significand-byte
		  (dpb double-float-bias double-float-exponent-byte 0))
	     (ldb (byte 32 0) sig))
	    (truly-the fixnum (+ exp double-float-digits))
	    (float sign x))))


;;; DECODE-DOUBLE-FLOAT  --  Public
;;;
;;;    Like DECODE-SINGLE-FLOAT, only doubly so.
;;;
(defun decode-double-float (x)
  (declare (double-float x))
  (let* ((abs (abs x))
	 (hi (double-float-high-bits abs))
	 (lo (double-float-low-bits abs))
	 (exp (ldb double-float-exponent-byte hi))
	 (sign (float-sign x))
	 (biased (truly-the double-float-exponent (- exp double-float-bias))))
    (assert (<= exp double-float-normal-exponent-max) ()
	    "Can't decode NAN or infinity: ~S." x)
    (cond ((zerop x)
	   (values 0.0d0 biased sign))
	  ((< exp double-float-normal-exponent-min)
	   (decode-double-denorm x))
	  (t
	   (values (make-double-float
		    (dpb double-float-bias double-float-exponent-byte hi)
		    lo)
		   biased sign)))))


;;; DECODE-FLOAT  --  Public
;;;
;;;    Dispatch to the appropriate type-specific function.
;;;
(defun decode-float (f)
  "Returns three values:
   1) a floating-point number representing the significand.  This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (number-dispatch ((f float))
    ((single-float)
     (decode-single-float f))
    ((double-float)
     (decode-double-float f))))


;;;; SCALE-FLOAT:

(proclaim '(maybe-inline scale-single-float scale-double-float))

;;; SCALE-FLOAT-MAYBE-UNDERFLOW  --  Internal
;;;
;;;    Handle float scaling where the X is denormalized or the result is
;;; denormalized or underflows to 0.
;;;
(defun scale-float-maybe-underflow (x exp)
  (multiple-value-bind (sig old-exp)
		       (integer-decode-float x)
    (let* ((digits (float-digits x))
	   (new-exp (+ exp old-exp digits
		       (etypecase x
			 (single-float single-float-bias)
			 (double-float double-float-bias))))
	   (sign (if (minusp (float-sign x)) 1 0)))
      (cond
       ((< new-exp
	   (etypecase x
	     (single-float single-float-normal-exponent-min)
	     (double-float double-float-normal-exponent-min)))
	(when (and (not (float-denormalized-p x))
		   (current-float-trap :inexact))
	  (error 'floating-point-inexact :operation 'scale-float
		 :operands (list x exp)))
	(let ((shift (1- new-exp)))
	  (cond
	   ((< shift (- (1- digits)))
	    (when (current-float-trap :underflow)
	      (error 'floating-point-underflow :operation 'scale-float
		     :operands (list x exp)))
	    (float-sign x 0.0))
	   (t
	    (etypecase x
	      (single-float (single-from-bits sign 0 (ash sig shift)))
	      (double-float (double-from-bits sign 0 (ash sig shift))))))))
       (t
	(etypecase x
	  (single-float (single-from-bits sign new-exp sig))
	  (double-float (double-from-bits sign new-exp sig))))))))


;;; SCALE-FLOAT-MAYBE-OVERFLOW  --  Internal
;;;
;;;    Called when scaling a float overflows, or the oringinal float was a NaN
;;; or infinity.  If overflow errors are trapped, then error, otherwise return
;;; the appropriate infinity.  If a NaN, signal or not as appropriate.
;;;
(defun scale-float-maybe-overflow (x exp)
  (cond
   ((float-infinity-p x)
    ;; Infinity is infinity, no matter how small...
    x)
   ((float-nan-p x)
    (when (and (float-trapping-nan-p x)
	       (current-float-trap :invalid))
      (error 'floating-point-invalid :operation 'scale-float
	     :operands (list x exp)))
    x)
   (t
    (when (current-float-trap :overflow)
      (error 'floating-point-overflow :operation 'scale-float
	     :operands (list x exp)))
    (when (current-float-trap :inexact)
      (error 'floating-point-inexact :operation 'scale-float
	     :operands (list x exp)))
    (* (float-sign x)
       (etypecase x
	 (single-float single-float-positive-infinity)
	 (double-float double-float-positive-infinity))))))


;;; SCALE-SINGLE-FLOAT, SCALE-DOUBLE-FLOAT  --  Internal
;;;
;;;    Scale a single or double float, calling the correct over/underflow
;;; functions.
;;;
(defun scale-single-float (x exp)
  (declare (single-float x) (fixnum exp))
  (let* ((bits (single-float-bits x))
	 (old-exp (ldb single-float-exponent-byte bits))
	 (new-exp (+ old-exp exp)))
    (cond
     ((zerop x) x)
     ((or (< old-exp single-float-normal-exponent-min)
	  (< new-exp single-float-normal-exponent-min))
      (scale-float-maybe-underflow x exp))
     ((or (> old-exp single-float-normal-exponent-max)
	  (> new-exp single-float-normal-exponent-max))
      (scale-float-maybe-overflow x exp))
     (t
      (make-single-float (dpb new-exp single-float-exponent-byte bits))))))
;;;
(defun scale-double-float (x exp)
  (declare (double-float x) (fixnum exp))
  (let* ((hi (double-float-high-bits x))
	 (lo (double-float-low-bits x))
	 (old-exp (ldb double-float-exponent-byte hi))
	 (new-exp (+ old-exp exp)))
    (cond
     ((zerop x) x)
     ((or (< old-exp double-float-normal-exponent-min)
	  (< new-exp double-float-normal-exponent-min))
      (scale-float-maybe-underflow x exp))
     ((or (> old-exp double-float-normal-exponent-max)
	  (> new-exp double-float-normal-exponent-max))
      (scale-float-maybe-overflow x exp))
     (t
      (make-double-float (dpb new-exp double-float-exponent-byte hi)
			 lo)))))


;;; SCALE-FLOAT  --  Public
;;;
;;;    Dispatch to the correct type-specific scale-float function.
;;;
(defun scale-float (f ex)
  "Returns the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (number-dispatch ((f float))
    ((single-float)
     (scale-single-float f ex))
    ((double-float)
     (scale-double-float f ex))))


;;;; Converting to/from floats:

(defun float (number &optional (other () otherp))
  "Converts any REAL to a float.  If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT.  If OTHER is provided, the
  result is the same float format as OTHER."
  (if otherp
      (number-dispatch ((number real) (other float))
	(((foreach rational single-float double-float)
	  (foreach single-float double-float))
	 (coerce number '(dispatch-type other))))
      (if (floatp number)
	  number
	  (coerce number 'single-float))))


(macrolet ((frob (name type)
	     `(defun ,name (x)
		(number-dispatch ((x real))
		  (((foreach single-float double-float fixnum))
		   (coerce x ',type))
		  ((bignum)
		   (bignum-to-float x ',type))
		  ((ratio)
		   (let ((num (numerator x))
			 (den (denominator x)))
		     (if (and (fixnump num) (fixnump den))
			 (/ (coerce num ',type) (coerce den ',type))
			 (float-bignum-ratio x ',type))))))))
  (frob %single-float single-float)
  (frob %double-float double-float))


;;; %UNARY-TRUNCATE  --  Interface
;;;
;;;    This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer.  Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
(defun %unary-truncate (number)
  (number-dispatch ((number real))
    ((integer) number)
    ((ratio) (truncate (numerator number) (denominator number)))
    (((foreach single-float double-float))
     (if (<= (float most-negative-fixnum number)
	     number
	     (float most-positive-fixnum number))
	 (truly-the fixnum (%unary-truncate number))
	 (multiple-value-bind (bits exp)
			      (integer-decode-float number)
	   (let ((res (ash bits exp)))
	     (if (minusp number)
		 (- res)
		 res)))))))


(defun rational (x)
  "RATIONAL produces a rational number for any real numeric argument.  This is
  more efficient than RATIONALIZE, but it assumes that floating-point is
  completely accurate, giving a result that isn't as pretty."
  (number-dispatch ((x real))
    (((foreach single-float double-float))
     (multiple-value-bind (bits exp)
			  (integer-decode-float x)
       (if (eql bits 0)
	   0
	   (let* ((int (if (minusp x) (- bits) bits))
		  (digits (float-digits x))
		  (ex (+ exp digits)))
	     (if (minusp ex)
		 (integer-/-integer int (ash 1 (+ digits (- ex))))
		 (integer-/-integer (ash int ex) (ash 1 digits)))))))
    ((rational) x)))


(defun rationalize (x)
  "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (number-dispatch ((x real))
    (((foreach single-float double-float))
     ;; Thanks to Kim Fateman, who stole this function rationalize-float
     ;; from macsyma's rational. Macsyma'a rationalize was written
     ;; by the legendary Gosper (rwg). Gosper is now working for Symbolics.
     ;; Guy Steele said about Gosper, "He has been called the
     ;; only living 17th century mathematician and is also the best
     ;; pdp-10 hacker I know." So, if you can understand or debug this
     ;; code you win big.
     (cond ((minusp x) (- (rationalize (- x))))
	   ((zerop x) 0)
	   (t
	    (let ((eps (if (typep x 'single-float)
			   single-float-epsilon
			   double-float-epsilon))
		  (y ())
		  (a ()))
	      (do ((xx x (setq y (/ (float 1.0 x) (- xx (float a x)))))
		   (num (setq a (truncate x))
			(+ (* (setq a (truncate y)) num) onum))
		   (den 1 (+ (* a den) oden))
		   (onum 1 num)
		   (oden 0 den))
		  ((and (not (zerop den))
			(not (> (abs (/ (- x (/ (float num x)
						(float den x)))
					x))
				eps)))
		   (integer-/-integer num den))
		(declare ((dispatch-type x) xx)))))))
    ((rational) x)))
