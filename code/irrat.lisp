;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/irrat.lisp,v 1.3 1990/07/31 17:22:14 wlott Exp $
;;;
;;; This file contains all the irrational functions.  Actually, most of the
;;; work is done by calling out to C...
;;;
;;; Author: William Lott.
;;; 

(in-package "KERNEL")


;;;; Random constants, utility functions, and macros.

(defconstant pi 3.14159265358979323846264338327950288419716939937511L0)
;(defconstant e 2.71828182845904523536028747135266249775724709369996L0)

(defmacro def-math-rtn (name num-args)
  (let ((function (intern (concatenate 'simple-string
				       "%"
				       (string-upcase name)))))
    `(def-c-routine (,name ,function) (double-float)
       ,@(let ((results nil))
	   (dotimes (i num-args (nreverse results))
	     (push (list (intern (format nil "ARG-~D" i))
			 'double-float)
		   results))))))

(eval-when (compile load eval)

(defun handle-reals (function var)
  `((((foreach fixnum single-float bignum ratio))
     (coerce (,function (coerce ,var 'double-float)) 'single-float))
    ((double-float)
     (,function ,var))))

); eval-when (compile load eval)


;;;; Stubs for the Unix math library.

;;; Please refer to the Unix man pages for details about these routines.

;;; Trigonometric.
(def-math-rtn "sin" 1)
(def-math-rtn "cos" 1)
(def-math-rtn "tan" 1)
(def-math-rtn "asin" 1)
(def-math-rtn "acos" 1)
(def-math-rtn "atan" 1)
(def-math-rtn "atan2" 2)
(def-math-rtn "sinh" 1)
(def-math-rtn "cosh" 1)
(def-math-rtn "tanh" 1)
(def-math-rtn "asinh" 1)
(def-math-rtn "acosh" 1)
(def-math-rtn "atanh" 1)

;;; Exponential and Logarithmic.
(def-math-rtn "exp" 1)
(def-math-rtn "expm1" 1)
(def-math-rtn "log" 1)
(def-math-rtn "log10" 1)
(def-math-rtn "log1p" 1)
(def-math-rtn "pow" 2)
(def-math-rtn "cbrt" 1)
(def-math-rtn "sqrt" 1)
(def-math-rtn "hypot" 2)


;;;; Power functions.

(defun exp (number)
  "Return e raised to the power NUMBER."
  (number-dispatch ((number number))
    (handle-reals %exp number)
    ((complex)
     (* (exp (realpart number))
	(cis (imagpart number))))))

;;; INTEXP -- Handle the rational base, integer power case.

(defparameter *intexp-maximum-exponent* 10000)

(defun intexp (base power)
  (when (> (abs power) *intexp-maximum-exponent*)
    (cerror "Continue with calculation."
	    "The absolute value of ~S exceeds ~S."
	    power '*intexp-maximum-exponent* base power))
  (cond ((minusp power)
	 (/ (intexp base (- power))))
	((eql base 2)
	 (ash 1 power))
	(t
	 (do ((nextn (ash power -1) (ash power -1))
	      (total (if (oddp power) base 1)
		     (if (oddp power) (* base total) total)))
	     ((zerop nextn) total)
	   (setq base (* base base))
	   (setq power nextn)))))

;;; This function calculates x raised to the nth power.  It separates
;;; the  cases by the type of n, for efficiency reasons, as powers can
;;; be calculated more efficiently if n is a positive integer,  Therefore,
;;; All integer values of n are calculated as positive integers, and
;;; inverted if negative.

(defun expt (base power)
  "Returns BASE raised to the POWER."
  (if (zerop power)
      ;; This is wrong if power isn't an integer.
      (typecase (realpart base)
	(single-float (coerce 1 'single-float))
	(double-float (coerce 1 'double-float))
	(t 1))
      (number-dispatch ((base number) (power number))
	(((foreach fixnum bignum ratio (complex rational)) integer)
	 (intexp base power))
	(((foreach single-float double-float) integer)
	 (coerce (%pow (coerce base 'double-float)
		       (coerce power 'double-float))
		 '(dispatch-type base)))
	(((foreach fixnum bignum ratio single-float)
	  (foreach ratio single-float))
	 (coerce (%pow (coerce base 'double-float)
		       (coerce power 'double-float))
		 'single-float))
	(((foreach fixnum bignum ratio single-float double-float) double-float)
	 (%pow (coerce base 'double-float) (coerce power 'double-float)))
	(((complex rational) ratio)
	 (* (expt (abs base) power)
	    (cis (* power (phase base)))))
	(((complex float) (foreach integer ratio))
	 (* (expt (abs base) power)
	    (cis (* power (phase base)))))
	(((foreach fixnum bignum ratio single-float double-float) complex)
	 (if (minusp base)
	     (/ (exp (* power (log (- base)))))
	     (exp (* power (log base)))))
	(((foreach (complex float) (complex rational)) complex)
	 (exp (* power (log base)))))))

(defun log (number &optional (base nil base-p))
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (if base-p
      (/ (log number) (log base))
      (number-dispatch ((number number))
	(((foreach fixnum bignum ratio single-float))
	 (if (minusp number)
	     (complex (log (- number)) (coerce pi 'single-float))
	     (coerce (%log (coerce number 'double-float)) 'single-float)))
	((double-float)
	 (if (minusp number)
	     (complex (log (- number)) (coerce pi 'double-float))
	     (%log number)))
	((complex) (complex (log (abs number)) (phase number))))))

(defun sqrt (number)
  "Return the square root of NUMBER."
  (number-dispatch ((number number))
    (((foreach fixnum bignum ratio single-float))
     (if (minusp number)
	 (exp (/ (log number) 2))
	 (coerce (%sqrt (coerce number 'double-float)) 'single-float)))
    ((double-float)
     (if (minusp number)
	 (exp (/ (log number) 2))
	 (%sqrt number)))
    ((complex) (exp (/ (log number) 2)))))

;;; ISQRT:  Integer square root - isqrt(n)**2 <= n
;;; Upper and lower bounds on the result are estimated using integer-length.
;;; On each iteration, one of the bounds is replaced by their mean.
;;; The lower bound is returned when the bounds meet or differ by only 1.
;;; Initial bounds guarantee that lg(sqrt(n)) = lg(n)/2 iterations suffice.

(defun isqrt (n)
  "Returns the root of the nearest integer less than
   n which is a perfect square."
  (if (and (integerp n) (not (minusp n)))
      (do* ((lg (integer-length n))
	    (lo (ash 1 (ash (1- lg) -1)))
	    (hi (+ lo (ash lo (if (oddp lg) -1 0))))) ;tighten by 3/4 if possible.
	   ((<= (1- hi) lo) lo)
	(let ((mid (ash (+ lo hi) -1)))
	  (if (<= (* mid mid) n) (setq lo mid) (setq hi mid))))
      (error "Isqrt: ~S argument must be a nonnegative integer" n)))



;;;; Trigonometic and Related Functions

(defun abs (number)
  "Returns the absolute value of the number."
  (number-dispatch ((number number))
    (((foreach single-float double-float fixnum rational))
     (abs number))
    ((complex)
     (let ((rx (realpart number))
	   (ix (imagpart number)))
       (etypecase rx
	 (rational
	  (sqrt (+ (* rx rx) (* ix ix))))
	 (single-float
	  (coerce (%hypot (coerce rx 'double-float)
			  (coerce ix 'double-float))
		  'single-float))
	 (double-float
	  (%hypot rx ix)))))))

(defun phase (number)
  "Returns the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0.  For non-complex negative
  numbers this is PI."
  (etypecase number
    ((or rational single-float)
     (if (minusp number)
	 (coerce pi 'single-float)
	 0.0f0))
    (double-float
     (if (minusp number)
	 (coerce pi 'double-float)
	 0.0d0))
    (complex
     (atan (imagpart number) (realpart number)))))


(defun sin (number)  
  "Return the sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sin number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sin x) (cosh y)) (* (cos x) (sinh y)))))))

(defun cos (number)
  "Return the cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cos number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cos x) (cosh y)) (- (* (sin x) (sinh y))))))))

(defun tan (number)
  "Return the tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tan number)
    ((complex)
     (let* ((num (sin number))
	    (denom (cos number)))
       (if (zerop denom) (error "~S undefined tangent." number)
	   (/ num denom))))))

(defun cis (theta)
  "Return cos(Theta) + i sin(Theta), AKA exp(i Theta)."
  (if (complexp theta)
      (error "Argument to CIS is complex: ~S" theta)
      (complex (cos theta) (sin theta))))

(proclaim '(inline mult-by-i))
(defun mult-by-i (number)
  (complex (imagpart number)
	   (- (realpart number))))

(defun complex-asin (number)
  (- (mult-by-i (log (+ (mult-by-i number) (sqrt (- 1 (* number number))))))))

(defun asin (number)
  "Return the arc sine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-asin number)
	 (coerce (%asin (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-asin number)
	 (coerce (%asin (coerce number 'double-float))
		 '(dispatch-type number))))
    ((complex)
     (complex-asin number))))

(defun complex-acos (number)
  (- (mult-by-i (log (+ number (mult-by-i (sqrt (- (* number number)))))))))

(defun acos (number)
  "Return the arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-acos number)
	 (coerce (%acos (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-acos number)
	 (coerce (%acos (coerce number 'double-float))
		 '(dispatch-type number))))
    ((complex)
     (complex-acos number))))


(defun atan (y &optional (x nil xp))
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (if xp
      (if (and (zerop x) (zerop y))
	  (multiple-value-bind
	      (mag exp sign-x)
	      (integer-decode-float (float x))
	    (declare (ignore mag exp))
	    (if (plusp sign-x)
		y
		(multiple-value-bind
		    (mag exp sign-y)
		    (integer-decode-float (float y))
		  (declare (ignore mag exp))
		  (if (minusp sign-y)
		      (- pi)
		      pi))))
	  (number-dispatch ((y real) (x real))
	    (((foreach fixnum bignum ratio single-float)
	      (foreach fixnum bignum ratio single-float))
	     (coerce (%atan2 (coerce y 'double-float)
			     (coerce x 'double-float))
		     'single-float))
	    ((double-float (foreach fixnum bignum ratio single-float))
	     (%atan2 y (coerce x 'double-float)))
	    (((foreach fixnum bignum ratio single-float double-float)
	      double-float)
	     (%atan2 (coerce y 'double-float) x))))
      (number-dispatch ((y number))
	(handle-reals %atan y)
	((complex)
	 (let ((im (imagpart y))
	       (re (realpart y)))
	   (/ (- (log (complex (- 1 im) re))
		 (log (complex (+ 1 im) (- re))))
	      (complex 0 2)))))))


(defun sinh (number)
  "Return the hyperbolic sine of NUMBER."
  (/ (- (exp number) (exp (- number))) 2))

(defun cosh (number)
  "Return the hyperbolic cosine of NUMBER."
  (/ (+ (exp number) (exp (- number))) 2))

(defun tanh (number)
  "Return the hyperbolic tangent of NUMBER."
  (/ (- (exp number) (exp (- number)))
     (+ (exp number) (exp (- number)))))


(defun asinh (number)
  "Return the hyperbolic arc sine of NUMBER."
  (log (+ number (sqrt (1+ (* number number))))))

(defun acosh (number)
  "Return the hyperbolic arc cosine of NUMBER."
  (log (+ number (* (1+ number) (sqrt (/ (1- number) (1+ number)))))))

(defun atanh (number)
  "Return the hyperbolic arc tangent of NUMBER."
  (log (* (1+ number) (sqrt (/ (- 1 (* number number)))))))

