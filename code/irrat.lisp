;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/irrat.lisp,v 1.12 1993/05/08 04:49:36 wlott Exp $")
;;;
;;; **********************************************************************
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

;;; Make these INLINE, since the call to C is at least as compact as a Lisp
;;; call, and saves number consing to boot.
;;;
(defmacro def-math-rtn (name num-args)
  (let ((function (intern (concatenate 'simple-string
				       "%"
				       (string-upcase name)))))
    `(progn
       (proclaim '(inline ,function))
       (export ',function)
       (alien:def-alien-routine (,name ,function) double-float
	 ,@(let ((results nil))
	     (dotimes (i num-args (nreverse results))
	       (push (list (intern (format nil "ARG-~D" i))
			   'double-float)
		     results)))))))

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

;;; This function precisely calculates base raised to an integral power.  It
;;; separates the cases by the sign of power, for efficiency reasons, as powers
;;; can be calculated more efficiently if power is a positive integer.  Values
;;; of power are calculated as positive integers, and inverted if negative.
;;;
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


;;; EXPT  --  Public
;;;
;;;    If an integer power of a rational, use INTEXP above.  Otherwise, do
;;; floating point stuff.  If both args are real, we try %POW right off,
;;; assuming it will return 0 if the result may be complex.  If so, we call
;;; COMPLEX-POW which directly computes the complex result.  We also separate
;;; the complex-real and real-complex cases from the general complex case.
;;;
(defun expt (base power)
  "Returns BASE raised to the POWER."
  (if (zerop power)
      (1+ (* base power))
      (labels ((real-expt (base power rtype)
		 (let* ((fbase (coerce base 'double-float))
			(fpower (coerce power 'double-float))
			(res (coerce (%pow fbase fpower) rtype)))
		   (if (and (zerop res) (minusp fbase))
		       (multiple-value-bind (re im)
					    (complex-pow fbase fpower)
			 (%make-complex (coerce re rtype) (coerce im rtype)))
		       res)))
	       (complex-pow (fbase fpower)
		 (let ((pow (%pow (- fbase) fpower))
		       (fpower*pi (* fpower pi)))
		   (values (* pow (%cos fpower*pi))
			   (* pow (%sin fpower*pi))))))
	(declare (inline real-expt))
	(number-dispatch ((base number) (power number))
	  (((foreach fixnum (or bignum ratio) (complex rational)) integer)
	   (intexp base power))
	  (((foreach single-float double-float) rational)
	   (real-expt base power '(dispatch-type base)))
	  (((foreach fixnum (or bignum ratio) single-float)
	    (foreach ratio single-float))
	   (real-expt base power 'single-float))
	  (((foreach fixnum (or bignum ratio) single-float double-float)
	    double-float)
	   (real-expt base power 'double-float))
	  ((double-float single-float)
	   (real-expt base power 'double-float))
	  (((foreach (complex rational) (complex float)) rational)
	   (* (expt (abs base) power)
	      (cis (* power (phase base)))))
	  (((foreach fixnum (or bignum ratio) single-float double-float)
	    complex)
	   (if (minusp base)
	       (/ (exp (* power (truly-the float (log (- base))))))
	       (exp (* power (truly-the float (log base))))))
	  (((foreach (complex float) (complex rational)) complex)
	   (exp (* power (log base))))))))

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
      (flet ((atan2 (y x)
	       (declare (type double-float y x))
	       (if (plusp (float-sign x))
		   ;; X is either +0.0 or > 0.0
		   (if (zerop x)
		       ;; X is +0.0.
		       (if (plusp (float-sign y))
			   ;; Y is either +0.0 or > 0.0
			   (if (zerop y) +0.0d0 (/ pi 2))
			   ;; Y is either -0.0 or < 0.0
			   (if (zerop y) -0.0d0 (/ pi 2)))
		       ;; X is > 0.0
		       (if (plusp (float-sign y))
			   ;; Y is either +0.0 or > 0.0
			   (if (zerop y) +0.0d0 (%atan2 y x))
			   ;; Y is either -0.0 or < 0.0
			   (if (zerop y) -0.0d0 (%atan2 y x))))
		   ;; X is either -0.0 or < 0.0
		   (if (zerop x)
		       ;; X is -0.0
		       (if (plusp (float-sign y))
			   ;; Y is either +0.0 or > 0.0
			   (if (zerop y) pi (/ pi 2))
			   ;; Y is either -0.0 or < 0.0
			   (if (zerop y) (- pi) (- (/ pi 2))))
		       ;; X is < 0.0
		       (if (zerop y) pi (%atan2 y x))))))
	(atan2 (number-dispatch ((y real))
		 (((foreach fixnum bignum ratio single-float))
		  (coerce y 'double-float))
		 ((double-float) y))
	       (number-dispatch ((x real))
		 (((foreach fixnum bignum ratio single-float))
		  (coerce x 'double-float))
		 ((double-float) x))))
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

