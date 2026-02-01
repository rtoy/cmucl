;; Tests of special irrational functions

(defpackage :irrat-tests
  (:use :cl :lisp-unit))

(in-package "IRRAT-TESTS")

(defun relerr (actual expected)
  (/ (abs (- actual expected))
     expected))

;; This tests that log base 2 returns the correct value and the
;; correct type.
(define-test log2.result-types
  (dolist (number '(4 4f0 4d0 #+double-double 4w0))
    (dolist (base '(2 2f0 2d0 #+double-double 2w0))
      ;; This tests that log returns the correct value and the correct type.
      (let* ((result (log number base))
	     (true-type (etypecase number
			  ((or integer single-float)
			   (etypecase base
			     ((or integer single-float) 'single-float)
			     (double-float 'double-float)
			     #+double-double
			     (ext:double-double-float 'ext:double-double-float)))
			  (double-float
			   (etypecase base
			     ((or integer single-float double-float)
			      'double-float)
			     #+double-double
			     (ext:double-double-float 'ext:double-double-float)))
			  #+double-double
			  (ext:double-double-float
			   'ext:double-double-float))))
	(assert-equal (coerce 2 true-type) result
		      number base)
	(assert-true (typep result true-type)
		     result true-type)))))

(define-test log2.special-cases
  (let* ((y (log 3/2 2))
	 (e (relerr y 0.5849625007211562d0)))
    (assert-true (<= e
		     2.308d-8)
		 e y))
  (let* ((y (log -3/2 2))
	 (ry (realpart y))
	 (iy (imagpart y))
	 (er (relerr ry 0.5849625007211562d0))
	 (ei (relerr iy (/ pi (log 2d0)))))
    (assert-true (<= er 2.308d-8)
		 er ry)
    (assert-true (<= ei 1.433d-8)
		 ei iy)))

;; This tests that log base 10 returns the correct value and the
;; correct type.
(define-test log10.result-types
  (dolist (number '(100 100f0 100d0 #+double-double 100w0))
    (dolist (base '(10 10f0 10d0 #+double-double 10w0))
      ;; This tests that log returns the correct value and the correct type.
      (let* ((result (log number base))
	     (true-type
	       (etypecase number
		 ((or integer single-float)
		  (etypecase base
		    ((or integer single-float)
		     'single-float)
		    (double-float
		     'double-float)
		    #+double-double
		    (ext:double-double-float
		     'ext:double-double-float)))
		 (double-float
		  (etypecase base
		    ((or integer single-float double-float)
		     'double-float)
		    #+double-double
		    (ext:double-double-float
		     'ext:double-double-float)))
		 #+double-double
		 (ext:double-double-float
		  'ext:double-double-float))))
	(assert-equalp 2 result
		       number base result)
	(assert-true (typep result true-type)
		     number base result true-type)))))

(define-test dd-log2.special-cases
  ;; Verify that for x = 10^k for k = 1 to 300 that (kernel::dd-%log2
  ;; x) is close to the expected value. Previously, a bug caused
  ;; (kernel::dd-%log2 100w0) to give 6.1699... instead of 6.64385.
  (loop for k from 1 below 300
	for x = (expt 10 k)
	for y = (kernel::dd-%log2 (float x 1w0))
	for z = (/ (log (float x 1d0)) (log 2d0))
	for e = (/ (abs (- y z)) z)
	do (assert-true (<= e 2d-16)
			k y z e))
  (let ((y (kernel::dd-%log2 (sqrt 2w0))))
    (assert-true (<= (relerr y 1/2)
		     (* 2.7 (scale-float 1d0 (- (float-digits 1w0)))))
		 y))
  (let ((y (kernel::dd-%log2 (sqrt 0.5w0))))
    (assert-true (<= (relerr y -1/2)
		     (* 2.7 (scale-float 1d0 (- (float-digits 1w0)))))
		 y))
  (assert-true (typep (log (ash 1 3000) 2) 'single-float))
  (assert-true (typep (log (ash 1 3000) 2f0) 'single-float))
  (assert-true (typep (log (ash 1 3000) 2d0) 'double-float))
  (assert-true (typep (log (ash 1 3000) 2w0) 'ext:double-double-float)))


(define-test dd-log2.powers-of-2
  (loop for k from -1074 below 1024
	for x = (scale-float 1w0 k)
	for y = (kernel::dd-%log2 x)
	do (assert-equalp k y
			  k x y)))

(define-test dd-log10.special-cases
  (let ((y (kernel::dd-%log10 (sqrt 10w0))))
    (assert-true (<= (relerr y 1/2)
		     (* 0.25 (scale-float 1d0 (- (float-digits 1w0)))))))
  (assert-true (typep (log (ash 1 3000) 10) 'single-float))
  (assert-true (typep (log (ash 1 3000) 10f0) 'single-float))
  (assert-true (typep (log (ash 1 3000) 10d0) 'double-float))
  (assert-true (typep (log (ash 1 3000) 10w0) 'ext:double-double-float)))

(define-test dd-log10.powers-of-ten
  ;; It would be nice if dd-%log10 produce the exact result for powers
  ;; of ten, but we currently don't. But note that the maximum
  ;; relative error is less than a double-double epsilon.
  (let ((threshold (* 0.109 (scale-float 1d0 (- (float-digits 1w0))))))
    (loop for k from -323 below 0
	  for x = (expt 10 k)
	  for y = (kernel::dd-%log10 (float x 1w0))
	  for e = (relerr y k)
	  do (assert-true (<= e threshold)
			  k e x y))
    (loop for k from 1 to 308
	  for x = (expt 10 k)
	  for y = (kernel::dd-%log10 (float x 1w0))
	  for e = (relerr y k)
	  do (assert-true (<= e threshold)
			  k e x y))))


(define-test log2.relationships
  (loop for k from 1 below 1000
	for x = (expt 1.1w0 k)
	for logx = (kernel::dd-%log2 x)
	for log1/x = (kernel::dd-%log2 (/ x))
	do (assert-true (<= (abs (+ logx log1/x)) (* 1 double-float-epsilon)))))

(define-test expt-integer
  (let ((power (1+ kernel::*intexp-maximum-exponent*)))
    ;; Make sure we error out in the usual case with the power too
    ;; large.
    (assert-error 'kernel::intexp-limit-error
		  (expt 2 power))
    (assert-error 'kernel::intexp-limit-error
		  (expt 2 (- power)))
    ;; But raising 0 or 1 to a power shouldn't signal anything, except
    ;; the obvious division-by-zero.
    (assert-eql 1 (expt 1 power))
    (cond ((evenp power)
	   (assert-eql 1 (expt -1 power))
	   (assert-eql -1 (expt -1 (1+ power))))
	  (t
	   (assert-eql -1 (expt -1 power))
	   (assert-eql 1 (expt -1 (1+ power)))))
    (assert-eql 0 (expt 0 power))
    (assert-error 'division-by-zero (expt 0 (- power)))))

(define-test sqrt-exceptional-vales
  ;; Short cuts for +infinity, -infinity, and NaN (where NaN has a
  ;; positive sign).
  (let ((nan (abs (ext:with-float-traps-masked (:invalid :divide-by-zero)
	       ;; This produces some NaN.  We don't care what the
	       ;; actual bits are.
	       (/ 0d0 0d0))))
	(inf #.ext:double-float-positive-infinity)
	(minf #.ext:double-float-negative-infinity))
    ;; These tests come from Kahan's paper, Branch Cuts for Elementary
    ;; Functions.
    (ext:with-float-traps-masked (:invalid)
      ;; sqrt(-beta +/- i0) = +0 +/- sqrt(beta), beta >= 0
      (assert-eql (complex +0d0 2d0)
		  (sqrt (complex -4d0 +0d0)))
      (assert-eql (complex +0d0 -2d0)
		  (sqrt (complex -4d0 -0d0)))
      ;; sqrt(x +/- inf) = +inf +/- inf for all finite x.
      (assert-eql (complex inf inf)
		  (sqrt (complex 4d0 inf)))
      (assert-eql (complex inf minf)
		  (sqrt (complex 4d0 minf)))
      ;; sqrt(NaN + i*beta) = NaN + i NaN
      (let ((z (sqrt (complex nan 4d0))))
	(assert-true (ext:float-nan-p (realpart z)))
	(assert-true (ext:float-nan-p (imagpart z))))
      ;; sqrt(beta +i NaN) = NaN + i NaN
      (let ((z (sqrt (complex 4d0 nan))))
	(assert-true (ext:float-nan-p (realpart z)))
	(assert-true (ext:float-nan-p (imagpart z))))
      ;; sqrt(NaN + iNaN) = NaN + i NaN
      (let ((z (sqrt (complex nan nan))))
	(assert-true (ext:float-nan-p (realpart z)))
	(assert-true (ext:float-nan-p (imagpart z))))
      ;; sqrt(inf +/- i beta) = inf +/- i0
      (assert-eql (complex inf +0d0)
		  (sqrt (complex inf 4d0)))
      (assert-eql (complex inf -0d0)
		  (sqrt (complex inf -4d0)))
      ;; sqrt(inf +/- i NaN) = inf + i NaN
      (let ((z (sqrt (complex inf nan))))
	(assert-eql inf (realpart z))
	(assert-true (ext:float-nan-p (imagpart z))))
      (let ((z (sqrt (complex inf (- nan)))))
	(assert-eql inf (realpart z))
	(assert-true (ext:float-nan-p (imagpart z))))
      ;; sqrt(-inf +/- i beta) = +0 +/- i*inf
      (assert-eql (complex 0d0 inf)
		  (sqrt (complex minf +4d0)))
      (assert-eql (complex 0d0 minf)
		  (sqrt (complex minf -4d0)))
      ;; sqrt(-inf +/- i NaN) = NaN +/- i inf
      (let ((z (sqrt (complex minf nan))))
	(assert-true (ext:float-nan-p (realpart z)))
	(assert-eql inf (imagpart z)))
      (let ((z (sqrt (complex minf (- nan)))))
	(assert-true (ext:float-nan-p (realpart z)))
	(assert-eql minf (imagpart z))))))

;; See bug #314
(define-test tanh-large
    (:tag :issues)
  (assert-eql (complex 1d0 -0d0)
              (tanh #c(200d0 -200d0)))
  (assert-eql (complex 1d0 +0d0)
              (tanh #c(200d0 +200d0)))
  (assert-eql (complex 1w0 -0w0)
              (tanh #c(200w0 -200w0)))
  (assert-eql (complex 1w0 +0w0)
              (tanh #c(200w0 200w0))))
  
  
;; See bug #424
(define-test hypot
    (:tag :issues)
  (assert-eql 3.8950612975366328d0
	      (kernel:%hypot 2.302585092994046d0 3.141592653589793d0))
  (let ((result (expt #C(2.302585092994046d0 3.141592653589793d0) 4)))
    (assert-eql -188.4466069439329d0
		(realpart result))
    (assert-eql -132.16721026205448d0
		(imagpart result))))

(define-test cos-tiny
    (:tag issues)
  ;; This test comes from the Maxima testsuite where core-math was not
  ;; computing cos(8.881784197001252d-16) correctly.  It should be
  ;; exactly 1.  We were getting one bit less.
  (assert-eql 1d0
	      (cos (kernel:make-double-float 1020264448 0))))

(define-test log.special-value
    (:tag issues)
  ;; This test comes from the Maxima testsuite where core-math was not
  ;; computing log(0.6899991035461426d0) => -0.37106498060016496d0.
  ;; The computed result looked it had like a single-precision accuracy.
  (assert-eql -0.37106498060016496d0
	      (kernel:%log (kernel:make-double-float (+ 1071644672 398457) 0))))

