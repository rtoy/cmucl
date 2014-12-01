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
