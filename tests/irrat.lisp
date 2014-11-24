;; Tests of special irrational functions

(defpackage :irrat-tests
  (:use :cl :lisp-unit))

(in-package "IRRAT-TESTS")

;; This tests that log base 2 returns the correct value and the
;; correct type.
(define-test log2
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

;; This tests that log base 10 returns the correct value and the
;; correct type.
(define-test log10
  (dolist (number '(100 100f0 100d0 #+double-double 100w0))
    (dolist (base '(10 10f0 10d0 #+double-double 10w0))
      ;; This tests that log returns the correct value and the correct type.
      (let* ((result (log number base))
	     (relerr (/ (abs (- result 2)) 2)))
	;; Figure out the expected type of the result and the maximum
	;; allowed relative error.  It turns out that for these test
	;; cases, the result is exactly 2 except when the result type
	;; is a double-double-float.  In that case, there is a slight
	;; error for (log 100w0 10).
	(multiple-value-bind (true-type allowed-error)
	    (etypecase number
	      ((or integer single-float)
	       (etypecase base
		 ((or integer single-float)
		  (values 'single-float 0))
		 (double-float
		  (values 'double-float 0))
		 #+double-double
		 (ext:double-double-float
		  (values 'ext:double-double-float
			  7.5d-33))))
	      (double-float
	       (etypecase base
		 ((or integer single-float double-float)
		  (values 'double-float 0))
		 #+double-double
		 (ext:double-double-float
		  (values 'ext:double-double-float
			  7.5d-33))))
	      #+double-double
	      (ext:double-double-float
	       (values 'ext:double-double-float
		       7.5d-33)))
	  (assert-true (<= relerr allowed-error)
		       number base result relerr allowed-error)
	  (assert-true (typep result true-type)
		       number baes result true-type))))))

(define-test dd-log2
  ;; Verify that for x = 10^k for k = 1 to 300 that (kernel::dd-%log2
  ;; x) is close to the expected value. Previously, a bug caused
  ;; (kernel::dd-%log2 100w0) to give 6.1699... instead of 6.64385.
  (loop for k from 1 below 300
	and x = (expt 10 k)
	and y = (kernel::dd-%log2 (float x 1w0))
	and z = (/ (log (float x 1d0)) (log 2d0))
	and e = (/ (abs (- y z)) z)
	do (assert-true (<= e 2d-16)
			k y z e)))