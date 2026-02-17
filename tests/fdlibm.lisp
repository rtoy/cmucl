;; Test that fdlibm routines signals exceptions as expected.

(defpackage :fdlibm-tests
  (:use :cl :lisp-unit))

(in-package "FDLIBM-TESTS")

(defparameter *qnan*
  (ext:with-float-traps-masked (:invalid)
    (* 0 ext:double-float-positive-infinity))
  "Some randon quiet double-float MaN value")

(defparameter *qnan-single-float*
  (ext:with-float-traps-masked (:invalid)
    (* 0 ext:single-float-positive-infinity))
  "Some randon quiet single-float MaN value")

(defparameter *snan*
  (kernel:make-double-float #x7ff00000 1)
  "A randon signaling double-float MaN value")

(defparameter *snan-single-float*
  (kernel:make-single-float #x7f800001)
  "A randon signaling single-float MaN value")

(define-test %cosh.exceptions
    (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%cosh 1000d0))
  (assert-error 'floating-point-overflow
		(kernel:%cosh -1000d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%cosh *snan*))
  (assert-true (ext:float-nan-p (kernel:%cosh *qnan*)))
  (assert-error 'floating-point-overflow
		(kernel:%cosh ext:double-float-positive-infinity))
  (assert-error 'floating-point-overflow
		(kernel:%cosh ext:double-float-negative-infinity))
  
  ;; Same, but with overflow's masked
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh 1000d0))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh -1000d0))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh ext:double-float-positive-infinity))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh ext:double-float-negative-infinity)))
  ;; Test NaN
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%cosh *snan*)))))

(define-test %coshf.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%coshf 100f0))
  (assert-error 'floating-point-overflow
		(kernel:%coshf -100f0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%coshf *snan-single-float*))
  (assert-true (ext:float-nan-p (kernel:%coshf *qnan-single-float*)))
  (assert-error 'floating-point-overflow
		(kernel:%coshf ext:single-float-positive-infinity))
  (assert-error 'floating-point-overflow
		(kernel:%coshf ext:single-float-negative-infinity))
  
  ;; Same, but with overflow's masked
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%coshf 100f0))
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%coshf -100f0))
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%coshf ext:single-float-positive-infinity))
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%coshf ext:single-float-negative-infinity)))
  ;; Test NaN
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%coshf *snan-single-float*)))))

(define-test %sinh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%sinh 1000d0))
  (assert-error 'floating-point-overflow
		(kernel:%sinh -1000d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%sinh *snan*))
  (assert-true (ext:float-nan-p (kernel:%sinh *qnan*)))

  ;; Same, but with overflow's masked
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%sinh 1000d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%sinh -1000d0))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%sinh ext:double-float-positive-infinity))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%sinh ext:double-float-negative-infinity)))
  ;; Test NaN
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%sinh *qnan*))))
  ;; sinh(x) = x for |x| < 2^-28.  Should signal inexact unless x = 0.
  (let ((x (scale-float 1d0 -29))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%sinh x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%sinh x)))))

(define-test %sinhf.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%sinhf 100f0))
  (assert-error 'floating-point-overflow
		(kernel:%sinhf -100f0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%sinhf *snan-single-float*))
  (assert-true (ext:float-nan-p (kernel:%sinhf *qnan-single-float*)))

  ;; Same, but with overflow's masked
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%sinhf 100f0))
    (assert-equal ext:single-float-negative-infinity
		  (kernel:%sinhf -100f0))
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%sinhf ext:single-float-positive-infinity))
    (assert-equal ext:single-float-negative-infinity
		  (kernel:%sinhf ext:single-float-negative-infinity)))
  ;; Test NaN
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%sinhf *qnan-single-float*))))
  ;; sinh(x) = x + x^3/6 + o(x^3).  Thus, if |x| < 2^-17, sinh(x) = x,
  ;; but we should signal inexact except if x is 0.  We're not trying
  ;; to get the exact value where sinh(x) = x.  Just something
  ;; obviously small enough.
  (let ((x (scale-float 1f0 -29))
	(x0 0f0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0f0 (kernel:%sinhf x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%sinhf x)))))

(define-test %tanh.exceptions
  (:tag :fdlibm)
  (assert-true (ext:float-nan-p (kernel:%tanh *qnan*)))
  (assert-error 'floating-point-invalid-operation
		(kernel:%tanh *snan*))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%tanh *snan*))))
  ;; tanh(x) = +/- 1 for |x| > 22, raising inexact, always.
  (let ((x 22.1d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%tanh x)))))

(define-test %tanhf.exceptions
  (:tag :fdlibm)
  (assert-true (ext:float-nan-p (kernel:%tanhf *qnan-single-float*)))
  (assert-error 'floating-point-invalid-operation
		(kernel:%tanhf *snan-single-float*))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%tanhf *snan-single-float*))))
  ;; tanhf(x) = +/- 1 for |x| > 9.1, raising inexact, always.  The
  ;; exact value from cr_tanhf appears to be 0x41102cb3u.  That is
  ;; 9.010913.
  (let ((x 9.1f0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%tanhf x)))))

(define-test %acosh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%acosh ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%acosh 0d0))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%acosh ext:double-float-positive-infinity)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%acosh 0d0)))))

(define-test %acoshf.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%acoshf ext:single-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%acoshf 0f0))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%acoshf ext:single-float-positive-infinity)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%acoshf 0f0)))))

(define-test %asinh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%asinh *snan*))
  (assert-error 'floating-point-overflow
		(kernel:%asinh ext:double-float-positive-infinity))
  (assert-error 'floating-point-overflow
		(kernel:%asinh ext:double-float-negative-infinity))
  (assert-true (ext:float-nan-p (kernel:%asinh *qnan*)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%asinh ext:double-float-positive-infinity))
    (assert-error ext:double-float-negative-infinity
		  (kernel:%asinh ext:double-float-negative-infinity)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%asinh *snan*))))
  (let ((x (scale-float 1d0 -29))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%asinh x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%asinh x)))))

(define-test %asinh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%asinhf *snan-single-float*))
  (assert-error 'floating-point-overflow
		(kernel:%asinhf ext:single-float-positive-infinity))
  (assert-error 'floating-point-overflow
		(kernel:%asinhf ext:single-float-negative-infinity))
  (assert-true (ext:float-nan-p (kernel:%asinhf *qnan-single-float*)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:single-float-positive-infinity
		  (kernel:%asinhf ext:single-float-positive-infinity))
    (assert-error ext:single-float-negative-infinity
		  (kernel:%asinhf ext:single-float-negative-infinity)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%asinhf *snan-single-float*))))
  ;; asinh(x) = x - x^3/6 + o(x^5).  For small enough x, asinh(x) = x
  ;; but we should signal inexact for those cases, except for when x =
  ;; 0.  The threshold is approximately 2^-17.
  (let ((x (scale-float 1f0 -17))
	(x0 0f0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0f0 (kernel:%asinhf x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%asinhf x)))))

(define-test %atanh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%atanh 2d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%atanh -2d0))
  (assert-error 'division-by-zero
		(kernel:%atanh 1d0))
  (assert-error 'division-by-zero
		(kernel:%atanh -1d0))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%atanh 2d0)))
    (assert-true (ext:float-nan-p (kernel:%atanh -2d0))))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%atanh 1d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%atanh -1d0))))

(define-test %expm1.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%expm1 709.8d0))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%expm1 ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%expm1 *snan*))
  (assert-true (ext:float-nan-p (kernel:%expm1 *qnan*)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		 (kernel:%expm1 709.8d0))
    )
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext::float-nan-p (kernel:%expm1 *snan*))))
  ;; expm1(x) = -1 for x < -56*log(2), signaling inexact
  #-core-math
  (let ((x (* -57 (log 2d0))))
    (ext:with-float-traps-enabled (:inexact)
	(assert-error 'floating-point-inexact
		      (kernel:%expm1 x)))))

(define-test %log1p.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%log1p -2d0))
  (assert-error #-core-math 'floating-point-overflow
		#+core-math 'division-by-zero
		(kernel:%log1p -1d0))
  (assert-true (ext:float-nan-p (kernel:%log1p *qnan*)))
  (ext:with-float-traps-masked (#-core-math :overflow
				#+core-math :divide-by-zero)
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log1p -1d0)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%log1p *snan*))))
  ;; log1p(x) = x for |x| < 2^-54, signaling inexact except for x = 0.
  (let ((x (scale-float 1d0 -55))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%log1p x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%log1p x)))))

(define-test %exp.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%exp 710d0))
  (assert-true (ext:float-nan-p (kernel:%exp *qnan*)))
  (assert-error 'floating-point-invalid-operation
		(kernel:%exp *snan*))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%exp ext:double-float-positive-infinity))
  (assert-equal 0d0
		(kernel:%exp -1000d0))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%exp 710d0)))
  (let ((modes (ext:get-floating-point-modes)))
    (unwind-protect
	 (progn
	   (ext:set-floating-point-modes :traps '(:underflow))
	   (assert-error 'floating-point-underflow
			 (kernel:%exp -1000d0)))
      (apply #'ext:set-floating-point-modes modes)))
  (let ((x (scale-float 1d0 -29))
	(x0 0d0))
    ;; exp(x) = x, |x| < 2^-28, with inexact exception unlees x = 0
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 1d0 (kernel:%exp x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%exp x)))))

(define-test %log.exception
  (:tag :fdlibm)
  (assert-error 'division-by-zero
		(kernel:%log 0d0))
  (assert-error 'division-by-zero
		(kernel:%log -0d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%log -1d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%log *snan*))
  (assert-true (ext:float-nan-p (kernel:%log *qnan*)))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log 0d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log -0d0)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%log -1d0)))
    (assert-true (ext:float-nan-p (kernel:%log *snan*)))))

(define-test %acos.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%acos 2d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%acos -2d0))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%acos 2d0)))
    (assert-true (ext:float-nan-p (kernel:%acos -2d0)))))

(define-test %asin.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%asin 2d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%asin -2d0))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%asin 2d0)))
    (assert-true (ext:float-nan-p (kernel:%asin -2d0)))))

(define-test %atan.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%atan *snan*))
  (assert-true (ext:float-nan-p (kernel:%atan *qnan*)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%atan *snan*))))
  ;; atan(x) = x for |x| < 2^-29, signaling inexact.
  (let ((x (scale-float 1d0 -30))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%atan x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%atan x)))))

(define-test %log10.exceptions
  (:tag :fdlibm)
  ;; %log10(2^k) = k
  (dotimes (k 23)
    (assert-equalp k
		  (kernel:%log10 (float (expt 10 k) 1d0))))
  (assert-error 'division-by-zero
		(kernel:%log10 0d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%log10 -1d0))
  (assert-true (ext:float-nan-p (kernel:%log10 *qnan*)))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%log10 ext:double-float-positive-infinity))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log10 0d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log10 -0d0)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%log10 -1d0)))))

(define-test %scalbn.exceptions
  (:tag :fdlibm)
  (let ((modes (ext:get-floating-point-modes)))
    (unwind-protect
	 (progn
	   (ext:set-floating-point-modes :traps '(:underflow))
	   (assert-error 'floating-point-underflow
			 (kernel:%scalbn 1d0 -51000)))
      (apply #'ext:set-floating-point-modes modes)))
  (assert-true 0d0
	       (kernel:%scalbn 1d0 -51000))
  (assert-true -0d0
	       (kernel:%scalbn -1d0 -51000))
  (assert-error 'floating-point-overflow
		(kernel:%scalbn ext:double-float-positive-infinity 1))
  (assert-error 'floating-point-invalid-operation
		(kernel:%scalbn *snan* 1))
  (assert-error 'floating-point-overflow
		(kernel:%scalbn most-positive-double-float 2))
  (assert-error 'floating-point-overflow
		(kernel:%scalbn most-negative-double-float 2))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%scalbn ext:double-float-positive-infinity 1))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%scalbn most-positive-double-float 2))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%scalbn most-negative-double-float 2))))

;;; These tests taken from github.com/rtoy/fdlibm-js
(define-test acosh-basic-tests
    (:tag :fdlibm)
  ;; acosh(1) = 0
  (assert-eql 0d0 (acosh 1d0))
  ;; acosh(1.5) = log((sqrt(5)+3)/2, case 1 < x < 2
  (assert-eql 0.9624236501192069d0 (acosh 1.5d0))
  ;; acosh(4) = log(sqrt(15)+4), case 2 < x < 2^28
  (assert-eql #-core-math 2.0634370688955608d0
	      #+core-math 2.0634370688955603d0
	      (acosh 4d0))
  ;; acosh(2^50), case 2^28 < x
  (assert-eql 35.35050620855721d0 (acosh (scale-float 1d0 50)))
  ;; No overflow for most positive
  (assert-eql #-core-math 710.4758600739439d0
	      #+core-math 710.475860073944d0
	      (acosh most-positive-double-float)))

(define-test asinh-basic-tests
    (:tag :fdlibm)
  (assert-eql -0d0 (asinh -0d0))
  (assert-eql 0d0 (asinh 0d0))
  (let ((x (scale-float 1d0 -29))
	(x0 0d0))
    ;; asinh(x) = x for x < 2^-28
    (assert-eql x (asinh x))
    (assert-eql (- x) (asinh (- x))))
  (let ((x (scale-float 1d0 -28)))
    ;; Case 2 > |x| >= 2^-28
    (assert-eql 3.725290298461914d-9 (asinh x))
    (assert-eql -3.725290298461914d-9 (asinh (- x))))
  (let ((x 1d0))
    ;; Case 2 > |x| >= 2^-28
    (assert-eql 0.881373587019543d0 (asinh x))
    (assert-eql -0.881373587019543d0 (asinh (- x))))
  (let ((x 5d0))
    ;; Case 2^28 > |x| > 2
    (assert-eql 2.3124383412727525d0 (asinh x))
    (assert-eql -2.3124383412727525d0 (asinh (- x))))
  (let ((x (scale-float 1d0 28)))
    ;; Case 2^28 > |x|
    (assert-eql 20.101268236238415d0 (asinh x))
    (assert-eql -20.101268236238415d0 (asinh (- x))))
  (let ((x most-positive-double-float))
    ;; No overflow for most-positive-double-float
    (assert-eql #-core-math 710.4758600739439d0
		#+core-math 710.475860073944d0
		(asinh x))
    (assert-eql #-core-math -710.4758600739439d0
		#+core-math -710.475860073944d0
		(asinh (- x)))))
  
(define-test atanh-basic-tests
    (:tag :fdlibm)
  (assert-eql +0d0 (atanh +0d0))
  (assert-eql -0d0 (atanh -0d0))
  ;; atanh(x) = x, |x| < 2^-28
  (let ((x (scale-float 1d0 -29)))
    (assert-eql x (atanh x))
    (assert-eql (- x) (atanh (- x))))
  ;; atanh(0.25) = log(5/3)/2, |x| < 0.5
  (let ((x 0.25d0))
    (assert-eql 0.25541281188299536d0 (atanh x))
    (assert-eql -0.25541281188299536d0 (atanh (- x)))
    ;; There's no guarantee that atanh(1/4) = log(5/3)2 in floating
    ;; point, but it's true in this case with fdlibm
    (assert-eql (/ (log (float 5/3 1d0)) 2) (atanh x)))
  ;; atanh(0.75) = log(7)/2, 0.5 < |x| < 1
  (let ((x 0.75d0))
    (assert-eql 0.9729550745276566d0 (atanh x))
    (assert-eql -0.9729550745276566d0 (atanh (- x)))
    ;; There's no guarantee that atanh(3/4) = log(7)2 in floating
    ;; point, but it's true in this case with fdlibm
    (assert-eql (/ (log 7d0) 2) (atanh x))))

(define-test cosh-basic-tests
    (:tag :fdlibm)
  ;; cosh(2^-55) = 1, tiny x case
  (let ((x (scale-float 1d0 -55)))
    (assert-eql 1d0 (cosh x))
    (assert-eql 1d0 (cosh (- x))))
  ;; cosh(2^-55) = 1, tiny x case
  (let ((x (scale-float 1d0 -56)))
    (assert-eql 1d0 (cosh x))
    (assert-eql 1d0 (cosh (- x))))
  ;; cosh(log(2)/4) = (sqrt(2) + 1)/2^(5/4), case |x| < log(2)/2
  (let ((x (/ (log 2d0) 4)))
    ;; This depends on (/ (log 2d0) 4) producing the value we really
    ;; want as the arg.
    (assert-eql 1.0150517651282178d0 (cosh x))
    (assert-eql 1.0150517651282178d0 (cosh (- x))))
  ;; cosh(10*log(2)) = 1048577/2048, case log(2)/2 < |x| < 22
  (let ((x (* 10 (log 2d0)))
	(y (float 1048577/2048 1d0)))
    (assert-eql y (cosh x))
    (assert-eql y (cosh (- x))))
  ;; cosh(32*log(2)), case 22 <= |x| < log(maxdouble)
  (let ((x (* 32 (log 2d0))))
    (assert-eql 2.1474836479999983d9 (cosh x))
    (assert-eql 2.1474836479999983d9 (cosh (- x))))
  ;; cosh(710.4758600739439), case log(maxdouble) <= |x| <= overflowthreshold
  (let ((x 710.4758600739439d0))
    (assert-eql 1.7976931348621744d308 (cosh x))
    (assert-eql 1.7976931348621744d308 (cosh (- x)))))

(define-test exp-basic-tests
    (:tag :fdlibm)
  ;; No overflow and no underflow
  (let ((x 709.7822265625d0))
    (assert-eql 1.7968190737295725d308 (exp x))
    (assert-eql 5.565390609552841d-309 (exp (- x))))
  ;; exp(7.09782712893383973096e+02), no overflow
  (assert-eql 1.7976931348622732d308 (exp 7.09782712893383973096d+02))
  ;; exp(-7.45133219101941108420e+02), no underflow
  (assert-eql 4.9406564584124654d-324 (exp -7.45133219101941108420d+02))
  ;; Overflow
  (assert-error 'floating-point-overflow (exp 709.7827128933841d0))
  ;; Case |x| < 2^-28
  (let ((x (scale-float 1d0 -29)))
    (assert-eql (+ 1 x) (exp x))
    (assert-eql (- 1 x) (exp (- x))))
  ;; exp(0.5), case log(2)/2 < |x| < 3/2*log(2)
  (let ((x 0.5d0))
    (assert-eql 1.6487212707001282d0 (exp x))
    (assert-eql 0.6065306597126334d0 (exp (- x))))
  ;; exp(2), case |x| > 3/2*log(2)
  (let ((x 2d0))
    (assert-eql 7.38905609893065d0 (exp x))
    (assert-eql 0.1353352832366127d0 (exp (- x))))
  ;; exp(2^-1022), case k < -1021
  (assert-eql 1d0 (exp (scale-float 1d0 -1022)))
  ;; exp(2^-1021), case k >= -1021
  (assert-eql 1d0 (exp (scale-float 1d0 -1021)))
  ;; exp(7.09782712893383973096e+02), no overflow
  (assert-eql 1.7976931348622732d308 (exp 7.09782712893383973096d+02))
  ;; overflow
  (assert-error 'floating-point-overflow (exp 709.7827128933841d0))
  ;; exp(-7.45133219101941108420e+02), no underflow
  (assert-eql 4.9406564584124654d-324 (exp -745.1332191019411d0))
  ;; exp(-745.1332191019412), underflows
  (assert-eql 0d0 (exp -745.1332191019412d0))
  ;; exp(1000) overflow
  (assert-error 'floating-point-overflow (exp 1000d0))
  ;; exp(-1000) underflow
  (assert-eql 0d0 (exp -1000d0)))

(define-test log-basic-tests
    (:tag :fdlibm)
  (assert-eql 0d0 (log 1d0))
  (assert-eql 1d0 (log (exp 1d0)))
  (assert-eql -1d0 (log (exp -1d0)))
  (assert-eql 0.5d0 (log (sqrt (exp 1d0))))
  (assert-eql -0.5d0 (log (sqrt (exp -1d0))))
  ;; Test a denormal arg
  (assert-eql -709.08956571282410d0 (log (scale-float 1d0 -1023)))
  ;; Largest double value
  (assert-eql 709.7827128933840d0 (log most-positive-double-float))
  ;; Tests case 0 < f < 2^-20, k = 0
  ;; log(1+2^-21)
  (assert-eql 4.7683704451632344d-7 (log (+ 1 (scale-float 1d0 -21))))
  ;; Tests case 0 < f < 2^-20, k = 1
  ;; log(2 + 2^-20)
  (assert-eql 0.6931476573969898d0 (log (+ 2(scale-float 1d0 -20))))
  (assert-eql 1.3862943611198906d0 (log 4d0))
  ;; Tests main path, i > 0, k = 0
  (assert-eql 0.3220828910287846d0
	      (log (kernel:make-double-float (+ #x3ff00000 #x6147a) 0)))
  ;; Tests main path, i > 0, k = 1
  (assert-eql 0.35065625373947773d0
	      (log (kernel:make-double-float (+ #x3ff00000 #x6b851) 0)))
  ;; Tests main path, i > 0, k = -1
  (assert-eql -0.3710642895311607d0
	      (log (kernel:make-double-float (+ #x3fe00000 #x6147a) 0)))
  ;; Tests main path, i < 0, k = 0
  (assert-eql 0.3220821999597803d0
	      (log (kernel:make-double-float (+ #x3ff00000 #x61479) 0)))
  ;; Tests main path, i < 0, k = 1
  (assert-eql 1.0152293805197257d0
	      (log (kernel:make-double-float (+ #x40000000 #x61479) 0)))
  ;; Tests main path, i < 0, k = -1
  (assert-eql -0.37106498060016496d0
	      (log (kernel:make-double-float (+ #x3fe00000 #x61479) 0))))

(define-test log-consistency
    (:tag :fdlibm)
  ;; |log(x) + log(1/x)| < 1.77635684e-15, x = 1.2^k, 0 <= k < 2000
  ;; The threshold is experimentally determined
  (let ((x 1d0)
	(max-value -1d0)
	(worst-x 0d0))
    (declare (double-float max-value)
	     (type (double-float 1d0) x))
    (dotimes (k 2000)
      (let ((y (abs (+ (log x) (log (/ x))))))
	(when (> y max-value)
	  (setf worst-x x
		max-value y))
	(setf x (* x 1.4d0))))
    (assert-true (< max-value
		    #-core-math 1.77635684d-15
		    #+core-math 1.42108548d-14)
		 max-value
		 worst-x))
  ;; |exp(log(x)) - x|/x < 5.6766649d-14, x = 1.4^k, 0 <= k < 2000
  (let ((x 1d0)
	(max-error 0d0)
	(worst-x 0d0))
    (declare (double-float max-error worst-x worst-y)
	     (type (double-float 1d0) x))
    (dotimes (k 2000)
      (let ((y (abs (/ (- (exp (log x)) x) x))))
	(when (> y max-error)
	  (setf worst-x x
		max-error y))
	(setf x (* x 1.4d0))))
    (assert-true (< max-error 5.6766649d-14)
		 max-error
		 worst-x
		 worst-y))
  ;; |exp(log(x)) - x|/x < 5.68410245d-14, x = 1.4^(-k), 0 <= k < 2000
  (let ((x 1d0)
	(max-error 0d0)
	(worst-x 0d0))
    (declare (double-float max-error worst-x worst-y)
	     (type (double-float (0d0)) x))
    (dotimes (k 2000)
      (let ((y (abs (/ (- (exp (log x)) x) x))))
	(when (> y max-error)
	  (setf worst-x x
		max-error y))
	(setf x (/ x 1.4d0))))
    (assert-true (< max-error 5.68410245d-14)
		 max-error
		 worst-x)))

(define-test sinh-basic-tests
    (:tag :fdlibm)
  (assert-eql +0d0 (sinh 0d0))
  (assert-eql -0d0 (sinh -0d0))
  ;; sinh(x) = x, |x| < 2^-28
  (let ((x (scale-float 1d0 -29)))
    (assert-eql x (sinh x))
    (assert-eql (- x) (sinh (- x))))
  ;; case |x| < 1
  (assert-eql 0.5210953054937474d0 (sinh 0.5d0))
  (assert-eql -0.5210953054937474d0 (sinh -0.5d0))
  ;; sinh(10*log(2)) = 1048575/2048, case |x| < 22
  (let ((x (* 10 (log 2d0)))
	(y (float 1048575/2048 1d0)))
    (assert-eql y (sinh x))
    (assert-eql (- y) (sinh (- x))))
  ;; sinh(10), case |x| < 22
  (let ((y 11013.232874703393d0))
    (assert-eql y (sinh 10d0))
    (assert-eql (- y) (sinh -10d0)))
  ;; sinh(32*log(2)), case |x| in [22, log(maxdouble)]
  (let ((x (* 32 (log 2d0)))
	(y 2.1474836479999983d9))
    (assert-eql y (sinh x))
    (assert-eql (- y) (sinh (- x))))
  ;; sinh(100), case |x| in [22, log(maxdouble)]
  (let ((y 1.3440585709080678d43))
    (assert-eql y (sinh 100d0))
    (assert-eql (- y) (sinh -100d0)))
  ;; sinh(710....), no overflow, case |x| in [log(maxdouble), overflowthreshold]
  (let ((x 710.4758600739439d0)
	(y 1.7976931348621744d308))
    (assert-eql y (sinh x))
    (assert-eql (- y) (sinh (- x))))
  ;; sinh(710.475860073944), overflow, case |x| > ovfthreshold]
  (let ((x 710.475860073944d0))
    (assert-error 'floating-point-overflow (sinh x))
    (assert-error 'floating-point-overflow (sinh (- x))))
  (assert-error 'floating-point-overflow (sinh 1000d0))
  (assert-error 'floating-point-overflow (sinh -1000d0)))

(define-test tanh-basic-tests
    (:tag :fdlibm)
  ;; case |x| < 2^-55
  (let ((x (scale-float 1d0 -56)))
    (assert-eql x (tanh x))
    (assert-eql (- x) (tanh (- x))))
  ;; tanh(log(2)) = 3/5, case |x| < 1
  (let ((x (log 2d0))
	(y (float 3/5 1d0)))
    (assert-eql y (tanh x))
    (assert-eql (- y) (tanh (- x))))
  ;; tanh(2*log(2)) = 15/17, case |x| < 22
  (let ((x (* 2 (log 2d0)))
	(y (float 15/17 1d0)))
    (assert-eql y (tanh x))
    (assert-eql (- y) (tanh (- x))))
  ;; tanh(100) = 1, case |x| > 22
  (assert-eql 1d0 (tanh 100d0))
  (assert-eql -1d0 (tanh -100d0))
  ;; tanh(1d300), no overflow
  (assert-eql 1d0 (tanh most-positive-double-float))
  (assert-eql -1d0 (tanh (- most-positive-double-float))))

(define-test %asin-basic-tests
    (:tag :fdlibm)
  (let ((x (scale-float 1d0 -28))
	(x0 0d0))
    ;; asin(x) = x for |x| < 2^-27, with inexact exception if x is not 0.
    (assert-eql x (kernel:%asin x))
    (assert-eql (- x) (kernel:%asin (- x)))))

(define-test %asin-exception
    (:tag :fdlibm)
  (let ((x (scale-float 1d0 -28))
	(x0 0d0))
    ;; asin(x) = x for |x| < 2^-27, with inexact exception if x is not 0.
    (assert-eql x (kernel:%asin x))
    (assert-eql (- x) (kernel:%asin (- x)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%asin x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%asin x)))))

(define-test %cos.exceptions
    (:tag :fdlibm)
  ;; cos(inf) signals invalid operation
  (assert-error 'floating-point-invalid-operation
		(kernel:%cos ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%cos ext:double-float-negative-infinity))
  ;; cos(nan) is NaN
  (assert-true (ext:float-nan-p (kernel:%cos *qnan*)))
  
  ;; cos(x) = 1 for |x| < 2^-27.  Signal inexact unless x = 0
  (let ((x (scale-float 1d0 -28))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 1d0 (kernel:%cos x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%cos x)))))

(define-test %sin.exceptions
    (:tag :fdlibm)
  ;; sin(inf) signals invalid operation
  (assert-error 'floating-point-invalid-operation
		(kernel:%sin ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%sin ext:double-float-negative-infinity))
  ;; sin(nan) is NaN
  (assert-true (ext:float-nan-p (kernel:%sin *qnan*)))

  ;; sin(x) = x for |x| < 2^-27.  Signal inexact unless x = 0
  (let ((x (scale-float 1d0 -28))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%sin x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%sin x)))))

(define-test %tan.exceptions
    (:tag :fdlibm)
  ;; tan(inf) signals invalid operation
  (assert-error 'floating-point-invalid-operation
		(kernel:%tan ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%tan ext:double-float-negative-infinity))
  ;; tan(nan) is NaN
  (assert-true (ext:float-nan-p (kernel:%sin *qnan*)))

  ;; tan(x) = x for |x| < 2^-28.  Signal inexact unless x = 0
  (let ((x (scale-float 1d0 -29))
	(x0 0d0))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql 0d0 (kernel:%tan x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (kernel:%tan x)))))

;; Test cases from e_pow.c for fdlibm.
(define-test %pow.case.1
    (:tag :fdlibm)
  ;; anything ^ 0 is 1
  (assert-equal 1d0
		(kernel:%pow ext:double-float-positive-infinity 0d0))
  (assert-equal 1d0
		(kernel:%pow ext:double-float-negative-infinity 0d0)))

(define-test %pow.case.2
    (:tag :fdlibm)
  ;; anything ^ 1 is itself
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow ext:double-float-positive-infinity 1d0))
  (assert-equal ext:double-float-negative-infinity
		(kernel:%pow ext:double-float-negative-infinity 1d0)))

(define-test %pow.case.3
    (:tag :fdlibm)
  ;; anything ^ NaN is NaN
  (assert-true (ext:float-nan-p
		(kernel:%pow pi *qnan*)))
  (assert-true (ext:float-nan-p
		(kernel:%pow ext:double-float-positive-infinity *qnan*))))

(define-test %pow.case.4
    (:tag :fdlibm)
  ;; NaN ^ non-zero is NaN
  (assert-true (ext:float-nan-p
		(kernel:%pow *qnan* pi)))
  (assert-true (ext:float-nan-p
		(kernel:%pow *qnan* ext:double-float-positive-infinity))))

(define-test %pow.case.5
    (:tag :fdlibm)
  ;; (|x| > 1) ^ +inf is +inf
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow pi ext:double-float-positive-infinity))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow (- pi) ext:double-float-positive-infinity)))

(define-test %pow.case.6
    (:tag :fdlibm)
  ;; (|x| > 1) ^ -inf is +0
  (assert-equal +0d0
		(kernel:%pow pi ext:double-float-negative-infinity))
  (assert-equal +0d0
		(kernel:%pow (- pi) ext:double-float-negative-infinity)))

(define-test %pow.case.7
    (:tag :fdlibm)
  ;; (|x| < 1) ^ +inf is +0
  (assert-equal +0d0
		(kernel:%pow 0.5d0 ext:double-float-positive-infinity))
  (assert-equal +0d0
		(kernel:%pow -0.5d0 ext:double-float-positive-infinity)))

(define-test %pow.case.8
    (:tag :fdlibm)
  ;; (|x| < 1) ^ -inf is +inf
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow 0.5d0 ext:double-float-negative-infinity))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow -0.5d0 ext:double-float-negative-infinity)))

(define-test %pow.case.9
    (:tag :fdlibm)
  ;; std::pow says 1^exp is 1 for any exp, including NaN.  (-1)^(+/-inf)
  ;; is 1.  No errors signaled.
  #+core-math
  (progn
    (assert-equal 1d0
		  (kernel:%pow 1d0 ext:double-float-positive-infinity))
    (assert-equal 1d0
		  (kernel:%pow 1d0 ext:double-float-negative-infinity))
    (assert-equal 1d0
		  (kernel:%pow 1d0 *qnan*))
    (assert-equal 1d0
		  (kernel:%pow -1d0 ext:double-float-positive-infinity))
    (assert-equal 1d0
		  (kernel:%pow -1d0 ext:double-float-negative-infinity)))
  #-core-math
  ;; +-1 ^ +-inf is NaN.
  ;;
  ;; But the implementation signals invalid operation, so we need to
  ;; check for that.
  ;;
  (progn
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow 1d0 ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow 1d0 ext:double-float-negative-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow -1d0 ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow -1d0 ext:double-float-negative-infinity))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p
		  (kernel:%pow 1d0 ext:double-float-positive-infinity)))
    (assert-true (ext:float-nan-p
		  (kernel:%pow 1d0 ext:double-float-negative-infinity)))
    (assert-true (ext:float-nan-p
		  (kernel:%pow -1d0 ext:double-float-positive-infinity)))
    (assert-true (ext:float-nan-p
		  (kernel:%pow -1d0 ext:double-float-negative-infinity))))))

(define-test %pow.case.10
    (:tag :fdlibm)
  ;; +0 ^ (+anything except 0, Nan) is +0
  (assert-equal +0d0
		(kernel:%pow +0d0 10d0))
  (assert-equal +0d0
		(kernel:%pow +0d0 ext:double-float-positive-infinity)))

(define-test %pow.case.11
    (:tag :fdlibm)
  ;; +0 ^ (+anything except 0, Nan, odd integer) is +0
  (assert-equal +0d0
		(kernel:%pow -0d0 10d0))
  (assert-equal +0d0
		(kernel:%pow -0d0 ext:double-float-positive-infinity)))

(define-test %pow.case.12
    (:tag :fdlibm)
  ;; +0 ^ (-anything except 0, Nan) is +inf
  ;;
  ;; But fdlibm signals error for (+0)^(-10) instead of returning inf.  Check this.
  (assert-error 'division-by-zero
		(kernel:%pow +0d0 -10d0))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%pow +0d0 -10d0)))
  ;; No signals here.
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow +0d0 ext:double-float-negative-infinity)))

(define-test %pow.case.13
    (:tag :fdlibm)
  ;; -0 ^ (-anything except 0, Nan, odd integer) is +inf
  ;;
  ;; But (-0)^(-10) signals division by zero
  (assert-error 'division-by-zero
		(kernel:%pow -0d0 -10d0))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%pow -0d0 -10d0)))
  ;; But no error here.
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow +0d0 ext:double-float-negative-infinity)))

(define-test %pow.case.14
    (:tag :fdlibm)
  ;; -0 ^ (odd integer) = -( +0 ^ (odd integer))
  (assert-equal (- (kernel:%pow +0d0 5d0))
		(kernel:%pow -0d0 5d0)))

(define-test %pow.case.15
    (:tag :fdlibm)
  ;; +inf ^ (+anything except 0, NaN) is +inf
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow ext:double-float-positive-infinity pi)))

(define-test %pow.case.16
    (:tag :fdlibm)
  ;; +inf ^ (-anything except 0, NaN) is +0
  (assert-equal +0d0
		(kernel:%pow ext:double-float-positive-infinity (- pi))))

(define-test %pow.case.17
    (:tag :fdlibm)
  ;; -inf ^ (anything) = -0 ^ (-anything)
  (assert-equal (ext:with-float-traps-masked (:divide-by-zero)
		  ;; This produces a divide-by-zero error so mask it
		  ;; to get a value.
		  (kernel:%pow -0d0 (- pi)))
		(kernel:%pow ext:double-float-negative-infinity pi))
  (assert-equal (kernel:%pow -0d0 pi)
		(kernel:%pow ext:double-float-negative-infinity (- pi))))

(define-test %pow.case.18
    (:tag :fdlibm)
  ;; (-anything) ^ integer is (-1)^integer * (+anything ^ integer)
  (dolist (base '(-2d0 -10d0))
    (dolist (power '(5 -5))
      (assert-equal (* (expt -1 power)
		       (kernel:%pow (- base) (coerce power 'double-float)))
		    (kernel:%pow base (coerce power 'double-float))
		    base power))))

(define-test %pow.case.19
    (:tag :fdlibm)
  ;; (-anything except 0 and inf) ^ non-integer is NaN
  ;;
  ;; But this signals invalid, so check for that too.
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow -2d0 1.5d0))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p
		  (kernel:%pow -2d0 1.5d0)))))
