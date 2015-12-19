;; Test that fdlibm routines signals exceptions as expected.

(defpackage :fdlibm-tests
  (:use :cl :lisp-unit))

(in-package "FDLIBM-TESTS")

(defparameter *qnan*
  (kernel::with-float-traps-masked (:invalid)
    (* 0 ext:double-float-positive-infinity))
  "Some randon quiet MaN value")

(defparameter *snan*
  (kernel:make-double-float #x7ff00000 1)
  "A randon signaling MaN value")

(define-test %cosh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%cosh 1000d0))
  (assert-error 'floating-point-overflow
		(kernel:%cosh -1000d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%cosh *snan*))
  (assert-true (ext:float-nan-p (kernel:%cosh *qnan*)))
  
  ;; Same, but with overflow's masked
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh 1000d0))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh -1000d0))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh ext:double-float-positive-infinity))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%cosh ext:double-float-negative-infinity)))
  ;; Test NaN
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%cosh *snan*)))))

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
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%sinh 1000d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%sinh -1000d0))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%sinh ext:double-float-positive-infinity))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%sinh ext:double-float-negative-infinity)))
  ;; Test NaN
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%sinh *qnan*)))))


(define-test %tanh.exceptions
  (:tag :fdlibm)
  (assert-true (ext:float-nan-p (kernel:%tanh *qnan*)))
  (assert-error 'floating-point-invalid-operation
		(kernel:%tanh *snan*))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%tanh *snan*)))))

(define-test %acosh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(kernel:%acosh ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%acosh 0d0))
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%acosh ext:double-float-positive-infinity)))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%acosh 0d0)))))

(define-test %asinh.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%asinh *snan*))
  (assert-error 'floating-point-overflow
		(kernel:%asinh ext:double-float-positive-infinity))
  (assert-error 'floating-point-overflow
		(kernel:%asinh ext:double-float-negative-infinity))
  (assert-true (ext:float-nan-p (kernel:%asinh *qnan*)))
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%asinh ext:double-float-positive-infinity))
    (assert-error ext:double-float-negative-infinity
		  (kernel:%asinh ext:double-float-negative-infinity)))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%asinh *snan*)))))

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
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%atanh 2d0)))
    (assert-true (ext:float-nan-p (kernel:%atanh -2d0))))
  (kernel::with-float-traps-masked (:divide-by-zero)
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
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		 (kernel:%expm1 709.8d0))
    )
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext::float-nan-p (kernel:%expm1 *snan*)))))

(define-test %log1p.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%log1p -2d0))
  (assert-error 'floating-point-overflow
		(kernel:%log1p -1d0))
  (assert-true (ext:float-nan-p (kernel:%log1p *qnan*)))
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log1p -1d0)))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%log1p *snan*)))))

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
  (kernel::with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%exp 710d0)))
  (let ((modes (ext:get-floating-point-modes)))
    (unwind-protect
	 (progn
	   (ext:set-floating-point-modes :traps '(:underflow))
	   (assert-error 'floating-point-underflow
			 (kernel:%exp -1000d0)))
      (apply #'ext:set-floating-point-modes modes))))

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
  (kernel::with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log 0d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log -0d0)))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%log -1d0)))
    (assert-true (ext:float-nan-p (kernel:%log *snan*)))))

(define-test %acos.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%acos 2d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%acos -2d0))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%acos 2d0)))
    (assert-true (ext:float-nan-p (kernel:%acos -2d0)))))

(define-test %asin.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%asin 2d0))
  (assert-error 'floating-point-invalid-operation
		(kernel:%asin -2d0))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%asin 2d0)))
    (assert-true (ext:float-nan-p (kernel:%asin -2d0)))))

(define-test %atan.exceptions
  (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(kernel:%atan *snan*))
  (assert-true (ext:float-nan-p (kernel:%atan *qnan*)))
  (kernel::with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (kernel:%atan *snan*)))))

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
  (kernel::with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log10 0d0))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%log10 -0d0)))
  (kernel::with-float-traps-masked (:invalid)
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
  (kernel::with-float-traps-masked (:overflow)
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
  (assert-eql 2.0634370688955608d0 (acosh 4d0))
  ;; acosh(2^50), case 2^28 < x
  (assert-eql 35.35050620855721d0 (acosh (scale-float 1d0 50)))
  ;; No overflow for most positive
  (assert-eql 710.4758600739439d0 (acosh most-positive-double-float)))

(define-test asinh-basic-tests
    (:tag :fdlibm)
  (assert-eql -0d0 (asinh -0d0))
  (assert-eql 0d0 (asinh 0d0))
  (let ((x (scale-float 1d0 -29)))
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
    (assert-eql 710.4758600739439d0 (asinh x))
    (assert-eql -710.4758600739439d0 (asinh (- x)))))
  
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
