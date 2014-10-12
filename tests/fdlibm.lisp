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
