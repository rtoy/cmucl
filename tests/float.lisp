;; Tests of float functions

(defpackage :float-tests
  (:use :cl :lisp-unit))

(in-package "FLOAT-TESTS")

(define-test decode-float
  (assert-true (funcall (compile nil #'(lambda (x)
					 (declare (type (double-float (0d0)) x))
					 (decode-float x)))
			1d0)))

(define-test log2.interp
  (loop for k from -1074 to 1023 do
    (let ((x (scale-float 1d0 k)))
      (assert-equalp k (log x 2)))))

(define-test log10.interp
  (loop for k from 0 to 22 do
    (let ((x (float (expt 10 k) 1d0)))
      (assert-equalp k (log x 10)))))

(compile 'clog2
	 #'(lambda (x)
	     (declare (type (double-float (0d0)) x))
	     (log x 2)))

(compile 'clog10
	 #'(lambda (x)
	     (declare (type (double-float (0d0)) x))
	     (log x 10)))

(define-test log2.compiled
  (loop for k from -1074 to 1023 do
    (let ((x (scale-float 1d0 k)))
      (assert-equalp k (clog2 x)))))

(define-test log10.compiled
  (loop for k from 0 to 22 do
    (let ((x (float (expt 10 k) 1d0)))
      (assert-equalp k (clog10 x)))))


(define-test integer-decode-float.double
  ;; Generate 100 random denormal values and compare what
  ;; integer-decode-float returns against what it should return.
  (dotimes (k 100)
    (let ((x (random least-positive-normalized-double-float)))
      (multiple-value-bind (hi lo)
	  (kernel:double-float-bits x)
	;; Verify that the exponent is 0, which it must be for a
	;; denormal number.
	(assert-equal 0
		      (ldb vm:double-float-exponent-byte hi)
		      x)
	;; Print out the fraction bits, and the bits returned by
	;; INTEGER-DECODE-FLOAT. We could do this differently, but
	;; this has the nice side effect of making it easy to see what
	;; is expected and what went wrong.
	(let* ((expected (format nil "~b~32,'0b" hi lo))
	       (actual (format nil "~b" (integer-decode-float x)))
	       (tail (subseq actual (length expected))))
	  ;; If everything is working correctly, the beginning of the
	  ;; actual bits must exactly match the expected bits.
	  (assert-true (every #'char=
			      expected
			      actual)
			x
			expected
			actual)
	  ;; And finally, the trailing part of the actual bits must be
	  ;; all zeroes, but this is a denormal number.
	  (assert-true (every #'(lambda (c)
				  (char= c #\0))
			      tail)
		       x
		       tail))))))

(define-test scale-float.double
  ;; As a side-effect of fixing INTEGER-DECODE-FLOAT, SCALE-FLOAT
  ;; should return the correct values now when scaling
  ;; denormals. Check a few denormal values.
  (dotimes (k 100)
    (let* ((x (random least-positive-normalized-double-float))
	   (scaled (scale-float x 54))
	   (mult (* x (scale-float 1d0 54))))
      ;; The result of SCALE-FLOAT and the multiplication should be
      ;; exactly equal because the multiplication by 2^54 is exactly
      ;; representable.
      (assert-equal scaled
		    mult
		    x
		    scaled
		    mult)))
  ;; Add the test caused the investigation of SCALE-FLOAT
  (let* ((x 1d-310)
	 (scaled (scale-float x 54))
	 (mult (* x (scale-float 1d0 54))))
    (assert-equal scaled
		    mult
		    x
		    scaled
		    mult)))

(define-test decode-float.double
  ;; As a side-effect of fixing INTEGER-DECODE-FLOAT, DECODE-FLOAT
  ;; should return the correct values now. We just spot check one
  ;; value here.
  (dotimes (k 100)
    (let ((x (random least-positive-normalized-double-float)))
      (multiple-value-bind (f e)
	  (decode-float x)
	(assert-equal x
		      (scale-float f e)
		      f
		      e)))))

