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

(define-test float-traps-masked
  ;; inf-inf signals invalid, which is masked so the result is NaN.
  (assert-true
   (ext:float-nan-p
    (ext:with-float-traps-masked (:invalid)
      (- ext:double-float-positive-infinity
	 ext:double-float-positive-infinity))))

  ;; Divide-by-zero is masked so dividing by zero returns infinity
  (assert-true
   (ext:float-infinity-p
    (ext:with-float-traps-masked (:divide-by-zero)
      (/ 100d0 0d0))))

  ;; Overflow is masked so 100 * most-positive-double returns infinity
  (assert-true
   (ext:float-infinity-p
    (ext:with-float-traps-masked (:overflow)
      (* 100 most-negative-double-float)))))

(define-test float-ratio.single
    (:tag :issues)
  ;; least-positive-single-float is 1.4012985e-45.  Let's test with
  ;; some rationals from 7/10*10^-45 to 1.41*10^-45 to make sure they
  ;; return 0 or least-positive-single-float
  (let ((expo (expt 10 -45)))
    ;; Need to make sure underflows are masked.
    (kernel::with-float-traps-masked (:underflow)
      ;; 7/10*10^-45 is just under halfway between 0 and least-positive,
      ;; so the answer is 0.
      (assert-equal 0f0 (kernel::float-ratio-float (* 7/10 expo) 'single-float))

      ;; These are all more than half way to
      ;; least-positive-single-float, so they should return that.
      (assert-equal least-positive-single-float
                    (kernel::float-ratio-float (* 8/10 expo) 'single-float))
      (assert-equal least-positive-single-float
                    (kernel::float-ratio-float (* 1 expo) 'single-float))
      (assert-equal least-positive-single-float
                    (kernel::float-ratio-float (* 14/10 expo) 'single-float))
      (assert-equal least-positive-single-float
                    (kernel::float-ratio-float (* 2 expo) 'single-float)))))

(define-test float-ratio.double
    (:tag :issues)
  ;; least-positive-double-float is 4.9406564584124654d-324.  Let's
  ;; test with some rationals from about 2*10^-324 to 4.94*10^-324 to make
  ;; sure they return 0 or least-positive-double-float
  (let ((expo (expt 10 -324)))
    ;; Need to make sure underflows are masked.
    (kernel::with-float-traps-masked (:underflow)
      ;; 247/100*10^-324 is just under halfway between 0 and least-positive,
      ;; so the answer is 0.
      (assert-equal 0d0 (kernel::float-ratio-float (* 247/100 expo) 'double-float))

      ;; These are all more than half way to
      ;; least-positive-double-float, so they should return that.
      (assert-equal least-positive-double-float
                    (kernel::float-ratio-float (* 248/100 expo) 'double-float))
      (assert-equal least-positive-double-float
                    (kernel::float-ratio-float (* 4 expo) 'double-float))
      (assert-equal least-positive-double-float
                    (kernel::float-ratio-float (* 494/100 expo) 'double-float))
      (assert-equal least-positive-double-float
                    (kernel::float-ratio-float (* 988/100 expo) 'double-float)))))
    
(define-test reader-error.small-single-floats
    (:tag :issues)
  ;; Test a number less than half of least-positive-single-float,
  ;; something a bit smaller, hen then something really small that
  ;; used to appear to hang cmucl because it was trying to compute the
  ;; a rational with a huge number of digits.
  (dolist (num '("1e-46" "1e-80" "1e-999999999"))
    (assert-error 'reader-error (read-from-string num)
                  num)))

(define-test reader-error.small-double-floats
    (:tag :issues)
  ;; Like reader-error.small-single-floats but for doubles
  (dolist (num '("1d-324" "1d-600" "1d-999999999"))
    (assert-error 'reader-error (read-from-string num)
                  num)))

(define-test reader-error.big-single-floats
    (:tag :issues)
  ;; Signal error for a number just a bit larger than
  ;; most-positive-single-float.  And a really big single-float.
  (assert-error 'reader-error (read-from-string "3.5e38"))
  (assert-error 'reader-error (read-from-string "1e999999999")))

(define-test reader-error.big-double-floats
    (:tag :issues)
  ;; Signal error for a number just a bit larger than
  ;; most-positive-double-float.  And a really big single-float.
  (assert-error 'reader-error (read-from-string "1.8d308"))
  (assert-error 'reader-error (read-from-string "1d999999999")))

(defun rounding-test (x)
  (declare (double-float x)
           (optimize (speed 3)))
  (* x (/ 1d0 x)))

(define-test rounding-mode.nearest
    (:tag :issues)
  (ext:with-float-rounding-mode (:nearest)
    (assert-equal 1d0 (rounding-test 3d0))))

(define-test rounding-mode.zero.1
    (:tag :issues)
  (ext:with-float-rounding-mode (:zero)
    (assert-equal 0.9999999999999999d0
                  (rounding-test 3d0))))

(define-test rounding-mode.zero.2
    (:tag :issues)
  (ext:with-float-rounding-mode (:zero)
    (assert-equal 0.9999999999999999d0
                  (rounding-test -3d0))))

(define-test rounding-mode.positive-infinity
    (:tag :issues)
  (ext:with-float-rounding-mode (:positive-infinity)
    (assert-equal 1.0000000000000002d0
                  (rounding-test 3d0))))

(define-test rounding-mode.negative-infinity
    (:tag :issues)
  (ext:with-float-rounding-mode (:negative-infinity)
    (assert-equal 0.9999999999999999d0
                  (rounding-test 3d0))))

(define-test reader.underflow-enabled
    (:tag :issues)
  ;; Test with FP underflow enabled, we can still read denormals
  ;; without problem.  For this test we only care that we get a
  ;; number, not the actual value.
  (dolist (n (list least-positive-single-float
                   least-positive-normalized-single-float
                   (/ (+ least-positive-single-float
                         least-positive-normalized-single-float)
                      2)
                   least-positive-double-float
                   least-positive-normalized-double-float
                   (/ (+ least-positive-double-float
                         least-positive-normalized-double-float)
                      2)
                   ))
    (assert-true (floatp
                  (ext:with-float-traps-enabled (:underflow)
                    (read-from-string (format nil "~A" n)))))))

(define-test reader-restarts.underflow
    (:tag :issues)
  ;; Test that we get a restart when reading floating-point numbers
  ;; that are too small to fit in a float.  Invoke the restart to
  ;; return 0.  All the numbers must be less than half the
  ;; leasst-positive float.
  (dolist (item '(("1e-46" 0f0)
                  ("1e-999" 0f0)
                  ("1d-324" 0d0)
                  ("1d-999" 0d0)))
    (destructuring-bind (string expected-value)
        item
      (assert-equal expected-value
                    (values (handler-bind
                                ((reader-error
                                   (lambda (c)
                                     (declare (ignore c))
                                     (invoke-restart 'lisp::continue))))
                              (read-from-string string)))))))
   
(define-test fp-overflow-restarts.infinity
    (:tag :issues)
  ;; Test that the "infinity" restart from reader on floating-point
  ;; overflow returns an infinity of the correct type and sign.
  (dolist (item (list (list "4e38" ext:single-float-positive-infinity)
                      (list "-4e38" ext:single-float-negative-infinity)
                      (list "2d308" ext:double-float-positive-infinity)
                      (list "-2d308" ext:double-float-negative-infinity)
                      ;; These test the short-cut case in the reader for
                      ;; very large numbers.
                      (list "4e999" ext:single-float-positive-infinity)
                      (list "-4e999" ext:single-float-negative-infinity)
                      (list "1d999" ext:double-float-positive-infinity)
                      (list "-1d999" ext:double-float-negative-infinity)))
    (destructuring-bind (string expected-result)
        item
      (assert-equal expected-result
                    (values (handler-bind ((reader-error
                                             (lambda (c)
                                               (declare (ignore c))
                                               (invoke-restart 'lisp::infinity))))
                              (read-from-string string)))))))

(define-test fp-overflow-restarts.huge
    (:tag :issues)
  ;; Test that the "largest-float" restart from reader on
  ;; floating-point overflow returns the largest float of the correct
  ;; type and sign.
  (dolist (item (list (list "4e38" most-positive-single-float)
                      (list "-4e38" most-negative-single-float)
                      (list "2d308" most-positive-double-float)
                      (list "-2d308" most-negative-double-float)
                      ;; These test the short-cut case in the reader for
                      ;; very large numbers.
                      (list "4e999" most-positive-single-float)
                      (list "-4e999" most-negative-single-float)
                      (list "1d999" most-positive-double-float)
                      (list "-1d999" most-negative-double-float)))
    (destructuring-bind (string expected-result)
        item
      (assert-equal expected-result
                    (handler-bind ((reader-error
                                     (lambda (c)
                                       (declare (ignore c))
                                       (values (invoke-restart 'lisp::largest-float)))))
                      (read-from-string string))))))

#+x86
(define-test x87-set-floating-point-modes
    (:tag :issues)
  (let ((new-mode (setf (x86::x87-floating-point-modes)
			(dpb 2 (byte 2 24)
			     (x86::x87-floating-point-modes)))))
    (assert-true (typep new-mode 'x86::float-modes))
    (assert-equal new-mode (setf (x86::x87-floating-point-modes) new-mode))))



;; Issue #458
(define-test dd-mult-overflow
  (:tag :issues)
  (assert-equal -2w300
		(* -2w300 1w0)))
