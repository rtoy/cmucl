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

;; Rudimentary code to read C %a formatted numbers that look like
;; "-0x1.c4dba4ba1ee79p-620".  We assume STRING is exactly in this
;; format.  No error-checking is done.
(defun parse-%a (string)
  (let* ((sign (if (char= (aref string 0) #\-)
		   -1
		   1))
	 (dot-posn (position #\. string))
	 (p-posn (position #\p string))
	 (lead (parse-integer string :start (1- dot-posn) :end dot-posn))
	 (frac (parse-integer string :start (1+ dot-posn) :end p-posn :radix 16))
	 (exp (parse-integer string :start (1+ p-posn))))
    (* sign
       (scale-float (float (+ (ash lead 52)
			      frac)
			   1d0)
		    (- exp 52)))))

;; Tests for complex division.  Tests 1-10 are from Baudin and Smith.
;; Test 11 is an example from Maxima.  Test 12 is an example from the
;; ansi-tests.  Tests 13-16 are for examples for improvement
;; iterations 1-4 from McGehearty.
;;
;; Each test is a list of values: x, y, z-true (the value of x/y), and
;; the bits of accuracy.
(defparameter *test-cases*
  (list
   ;; 1
   (list (complex 1d0 1d0)
	 (complex 1d0 (scale-float 1d0 1023))
	 (complex (scale-float 1d0 -1023)
		  (scale-float -1d0 -1023))
	 53)
   ;; 2
   (list (complex 1d0 1d0)
	 (complex (scale-float 1d0 -1023) (scale-float 1d0 -1023))
	 (complex (scale-float 1d0 1023) 0)
	 53)
   ;; 3
   (list (complex (scale-float 1d0 1023) (scale-float 1d0 -1023))
	 (complex (scale-float 1d0 677) (scale-float 1d0 -677))
	 (complex (scale-float 1d0 346) (scale-float -1d0 -1008))
	 53)
   ;; 4
   (list (complex (scale-float 1d0 1023) (scale-float 1d0 1023))
	 (complex 1d0 1d0)
	 (complex (scale-float 1d0 1023) 0)
	 53)
   ;; 5
   (list (complex (scale-float 1d0 1020) (scale-float 1d0 -844))
	 (complex (scale-float 1d0 656) (scale-float 1d0 -780))
	 (complex (scale-float 1d0 364) (scale-float -1d0 -1072))
	 53)
   ;; 6
   (list (complex (scale-float 1d0 -71) (scale-float 1d0 1021))
	 (complex (scale-float 1d0 1001) (scale-float 1d0 -323))
	 (complex (scale-float 1d0 -1072) (scale-float 1d0 20))
	 53)
   ;; 7
   (list (complex (scale-float 1d0 -347) (scale-float 1d0 -54))
	 (complex (scale-float 1d0 -1037) (scale-float 1d0 -1058))
	 (complex 3.898125604559113300d289 8.174961907852353577d295)
	 53)
   ;; 8
   (list (complex (scale-float 1d0 -1074) (scale-float 1d0 -1074))
	 (complex (scale-float 1d0 -1073) (scale-float 1d0 -1074))
	 (complex 0.6d0 0.2d0)
	 53)
   ;; 9
   (list (complex (scale-float 1d0 1015) (scale-float 1d0 -989))
	 (complex (scale-float 1d0 1023) (scale-float 1d0 1023))
	 (complex 0.001953125d0 -0.001953125d0)
	 53)
   ;; 10
   (list (complex (scale-float 1d0 -622) (scale-float 1d0 -1071))
	 (complex (scale-float 1d0 -343) (scale-float 1d0 -798))
	 (complex 1.02951151789360578d-84 6.97145987515076231d-220)
	 53)
   ;; 11
   ;; From Maxima
   (list #c(5.43d-10 1.13d-100)
	 #c(1.2d-311 5.7d-312)
	 #c(3.691993880674614517999740937026568563794896024143749539711267954d301
	    -1.753697093319947872394996242210428954266103103602859195409591583d301)
	 52)
   ;; 12
   ;; Found by ansi tests. z/z should be exactly 1.
   (list #c(1.565640716292489d19 0.0d0)
	 #c(1.565640716292489d19 0.0d0)
	 #c(1d0 0)
	 53)
   ;; 13
   ;; Iteration 1.  Without this, we would instead return
   ;;
   ;;   (complex (parse-%a "0x1.ba8df8075bceep+155") (parse-%a "-0x1.a4ad6329485f0p-895"))
   ;;
   ;; whose imaginary part is quite a bit off.
   (list (complex (parse-%a "0x1.73a3dac1d2f1fp+509") (parse-%a "-0x1.c4dba4ba1ee79p-620"))
	 (complex (parse-%a "0x1.adf526c249cf0p+353") (parse-%a "0x1.98b3fbc1677bbp-697"))
	 (complex (parse-%a "0x1.BA8DF8075BCEEp+155") (parse-%a "-0x1.A4AD628DA5B74p-895"))
	 53)
   ;; 14
   ;; Iteration 2.
   (list (complex (parse-%a "-0x0.000000008e4f8p-1022") (parse-%a "0x0.0000060366ba7p-1022"))
	 (complex (parse-%a "-0x1.605b467369526p-245") (parse-%a "0x1.417bd33105808p-256"))
	 (complex (parse-%a "0x1.cde593daa4ffep-810") (parse-%a "-0x1.179b9a63df6d3p-799"))
	 52)
   ;; 15
   ;; Iteration 3
   (list (complex (parse-%a "0x1.cb27eece7c585p-355 ") (parse-%a "0x0.000000223b8a8p-1022"))
	 (complex (parse-%a "-0x1.74e7ed2b9189fp-22") (parse-%a "0x1.3d80439e9a119p-731"))
	 (complex (parse-%a "-0x1.3b35ed806ae5ap-333") (parse-%a "-0x0.05e01bcbfd9f6p-1022"))
	 53)
   ;; 16
   ;; Iteration 4
   (list (complex (parse-%a "-0x1.f5c75c69829f0p-530") (parse-%a "-0x1.e73b1fde6b909p+316"))
	 (complex (parse-%a "-0x1.ff96c3957742bp+1023") (parse-%a "0x1.5bd78c9335899p+1021"))
	 (complex (parse-%a "-0x1.423c6ce00c73bp-710") (parse-%a "0x1.d9edcf45bcb0ep-708"))
	 52)
   ))

;; Relative error in terms of bits of accuracy.  This is the
;; definition used by Baudin and Smith.  A result of 53 means the two
;; numbers have identical bits.  For complex numbers, we use the min
;; of the bits of accuracy of the real and imaginary parts.
(defun rel-err (computed expected)
  (flet ((rerr (c e)
	   (let ((diff (abs (- c e))))
	     (if (zerop diff)
		 (float-digits diff)
		 (floor (- (log (/ diff (abs e)) 2d0)))))))
    (min (rerr (realpart computed) (realpart expected))
	 (rerr (imagpart computed) (imagpart expected)))))

(define-test complex-division.double
  (:tag :issues)
  (loop for k from 1
	for test in *test-cases*
	do
	   (destructuring-bind (x y z-true expected-rel)
	       test
	     (let* ((z (/ x y))
		    (rel (rel-err z z-true)))
	       (assert-equal expected-rel
			     rel
			     k x y z z-true diff rel)))))

(define-test complex-division.misc
    (:tag :issue)
  (let ((num '(1
	       1/2
	       1.0
	       1d0
	       #c(1 2)
	       #c(1.0 2.0)
	       #c(1d0 2d0)
	       #c(1w0 2w0))))
    ;; Try all combinations of divisions of different types.  This is
    ;; primarily to test that we got all the numeric contagion cases
    ;; for division in CL:/.
    (dolist (x num)
      (dolist (y num)
	(assert-true (/ x y)
		     x y)))))

(define-test complex-division.single
    (:tag :issues)
  (let* ((x #c(1 2))
	 (y (complex (expt 2 127) (expt 2 127)))
	 (expected (coerce (/ x y)
			   '(complex single-float))))
    ;; A naive implementation of complex division would cause an
    ;; overflow in computing the denominator.
    (assert-equal expected
		  (/ (coerce x '(complex single-float))
		     (coerce y '(complex single-float)))
		  x
		  y)))
