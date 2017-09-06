;;; Tests for the basic trig, hyperbolic, exponential and log
;;; functions.

(defpackage :trig-tests
  (:use :cl :lisp-unit))

(in-package "TRIG-TESTS")

(define-test sin.signed-zeroes
  "Test sin for 0d0 and -0d0"
  (:tag :sin :signed-zeroes)
  (assert-eql 0d0 (sin 0d0))
  (assert-eql -0d0 (sin -0d0)))


(define-test sin.very-small
  "Tests sin for the case of |x| < 2^-27, but not 0."
  (:tag :sin)
  (assert-eql (scale-float 1d0 -28)
	      (sin (scale-float 1d0 -28))))

(define-test sin.no-reduction
  "Test sin for small args without reduction"
  (:tag :sin)
  (assert-eql 0.479425538604203d0
	      (sin .5d0))
  (assert-eql -0.479425538604203d0
	      (sin -0.5d0)))

(define-test sin.pi/2
  "Test for arg near pi/2"
  (:tag :sin)
  (assert-eql 1d0 (sin (/ pi 2))))

(define-test sin.arg-reduction
  "Test for sin with arg reduction"
  (:tag :sin)
  ;; Test for argument reduction with n mod 4 = 0
  (assert-eql -7.07106781186547675943154203316156531867416581156d-1
	      (sin (* 7/4 pi)))
    ;; Test for argument reduction with n mod 4 = 1
  (assert-eql 7.07106781186547329560731709118834541043171055432d-1
	      (sin (* 9/4 pi)))
  ;; Test for argument reduction with n mod 4 = 2
  (assert-eql 7.07106781186548390575743300374993861263439430213d-1
	      (sin (* 11/4 pi)))
  ;; Test for argument reduction with n mod 4 = 3
  (assert-eql -7.07106781186547871002109559079472349116005337743d-1
	      (sin (* 13/4 pi)))
  ;; Test for argument reduction, big value
  (assert-eql 0.377820109360752d0
	      (sin (scale-float 1d0 120))))

(define-test sin.exceptions
  "Test sin for exceptional values"
  (:tag :sin :exceptions)
  (kernel::with-float-traps-masked ()
    (assert-error 'floating-point-invalid-operation
		  (sin ext:double-float-positive-infinity))
    (assert-error 'floating-point-invalid-operation
		  (sin ext:double-float-negative-infinity))))

(define-test cos.signed-zeroes
  "Test cos for 0d0 and -0d0"
  (:tag :cos :signed-zeroes)
  (assert-eql 1d0 (cos 0d0))
  (assert-eql 1d0 (cos -0d0)))

(define-test cos.very-small
  "Test cos for |x| < 2^-27"
  (:tag :cos)
  (assert-eql 1d0 (cos (scale-float 1d0 -28))))

(define-test cos.code-paths
  "Tests various code paths in cos evaluation"
  (:tag :cos)
  ;; Test for branch |x| < .3
  (assert-eql 0.9689124217106447d0
	      (cos 0.25d0))
  ;; Test for branch |x| > .3 and \x| < .78125
  (assert-eql 8.7758256189037271611628158260382965199164519711d-1
	      (cos 0.5d0))
  ;; Test for branch |x| > .3 and |x| > .78125
  (assert-eql 0.7073882691671998d0
	      (cos 0.785d0)))

(define-test cos.pi/2  
  "Test cos(pi/2)"
  (:tag :cos)
  (assert-eql 6.123233995736766d-17
	      (cos (/ pi 2))))

(define-test cos.arg-reduction
  "Test for cos with arg reduction"
  (:tag :cos)
    ;; Test for argument reduction with n mod 4 = 0
  (assert-eql 7.07106781186547372858534520893509069186435867941d-1
	      (cos (* 7/4 pi)))
  ;; Test for argument reduction with n mod 4 = 1
  (assert-eql 7.0710678118654771924095701509080985020443197242d-1
	      (cos (* 9/4 pi)))
  ;; Test for argument reduction with n mod 4 = 2
  (assert-eql -7.07106781186546658225945423833643190916000739026d-1
	      (cos (* 11/4 pi)))
  ;; Test for argument reduction with n mod 4 = 3
  (assert-eql -7.07106781186547177799579165130055836531929091466d-1
	      (cos (* 13/4 pi)))
  ;; Test for argument reduction
  (assert-eql -0.9258790228548379d0
	      (cos (scale-float 1d0 120))))

(define-test cos.exceptions
  "Test cos for exceptional values"
  (:tag :sin :exceptions)
  (kernel::with-float-traps-masked ()
    (assert-error 'floating-point-invalid-operation
		  (cos ext:double-float-positive-infinity))
    (assert-error 'floating-point-invalid-operation
		  (cos ext:double-float-negative-infinity))))

(define-test tan.signed-zeroes
  "Test tan for 0d0 and -0d0"
  (:tag :tan :signed-zeroes)
  (assert-eql 0d0 (tan 0d0))
  (assert-eql -0d0 (tan -0d0)))

(define-test tan.very-small
  "Test for tan, |x| < 2^-28"
  (:tag :tan)
  (assert-eql (scale-float 1d0 -29)
	      (tan (scale-float 1d0 -29)))
  (assert-eql (scale-float -1d0 -29)
	      (tan (scale-float -1d0 -29))))

(define-test tan.pi/2
  "Test for tan(pi/2)"
  (:tag :tan)
  (assert-eql 1.63312393531953697559677370415289165308640681049d16
	      (tan (/ pi 2))))

(define-test tan.code-paths
  "Tests for various code paths in tan"
  (:tag :tan)
  ;; |x| < .6744
  (assert-eql 5.4630248984379051325517946578028538329755172018d-1
	      (tan 0.5d0))
  ;; |x = 11/16 = 0.6875 > .6744
  (assert-eql 8.21141801589894121911423965374711700875371645309d-1
	      (tan (float 11/16 1d0)))
  ;; This was found by maxima's testsuite.  A bug in kernel-tan when
  ;; returning cot(x).
  (assert-eql 2.0000000000000028604455051971538975562294147582d0
	      (tan 1.107148717794091d0)))

(define-test tan.arg-reduction
  "Test for tan with arg reduction"
  (:tag :tan)
  ;; Test for argument reduction with n even
  (assert-eql -1.00000000000000042862637970157370388940976433505d0
	      (tan (* 7/4 pi)))
  ;; Test for argument reduction with n odd
  (assert-eql 9.99999999999999448908940383691222098948324989275d-1
	      (tan (* 9/4 pi)))
  (assert-eql -4.08066388841804238545143494525595117765084022768d-1
	      (tan (scale-float 1d0 120))))

(define-test tan.exceptions
  "Test tan for exceptional values"
  (:tag :sin :exceptions)
  (kernel::with-float-traps-masked ()
    (assert-error 'floating-point-invalid-operation
		  (tan ext:double-float-positive-infinity))
    (assert-error 'floating-point-invalid-operation
		  (tan ext:double-float-negative-infinity))))

(define-test sincos.signed-zeroes
  "Test sincos at 0d0, -0d0"
  (:tag :sincos :signed-zeroes)
  (assert-equal '(0d0 1d0)
		(multiple-value-list (kernel::%sincos 0d0)))
  (assert-equal '(-0d0 1d0)
		(multiple-value-list (kernel::%sincos -0d0))))

;; Test sincos at a bunch of random points and compare the result from
;; sin and cos.  If they differ, save the result in a list to be
;; returned.
(defun sincos-test (limit n)
  (let (results)
    (dotimes (k n)
      (let* ((x (random limit))
	     (s-exp (sin x))
	     (c-exp (cos x)))
	(multiple-value-bind (s c)
	    (kernel::%sincos x)
	  (unless (and (eql s s-exp)
		       (eql c c-exp))
	    (push (list x
			(list s s-exp)
			(list c c-exp))
		  results)))))
    results))

(define-test sincos.consistent
  "Test sincos is consistent with sin and cos"
  (:tag :sincos)
  ;; Small values
  (assert-eql nil
	      (sincos-test (/ pi 4) 1000))
  ;; Medium
  (assert-eql nil
	      (sincos-test 16d0 1000))
  ;; Large
  (assert-eql nil
	      (sincos-test (scale-float 1d0 120) 1000))
  ;; Very large
  (assert-eql nil
	      (sincos-test (scale-float 1d0 1023) 1000)))

(defun close-to (actual expected &optional (threshold double-float-epsilon))
  "Determine if Actual is close to Expected.  If Expected is not zero,
  then close-to returns t if |Actual - Expected|/|Expected| <=
  Threshold.  If Expected is 0, then close-to returns T if |Actual -
  Expected| <= threshold.  In either of these conditions does not
  hold, then a list of the actual error (relative or absolute), the
  actual value and the expected value is returned."
  (let ((err (if (zerop expected)
		 (abs (- actual expected))
		 (/ (abs (- actual expected))
		    (abs expected)))))
    (if (<= err threshold)
	t
	(list err actual expected))))


;;; Tests for double-double-floats
(define-test dd-sin.signed-zeroes
  "Test sin for 0w0 and -0w0"
  (:tag :sin :double-double :signed-zeroes)
  (assert-eql 0w0 (sin 0w0))
  (assert-equal -0w0 (sin -0w0)))

(define-test dd-sin.no-reduction
  "Test sin for small args without reduction"
  (:tag :sin :double-double)
  (assert-eq t (close-to
		(sin .5w0)
		4.794255386042030002732879352155713880818033679406006751886166131w-1
		1w-32))
  (assert-eq t (close-to
		(sin -0.5w0)
		-4.794255386042030002732879352155713880818033679406006751886166131w-1
		1w-32)))

(define-test dd-sin.pi/2
  "Test for arg near pi/2"
  (:tag :sin :double-double)
  (assert-eq t (close-to
		(sin (/ kernel:dd-pi 2))
		1w0
		1w-50)))

;; The reference value were computed using maxima.  Here's how to
;; compute the reference value.  Set fpprec:64 to tell maxima to use
;; 64 digits of precision. For 7/4*pi, do (integer-decode-float (* 7/4
;; kernel:dd-pi)) to get the exact rational representation of the
;; desired double-double-float.  Then bfloat(sin(<rational>)).
(define-test dd-sin.arg-reduction
  "Test for sin with arg reduction"
  (:tag :sin :double-double)
  ;; Test for argument reduction with n mod 4 = 0
  (assert-eq t (close-to
		(sin (* 7/4 kernel:dd-pi))
		-7.07106781186547524400844362104849691328261037289050238659653433w-1
		0w0))
    ;; Test for argument reduction with n mod 4 = 1
  (assert-eq t (close-to
		(sin (* 9/4 kernel:dd-pi))
		7.07106781186547524400844362104858161816423215627023442400880643w-1
		0w0))
  ;; Test for argument reduction with n mod 4 = 2
  (assert-eq t (close-to
		(sin (* 11/4 kernel:dd-pi))
		7.071067811865475244008443621048998682901731241858306822215522497w-1
		8.716w-33))
  ;; Test for argument reduction with n mod 4 = 3
  (assert-eq t (close-to
		(sin (* 13/4 kernel:dd-pi))
		-7.071067811865475244008443621048777109664479707052746581685893187w-1
		8.716w-33))
  ;; Test for argument reduction, big value
  (assert-eq t (close-to
		(sin (scale-float 1w0 120))
		3.778201093607520226555484700569229919605866976512306642257987199w-1
		8.156w-33)))

(define-test dd-cos.signed-zeroes
  "Test cos for 0w0 and -0w0"
  (:tag :cos :double-double :signed-zeroes)
  (assert-eql 1w0 (cos 0w0))
  (assert-equal 1w0 (cos -0w0)))

(define-test dd-cos.no-reduction
  "Test cos for small args without reduction"
  (:tag :cos :double-double)
  (assert-eq t (close-to
		(cos .5w0)
		8.775825618903727161162815826038296519916451971097440529976108683w-1
		0w0))
  (assert-eq t (close-to
		(cos -0.5w0)
		8.775825618903727161162815826038296519916451971097440529976108683w-1
		0w0)))

(define-test dd-cos.pi/2
  "Test for arg near pi/2"
  (:tag :cos :double-double)
  (assert-eq t (close-to
		(cos (/ kernel:dd-pi 2))
		-1.497384904859169777320797133937725094986669701841027904483071358w-33
		0w0)))

(define-test dd-cos.arg-reduction
  "Test for cos with arg reduction"
  (:tag :cos :double-double)
  ;; Test for argument reduction with n mod 4 = 0
  (assert-eq t (close-to
		(cos (* 7/4 kernel:dd-pi))
		7.07106781186547524400844362104849691328261037289050238659653433w-1
		0w0))
    ;; Test for argument reduction with n mod 4 = 1
  (assert-eq t (close-to
		(cos (* 9/4 kernel:dd-pi))
		7.07106781186547524400844362104858161816423215627023442400880643w-1
		3.487w-32))
  ;; Test for argument reduction with n mod 4 = 2
  (assert-eq t (close-to
		(cos (* 11/4 kernel:dd-pi))
		-7.071067811865475244008443621048998682901731241858306822215522497w-1
		1.482w-31))
  ;; Test for argument reduction with n mod 4 = 3
  (assert-eq t (close-to
		(cos (* 13/4 kernel:dd-pi))
		-7.071067811865475244008443621048777109664479707052746581685893187w-1
		7.845w-32))
  ;; Test for argument reduction, big value
  (assert-eq t (close-to
		(cos (scale-float 1w0 120))
		-9.258790228548378673038617641074149467308332099286564602360493726w-1
		0w0)))

(define-test dd-tan.signed-zeroes
  "Test tan for 0w0 and -0w0"
  (:tag :tan :double-double :signed-zeroes)
  (assert-eql 0w0 (tan 0w0))
  (assert-equal -0w0 (tan -0w0)))

(define-test dd-tan.no-reduction
  "Test tan for small args without reduction"
  (:tag :tan :double-double)
  (assert-eq t (close-to
		(tan .5w0)
		5.463024898437905132551794657802853832975517201797912461640913859w-1
		0w0))
  (assert-eq t (close-to
		(tan -0.5w0)
		-5.463024898437905132551794657802853832975517201797912461640913859w-1
		0w0)))

(define-test dd-tan.pi/2
  "Test for arg near pi/2"
  (:tag :tan :double-double)
  (assert-eq t (close-to
		(tan (/ kernel:dd-pi 2))
		-6.67830961000672557834948096545679895621313886078988606234681001w32
		0w0)))

(define-test dd-tan.arg-reduction
  "Test for tan with arg reduction"
  (:tag :tan :double-double)
  ;; Test for argument reduction with n even
  (assert-eq t (close-to
		(tan (* 7/4 kernel:dd-pi))
		-1.000000000000000000000000000000001844257310064121018312678894979w0
		3.422w-49))
  ;; Test for argument reduction with n odd
  (assert-eq t (close-to
		(tan (* 9/4 kernel:dd-pi))
		1.000000000000000000000000000000025802415787810837455445433037983w0
		0w0))
  ;; Test for argument reduction, big value
  (assert-eq t (close-to
		(tan (scale-float 1w0 120))
		-4.080663888418042385451434945255951177650840227682488471558860153w-1
		1.888w-33)))

(define-test dd-sincos.signed-zeroes
  "Test sincos at 0d0, -0d0"
  (:tag :sincos :signed-zeroes :double-double)
  (assert-equal '(0w0 1w0)
		(multiple-value-list (kernel::dd-%sincos 0w0)))
  (assert-equal '(-0w0 1w0)
		(multiple-value-list (kernel::dd-%sincos -0w0))))

;; Test sincos at a bunch of random points and compare the result from
;; sin and cos.  If they differ, save the result in a list to be
;; returned.
(defun dd-sincos-test (limit n)
  (let (results)
    (dotimes (k n)
      (let* ((x (random limit))
	     (s-exp (sin x))
	     (c-exp (cos x)))
	(multiple-value-bind (s c)
	    (kernel::dd-%sincos x)
	  (unless (and (eql s s-exp)
		       (eql c c-exp))
	    (push (list x
			(list s s-exp)
			(list c c-exp))
		  results)))))
    results))

(define-test dd-sincos.consistent
  "Test sincos is consistent with sin and cos"
  (:tag :sincos :double-double)
  ;; Small values
  (assert-eql nil
	      (dd-sincos-test (/ kernel:dd-pi 4) 1000))
  ;; Medium
  (assert-eql nil
	      (dd-sincos-test 16w0 1000))
  ;; Large
  (assert-eql nil
	      (dd-sincos-test (scale-float 1w0 120) 1000))
  ;; Very large
  (assert-eql nil
	      (dd-sincos-test (scale-float 1w0 1023) 1000)))


;;; Tests for branch cuts.


;; Compute fun(arg) and check that the signs of the real and imaginary
;; parts match the value of real-sign and imag-sign, respectively.
;; Return T if the signs match.
(defun check-signs (fun arg real-sign imag-sign)
  (let* ((z (funcall fun arg))
	 (x (realpart z))
	 (y (imagpart z)))
    (cond ((and (= (float-sign x) real-sign)
		(= (float-sign y) imag-sign))
	   t)
	  (t
	   (format t "Sign of result doesn't match expected signs~%~
                 ~& fun = ~A~
                 ~& arg = ~A~
                 ~& res = ~A~
                 ~& expected = ~A ~A~%"
		   fun arg z real-sign imag-sign)
	   nil))))

;; Return the signs of the real and imaginary parts of z.
(defun get-signs (z)
  (values (float-sign (realpart z))
	  (float-sign (imagpart z))))

;; Carefully compute 1-z. For z = x + i*y, we want 1-x - i*y, which
;; only really matters when y is a signed zero.
(defun 1-z (z)
  (if (complexp z)
      (complex (- 1 (realpart z)) (- (imagpart z)))
      (- 1 z)))

(defun z-1 (z)
  (if (complexp z)
      (complex (- (realpart z) 1)
	       (imagpart z))
      (- z 1)))

  
;; Carefully compute 1+z. For z = x + i*y, we want 1+x + i*y, which
;; only really matters when y is a signed zero.
(defun 1+z (z)
  (if (complexp z)
      (complex (+ 1 (realpart z)) (imagpart z))
      (+ 1 z)))

(defun r-z (r z)
  (if (complexp z)
      (complex (- r (realpart z))
	       (- (imagpart z)))
      (- r z)))

;; Carefully compute i*z = i*(x+i*y) = -y + i*x.
(defun i*z (z)
  (if (complexp z)
      (complex (- (imagpart z)) (realpart z))
      (complex 0 z)))

;; Carefully compute r*z, where r is a real value and z is complex.
(defun r*z (r z)
  (if (complexp z)
      (complex (* r (realpart z)) (* r (imagpart z)))
      (* r z)))

;; asin(x) = -i*log(i*x + sqrt(1-x^2))
;;
;; The branch cut is the real axis |x| > 1.  For x < -1, it is
;; continuous with quadrant II; for x > 1, continuous with quadrant
;; IV.
;;
(defun asin-def (z)
  (- (i*z (log (+ (i*z z)
		  (sqrt (1-z (* z z))))))))


(define-test branch-cut.asin
  (:tag :asin :branch-cuts)
  ;; Test for x < -1, which is continuous with Quadrant II.  Compute
  ;; the value at #c(-2d0 1d-10) and check that components of
  ;; asin(-2+0.0*i) have the same signs as the reference value.
  (multiple-value-bind (tr ti)
      (get-signs (asin-def #c(-2d0 1d-20)))
    (assert-true (check-signs #'asin -2d0 tr ti))
    (assert-true (check-signs #'asin -2w0 tr ti))
    (assert-true (check-signs #'asin #c(-2d0 0) tr ti))
    (assert-true (check-signs #'asin #c(-2w0 0) tr ti)))
  ;; Test the other side of the branch cut for x < -1.
  (multiple-value-bind (tr ti)
      (get-signs (asin-def #c(-2d0 -1d-20)))
    (assert-true (check-signs #'asin #c(-2d0 -0d0) tr ti))
    (assert-true (check-signs #'asin #c(-2w0 -0w0) tr ti)))

  ;; Test for x > 1, which is continuous with Quadrant IV, using the
  ;; value at #c(+2d0 1d-10) as the reference
  (multiple-value-bind (tr ti)
      (get-signs (asin-def #c(2d0 1d-20)))
    (assert-true (check-signs #'asin #c(2d0 0) tr ti))
    (assert-true (check-signs #'asin #c(2w0 0) tr ti)))
  ;; Test the other side of the branch cut for x > 1.
  (multiple-value-bind (tr ti)
      (get-signs (asin-def #c(2d0 -1d-20)))
    (assert-true (check-signs #'asin 2d0 tr ti))
    (assert-true (check-signs #'asin 2w0 tr ti))
    (assert-true (check-signs #'asin #c(2d0 -0d0) tr ti))
    (assert-true (check-signs #'asin #c(2w0 -0w0) tr ti))))

;; acos(z) = pi/2 - asin(z).
;;
;; The branch cut is the real axis for |x| > 1.  For x < -1, it is
;; continous with Quadrant II; for x > 1, Quadrant IV.
(defun acos-def (z)
  (if (typep z 'kernel:double-double-float)
      (r-z (/ kernel:dd-pi 2)
	   (asin-def z))
      (r-z (/ pi 2)
	   (asin-def z))))

(define-test branch-cut.acos
  (:tag :acos :branch-cuts)
  ;; Test for x < -1, which is continuous with Quadrant II.  Compute
  ;; the value at #c(-2d0 1d-10) and check that components of
  ;; acos(-2+0.0*i) have the same signs as the reference value.
  (multiple-value-bind (tr ti)
      (get-signs (acos-def #c(-2d0 1d-20)))
    (assert-true (check-signs #'acos -2d0 tr ti))
    (assert-true (check-signs #'acos -2w0 tr ti))
    (assert-true (check-signs #'acos #c(-2d0 0) tr ti))
    (assert-true (check-signs #'acos #c(-2w0 0) tr ti)))
  ;; Test the other side of the branch cut for x < -1.
  (multiple-value-bind (tr ti)
      (get-signs (acos-def #c(-2d0 -1d-20)))
    (assert-true (check-signs #'acos #c(-2d0 -0d0) tr ti))
    (assert-true (check-signs #'acos #c(-2w0 -0w0) tr ti)))

  ;; Test for x > 1, which is continuous with Quadrant IV, using the
  ;; value at #c(+2d0 1d-10) as the reference
  (multiple-value-bind (tr ti)
      (get-signs (acos-def #c(2d0 1d-20)))
    (assert-true (check-signs #'acos #c(2d0 0) tr ti))
    (assert-true (check-signs #'acos #c(2w0 0) tr ti)))
  ;; Test the other side of the branch cut for x > 1.
  (multiple-value-bind (tr ti)
      (get-signs (acos-def #c(2d0 -1d-20)))
    (assert-true (check-signs #'acos 2d0 tr ti))
    (assert-true (check-signs #'acos 2w0 tr ti))
    (assert-true (check-signs #'acos #c(2d0 -0d0) tr ti))
    (assert-true (check-signs #'acos #c(2w0 -0w0) tr ti))))

;; atan(z) = (log(1+i*z) - log(1-i*z))/(2*i)
;;         = -i/2*(log(1+i*z) - log(1-i*z))
;;
;; WARNING: The CLHS is a bit confused here. Two definitions of atan
;; are given in the CLHS
;; http://www.lispworks.com/documentation/HyperSpec/Body/f_asin_.htm
;; and they are not consistent.  Plus, there is a typo in the second
;; definition. (Missing parens.)
;;
;; For clarification, we turn to
;; http://www.lispworks.com/documentation/HyperSpec/Issues/iss069_w.htm,
;; which recommends using the second formula and also puts in the
;; parentheses in the correct places.
;;
;; BUT, this is further confused by the example that atan(0+2*i) is
;; 1.57-0.549*i for the proposed formula but -1.57+0.549*i under the
;; current formula.
;;
;;
;; I think the inconsistency is that the results are derived without
;; signed zeroes.  But we have signed zeroes, so let us derive the
;; actual value of atan(0+2*i) using the (second) formula.
;;
;;   atan(0+2*i) = (log(1+i*(0+2*i)) - log(1-i*(0+2*i)))/(2*i)
;;      = (log(1+(-2+0*i)) - log(1-(-2+0*i)))/(2*i)
;;      = (log(-1-0*i) - log(3-0*i))/(2*i)
;;      = ((log(1) - pi*i) - (log(3) - 0*i))/(2*i)
;;      = (-log(3) - pi*i)/(2*i)
;;      = -pi/2 + log(3)/2*i
;;
;; The branch cut is the imaginary axis, |y| > 1.  For y < -1, atan is
;; continuous with Quadrant IV; for y > 1, Quadrant II.
(defun atan-def (z)
  (let* ((iz (i*z z))
	 (w (- (log (1+z iz))
	       (log (1-z iz)))))
    (r*z -1/2 (i*z w))))

(define-test branch-cut.atan
  (:tag :atan :branch-cuts)
  ;; Test for y < -1, which is continuous with Quadrant IV.  Use the
  ;; value at #c(1d-20 -2d0) as the reference.
  (multiple-value-bind (tr ti)
      (get-signs (atan-def #c(1d-20 -2d0)))
    (assert-true (check-signs #'atan #c(0d0 -2d0) tr ti))
    (assert-true (check-signs #'atan #c(0w0 -2w0) tr ti)))
  ;; Test the other side of the branch cut for x < -1.
  (multiple-value-bind (tr ti)
      (get-signs (atan-def #c(-1d-20 -2d0)))
    (assert-true (check-signs #'atan #c(-0d0 -2d0) tr ti))
    (assert-true (check-signs #'atan #c(-0w0 -2w0) tr ti)))

  ;; Test for y > 1, which is continuous with Quadrant II, using the
  ;; value at #c(-1d-20 +2d0) as the reference
  (multiple-value-bind (tr ti)
      (get-signs (atan-def #c(-1d-20 2d0)))
    (assert-true (check-signs #'atan #c(-0d0 2d0) tr ti))
    (assert-true (check-signs #'atan #c(-0w0 2w0) tr ti)))
  ;; Test the other side of the branch cut for x > 1.
  (multiple-value-bind (tr ti)
      (get-signs (atan-def #c(1d-20 2d0)))
    (assert-true (check-signs #'atan #c(0d0 2d0) tr ti))
    (assert-true (check-signs #'atan #c(0d0 2w0) tr ti))))

;; asinh(z) = log(z + sqrt(1+z^2))
;;
;; The branch cut is the imaginary axis with |y| > 1. For y > 1, asinh
;; is continuous with Quadrant I.  For y < -1, it is continuous with
;; Quadrant III.

(defun asinh-def (z)
  (log (+ z (sqrt (1+z (* z z))))))

(define-test branch-cut.asinh
  (:tag :asinh :branch-cuts)
  ;; Test for y < -1, which is continuous with Quadrant I.  Use the
  ;; value at #c(1d-20 -2d0) as the reference.
  (multiple-value-bind (tr ti)
      (get-signs (asinh-def #c(1d-20 -2d0)))
    (assert-true (check-signs #'asinh #c(0d0 -2d0) tr ti))
    (assert-true (check-signs #'asinh #c(0w0 -2w0) tr ti)))
  ;; Test the other side of the branch cut for y < -1.
  (multiple-value-bind (tr ti)
      (get-signs (asinh-def #c(-1d-20 -2d0)))
    (assert-true (check-signs #'asinh #c(-0d0 -2d0) tr ti))
    (assert-true (check-signs #'asinh #c(-0w0 -2w0) tr ti)))

  ;; Test for y > 1, which is continuous with Quadrant III, using the
  ;; value at #c(-1d-20 +2d0) as the reference
  (multiple-value-bind (tr ti)
      (get-signs (asinh-def #c(-1d-20 2d0)))
    (assert-true (check-signs #'asinh #c(-0d0 2d0) tr ti))
    (assert-true (check-signs #'asinh #c(-0w0 2w0) tr ti)))
  ;; Test the other side of the branch cut for x > 1.
  (multiple-value-bind (tr ti)
      (get-signs (asinh-def #c(1d-20 2d0)))
    (assert-true (check-signs #'asinh #c(0d0 2d0) tr ti))
    (assert-true (check-signs #'asinh #c(0d0 2w0) tr ti))))

;; acosh(z) = 2*log(sqrt((z+1)/2) + sqrt((z-1)/2))
;;
;; The branch cut is along the real axis with x < 1.  For x < 0, it is
;; continuous with Quadrant II.  For 0< x < 1, it is continuous with
;; Quadrant I.

(defun acosh-def (z)
  (r*z 2
       (log (+ (sqrt (r*z 1/2 (1+z z)))
	       (sqrt (r*z 1/2 (z-1 z)))))))


(define-test branch-cut.acosh
  (:tag :acosh :branch-cuts)
  ;; Test for x < 0, which is continuous with Quadrant II.  Use the
  ;; value at #c(-2d0 1d-20) as a reference.
  (multiple-value-bind (tr ti)
      (get-signs (acosh-def #c(-2d0 1d-20)))
    (assert-true (check-signs #'acosh -2d0 tr ti))
    ;;(assert-true (check-signs #'acosh -2w0 tr ti))
    (assert-true (check-signs #'acosh #c(-2d0 0) tr ti))
    ;;(assert-true (check-signs #'acosh #c(-2w0 0) tr ti))
    )
  ;; Test the other side of the branch cut for x < -1.
  (multiple-value-bind (tr ti)
      (get-signs (acosh-def #c(-2d0 -1d-20)))
    (assert-true (check-signs #'acosh #c(-2d0 -0d0) tr ti))
    ;;(assert-true (check-signs #'acosh #c(-2w0 -0w0) tr ti))
    )

  ;; Test for 0 < x < 1, which is continuous with Quadrant I, using the
  ;; value at #c(0.25d0 1d-10) as the reference.
  (multiple-value-bind (tr ti)
      (get-signs (acosh-def #c(0.25d0 1d-20)))
    (assert-true (check-signs #'acosh #c(0.25d0 0) tr ti))
    (assert-true (check-signs #'acosh #c(0.25w0 0) tr ti))
    )
  ;; Test the other side of the branch cut for 0 < x < 1.
  (multiple-value-bind (tr ti)
      (get-signs (acosh-def #c(0.25d0 -1d-20)))
    (assert-true (check-signs #'acosh #c(0.25d0 -0d0) tr ti))
    (assert-true (check-signs #'acosh #c(0.25w0 -0w0) tr ti))))

;; atanh(z) = 1/2*(log(1+z) - log(1-z))
;;
;; The branch cut is on the real axis for |x| > 1.  For x < -1, it is
;; continuous with Quadrant III.  For x > 1, it is continuous with
;; quadrant I.
;;
;; NOTE: The rules above are what is given by the CLHS. However,
;; consider the value of atanh(-2) and atanh(-2-0.0*i)
;;
;;  atanh(-2) = 1/2*(log(1-2) - log(1+2))
;;            = 1/2*(log(-1) - log(3))
;;            = 1/2*(i*pi - log(3))
;;            = -1/2*log(3) + i*pi/2
;;
;;  atanh(-2-0*i) = 1/2*(log(1+(-2-0*i)) - log(1-(-2-0*i)))
;;                = 1/2*(log(-1-0*i) - log(3-0*i))
;;                = 1/2*(-i*pi - log(3))
;;                = -1/2*log(3) - i*pi/2
;;
;;  atanh(-2+0*i) = 1/2*(log(1+(-2+0*i)) - log(1-(-2+0*i)))
;;                = 1/2*(log(-1+0*i) - log(3-0*i))
;;                = 1/2*(i*pi - log(3))
;;                = -1/2*log(3) + i*pi/2
;;
;; Thus, atanh(-2) is continuous with Quadrant II, NOT continuous with
;; Quadrant III!
;;
;; The formula, however, is clear.  We will use the formula and ignore
;; the text in the CLHS.
(defun atanh-def (z)
  (r*z 1/2
       (- (log (1+z z))
	  (log (1-z z)))))

(define-test branch-cut.atanh
  (:tag :atanh :branch-cuts)
  ;; Test for x < -1, which is continuous with Quadrant II.  Use the
  ;; the value at #c(-2d0 +1d-20) as the reference.
  (multiple-value-bind (tr ti)
      (get-signs (atanh-def #c(-2d0 1d-20)))
    (assert-true (check-signs #'atanh -2d0 tr ti))
    (assert-true (check-signs #'atanh -2w0 tr ti))
    (assert-true (check-signs #'atanh #c(-2d0 0d0) tr ti))
    (assert-true (check-signs #'atanh #c(-2w0 0w0) tr ti)))
  ;; Test the other side of the branch cut for x < -1.
  (multiple-value-bind (tr ti)
      (get-signs (atanh-def #c(-2d0 -1d-20)))
    (assert-true (check-signs #'atanh #c(-2d0 -0d0) tr ti))
    (assert-true (check-signs #'atanh #c(-2w0 -0w0) tr ti)))

  ;; Test for x > 1, which is continuous with Quadrant IV, using the
  ;; value at #c(+2d0 -1d-10) as the reference
  (multiple-value-bind (tr ti)
      (get-signs (atanh-def #c(2d0 -1d-10)))
    (assert-true (check-signs #'atanh 2d0 tr ti))
    (assert-true (check-signs #'atanh 2w0 tr ti))
    (assert-true (check-signs #'atanh #c(2d0 -0d0) tr ti))
    (assert-true (check-signs #'atanh #c(2w0 -0w0) tr ti)))
  ;; Test the other side of the branch cut for x > 1.
  (multiple-value-bind (tr ti)
      (get-signs (atanh-def #c(2d0 +1d-20)))
    (assert-true (check-signs #'atanh #c(2d0 0d0) tr ti))
    (assert-true (check-signs #'atanh #c(2w0 0w0) tr ti))))
