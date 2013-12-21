;;; Tests for the basic trig functions, now implemented in Lisp.

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

;; Compute the relative error between actual and expected if expected
;; is not zero. Otherwise, return absolute error between actual and
;; expected.  If the error is less than the threshold, return T.
;; Otherwise return the actual (relative or absolute) error.
(defun rel-or-abs-error (actual expected &optional (threshold double-float-epsilon))
  (let ((err (if (zerop expected)
		 (abs (- actual expected))
		 (/ (abs (- actual expected))
		    (abs expected)))))
    (if (<= err threshold)
	t
	err)))

(define-test dd-sin.signed-zeroes
  "Test sin for 0w0 and -0w0"
  (:tag :sin :double-double :signed-zeroes)
  (assert-eql 0w0 (sin 0w0))
  (assert-equal -0w0 (sin -0w0)))

(define-test dd-sin.no-reduction
  "Test sin for small args without reduction"
  (:tag :sin :double-double)
  (assert-eq t (rel-or-abs-error
		(sin .5w0)
		4.794255386042030002732879352155713880818033679406006751886166131w-1
		1w-32))
  (assert-eq t (rel-or-abs-error
		(sin -0.5w0)
		-4.794255386042030002732879352155713880818033679406006751886166131w-1
		1w-32)))

(define-test dd-sin.pi/2
  "Test for arg near pi/2"
  (:tag :sin :double-double)
  (assert-eq t (rel-or-abs-error
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
  (assert-eq t (rel-or-abs-error
		(sin (* 7/4 kernel:dd-pi))
		-7.07106781186547524400844362104849691328261037289050238659653433w-1
		0w0))
    ;; Test for argument reduction with n mod 4 = 1
  (assert-eq t (rel-or-abs-error
		(sin (* 9/4 kernel:dd-pi))
		7.07106781186547524400844362104858161816423215627023442400880643w-1
		0w0))
  ;; Test for argument reduction with n mod 4 = 2
  (assert-eq t (rel-or-abs-error
		(sin (* 11/4 kernel:dd-pi))
		7.071067811865475244008443621048998682901731241858306822215522497w-1
		8.716w-33))
  ;; Test for argument reduction with n mod 4 = 3
  (assert-eq t (rel-or-abs-error
		(sin (* 13/4 kernel:dd-pi))
		-7.071067811865475244008443621048777109664479707052746581685893187w-1
		8.716w-33))
  ;; Test for argument reduction, big value
  (assert-eq t (rel-or-abs-error
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
  (assert-eq t (rel-or-abs-error
		(cos .5w0)
		8.775825618903727161162815826038296519916451971097440529976108683w-1
		0w0))
  (assert-eq t (rel-or-abs-error
		(cos -0.5w0)
		8.775825618903727161162815826038296519916451971097440529976108683w-1
		0w0)))

(define-test dd-cos.pi/2
  "Test for arg near pi/2"
  (:tag :cos :double-double)
  (assert-eq t (rel-or-abs-error
		(cos (/ kernel:dd-pi 2))
		-1.497384904859169777320797133937725094986669701841027904483071358w-33
		0w0)))

(define-test dd-cos.arg-reduction
  "Test for cos with arg reduction"
  (:tag :cos :double-double)
  ;; Test for argument reduction with n mod 4 = 0
  (assert-eq t (rel-or-abs-error
		(cos (* 7/4 kernel:dd-pi))
		7.07106781186547524400844362104849691328261037289050238659653433w-1
		0w0))
    ;; Test for argument reduction with n mod 4 = 1
  (assert-eq t (rel-or-abs-error
		(cos (* 9/4 kernel:dd-pi))
		7.07106781186547524400844362104858161816423215627023442400880643w-1
		3.487w-32))
  ;; Test for argument reduction with n mod 4 = 2
  (assert-eq t (rel-or-abs-error
		(cos (* 11/4 kernel:dd-pi))
		-7.071067811865475244008443621048998682901731241858306822215522497w-1
		1.482w-31))
  ;; Test for argument reduction with n mod 4 = 3
  (assert-eq t (rel-or-abs-error
		(cos (* 13/4 kernel:dd-pi))
		-7.071067811865475244008443621048777109664479707052746581685893187w-1
		7.845w-32))
  ;; Test for argument reduction, big value
  (assert-eq t (rel-or-abs-error
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
  (assert-eq t (rel-or-abs-error
		(tan .5w0)
		5.463024898437905132551794657802853832975517201797912461640913859w-1
		0w0))
  (assert-eq t (rel-or-abs-error
		(tan -0.5w0)
		-5.463024898437905132551794657802853832975517201797912461640913859w-1
		0w0)))

(define-test dd-tan.pi/2
  "Test for arg near pi/2"
  (:tag :tan :double-double)
  (assert-eq t (rel-or-abs-error
		(tan (/ kernel:dd-pi 2))
		-6.67830961000672557834948096545679895621313886078988606234681001w32
		0w0)))

(define-test dd-tan.arg-reduction
  "Test for tan with arg reduction"
  (:tag :tan :double-double)
  ;; Test for argument reduction with n even
  (assert-eq t (rel-or-abs-error
		(tan (* 7/4 kernel:dd-pi))
		-1.000000000000000000000000000000001844257310064121018312678894979w0
		6.467w-33))
  ;; Test for argument reduction with n odd
  (assert-eq t (rel-or-abs-error
		(tan (* 9/4 kernel:dd-pi))
		1.000000000000000000000000000000025802415787810837455445433037983w0
		5.773w-33))
  ;; Test for argument reduction, big value
  (assert-eq t (rel-or-abs-error
		(tan (scale-float 1w0 120))
		-4.080663888418042385451434945255951177650840227682488471558860153w-1
		1.888w-33)))

