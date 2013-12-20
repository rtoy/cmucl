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

