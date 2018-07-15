;;; Tests for the bignum operations

(defpackage :bignum-tests
  (:use :cl :lisp-unit))

(in-package #:bignum-tests)

(define-test hd-mult.same-size
  "Test bignum multiplier"
  (:tag :bignum-tests)
  ;; x and y are randomly generated 128 integers. No particular reason
  ;; for these values, except that they're bignums.
  (let ((x 248090201001762284446997112921270181259)
	(y 313102667534462314033767199170708979663)
	(prod 77677703722812705876871716049945873590003455155145426220435549433670954735717))
    ;; Verify the we get the right results for various signed values of x and y.
    (assert-equal prod (* x y))
    (assert-equal (- prod) (* (- x) y))
    (assert-equal (- prod) (* x (- y)))
    (assert-equal prod (* (- x) (- y)))
    ;; Nake sure it's commutative
    (assert-equal prod (* y x))
    (assert-equal (- prod) (* y (- x)))
    (assert-equal (- prod) (* (- y) x))
    (assert-equal prod (* (- y) (- x)))))

(define-test hd-mult.diff-size
  "Test bignum multiplier"
  (:tag :bignum-tests)
  ;; x is a randomly generated bignum.  y is a small bignum.
  (let ((x 248090201001762284446997112921270181259)
	(y (1+ most-positive-fixnum))
	(prod 133192412470079431258262755675409306410924638208))
    ;; Verify the we get the right results for various signed values of x and y.
    (assert-equal prod (* x y))
    (assert-equal (- prod) (* (- x) y))
    (assert-equal (- prod) (* x (- y)))
    (assert-equal prod (* (- x) (- y)))
    ;; Nake sure it's commutative
    (assert-equal prod (* y x))
    (assert-equal (- prod) (* y (- x)))
    (assert-equal (- prod) (* (- y) x))
    (assert-equal prod (* (- y) (- x)))))


(define-test hd-mult.random
  "Test bignum multiplier with random values"
  (:tag :bignum-tests)
  (let ((rng (kernel::make-random-object :state (kernel:init-random-state)
					 :rand 0
					 :cached-p nil))
	(range (ash 1 128)))
    (flet ((gen-bignum (x sign)
	     (do ((r (random x rng) (random x rng)))
		 ((typep r 'bignum)
		  (if (zerop sign)
		      r
		      (- r))))))
      (dotimes (k 100)
	(let* ((r1 (gen-bignum range (random 2 rng)))
	       (r2 (gen-bignum range (random 2 rng)))
	       (prod-knuth (bignum::classical-multiply-bignums-knuth r1 r2))
	       (prod-hd (bignum::classical-multiply-bignums r1 r2)))
	  (assert-equal prod-knuth prod-hd r1 r2))))))


;; Just for simple timing tests so we can redo the timing tests if needed.
#+nil
(define-test hd-timing
  "Test execution time"
  (:tag :bignum-tests)
  (let ((rng (kernel::make-random-object :state
					 (kernel:init-random-state)
					 :rand 0 :cached-p nil))
	(range (ash 1 128))
	(reps 10000))
    (flet ((gen-bignum (x sign)
	     (do ((r (random x rng) (random x rng)))
		 ((typep r 'bignum)
		  (if (zerop sign)
		      r (- r))))))
      (let* ((r1 (gen-bignum range 1))
	     (r2 (gen-bignum range 1)) res)
	(time
	 (dotimes (k reps)
	   (declare (fixnum k))
	   (setf res (bignum::classical-multiply-bignums-knuth r1 r2))))
	(print res)
	(time
	 (dotimes (k reps)
	   (declare (fixnum k))
	   (setf res (bignum::classical-multiply-bignums r1 r2))))
	(print res)))))

