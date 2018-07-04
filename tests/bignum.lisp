;;; Tests for the bignum operations

(defpackage :bignum-tests
  (:use :cl :lisp-unit))

(in-package #:bignum-tests)

(define-test hd-mult
  "Test bignum multiplier"
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
	       (prod-knuth (bignum::classical-multiply-bignums r1 r2))
	       (prod-hd (bignum::classical-multiply-bignum-hd r1 r2)))
	  (assert-equal prod-knuth prod-hd r1 r2))))))
