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

