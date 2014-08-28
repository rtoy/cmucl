;; Tests for various source transformations.

(defpackage :srctran-tests
  (:use :cl :lisp-unit))

(in-package "SRCTRAN-TESTS")

(define-test floor-quotient-bound
  "Test the first value of FLOOR returns the correct interval"
  (assert-equalp (c::make-interval :low 0 :high 10)
		 (c::floor-quotient-bound
		  (c::make-interval :low 0.3 :high 10.3)))
  (assert-equalp (c::make-interval :LOW 0 :HIGH 10)
		 (c::floor-quotient-bound
		  (c::make-interval :low 0.3 :high '(10.3))))
  (assert-equalp (c::make-interval :LOW 0 :HIGH 10)
		(c::floor-quotient-bound (c::make-interval :low 0.3 :high 10)))
  (assert-equalp (c::make-interval :LOW 0 :HIGH 9)
		 (c::floor-quotient-bound (c::make-interval :low 0.3 :high '(10))))
  (assert-equalp (c::make-interval :LOW 0 :HIGH 9)
		 (c::floor-quotient-bound (c::make-interval :low 0.3 :high '(10))))
  (assert-equalp (c::make-interval :LOW 0 :HIGH 10)
		 (c::floor-quotient-bound (c::make-interval :low '(0.3) :high 10.3)))
  (assert-equalp (c::make-interval :LOW 0 :HIGH 10)
		 (c::floor-quotient-bound (c::make-interval :low '(0.0) :high 10.3)))
  (assert-equalp (c::make-interval :LOW -2 :HIGH 10)
		 (c::floor-quotient-bound (c::make-interval :low '(-1.3) :high 10.3)))
  (assert-equalp (c::make-interval :LOW -1 :HIGH 10)
		 (c::floor-quotient-bound (c::make-interval :low '(-1.0) :high 10.3)))
  (assert-equalp (c::make-interval :LOW -1 :HIGH 10)
		 (c::floor-quotient-bound (c::make-interval :low -1.0 :high 10.3))))

(define-test floor-rem-bound
  "Test the second value of FLOOR returns the correct interval"
  (assert-equalp (c::make-interval :low 0 :high '(10.3))
		 (c::floor-rem-bound (c::make-interval :low 0.3 :high 10.3)))
  (assert-equalp (c::make-interval :low 0 :high '(10.3))
		 (c::floor-rem-bound (c::make-interval :low 0.3 :high '(10.3))))
  (assert-equalp (c::make-interval :low '(-10) :high 0)
		 (c::floor-rem-bound (c::make-interval :low -10 :high -2.3)))
  (assert-equalp (c::make-interval :low 0 :high '(10))
		 (c::floor-rem-bound (c::make-interval :low 0.3 :high 10)))
  (assert-equalp (c::make-interval :low '(-10.3) :high '(10.3))
		 (c::floor-rem-bound (c::make-interval :low '(-1.3) :high 10.3)))
  (assert-equalp (c::make-interval :low '(-20.3) :high '(20.3))
		 (c::floor-rem-bound (c::make-interval :low '(-20.3) :high 10.3))))

(define-test ceiling-quotient-bound
  "Test the first value of CEILING returns the correct interval"
  (assert-equalp (c::make-interval :low 1 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low 0.3 :high 10.3)))
  (assert-equalp (c::make-interval :low 1 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low 0.3 :high '(10.3))))
  (assert-equalp (c::make-interval :low 1 :high 10)
		 (c::ceiling-quotient-bound (c::make-interval :low 0.3 :high 10)))
  (assert-equalp (c::make-interval :low 1 :high 10)
		 (c::ceiling-quotient-bound (c::make-interval :low 0.3 :high '(10))))
  (assert-equalp (c::make-interval :low 1 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low '(0.3) :high 10.3)))
  (assert-equalp (c::make-interval :low 1 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low '(0.0) :high 10.3)))
  (assert-equalp (c::make-interval :low -1 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low '(-1.3) :high 10.3)))
  (assert-equalp (c::make-interval :low 0 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low '(-1.0) :high 10.3)))
  (assert-equalp (c::make-interval :low -1 :high 11)
		 (c::ceiling-quotient-bound (c::make-interval :low -1.0 :high 10.3))))

(define-test ceiling-rem-bound
  "Test the second value of CEILING returns the correct interval"
  (assert-equalp (c::make-interval :low '(-10.3) :high 0)
		 (c::ceiling-rem-bound (c::make-interval :low 0.3 :high 10.3)))
  (assert-equalp (c::make-interval :low '(-10.3) :high 0)
		 (c::ceiling-rem-bound (c::make-interval :low 0.3 :high '(10.3))))
  (assert-equalp (c::make-interval :low 0 :high '(10))
		 (c::ceiling-rem-bound (c::make-interval :low -10 :high -2.3)))
  (assert-equalp (c::make-interval :low '(-10) :high 0)
		 (c::ceiling-rem-bound (c::make-interval :low 0.3 :high 10)))
  (assert-equalp (c::make-interval :low '(-10.3) :high '(10.3))
		 (c::ceiling-rem-bound (c::make-interval :low '(-1.3) :high 10.3)))
  (assert-equalp (c::make-interval :low '(-20.3) :high '(20.3))
		 (c::ceiling-rem-bound (c::make-interval :low '(-20.3) :high 10.3))))
