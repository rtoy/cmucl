;;; Tests for the double-double arithmetic..

(defpackage :dd-tests
  (:use :cl :lisp-unit))

(in-package "DD-TESTS")

(define-test two-prod
  "Test two-prod"
  ;; This should not overflow anymore.
  (assert-equal (values 1.7976931281653871d308
			-4.9896007738368d291)
		(c::two-prod 1.7976931214684583d308 (1+ (scale-float 1d0 -28)))))
