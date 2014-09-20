;; Tests for various float transformations.

(defpackage :float-tran-tests
  (:use :cl :lisp-unit))

(in-package "FLOAT-TRAN-TESTS")

(define-test decode-float-sign
  "Test type derivation of the sign from decode-float"
  (assert-equalp (c::make-member-type :members (list 1f0 -1f0))
		 (c::decode-float-sign-derive-type-aux (c::specifier-type 'single-float)))
  (assert-equalp (c::make-member-type :members (list 1d0 -1d0))
		 (c::decode-float-sign-derive-type-aux (c::specifier-type 'double-float)))
  (assert-equalp (c::make-member-type :members (list 1f0))
		 (c::decode-float-sign-derive-type-aux (c::specifier-type '(single-float (0f0))))))

  