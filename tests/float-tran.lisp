
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

(define-test decode-float-exp
  "Test type derivation of the exponent from decode-float"
  (assert-equalp (c::specifier-type '(integer -148 128))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type 'single-float)))
  (assert-equalp (c::specifier-type '(integer -1073 1024))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type 'double-float)))
  #+double-double
  (assert-equalp (c::specifier-type '(integer -1073 1024))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type 'double-double-float)))
  (assert-equalp (c::specifier-type '(integer 2 8))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type '(double-float 2d0 128d0)))))
