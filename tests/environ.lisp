;; Tests for environment dictionary

(defpackage :environ-tests
  (:use :cl :lisp-unit))

(in-package "ENVIRON-TESTS")

(define-test software-type
    (:tag :issues)
  (let ((type (software-type)))
    ;; Can't really test anything, so just verify we get a non-empty
    ;; string.
    (assert-true (typep type 'string))
    (assert-true (plusp (length type)))))

(define-test software-version
    (:tag :issues)
  (let ((version (software-version)))
    ;; Can't really test anything, so just verify we get a non-empty
    ;; string.
    (assert-true (typep version 'string))
    (assert-true (plusp (length version)))))

