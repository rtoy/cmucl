;;; Tests from gitlab issues

(defpackage :issues-tests
  (:use :cl :lisp-unit))

(in-package "ISSUES-TESTS")

(defun square (x)
  (expt x 2))

(define-compiler-macro square (&whole form arg)
  (declare (ignore arg))
  form)

(define-test issue.1.a
    (:tag :issues)
  (assert-equal
   '(square x)
   (funcall (compiler-macro-function 'square) '(square x) nil)))

(define-test issue.1.b
    (:tag :issues)
  (assert-equal
   '(square x)
   (funcall (compiler-macro-function 'square) '(funcall #'square x) nil)))
