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

(define-test issue.5
    (:tag :issues)
  (assert-true
   (handler-case
       (let ((f (compile nil '(lambda (list)
			       (declare (type list list)
				(optimize (speed 1) (safety 1) (compilation-speed 1) (space 1) (debug 1)))
			       (elt list 3)))))
	 (funcall f (list 0 1 2)))
     ;; ELT should signal an error in this case.
     (lisp::index-too-large-error ()
       t)
     (t ()
       nil))))
