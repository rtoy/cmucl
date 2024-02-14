;;; Tests from gitlab issues

(defpackage :loop-tests
  (:use :cl :lisp-unit))

(in-package "LOOP-TESTS")

(define-test loop-var-nil
    (:tag :issues)
  ;; Just verify that (loop for var nil ...) works.  Previously it
  ;; signaled an error.  See Gitlab issue #256.
  (assert-equal '(1 2)
                (loop for var nil from 1 to 2 collect var)))

