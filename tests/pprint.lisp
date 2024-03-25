;; Tests for pprinter

(defpackage :pprint-tests
  (:use :cl :lisp-unit))

(in-package "PPRINT-TESTS")

(define-test pprint.with-float-traps-masked
    (:tag :issues)
  (assert-equal 
"
(WITH-FLOAT-TRAPS-MASKED (:UNDERFLOW)
  (PRINT \"Hello\"))"
     (with-output-to-string (s)
       (pprint '(ext:with-float-traps-masked (:underflow)
                 (print "Hello"))
               s))))

(define-test pprint.with-float-traps-enabled
    (:tag :issues)
  (assert-equal 
"
(WITH-FLOAT-TRAPS-ENABLED (:UNDERFLOW)
  (PRINT \"Hello\"))"
     (with-output-to-string (s)
       (pprint '(ext:with-float-traps-enabled (:underflow)
                 (print "Hello"))
               s))))
