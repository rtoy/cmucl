;; Tests for pprinter

(defpackage :pprint-tests
  (:use :cl :lisp-unit))

(in-package "PPRINT-TESTS")

(define-test pprint.with-float-traps-masked
    (:tag :issues)
  (assert-equal 
"
(WITH-FLOAT-TRAPS-MASKED (:UNDERFLOW)
  (LET* ((RATIO
          (/ (* (EXPT 10 PPRINT-TESTS::EXPONENT) NUMBER)
             PPRINT-TESTS::DIVISOR))
         (PPRINT-TESTS::RESULT (COERCE RATIO PPRINT-TESTS::FLOAT-FORMAT)))
    (WHEN (AND (ZEROP PPRINT-TESTS::RESULT) (NOT (ZEROP NUMBER)))
      (ERROR \"Underflow\"))
    PPRINT-TESTS::RESULT))"
     (with-output-to-string (s)
       (pprint '(ext:with-float-traps-masked (:underflow)
                 (let* ((ratio (/ (* (expt 10 exponent) number)
                                  divisor))
	                (result (coerce ratio float-format)))
                   (when (and (zerop result) (not (zerop number)))
                     (error "Underflow"))
                   result))
               s))))

(define-test pprint.with-float-traps-enabled
    (:tag :issues)
  (assert-equal 
"
(WITH-FLOAT-TRAPS-ENABLED (:UNDERFLOW)
  (LET* ((RATIO
          (/ (* (EXPT 10 PPRINT-TESTS::EXPONENT) NUMBER)
             PPRINT-TESTS::DIVISOR))
         (PPRINT-TESTS::RESULT (COERCE RATIO PPRINT-TESTS::FLOAT-FORMAT)))
    (WHEN (AND (ZEROP PPRINT-TESTS::RESULT) (NOT (ZEROP NUMBER)))
      (ERROR \"Underflow\"))
    PPRINT-TESTS::RESULT))"
     (with-output-to-string (s)
       (pprint '(ext:with-float-traps-enabled (:underflow)
                 (let* ((ratio (/ (* (expt 10 exponent) number)
                                  divisor))
	                (result (coerce ratio float-format)))
                   (when (and (zerop result) (not (zerop number)))
                     (error "Underflow"))
                   result))
               s))))


  
