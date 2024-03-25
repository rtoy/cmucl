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

(define-test pprint.handler-case  
    (:tag :issues)
  (assert-equal 
"
(HANDLER-CASE (SIGNAL CONDITION)
  (WARNING NIL
    \"Lots of smoke, but no fire.\")
  ((OR ARITHMETIC-ERROR CONTROL-ERROR CELL-ERROR STREAM-ERROR) (CONDITION)
    (FORMAT NIL \"~S looks especially bad.\" CONDITION))
  (SERIOUS-CONDITION (CONDITION)
    (FORMAT NIL \"~S looks serious.\" CONDITION))
  (CONDITION NIL
    \"Hardly worth mentioning.\"))"
    (with-output-to-string (s)
      (pprint '(handler-case (signal condition)
                (warning () "Lots of smoke, but no fire.")
                ((or arithmetic-error control-error cell-error stream-error)
                 (condition)
                 (format nil "~S looks especially bad." condition))
                (serious-condition (condition)
                 (format nil "~S looks serious." condition))
                (condition () "Hardly worth mentioning."))
              s))))
