;; Tests of string functions

(defpackage :string-tests
  (:use :cl :lisp-unit))

(in-package "STRING-TESTS")

(defun make-test-string ()
  ;; Create a string consisting of all the code units EXCEPT for the
  ;; surrogates because string casing handles that differently.
  (coerce
   (loop for code from 0 to #xffff
         unless (lisp::surrogatep code)
         collect (code-char code))
   'string))

(define-test string-upcase
    (:tag :issues)
  (let* ((s (make-test-string))
         (s-upcase (string-upcase s)))
    (assert-false
     (loop for expected across s
           and actual across s-upcase
           when (char/= actual (char-upcase expected))
             collect (list (char-upcase (char-code expected))
                           (char-code actual))))))

(define-test string-downcase
    (:tag :issues)
  (let* ((s (make-test-string))
         (s-downcase (string-downcase s)))
    (assert-false
     (loop for expected across s
           and actual across s-downcase
           when (char/= actual (char-downcase expected))
             collect (list (char-downcase (char-code expected))
                           (char-code actual))))))
