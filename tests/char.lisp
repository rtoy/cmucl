;; Tests of char functions

(defpackage :char-tests
  (:use :cl :lisp-unit))

(in-package "CHAR-TESTS")

(define-test char-equal
  (:tag :issues)
  (let ((test-codes
          ;; Find all the codes where the CL lower case character
          ;; doesn't match the Unicode lowe case character.
          (loop for code from 128 below char-code-limit
               for ch = (code-char code)
               when (/= (char-code (char-downcase ch)) (or (lisp::unicode-lower code) code))
                 collect code)))
    (dolist (code test-codes)
      (assert-false (char-equal (code-char (lisp::unicode-lower code))
                                (code-char code))
                    code
                    (lisp::unicode-lower code)))))
