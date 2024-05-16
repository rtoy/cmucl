;; Tests of string functions

(defpackage :string-tests
  (:use :cl :lisp-unit))

(in-package "STRING-TESTS")

(define-test string-upcase
    (:tag :issues)
  (let ((s (coerce (mapcar #'code-char
                           ;; Some special characters for testing.
                           ;; Micro_Sign shouldn't upcase.  #x1c5 and
                           ;; #x1c8 have a unicode category of Lt so
                           ;; they shouldn't upcase either.
                           '(#xb5 #x1c5 #x1c8))
                   'string)))
    ;; Verify that string-upcase returns the same characters as if we
    ;; did char-upcase on each one.  (This only works if we don't have
    ;; surrogate characters in the string!)
    (assert-equal (map 'list #'(lambda (c)
                                 (char-name (char-upcase c)))
                       s)
                  (map 'list #'char-name
                       (string-upcase s)))))

(define-test string-downcase
    (:tag :issues)
  (let ((s (coerce (mapcar #'code-char
                           ;; Some special characters for testing.
                           ;; Micro_Sign shouldn't upcase.  #x1c5 and
                           ;; #x1c8 have a unicode category of Lt so
                           ;; they shouldn't upcase either.
                           '(#xb5 #x1c5 #x1c8))
                   'string)))
    ;; Verify that string-downcase returns the same characters as if we
    ;; did char-downcase on each one.  (This only works if we don't have
    ;; surrogate characters in the string!)
    (assert-equal (map 'list #'(lambda (c)
                                 (char-name (char-downcase c)))
                       s)
                  (map 'list #'char-name (string-downcase s)))))
