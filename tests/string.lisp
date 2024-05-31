;; Tests of string functions

(defpackage :string-tests
  (:use :cl :lisp-unit))

(in-package "STRING-TESTS")

(defun make-test-string ()
  ;; Create a string consisting of all the code units EXCEPT for the
  ;; surrogates because string casing handles that differently.
  (let ((s (make-string char-code-limit)))
    (dotimes (k char-code-limit)
      (setf (aref s k) (code-char k)))
    s))

(define-test string-upcase
    (:tag :issues)
  (let* ((s (make-test-string))
         (s-upcase (string-upcase s)))
    (assert-false
     (loop for expected across s
           and actual across s-upcase
           when (char/= actual (char-upcase expected))
             collect (list (char-upcase expected)
                           (char-code actual))))))

(define-test string-downcase
    (:tag :issues)
  (let* ((s (make-test-string))
         (s-downcase (string-downcase s)))
    (assert-false
     (loop for expected across s
           and actual across s-downcase
           when (char/= actual (char-downcase expected))
             collect (list (char-downcase expected)
                           (char-code actual))))))

(define-test nstring-upcase
    (:tag :issues)
  (let* ((s (make-test-string))
         (ns (make-test-string))
         (ns-upcase (nstring-upcase ns)))
    (assert-false
     (loop for expected across s
           and actual across ns
           when (char/= actual (char-upcase expected))
             collect (list (char-upcase expected)
                           (char-code actual))))))

(define-test nstring-downcase
    (:tag :issues)
  (let* ((s (make-test-string))
         (ns (make-test-string))
         (ns-downcase (nstring-downcase ns)))
    (assert-false
     (loop for expected across s
           and actual across ns
           when (char/= actual (char-downcase expected))
             collect (list (char-downcase expected)
                           (char-code actual))))))
