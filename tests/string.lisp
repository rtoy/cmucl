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

(define-test string-capitalize
    (:tag :issues)
  (let* ((s (string-capitalize
             ;; #\Latin_Small_Letter_Dz and #\Latin_Small_Letter_Lj
             ;; #have Unicode titlecase characters that differ from
             ;; CHAR-UPCASE.  This tests that STRING-CAPITALIZE use
             ;; CHAR-UPCASE to produce the capitalization.
             (coerce
              '(#\Latin_Small_Letter_Dz
                #\a #\b
                #\space
                #\Latin_Small_Letter_Lj
                #\A #\B)
              'string)))
         (expected
           ;; Manually convert the test string by calling CHAR-UPCASE
           ;; or CHAR-DOWNCASE (or IDENTITY) to get the desired
           ;; capitalized string.
           (map 'list
                #'(lambda (c f)
                    (funcall f c))
                s
                (list #'char-upcase
                      #'char-downcase
                      #'char-downcase
                      #'identity
                      #'char-upcase
                      #'char-downcase
                      #'char-downcase))))
    (assert-equal
     (map 'list #'char-name expected)
     (map 'list #'char-name s))))
