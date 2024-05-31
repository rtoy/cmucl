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
  (let* ((s
           ;; This string contains a couple of characters where
           ;; Unicode has a titlecase version of the character.  We
           ;; want to make sure we use char-upcase to capitalize the
           ;; string instead of lisp::char-titlecse
           (coerce
             '(#\Latin_Small_Letter_Dz
               #\a #\b
               #\space
               #\Latin_Small_Letter_Lj
               #\A #\B)
             'string))
         (expected
           ;; Manually convert S to a capitalized string using
           ;; char-upcase/downcase.
           (map 'string
                #'(lambda (ch f)
                    (funcall f ch))
                s
                (list #'char-upcase
                      #'char-downcase
                      #'char-downcase
                      #'char-downcase
                      #'char-upcase
                      #'char-downcase
                      #'char-downcase))))
    (assert-equal
     (map 'list #'char-name expected)
     (map 'list #'char-name (string-capitalize s)))))
