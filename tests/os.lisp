(defpackage :os-tests
  (:use :cl :lisp-unit))

(in-package "OS-TESTS")

(define-test user-homedir
  "Test user-homedir"
  (:tag :issues)
  ;; Simple test to see if unix-get-user-homedir returns the expected
  ;; value.  Use getuid and getpwuid to figure out what the name and
  ;; home directory should be.
  (let* ((uid (unix:unix-getuid))
         (user-info (unix:unix-getpwuid uid)))
    (assert-true uid)
    (assert-true user-info)
    (let* ((info-dir (unix:user-info-dir user-info))
           (info-name (unix:user-info-name user-info))
           (expected-home-pathname (pathname
                                    (concatenate 'string info-dir "/")))
           (home-pathname (unix:unix-get-user-homedir info-name)))
      (assert-true info-dir)
      (assert-true info-name)

      (assert-equal home-pathname expected-home-pathname))))
