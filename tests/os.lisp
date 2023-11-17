(defpackage :os-tests
  (:use :cl :lisp-unit))

(in-package "OS-TESTS")

(define-test user-homedir
  "Test user-homedir"
  (:tag :issues)
  ;; Simple test to see if unix-get-user-homedir returns the expected
  ;; value.  We assume the envvar USERNAME and HOME exist and are
  ;; correctly set up for the user running this test.
  (let ((user-name (unix:unix-getenv "USERNAME")))
    (assert-true user-name)
    (when user-name
      (let ((expected-homedir (pathname
                               (concatenate 'string
                                            (unix:unix-getenv "HOME")
                                            "/")))
            (homedir (unix:unix-get-user-homedir user-name)))
        (assert-true expected-homedir)
        (assert-equal homedir expected-homedir)))))
