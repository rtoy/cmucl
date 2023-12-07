(defpackage :os-tests
  (:use :cl :lisp-unit))

(in-package "OS-TESTS")


(define-test user-homedir.1
  "Test user-homedir"
  (:tag :issues)
  ;; Simple test to see if get-user-homedir-namestring returns the
  ;; expected value.  Use getuid and getpwuid to figure out what the
  ;; name and home directory should be.
  (let* ((uid (unix:unix-getuid))
         (user-info (unix:unix-getpwuid uid)))
    (assert-true uid)
    (assert-true user-info)
    (let* ((info-dir (unix:user-info-dir user-info))
           (info-name (unix:user-info-name user-info)))
      (multiple-value-bind (home-namestring status)
          (system:get-user-homedir-namestring info-name)
        (assert-true info-dir)
        (assert-true info-name)

        (assert-equal home-namestring info-dir)
        (assert-eql status 0)))))

(define-test user-homedir.2
  "Test user-homedir"
  (:tag :issues)
  ;; Simple test to see if get-user-homedir-pathname returns the expected
  ;; value for a user that does not exist.  Well, we assume such a
  ;; user doesn't exist.
      (multiple-value-bind (home-pathname status)
          (system:get-user-homedir-namestring "zotuserunknown")
        (assert-eql home-pathname nil)
        (assert-eql status 0)))
