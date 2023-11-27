;;; Tests for the functions in filesys.lisp.

(defpackage :filesys-tests
  (:use :cl :lisp-unit))

(in-package "FILESYS-TESTS")

;; These tests for unix-namestring come from the cmucl-help mailing
;; list, Sep 26, 2014 by Jared C. Davis

(define-test unix-namestring.1.exists
  ;; Make sure the desired directories exist.
  (assert-equal "/tmp/foo/bar/hello.txt"
		(ensure-directories-exist "/tmp/foo/bar/hello.txt"))
  (dolist (path '("/tmp/hello.txt"
		  "/tmp/foo/"
		  "/tmp/foo/hello.txt"
		  "/tmp/foo/bar/hello.txt"
		  "/tmp/foo/bar/bye.txt"
		  "/tmp/foo/bar/"
		  "/tmp/foo/bar/baz"
		  "/tmp/foo/bye.txt"
		  "/tmp/bye.txt"))
    (assert-equal path
		  (ext:unix-namestring path nil)
		  path)))

(define-test unix-namestring.1.non-existent
  ;; Make sure the desired directories exist.
  (assert-equal "/tmp/foo/bar/hello.txt"
		(ensure-directories-exist "/tmp/foo/bar/hello.txt"))
  ;; These paths contain directories that don't exist.
  (dolist (path '("/tmp/oops/"
		  "/tmp/oops/hello.txt"
		  "/tmp/foo/oops/hello.txt"
		  "/tmp/foo/bar/oops/hello.txt"
		  "/tmp/foo/oops/"
		  ))
    (assert-equal path
		  (ext:unix-namestring path nil)
		  path)))

(define-test unix-namestring.2
  ;; Make sure the desired directories exist.
  (assert-equal "/tmp/foo/bar/hello.txt"
		(ensure-directories-exist "/tmp/foo/bar/hello.txt"))
  (unwind-protect
       (progn
	 ;; Create a symlink loop
	 ;; ln -s /tmp/foo/bar/symlink /tmp/foo/
	 (unix:unix-unlink "/tmp/foo/bar/symlink")
	 (assert-equal t
		       (unix:unix-symlink "/tmp/foo/" "/tmp/foo/bar/symlink"))
	 (assert-equal "/tmp/foo/bar/symlink"
		       (ext:unix-namestring "/tmp/foo/bar/symlink" nil)))
    (unix:unix-unlink "/tmp/foo/bar/symlink")))

(define-test user-homedir.1
  "Test user-homedir"
  (:tag :issues)
  ;; Simple test to see if get-user-homedir-pathname returns the
  ;; expected value.  Use getuid and getpwuid to figure out what the
  ;; name and home directory should be.
  (let* ((uid (unix:unix-getuid))
         (user-info (unix:unix-getpwuid uid)))
    (assert-true uid)
    (assert-true user-info)
    (let* ((info-dir (unix:user-info-dir user-info))
           (info-name (unix:user-info-name user-info))
           (expected-home-pathname (pathname
                                    (concatenate 'string info-dir "/"))))
      (multiple-value-bind (home-pathname status)
          (ext:get-user-homedir-pathname info-name)
        (assert-true info-dir)
        (assert-true info-name)

        (assert-equal home-pathname expected-home-pathname)
        (assert-eql status 0)))))

(define-test user-homedir.2
  "Test user-homedir"
  (:tag :issues)
  ;; Simple test to see if get-user-homedir-pathname returns the expected
  ;; value for a user that does not exist.  Well, we assume such a
  ;; user doesn't exist.
      (multiple-value-bind (home-pathname status)
          (ext:get-user-homedir-pathname "zotuserunknown")
        (assert-eql home-pathname nil)
        (assert-eql status 0)))    
  
