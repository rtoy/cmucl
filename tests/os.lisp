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

#+linux
(define-test stat.64-bit-timestamp-2038
    (:tag :issues)
  (let ((test-file #.(merge-pathnames "resources/64-bit-timestamp-2038.txt"
				      cl:*load-pathname*)))
    (assert-true (probe-file test-file))
    (multiple-value-bind (ok st-dev st-ino st-mode st-nlink st-uid st-gid st-rdev st-size
			  st-atime st-mtime
			  st-ctime st-blksize st-blocks)
	(unix:unix-stat (namestring test-file))
      (declare (ignore st-dev st-ino st-mode st-nlink st-uid st-gid st-rdev st-size
		       st-ctime st-blksize st-blocks))
      (assert-true ok)
      (assert-equal 2153718000 st-atime)
      (assert-equal 2153718000 st-mtime))))

#+linux
(define-test stat.64-bit-timestamp-2106
    (:tag :issues)
  (let ((test-file #.(merge-pathnames "resources/64-bit-timestamp-2106.txt"
				      cl:*load-pathname*)))
    (assert-true (probe-file test-file))
    (multiple-value-bind (ok st-dev st-ino st-mode st-nlink st-uid st-gid st-rdev st-size
			  st-atime st-mtime
			  st-ctime st-blksize st-blocks)
	(unix:unix-stat (namestring test-file))
      (declare (ignore st-dev st-ino st-mode st-nlink st-uid st-gid st-rdev
		       st-ctime st-blksize st-blocks))
      (assert-true ok)
      (assert-equal 4299548400 st-atime)
      (assert-equal 4299548400 st-mtime))))
