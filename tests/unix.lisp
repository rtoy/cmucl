;;; Tests for the unix interface

(defpackage :unix-tests
  (:use :cl :lisp-unit))

(in-package "UNIX-TESTS")

(define-test mkstemp.name-returned
  (:tag :issues)
  (let (fd name)
    (unwind-protect
	 (progn
	   (multiple-value-setq (fd name)
	     (unix::unix-mkstemp "test-XXXXXX"))
	   (assert-true fd)
	   (assert-false (search "XXXXXX" name)))
      (when fd
	(unix:unix-unlink name)))))

(define-test mkstemp.bad-path
  (:tag :issues)
  (multiple-value-bind (fd errno)
      ;; Assumes that the directory "random-dir" doesn't exist
      (unix::unix-mkstemp "random-dir/test-XXXXXX")
    ;; Can't create and open the file so the FD should be NIL, and a
    ;; positive Unix errno value should be returned.
    (assert-false fd)
    (assert-true (and (integerp errno) (plusp errno)))))

(define-test mkstemp.bad-template
  (:tag :issues)
  (multiple-value-bind (fd errno)
      (unix::unix-mkstemp "test-XXXXX")
    ;; The template doesn't have enough X's so the FD should be NIL,
    ;; and a positive Unix errno value should be returned.
    (assert-false fd)
    (assert-true (and (integerp errno) (plusp errno)))))

(define-test mkdtemp.name-returned
  (:tag :issues)
  (let (name)
    (unwind-protect
	 (progn
	   (setf name (unix::unix-mkdtemp "dir-XXXXXX"))
	   ;; Verify that the dir name no longer has X's.
	   (assert-true (stringp name))
	   (assert-false (search "XXXXXX" name)))
      (when name
	(unix:unix-rmdir name)))))

(define-test mkdtemp.bad-path
  (:tag :issues)
  (multiple-value-bind (result errno)
      (unix::unix-mkdtemp "random-dir/dir-XXXXXX")
    (assert-false result)
    (assert-true (and (integerp errno) (plusp errno)))))

(define-test mkdtemp.bad-template
  (:tag :issues)
  (multiple-value-bind (result errno)
      (unix::unix-mkdtemp "dir-XXXXX")
    (assert-false result)
    (assert-true (and (integerp errno) (plusp errno)))))

