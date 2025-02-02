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

(define-test mkstemp.name-returned.2
  (:tag :issues)
  (let ((unix::*filename-encoding* :utf-8)
	fd name)
    (unwind-protect
	 (progn
	   ;; Temp name starts with a lower case alpha character.
	   (let* ((template (concatenate 'string (string #\u+3b1)
					 "test-XXXXXX"))
		  (x-posn (position #\X template)))
	     (multiple-value-setq (fd name)
	       (unix::unix-mkstemp template))
	     (assert-true fd)
	     (assert-false (search "XXXXXX" name)
			   name)
	     (assert-true (string= name template :end1 x-posn :end2 x-posn)
			  name)))
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
      (unix::unix-mkstemp "test-")
    ;; The template doesn't have enough X's so the FD should be NIL,
    ;; and a positive Unix errno value should be returned.
    ;;
    ;; Note that Darwin allows any number of X's in the template but
    ;; Linux requires exactly 6.  Just test with no X's to handle all
    ;; OSes.
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

(define-test mkdtemp.name-returned.2
  (:tag :issues)
  (let ((unix::*filename-encoding* :utf-8)
	name)
    (unwind-protect
	 (progn
	   ;; Temp name starts with a lower case alpha character.
	   (let* ((template (concatenate 'string (string #\u+3b1)
					 "dir-XXXXXX"))
		  (x-posn (position #\X template)))
	     (setf name (unix::unix-mkdtemp template))
	     ;; Verify that the dir name no longer has X's.
	     (assert-true (stringp name))
	     (assert-false (search "XXXXXX" name))
	     (assert-true (string= name template :end1 x-posn :end2 x-posn)
			  name x-posn)))
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
      (unix::unix-mkdtemp "dir-")
    ;; No X's in template, like for mkstemp.bad-template test.
    (assert-false result)
    (assert-true (and (integerp errno) (plusp errno)))))

