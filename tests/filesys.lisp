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

	 
    
  
