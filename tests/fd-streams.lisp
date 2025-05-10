;;;; -*- Lisp -*-

(defpackage fd-streams-tests
  (:use #:common-lisp #:lisp-unit))

(in-package #:fd-streams-tests)

(defparameter *test-path*
  (merge-pathnames (make-pathname :name :unspecific :type :unspecific
                                  :version :unspecific)
                   *load-truename*)
  "Directory for temporary test files.")

(defparameter *test-file*
  (merge-pathnames #p"test-data.tmp" *test-path*))

(eval-when (:load-toplevel)
  (ensure-directories-exist *test-path* :verbose t))

#+nil
(define-test clear-output-1
  (:tag :trac)
  (assert-eql
   0
   (unwind-protect
	(let ((s (open *test-file*
		       :direction :output
		       :if-exists :supersede)))
	  ;; Write a character to the (fully buffered) output
	  ;; stream. Clear the output and close the file. Nothing
	  ;; should have been written to the file.
	  (write-char #\a s)
	  (clear-output s)
	  (close s)
	  (setf s (open *test-file*))
	  (file-length s))
     (delete-file *test-file*))))

(define-test file-position.1
    (:tag :issues)
  ;; Create a short test file
  (let ((test-file (merge-pathnames #p"file-pos.txt" *test-path*)))
    (with-open-file (s test-file
		       :direction :output
		       :if-exists :supersede)
      (write-string "aaaaaa" s)
      (write-char #\newline s))
    (with-open-file (s test-file)
      (read-line s)
      (assert-true (file-position s 0))
      (assert-equal (file-position s) 0))))

(define-test file-position.2
    (:tag :issues)
  ;; Create a test file just longer than the internal in-buffer length
  ;; and the first line is more than 512 characters long.
  (let ((test-file (merge-pathnames #p"file-pos.txt" *test-path*)))
    (with-open-file (s test-file
		       :direction :output
		       :if-exists :supersede)
      (write-string (make-string 512 :initial-element #\a) s)
      (write-char #\newline s)
      (write-string "zzzzz" s)
      (write-char #\newline s))
    (with-open-file (s test-file)
      (read-line s)
      (assert-true (file-position s 0))
      (assert-equal (file-position s) 0))))

(define-test file-position.3
    (:tag :issues)
  ;; Create a test file just longer than the internal in-buffer
  ;; length.  This tests the case where the in-buffer does not have
  ;; enough octets to form a complete character.  (See comment in
  ;; fd-stream-file-position.
  (let ((test-file (merge-pathnames #p"file-pos.txt" *test-path*)))
    (with-open-file (s test-file
		       :external-format :utf-8
		       :direction :output
		       :if-exists :supersede)
      (write-char #\a s)
      ;; STR is a string consisting of the single codepoint #x11000
      ;; which is 4 octets when encoded using utf-8.
      (let ((str (lisp::codepoints-string '(#x11000))))
	(dotimes (k 128)
	  (write-string str s)))
      (write-char #\newline s)
      (write-string "zzzzz" s)
      (write-char #\newline s))
    (with-open-file (s test-file :external-format :utf-8)
      (read-line s)
      (assert-true (file-position s 0))
      (assert-equal (file-position s) 0))))
