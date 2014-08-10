;;;; -*- Lisp -*-

(require :gray-streams)

(defpackage gray-streams-tests
  (:use #:common-lisp #:lisp-unit))

(in-package #:gray-streams-tests)

(defparameter *test-path*
  (merge-pathnames (make-pathname :name :unspecific :type :unspecific
                                  :version :unspecific)
                   *load-truename*)
  "Directory for temporary test files.")

(defparameter *test-file*
  (merge-pathnames #p"test-data.tmp" *test-path*))

(eval-when (:load-toplevel)
  (ensure-directories-exist *test-path* :verbose t))

(define-test clear-output-1
  (:tag :trac)
  ;; Create a Gray stream and make sure that clear-output works.
  (assert-eql
   0
   (let ((s (open *test-file*
		  :direction :output
		  :if-exists :supersede
		  :class 'lisp::character-output-stream)))
     (unwind-protect
	  (progn
	    (write-char #\a s)
	    (clear-output s)
	    (close s)
	    (setf s (open *test-file*))
	    (file-length s))
       (close s)
       (delete-file *test-file*)))))
