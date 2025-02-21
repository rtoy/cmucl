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

(define-test clear-output-1
  (:tag :trac)
  (assert-eql
   0
   (ext:with-temporary-file (test-file)
     (let ((s (open test-file
		    :direction :output
		    :if-exists :supersede)))
       ;; Write a character to the (fully buffered) output
       ;; stream. Clear the output and close the file. Nothing
       ;; should have been written to the file.
       (write-char #\a s)
       (clear-output s)
       (close s)
       (setf s (open test-file))
       (file-length s)))))
