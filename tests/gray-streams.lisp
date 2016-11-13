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

(defparameter *test-file-2*
  (merge-pathnames #P"test-data-gray.tmp" *test-path*))

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

(define-test format-abs-stream-advance
  (:tag :trac)
  ;; Create a lisp stream and a Gray stream and test format ~T on
  ;; each. Compare the length of each file and declare success if the
  ;; lengths are the same.
  ;;
  ;; FIXME: This doesn't actually test that STREAM-ADVANCE-TO-COLUMN
  ;; was actually called. Another test should be added for that. We're
  ;; testing functionality here. It was verified manually using TRACE
  ;; that FORMAT on a Gray stream does in fact call
  ;; STREAM-ADVANCE-TO-COLUMN
  (assert-equal "18 18"
   (let ((lisp-stream (open *test-file*
			    :direction :output
			    :if-exists :supersede))
	 (gray-stream (open *test-file-2*
			    :direction :output
			    :if-exists :supersede
			    :class 'lisp::character-output-stream)))
     (unwind-protect
	  (progn
	    (format lisp-stream "~10T")
	    (format lisp-stream "~8,10T")
	    (format gray-stream "~10T")
	    (format gray-stream "~8,10T")
	    (force-output lisp-stream)
	    (force-output gray-stream)
	    (format nil "~D ~D"
		    (file-position lisp-stream)
		    (file-position gray-stream)))
       (close lisp-stream)
       (close gray-stream)
       (delete-file *test-file*)
       (delete-file *test-file-2*)))))

(define-test format-rel-stream-advance
  (:tag :trac)
  ;; Create a lisp stream and a Gray stream and test format ~@T on
  ;; each. Compare the length of each file and declare success if the
  ;; lengths are the same.
  ;;
  ;; FIXME: This doesn't actually test that STREAM-ADVANCE-TO-COLUMN
  ;; was actually called. Another test should be added for that. We're
  ;; testing functionality here. It was verified manually using TRACE
  ;; that FORMAT on a Gray stream does in fact call
  ;; STREAM-ADVANCE-TO-COLUMN
  (assert-equal "20 20"
   (let ((lisp-stream (open *test-file*
			    :direction :output
			    :if-exists :supersede))
	 (gray-stream (open *test-file-2*
			    :direction :output
			    :if-exists :supersede
			    :class 'lisp::character-output-stream)))
     (unwind-protect
	  (progn
	    (format lisp-stream "~10T")
	    (format lisp-stream "~8,10@T")
	    (format gray-stream "~10T")
	    (format gray-stream "~8,10@T")
	    (force-output lisp-stream)
	    (force-output gray-stream)
	    (format nil "~D ~D" 
		    (file-position lisp-stream)
		    (file-position gray-stream)))
       (close lisp-stream)
       (close gray-stream)
       (delete-file *test-file*)
       (delete-file *test-file-2*)))))
