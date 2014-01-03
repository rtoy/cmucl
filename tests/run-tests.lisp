;;;; -*- Mode: lisp -*-

;;;; Main script to run all of the tests in the tests directory.
;;;; It is intended to be run using something like
;;;;
;;;;   lisp -load tests/run-tests.lisp -eval '(cmucl-test-runner:run-all-tests)'
;;;;
;;;; The exit code indicates whether there were any test failures.  A
;;;; non-zero code indicates a failure of some sort.
;;;;
;;;; It is assumed that either asdf or quicklisp is set up
;;;; appropriately so that lisp-unit can be automatically loaded

(defpackage :cmucl-test-runner
  (:use :cl)
  (:export #:*test-files*
	   #:*test-names*
	   #:load-test-files
	   #:run-loaded-tests
	   #:run-all-tests
	   #:print-test-results))

(in-package :cmucl-test-runner)

(require :lisp-unit)

;; Be rather verbose in printing the tests
(setf lisp-unit:*print-summary* t)
(setf lisp-unit:*print-failures* t)
(setf lisp-unit:*print-errors* t)

(defvar *load-path* *load-pathname*)

(defvar *test-files*
  nil)

(defvar *test-names*
  nil)

(defun load-test-files (&optional (test-directory #p"tests/"))
  (dolist (file (directory (merge-pathnames "*.lisp" test-directory)))
    (unless (equal file *load-path*)
      (let ((basename (pathname-name file)))
	(push (concatenate 'string (string-upcase basename) "-TESTS")
	      *test-names*)
	(push file *test-files*)
	(load file))))
  (setf *test-files* (nreverse *test-files*))
  (setf *test-names* (nreverse *test-names*)))

;; Look through all the files in the tests directory and load them.
;; Then run all of the tests.  For each file, it ia assumed that a
;; package is created that is named with "-TESTS" appended to he
;; pathname-name of the file.
(defun run-loaded-tests ()
  (let (test-results)
    (dolist (test *test-names*)
      (push (lisp-unit:run-tests :all test)
	    test-results))
    (nreverse test-results)))

(defun print-test-results (results)
  (let ((passed 0)
	(failed 0)
	(execute-errors 0)
	failed-tests
	execute-error-tests)
    (dolist (result results)
      (incf passed (lisp-unit::pass result))
      (incf failed (lisp-unit::fail result))
      (incf execute-errors (lisp-unit::exerr result))
      (when (lisp-unit::failed-tests result)
	(setf failed-tests
	      (append (lisp-unit::failed-tests result)
		      failed-tests)))
      (when (lisp-unit::error-tests result)
	(setf execute-error-tests
	      (append (lisp-unit::error-tests result)
		      execute-error-tests))))
    (format t "~2&-------------------------------------------------~%")
    (format t "Summary of all testsuites~2%")
    (format t "~D testsuites were run~%" (length results))
    (format t " ~5D tests total~%" (+ passed failed execute-errors))
    (format t " ~5D tests failed~%" failed)
    (format t " ~5D tests with execution errors~%" execute-errors)
    (format t "~5,2f% of the tests passed~%"
	    (float (* 100
		      (- 1 (/ (+ failed execute-errors)
			      (+ passed failed execute-errors))))))
    ;; Print some info about any failed tests.  Then exit.  We want to
    ;; set the exit code so that any scripts runnning this can
    ;; determine if there were any test failures.
    (cond ((plusp (+ failed execute-errors))
	   (when failed-tests
	     (format t "~2&Failed tests: ~S~%" failed-tests))
	   (when execute-error-tests
	     (format t "~2&Execute failures: ~S~%" execute-error-tests))
	   (unix:unix-exit 1))
	  (t
	   (unix:unix-exit 0)))))

(defun run-all-tests (&optional (test-directory #P"tests/"))
  (load-test-files test-directory)
  (print-test-results (run-loaded-tests)))

;;(run-all-tests)
;;(quit)
