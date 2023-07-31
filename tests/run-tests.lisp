;;;; -*- Mode: lisp -*-

;;;; Main script to run all of the tests in the tests directory.
;;;; It is intended to be run using something like
;;;;
;;;;   lisp -noinit -load tests/run-tests.lisp -eval '(cmucl-test-runner:run-all-tests)'
;;;;
;;;;
;;;; To run selected tests:
;;;;
;;;;   lisp -noinit -load tests/run-tests.lisp -eval '(progn (cmucl-test-runner:load-test-files) (cmucl-test-runner:run-test <list>))'
;;;;
;;;; where <list> is a list of the test names such as "ISSUES-TESTS",
;;;; "IRRAT-TESTS", etc.  The test names are basically the file name
;;;; with a suffix of "-TESTS".
;;;;
;;;; Note that you cannot run these tests from a binary created during
;;;; a build process. You must run
;;;;
;;;;   bin/make-dist.sh -I inst-dir build-dir
;;;;
;;;; to install everything in some temporary directory. This is needed
;;;; because the simple-streams test needs to load simple-streams, and
;;;; the build directory isn't set up for that.
;;;;
;;;; The exit code indicates whether there were any test failures.  A
;;;; non-zero code indicates a failure of some sort.
;;;;

(defpackage :cmucl-test-runner
  (:use :cl)
  (:export #:*test-files*
	   #:*test-names*
	   #:load-test-files
	   #:run-loaded-tests
	   #:run-all-tests
	   #:run-test
	   #:print-test-results))

(in-package :cmucl-test-runner)

(require :asdf)
(require :lisp-unit)

;; Be rather verbose in printing the tests
(setf lisp-unit:*print-summary* t)
(setf lisp-unit:*print-failures* t)
(setf lisp-unit:*print-errors* t)

(defvar *load-path* (truename *load-pathname*))

(defvar *test-files*
  nil)

(defvar *test-names*
  nil)

;; Look through all the files in the TEST-DIRECTORY and load them.
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

;; Run all of the tests in *TEST-NAMES*.  Return a list of all of the
;; lisp-unit results for each of the test sets.
(defun run-loaded-tests ()
  (let (test-results)
    (dolist (test *test-names*)
      (push (lisp-unit:run-tests :all test)
	    test-results))
    (nreverse test-results)))

;; Run selected tests
(defun run-test (&rest tests)
  (let (test-results)
    (dolist (test tests)
      (push (lisp-unit:run-tests :all test)
	    test-results))
    (print-test-results (nreverse test-results) :verbose t)))

;; Print out a summary of test results produced from RUN-LOADED-TESTS.
(defun print-test-results (results &key verbose exitp)
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
    (format t "~5,3f% of the tests passed~%"
	    (let ((total (+ passed failed execute-errors)))
	      (if (zerop total)
		  0.0
		  (* 100.0 (- 1.0 (/ (- total passed) total))))))
    ;; Print some info about any failed tests.  Then exit.  We want to
    ;; set the exit code so that any scripts runnning this can
    ;; determine if there were any test failures.
    (cond ((plusp (+ failed execute-errors))
	   (when failed-tests
	     (format t "~2&Failed tests: ~S~%" failed-tests)
	     (dolist (result results)
	       (lisp-unit:print-failures result)))
	   (when execute-error-tests
	     (format t "~2&Execute failures: ~S~%" execute-error-tests)
	     (dolist (result results)
	       (lisp-unit:print-errors result)))
	   (when exitp
	     (unix:unix-exit 1)))
	  (t
	   (when exitp
	     (unix:unix-exit 0))))))

;; Look through all the files in the TEST-DIRECTORY and load them.
;; Then run all of the tests.  For each file, it ia assumed that a
;; package is created that is named with "-TESTS" appended to he
;; pathname-name of the file.
(defun run-all-tests (&key (test-directory #P"tests/") (verbose t))
  (load-test-files test-directory)
  (print-test-results (run-loaded-tests) :verbose t :exitp t))

;;(run-all-tests)
;;(quit)
