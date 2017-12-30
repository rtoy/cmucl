;;; Tests for PCL, taken from src/pcl/rt.

(defpackage "PCL-TESTS"
  (:use "COMMON-LISP" "PCL" "LISP-UNIT"))

(in-package "PCL-TESTS")

;; Simple macro converting RT's DEFTEST to lisp-unit's DEFINE-TEST.
(defmacro deftest (name form &rest values)
  (let ((results (gensym "RESULTS-")))
    `(define-test ,name
	 (:tag :pcl)
	 (let ((,results (multiple-value-list ,form)))
	   (assert-equalp ,results
			  ',values)))))

(defmacro define-compiled-test (name form &rest values)
  `(progn
     (defun ,name () ,form)
     (deftest ,name (,name) ,@values)))

(require :asdf)

(setf (logical-pathname-translations "pcl-test")
      (list (list "*.*.*"
		  (merge-pathnames #p"pcl/*.*"
				   *load-truename*))))

;; Just smash it with our path so we don't accidentally look up
;; pcl-files somewhere else.
(setf asdf:*central-registry*
      (list (make-pathname :directory (pathname-directory *load-pathname*))))

(require :pcl-tests)

(asdf:oos 'asdf:load-op :pcl-tests)
