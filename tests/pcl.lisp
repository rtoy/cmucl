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

(asdf:defsystem :pcl-test
  :pathname "pcl/"
  :components
  ((:file "pkg")
   #+gerds-pcl
   (:file "ctor"
    :depends-on ("pkg"))
   (:file "defclass"
    :depends-on ("pkg"))
   (:file "make-instance"
    :depends-on ("pkg" #+gerds-pcl "ctor"))
   (:file "reinitialize-instance"
    :depends-on ("pkg" "make-instance"))
   (:file "slot-value"
    :depends-on ("pkg" "make-instance"))
   (:file "slot-boundp"
    :depends-on ("pkg" "make-instance"))
   (:file "slot-missing"
    :depends-on ("pkg" "make-instance"))
   (:file "slot-accessors"
    :depends-on ("pkg" "make-instance"))
   (:file "slot-type"
    :depends-on ("pkg" "slot-value"))
   (:file "inline-access"
    :depends-on ("pkg" "slot-type"))
   (:file "method-combination"
    :depends-on ("pkg"))
   (:file "pv"
    :depends-on ("pkg"))
   (:file "defgeneric"
    :depends-on ("pkg"))
   (:file "defmethod"
    :depends-on ("pkg"))
   (:file "find-method"
    :depends-on ("pkg"))
   (:file "methods"
    :depends-on ("pkg"))))

(asdf:oos 'asdf:load-op :pcl-test)
