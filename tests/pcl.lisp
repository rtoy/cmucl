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

(require :defsystem)

(setf (logical-pathname-translations "pcl-test")
      (list (list "*.*.*"
		  (merge-pathnames #p"pcl/*.*"
				   *load-truename*))))

(mk:defsystem :pcl-test
    :initially-do (progn )
    :source-pathname "pcl-test:"
    :binary-pathname "pcl-test:"
    :components
    ((:file "pkg"
	    :source-extension "lisp")
     #+gerds-pcl
     (:file "ctor"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "defclass"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "make-instance"
	    :source-extension "lisp"
	    :depends-on ("pkg" #+gerds-pcl "ctor"))
     (:file "reinitialize-instance"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-value"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-boundp"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-missing"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-accessors"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-type"
	    :source-extension "lisp"
	    :depends-on ("pkg" "slot-value"))
     (:file "inline-access"
	    :source-extension "lisp"
	    :depends-on ("pkg" "slot-type"))
     (:file "method-combination"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "pv"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "defgeneric"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "defmethod"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "find-method"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "methods"
	    :source-extension "lisp"
	    :depends-on ("pkg"))))

(mk:oos :pcl-test :compile)
