;;; -*- Mode: lisp -*-

(asdf:defsystem :pcl-tests
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
