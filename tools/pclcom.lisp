
(in-package "USER")

(when (find-package "PCL")
  ;;
  ;; Undefine all generic functions exported from Lisp so that bootstrapping
  ;; doesn't get confused.
  (let ((class (find-class 'generic-function nil)))
    (when class
      (do-external-symbols (sym "LISP")
	(when (and (fboundp sym)
		   (typep (fdefinition sym) class))
	  (fmakunbound sym))
	(let ((ssym `(setf ,sym)))
	  (when (and (fboundp ssym)
		     (typep (fdefinition ssym) class))
	    (fmakunbound ssym))))))

  ;; Undefine all PCL classes, and clear CLASS-PCL-CLASS slots.
  (let ((wot (find-symbol "*FIND-CLASS*" "PCL")))
    (when (and wot (boundp wot))
      (do-hash (name ignore (symbol-value wot))
	(declare (ignore ignore))
	(let ((class (find-class name nil)))
	  (cond ((not class))
		((typep class 'kernel::std-class)
		 (setf (kernel:class-cell-class
			(kernel:find-class-cell name))
		       nil)
		 (setf (info type kind name) nil))
		(t
		 (setf (kernel:class-pcl-class class) nil)))))))

  (rename-package "PCL" "OLD-PCL")
  (make-package "PCL"))

(when (find-package  "SLOT-ACCESSOR-NAME")
  (rename-package "SLOT-ACCESSOR-NAME" "OLD-SLOT-ACCESSOR-NAME"))

(setf c:*suppress-values-declaration* t)
(pushnew :setf *features*)

(setf (search-list "pcl:") '("target:pcl/"))

(let ((obj (make-pathname :defaults "pcl:defsys"
			  :type (c:backend-fasl-file-type c:*backend*))))
  (when (< (or (file-write-date obj) 0)
	   (file-write-date "pcl:defsys.lisp"))
    (compile-file "pcl:defsys")))

(load "pcl:defsys" :verbose t)

(import 'kernel:funcallable-instance-p (find-package "PCL"))

(with-compilation-unit
    (:optimize '(optimize (debug #+small .5 #-small 2)
			  (speed 2) (safety #+small 0 #-small 2)
			  (inhibit-warnings 2))
     :optimize-interface '(optimize-interface #+small (safety 1))
     :context-declarations
     '((:external (declare (optimize-interface (safety 2) (debug 1))))
       (:macro (declare (optimize (speed 0))))))
 (pcl::compile-pcl))
