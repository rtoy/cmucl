
(in-package "USER")

(setf c:*suppress-values-declaration* t)

(setf (search-list "pcl:") '("target:pcl/"))

(let ((obj (make-pathname :defaults "pcl:defsys"
			  :type (c:backend-fasl-file-type c:*backend*))))
  (when (< (or (file-write-date obj) 0)
	   (file-write-date "pcl:defsys.lisp"))
    (compile-file "pcl:defsys")))

(load "pcl:defsys" :verbose t)

(import 'kernel:funcallable-instance-p (find-package "PCL"))

(with-compilation-unit
    (:optimize '(optimize (debug-info #+small .5 #-small 2)
			  (speed 2)
			  (inhibit-warnings 2))
     :context-declarations
     '((:external (declare (optimize-interface (safety 2) (debug-info 1))))))
 (pcl::compile-pcl))
