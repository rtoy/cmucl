;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/pclcom.lisp,v 1.19 1999/01/09 11:05:20 dtc Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "USER")

(when (find-package "PCL")
  ;; Load the lisp:documentation functions.
  (load "target:code/misc")

  ;;
  ;; Blow away make-instance optimizer so that it doesn't confuse
  ;; bootstrapping.
  (setf (compiler-macro-function 'make-instance) nil)
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

  ;; Rename the PCL package to OLD-PCL, then restoring pcl::class and
  ;; pcl::..slot-unbound.. back to the PCL package as they need be
  ;; consistent with the symbols recognised by the compiler.
  (let ((class 'pcl::class)
	(slot-unbound 'pcl::..slot-unbound..))
    (rename-package "PCL" "OLD-PCL")
    (make-package "PCL")
    (shadowing-import class "PCL")
    (kernel:%set-symbol-package class (find-package "PCL"))
    (import slot-unbound "PCL")
    (kernel:%set-symbol-package slot-unbound (find-package "PCL"))))

(when (find-package  "SLOT-ACCESSOR-NAME")
  (rename-package "SLOT-ACCESSOR-NAME" "OLD-SLOT-ACCESSOR-NAME"))

(when (find-package "CLOS-MOP")
  (rename-package "CLOS-MOP" "OLD-CLOS-MOP"))

(setf c:*suppress-values-declaration* t)
(pushnew :setf *features*)

(setf (search-list "pcl:") '("target:pcl/"))

(let ((obj (make-pathname :defaults "pcl:defsys"
			  :type (c:backend-byte-fasl-file-type c:*backend*))))
  (when (< (or (file-write-date obj) 0)
	   (file-write-date "pcl:defsys.lisp"))
    (compile-file "pcl:defsys" :byte-compile t)))

(load "pcl:defsys" :verbose t)

(import 'kernel:funcallable-instance-p (find-package "PCL"))

(with-compilation-unit
    (:optimize '(optimize (debug #+small .5 #-small 2)
			  (speed 2) (safety #+small 0 #-small 2)
			  (inhibit-warnings 2))
     :optimize-interface '(optimize-interface #+small (safety 1))
     :context-declarations
     '((:external (declare (optimize-interface (safety 2) (debug 1))))
       ((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
	(declare (optimize (speed 0))))))
 (pcl::compile-pcl))
