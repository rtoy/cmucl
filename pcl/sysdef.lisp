;;; -*- Mode: Lisp; Base: 10; Syntax: Common-Lisp; Package: DSYS -*-
;;; File: sysdef.lisp 
;;; Author: Richard Harris
;;;
;;;	ROSE - Rensselaer Object System for Engineering
;;;	Common Lisp Implementation
;;;
;;; 			   Copyright (c) 1990 by 
;;; 	      Rensselaer Polytechnic Institute, Troy, New York.
;;; 			    All Rights Reserved
;;;
;;;	THE SOFTWARE AND ACCOMPANYING WRITTEN MATERIALS ARE PROVIDED
;;;	\"AS IS\" AND WITHOUT ANY WARRANTY, INCLUDING BUT NOT LIMITED 
;;;	TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;	A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND
;;;	PERFORMANCE OF THE SOFTWARE AND USE OF THE ACCOMPANYING WRITTEN
;;;	MATERIALS IS ASSUMED BY YOU.  IN NO EVENT SHALL RENSSELAER 
;;;	POLYTECHNIC INSTITUTE BE LIABLE FOR ANY LOST REVENUE, LOST 
;;;	PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES, EVEN
;;;	IF ADVISED OF THE POSSIBILITIES OF SUCH DAMAGES, WHERE DAMAGES
;;;	ARISE OUT OF OR IN CONNECTION WITH THE USE OF, PERFORMANCE OR
;;;	NONPERFORMANCE OF THIS SOFTWARE.
;;;
;;;	This software and accompanying written materials may not be 
;;;	distributed outside your organization or outside the United 
;;;	States of America without express written authorization from
;;;	Rensselaer Polytechnic Institute.
;;;
;;;	This work has been sponsored in part by Defense Advanced Research 
;;;	Projects Agency (DARPA) under contract number MDA972-88-C0047 for
;;; 	DARPA Initiative in Concurrent Engineering (DICE).  This material
;;;	may be reproduced by or for the U.S. Government pursuant to the 
;;;	copyright license under the clause at DFARS 252.227-7013 7/26/90.
;;; 

(in-package "DSYS")

(defvar *pcl-compiled-p* nil)
(defvar *pcl-loaded-p* nil)

(defun pcl-sym (sym)
  (intern (string sym) (or (find-package :pcl)
			   (make-package :pcl :use '(:lisp)))))

(defun kill-pcl-package ()
  (if (and (find-package "PCL")
	   (fboundp (pcl-sym "RESET-PCL-PACKAGE")))
      (funcall (pcl-sym "RESET-PCL-PACKAGE"))
      (rename-package "PCL" (symbol-name (gensym))))
  (let ((defsys (subfile '("pcl") :name "defsys")))
    (set (pcl-sym "*PCL-DIRECTORY*") defsys)
    (load-file defsys))
  (mapc #'(lambda (path)
	    (setf (lfi-fwd (get-loaded-file-info path)) 0))
	(pcl-binary-files)))

(defun pcl-binary-files ()
  (funcall (pcl-sym 'system-binary-files) (pcl-sym 'pcl)))

(defun maybe-load-defsys (&optional compile-defsys-p)
  (let ((defsys (subfile '("pcl") :name "defsys"))
	(*use-default-pathname-type* nil)
	(*skip-load-if-loaded-p* t)
	(*skip-compile-file-fwd* 0))
    (set (pcl-sym "*PCL-DIRECTORY*") defsys)
    (when compile-defsys-p
      (compile-file defsys))
    (let ((b-s (pcl-sym '*boot-state*)))
      (when (and (boundp b-s) (symbol-value b-s))
	#+ignore (kill-pcl-package)))
    (load-file defsys)))  

(defun maybe-load-pcl (&optional force-p compile-defsys-p)
  (unless (and (null force-p)
	       (fboundp (pcl-sym 'get-system))
	       (every #'(lambda (path)
			  (let* ((path-fwd (file-write-date path))
				 (lfi (get-loaded-file-info path)))
			    (and lfi path-fwd (= path-fwd (lfi-fwd lfi)))))
		      (pcl-binary-files)))
    (maybe-load-defsys compile-defsys-p)
    (funcall (pcl-sym 'load-pcl))))

(defsystem pcl
    (:pretty-name "PCL")
  #+akcl
  (:forms 
   :compile (let ((cfn (merge-pathnames "../cmpnew/collectfn.lisp" 
					si::*system-directory*)))
	      (unless (probe-file cfn)
		(run-unix-command 
		 (format nil "ln ~A ~A"
			 (namestring (make-pathname :defaults cfn :type "lsp"))
			 (namestring cfn))))))
  #+akcl
  #.(merge-pathnames "../cmpnew/collectfn" si::*system-directory*)
  #+akcl
  #.(merge-pathnames "../lsp/sys-proclaim" si::*system-directory*)
  (:forms 
   :compile
   (progn
     (maybe-load-defsys t)
     (if (and (fboundp (pcl-sym 'operation-transformations))
	      (every #'(lambda (trans)
			 (eq (car trans) :load))
		     (funcall (pcl-sym 'operation-transformations)
			      (pcl-sym 'pcl) :compile)))
	 (maybe-load-pcl)
	 (let ((b-s (pcl-sym '*boot-state*)))
	   (when (and (boundp b-s) (symbol-value b-s))
	     (kill-pcl-package))
	   #+akcl (compiler::emit-fn t)
	   (#+cmu with-compilation-unit #-cmu progn
	    #+cmu (:optimize 
		   '(optimize (user::debug-info #+small .5 #-small 2)
		              (speed #+testing 1 #-testing 2)
		              (safety #+testing 3 #-testing 0)
		              #+ignore (user::inhibit-warnings 2))
		   :context-declarations
		   '(#+ignore
		     (:external (declare (user::optimize-interface 
					  (safety 2) (debug-info 1))))))
	     #+testing
	     (proclaim '(optimize (speed 1) (safety 3) #+lucid (compilation-speed 3)))
	     #-testing
	     (proclaim '(optimize (speed 3) (safety 0) #+lucid (compilation-speed 0)))
	     (funcall (pcl-sym "COMPILE-PCL")))
	   (kill-pcl-package)
	   (maybe-load-pcl t))))
   :load
   (maybe-load-pcl)))

(defparameter *pcl-files*
  '((("systems") "lisp"
     "pcl")
    (("pcl") "lisp"
     "sysdef"
     "boot" "braid" "cache" "cloe-low" "cmu-low" "combin" "compat"
     "construct" "coral-low" "cpatch" "cpl" "ctypes" "defclass" "defcombin"
     "defs" "defsys" "dfun" "dlap" "env" "excl-low" "fin" "fixup" "fngen" "fsc"
     "gcl-patches" "genera-low" "gold-low" "hp-low" "ibcl-low" "ibcl-patches"
     "init" "iterate" "kcl-low" "kcl-patches" "lap" "low" "lucid-low" "macros"
     "methods" "pcl-env-internal" "pcl-env" "pkg" "plap" "precom1" "precom2"
     "precom4" "pyr-low" "pyr-patches" "quadlap" "rel-7-2-patches" "rel-8-patches"
     "slots" "std-class" "sys-proclaim" "ti-low" "ti-patches" "vaxl-low" "vector" "walk"
     "xerox-low" "xerox-patches")
    (("pcl") "text"
     "12-7-88-notes" "3-17-88-notes" "3-19-87-notes" "4-21-87-notes"
     "4-29-87-notes" "5-22-87-notes" "5-22-89-notes" "8-28-88-notes"
     "get-pcl" "kcl-mods" "kcl-notes" "lap" "notes" "pcl-env" "readme")))

