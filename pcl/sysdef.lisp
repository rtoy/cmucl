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

(defsystem pcl
    (:pretty-name "PCL")
  (:forms 
   :compile 
   (let ((defsys (subfile '("pcl") :name "defsys")))
     (unless *pcl-compiled-p*
       (let ((*skip-load-if-loaded-p* t)
	     (*skip-compile-file-fwd* 0))
	 (set (intern (symbol-name ':*pcl-directory*) 
		      (or (find-package :pcl)
			  (make-package :pcl :use '(:lisp))))
	      defsys)
	 (compile-file defsys)
	 (load-file defsys))
       (funcall (intern "COMPILE-PCL" "PCL"))
       (setq *pcl-compiled-p* t)
       (let ((b-s (find-symbol "*BOOT-STATE*" "PCL"))
	     (*skip-load-if-loaded-p* nil))
	 (when (and b-s (boundp b-s) (symbol-value b-s))
	   #+genera (scl:pkg-kill "PCL")
	   #+lucid (lcl:delete-package "PCL")
	   #-(or genera lucid) (rename-package "PCL" (symbol-name (gensym)))
	   (set (intern (symbol-name ':*pcl-directory*) 
		      (or (find-package :pcl)
			  (make-package :pcl :use '(:lisp))))
	      defsys)
	   (load-file defsys))
	 (funcall (intern "LOAD-PCL" "PCL")))
       (setq *pcl-loaded-p* t)))
   :load
   (unless *pcl-loaded-p*
     (let ((defsys (subfile '("pcl") :name "defsys")))
       (let ((*skip-load-if-loaded-p* t))
	 (set (intern (symbol-name ':*pcl-directory*) 
		      (or (find-package :pcl)
			  (make-package :pcl :use '(:lisp))))
	      defsys)
	 (load-file defsys))
       (funcall (intern "LOAD-PCL" "PCL"))
       (setq *pcl-loaded-p* t)))))

(pushnew 'pcl *auto-load-systems*)

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
     "slots" "std-class" "ti-low" "ti-patches" "vaxl-low" "vector" "walk"
     "xerox-low" "xerox-patches")
    (("pcl") "text"
     "12-7-88-notes" "3-17-88-notes" "3-19-87-notes" "4-21-87-notes"
     "4-29-87-notes" "5-22-87-notes" "5-22-89-notes" "8-28-88-notes"
     "get-pcl" "kcl-mods" "kcl-notes" "lap" "notes" "pcl-env" "readme")))

