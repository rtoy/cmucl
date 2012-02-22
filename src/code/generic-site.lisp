;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/generic-site.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file is installed as "library:site-init" in binary
;;; distributions of CMUCL.
;;;
(in-package "SYSTEM")

;;; Put your site name here...
(setq *short-site-name* "Unknown")
(setq *long-site-name* "Site name not initialized")

;;; We would appreciate it if each site establishes a local maintainer who can
;;; filter bug reports from novice users to make sure that they really have
;;; found a bug.  Fill in the maintainer's address here..
(when *herald-items*
  (rplaca
   (cdr (member :bugs *herald-items*))
   '("Send questions and bug reports to your local CMUCL maintainer, " terpri
     "or see <http://www.cons.org/cmucl/support.html>." terpri
     "Loaded subsystems:" terpri)))

;;; If you have sources installed on your system, un-comment the following form
;;; and change it to point to the source location.  This will allow the Hemlock
;;; "Edit Definition" command and the debugger to find sources for functions in
;;; the core.
;;;
;;; The definition below assumes the default tree structure in a CMUCL
;;; distribution:
;;;
;;; top
;;;   bin/
;;;   lib/
;;;   man/
;;;   src/
;;;
;;; If your sources are located somewhere else, change this
;;; accordingly.
#|
(setf (search-list "target:")
      (list
       (namestring
	(make-pathname
	 :directory (pathname-directory
		     (merge-pathnames (make-pathname
				       :directory '(:relative :up :up
						    :up "src"))
				      (pathname lisp::*cmucl-core-path*)))))))
|#
