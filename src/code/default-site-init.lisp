;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/default-site-init.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This is the default site init file and should be installed as
;;; "library:default-site-init".  This file is used only if
;;; "library:site-init" does not exist.

;;;
(in-package "SYSTEM")

;;; Put your site name here...
;; (setq *short-site-name* "Unknown")
;; (setq *long-site-name* "Site name not initialized")

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
;;;    cmucl/
;;;     lib/
;;;      lisp*.coore
;;;   man/
;;;   src/
;;;
;;; If your sources are located somewhere else, change this
;;; accordingly.
(setf (search-list "target:")
      '("library:../src/"))

