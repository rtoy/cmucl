;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/generic-site.lisp,v 1.1 1991/10/17 18:56:58 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Generic site specific initialization for CMU CL.  This can be used as a
;;; template for non-cmu "library:site-init" files.
;;;
(in-package "SYSTEM")

;;; Define the site to be undefined.
(setq *short-site-name* "Unknown")
(setq *long-site-name* "Site name not initialized")

;;; Use standard X fonts for Hemlock, since the default ones may not work
;;; everywhere.
;;;
(hi:setv ed::open-paren-highlighting-font "*-courier-bold-r-normal--12-*")
(hi:setv ed::default-font "*-courier-medium-r-normal--12-*")
(hi:setv ed::active-region-highlighting-font "*-courier-medium-o-normal--12-*")
