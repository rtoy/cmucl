;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/generic-site.lisp,v 1.3 1991/10/22 16:15:53 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Generic site specific initialization for CMU CL.  This can be used as a
;;; template for non-cmu "library:site-init" files.
;;;
(in-package "SYSTEM")

;;; Put your site name here...
(setq *short-site-name* "Unknown")
(setq *long-site-name* "Site name not initialized")

;;; The following Hemlock initializations will error if run in a core without
;;; hemlock.

;;; If you have sources installed on your system, un-comment the following form
;;; and change it to point to the source location.  This will allow the Hemlock
;;; "Edit Definition" command to find sources for functions in the core.  If
;;; this doesn't work, check that first part of the translation really does
;;; have the correct prefix.
;;;
#|
(ed::add-definition-dir-translation
 "/afs/cs.cmu.edu/project/clisp-1/sun4c_41/15/"
 "<your source location here>")
|#

;;; Use standard X fonts for Hemlock, since the default ones may not work
;;; everywhere.  By default, Hemlock used 8x13 and a non-standard underline
;;; font, 8x13u (which is part of the distribution.)  Unfortunately, we don't
;;; have source for this font, since it was created by hand-editing the
;;; bitmaps.
;;;
(hi:setv ed::open-paren-highlighting-font "*-courier-bold-r-normal--*-120-*")
(hi:setv ed::default-font "*-courier-medium-r-normal--*-120-*")
(hi:setv ed::active-region-highlighting-font
	 "*-courier-medium-o-normal--*-120-*")
