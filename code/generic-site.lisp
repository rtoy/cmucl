;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/generic-site.lisp,v 1.6 1993/08/19 14:06:37 ram Exp $")
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

;;; We would appreciate it if each site establishes a local maintainer who can
;;; filter bug reports from novice users to make sure that they really have
;;; found a bug.  Fill in the maintainer's address here..
(setf (getf *herald-items* :bugs)
      '("Send bug reports and questions to your local CMU CL maintainer, or to
cmucl-bugs@cs.cmu.edu.
Loaded subsystems:"))

;;; If you have sources installed on your system, un-comment the following form
;;; and change it to point to the source location.  This will allow the Hemlock
;;; "Edit Definition" command and the debugger to find sources for functions in
;;; the core.
#|
(setf (search-list "target:") "<the source tree root>/")
|#

;;; The following Hemlock initializations will error if run in a core without
;;; hemlock.
;;;
;;; Use standard X fonts for Hemlock, since the default ones may not work
;;; everywhere.  By default, Hemlock used 8x13 and a non-standard underline
;;; font, 8x13u (which is part of the distribution.)  Unfortunately, we don't
;;; have source for this font, since it was created by hand-editing the
;;; bitmaps.
;;;
(when (member :hemlock *features*)
  (hi::%set-variable-value 'ed::open-paren-highlighting-font :global nil
			   "*-courier-bold-r-normal--*-120-*")
  (hi::%set-variable-value 'ed::default-font :global nil
			   "*-courier-medium-r-normal--*-120-*")
  (hi::%set-variable-value 'ed::active-region-highlighting-font :global nil
			   "*-courier-medium-o-normal--*-120-*"))
