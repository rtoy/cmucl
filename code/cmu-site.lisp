;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/cmu-site.lisp,v 1.1 1991/08/30 17:43:33 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Site specific initialization for CMU.  This can be used as a template for
;;; non-cmu "library:site-init" files.
;;;
(in-package "SYSTEM")

(setq *short-site-name* "CMU-SCS")
(setq *long-site-name* "Carnegie-Mellon University School of Computer Science")
