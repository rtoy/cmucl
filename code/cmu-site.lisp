;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/cmu-site.lisp,v 1.2 1994/10/31 04:11:27 ram Rel $")
;;;
;;; **********************************************************************
;;;
;;; Site specific initialization for CMU.  This can be used as a template for
;;; non-cmu "library:site-init" files.
;;;
(in-package "SYSTEM")

(setq *short-site-name* "CMU-SCS")
(setq *long-site-name* "Carnegie-Mellon University School of Computer Science")
