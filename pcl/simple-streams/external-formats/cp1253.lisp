;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/cp1253.lisp,v 1.2 2009/06/11 16:04:02 rtoy Exp $")

(in-package "STREAM")

(define-external-format :cp1253 (:mac-roman)
  ((table +ms-cp1253+)))
