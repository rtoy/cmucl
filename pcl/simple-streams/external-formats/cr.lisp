;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/cr.lisp,v 1.1.2.2 2009/09/03 20:36:27 rtoy Exp $")

(in-package "STREAM")

;; Convert CR to/from #\newline.
(define-composing-external-format :cr (:size 1)
  (input (state input unput tmp1 tmp2)
    `(multiple-value-bind (,tmp1 ,tmp2)
	 ,input
       (if (= ,tmp1 (char-code #\return))
	   (values (char-code #\newline) ,tmp2)
	   (values ,tmp1 ,tmp2))))
  (output (code state output)
    `(if (= ,code (char-code #\newline))
	 (,output (char-code #\return))
	 (,output ,code))))
