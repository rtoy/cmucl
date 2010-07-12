;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/cr.lisp,v 1.4 2010/07/12 14:42:11 rtoy Rel $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; Convert CR to/from #\newline.
(define-composing-external-format :cr (:size 1 :documentation
"CR is a composing external format that converts CR (carriage return)
to a Lisp newline character.  This is the Mac end-of-line character
sequence.")
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
