;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/crlf.lisp,v 1.2 2009/06/11 16:04:02 rtoy Exp $")

(in-package "STREAM")

(define-composing-external-format :crlf (:max 2)
  (input (state input unput tmp1 tmp2 tmp3 tmp4)
    `(multiple-value-bind (,tmp1 ,tmp2) ,input
       (when (= ,tmp1 #x000D)
	 (multiple-value-bind (,tmp3 ,tmp4) ,input
	   ;; @@ tmp3 may be NIL, if file ends in LF
	   (if (= ,tmp3 #x000A)
	       (setq ,tmp1 ,tmp3 ,tmp2 (+ ,tmp2 ,tmp4))
	       (,unput ,tmp4))))
       (values ,tmp1 ,tmp2)))
  (output (code state output)
    `(progn
       (when (= ,code #x000A)
	 (,output #x000D))
       (,output ,code))))
