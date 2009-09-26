;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Raymond Toy and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/utf-32-le.lisp,v 1.2.4.1 2009/09/26 13:43:37 rtoy Exp $")

(in-package "STREAM")

(define-external-format :utf-32-le (:size 4)
  ()

  (octets-to-code (state input unput c c1 c2 c3 c4)
    `(let* ((,c1 ,input)
	    (,c2 ,input)
	    (,c3 ,input)
	    (,c4 ,input)
	    (,c (+ ,c1
		   (ash ,c2 8)
		   (ash ,c3 16)
		   (ash ,c4 24))))
       (declare (type (unsigned-byte 8) ,c1 ,c2 ,c3 ,c4)
		(optimize (speed 3)))
       (cond ((or (> ,c #x10ffff)
		  (lisp::surrogatep ,c))
	      ;; Surrogates are illegal.  Use replacement character.
	      (values +replacement-character-code+ 4))
	     (t
	      (values ,c 4)))))

  (code-to-octets (code state output i)
    `(dotimes (,i 4)
       (,output (ldb (byte 8 (* 8 ,i)) ,code)))))
