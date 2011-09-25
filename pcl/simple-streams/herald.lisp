;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: src/pcl/simple-streams/herald.lisp $")
;;;
;;; **********************************************************************
;;;
;;; 

(in-package "STREAM")

(setf (getf ext:*herald-items* :simple-streams)
      `("    Simple Streams"))

(pushnew :simple-streams *features*)
(provide :simple-streams)
