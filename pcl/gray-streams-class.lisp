;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/gray-streams-class.lisp,v 1.2 1998/06/02 02:43:15 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Class definitions for the CMUCL Gray streams implementation.
;;; Based on the stream-definition-by-user proposal by David N. Gray.
;;;

(in-package "LISP")



;;; Bootstrap the fundamental-stream class.
(let ((pcl::*pcl-class-boot* 'fundamental-stream))
  (defclass fundamental-stream (standard-object stream)
    ()
    (:documentation "Base class for all CLOS streams")))

;;; Define the stream classes.
(defclass fundamental-input-stream (fundamental-stream))

(defclass fundamental-output-stream (fundamental-stream))

(defclass fundamental-character-stream (fundamental-stream))

(defclass fundamental-binary-stream (fundamental-stream))

(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream))

(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream))

(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream))

(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream))


;;; Example character input and output streams.

(defclass character-output-stream (fundamental-character-output-stream)
  ((lisp-stream :initarg :lisp-stream
		:accessor character-output-stream-lisp-stream)))

(defclass character-input-stream (fundamental-character-input-stream)
  ((lisp-stream :initarg :lisp-stream
		:accessor character-input-stream-lisp-stream)))
