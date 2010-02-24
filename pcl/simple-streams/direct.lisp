;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/direct.lisp,v 1.3 2007/11/05 15:25:04 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; Direct-Simple-Stream and Buffer-(Input|Output)-Simple-Stream

(in-package "STREAM")

(export '(direct-simple-stream
	  buffer-input-simple-stream
	  buffer-output-simple-stream))

(def-stream-class direct-simple-stream (single-channel-simple-stream)
  ())

(def-stream-class buffer-input-simple-stream (direct-simple-stream)
  ())

(def-stream-class buffer-output-simple-stream (direct-simple-stream)
  ((out-buffer :initform nil :type (or kernel:simple-stream-buffer null))
   (outpos :initform 0 :type fixnum)
   (max-out-pos :initform 0 :type fixnum)))

(defmethod device-file-length ((stream direct-simple-stream))
  ;; return buffer length
  )

(defmethod device-open ((stream buffer-input-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream buffer-output-simple-stream) options)
  #| do something |#
  stream)
