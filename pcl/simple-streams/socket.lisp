;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/socket.lisp,v 1.1 2003/06/06 16:23:46 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Socket-simple-stream and socket-base-simple-stream

(in-package "STREAM")

(export '(socket-simple-stream socket-base-simple-stream))

(def-stream-class socket-simple-stream (dual-channel-simple-stream)
  ())

(def-stream-class socket-base-simple-stream (dual-channel-simple-stream)
  ())

(defmethod device-open ((stream socket-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream socket-base-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-write ((stream socket-base-simple-stream) buffer
                         start end blocking)
  ;; @@2
  (call-next-method))
