;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/string.lisp,v 1.1 2003/06/06 16:23:46 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; String-Simple-Stream and relatives

(in-package "STREAM")

(export '(string-input-simple-stream string-output-simple-stream
	  fill-pointer-output-simple-stream composing-stream
	  xp-simple-stream annotation-output-simple-stream))

(def-stream-class string-input-simple-stream (string-simple-stream)
  ())

(def-stream-class string-output-simple-stream (string-simple-stream)
  ((out-buffer :initform nil :type (or simple-stream-buffer null))
   (outpos :initform 0 :type fixnum)
   (max-out-pos :initform 0 :type fixnum)))

(def-stream-class fill-pointer-output-simple-stream
    (string-output-simple-stream)
  ())

(def-stream-class composing-stream (string-simple-stream)
  ())

(def-stream-class xp-simple-stream (string-output-simple-stream)
  ())

(def-stream-class annotation-output-simple-stream (string-output-simple-stream)
  ())

(defmethod device-file-position ((stream string-simple-stream))
  ;; get string length (of input or output buffer?)
  )

(defmethod (setf device-file-position) (value (stream string-simple-stream))
  ;; set string length (of input or output buffer?)
  )

(defmethod device-file-length ((stream string-simple-stream))
  ;; return string length
  )

(defmethod device-open ((stream string-input-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-extend ((stream string-input-simple-stream) need action)
  (declare (ignore need action))
  nil)

(defmethod device-open ((stream string-output-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-extend ((stream string-output-simple-stream) need action)
  ;; @@3
  )

(defmethod device-open ((stream fill-pointer-output-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-file-position ((stream fill-pointer-output-simple-stream))
  ;; get fill pointer (of input or output buffer?)
  )

(defmethod (setf device-file-position)
    (value (stream fill-pointer-output-simple-stream))
  ;; set fill pointer (of input or output buffer?)
  )

(defmethod device-extend ((stream fill-pointer-output-simple-stream)
                          need action)
  ;; @@4
  )

(defmethod device-open ((stream xp-simple-stream) options)
  #| do something |#
  stream)
