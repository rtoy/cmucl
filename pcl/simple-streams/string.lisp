;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/string.lisp,v 1.2 2003/06/07 17:56:28 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; String-Simple-Stream and relatives

(in-package "STREAM")

(export '(string-input-simple-stream string-output-simple-stream
	  fill-pointer-output-simple-stream composing-stream
	  xp-simple-stream annotation-output-simple-stream))

(def-stream-class string-input-simple-stream (string-simple-stream)
  ((buffer :initform nil :type (or simple-stream-buffer null))
   (buffpos :initform 0 :type fixnum)
   (buffer-ptr :initform 0 :type fixnum)
   (buf-len :initform 0 :type fixnum)))

(def-stream-class string-output-simple-stream (string-simple-stream)
  ((buffer :initform nil :type (or simple-stream-buffer null))
   (buffpos :initform 0 :type fixnum)
   (buffer-ptr :initform 0 :type fixnum)
   (buf-len :initform 0 :type fixnum)
   (out-buffer :initform nil :type (or simple-stream-buffer null))
   (outpos :initform 0 :type fixnum)
   (max-out-pos :initform 0 :type fixnum)))

(def-stream-class composing-stream (string-simple-stream)
  ())

(def-stream-class fill-pointer-output-simple-stream
    (string-output-simple-stream)
  ())

(def-stream-class xp-simple-stream (string-output-simple-stream)
  ())

(def-stream-class annotation-output-simple-stream (string-output-simple-stream)
  ())

(defmethod device-open :before ((stream string-input-simple-stream) options)
  (with-stream-class (string-input-simple-stream stream)
    (let ((string (getf options :string)))
      (when (and string (null (sm buffer stream)))
	(let ((start (getf options :start))
	      (end (or (getf options :end) (length string))))
	  (setf (sm buffer stream) string
		(sm buffpos stream) start
		(sm buffer-ptr stream) end))))
    (install-string-input-character-strategy stream)
    (add-stream-instance-flags stream :string :input :simple)))

(defmethod device-open :before ((stream string-output-simple-stream) options)
  (with-stream-class (string-output-simple-stream stream)
    (unless (sm out-buffer stream)
      (let ((string (getf options :string)))
	(if string
	    (setf (sm out-buffer stream) string
		  (sm max-out-pos stream) (length string))
	    (let ((buflen (max (device-buffer-length stream) 16)))
	      (setf (sm out-buffer stream) (make-string buflen)
		    (sm max-out-pos stream) buflen)))))
    (unless (sm control-out stream)
      (setf (sm control-out stream) *std-control-out-table*))
    (install-string-output-character-strategy stream)
    (add-stream-instance-flags stream :string :output :simple)))

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

(defmethod device-open ((stream string-output-simple-stream) options)
  #| do something |#
  stream)

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

(defmethod device-open ((stream xp-simple-stream) options)
  #| do something |#
  stream)
