;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/terminal.lisp,v 1.1 2003/06/06 16:23:46 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Terminal-Simple-Stream

(in-package "STREAM")

(export '(terminal-simple-stream))

(defvar *terminal-control-in-table*
  (make-control-table #\Newline #'std-dc-newline-in-handler))

(def-stream-class terminal-simple-stream (dual-channel-simple-stream)
  ())

(defmethod device-open ((stream terminal-simple-stream) options)
  (with-stream-class (terminal-simple-stream stream)
    (when (getf options :input-handle)
      (setf (sm input-handle stream) (getf options :input-handle))
      (add-stream-instance-flags stream :simple :interactive :dual :input)
      (unless (sm buffer stream)
        (let ((length (device-buffer-length stream)))
          (setf (sm buffer stream) (make-string length)
                (sm buf-len stream) length)))
      (setf (sm control-in stream) *terminal-control-in-table*))
    (when (getf options :output-handle)
      (setf (sm output-handle stream) (getf options :output-handle))
      (add-stream-instance-flags stream :simple :interactive :dual :output)
      (unless (sm out-buffer stream)
        (let ((length (device-buffer-length stream)))
          (setf (sm out-buffer stream) (make-string length)
                (sm max-out-pos stream) length)))
      (setf (sm control-out stream) *std-control-out-table*))
    (install-dual-channel-character-strategy
     stream (getf options :external-format :default)))
  #| do something |#
  stream)

(defmethod device-read ((stream terminal-simple-stream) buffer
                        start end blocking)
  (let ((result (call-next-method)))
    (if (= result -1) -2 result)))

(defmethod device-clear-input ((stream terminal-simple-stream) buffer-only)
  )
