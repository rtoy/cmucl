;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/classes.lisp,v 1.2 2003/06/07 17:56:28 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Base class and generic function definitions for simple-streams

(in-package "STREAM")

(eval-when (:compile-toplevel)
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'compile pcl::*defgeneric-times*))

#+(or)
(declaim (ext:slots (inline simple-stream single-channel-simple-stream
			    dual-channel-simple-stream string-simple-stream)
		    (slot-boundp (simple-stream %flags plist mode
						input-handle output-handle))))


;;;; Types for buffer and strategy functions

(deftype simple-stream-buffer ()
  '(or sys:system-area-pointer (kernel:simple-unboxed-array (*))))

(deftype blocking ()
  `(member t nil :bnb))

(deftype j-listen-fn ()
  '(function (simple-stream) boolean))

(deftype j-read-char-fn ()
  '(function (simple-stream boolean t boolean) t)) ; may return EOF-VALUE

(deftype j-read-chars-fn ()
  '(function (simple-stream string (or character null) fixnum fixnum blocking)
	     (values fixnum &optional (member nil t :eof))))

(deftype j-write-char-fn ()
  '(function ((or character null) simple-stream) (or character null)))

(deftype j-write-chars-fn ()
  '(function (string simple-stream fixnum fixnum) t)) ; return chars-written?

(deftype j-unread-char-fn ()
  '(function (simple-stream t) t)) ; "relaxed" arg is boolean?  what return?

;;;; Base simple-stream classes

(def-stream-class simple-stream (standard-object stream)
  ((%flags :initform 0 :type fixnum)
   (plist :initform nil :type list :accessor stream-plist)

   (j-listen :initform #'cl::ill-in-any :type j-listen-fn)
   (j-read-char :initform #'cl::ill-in-any :type j-read-char-fn)
   (j-read-chars :initform #'cl::ill-in-any :type j-read-chars-fn)
   (j-unread-char :initform #'cl::ill-in-any :type j-unread-char-fn)
   (j-write-char :initform #'cl::ill-out-any :type j-write-char-fn) ;@@
   (j-write-chars :initform #'cl::ill-out-any :type j-write-chars-fn) ;@@

   (external-format :initform :default)

   (mode :initform 0 :type fixnum)
   (input-handle :initform nil :initarg :input-handle
		 :type (or null fixnum stream)
		 :accessor stream-input-handle)
   (output-handle :initform nil :initarg :output-handle
		  :type (or null fixnum stream)
		  :accessor stream-output-handle)
   (control-in :initform nil :type (or null simple-vector))
   (control-out :initform nil :type (or null simple-vector))

   (melded-stream :type (or null simple-stream))
   (melding-base :type (or null simple-stream))

   (encapsulated-char-read-size :initform 0 :type fixnum)
   (last-char-read-size :initform 0 :type fixnum)
   (charpos :initform 0 :type (or null integer)
	    :accessor stream-line-column)
   (record-end :initform nil :type (or null fixnum))))

(def-stream-class single-channel-simple-stream (simple-stream)
  ((buffer :initform nil :type (or simple-stream-buffer null))
   (buffpos :initform 0 :type fixnum)
   (buffer-ptr :initform 0 :type fixnum)
   (buf-len :initform 0 :type fixnum)))

(def-stream-class dual-channel-simple-stream (simple-stream)
  ((buffer :initform nil :type (or simple-stream-buffer null))
   (buffpos :initform 0 :type fixnum)
   (buffer-ptr :initform 0 :type fixnum)
   (buf-len :initform 0 :type fixnum)
   (out-buffer :initform nil :type (or simple-stream-buffer null))
   (outpos :initform 0 :type fixnum)
   (max-out-pos :initform 0 :type fixnum)))

(def-stream-class string-simple-stream (simple-stream)
  ())

(def-stream-class file-simple-stream (single-channel-simple-stream)
  ((pathname :initform nil :initarg :pathname)
   (filename :initform nil :initarg :filename)
   (original :initform nil :initarg :original)
   (delete-original :initform nil :initarg :delete-original)))

;;;; Generic function definitions

(defgeneric device-open (stream options)
  (:documentation "Write me"))

(defgeneric device-close (stream abort)
  (:documentation "Write me"))

(defgeneric device-buffer-length (stream)
  (:documentation "Write me"))

(defgeneric device-file-position (stream)
  (:documentation "Write me"))

(defgeneric (setf device-file-position) (value stream)
  (:argument-precedence-order stream value)
  (:documentation "Write me"))

(defgeneric device-file-length (stream)
  (:documentation "Write me"))

(defgeneric device-read (stream buffer start end blocking)
  (:documentation "Write me"))

(defgeneric device-clear-input (stream buffer-only)
  (:documentation "Write me"))

(defgeneric device-write (stream buffer start end blocking)
  (:documentation "Write me"))

(defgeneric device-clear-output (stream)
  (:documentation "Write me"))

(defgeneric device-finish-record (stream blocking action)
  (:documentation "Write me"))


(defmethod shared-initialize :after ((instance simple-stream) slot-names
				     &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names))
  (unless (slot-boundp instance 'melded-stream)
    (setf (slot-value instance 'melded-stream) instance)
    (setf (slot-value instance 'melding-base) instance))
  (unless (device-open instance initargs)
    (device-close instance t)))

(defmethod print-object ((object simple-stream) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (cond ((not (any-stream-instance-flags object :simple))
	   (princ "Invalid " stream))
	  ((not (any-stream-instance-flags object :input :output))
	   (princ "Closed " stream)))
    (format stream "~:(~A~)" (type-of object))))

(defmethod print-object ((object file-simple-stream) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (with-stream-class (file-simple-stream object)
      (cond ((not (any-stream-instance-flags object :simple))
	   (princ "Invalid " stream))
	  ((not (any-stream-instance-flags object :input :output))
	   (princ "Closed " stream)))
      (format stream "~:(~A~) for ~S"
	      (type-of object) (sm filename object)))))

(defmethod device-close :around ((stream simple-stream) abort)
  (with-stream-class (simple-stream stream)
    (when (any-stream-instance-flags stream :input :output)
      (when (any-stream-instance-flags stream :output)
	(if abort
	    (clear-output stream)
	    (finish-output stream)))
      (call-next-method)
      (setf (sm input-handle stream) nil
	    (sm output-handle stream) nil
	    (sm j-listen stream) #'cl::closed-flame
	    (sm j-read-char stream) #'cl::closed-flame
	    (sm j-read-chars stream) #'cl::closed-flame
	    (sm j-unread-char stream) #'cl::closed-flame
	    (sm j-write-char stream) #'cl::closed-flame	;@@
	    (sm j-write-chars stream) #'cl::closed-flame) ;@@
      (remove-stream-instance-flags stream :input :output)
      (ext:cancel-finalization stream))))

(defmethod device-close ((stream simple-stream) abort)
  (declare (ignore abort))
  t)

(defmethod device-buffer-length ((stream simple-stream))
  4096)

(defmethod device-file-position ((stream simple-stream))
  (with-stream-class (simple-stream stream)
    (cond ((any-stream-instance-flags stream :dual)
	   (with-stream-class (dual-channel-simple-stream stream)
	     (sm buffpos stream)))
	  ((any-stream-instance-flags stream :string)
	   (with-stream-class (string-simple-stream stream)
	     (sm buffpos stream)))
	  (t
	   (with-stream-class (single-channel-simple-stream stream)
	     (sm buffpos stream))))))

(defmethod (setf device-file-position) (value (stream simple-stream))
  (with-stream-class (simple-stream stream)
    (cond ((any-stream-instance-flags stream :dual)
	   (with-stream-class (dual-channel-simple-stream stream)
	     (setf (sm buffpos stream) value)))
	  ((any-stream-instance-flags stream :string)
	   (with-stream-class (string-simple-stream stream)
	     (setf (sm buffpos stream) value)))
	  (t
	   (with-stream-class (single-channel-simple-stream stream)
	     (setf (sm buffpos stream) value))))))

(defmethod device-file-length ((stream simple-stream))
  nil)

(defmethod device-read ((stream single-channel-simple-stream) buffer
                        start end blocking)
;;  (when (and (null buffer) (not (eql start end)))
;;    (with-stream-class (single-channel-simple-stream stream)
;;      (setq buffer (sm buffer stream))
;;      (setq end (sm buf-len stream))))
  (read-octets stream buffer start end blocking))

(defmethod device-read ((stream dual-channel-simple-stream) buffer
                        start end blocking)
  (when (null buffer)
    (with-stream-class (dual-channel-simple-stream stream)
      (setq buffer (sm buffer stream))
      (setq end (- (sm buf-len stream) start))))
  (read-octets stream buffer start end blocking))

(defmethod device-clear-input ((stream simple-stream) buffer-only)
  (declare (ignore buffer-only))
  nil)

(defmethod device-write ((stream single-channel-simple-stream) buffer
                         start end blocking)
  (when (and (null buffer) (not (eql start end)))
    (with-stream-class (single-channel-simple-stream stream)
      (setf buffer (sm buffer stream))
      (setf end (sm buffpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-write ((stream dual-channel-simple-stream) buffer
                         start end blocking)
  (when (and (null buffer) (not (eql start end)))
    (with-stream-class (dual-channel-simple-stream stream)
      (setf buffer (sm out-buffer stream))
      (setf end (sm outpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-clear-output ((stream simple-stream))
  nil)


;;;; Fixups when Gray streams support is present

(when (find-class 'ext:fundamental-stream nil)
  (defmethod stream-element-type ((stream stream:simple-stream))
    '(unsigned-byte 8))
  (defmethod open-stream-p ((stream stream:simple-stream))
    (any-stream-instance-flags stream :input :output))
  (defmethod close ((stream stream:simple-stream) &key abort)
    (stream:device-close stream abort))
  (defmethod input-stream-p ((stream stream:simple-stream))
    (any-stream-instance-flags stream :input))
  (defmethod output-stream-p ((stream stream:simple-stream))
    (any-stream-instance-flags stream :output))
) ; WHEN
