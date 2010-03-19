;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream-extfmt.lisp,v 1.7 2010/03/19 15:18:59 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementation of external-formats for fd-streams

(in-package "LISP")

(intl:textdomain "cmucl")

;; an fd-sout that works with external-formats; needs slots in fd-stream
(defun fd-sout (stream thing start end)
  (let ((start (or start 0))
	(end (or end (length (the vector thing)))))
    (declare (type index start end))
    (if (stringp thing)
	(let ((last-newline (and (find #\newline (the simple-string thing)
				       :start start :end end)
				 (position #\newline (the simple-string thing)
					   :from-end t
					   :start start
					   :end end))))
	  (funcall (ef-sout (fd-stream-external-format stream))
		   stream thing start end)
	  (ecase (fd-stream-buffering stream)
	    (:full #| do nothing |#)
	    (:line
	     (when last-newline
	       (flush-output-buffer stream)))
	    (:none
	     (flush-output-buffer stream)))
	  (if last-newline
	      (setf (fd-stream-char-pos stream)
		    (- end last-newline 1))
	      (incf (fd-stream-char-pos stream)
		    (- end start))))
	(ecase (fd-stream-buffering stream)
	  ((:line :full)
	   (output-raw-bytes stream thing start end))
	  (:none
	   (do-output stream thing start end nil))))))

(defun (setf stream-external-format) (extfmt stream)
  (declare (type stream stream))
  (stream-dispatch stream
    ;; simple-stream
    (error _"Loading simple-streams should redefine this")
    ;; lisp-stream
    (typecase stream
      (fd-stream (%set-fd-stream-external-format stream extfmt))
      (synonym-stream (setf (stream-external-format
			     (symbol-value (synonym-stream-symbol stream)))
			  extfmt))
      (t (error _"Don't know how to set external-format for ~S." stream)))
    ;; fundamental-stream
    (error _"Setting external-format on Gray streams not supported."))
  extfmt)

(defun %set-fd-stream-external-format (stream extfmt &optional (updatep t))
  (declare (type fd-stream stream))
  (let ((old-format (fd-stream-external-format stream)))
    (setf (fd-stream-external-format stream)
	  (stream::ef-name (stream::find-external-format extfmt))
	  (fd-stream-oc-state stream) nil
	  (fd-stream-co-state stream) nil)
    (when (fd-stream-ibuf-sap stream)	; input stream
      (setf (fd-stream-in stream) (ef-cin extfmt)))
    (when (fd-stream-obuf-sap stream)	; output stream
      (setf (fd-stream-out stream) (ef-cout extfmt)
	    ;;@@ (fd-stream-sout stream) (ef-sout extfmt)
	    ))
    (when (and lisp::*enable-stream-buffer-p* updatep
	       (lisp-stream-string-buffer stream))
      ;; We want to reconvert any octets that haven't been converted
      ;; yet.  So, we need to figure out which octet to start with.
      ;; This is done by converting (the previously converted) octets
      ;; until we've converted the right number of characters.
      (let ((ibuf (lisp-stream-in-buffer stream))
	    (sindex (1- (lisp-stream-string-index stream)))
	    (index 0)
	    (state (fd-stream-saved-oc-state stream)))
	;; Reconvert all the octets we've already converted and read.
	;; We don't know how many octets that is, but do know how many
	;; characters there are.
	(multiple-value-bind (s pos count new-state)
	    (octets-to-string ibuf
			      :start 0
			      :external-format old-format
			      :string (make-string sindex)
			      :state state)
	  (declare (ignore s pos))
	  (setf state new-state)
	  (setf index count))
	
	;; We now know the last octet that was used.  Now convert the
	;; rest of the octets using the new format.
	(multiple-value-bind (s pos count new-state)
	    (octets-to-string ibuf
			      :start index
			      :end (fd-stream-in-length stream)
			      :external-format (fd-stream-external-format stream)
			      :string (lisp-stream-string-buffer stream)
			      :s-start 1
			      :state state)
	  (declare (ignore s))
	  (setf (lisp-stream-string-index stream) 1)
	  (setf (lisp-stream-string-buffer-len stream) pos)
	  (setf (lisp-stream-in-index stream) (+ index count))
	  (setf (fd-stream-oc-state stream) new-state))))
    extfmt))


(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-cin+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-cout+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-sout+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-os+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-so+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-en+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-de+)

(setf lisp::*enable-stream-buffer-p* t)
