;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream-extfmt.lisp,v 1.10.2.1 2010/09/06 15:41:30 rtoy Exp $")
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
    (error (intl:gettext "Loading simple-streams should redefine this"))
    ;; lisp-stream
    (typecase stream
      (fd-stream (%set-fd-stream-external-format stream extfmt))
      (synonym-stream (setf (stream-external-format
			     (symbol-value (synonym-stream-symbol stream)))
			  extfmt))
      (t (error (intl:gettext "Don't know how to set external-format for ~S.") stream)))
    ;; fundamental-stream
    (error (intl:gettext "Setting external-format on Gray streams not supported.")))
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
    ;; FIXME: We currently don't handle the case of changing from
    ;; ISO8859-1 to something else.  This is because ISO8859-1 doesn't
    ;; use the string-buffer, so when we switch to another external
    ;; format that does, we need to set up the string-buffer
    ;; appropriately.
    (when (and lisp::*enable-stream-buffer-p* updatep
	       (lisp-stream-string-buffer stream))
      ;; We want to reconvert any octets that haven't been converted
      ;; yet.  So, we need to figure out which octet to start with.
      ;; This is done by converting (the previously converted) octets
      ;; until we've converted the right number of characters.
      (let ((ibuf (lisp-stream-in-buffer stream))
	    (sindex (lisp-stream-string-index stream))
	    (index 0)
	    (state (fd-stream-saved-oc-state stream)))
	;; Reconvert all the octets we've already converted and read.
	;; We don't know how many octets that is, but do know how many
	;; characters there are.
	(multiple-value-bind (s pos count new-state)
	    (octets-to-string ibuf
			      :start 0
			      :external-format old-format
			      :string (make-string (1- sindex))
			      :state state
			      :error (fd-stream-octets-to-char-error stream))
	  (declare (ignore s pos))
	  (setf state new-state)
	  (setf index count))
	;; We now know the last octet that was used.  Now convert the
	;; rest of the octets using the new format.  The new
	;; characters are placed in the string buffer at the point
	;; just after the last character that we've already read.
	(multiple-value-bind (s pos count new-state)
	    (octets-to-string ibuf
			      :start index
			      :end (fd-stream-in-length stream)
			      :external-format (fd-stream-external-format stream)
			      :string (lisp-stream-string-buffer stream)
			      :s-start sindex
			      :state state
			      :error (fd-stream-octets-to-char-error stream))
	  (cond ((eq (fd-stream-external-format stream) :iso8859-1)
		 ;; ISO8859-1 doesn't use the string-buffer, so we
		 ;; need to copy the string to the in-buffer and then
		 ;; set the string-buffer to nil to indicate we're not
		 ;; using the string buffer anymore.
		 (let ((index (- in-buffer-length count)))
		   (dotimes (k count)
		     (setf (aref ibuf (+ k index))
			   (char-code (aref s (+ k sindex)))))
		   (setf (lisp-stream-in-index stream) index)
		   (setf (lisp-stream-string-buffer stream) nil)
		   (setf (lisp-stream-string-buffer-len stream) 0)
		   (setf (lisp-stream-string-index stream) 0)))
		(t
		 (setf (lisp-stream-string-index stream) sindex)
		 (setf (lisp-stream-string-buffer-len stream) pos)
		 (setf (lisp-stream-in-index stream) (+ index count))
		 (setf (fd-stream-oc-state stream) new-state))))))
    extfmt))



(setf lisp::*enable-stream-buffer-p* t)
