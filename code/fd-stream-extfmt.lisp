;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream-extfmt.lisp,v 1.13 2010/09/24 00:36:03 rtoy Rel $")
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
    ;; The following handles the case of setting the external format
    ;; for input streams where we need to handle the various buffering
    ;; strategies.
    ;;
    (cond
      ((eq old-format (fd-stream-external-format stream))
       ;; Nothing to do if the new and old formats are the same.
       )
      ((and lisp::*enable-stream-buffer-p* updatep
	    (lisp-stream-string-buffer stream))
       ;; We want to reconvert any octets that haven't been converted
       ;; yet.  So, we need to figure out which octet to start with.
       ;; This is done by converting (the previously converted) octets
       ;; until we've converted the right number of characters.  Or,
       ;; since we have the octet-count, just sum up them up to figure
       ;; out how many octets we've already consumed.
       (let* ((ibuf (lisp-stream-in-buffer stream))
	      (sindex (lisp-stream-string-index stream))
	      (octet-count (fd-stream-octet-count stream))
	      (oc (make-array in-buffer-length :element-type '(unsigned-byte 8)))
	      (index (loop for k of-type fixnum from 0 below (1- sindex)
			summing (aref octet-count k))))
	 ;; We now know the last octet that was used.  Now convert the
	 ;; rest of the octets using the new format.  The new
	 ;; characters are placed in the string buffer at the point
	 ;; just after the last character that we've already read.
	 (multiple-value-bind (s pos count new-state)
	     (stream::octets-to-string-counted ibuf
					       oc
					       :start index
					       :end (fd-stream-in-length stream)
					       :external-format (fd-stream-external-format stream)
					       :string (lisp-stream-string-buffer stream)
					       :s-start sindex
					       :error (fd-stream-octets-to-char-error stream))
	   (replace octet-count oc :start1 index :end2 pos)
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
      ((and updatep (lisp-stream-in-buffer stream))
       ;; This means the external format was ISO8859-1 and we're
       ;; switching to something else.  If so, we need to convert all
       ;; the octets that haven't been processed yet and place them in
       ;; the string buffer.  We also need to adjust the in-buffer to
       ;; put those octets in the expected place at the beginning of
       ;; in-buffer.
       (let ((index (lisp-stream-in-index stream))
	     (ibuf (lisp-stream-in-buffer stream)))
	 (setf (lisp-stream-string-buffer stream)
	       (make-string (1+ in-buffer-length)))
	 (setf (lisp-stream-string-index stream) 1)
	 ;; Set the unread char to be the last read octet.
	 (setf (aref (lisp-stream-string-buffer stream) 0)
	       (code-char (aref ibuf (1- index))))
	 
	 (let ((oc (or (fd-stream-octet-count stream)
		       (setf (fd-stream-octet-count stream)
			     (make-array in-buffer-length :element-type '(unsigned-byte 8))))))
	   (multiple-value-bind (s pos count new-state)
	       (stream::octets-to-string-counted ibuf
						 oc
						 :start index
						 :external-format (fd-stream-external-format stream)
						 :string (lisp-stream-string-buffer stream)
						 :s-start 1
						 :error (fd-stream-octets-to-char-error stream))
	     (declare (ignore s))
	     (setf (lisp-stream-string-buffer-len stream) pos)
	     (setf (fd-stream-oc-state stream) new-state)
	     ;; Move the octets from the end of the in-buffer to the
	     ;; beginning.  Set the index to the number of octets we've
	     ;; processed.
	     (replace ibuf ibuf :start2 index)
	     (setf (lisp-stream-in-index stream) count))))))
    extfmt))



(setf lisp::*enable-stream-buffer-p* t)
