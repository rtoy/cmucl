;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream-extfmt.lisp,v 1.2 2009/06/11 16:03:57 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementation of external-formats for fd-streams

(in-package "LISP")

(defun output-char-none-buffered (stream char)
  (funcall (ef-cout (fd-stream-external-format stream))
	   stream char)
  (if (char= char #\Newline)
      (setf (fd-stream-char-pos stream) 0)
      (incf (fd-stream-char-pos stream)))
  (flush-output-buffer stream)
  (values))

(defun output-char-line-buffered (stream char)
  (funcall (ef-cout (fd-stream-external-format stream))
	   stream char)
  (if (char= char #\Newline)
      (progn (setf (fd-stream-char-pos stream) 0)
	     (flush-output-buffer stream))
      (incf (fd-stream-char-pos stream)))
  (values))

(defun output-char-full-buffered (stream char)
  (funcall (ef-cout (fd-stream-external-format stream))
	   stream char)
  (if (char= char #\Newline)
      (setf (fd-stream-char-pos stream) 0)
      (incf (fd-stream-char-pos stream)))
  (values))

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

(defun input-character (stream eof-error eof-value)
  ;; This needs to go away.  Unreading a character needs to be done by backing
  ;; up the buffer head pointer, so that the external format will re-build the
  ;; character - else changing extfmts won't work.  But until we get around
  ;; to teaching UNREAD-CHAR to DTRT, keep this to maintain compatibility...
  (if (fd-stream-unread stream)
      (prog1 (fd-stream-unread stream)
	(setf (fd-stream-unread stream) nil)
	(setf (fd-stream-listen stream) nil))
      (let ((char (funcall (ef-cin (fd-stream-external-format stream))
			   stream)))
	(if char
	    char
	    (eof-or-lose stream eof-error eof-value)))))


(setf (fd-stream-out *stdout*) #'output-char-line-buffered
      (fd-stream-sout *stdout*) #'fd-sout
      (fd-stream-out *stderr*) #'output-char-line-buffered
      (fd-stream-sout *stderr*) #'fd-sout
      (fd-stream-in *stdin*) #'input-character)
