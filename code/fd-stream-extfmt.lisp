;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream-extfmt.lisp,v 1.3 2009/08/10 16:47:41 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; Implementation of external-formats for fd-streams

(in-package "LISP")

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
    (error "Loading simple-streams should redefine this")
    ;; lisp-stream
    (typecase stream
      (fd-stream (%set-fd-stream-external-format stream extfmt))
      (synonym-stream (setf (stream-external-format
			     (symbol-value (synonym-stream-symbol stream)))
			  extfmt))
      (t (error "Don't know how to set external-format for ~S." stream)))
    ;; fundamental-stream
    (error "Setting external-format on Gray streams not supported."))
  extfmt)



(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-cin+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-cout+)
(stream::precompile-ef-slot :iso8859-1 #.stream::+ef-sout+)



;(set-terminal-coding-system :iso8859-1)
