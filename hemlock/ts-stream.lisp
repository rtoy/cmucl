;;; -*- Package: Hemlock; Log: hemlock.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC).
;;; **********************************************************************
;;;
;;; This file implements typescript streams.
;;;
;;; Written by William Lott.
;;;

(in-package "HEMLOCK")

(defconstant ts-stream-output-buffer-size 512)

(defstruct (ts-stream
	    (:include stream
		      (in #'%ts-stream-in)
		      (out #'%ts-stream-out)
		      (sout #'%ts-stream-sout)
		      (misc #'%ts-stream-misc))
	    (:print-function %ts-stream-print)
	    (:constructor make-ts-stream (wire typescript)))
  wire
  typescript
  (output-buffer (make-string ts-stream-output-buffer-size)
		 :type simple-string)
  (output-buffer-index 0 :type fixnum)
  (char-pos 0)
  current-input
  (input-read-index 0 :type fixnum))

(defun %ts-stream-print (ts stream depth)
  (declare (ignore ts depth))
  (write-string "#<TS Stream>" stream))

;;; TS-STREAM-ACCEPT-INPUT --- internal interface.
;;;
;;; This routine is called by the editor to indicate that the user has typed
;;; more input.
;;;
(defun ts-stream-accept-input (remote string)
  (let ((stream (wire:remote-object-value remote)))
    (system:without-interrupts
      (setf (ts-stream-current-input stream)
	    (nconc (ts-stream-current-input stream)
		   (list string)))
      (setf (ts-stream-char-pos stream) 0)))
  nil)

;;; %TS-STREAM-LISTEN --- internal.
;;;
;;; Determine if there is any input available.  If we don't think so, process
;;; all pending events, and look again.
;;; 
(defun %ts-stream-listen (stream)
  (flet ((check ()
		(system:without-interrupts
		 (loop
		   (let ((current (ts-stream-current-input stream)))
		     (cond ((null current)
			    (return nil))
			   ((>= (ts-stream-input-read-index stream)
				(length (the simple-string (car current))))
			    (pop (ts-stream-current-input stream))
			    (setf (ts-stream-input-read-index stream) 0))
			   (t
			    (return t))))))))
    (or (check)
	(progn
	  (system:serve-all-events 0)
	  (check)))))

;;; WAIT-FOR-TYPESCRIPT-INPUT --- internal.
;;;
;;; Keep calling server until some input shows up.
;;; 
(defun wait-for-typescript-input (stream)
  (unless (%ts-stream-listen stream)
    (let ((wire (ts-stream-wire stream))
	  (ts (ts-stream-typescript stream)))
      (wire:remote wire
		   (ts-buffer-ask-for-input ts))
      (wire:wire-force-output wire)
      (loop
	(system:serve-all-events)
	(when (%ts-stream-listen stream)
	  (return))))))

;;; %TS-STREAM-IN --- internal.
;;;
;;; The READ-CHAR stream method.
;;; 
(defun %ts-stream-in (stream &optional eoferr eofval)
  (declare (ignore eoferr eofval)) ; EOF's are impossible.
  (wait-for-typescript-input stream)
  (system:without-interrupts
   (prog1
       (schar (car (ts-stream-current-input stream))
	      (ts-stream-input-read-index stream))
     (incf (ts-stream-input-read-index stream)))))



;;; %TS-STREAM-READ-LINE --- internal.
;;;
;;; The READ-LINE stream method.  Note: here we take advantage of the fact that
;;; newlines will only appear at the end of strings.
;;; 
(defun %ts-stream-read-line (stream eoferr eofval)
  (declare (ignore eoferr eofval))
  (macrolet ((next-str ()
	       '(progn
		  (wait-for-typescript-input stream)
		  (system:without-interrupts
		    (prog1
			(if (zerop (ts-stream-input-read-index stream))
			    (pop (ts-stream-current-input stream))
			    (subseq (pop (ts-stream-current-input stream))
				    (ts-stream-input-read-index stream)))
		      (setf (ts-stream-input-read-index stream) 0))))))
    (do ((result (next-str) (concatenate 'simple-string result (next-str))))
	((char= (schar result (1- (length result))) #\newline)
	 (values (subseq result 0 (1- (length result)))
		 nil))
      (declare (simple-string result)))))

;;; %TS-STREAM-FLSBUF --- internal.
;;;
;;; Flush the output buffer associated with stream.
;;; 
(defun %ts-stream-flsbuf (stream)
  (when (and (ts-stream-wire stream)
	     (ts-stream-output-buffer stream)
	     (not (zerop (ts-stream-output-buffer-index stream))))
    (wire:remote (ts-stream-wire stream)
      (ts-buffer-output-string (ts-stream-typescript stream)
			       (subseq (the simple-string
					    (ts-stream-output-buffer stream))
				       0
				       (ts-stream-output-buffer-index stream))))
    (setf (ts-stream-output-buffer-index stream)
	  0)))

;;; %TS-STREAM-OUT --- internal.
;;;
;;; Output a single character to stream.
;;;
(defun %ts-stream-out (stream char)
  (declare (base-character char))
  (when (= (ts-stream-output-buffer-index stream)
	   ts-stream-output-buffer-size)
    (%ts-stream-flsbuf stream))
  (setf (schar (ts-stream-output-buffer stream)
	       (ts-stream-output-buffer-index stream))
	char)
  (incf (ts-stream-output-buffer-index stream))
  (incf (ts-stream-char-pos stream))
  (when (= (char-code char)
	   (char-code #\Newline))
    (%ts-stream-flsbuf stream)
    (setf (ts-stream-char-pos stream) 0)
    (wire:wire-force-output (ts-stream-wire stream)))
  char)

;;; %TS-STREAM-SOUT --- internal.
;;;
;;; Output a string to stream.
;;; 
(defun %ts-stream-sout (stream string start end)
  (declare (simple-string string))
  (declare (fixnum start end))
  (let ((wire (ts-stream-wire stream))
	(newline (position #\Newline string :start start :end end :from-end t))
	(length (- end start)))
    (when wire
      (let ((index (ts-stream-output-buffer-index stream)))
	(cond ((> (+ index length)
		  ts-stream-output-buffer-size)
	       (%ts-stream-flsbuf stream)
	       (wire:remote wire
		 (ts-buffer-output-string (ts-stream-typescript stream)
					  (subseq string start end)))
	       (when newline
		 (wire:wire-force-output wire)))
	      (t
	       (replace (the simple-string (ts-stream-output-buffer stream))
			string
			:start1 index
			:end1 (+ index length)
			:start2 start
			:end2 end)
	       (incf (ts-stream-output-buffer-index stream)
		     length)
	       (when newline
		 (%ts-stream-flsbuf stream)
		 (wire:wire-force-output wire))))))
    (setf (ts-stream-char-pos stream)
	  (if newline
	    (- end newline 1)
	    (+ (ts-stream-char-pos stream)
	       length)))))

;;; %TS-STREAM-UNREAD --- internal.
;;;
;;; Unread a single character.
;;; 
(defun %ts-stream-unread (stream char)
  (system:without-interrupts
    (cond ((and (ts-stream-current-input stream)
		(> (ts-stream-input-read-index stream) 0))
	   (setf (schar (car (ts-stream-current-input stream))
			(decf (ts-stream-input-read-index stream)))
		 char))
	  (t
	   (push (string char) (ts-stream-current-input stream))
	   (setf (ts-stream-input-read-index stream) 0)))))

;;; %TS-STREAM-CLOSE --- internal.
;;;
;;; Can't do much, 'cause the wire is shared.
;;; 
(defun %ts-stream-close (stream abort)
  (unless abort
    (force-output stream))
  (lisp::set-closed-flame stream))

;;; %TS-STREAM-CLEAR-INPUT --- internal.
;;;
;;; Pass the request to the editor and clear any buffered input.
;;;
(defun %ts-stream-clear-input (stream)
  (when (ts-stream-wire stream)
    (wire:remote-value (ts-stream-wire stream)
      (ts-buffer-clear-input (ts-stream-typescript stream))))
  (system:without-interrupts
    (setf (ts-stream-current-input stream) nil
	  (ts-stream-input-read-index stream) 0)))

;;; %TS-STREAM-MISC --- internal.
;;;
;;; The misc stream method.
;;; 
(defun %ts-stream-misc (stream operation &optional arg1 arg2)
  (case operation
    (:read-line
     (%ts-stream-read-line stream arg1 arg2))
    (:listen
     (%ts-stream-listen stream))
    (:unread
     (%ts-stream-unread stream arg1))
    (:close
     (%ts-stream-close stream arg1))
    (:clear-input
     (%ts-stream-clear-input stream)
     t)
    (:finish-output
     (when (ts-stream-wire stream)
       (%ts-stream-flsbuf stream)
       ;; Note: for the return value to come back,
       ;; all pending RPCs must have completed.
       (wire:remote-value (ts-stream-wire stream)
	 (ts-buffer-finish-output (ts-stream-typescript stream))))
     t)
    (:force-output
     (when (ts-stream-wire stream)
       (%ts-stream-flsbuf stream)
       (wire:wire-force-output (ts-stream-wire stream)))
     t)
    (:clear-output
     (setf (ts-stream-output-buffer-index stream) 0)
     t)
    (:element-type
     'char-string)
    (:charpos
     (ts-stream-char-pos stream))
    (:line-length
     (wire:remote-value (ts-stream-wire stream)
       (ts-buffer-line-length (ts-stream-typescript stream))))))
