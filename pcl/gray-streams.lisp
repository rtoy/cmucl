;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/gray-streams.lisp,v 1.1 1998/05/05 00:33:20 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Gray streams implementation for CMUCL.
;;;

(in-package "LISP")



(fmakunbound 'stream-element-type)

(defgeneric stream-element-type (stream)
  (:documentation "Returns a type specifier for the kind of object returned by the Stream."))

(defmethod stream-element-type ((stream lisp-stream))
  (funcall (lisp-stream-misc stream) stream :element-type))

(defmethod stream-element-type ((stream fundamental-character-stream))
  'character)



(defgeneric pcl-open-stream-p (stream)
  (:documentation "Return true if Stream is not closed."))

(defmethod pcl-open-stream-p ((stream lisp-stream))
  (not (eq (lisp-stream-in stream) #'closed-flame)))

(defmethod pcl-open-stream-p ((stream fundamental-stream))
  nil)

;;; Bootstrapping hack.
(pcl-open-stream-p (make-string-output-stream))
(setf (fdefinition 'open-stream-p) #'pcl-open-stream-p)




(defgeneric pcl-close (stream &key abort)
  (:documentation "Closes the given Stream.  No more I/O may be performed, but inquiries
  may still be made.  If :Abort is non-nil, an attempt is made to clean
  up the side effects of having created the stream."))

(defmethod pcl-close ((stream lisp-stream) &key abort)
  (when (open-stream-p stream)
    (funcall (lisp-stream-misc stream) stream :close abort))
  t)

(setf (fdefinition 'close) #'pcl-close)



(fmakunbound 'input-stream-p)

(defgeneric input-stream-p (stream)
  (:documentation "Returns non-nil if the given Stream can perform input operations."))

(defmethod input-stream-p ((stream lisp-stream))
  (and (not (eq (lisp-stream-in stream) #'closed-flame))
       (or (not (eq (lisp-stream-in stream) #'ill-in))
	   (not (eq (lisp-stream-bin stream) #'ill-bin)))))

(defmethod input-stream-p ((stream fundamental-input-stream))
  t)



(fmakunbound 'output-stream-p)

(defgeneric output-stream-p (stream)
  (:documentation "Returns non-nil if the given Stream can perform output operations."))

(defmethod output-stream-p ((stream lisp-stream))
  (and (not (eq (lisp-stream-in stream) #'closed-flame))
       (or (not (eq (lisp-stream-out stream) #'ill-out))
	   (not (eq (lisp-stream-bout stream) #'ill-bout)))))

(defmethod output-stream-p ((stream fundamental-output-stream))
  t)


;;; Character input streams.

(defgeneric stream-read-char (stream)
  (:documentation "Reads one character, :eof on end of file."))

(defgeneric stream-unread-char (stream character)
  (:documentation "Unreads one character, and returns nil."))

(defgeneric stream-read-char-no-hang (stream)
  (:documentation "Reads either a character, or NIL is none is available,
  and :eof on end of file."))

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (stream-read-char stream))

(defgeneric stream-peek-char (stream)
  (:documentation "Reads either a character or :eof without removing the character
  from the stream."))

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof)
      (stream-unread-char stream char))
    char))

(defgeneric stream-listen (stream)
  (:documentation "Return true if input is available, otherwise false."))

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (when (characterp char)
      (stream-unread-char stream char)
      char)))

(defgeneric stream-read-line (stream)
  (:documentation "Reads the first line, and returns a string and T as the second
  value if the line was terminated by end-of-file."))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (let ((res (make-string 80))
	(len 80)
	(index 0))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
	      (return (values (shrink-vector res index) t)))
	     (t
	      (when (char= ch #\newline)
		(return (values (shrink-vector res index) nil)))
	      (when (= index len)
		(setq len (* len 2))
		(let ((new (make-string len)))
		  (replace new res)
		  (setq res new)))
	      (setf (schar res index) ch)
	      (incf index)))))))

(defgeneric stream-clear-input (stream)
  (:documentation "Clears buffered input, and returns Nil."))

(defmethod stream-clear-input ((stream fundamental-character-input-stream))
  nil)


;;; Character output streams.

(defgeneric stream-write-char (stream character)
  (:documentation "Outputs the Character to the Stream."))

(defgeneric stream-line-column (stream)
  (:documentation "Return the current column number for the stream or Nil."))

;;; Stream-line-length is a CMUCL extension to Gray streams.
(defgeneric stream-line-length (stream)
  (:documentation "Return the stream line length or Nil."))

(defmethod stream-line-length ((stream fundamental-character-output-stream))
  nil)

(defgeneric stream-start-line-p (stream)
  (:documentation "Return true when at the start of a line otherwise false."))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (eql (stream-line-column stream) 0))

(defgeneric stream-write-string (stream string &optional (start 0) end)
  (:documentation "Outputs the String to the given Stream."))

(defmethod stream-write-string ((stream fundamental-character-output-stream)
				string &optional (start 0) end)
  (declare (string string)
	   (fixnum start))
  (let ((end (or end (length string))))
    (declare (fixnum end))
    (do ((pos start (1+ pos)))
	((>= pos end))
      (declare (type index pos))
      (stream-write-char stream (aref string pos))))
  string)

(defgeneric stream-terpri (stream)
  (:documentation "Outputs a new line to the Stream."))

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline))

(defgeneric stream-fresh-line (stream)
  (:documentation "Outputs a new line to the Stream if it is not positioned at the begining of
   a line.  Returns T if it output a new line, nil otherwise."))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defgeneric stream-finish-output (stream)
  (:documentation "Attempts to ensure that all output sent to the Stream has reached its
   destination, and only then returns false."))

(defmethod stream-finish-output ((stream fundamental-output-stream))
  nil)

(defgeneric stream-force-output (stream)
  (:documentation "Attempts to force any buffered output to be sent."))

(defmethod stream-force-output ((stream fundamental-output-stream))
  nil)

(defgeneric stream-clear-output (stream)
  (:documentation "Clears the given output Stream."))

(defmethod stream-clear-output ((stream fundamental-output-stream))
  nil)

(defgeneric stream-advance-to-column (stream column)
  (:documentation "Write enough space to the stream so that the next character is
  written to the given column. Returns true if successful, or false if not
  supported."))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream)
				     column)
  (let ((current-column (stream-line-column stream)))
    (when current-column
      (let ((fill (- column current-column)))
	(dotimes (i fill)
	  (stream-write-char stream #\Space)))
      T)))


;;; Binary streams.

(defgeneric stream-read-byte (stream)
  (:documentation "Returns an integer of :eof."))

(defgeneric stream-write-byte (stream integer)
  (:documentation "Writes integer to stream, returning integer."))


;;; Example character output stream encapsulating a lisp-stream.
(defun make-character-output-stream (lisp-stream)
  (declare (type lisp-stream lisp-stream))
  (make-instance 'character-output-stream :lisp-stream lisp-stream))

(defmethod open-stream-p ((stream character-output-stream))
  (open-stream-p (character-output-stream-lisp-stream stream)))

(defmethod close ((stream character-output-stream) &key abort)
  (close (character-output-stream-lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream character-output-stream))
  (input-stream-p (character-output-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-output-stream))
  (output-stream-p (character-output-stream-lisp-stream stream)))

(defmethod stream-write-char ((stream character-output-stream) character)
  (write-char character (character-output-stream-lisp-stream stream)))

(defmethod stream-line-column ((stream character-output-stream))
  (charpos (character-output-stream-lisp-stream stream)))

(defmethod stream-line-length ((stream character-output-stream))
  (line-length (character-output-stream-lisp-stream stream)))

(defmethod stream-finish-output ((stream character-output-stream))
  (finish-output (character-output-stream-lisp-stream stream)))

(defmethod stream-force-output ((stream character-output-stream))
  (force-output (character-output-stream-lisp-stream stream)))

(defmethod stream-clear-output ((stream character-output-stream))
  (clear-output (character-output-stream-lisp-stream stream)))


;;; Example character input stream encapsulating a lisp-stream.

(defun make-character-input-stream (lisp-stream)
  (declare (type lisp-stream lisp-stream))
  (make-instance 'character-input-stream :lisp-stream lisp-stream))

(defmethod open-stream-p ((stream character-input-stream))
  (open-stream-p (character-input-stream-lisp-stream stream)))

(defmethod close ((stream character-input-stream) &key abort)
  (close (character-input-stream-lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream character-input-stream))
  (input-stream-p (character-input-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-input-stream))
  (output-stream-p (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char ((stream character-input-stream))
  (read-char (character-input-stream-lisp-stream stream)))

(defmethod stream-unread-char ((stream character-input-stream) character)
  (unread-char character (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char-no-hang ((stream character-input-stream))
  (read-char-no-hang (character-input-stream-lisp-stream stream) nil :eof))

#+nil
(defmethod stream-peek-char ((stream character-input-stream))
  (peek-char nil (character-input-stream-lisp-stream stream) nil :eof))

#+nil
(defmethod stream-listen ((stream character-input-stream))
  (listen (character-input-stream-lisp-stream stream)))

(defmethod stream-clear-input ((stream character-input-stream))
  (clear-input (character-input-stream-lisp-stream stream)))
