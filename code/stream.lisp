;;; -*- Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/stream.lisp,v 1.30 1998/05/04 01:27:16 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stream functions for Spice Lisp.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
;;; This file contains the OS-independent stream functions.
;;;
(in-package "LISP")

(export '(broadcast-stream make-broadcast-stream broadcast-stream-streams 
	  synonym-stream make-synonym-stream synonym-stream-symbol
	  concatenated-stream make-concatenated-stream
	  concatenated-stream-streams
	  two-way-stream make-two-way-stream two-way-stream-input-stream
	  two-way-stream-output-stream
	  echo-stream make-echo-stream echo-stream-input-stream
	  echo-stream-output-stream
	  make-string-input-stream make-string-output-stream
	  get-output-stream-string stream-element-type input-stream-p
	  output-stream-p open-stream-p interactive-stream-p
	  open-stream-p close read-line read-char
	  unread-char peek-char listen read-char-no-hang clear-input read-byte
	  write-char write-string write-line terpri fresh-line
	  finish-output force-output clear-output write-byte
          stream streamp string-stream
	  *standard-input* *standard-output*
          *error-output* *query-io* *debug-io* *terminal-io* *trace-output*))

(in-package "SYSTEM")
(export '(make-indenting-stream read-n-bytes))

(in-package "EXT")

(export '(get-stream-command
	  stream-command stream-command-p stream-command-name 
	  stream-command-args make-stream-command make-case-frob-stream))

(in-package "LISP")

(deftype string-stream ()
  '(or string-input-stream string-output-stream
       fill-pointer-output-stream))

;;;; Standard streams:
;;;
;;; The initialization of these streams is performed by Stream-Init,
;;; which lives in the file of machine-specific stream functions.
;;;
(defvar *terminal-io* () "Terminal I/O stream.")
(defvar *standard-input* () "Default input stream.")
(defvar *standard-output* () "Default output stream.")
(defvar *error-output* () "Error output stream.")
(defvar *query-io* () "Query I/O stream.")
(defvar *trace-output* () "Trace output stream.")
(defvar *debug-io* () "Interactive debugging stream.")

(defun ill-in (stream &rest ignore)
  (declare (ignore ignore))
  (error "~S is not a character input stream." stream))
(defun ill-out (stream &rest ignore)
  (declare (ignore ignore))
  (error "~S is not a character output stream." stream))
(defun ill-bin (stream &rest ignore)
  (declare (ignore ignore))
  (error "~S is not a binary input stream." stream))
(defun ill-bout (stream &rest ignore)
  (declare (ignore ignore))
  (error "~S is not a binary output stream." stream))
(defun closed-flame (stream &rest ignore)
  (declare (ignore ignore))
  (error "~S is closed." stream))
(defun do-nothing (&rest ignore)
  (declare (ignore ignore)))

(defun %print-stream (structure stream d)
  (declare (ignore d structure))
  (write-string "#<Bare Stream>" stream))

;;; HOW THE STREAM STRUCTURE IS USED:
;;;
;;;    Many of the slots of the stream structure contain functions
;;; which are called to perform some operation on the stream.  Closed
;;; streams have #'Closed-Flame in all of their function slots.  If
;;; one side of an I/O or echo stream is closed, the whole stream is
;;; considered closed.  The functions in the operation slots take
;;; arguments as follows:
;;;
;;; In:			Stream, Eof-Errorp, Eof-Value
;;; Bin:		Stream, Eof-Errorp, Eof-Value
;;; N-Bin:		Stream, Buffer, Start, Numbytes, Eof-Errorp
;;; Out:		Stream, Character
;;; Bout:		Stream, Integer
;;; Sout:		Stream, String, Start, End
;;; Misc:		Stream, Operation, &Optional Arg1, Arg2
;;;
;;;    In order to save space, some of the less common stream operations
;;; are handled by just one function, the Misc method.  This function
;;; is passed a keyword which indicates the operation to perform.
;;; The following keywords are used:
;;;  :listen 		- Return the following values:
;;; 			     t if any input waiting.
;;; 			     :eof if at eof.
;;; 			     nil if no input is available and not at eof.
;;;  :unread		- Unread the character Arg.
;;;  :close		- Do any stream specific stuff to close the stream.
;;;			  The methods are set to closed-flame by the close
;;;			  function, so that need not be done by this
;;;			  function.
;;;  :clear-input 	- Clear any unread input
;;;  :finish-output,
;;;  :force-output	- Cause output to happen
;;;  :clear-output	- Clear any undone output
;;;  :element-type 	- Return the type of element the stream deals wit<h.
;;;  :line-length	- Return the length of a line of output.
;;;  :charpos		- Return current output position on the line.
;;;  :file-length	- Return the file length of a file stream.
;;;  :file-position	- Return or change the current position of a file stream.
;;;  :file-name		- Return the name of an associated file.
;;;  :interactive-p     - Is this an interactive device?
;;;
;;;    In order to do almost anything useful, it is necessary to
;;; define a new type of structure that includes stream, so that the
;;; stream can have some state information.
;;;
;;; THE STREAM IN-BUFFER:
;;;
;;;    The In-Buffer in the stream holds characters or bytes that
;;; are ready to be read by some input function.  If there is any
;;; stuff in the In-Buffer, then the reading function can use it
;;; without calling any stream method.  Any stream may put stuff in
;;; the In-Buffer, and may also assume that any input in the In-Buffer
;;; has been consumed before any in-method is called.  If a text
;;; stream has in In-Buffer, then the first character should not be
;;; used to buffer normal input so that it is free for unreading into.
;;;
;;;    The In-Buffer slot is a vector In-Buffer-Length long.  The
;;; In-Index is the index in the In-Buffer of the first available
;;; object.  The available objects are thus between In-Index and the
;;; length of the In-Buffer.
;;;
;;;    When this buffer is only accessed by the normal stream
;;; functions, the number of function calls is halved, thus
;;; potentially doubling the speed of simple operations.  If the
;;; Fast-Read-Char and Fast-Read-Byte macros are used, nearly all
;;; function call overhead is removed, vastly speeding up these
;;; important operations.
;;;
;;;    If a stream does not have an In-Buffer, then the In-Buffer slot
;;; must be nil, and the In-Index must be In-Buffer-Length.  These are
;;; the default values for the slots. 


;;; Stream manipulation functions.

(defun input-stream-p (stream)
  "Returns non-nil if the given Stream can perform input operations."
  (and (lisp-stream-p stream)
       (not (eq (lisp-stream-in stream) #'closed-flame))
       (or (not (eq (lisp-stream-in stream) #'ill-in))
	   (not (eq (lisp-stream-bin stream) #'ill-bin)))))

(defun output-stream-p (stream)
  "Returns non-nil if the given Stream can perform output operations."
  (and (lisp-stream-p stream)
       (not (eq (lisp-stream-in stream) #'closed-flame))
       (or (not (eq (lisp-stream-out stream) #'ill-out))
	   (not (eq (lisp-stream-bout stream) #'ill-bout)))))

(defun open-stream-p (stream)
  "Return true if Stream is not closed."
  (declare (type stream stream))
  (not (eq (lisp-stream-in stream) #'closed-flame)))

(defun stream-element-type (stream)
  "Returns a type specifier for the kind of object returned by the Stream."
  (declare (type stream stream))
  (funcall (lisp-stream-misc stream) stream :element-type))

(defun interactive-stream-p (stream)
  "Return true if Stream does I/O on a terminal or other interactive device."
  (declare (type stream stream))
  (funcall (lisp-stream-misc stream) stream :interactive-p))

(defun open-stream-p (stream)
  "Return true if and only if STREAM has not been closed."
  (declare (type stream stream))
  (not (eq (lisp-stream-in stream) #'closed-flame)))

(defun close (stream &key abort)
  "Closes the given Stream.  No more I/O may be performed, but inquiries
  may still be made.  If :Abort is non-nil, an attempt is made to clean
  up the side effects of having created the stream."
  (declare (type stream stream))
  (when (open-stream-p stream)
    (funcall (lisp-stream-misc stream) stream :close abort))
  t)

(defun set-closed-flame (stream)
  (setf (lisp-stream-in stream) #'closed-flame)
  (setf (lisp-stream-bin stream) #'closed-flame)
  (setf (lisp-stream-n-bin stream) #'closed-flame)
  (setf (lisp-stream-in stream) #'closed-flame)
  (setf (lisp-stream-out stream) #'closed-flame)
  (setf (lisp-stream-bout stream) #'closed-flame)
  (setf (lisp-stream-sout stream) #'closed-flame)
  (setf (lisp-stream-misc stream) #'closed-flame))


;;;; File position and file length.

;;; File-Position  --  Public
;;;
;;;    Call the misc method with the :file-position operation.
;;;
(defun file-position (stream &optional position)
  "With one argument returns the current position within the file
   File-Stream is open to.  If the second argument is supplied, then
   this becomes the new file position.  The second argument may also
   be :start or :end for the start and end of the file, respectively."
  (declare (stream stream)
	   (type (or index (member nil :start :end)) position))
  (cond
   (position
    (setf (lisp-stream-in-index stream) in-buffer-length)
    (funcall (lisp-stream-misc stream) stream :file-position position))
   (t
    (let ((res (funcall (lisp-stream-misc stream) stream :file-position nil)))
      (when res (- res (- in-buffer-length (lisp-stream-in-index stream))))))))


;;; File-Length  --  Public
;;;
;;;    Like File-Position, only use :file-length.
;;;
(defun file-length (stream)
  "This function returns the length of the file that File-Stream is open to."
  (declare (stream stream))
  (funcall (lisp-stream-misc stream) stream :file-length))


;;; Input functions:

(defun read-line (&optional (stream *standard-input*) (eof-errorp t) eof-value
			    recursive-p)
  "Returns a line of text read from the Stream as a string, discarding the
  newline character."
  (declare (ignore recursive-p))
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (prepare-for-fast-read-char stream
         (let ((res (make-string 80))
	       (len 80)
	       (index 0))
	   (loop
	    (let ((ch (fast-read-char nil nil)))
	      (cond (ch
		     (when (char= ch #\newline)
		       (done-with-fast-read-char)
		       (return (values (shrink-vector res index) nil)))
		     (when (= index len)
		       (setq len (* len 2))
		       (let ((new (make-string len)))
			 (replace new res)
			 (setq res new)))
		     (setf (schar res index) ch)
		     (incf index))
		    ((zerop index)
		     (done-with-fast-read-char)
		     (return (values (eof-or-lose stream eof-errorp eof-value)
				     t)))
		    ;; since fast-read-char hit already the eof char, we
		    ;; shouldn't do another read-char
		    (t
		     (done-with-fast-read-char)
		     (return (values (shrink-vector res index) t)))))))))
      (fundamental-stream
       (multiple-value-bind (string eof)
	   (stream-read-line stream)
	 (if (and eof (zerop (length string)))
	     (values (eof-or-lose stream eof-errorp eof-value) t)
	     (values string eof)))))))

;;; We proclaim them inline here, then proclaim them notinline at EOF,
;;; so, except in this file, they are not inline by default, but they can be.
;;;
(proclaim '(inline read-char unread-char read-byte listen))
(defun read-char (&optional (stream *standard-input*) (eof-errorp t) eof-value
			    recursive-p)
  "Inputs a character from Stream and returns it."
  (declare (ignore recursive-p))
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (prepare-for-fast-read-char stream
         (prog1
	     (fast-read-char eof-errorp eof-value)
	   (done-with-fast-read-char))))
      (fundamental-stream
       (let ((char (stream-read-char stream)))
	 (if (eq char :eof)
	     (eof-or-lose stream eof-errorp eof-value)
	     char))))))

(defun unread-char (character &optional (stream *standard-input*))
  "Puts the Character back on the front of the input Stream."
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (let ((index (1- (lisp-stream-in-index stream)))
	     (buffer (lisp-stream-in-buffer stream)))
	 (declare (fixnum index))
	 (when (minusp index) (error "Nothing to unread."))
	 (cond (buffer
		(setf (aref buffer index) (char-code character))
		(setf (lisp-stream-in-index stream) index))
	       (t
		(funcall (lisp-stream-misc stream) stream 
			 :unread character)))))
      (fundamental-stream
       (stream-unread-char stream character))))
  nil)

(defun peek-char (&optional (peek-type nil) (stream *standard-input*)
			    (eof-errorp t) eof-value recursive-p)
  "Peeks at the next character in the input Stream.  See manual for details."
  (declare (ignore recursive-p))
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (let ((char (read-char stream eof-errorp eof-value)))
	 (cond ((eq char eof-value) char)
	       ((characterp peek-type)
		(do ((char char (read-char stream eof-errorp eof-value)))
		    ((or (eq char eof-value) (char= char peek-type))
		     (unless (eq char eof-value)
		       (unread-char char stream))
		     char)))
	       ((eq peek-type t)
		(do ((char char (read-char stream eof-errorp eof-value)))
		    ((or (eq char eof-value) (not (whitespace-char-p char)))
		     (unless (eq char eof-value)
		       (unread-char char stream))
		     char)))
	       (t
		(unread-char char stream)
		char))))
      (fundamental-stream
       (cond ((characterp peek-type)
	      (do ((char (stream-read-char stream) (stream-read-char stream)))
		  ((or (eq char :eof) (char= char peek-type))
		   (cond ((eq char :eof)
			  (eof-or-lose stream eof-errorp eof-value))
			 (t
			  (stream-unread-char stream char)
			  char)))))
	     ((eq peek-type t)
	      (do ((char (stream-read-char stream) (stream-read-char stream)))
		  ((or (eq char :eof) (not (whitespace-char-p char)))
		   (cond ((eq char :eof)
			  (eof-or-lose stream eof-errorp eof-value))
			 (t
			  (stream-unread-char stream char)
			  char)))))
	     (t
	      (let ((char (stream-peek-char stream)))
		(if (eq char :eof)
		    (eof-or-lose stream eof-errorp eof-value)
		    char))))))))

(defun listen (&optional (stream *standard-input*))
  "Returns T if a character is availible on the given Stream."
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (or (/= (the fixnum (lisp-stream-in-index stream)) in-buffer-length)
	   ;; Test for t explicitly since misc methods return :eof sometimes.
	   (eq (funcall (lisp-stream-misc stream) stream :listen) t)))
      (fundamental-stream
       (stream-listen stream)))))

(defun read-char-no-hang (&optional (stream *standard-input*)
				    (eof-errorp t) eof-value recursive-p)
  "Returns the next character from the Stream if one is availible, or nil."
  (declare (ignore recursive-p))
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (if (funcall (lisp-stream-misc stream) stream :listen)
	   ;; On t or :eof get READ-CHAR to do the work.
	   (read-char stream eof-errorp eof-value)
	   nil))
      (fundamental-stream
       (let ((char (stream-read-char-no-hang stream)))
	 (if (eq char :eof)
	     (eof-or-lose stream eof-errorp eof-value)
	     char))))))


(defun clear-input (&optional (stream *standard-input*))
  "Clears any buffered input associated with the Stream."
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (setf (lisp-stream-in-index stream) in-buffer-length)
       (funcall (lisp-stream-misc stream) stream :clear-input))
      (fundamental-stream
       (stream-clear-input stream))))
  nil)

(defun read-byte (stream &optional (eof-errorp t) eof-value)
  "Returns the next byte of the Stream."
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (prepare-for-fast-read-byte stream
         (prog1
	     (fast-read-byte eof-errorp eof-value t)
	   (done-with-fast-read-byte))))
      (fundamental-stream
       (let ((char (stream-read-byte stream)))
	 (if (eq char :eof)
	     (eof-or-lose stream eof-errorp eof-value)
	     char))))))

(defun read-n-bytes (stream buffer start numbytes &optional (eof-errorp t))
  "Reads Numbytes bytes into the Buffer starting at Start, returning the number
   of bytes read.
   -- If EOF-ERROR-P is true, an END-OF-FILE condition is signalled if
      end-of-file is encountered before Count bytes have been read.
   -- If EOF-ERROR-P is false, READ-N-BYTES reads as much data is currently
      available (up to count bytes.)  On pipes or similar devices, this
      function returns as soon as any adata is available, even if the amount
      read is less than Count and eof has not been hit."
  (declare (type lisp-stream stream)
	   (type index numbytes start)
	   (type (or (simple-array * (*)) system-area-pointer) buffer))
  (let* ((stream (stream-synonym-of stream lisp-stream))
	 (in-buffer (lisp-stream-in-buffer stream))
	 (index (lisp-stream-in-index stream))
	 (num-buffered (- in-buffer-length index)))
    (declare (fixnum index num-buffered))
    (cond
     ((not in-buffer)
      (funcall (lisp-stream-n-bin stream) stream buffer start numbytes eof-errorp))
     ((<= numbytes num-buffered)
      (%primitive byte-blt in-buffer index buffer start (+ start numbytes))
      (setf (lisp-stream-in-index stream) (+ index numbytes))
      numbytes)
     (t
      (let ((end (+ start num-buffered)))
	(%primitive byte-blt in-buffer index buffer start end)
	(setf (lisp-stream-in-index stream) in-buffer-length)
	(+ (funcall (lisp-stream-n-bin stream) stream buffer end
		    (- numbytes num-buffered)
		    eof-errorp)
	   num-buffered))))))


;;; Amount of space we leave at the start of the in-buffer for unreading.  4
;;; instead of 1 to allow word-aligned copies.
;;;
(defconstant in-buffer-extra 4)

;;; FAST-READ-CHAR-REFILL  --  Interface
;;;
;;;    This function is called by the fast-read-char expansion to refill the
;;; in-buffer for text streams.  There is definitely an in-buffer, and hence
;;; myst be an n-bin method.
;;;
(defun fast-read-char-refill (stream eof-errorp eof-value)
  (let* ((ibuf (lisp-stream-in-buffer stream))
	 (count (funcall (lisp-stream-n-bin stream) stream
			 ibuf in-buffer-extra
			 (- in-buffer-length in-buffer-extra)
			 nil))
	 (start (- in-buffer-length count)))
    (declare (type index start count))
    (cond ((zerop count)
	   (setf (lisp-stream-in-index stream) in-buffer-length)
	   (funcall (lisp-stream-in stream) stream eof-errorp eof-value))
	  (t
	   (when (/= start in-buffer-extra)
	     (bit-bash-copy ibuf (+ (* in-buffer-extra vm:byte-bits)
				    (* vm:vector-data-offset vm:word-bits))
			    ibuf (+ (the index (* start vm:byte-bits))
				    (* vm:vector-data-offset vm:word-bits))
			    (* count vm:byte-bits)))
	   (setf (lisp-stream-in-index stream) (1+ start))
	   (code-char (aref ibuf start))))))


;;; FAST-READ-BYTE-REFILL  --  Interface
;;;
;;;    Similar to FAST-READ-CHAR-REFILL, but we don't have to leave room for
;;; unreading.
;;;
(defun fast-read-byte-refill (stream eof-errorp eof-value)
  (let* ((ibuf (lisp-stream-in-buffer stream))
	 (count (funcall (lisp-stream-n-bin stream) stream
			 ibuf 0 in-buffer-length
			 nil))
	 (start (- in-buffer-length count)))
    (declare (type index start count))
    (cond ((zerop count)
	   (setf (lisp-stream-in-index stream) in-buffer-length)
	   (funcall (lisp-stream-bin stream) stream eof-errorp eof-value))
	  (t
	   (unless (zerop start)
	     (bit-bash-copy ibuf (* vm:vector-data-offset vm:word-bits)
			    ibuf (+ (the index (* start vm:byte-bits))
				    (* vm:vector-data-offset vm:word-bits))
			    (* count vm:byte-bits)))
	   (setf (lisp-stream-in-index stream) (1+ start))
	   (aref ibuf start)))))
  

;;; Output functions:

(defun write-char (character &optional (stream *standard-output*))
  "Outputs the Character to the Stream."
  (with-stream stream (lisp-stream-out character)
	       (stream-write-char character))
  character)

(defun terpri (&optional (stream *standard-output*))
  "Outputs a new line to the Stream."
  (with-stream stream (lisp-stream-out #\newline) (stream-terpri))
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  "Outputs a new line to the Stream if it is not positioned at the begining of
   a line.  Returns T if it output a new line, nil otherwise."
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (when (/= (or (charpos stream) 1) 0)
	 (funcall (lisp-stream-out stream) stream #\newline)
	 t))
      (fundamental-stream
       (stream-fresh-line stream)))))

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) (end (length (the vector string))))
  "Outputs the String to the given Stream."
  (write-string* string stream start end))

(defun write-string* (string &optional (stream *standard-output*)
			     (start 0) (end (length (the vector string))))
  (declare (fixnum start end))
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (if (array-header-p string)
	   (with-array-data ((data string) (offset-start start)
			     (offset-end end))
	     (funcall (lisp-stream-sout stream)
		      stream data offset-start offset-end))
	   (funcall (lisp-stream-sout stream) stream string start end))
       string)
      (fundamental-stream
       (stream-write-string stream string start end)))))

(defun write-line (string &optional (stream *standard-output*)
			  &key (start 0) (end (length string)))
  "Outputs the String to the given Stream, followed by a newline character."
  (write-line* string stream start end))

(defun write-line* (string &optional (stream *standard-output*)
			   (start 0) (end (length string)))
  (declare (fixnum start end))
  (let ((stream (stream-synonym-of stream)))
    (etypecase stream
      (lisp-stream
       (if (array-header-p string)
	   (with-array-data ((data string) (offset-start start)
			     (offset-end end))
	     (with-stream stream (lisp-stream-sout data offset-start
						   offset-end)))
	   (with-stream stream (lisp-stream-sout string start end)))
       (funcall (lisp-stream-out stream) stream #\newline))
      (fundamental-stream
       (stream-write-string stream string start end)
       (stream-write-char stream #\Newline)))
    string))

(defun charpos (&optional (stream *standard-output*))
  "Returns the number of characters on the current line of output of the given
  Stream, or Nil if that information is not availible."
  (with-stream stream (lisp-stream-misc :charpos) (stream-line-column)))

(defun line-length (&optional (stream *standard-output*))
  "Returns the number of characters that will fit on a line of output on the
  given Stream, or Nil if that information is not available."
  (with-stream stream (lisp-stream-misc :line-length) (stream-line-length)))

(defun finish-output (&optional (stream *standard-output*))
  "Attempts to ensure that all output sent to the Stream has reached its
   destination, and only then returns."
  (with-stream stream (lisp-stream-misc :finish-output) (stream-finish-output))
  nil)

(defun force-output (&optional (stream *standard-output*))
  "Attempts to force any buffered output to be sent."
  (with-stream stream (lisp-stream-misc :force-output) (stream-force-output))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  "Clears the given output Stream."
  (with-stream stream (lisp-stream-misc :clear-output) (stream-force-output))
  nil)

(defun write-byte (integer stream)
  "Outputs the Integer to the binary Stream."
  (with-stream stream (lisp-stream-bout integer) (stream-write-byte))
  integer)

;;;; Broadcast streams:

(defstruct (broadcast-stream (:include lisp-stream
				       (out #'broadcast-out)
				       (bout #'broadcast-bout)
				       (sout #'broadcast-sout)
				       (misc #'broadcast-misc))
			     (:print-function %print-broadcast-stream)
			     (:constructor make-broadcast-stream (&rest streams)))
  ;; This is a list of all the streams we broadcast to.
  (streams () :type list :read-only t))

(setf (documentation 'make-broadcast-stream 'function)
 "Returns an ouput stream which sends its output to all of the given streams.")

(defun %print-broadcast-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Broadcast Stream>" stream))

(macrolet ((out-fun (fun method &rest args)
	     `(defun ,fun (stream ,@args)
		(dolist (stream (broadcast-stream-streams stream))
		  (funcall (,method stream) stream ,@args)))))
  (out-fun broadcast-out lisp-stream-out char)
  (out-fun broadcast-bout lisp-stream-bout byte)
  (out-fun broadcast-sout lisp-stream-sout string start end))

(defun broadcast-misc (stream operation &optional arg1 arg2)
  (let ((streams (broadcast-stream-streams stream)))
    (case operation
      (:charpos
       (dolist (stream streams)
	 (let ((charpos (funcall (lisp-stream-misc stream) stream :charpos)))
	   (if charpos (return charpos)))))
      (:line-length
       (let ((min nil))
	 (dolist (stream streams min)
	   (let ((res (funcall (lisp-stream-misc stream) stream :line-length)))
	     (when res (setq min (if min (min res min) res)))))))
      (:element-type
       (let (res)
	 (dolist (stream streams (if (> (length res) 1) `(and ,@res) res))
	   (pushnew (funcall (lisp-stream-misc stream) stream :element-type) res
		    :test #'equal))))
      (:close)
      (t
       (let ((res nil))
	 (dolist (stream streams res)
	   (setq res (funcall (lisp-stream-misc stream) stream operation
			      arg1 arg2))))))))

;;;; Synonym Streams:

(defstruct (synonym-stream (:include lisp-stream
				     (in #'synonym-in)
				     (bin #'synonym-bin)
				     (n-bin #'synonym-n-bin)
				     (out #'synonym-out)
				     (bout #'synonym-bout)
				     (sout #'synonym-sout)
				     (misc #'synonym-misc))
			   (:print-function %print-synonym-stream)
			   (:constructor make-synonym-stream (symbol)))
  ;; This is the symbol, the value of which is the stream we are synonym to.
  (symbol nil :type symbol :read-only t))

(defun %print-synonym-stream (s stream d)
  (declare (ignore d))
  (format stream "#<Synonym Stream to ~S>" (synonym-stream-symbol s)))

(setf (documentation 'make-synonym-stream 'function)
  "Returns a stream which performs its operations on the stream which is the
   value of the dynamic variable named by Symbol.")

;;; The output simple output methods just call the corresponding method
;;; in the synonymed stream.
;;;
(macrolet ((out-fun (name slot &rest args)
	     `(defun ,name (stream ,@args)
		(declare (optimize (safety 1)))
		(let ((syn (symbol-value (synonym-stream-symbol stream))))
		  (funcall (,slot syn) syn ,@args)))))
  (out-fun synonym-out lisp-stream-out ch)
  (out-fun synonym-bout lisp-stream-bout n)
  (out-fun synonym-sout lisp-stream-sout string start end))


;;; Bind synonym stream to this so that SPIO can turn on the right frob in
;;; the icon when we are in a terminal input wait.
;;;
(defvar *previous-stream* nil)

;;; For the input methods, we just call the corresponding function on the
;;; synonymed stream.  These functions deal with getting input out of
;;; the In-Buffer if there is any.
;;;
(macrolet ((in-fun (name fun &rest args)
	     `(defun ,name (stream ,@args)
		(declare (optimize (safety 1)))
		(let ((*previous-stream* stream))
		  (,fun (symbol-value (synonym-stream-symbol stream)) ,@args)))))
  (in-fun synonym-in read-char eof-errorp eof-value)
  (in-fun synonym-bin read-byte eof-errorp eof-value)
  (in-fun synonym-n-bin read-n-bytes buffer start numbytes eof-errorp))


;;; Synonym-Misc  --  Internal
;;;
;;;    We have to special-case the operations which could look at stuff in
;;; the in-buffer.
;;;
(defun synonym-misc (stream operation &optional arg1 arg2)
  (declare (optimize (safety 1)))
  (let ((syn (symbol-value (synonym-stream-symbol stream)))
	(*previous-stream* stream))
    (case operation
      (:listen (or (/= (the fixnum (lisp-stream-in-index syn)) in-buffer-length)
		   (funcall (lisp-stream-misc syn) syn :listen)))
      (t
       (funcall (lisp-stream-misc syn) syn operation arg1 arg2)))))

;;;; Two-Way streams:

(defstruct (two-way-stream
	    (:include lisp-stream
		      (in #'two-way-in)
		      (bin #'two-way-bin)
		      (n-bin #'two-way-n-bin)
		      (out #'two-way-out)
		      (bout #'two-way-bout)
		      (sout #'two-way-sout)
		      (misc #'two-way-misc))
	    (:print-function %print-two-way-stream)
	    (:constructor make-two-way-stream (input-stream output-stream)))
  ;; We read from this stream...
  (input-stream (required-argument) :type stream :read-only t)
  ;; And write to this one
  (output-stream (required-argument) :type stream :read-only t))

(defun %print-two-way-stream (s stream d)
  (declare (ignore d))
  (format stream "#<Two-Way Stream, Input = ~S, Output = ~S>"
	  (two-way-stream-input-stream s)
	  (two-way-stream-output-stream s)))

(setf (documentation 'make-two-way-stream 'function)
  "Returns a bidirectional stream which gets its input from Input-Stream and
   sends its output to Output-Stream.")

(macrolet ((out-fun (name slot &rest args)
	     `(defun ,name (stream ,@args)
		(let ((syn (two-way-stream-output-stream stream)))
		  (funcall (,slot syn) syn ,@args)))))
  (out-fun two-way-out lisp-stream-out ch)
  (out-fun two-way-bout lisp-stream-bout n)
  (out-fun two-way-sout lisp-stream-sout string start end))

(macrolet ((in-fun (name fun &rest args)
	     `(defun ,name (stream ,@args)
		(force-output (two-way-stream-output-stream stream))
		(,fun (two-way-stream-input-stream stream) ,@args))))
  (in-fun two-way-in read-char eof-errorp eof-value)
  (in-fun two-way-bin read-byte eof-errorp eof-value)
  (in-fun two-way-n-bin read-n-bytes buffer start numbytes eof-errorp))

(defun two-way-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (in-method (lisp-stream-misc in))
	 (out (two-way-stream-output-stream stream))
	 (out-method (lisp-stream-misc out)))
    (case operation
      (:listen (or (/= (the fixnum (lisp-stream-in-index in)) in-buffer-length)
		   (funcall in-method in :listen)))
      ((:finish-output :force-output :clear-output)
       (funcall out-method out operation arg1 arg2))
      ((:clear-input :unread)
       (funcall in-method in operation arg1 arg2))
      (:element-type
       (let ((in-type (funcall in-method in :element-type))
	     (out-type (funcall out-method out :element-type)))
	 (if (equal in-type out-type)
	     in-type `(and ,in-type ,out-type))))
      (:close 
       (set-closed-flame stream))
      (t
       (or (funcall in-method in operation arg1 arg2)
	   (funcall out-method out operation arg1 arg2))))))

;;;; Concatenated Streams:

(defstruct (concatenated-stream
	    (:include lisp-stream
		      (in #'concatenated-in)
		      (bin #'concatenated-bin)
		      (misc #'concatenated-misc))
	    (:print-function %print-concatenated-stream)
	    (:constructor
	     make-concatenated-stream (&rest streams &aux (current streams))))
  ;; The car of this is the stream we are reading from now.
  current
  ;; This is a list of all the streams.  We need to remember them so that
  ;; we can close them.
  (streams nil :type list :read-only t))

(defun %print-concatenated-stream (s stream d)
  (declare (ignore d))
  (format stream "#<Concatenated Stream, Streams = ~S>"
	  (concatenated-stream-streams s)))

(setf (documentation 'make-concatenated-stream 'function)
  "Returns a stream which takes its input from each of the Streams in turn,
   going on to the next at EOF.")

(macrolet ((in-fun (name fun)
	     `(defun ,name (stream eof-errorp eof-value)
		(do ((current (concatenated-stream-current stream) (cdr current)))
		    ((null current)
		     (eof-or-lose stream eof-errorp eof-value))
		  (let* ((stream (car current))
			 (result (,fun stream nil nil)))
		    (when result (return result)))
		  (setf (concatenated-stream-current stream) current)))))
  (in-fun concatenated-in read-char)
  (in-fun concatenated-bin read-byte))

(defun concatenated-misc (stream operation &optional arg1 arg2)
  (let ((left (concatenated-stream-current stream)))
    (when left
      (let* ((current (car left))
	     (misc (lisp-stream-misc current)))
	(case operation
	  (:listen
	   (loop
	     (let ((stuff (funcall misc current :listen)))
	       (cond ((eq stuff :eof)
		      ;; Advance current, and try again.
		      (pop (concatenated-stream-current stream))
		      (setf current
			    (car (concatenated-stream-current stream)))
		      (unless current
			;; No further streams.  EOF.
			(return :eof))
		      (setf misc (lisp-stream-misc current)))
		     (stuff
		      ;; Stuff's available.
		      (return t))
		     (t
		      ;; Nothing available yet.
		      (return nil))))))
	  (:close
	   (set-closed-flame stream))
	  (t
	   (funcall misc current operation arg1 arg2)))))))

;;;; Echo Streams:

(defstruct (echo-stream
	    (:include two-way-stream
		      (in #'echo-in)
		      (bin #'echo-bin)
		      (misc #'echo-misc)
		      (n-bin #'ill-bin))
	    (:print-function %print-echo-stream)
	    (:constructor make-echo-stream (input-stream output-stream)))
  unread-stuff)


(macrolet ((in-fun (name fun out-slot &rest args)
	     `(defun ,name (stream ,@args)
		(or (pop (echo-stream-unread-stuff stream))
		    (let* ((in (echo-stream-input-stream stream))
			   (out (echo-stream-output-stream stream))
			   (result (,fun in ,@args)))
		      (funcall (,out-slot out) out result)
		      result)))))
  (in-fun echo-in read-char lisp-stream-out eof-errorp eof-value)
  (in-fun echo-bin read-byte lisp-stream-bout eof-errorp eof-value))

(defun echo-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (in-method (lisp-stream-misc in))
	 (out (two-way-stream-output-stream stream))
	 (out-method (lisp-stream-misc out)))
    (case operation
      (:listen (or (not (null (echo-stream-unread-stuff stream)))
		   (/= (the fixnum (lisp-stream-in-index in)) in-buffer-length)
		   (funcall in-method in :listen)))
      (:unread (push arg1 (echo-stream-unread-stuff stream)))
      (:element-type
       (let ((in-type (funcall in-method in :element-type))
	     (out-type (funcall out-method out :element-type)))
	 (if (equal in-type out-type)
	     in-type `(and ,in-type ,out-type))))
      (:close
       (set-closed-flame stream))
      (t
       (or (funcall in-method in operation arg1 arg2)
	   (funcall out-method out operation arg1 arg2))))))

(defun %print-echo-stream (s stream d)
  (declare (ignore d))
  (format stream "#<Echo Stream, Input = ~S, Output = ~S>"
	  (two-way-stream-input-stream s)
	  (two-way-stream-output-stream s)))

(setf (documentation 'make-echo-stream 'function)
  "Returns a bidirectional stream which gets its input from Input-Stream and
   sends its output to Output-Stream.  In addition, all input is echoed to
   the output stream")

;;;; String Input Streams:

(defstruct (string-input-stream
	     (:include lisp-stream
		       (in #'string-inch)
		       (bin #'string-binch)
		       (n-bin #'string-stream-read-n-bytes)
		       (misc #'string-in-misc))
	     (:print-function %print-string-input-stream)
					;(:constructor nil)
	     (:constructor internal-make-string-input-stream
			   (string current end)))
  (string nil :type simple-string)
  (current nil :type fixnum)
  (end nil :type fixnum))

(defun %print-string-input-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<String-Input Stream>" stream))
  
(defun string-inch (stream eof-errorp eof-value)
  (let ((string (string-input-stream-string stream))
	(index (string-input-stream-current stream)))
    (declare (simple-string string) (fixnum index))
    (cond ((= index (the fixnum (string-input-stream-end stream)))
	   (eof-or-lose stream eof-errorp eof-value))
	  (t
	   (setf (string-input-stream-current stream) (1+ index))
	   (aref string index)))))

(defun string-binch (stream eof-errorp eof-value)
  (let ((string (string-input-stream-string stream))
	(index (string-input-stream-current stream)))
    (declare (simple-string string) (fixnum index))
    (cond ((= index (the fixnum (string-input-stream-end stream)))
	   (eof-or-lose stream eof-errorp eof-value))
	  (t
	   (setf (string-input-stream-current stream) (1+ index))
	   (char-code (aref string index))))))

(defun string-stream-read-n-bytes (stream buffer start requested eof-errorp)
  (declare (type string-input-stream stream)
	   (type index start requested))
  (let ((string (string-input-stream-string stream))
	(index (string-input-stream-current stream))
	(end (string-input-stream-end stream)))
    (declare (simple-string string) (fixnum index end))
    (cond ((>= (+ index requested) end)
	   (eof-or-lose stream eof-errorp nil))
	  (t
	   (setf (string-input-stream-current stream) (+ index requested))
	   (system:without-gcing
	    (system-area-copy (vector-sap string)
			      (* index vm:byte-bits)
			      (if (typep buffer 'system-area-pointer)
				  buffer
				  (vector-sap buffer))
			      (* start vm:byte-bits)
			      (* requested vm:byte-bits)))
	   requested))))

(defun string-in-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if arg1
	 (setf (string-input-stream-current stream) arg1)
	 (string-input-stream-current stream)))
    (:file-length (length (string-input-stream-string stream)))
    (:unread (decf (string-input-stream-current stream)))
    (:listen (or (/= (the fixnum (string-input-stream-current stream))
		     (the fixnum (string-input-stream-end stream)))
		 :eof))
    (:element-type 'base-char)))
  
(defun make-string-input-stream (string &optional
					(start 0) (end (length string)))
  "Returns an input stream which will supply the characters of String between
  Start and End in order."
  (declare (type string string)
	   (type index start)
	   (type (or index null) end))
  (internal-make-string-input-stream (coerce string 'simple-string)
				     start end))

;;;; String Output Streams:

(defstruct (string-output-stream
	    (:include lisp-stream
		      (out #'string-ouch)
		      (sout #'string-sout)
		      (misc #'string-out-misc))
	    (:print-function %print-string-output-stream)
	    (:constructor make-string-output-stream ()))
  ;; The string we throw stuff in.
  (string (make-string 40) :type simple-string)
  ;; Index of the next location to use.
  (index 0 :type fixnum))

(defun %print-string-output-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<String-Output Stream>" stream))

(setf (documentation 'make-string-output-stream 'function)
  "Returns an Output stream which will accumulate all output given it for
   the benefit of the function Get-Output-Stream-String.")

(defun string-ouch (stream character)
  (let ((current (string-output-stream-index stream))
	(workspace (string-output-stream-string stream)))
    (declare (simple-string workspace) (fixnum current))
    (if (= current (the fixnum (length workspace)))
	(let ((new-workspace (make-string (* current 2))))
	  (replace new-workspace workspace)
	  (setf (aref new-workspace current) character)
	  (setf (string-output-stream-string stream) new-workspace))
	(setf (aref workspace current) character))
    (setf (string-output-stream-index stream) (1+ current))))

(defun string-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (let* ((current (string-output-stream-index stream))
	 (length (- end start))
	 (dst-end (+ length current))
	 (workspace (string-output-stream-string stream)))
    (declare (simple-string workspace)
	     (fixnum current length dst-end))
    (if (> dst-end (the fixnum (length workspace)))
	(let ((new-workspace (make-string (+ (* current 2) length))))
	  (replace new-workspace workspace :end2 current)
	  (replace new-workspace string
		   :start1 current :end1 dst-end
		   :start2 start :end2 end)
	  (setf (string-output-stream-string stream) new-workspace))
	(replace workspace string
		 :start1 current :end1 dst-end
		 :start2 start :end2 end))
    (setf (string-output-stream-index stream) dst-end)))

(defun string-out-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if (null arg1)
	 (string-output-stream-index stream)))
    (:charpos
     (do ((index (1- (the fixnum (string-output-stream-index stream)))
		 (1- index))
	  (count 0 (1+ count))
	  (string (string-output-stream-string stream)))
	 ((< index 0) count)
       (declare (simple-string string)
		(fixnum index count))
       (if (char= (schar string index) #\newline)
	   (return count))))
    (:element-type 'base-char)))

(defun get-output-stream-string (stream)
  "Returns a string of all the characters sent to a stream made by
   Make-String-Output-Stream since the last call to this function."
  (declare (type string-output-stream stream))
  (let* ((length (string-output-stream-index stream))
	 (result (make-string length)))
    (replace result (string-output-stream-string stream))
    (setf (string-output-stream-index stream) 0)
    result))

(defun dump-output-stream-string (in-stream out-stream)
  "Dumps the characters buffer up in the In-Stream to the Out-Stream as
  Get-Output-Stream-String would return them."
  (write-string* (string-output-stream-string in-stream) out-stream
		 0 (string-output-stream-index in-stream))
  (setf (string-output-stream-index in-stream) 0))

;;;; Fill-pointer streams:
;;;
;;;    Fill pointer string output streams are not explicitly mentioned in
;;; the CLM, but they are required for the implementation of With-Output-To-String.

(defstruct (fill-pointer-output-stream
 	    (:include lisp-stream
		      (out #'fill-pointer-ouch)
		      (sout #'fill-pointer-sout)
		      (misc #'fill-pointer-misc))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (write-string "#<Fill-Pointer String Output Stream>" stream)))
	    (:constructor make-fill-pointer-output-stream (string)))
  ;; The string we throw stuff in.
  string)

 
(defun fill-pointer-ouch (stream character)
  (let* ((buffer (fill-pointer-output-stream-string stream))
	 (current (fill-pointer buffer))
	 (current+1 (1+ current)))
    (declare (fixnum current))
    (with-array-data ((workspace buffer) (start) (end))
      (declare (simple-string workspace))
      (let ((offset-current (+ start current)))
	(declare (fixnum offset-current))
	(if (= offset-current end)
	    (let* ((new-length (* current 2))
		   (new-workspace (make-string new-length)))
	      (declare (simple-string new-workspace))
	      (%primitive byte-blt workspace start new-workspace 0 current)
	      (setf workspace new-workspace)
	      (setf offset-current current)
	      (set-array-header buffer workspace new-length
				current+1 0 new-length nil))
	    (setf (fill-pointer buffer) current+1))
	(setf (schar workspace offset-current) character)))
    current+1))


(defun fill-pointer-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (let* ((buffer (fill-pointer-output-stream-string stream))
	 (current (fill-pointer buffer))
	 (string-len (- end start))
	 (dst-end (+ string-len current)))
    (declare (fixnum current dst-end string-len))
    (with-array-data ((workspace buffer) (dst-start) (dst-length))
      (declare (simple-string workspace))
      (let ((offset-dst-end (+ dst-start dst-end))
	    (offset-current (+ dst-start current)))
	(declare (fixnum offset-dst-end offset-current))
	(if (> offset-dst-end dst-length)
	    (let* ((new-length (+ (the fixnum (* current 2)) string-len))
		   (new-workspace (make-string new-length)))
	      (declare (simple-string new-workspace))
	      (%primitive byte-blt workspace dst-start new-workspace 0 current)
	      (setf workspace new-workspace)
	      (setf offset-current current)
	      (setf offset-dst-end dst-end)
	      (set-array-header buffer workspace new-length
				dst-end 0 new-length nil))
	    (setf (fill-pointer buffer) dst-end))
	(%primitive byte-blt string start
		    workspace offset-current offset-dst-end)))
    dst-end))


(defun fill-pointer-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos
     (let* ((buffer (fill-pointer-output-stream-string stream))
	    (current (fill-pointer buffer)))
       (with-array-data ((string buffer) (start) (end current))
	 (declare (simple-string string) (ignore start))
	 (let ((found (position #\newline string :test #'char=
				:end end :from-end t)))
	   (if found
	       (- end (the fixnum found))
	       current)))))
     (:element-type 'base-char)))

;;;; Indenting streams:

(defstruct (indenting-stream (:include lisp-stream
				       (out #'indenting-out)
				       (sout #'indenting-sout)
				       (misc #'indenting-misc))
			     (:print-function %print-indenting-stream)
			     (:constructor make-indenting-stream (stream)))
  ;; The stream we're based on:
  stream
  ;; How much we indent on each line:
  (indentation 0))

(setf (documentation 'make-indenting-stream 'function)
 "Returns an ouput stream which indents its output by some amount.")

(defun %print-indenting-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Indenting Stream>" stream))

;;; Indenting-Indent writes the right number of spaces needed to indent output on
;;; the given Stream based on the specified Sub-Stream.

(defmacro indenting-indent (stream sub-stream)
  `(do ((i 0 (+ i 60))
	(indentation (indenting-stream-indentation ,stream)))
       ((>= i indentation))
     (write-string*
      "                                                            "
      ,sub-stream 0 (min 60 (- indentation i)))))

;;; Indenting-Out writes a character to an indenting stream.

(defun indenting-out (stream char)
  (let ((sub-stream (indenting-stream-stream stream)))
    (write-char char sub-stream)
    (if (char= char #\newline)
	(indenting-indent stream sub-stream))))

;;; Indenting-Sout writes a string to an indenting stream.

(defun indenting-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (do ((i start)
       (sub-stream (indenting-stream-stream stream)))
      ((= i end))
    (let ((newline (position #\newline string :start i :end end)))
      (cond (newline
	     (write-string* string sub-stream i (1+ newline))
	     (indenting-indent stream sub-stream)
	     (setq i (+ newline 1)))
	    (t
	     (write-string* string sub-stream i end)
	     (setq i end))))))

;;; Indenting-Misc just treats just the :Line-Length message differently.
;;; Indenting-Charpos says the charpos is the charpos of the base stream minus
;;; the stream's indentation.

(defun indenting-misc (stream operation &optional arg1 arg2)
  (let ((sub-stream (indenting-stream-stream stream)))
    (etypecase sub-stream
      (lisp-stream
       (let ((method (lisp-stream-misc sub-stream)))
	 (case operation
	   (:line-length
	    (let ((line-length (funcall method sub-stream operation)))
	      (if line-length
		  (- line-length (indenting-stream-indentation stream)))))
	   (:charpos
	    (let ((charpos (funcall method sub-stream operation)))
	      (if charpos
		  (- charpos (indenting-stream-indentation stream)))))       
	   (t
	    (funcall method sub-stream operation arg1 arg2)))))
      (fundamental-stream
       (case operation
	 (:line-length
	  (let ((line-length (stream-line-length sub-stream)))
	    (if line-length
		(- line-length (indenting-stream-indentation stream)))))
	 (:charpos
	  (let ((charpos (stream-line-column sub-stream)))
	    (if charpos
		(- charpos (indenting-stream-indentation stream)))))
	 (t
	  ;; XX TODO 
	  (format t "* Warning: ignoring ~s ~s ~s on ~s~%"
		  operation arg1 arg2 sub-stream)))))))


(proclaim '(maybe-inline read-char unread-char read-byte listen))



;;;; Case frobbing streams, used by format ~(...~).

(defstruct (case-frob-stream
	    (:include lisp-stream
		      (:misc #'case-frob-misc))
	    (:constructor %make-case-frob-stream (target out sout)))
  (target (required-argument) :type stream))

(defun make-case-frob-stream (target kind)
  "Returns a stream that sends all output to the stream TARGET, but modifies
   the case of letters, depending on KIND, which should be one of:
     :upcase - convert to upper case.
     :downcase - convert to lower case.
     :capitalize - convert the first letter of words to upper case and the
        rest of the word to lower case.
     :capitalize-first - convert the first letter of the first word to upper
        case and everything else to lower case."
  (declare (type stream target)
	   (type (member :upcase :downcase :capitalize :capitalize-first)
		 kind)
	   (values stream))
  (if (case-frob-stream-p target)
      ;; If we are going to be writing to a stream that already does case
      ;; frobbing, why bother frobbing the case just so it can frob it
      ;; again?
      target
      (multiple-value-bind
	  (out sout)
	  (ecase kind
	    (:upcase
	     (values #'case-frob-upcase-out
		     #'case-frob-upcase-sout))
	    (:downcase
	     (values #'case-frob-downcase-out
		     #'case-frob-downcase-sout))
	    (:capitalize
	     (values #'case-frob-capitalize-out
		     #'case-frob-capitalize-sout))
	    (:capitalize-first
	     (values #'case-frob-capitalize-first-out
		     #'case-frob-capitalize-first-sout)))
	(%make-case-frob-stream target out sout))))

(defun case-frob-misc (stream op &optional arg1 arg2)
  (declare (type case-frob-stream stream))
  (case op
    (:close)
    (t
     (let ((target (case-frob-stream-target stream)))
       (funcall (lisp-stream-misc target) target op arg1 arg2)))))


(defun case-frob-upcase-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (funcall (lisp-stream-out target) target (char-upcase char))))

(defun case-frob-upcase-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (len (length str))
	 (end (or end len)))
    (funcall (lisp-stream-sout target) target
	     (if (and (zerop start) (= len end))
		 (string-upcase str)
		 (nstring-upcase (subseq str start end)))
	     0
	     (- end start))))

(defun case-frob-downcase-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (funcall (lisp-stream-out target) target (char-downcase char))))

(defun case-frob-downcase-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (len (length str))
	 (end (or end len)))
    (funcall (lisp-stream-sout target) target
	     (if (and (zerop start) (= len end))
		 (string-downcase str)
		 (nstring-downcase (subseq str start end)))
	     0
	     (- end start))))

(defun case-frob-capitalize-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (funcall (lisp-stream-out target) target (char-upcase char))
	   (setf (case-frob-stream-out stream)
		 #'case-frob-capitalize-aux-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-capitalize-aux-sout))
	  (t
	   (funcall (lisp-stream-out target) target char)))))

(defun case-frob-capitalize-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (str (subseq str start end))
	 (len (length str))
	 (inside-word nil))
    (dotimes (i len)
      (let ((char (schar str i)))
	(cond ((not (alphanumericp char))
	       (setf inside-word nil))
	      (inside-word
	       (setf (schar str i) (char-downcase char)))
	      (t
	       (setf inside-word t)
	       (setf (schar str i) (char-upcase char))))))
    (when inside-word
      (setf (case-frob-stream-out stream)
	    #'case-frob-capitalize-aux-out)
      (setf (case-frob-stream-sout stream)
	    #'case-frob-capitalize-aux-sout))
    (funcall (lisp-stream-sout target) target str 0 len)))

(defun case-frob-capitalize-aux-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (funcall (lisp-stream-out target) target (char-downcase char)))
	  (t
	   (funcall (lisp-stream-out target) target char)
	   (setf (case-frob-stream-out stream)
		 #'case-frob-capitalize-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-capitalize-sout)))))

(defun case-frob-capitalize-aux-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (str (subseq str start end))
	 (len (length str))
	 (inside-word t))
    (dotimes (i len)
      (let ((char (schar str i)))
	(cond ((not (alphanumericp char))
	       (setf inside-word nil))
	      (inside-word
	       (setf (schar str i) (char-downcase char)))
	      (t
	       (setf inside-word t)
	       (setf (schar str i) (char-upcase char))))))
    (unless inside-word
      (setf (case-frob-stream-out stream)
	    #'case-frob-capitalize-out)
      (setf (case-frob-stream-sout stream)
	    #'case-frob-capitalize-sout))
    (funcall (lisp-stream-sout target) target str 0 len)))

(defun case-frob-capitalize-first-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (funcall (lisp-stream-out target) target (char-upcase char))
	   (setf (case-frob-stream-out stream)
		 #'case-frob-downcase-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-downcase-sout))
	  (t
	   (funcall (lisp-stream-out target) target char)))))

(defun case-frob-capitalize-first-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (str (subseq str start end))
	 (len (length str)))
    (dotimes (i len)
      (let ((char (schar str i)))
	(when (alphanumericp char)
	  (setf (schar str i) (char-upcase char))
	  (do ((i (1+ i) (1+ i)))
	      ((= i len))
	    (setf (schar str i) (char-downcase (schar str i))))
	  (setf (case-frob-stream-out stream)
		#'case-frob-downcase-out)
	  (setf (case-frob-stream-sout stream)
		#'case-frob-downcase-sout)
	  (return))))
    (funcall (lisp-stream-sout target) target str 0 len)))


;;;; Public interface from "EXTENSIONS" package.

(defstruct (stream-command (:print-function print-stream-command)
			   (:constructor make-stream-command
					 (name &optional args)))
  (name nil :type symbol)
  (args nil :type list))

(defun print-stream-command (obj str n)
  (declare (ignore n))
  (format str "#<Stream-Cmd ~S>" (stream-command-name obj)))


;;; GET-STREAM-COMMAND -- Public.
;;;
;;; We can't simply call the stream's misc method because nil is an
;;; ambiguous return value: does it mean text arrived, or does it mean the
;;; stream's misc method had no :get-command implementation.  We can't return
;;; nil until there is text input.  We don't need to loop because any stream
;;; implementing :get-command would wait until it had some input.  If the
;;; LISTEN fails, then we have some random stream we must wait on.
;;;
(defun get-stream-command (stream)
  "This takes a stream and waits for text or a command to appear on it.  If
   text appears before a command, this returns nil, and otherwise it returns
   a command."
  (let ((cmdp (funcall (lisp-stream-misc stream) stream :get-command)))
    (cond (cmdp)
	  ((listen stream)
	   nil)
	  (t
	   ;; This waits for input and returns nil when it arrives.
	   (unread-char (read-char stream) stream)))))


;;; READ-SEQUENCE -- Public
;;;
(defun read-sequence (seq stream &key (start 0) (end nil))
  "Destructively modify SEQ by reading elements from STREAM.
  SEQ is bounded by START and END. SEQ is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (declare (type sequence seq)
	   (type stream stream)
	   (type index start)
	   (type sequence-end end)
	   (values index))
  (let ((end (or end (length seq))))
    (declare (type index end))
    (etypecase seq
      (list
       (let ((read-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'read-char
		  #'read-byte)))
	 (do ((rem (nthcdr start seq) (rest rem))
	      (i start (1+ i)))
	     ((or (endp rem) (>= i end)) i)
	   (declare (type list rem)
		    (type index i))
	   (let ((el (funcall read-function stream nil '%%RWSEQ-EOF%%)))
	     (when (eq el '%%RWSEQ-EOF%%)
	       (return i))
	     (setf (first rem) el)))))
      (vector
       (with-array-data ((data seq) (offset-start start) (offset-end end))
	 (if (or (typep data '(simple-array (unsigned-byte 8) (*)))
		 #+signed-array
		 (typep data '(simple-array (signed-byte 8) (*)))
		 (and (simple-string-p data) (fd-stream-p stream)))
	     (let* ((numbytes (- end start))
		    (bytes-read (system:read-n-bytes
				 stream data offset-start numbytes nil)))
	       (if (< bytes-read numbytes)
		   (+ start bytes-read)
		   end))
	     (let ((read-function
		    (if (subtypep (stream-element-type stream) 'character)
			#'read-char
			#'read-byte)))
	       (do ((i offset-start (1+ i)))
		   ((>= i offset-end) end)
		 (declare (type index i))
		 (let ((el (funcall read-function stream nil '%%RWSEQ-EOF%%)))
		   (when (eq el '%%RWSEQ-EOF%%)
		     (return (- i offset-start)))
		   (setf (aref data i) el))))))))))

;;; WRITE-SEQUENCE -- Public
;;;
(defun write-sequence (seq stream &key (start 0) (end nil))
  "Write the elements of SEQ bounded by START and END to STREAM."
  (declare (type sequence seq)
	   (type stream stream)
	   (type index start)
	   (type sequence-end end)
	   (values sequence))
  (let ((end (or end (length seq))))
    (declare (type index start end))
    (etypecase seq
      (list
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'write-char
		  #'write-byte)))
	 (do ((rem (nthcdr start seq) (rest rem))
	      (i start (1+ i)))
	     ((or (endp rem) (>= i end)) seq)
	   (declare (type list rem)
		    (type index i))
	   (funcall write-function (first rem) stream))))
      (string
       (write-string* seq stream start end))
      (vector
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'write-char
		  #'write-byte)))
	 (do ((i start (1+ i)))
	     ((>= i end) seq)
	   (declare (type index i))
	   (funcall write-function (aref seq i) stream)))))))
