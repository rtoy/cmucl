;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Stream functions for Spice Lisp.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
;;; This file contains the machine-independent stream functions.  Another
;;; file (VAXIO, SPIO, or VMIO) contains functions used by this file for
;;; a specific machine.
;;;
(in-package "LISP")

(export '(make-broadcast-stream make-synonym-stream
	  make-broadcast-stream make-concatenated-stream make-two-way-stream
	  make-echo-stream make-string-input-stream make-string-output-stream
	  get-output-stream-string stream-element-type input-stream-p
	  output-stream-p close read-line read-char
	  unread-char peek-char listen read-char-no-hang clear-input read-byte
	  write-char write-string write-line terpri fresh-line
	  finish-output force-output clear-output write-byte
          stream streamp *standard-input* *standard-output*
          *error-output* *query-io* *debug-io* *terminal-io* *trace-output*))

(in-package 'system)
(export '(make-indenting-stream read-n-bytes))
(in-package 'lisp)

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
;;;  :read-line		- Do a read-line.
;;;  :listen 		- Return true if any input waiting.
;;;  :unread		- Unread the character Arg.
;;;  :close		- Do any stream specific stuff to close the stream.
;;;			  The methods are set to closed-flame by the close
;;;			  function, so that need not be done by this
;;;			  function.
;;;  :clear-input 	- Clear any unread input
;;;  :finish-output,
;;;  :force-output	- Cause output to happen
;;;  :clear-output	- Clear any undone output
;;;  :element-type 	- Return the type of element the stream deals with.
;;;  :line-length	- Return the length of a line of output.
;;;  :charpos		- Return current output position on the line.
;;;  :file-length	- Return the file length of a file stream.
;;;  :file-position	- Return or change the current position of a file stream.
;;;  :file-name		- Return the name of an associated file.
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
  (and (streamp stream)
       (not (eq (stream-in stream) #'closed-flame))
       (or (not (eq (stream-in stream) #'ill-in))
	   (not (eq (stream-bin stream) #'ill-bin)))))

(defun output-stream-p (stream)
  "Returns non-nil if the given Stream can perform output operations."
  (and (streamp stream)
       (not (eq (stream-in stream) #'closed-flame))
       (or (not (eq (stream-out stream) #'ill-out))
	   (not (eq (stream-bout stream) #'ill-bout)))))

(defun stream-element-type (stream)
  "Returns a type specifier for the kind of object returned by the Stream."
  (if (streamp stream)
      (funcall (stream-misc stream) stream :element-type)
      (error "~S is not a stream." stream)))

(defun close (stream &key abort)
  "Closes the given Stream.  No more I/O may be performed, but inquiries
  may still be made.  If :Abort is non-nil, an attempt is made to clean
  up the side effects of having created the stream."
  (if (streamp stream)
      (unless (eq (stream-in stream) #'closed-flame)
	(funcall (stream-misc stream) stream :close abort))
      (error "~S is not a stream." stream))
  t)

(defun set-closed-flame (stream)
  (setf (stream-in stream) #'closed-flame)
  (setf (stream-bin stream) #'closed-flame)
  (setf (stream-n-bin stream) #'closed-flame)
  (setf (stream-in stream) #'closed-flame)
  (setf (stream-out stream) #'closed-flame)
  (setf (stream-bout stream) #'closed-flame)
  (setf (stream-sout stream) #'closed-flame)
  (setf (stream-misc stream) #'closed-flame))

;;; Input functions:

(defun read-line (&optional (stream *standard-input*) (eof-errorp t) eof-value
			    recursive-p)
  "Returns a line of text read from the Stream as a string, discarding the
  newline character."
  (declare (ignore recursive-p))
  (let* ((stream (in-synonym-of stream))
	 (buffer (stream-in-buffer stream))
	 (index (stream-in-index stream)))
    (declare (fixnum index))
    (if (simple-string-p buffer)
	(let ((nl (%primitive find-character buffer index in-buffer-length
			      #\newline)))
	  (if nl
	      (values (prog1 (subseq (the simple-string buffer) index nl)
			     (setf (stream-in-index stream) (1+ (the fixnum nl))))
		      nil)
	      (multiple-value-bind (str eofp)
				   (funcall (stream-misc stream) stream
					    :read-line eof-errorp eof-value)
		(declare (simple-string str))
		(if (= index in-buffer-length)
		    (values str eofp)
		    (values (prog1
			     (concatenate 'simple-string
					  (subseq buffer index in-buffer-length)
					  str)
			     (setf (stream-in-index stream) in-buffer-length))
			    eofp)))))
	(funcall (stream-misc stream) stream :read-line eof-errorp eof-value))))

;;; We proclaim them inline here, then proclaim them notinline at EOF,
;;; so, except in this file, they are not inline by default, but they can be.
;;;
(proclaim '(inline read-char unread-char read-byte listen))
(defun read-char (&optional (stream *standard-input*) (eof-errorp t) eof-value
			    recursive-p)
  "Inputs a character from Stream and returns it."
  (declare (ignore recursive-p))
  (let* ((stream (in-synonym-of stream))
	 (index (stream-in-index stream)))
    (declare (fixnum index))
    (if (eql index in-buffer-length)
	(funcall (stream-in stream) stream eof-errorp eof-value)
	(prog1 (aref (stream-in-buffer stream) index)
	       (setf (stream-in-index stream) (1+ index))))))

(defun unread-char (character &optional (stream *standard-input*))
  "Puts the Character back on the front of the input Stream."
  (let* ((stream (in-synonym-of stream))
	 (index (1- (the fixnum (stream-in-index stream))))
	 (buffer (stream-in-buffer stream)))
    (declare (fixnum index))
    (when (minusp index) (error "Nothing to unread."))
    (if buffer
	(setf (aref (the simple-array buffer) index) character
	      (stream-in-index stream) index)
	(funcall (stream-misc stream) stream :unread character)))
  nil)

(defun peek-char (&optional (peek-type nil) (stream *standard-input*)
			    (eof-errorp t) eof-value recursive-p)
  "Peeks at the next character in the input Stream.  See manual for details."
  (declare (ignore recursive-p))
  (let* ((stream (in-synonym-of stream))
	 (char (read-char stream eof-errorp eof-value)))
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

(defun listen (&optional (stream *standard-input*))
  "Returns T if a character is availible on the given Stream."
  (let ((stream (in-synonym-of stream)))
    (or (/= (the fixnum (stream-in-index stream)) in-buffer-length)
	(funcall (stream-misc stream) stream :listen))))

(defun read-char-no-hang (&optional (stream *standard-input*)
				    (eof-errorp t) eof-value recursive-p)
  "Returns the next character from the Stream if one is availible, or nil."
  (declare (ignore recursive-p))
  (if (listen stream) (read-char stream eof-errorp eof-value) nil))

(defun clear-input (&optional (stream *standard-input*))
  "Clears any buffered input associated with the Stream."
  (let ((stream (in-synonym-of stream)))
    (setf (stream-in-index stream) in-buffer-length)
    (funcall (stream-misc stream) stream :clear-input)
    nil))

(defun read-byte (stream &optional (eof-errorp t) eof-value)
  "Returns the next byte of the Stream."
  (let* ((stream (in-synonym-of stream))
	 (index (stream-in-index stream)))
    (declare (fixnum index))
    (if (eql index in-buffer-length)
	(funcall (stream-bin stream) stream eof-errorp eof-value)
	(prog1 (aref (stream-in-buffer stream) index)
	       (setf (stream-in-index stream) (1+ index))))))

(defun read-n-bytes (stream buffer start numbytes &optional (eof-errorp t))
  "Reads Numbytes bytes into the Buffer starting at Start, and returns
  the number of bytes actually read if the end of file was hit before Numbytes
  bytes were read (and Eof-Errorp is false)."
  (declare (fixnum numbytes))
  (let* ((stream (in-synonym-of stream))
	 (in-buffer (stream-in-buffer stream))
	 (index (stream-in-index stream))
	 (num-buffered (- in-buffer-length index)))
    (declare (fixnum index num-buffered))
    (cond
     ((not in-buffer)
      (with-in-stream stream stream-n-bin buffer start numbytes eof-errorp))
     ((not (eql (%primitive get-vector-access-code in-buffer) 3))
      (error "N-Bin only works on 8-bit-like streams."))
     ((<= numbytes num-buffered)
      (%primitive byte-blt in-buffer index buffer start (+ start numbytes))
      (setf (stream-in-index stream) (+ index numbytes))
      numbytes)
     (t
      (let ((end (+ start num-buffered)))
	(%primitive byte-blt in-buffer index buffer start end)
	(setf (stream-in-index stream) in-buffer-length)
	(+ (with-in-stream stream stream-n-bin buffer end
				   (- numbytes num-buffered)
				   eof-errorp)
	   num-buffered))))))

;;; Output functions:

(defun write-char (character &optional (stream *standard-output*))
  "Outputs the Character to the Stream."
  (with-out-stream stream stream-out character)
  character)

(defun terpri (&optional (stream *standard-output*))
  "Outputs a new line to the Stream."
  (with-out-stream stream stream-out #\newline)
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  "Outputs a new line to the Stream if it is not positioned at the begining of
   a line.  Returns T if it output a new line, nil otherwise."
  (let ((stream (out-synonym-of stream)))
    (when (/= (or (charpos stream) 1) 0)
      (funcall (stream-out stream) stream #\newline)
      t)))

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) (end (length (the vector string))))
  "Outputs the String to the given Stream."
  (write-string* string stream start end))

(defun write-string* (string &optional (stream *standard-output*)
			     (start 0) (end (length (the vector string))))
  (declare (fixnum start end))
  (if (array-header-p string)
      (with-array-data ((data string) (offset-start start) (offset-end end))
	(with-out-stream stream stream-sout data offset-start offset-end))
      (with-out-stream stream stream-sout string start end))
  string)

(defun write-line (string &optional (stream *standard-output*)
			  &key (start 0) (end (length string)))
  "Outputs the String to the given Stream, followed by a newline character."
  (write-line* string stream start end))

(defun write-line* (string &optional (stream *standard-output*)
			   (start 0) (end (length string)))
  (declare (fixnum start end))
  (let ((stream (out-synonym-of stream)))
    (if (array-header-p string)
	(with-array-data ((data string) (offset-start start) (offset-end end))
	  (with-out-stream stream stream-sout data offset-start offset-end))
	(with-out-stream stream stream-sout string start end))
    (funcall (stream-out stream) stream #\newline))
  string)

(defun charpos (&optional (stream *standard-output*))
  "Returns the number of characters on the current line of output of the given
  Stream, or Nil if that information is not availible."
  (with-out-stream stream stream-misc :charpos))

(defun line-length (&optional (stream *standard-output*))
  "Returns the number of characters that will fit on a line of output on the
  given Stream, or Nil if that information is not available."
  (with-out-stream stream stream-misc :line-length))

(defun finish-output (&optional (stream *standard-output*))
  "Attempts to ensure that all output sent to the the Stream has reached its
   destination, and only then returns."
  (with-out-stream stream stream-misc :finish-output)
  nil)

(defun force-output (&optional (stream *standard-output*))
  "Attempts to force any buffered output to be sent."
  (with-out-stream stream stream-misc :force-output)
  nil)

(defun clear-output (&optional (stream *standard-output*))
  "Clears the given output Stream."
  (with-out-stream stream stream-misc :clear-output)
  nil)

(defun write-byte (integer stream)
  "Outputs the Integer to the binary Stream."
  (with-out-stream stream stream-bout integer)
  integer)

;;;; Broadcast streams:

(defstruct (broadcast-stream (:include stream
				       (out #'broadcast-out)
				       (bout #'broadcast-bout)
				       (sout #'broadcast-sout)
				       (misc #'broadcast-misc))
			     (:print-function %print-broadcast-stream)
			     (:constructor make-broadcast-stream (&rest streams)))
  ;; This is a list of all the streams we broadcast to.
  streams)

(setf (documentation 'make-broadcast-stream 'function)
 "Returns an ouput stream which sends its output to all of the given streams.")

(defun %print-broadcast-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Broadcast Stream>" stream))

(macrolet ((out-fun (fun method &rest args)
	     `(defun ,fun (stream ,@args)
		(dolist (stream (broadcast-stream-streams stream))
		  (funcall (,method stream) stream ,@args)))))
  (out-fun broadcast-out stream-out char)
  (out-fun broadcast-bout stream-bout byte)
  (out-fun broadcast-sout stream-sout string start end))

(defun broadcast-misc (stream operation &optional arg1 arg2)
  (let ((streams (broadcast-stream-streams stream)))
    (case operation
      (:charpos
       (dolist (stream streams)
	 (let ((charpos (funcall (stream-misc stream) stream :charpos)))
	   (if charpos (return charpos)))))
      (:line-length
       (let ((min nil))
	 (dolist (stream streams min)
	   (let ((res (funcall (stream-misc stream) stream :line-length)))
	     (when res (setq min (if min (min res min) res)))))))
      (:element-type
       (let (res)
	 (dolist (stream streams (if (> (length res) 1) `(and ,@res) res))
	   (pushnew (funcall (stream-misc stream) stream :element-type) res
		    :test #'equal))))
      (t
       (let ((res nil))
	 (dolist (stream streams res)
	   (setq res (funcall (stream-misc stream) stream operation
			      arg1 arg2))))))))

;;;; Synonym Streams:

(defstruct (synonym-stream (:include stream
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
  symbol)

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
		(let ((syn (symbol-value (synonym-stream-symbol stream))))
		  (funcall (,slot syn) syn ,@args)))))
  (out-fun synonym-out stream-out ch)
  (out-fun synonym-bout stream-bout n)
  (out-fun synonym-sout stream-sout string start end))


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
  (let ((syn (symbol-value (synonym-stream-symbol stream)))
	(*previous-stream* stream))
    (case operation
      (:read-line (read-line syn))
      (:listen (or (/= (the fixnum (stream-in-index syn)) in-buffer-length)
		   (funcall (stream-misc syn) syn :listen)))
      (t
       (funcall (stream-misc syn) syn operation arg1 arg2)))))

;;;; Two-Way streams:

(defstruct (two-way-stream
	    (:include stream
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
  input-stream
  ;; And write to this one
  output-stream)

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
  (out-fun two-way-out stream-out ch)
  (out-fun two-way-bout stream-bout n)
  (out-fun two-way-sout stream-sout string start end))

(macrolet ((in-fun (name fun &rest args)
	     `(defun ,name (stream ,@args)
		(,fun (two-way-stream-input-stream stream) ,@args))))
  (in-fun two-way-in read-char eof-errorp eof-value)
  (in-fun two-way-bin read-byte eof-errorp eof-value)
  (in-fun two-way-n-bin read-n-bytes buffer start numbytes eof-errorp))

(defun two-way-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (in-method (stream-misc in))
	 (out (two-way-stream-output-stream stream))
	 (out-method (stream-misc out)))
    (case operation
      (:listen (or (/= (the fixnum (stream-in-index in)) in-buffer-length)
		   (funcall in-method in :listen)))
      (:read-line (read-line in arg1 arg2))
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
       (funcall in-method in :close arg1)
       (funcall out-method out :close arg1)
       (set-closed-flame stream))
      (t
       (or (funcall in-method in operation arg1 arg2)
	   (funcall out-method out operation arg1 arg2))))))

;;;; Concatenated Streams:

(defstruct (concatenated-stream
	    (:include stream
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
  streams)

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

;;;    Concatenated-Readline is somewhat hairy, since we may need to
;;; do several readlines and concatenate the result if the lines are
;;; terminated by eof.
;;;
(defun concatenated-readline (stream eof-errorp eof-value)
  ;; Loop until we find a stream that will give us something or we error
  ;; out.
  (do ((current (concatenated-stream-current stream) (cdr current)))
      ((null current)
       (eof-or-lose stream eof-errorp eof-value))
    (setf (concatenated-stream-current stream) current)
    (let ((this (car current)))
      (multiple-value-bind (result eofp)
			   (read-line this nil nil)
	(declare (simple-string result))
	;; Once we have found some input, we loop until we either find a 
	;; line not terminated by eof or hit eof on the last stream.
	(when result
	  (do ((current (cdr current) (cdr current))
	       (new ""))
	      ((or (not eofp) (null current))
	       (return-from concatenated-readline (values result eofp)))
	    (declare (simple-string new))
	    (setf (concatenated-stream-current stream) current)
	    (let ((this (car current)))
	      (multiple-value-setq (new eofp)
		(read-line this nil nil))
	      (if new
		  (setq result (concatenate 'simple-string result new))
		  (setq eofp t)))))))))

(defun concatenated-misc (stream operation &optional arg1 arg2)
  (if (eq operation :read-line)
      (concatenated-readline stream arg1 arg2)
      (let ((left (concatenated-stream-current stream)))
	(when left
	  (let* ((current (car left))
		 (misc (stream-misc current)))
	    (case operation
	      (:listen (or (/= (the fixnum (stream-in-index current)) in-buffer-length)
			   (funcall misc current :listen)))
	      (:close
	       (dolist (stream (concatenated-stream-streams stream))
		 (funcall (stream-misc stream) stream :close arg1))
	       (set-closed-flame stream))
	      (t
	       (funcall misc current operation arg1 arg2))))))))

;;;; Echo Streams:

(defstruct (echo-stream
	    (:include two-way-stream
		      (in #'echo-in)
		      (bin #'echo-bin)
		      (misc #'echo-misc)
		      (n-bin #'ill-bin))
	    (:print-function %print-echo-stream)
	    (:constructor make-echo-stream (input-stream output-stream))))


(macrolet ((in-fun (name fun out-slot &rest args)
	     `(defun ,name (stream ,@args)
		(let* ((in (two-way-stream-input-stream stream))
		       (out (two-way-stream-output-stream stream))
		       (result (,fun in ,@args)))
		  (funcall (,out-slot out) out result)
		  result))))
  (in-fun echo-in read-char stream-out eof-errorp eof-value)
  (in-fun echo-bin read-byte stream-bout eof-errorp eof-value))

(defun echo-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (in-method (stream-misc in))
	 (out (two-way-stream-output-stream stream))
	 (out-method (stream-misc out)))
    (case operation
      (:listen (or (/= (the fixnum (stream-in-index in)) in-buffer-length)
		   (funcall in-method in :listen)))
      (:read-line
       (multiple-value-bind (result eofp)
			    (read-line in arg1 arg2)
	 (if eofp
	     (write-string result out)
	     (write-line result out))
	 (values result eofp)))
      (:element-type
       (let ((in-type (funcall in-method in :element-type))
	     (out-type (funcall out-method out :element-type)))
	 (if (equal in-type out-type)
	     in-type `(and ,in-type ,out-type))))
      (:close
       (funcall in-method in :close arg1)
       (funcall out-method out :close arg1)
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
	    (:include stream
		      (in #'string-inch)
		      (misc #'string-in-misc))
	    (:print-function %print-string-input-stream)
	    (:constructor nil)
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

(defun string-in-misc (stream operation &optional arg1 arg2)
  (case operation
    (:file-position
     (if (null arg1)
       (string-input-stream-current stream)))
    (:read-line
     (let ((string (string-input-stream-string stream))
	   (current (string-input-stream-current stream))
	   (end (string-input-stream-end stream)))
       (declare (simple-string string) (fixnum current end))
       (if (= current end)
	   (eof-or-lose stream arg1 arg2)
	   (let ((pos (%primitive find-character string current end #\newline)))
	     (if pos
		 (let* ((res-length (- (the fixnum pos) current))
			(result (make-string res-length)))
		   (%primitive byte-blt string current result 0 res-length)
		   (setf (string-input-stream-current stream)
			 (1+ (the fixnum pos)))
		   (values result nil))
		 (let* ((res-length (- end current))
			(result (make-string res-length)))
		   (%primitive byte-blt string current result 0 res-length)
		   (setf (string-input-stream-current stream) end)
		   (values result t)))))))
    (:unread (decf (string-input-stream-current stream)))
    (:listen (not (= (the fixnum (string-input-stream-current stream))
		     (the fixnum (string-input-stream-end stream)))))
    (:element-type 'string-char)))
  
(defun make-string-input-stream (string &optional
					(start 0) (end (length string)))
  "Returns an input stream which will supply the characters of String between
  Start and End in order."
  (if (stringp string)
      (internal-make-string-input-stream (coerce string 'simple-string)
					 start end)
      (error "~S is not a string." string)))

;;;; String Output Streams:

(defstruct (string-output-stream
	    (:include stream
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
	  (%primitive byte-blt workspace 0 new-workspace 0 current)
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
	  (%primitive byte-blt workspace 0 new-workspace 0 current)
	  (%primitive byte-blt string start new-workspace current dst-end)
	  (setf (string-output-stream-string stream) new-workspace))
	(%primitive byte-blt string start workspace current dst-end))
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
    (:element-type 'string-char)))

(defun get-output-stream-string (stream)
  "Returns a string of all the characters sent to a stream made by
   Make-String-Output-Stream since the last call to this function."
  (if (streamp stream)
      (let* ((length (string-output-stream-index stream))
	     (result (make-string length)))
	(%primitive byte-blt (string-output-stream-string stream) 0
		      result 0 length)
	(setf (string-output-stream-index stream) 0)
	result)
      (error "~S is not a string stream.")))

(defun dump-output-stream-string (in-stream out-stream)
  "Dumps the characters buffer up in the In-Stream to the Out-Stream as
  Get-Output-Stream-String would return them."
  (write-string (string-output-stream-string in-stream) out-stream
		:start 0 :end (string-output-stream-index in-stream))
  (setf (string-output-stream-index in-stream) 0))

;;;; Fill-pointer streams:
;;;
;;;    Fill pointer string output streams are not explicitly mentioned in
;;; the CLM, but they are required for the implementation of With-Output-To-String.

(defstruct (fill-pointer-output-stream
 	    (:include stream
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
	 (current (%primitive header-ref buffer %array-fill-pointer-slot))
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
	    (%primitive header-set buffer %array-fill-pointer-slot current+1))
	(setf (schar workspace offset-current) character)))
    current+1))


(defun fill-pointer-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (let* ((buffer (fill-pointer-output-stream-string stream))
	 (current (%primitive header-ref buffer %array-fill-pointer-slot))
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
	    (%primitive header-set buffer %array-fill-pointer-slot dst-end))
	(%primitive byte-blt string start
		    workspace offset-current offset-dst-end)))
    dst-end))


(defun fill-pointer-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos
     (let* ((buffer (fill-pointer-output-stream-string stream))
	    (current (%primitive header-ref buffer %array-fill-pointer-slot)))
       (with-array-data ((string buffer) (start) (end current))
	 (declare (simple-string string) (ignore start))
	 (let ((found (position #\newline string :test #'char=
				:end end :from-end t)))
	   (if found
	       (- end (the fixnum found))
	       current)))))
     (:element-type 'string-char)))

;;;; Indenting streams:

(defstruct (indenting-stream (:include stream
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
     (funcall (stream-sout ,sub-stream) ,sub-stream
	      "                                                            "
	      0 (min 60 (- indentation i)))))

;;; Indenting-Out writes a character to an indenting stream.

(defun indenting-out (stream char)
  (let ((sub-stream (indenting-stream-stream stream)))
    (funcall (stream-out sub-stream) sub-stream char)
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
	     (funcall (stream-sout sub-stream) sub-stream string i (1+ newline))
	     (indenting-indent stream sub-stream)
	     (setq i (+ newline 1)))
	    (t
	     (funcall (stream-sout sub-stream) sub-stream string i end)
	     (setq i end))))))

;;; Indenting-Misc just treats just the :Line-Length message differently.
;;; Indenting-Charpos says the charpos is the charpos of the base stream minus
;;; the stream's indentation.

(defun indenting-misc (stream operation &optional arg1 arg2)
  (let* ((sub-stream (indenting-stream-stream stream))
	 (method (stream-misc sub-stream)))
    (case operation
      (:line-length
       (let ((line-length (funcall method sub-stream operation)))
	 (if line-length
	     (- line-length (indenting-stream-indentation stream)))))
      (:charpos
       (let* ((sub-stream (indenting-stream-stream stream))
	      (charpos (funcall method sub-stream operation)))
	 (if charpos
	     (- charpos (indenting-stream-indentation stream)))))       
      (t
       (funcall method sub-stream operation arg1 arg2)))))

(proclaim '(notinline read-char unread-char read-byte listen))
