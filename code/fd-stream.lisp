;;; -*- Log: code.log; Package: LISP -*-

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fd-stream.lisp,v 1.6 1990/09/06 19:40:38 wlott Exp $
;;;
;;; Streams for UNIX file descriptors.
;;;
;;; Written by William Lott, July 1989 - January 1990.
;;; 
;;; **********************************************************************


(in-package "SYSTEM")

(export '(fd-stream fd-stream-p fd-stream-fd make-fd-stream
	  beep *beep-function* 
	  output-raw-bytes
	  *tty* *stdin* *stdout* *stderr*))


(in-package "EXTENSIONS")

(export '(*backup-extension*))


(in-package "LISP")


;;;; Buffer manipulation routines.

(defvar *available-buffers* ()
  "List of available buffers. Each buffer is an dynamic alien.")

(defconstant bytes-per-buffer (* 4 1024)
  "Number of bytes per buffer.")

;;; NEXT-AVAILABLE-BUFFER -- Internal.
;;;
;;; Returns the next available alien buffer, creating one if necessary.
;;;
(proclaim '(inline next-available-buffer))
;;;
(defun next-available-buffer ()
  (if *available-buffers*
      (pop *available-buffers*)
      (allocate-system-memory bytes-per-buffer)))


;;;; The FD-STREAM structure.

(defstruct (fd-stream
	    (:print-function %print-fd-stream)
	    (:constructor %make-fd-stream)
	    (:include stream
		      (misc #'fd-stream-misc-routine)))

  (name nil)		      ; The name of this stream
  (file nil)		      ; The file this stream is for
  (original nil)	      ; The original file (for :if-exists :rename)
  (delete-original nil)	      ; for :if-exists :rename-and-delete
  (element-size 1)	      ; Number of bytes per element.
  (element-type 'base-character) ; The type of element being transfered.
  (fd -1 :type fixnum)	      ; The file descriptor
  (buffering :full)	      ; One of :none, :line, or :full
  (char-pos nil)	      ; Character position if known.
  (listen nil)		      ; T if we don't need to listen

  ;; The input buffer.
  (unread nil)
  (ibuf-sap nil)
  (ibuf-length nil)
  (ibuf-head 0 :type fixnum)
  (ibuf-tail 0 :type fixnum)

  ;; The output buffer.
  (obuf-sap nil)
  (obuf-length nil)
  (obuf-tail 0 :type fixnum)

  ;; Output flushed, but not written due to non-blocking io.
  (output-later nil)
  (handler nil))

(defun %print-fd-stream (fd-stream stream depth)
  (declare (ignore depth))
  (format stream "#<Stream for ~A>"
	  (fd-stream-name fd-stream)))



;;;; Output routines and related noise.

(defvar *output-routines* ()
  "List of all available output routines. Each element is a list of the
  element-type output, the kind of buffering, the function name, and the number
  of bytes per element.")

;;; DO-OUTPUT-LATER -- internal
;;;
;;;   Called by the server when we can write to the given file descriptor.
;;; Attemt to write the data again. If it worked, remove the data from the
;;; output-later list. If it didn't work, something is wrong.
;;;
(defun do-output-later (stream)
  (let* ((stuff (pop (fd-stream-output-later stream)))
	 (base (car stuff))
	 (start (cadr stuff))
	 (end (caddr stuff))
	 (reuse-sap (cadddr stuff))
	 (length (- end start)))
    (multiple-value-bind
	(count errno)
	(mach:unix-write (fd-stream-fd stream)
			 base
			 start
			 length)
      (cond ((eql count length) ; Hot damn, it workded.
	     (when reuse-sap
	       (push base *available-buffers*)))
	    ((not (null count)) ; Sorta worked.
	     (push (list base
			 (+ start count)
			 end)
		   (fd-stream-output-later stream)))
	    ((= errno mach:ewouldblock)
	     (error "Write would have blocked, but SERVER told us to go."))
	    (t
	     (error "While writing ~S: ~A"
		    stream
		    (mach:get-unix-error-msg errno))))))
  (unless (fd-stream-output-later stream)
    (system:remove-fd-handler (fd-stream-handler stream))
    (setf (fd-stream-handler stream) nil)))

;;; OUTPUT-LATER -- internal
;;;
;;;   Arange to output the string when we can write on the file descriptor.
;;;
(defun output-later (stream base start end reuse-sap)
  (cond ((null (fd-stream-output-later stream))
	 (setf (fd-stream-output-later stream)
	       (list (list base start end reuse-sap)))
	 (setf (fd-stream-handler stream)
	       (system:add-fd-handler (fd-stream-fd stream)
				      :output
				      #'(lambda (fd)
					  (declare (ignore fd))
					  (do-output-later stream)))))
	(t
	 (nconc (fd-stream-output-later stream)
		(list (list base start end reuse-sap)))))
  (when reuse-sap
    (let ((new-buffer (next-available-buffer)))
      (setf (fd-stream-obuf-sap stream) new-buffer)
      (setf (fd-stream-obuf-length stream) bytes-per-buffer))))

;;; DO-OUTPUT -- internal
;;;
;;;   Output the given noise. Check to see if there are any pending writes. If
;;; so, just queue this one. Otherwise, try to write it. If this would block,
;;; queue it.
;;;
(defun do-output (stream base start end reuse-sap)
  (if (not (null (fd-stream-output-later stream))) ; something buffered.
    (progn
      (output-later stream base start end reuse-sap)
      ;; ### check to see if any of this noise can be output
      )
    (let ((length (- end start)))
      (multiple-value-bind
	  (count errno)
	  (mach:unix-write (fd-stream-fd stream) base start length)
	(cond ((eql count length)) ; Hot damn, it worked.
	      ((not (null count))
	       (output-later stream base (+ start count) end reuse-sap))
	      ((= errno mach:ewouldblock)
	       (output-later stream base start end reuse-sap))
	      (t
	       (error "While writing ~S: ~A"
		      stream
		      (mach:get-unix-error-msg errno))))))))

;;; FLUSH-OUTPUT-BUFFER -- internal
;;;
;;;   Flush any data in the output buffer.
;;;
(defun flush-output-buffer (stream)
  (let ((length (fd-stream-obuf-tail stream)))
    (unless (= length 0)
      (do-output stream (fd-stream-obuf-sap stream) 0 length t)
      (setf (fd-stream-obuf-tail stream) 0))))

;;; DEF-OUTPUT-ROUTINES -- internal
;;;
;;;   Define output routines that output numbers size bytes long for the
;;; given bufferings. Use body to do the actual output.
;;;
(defmacro def-output-routines ((name size &rest bufferings) &body body)
  (cons 'progn
	(mapcar
	    #'(lambda (buffering)
		(let ((function
		       (intern (let ((*print-case* :upcase))
				 (format nil name (car buffering))))))
		  `(progn
		     (defun ,function (stream byte)
		       ,(unless (eq (car buffering) :none)
			  `(when (< (fd-stream-obuf-length stream)
				    (+ (fd-stream-obuf-tail stream)
				       ,size))
			     (flush-output-buffer stream)))
		       ,@body
		       (incf (fd-stream-obuf-tail stream) ,size)
		       ,(ecase (car buffering)
			  (:none
			   `(flush-output-buffer stream))
			  (:line
			   `(when (eq (char-code byte) (char-code #\Newline))
			      (flush-output-buffer stream)))
			  (:full
			   ))
		       (values))
		     (setf *output-routines*
			   (nconc *output-routines*
				  ',(mapcar
					#'(lambda (type)
					    (list type
						  (car buffering)
						  function
						  size))
				      (cdr buffering)))))))
	  bufferings)))

(def-output-routines ("OUTPUT-CHAR-~A-BUFFERED"
		      1
		      (:none character)
		      (:line character)
		      (:full character))
  (if (eq (char-code byte)
	  (char-code #\Newline))
      (setf (fd-stream-char-pos stream) 0)
      (incf (fd-stream-char-pos stream)))
  (setf (sap-ref-8 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	(char-code byte)))

(def-output-routines ("OUTPUT-BYTE-~A-BUFFERED"
		      1
		      (:none (signed-byte 8) (unsigned-byte 8))
		      (:full (signed-byte 8) (unsigned-byte 8)))
  (when (characterp byte)
    (if (eq (char-code byte)
	    (char-code #\Newline))
      (setf (fd-stream-char-pos stream) 0)
      (incf (fd-stream-char-pos stream))))
  (setf (sap-ref-8 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SHORT-~A-BUFFERED"
		      2
		      (:none (signed-byte 16) (unsigned-byte 16))
		      (:full (signed-byte 16) (unsigned-byte 16)))
  (setf (sap-ref-16 (fd-stream-obuf-sap stream)
		    (truncate (fd-stream-obuf-tail stream) 2))
	byte))

(def-output-routines ("OUTPUT-LONG-~A-BUFFERED"
		      4
		      (:none (signed-byte 32) (unsigned-byte 32))
		      (:full (signed-byte 32) (unsigned-byte 32)))
  (setf (sap-ref-32 (fd-stream-obuf-sap stream)
		    (truncate (fd-stream-obuf-tail stream) 4))
	byte))

;;; OUTPUT-RAW-BYTES -- public
;;;
;;;   Does the actual output. If there is space to buffer the string, buffer
;;; it. If the string would normally fit in the buffer, but doesn't because
;;; of other stuff in the buffer, flush the old noise out of the buffer and
;;; put the string in it. Otherwise we have a very long string, so just
;;; send it directly (after flushing the buffer, of course).
;;;
(defun output-raw-bytes (stream thing &optional start end)
  "Output THING to stream.  THING can be any kind of vector or a sap.  If THING
  is a SAP, END must be supplied (as length won't work)."
  (let ((start (or start 0))
	(end (or end (length thing))))
    (declare (fixnum start end))
    (let* ((len (fd-stream-obuf-length stream))
	   (tail (fd-stream-obuf-tail stream))
	   (space (- len tail))
	   (bytes (- end start))
	   (newtail (+ tail bytes)))
      (cond ((minusp bytes) ; Error case
	     (cerror "Just go on as if nothing happened..."
		     "~S called with :END before :START!"
		     'output-raw-bytes))
	    ((zerop bytes)) ; Easy case
	    ((<= bytes space)
	     (if (system-area-pointer-p thing)
		 (system-area-copy thing
				   (* start vm:byte-bits)
				   (fd-stream-obuf-sap stream)
				   (* tail vm:byte-bits)
				   (* bytes vm:byte-bits))
		 (copy-to-system-area thing
				      (+ (* start vm:byte-bits)
					 (* vm:vector-data-offset vm:word-bits))
				      (fd-stream-obuf-sap stream)
				      (* tail vm:byte-bits)
				      (* bytes vm:byte-bits)))
	     (setf (fd-stream-obuf-tail stream) newtail))
	    ((<= bytes len)
	     (flush-output-buffer stream)
	     (if (system-area-pointer-p thing)
		 (system-area-copy thing
				   (* start vm:byte-bits)
				   (fd-stream-obuf-sap stream)
				   0
				   (* bytes vm:byte-bits))
		 (copy-to-system-area thing
				      (+ (* start vm:byte-bits)
					 (* vm:vector-data-offset vm:word-bits))
				      (fd-stream-obuf-sap stream)
				      0
				      (* bytes vm:byte-bits)))
	     (setf (fd-stream-obuf-tail stream) bytes))
	    (t
	     (flush-output-buffer stream)
	     (do-output stream thing start end nil))))))

;;; FD-SOUT -- internal
;;;
;;;   Routine to use to output a string. If the stream is unbuffered, slam
;;; the string down the file descriptor, otherwise use OUTPUT-RAW-BYTES to
;;; buffer the string. Update charpos by checking to see where the last newline
;;; was.
;;;
;;;   Note: some bozos (the FASL dumper) call write-string with things other
;;; than strings. Therefore, we must make sure we have a string before calling
;;; position on it.
;;; 
(defun fd-sout (stream thing start end)
  (let ((start (or start 0))
	(end (or end (length thing))))
    (declare (fixnum start end))
    (if (stringp thing)
	(let ((last-newline (and (find #\newline (the simple-string thing)
				       :start start :end end)
				 (position #\newline (the simple-string thing)
					   :from-end t
					   :start start
					   :end end))))
	  (ecase (fd-stream-buffering stream)
	    (:full
	     (output-raw-bytes stream thing start end))
	    (:line
	     (output-raw-bytes stream thing start end)
	     (when last-newline
	       (flush-output-buffer stream)))
	    (:none
	     (do-output stream thing start end nil)))
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

;;; PICK-OUTPUT-ROUTINE -- internal
;;;
;;;   Find an output routine to use given the type and buffering. Return as
;;; multiple values the routine, the real type transfered, and the number of
;;; bytes per element.
;;;
(defun pick-output-routine (type buffering)
  (dolist (entry *output-routines*)
    (when (and (subtypep type (car entry))
	       (eq buffering (cadr entry)))
      (return (values (symbol-function (caddr entry))
		      (car entry)
		      (cadddr entry))))))


;;;; Input routines and related noise.

(defvar *input-routines* ()
  "List of all available input routines. Each element is a list of the
  element-type input, the function name, and the number of bytes per element.")

;;; DO-INPUT -- internal
;;;
;;;   Fills the input buffer, and returns the first character. Throws to
;;; eof-input-catcher if the eof was reached. Drops into system:server if
;;; necessary.
;;;
(defun do-input (stream)
  (let ((fd (fd-stream-fd stream))
	(ibuf-sap (fd-stream-ibuf-sap stream))
	(buflen (fd-stream-ibuf-length stream))
	(head (fd-stream-ibuf-head stream))
	(tail (fd-stream-ibuf-tail stream)))
    (unless (zerop head)
      (cond ((eq head tail)
	     (setf head 0)
	     (setf tail 0)
	     (setf (fd-stream-ibuf-head stream) 0)
	     (setf (fd-stream-ibuf-tail stream) 0))
	    (t
	     (decf tail head)
	     (system-area-copy ibuf-sap (* head vm:byte-bits)
			       ibuf-sap 0 (* tail vm:byte-bits))
	     (setf head 0)
	     (setf (fd-stream-ibuf-head stream) 0)
	     (setf (fd-stream-ibuf-tail stream) tail))))
    (setf (fd-stream-listen stream) nil)
    #+serve-event
    (multiple-value-bind
	(count errno)
	(mach:unix-select (1+ fd) (ash 1 fd) 0 0 0)
      (case count
	(1)
	(0
	 (system:wait-until-fd-usable fd :input))
	(t
	 (error "Problem checking to see if ~S is readable: ~A"
		stream
		(mach:get-unix-error-msg errno)))))
    (multiple-value-bind
	(count errno)
	(mach:unix-read fd
			(system:int-sap (+ (system:sap-int ibuf-sap) tail))
			(- buflen tail))
      (cond ((null count)
	     (if #+serve-event (eql errno mach:ewouldblock)
		 #-serve-event nil
	       (progn
		 (system:wait-until-fd-usable fd :input)
		 (do-input stream))
	       (error "Error reading ~S: ~A"
		      stream
		      (mach:get-unix-error-msg errno))))
	    ((zerop count)
	     (throw 'eof-input-catcher nil))
	    (t
	     (incf (fd-stream-ibuf-tail stream) count))))))
			
;;; INPUT-AT-LEAST -- internal
;;;
;;;   Makes sure there are at least ``bytes'' number of bytes in the input
;;; buffer. Keeps calling do-input until that condition is met.
;;;
(defmacro input-at-least (stream bytes)
  (let ((stream-var (gensym))
	(bytes-var (gensym)))
    `(let ((,stream-var ,stream)
	   (,bytes-var ,bytes))
       (loop
	 (when (>= (- (fd-stream-ibuf-tail ,stream-var)
		      (fd-stream-ibuf-head ,stream-var))
		   ,bytes-var)
	   (return))
	 (do-input ,stream-var)))))

;;; INPUT-WRAPPER -- intenal
;;;
;;;   Macro to wrap around all input routines to handle eof-error noise. This
;;; should make provisions for filling stream-in-buffer.
;;;
(defmacro input-wrapper ((stream bytes eof-error eof-value) &body read-forms)
  (let ((stream-var (gensym))
	(element-var (gensym)))
    `(let ((,stream-var ,stream))
       (if (fd-stream-unread ,stream)
	 (prog1
	     (fd-stream-unread ,stream)
	   (setf (fd-stream-unread ,stream) nil)
	   (setf (fd-stream-listen ,stream) nil))
	 (let ((,element-var
		(catch 'eof-input-catcher
		  (input-at-least ,stream-var ,bytes)
		  ,@read-forms)))
	   (cond (,element-var
		  (incf (fd-stream-ibuf-head ,stream-var) ,bytes)
		  ,element-var)
		 (,eof-error
		  (error "EOF while reading ~S" stream))
		 (t
		  ,eof-value)))))))

;;; DEF-INPUT-ROUTINE -- internal
;;;
;;;   Defines an input routine.
;;;
(defmacro def-input-routine (name
			     (type size sap head)
			     &rest body)
  `(progn
     (defun ,name (stream eof-error eof-value)
       (input-wrapper (stream ,size eof-error eof-value)
	 (let ((,sap (fd-stream-ibuf-sap stream))
	       (,head (fd-stream-ibuf-head stream)))
	   ,@body)))
     (setf *input-routines*
	   (nconc *input-routines*
		  (list (list ',type ',name ',size))))))

;;; INPUT-CHARACTER -- internal
;;;
;;;   Routine to use in stream-in slot for reading string chars.
;;;
(def-input-routine input-character
		   (character 1 sap head)
  (code-char (sap-ref-8 sap head)))

;;; INPUT-UNSIGNED-8BIT-BYTE -- internal
;;;
;;;   Routine to read in an unsigned 8 bit number.
;;;
(def-input-routine input-unsigned-8bit-byte
		   ((unsigned-byte 8) 1 sap head)
  (sap-ref-8 sap head))

;;; INPUT-SIGNED-8BIT-BYTE -- internal
;;;
;;;   Routine to read in a signed 8 bit number.
;;;
(def-input-routine input-signed-8bit-number
		   ((signed-byte 8) 1 sap head)
  (signed-sap-ref-8 sap head))

;;; INPUT-UNSIGNED-16BIT-BYTE -- internal
;;;
;;;   Routine to read in an unsigned 16 bit number.
;;;
(def-input-routine input-unsigned-16bit-byte
		   ((unsigned-byte 16) 2 sap head)
  (sap-ref-16 sap (truncate head 2)))

;;; INPUT-SIGNED-16BIT-BYTE -- internal
;;;
;;;   Routine to read in a signed 16 bit number.
;;;
(def-input-routine input-signed-16bit-byte
		   ((signed-byte 16) 2 sap head)
  (signed-sap-ref-16 sap (truncate head 2)))

;;; INPUT-UNSIGNED-32BIT-BYTE -- internal
;;;
;;;   Routine to read in a unsigned 32 bit number.
;;;
(def-input-routine input-unsigned-32bit-byte
		   ((unsigned-byte 32) 4 sap head)
  (sap-ref-32 sap (truncate head 4)))

;;; INPUT-SIGNED-32BIT-BYTE -- internal
;;;
;;;   Routine to read in a signed 32 bit number.
;;;
(def-input-routine input-signed-32bit-byte
		   ((signed-byte 32) 4 sap head)
  (signed-sap-ref-32 sap (truncate head 2)))

;;; PICK-INPUT-ROUTINE -- internal
;;;
;;;   Find an input routine to use given the type. Return as multiple values
;;; the routine, the real type transfered, and the number of bytes per element.
;;;
(defun pick-input-routine (type)
  (dolist (entry *input-routines*)
    (when (subtypep type (car entry))
      (return (values (symbol-function (cadr entry))
		      (car entry)
		      (caddr entry))))))

;;; STRING-FROM-SAP -- internal
;;;
;;;   Returns a string constructed from the sap, start, and end.
;;;
(defun string-from-sap (sap start end)
  (let* ((length (- end start))
	 (string (make-string length)))
    (copy-from-system-area sap (* start vm:byte-bits)
			   string (* vm:vector-data-offset vm:word-bits)
			   (* length vm:byte-bits))
    string))

;;; FD-STREAM-READ-LINE -- internal
;;;
;;;   Reads a line, returning a simple string. Note: this relies on the fact
;;; that the input buffer does not change during do-input.
;;;
(defun fd-stream-read-line (stream eof-error-p eof-value)
  (let ((eof t))
    (values
     (or (let ((sap (fd-stream-ibuf-sap stream))
	       (results (when (fd-stream-unread stream)
			  (prog1
			      (list (string (fd-stream-unread stream)))
			    (setf (fd-stream-unread stream) nil)
			    (setf (fd-stream-listen stream) nil)))))
	   (catch 'eof-input-catcher
	     (loop
	       (input-at-least stream 1)
	       (let* ((head (fd-stream-ibuf-head stream))
		      (tail (fd-stream-ibuf-tail stream))
		      (newline (do ((index head (1+ index)))
				   ((= index tail) nil)
				 (when (= (sap-ref-8 sap index)
					  (char-code #\newline))
				   (return index))))
		      (end (or newline tail)))
		 (push (string-from-sap sap head end)
		       results)

		 (when newline
		   (setf eof nil)
		   (setf (fd-stream-ibuf-head stream)
			 (1+ newline))
		   (return))
		 (setf (fd-stream-ibuf-head stream) end))))
	   (cond ((null results)
		  nil)
		 ((null (cdr results))
		  (car results))
		 (t
		  (apply #'concatenate 'simple-string (nreverse results)))))
	 (if eof-error-p
	     (error "EOF while reading ~S" stream)
	     eof-value))
     eof)))


;;; FD-STREAM-READ-N-BYTES -- internal
;;;
;;; The n-bin routine.
;;; 
(defun fd-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (let* ((sap (fd-stream-ibuf-sap stream))
	 (elsize (fd-stream-element-size stream))
	 (offset (* elsize start))
	 (bytes (* elsize requested))
	 (result
	  (catch 'eof-input-catcher
	    (loop
	      (input-at-least stream 1)
	      (let* ((head (fd-stream-ibuf-head stream))
		     (tail (fd-stream-ibuf-tail stream))
		     (available (- tail head))
		     (copy (min available bytes)))
		(if (typep buffer 'system-area-pointer)
		    (system-area-copy sap (* head vm:byte-bits)
				      buffer (* offset vm:byte-bits)
				      (* copy vm:byte-bits))
		    (copy-from-system-area sap (* head vm:byte-bits)
					   buffer (+ (* offset vm:byte-bits)
						     (* vm:vector-data-offset
							vm:word-bits))
					   (* copy vm:byte-bits)))
		(incf (fd-stream-ibuf-head stream) copy)
		(incf offset copy)
		(decf bytes copy))
	      (when (zerop bytes)
		(return requested))))))
    (cond (result)
	  ((not eof-error-p)
	   (- requested (/ bytes elsize)))
	  (t
	   (error "Hit eof on ~S after reading ~D ~D~2:*-bit byte~P~*, ~
		   but ~D~2:* ~D-bit byte~P~:* ~[were~;was~:;were~] requested."
		  stream
		  (- requested (/ bytes elsize))
		  (* elsize 8)
		  requested)))))


;;;; Utility functions (misc routines, etc)

;;; SET-ROUTINES -- internal
;;;
;;;   Fill in the various routine slots for the given type. Input-p and output-p
;;; indicate what slots to fill. The buffering slot must be set prior to
;;; calling this routine.
;;;
(defun set-routines (stream type input-p output-p)
  (let ((target-type (case type
		       ((:default unsigned-byte)
			'(unsigned-byte 8))
		       (signed-byte
			'(signed-byte 8))
		       (t
			type)))
	(input-type nil)
	(output-type nil)
	(input-size nil)
	(output-size nil))
    
    (when (fd-stream-obuf-sap stream)
      (push (fd-stream-obuf-sap stream) *available-buffers*)
      (setf (fd-stream-obuf-sap stream) nil))
    (when (fd-stream-ibuf-sap stream)
      (push (fd-stream-ibuf-sap stream) *available-buffers*)
      (setf (fd-stream-ibuf-sap stream) nil))
    
    (when input-p
      (multiple-value-bind
	  (routine type size)
	  (pick-input-routine target-type)
	(unless routine
	  (error "Could not find any input routine for ~S" target-type))
	(setf (fd-stream-ibuf-sap stream) (next-available-buffer))
	(setf (fd-stream-ibuf-length stream) bytes-per-buffer)
	(setf (fd-stream-ibuf-tail stream) 0)
	(if (subtypep type 'character)
	  (setf (fd-stream-in stream) routine
		(fd-stream-bin stream) #'ill-bin
		(fd-stream-n-bin stream) #'ill-bin)
	  (setf (fd-stream-in stream) #'ill-in
		(fd-stream-bin stream) routine
		(fd-stream-n-bin stream) #'fd-stream-read-n-bytes))
	(setf input-size size)
	(setf input-type type)))

    (when output-p
      (multiple-value-bind
	  (routine type size)
	  (pick-output-routine target-type (fd-stream-buffering stream))
	(unless routine
	  (error "Could not find any output routine for ~S buffered ~S."
		 (fd-stream-buffering stream)
		 target-type))
	(setf (fd-stream-obuf-sap stream) (next-available-buffer))
	(setf (fd-stream-obuf-length stream) bytes-per-buffer)
	(setf (fd-stream-obuf-tail stream) 0)
	(if (subtypep type 'character)
	  (setf (fd-stream-out stream) routine
		(fd-stream-bout stream) #'ill-bout)
	  (setf (fd-stream-out stream)
		(or (if (eql size 1)
		      (pick-output-routine 'base-character
					   (fd-stream-buffering stream)))
		    #'ill-out)
		(fd-stream-bout stream) routine))
	(setf (fd-stream-sout stream)
	      (if (eql size 1) #'fd-sout #'ill-out))
	(setf (fd-stream-char-pos stream) 0)
	(setf output-size size)
	(setf output-type type)))

    (when (and input-size output-size
	       (not (eq input-size output-size)))
      (error "Element sizes for input (~S:~S) and output (~S:~S) differ?"
	     input-type input-size
	     output-type output-size))
    (setf (fd-stream-element-size stream)
	  (or input-size output-size))

    (setf (fd-stream-element-type stream)
	  (cond ((equal input-type output-type)
		 input-type)
		((or (null output-type) (subtypep input-type output-type))
		 input-type)
		((subtypep output-type input-type)
		 output-type)
		(t
		 (error "Input type (~S) and output type (~S) are unrelated?"
			input-type
			output-type))))))

;;; FD-STREAM-MISC-ROUTINE -- input
;;;
;;;   Handle the various misc operations on fd-stream.
;;;
(defun fd-stream-misc-routine (stream operation &optional arg1 arg2)
  (case operation
    (:read-line
     (fd-stream-read-line stream arg1 arg2))
    (:listen
     (or (not (eql (fd-stream-ibuf-head stream)
		   (fd-stream-ibuf-tail stream)))
	 (fd-stream-listen stream)
	 (setf (fd-stream-listen stream)
	       (not (zerop (mach:unix-select (1+ (fd-stream-fd stream))
					     (ash 1 (fd-stream-fd stream))
					     0
					     0
					     0))))))
    (:unread
     (setf (fd-stream-unread stream) arg1)
     (setf (fd-stream-listen stream) t))
    (:close
     (cond (arg1
	    ;; We got us an abort on our hands.
	    (when (and (fd-stream-file stream)
		       (fd-stream-obuf-sap stream))
	      ;; Can't do anything unless we know what file were dealing with,
	      ;; and we don't want to do anything strange unless we were
	      ;; writing to the file.
	      (if (fd-stream-original stream)
		  ;; Have an handle on the original, just revert.
		  (multiple-value-bind
		      (okay err)
		      (mach:unix-rename (fd-stream-original stream)
					(fd-stream-file stream))
		    (unless okay
		      (cerror "Go on as if nothing bad happened."
		        "Could not restore ~S to it's original contents: ~A"
			      (fd-stream-file stream)
			      (mach:get-unix-error-msg err))))
		  ;; Can't restore the orignal, so nuke that puppy.
		  (multiple-value-bind
		      (okay err)
		      (mach:unix-unlink (fd-stream-file stream))
		    (unless okay
		      (cerror "Go on as if nothing bad happened."
			      "Could not remove ~S: ~A"
			      (fd-stream-file stream)
			      (mach:get-unix-error-msg err)))))))
	   (t
	    (fd-stream-misc-routine stream :finish-output)
	    (when (and (fd-stream-original stream)
		       (fd-stream-delete-original stream))
	      (multiple-value-bind
		  (okay err)
		  (mach:unix-unlink (fd-stream-original stream))
		(unless okay
		  (cerror "Go on as if nothing bad happened."
			  "Could not delete ~S during close of ~S: ~A"
			  (fd-stream-original stream)
			  stream
			  (mach:get-unix-error-msg err)))))))
     (mach:unix-close (fd-stream-fd stream))
     (when (fd-stream-obuf-sap stream)
       (push (fd-stream-obuf-sap stream) *available-buffers*)
       (setf (fd-stream-obuf-sap stream) nil))
     (when (fd-stream-ibuf-sap stream)
       (push (fd-stream-ibuf-sap stream) *available-buffers*)
       (setf (fd-stream-ibuf-sap stream) nil))
     (lisp::set-closed-flame stream))
    (:clear-input
     (setf (fd-stream-ibuf-head stream) 0)
     (setf (fd-stream-ibuf-tail stream) 0)
     (loop
       (multiple-value-bind
	   (count errno)
	   (mach:unix-select (1+ fd) (ash 1 fd) 0 0 0)
	 (cond ((eql count 1)
		(do-input stream)
		(setf (fd-stream-ibuf-head stream) 0)
		(setf (fd-stream-ibuf-tail stream) 0))
	       (t
		(return))))))
    (:force-output
     (flush-output-buffer stream))
    (:finish-output
     (flush-output-buffer stream)
     #+serve-event
     (do ()
	 ((null (fd-stream-output-later stream)))
       (system:serve-all-events)))
    (:element-type
     (fd-stream-element-type stream))
    (:line-length
     80)
    (:charpos
     (fd-stream-char-pos stream))
    (:file-length
     (multiple-value-bind
	 (okay dev ino mode nlink uid gid rdev size
	       atime mtime ctime blksize blocks)
	 (mach:unix-fstat (fd-stream-fd stream))
       (declare (ignore ino nlink uid gid rdev
			atime mtime ctime blksize blocks))
       (unless okay
	 (error "Error fstating ~S: ~A"
		stream
		(mach:get-unix-error-msg dev)))
       (if (zerop mode)
	 nil
	 (/ size (fd-stream-element-size stream)))))
    (:file-position
     (fd-stream-file-position stream arg1))
    (:file-name
     (fd-stream-file stream))))

;;; FD-STREAM-FILE-POSITION -- internal.
;;;
(defun fd-stream-file-position (stream &optional newpos)
  (if (null newpos)
      (system:without-interrupts
	;; First, find the position of the UNIX file descriptor in the
	;; file.
	(multiple-value-bind
	    (posn errno)
	    (mach:unix-lseek (fd-stream-fd stream) 0 mach:l_incr)
	  (cond ((numberp posn)
		 ;; Adjust for buffered output:
		 ;;  If there is any output buffered, the *real* file position
		 ;; will be larger than reported by lseek because lseek
		 ;; obviously cannot take into account output we have not
		 ;; sent yet.
		 (dolist (later (fd-stream-output-later stream))
		   (incf posn (- (caddr later) (cadr later))))
		 (incf posn (fd-stream-obuf-tail stream))
		 ;; Adjust for unread input:
		 ;;  If there is any input read from UNIX but not supplied to
		 ;; the user of the stream, the *real* file position will
		 ;; smaller than reported, because we want to look like the
		 ;; unread stuff is still available.
		 (decf posn (- (fd-stream-ibuf-tail stream)
			       (fd-stream-ibuf-head stream)))
		 (when (fd-stream-unread stream)
		   (decf posn))
		 ;; Divide bytes by element size.
		 (/ posn (fd-stream-element-size stream)))
		((eq errno mach:espipe)
		 nil)
		(t
		 (system:with-interrupts
		   (error "Error lseek'ing ~S: ~A"
			  stream
			  (mach:get-unix-error-msg errno)))))))
      (let (offset origin)
	;; Make sure we don't have any output pending, because if we move the
	;; file pointer before writing this stuff, it will be written in the
	;; wrong location.
	(flush-output-buffer stream)
	(do ()
	    ((null (fd-stream-output-later stream)))
	  (system:serve-all-events))
	;; Clear out any pending input to force the next read to go to the
	;; disk.
	(setf (fd-stream-unread stream) nil)
	(setf (fd-stream-ibuf-head stream) 0)
	(setf (fd-stream-ibuf-tail stream) 0)
	;; Trash cashed value for listen, so that we check next time.
	(setf (fd-stream-listen stream) nil)
	;; Now move it.
	(cond ((eq newpos :start)
	       (setf offset 0 origin mach:l_set))
	      ((eq newpos :end)
	       (setf offset 0 origin mach:l_xtnd))
	      ((numberp newpos)
	       (setf offset (* newpos (fd-stream-element-size stream))
		     origin mach:l_set))
	      (t
	       (error "Invalid position given to file-position: ~S" newpos)))
	(multiple-value-bind
	    (posn errno)
	    (mach:unix-lseek (fd-stream-fd stream) offset origin)
	  (cond ((numberp posn)
		 t)
		((eq errno mach:espipe)
		 nil)
		(t
		 (error "Error lseek'ing ~S: ~A"
			stream
			(mach:get-unix-error-msg errno))))))))



;;;; Creation routines (MAKE-FD-STREAM and OPEN)

;;; MAKE-FD-STREAM -- Public.
;;;
;;; Returns a FD-STREAM on the given file.
;;;
(defun make-fd-stream (fd
		       &key
		       (input nil input-p)
		       (output nil output-p)
		       (element-type 'base-character)
		       (buffering :full)
		       file
		       original
		       delete-original
		       (name (if file
				 (format nil "file ~S" file)
				 (format nil "descriptor ~D" fd))))
  "Create a stream for the given unix file descriptor.  If input is non-nil,
   allow input operations.  If output is non-nil, allow output operations. If
   neither input nor output are specified, default to allowing input.
   element-type indicates the element type to use (as for open).  Buffering
   indicates the kind of buffering to use (one of :none, :line, or :full). If
   file is spesified, it should be the name of the file.  Name is used to
   identify the stream when printed."
  (cond ((not (or input-p output-p))
	 (setf input t))
	((not (or input output))
	 (error "File descriptor must be opened either for input or output.")))
  (let ((stream (%make-fd-stream :fd fd
				 :name name
				 :file file
				 :original original
				 :delete-original delete-original
				 :buffering buffering)))
    (set-routines stream element-type input output)
    stream))

;;; PICK-PACKUP-NAME -- internal
;;;
;;; Pick a name to use for the backup file.
;;;
(defvar *backup-extension* ".BAK"
  "This is a string that OPEN tacks on the end of a file namestring to produce
   a name for the :if-exists :rename-and-delete and :rename options.  Also,
   this can be a function that takes a namestring and returns a complete
   namestring.")
;;;
(defun pick-backup-name (name)
  (etypecase *backup-extension*
    (string (concatenate 'simple-string name *backup-extension*))
    (function (funcall *backup-extension* name))))

;;; ASSURE-ONE-OF -- internal
;;;
;;; Assure that the given arg is one of the given list of valid things.
;;; Allow the user to fix any problems.
;;; 
(defun assure-one-of (item list what)
  (unless (member item list)
    (loop
      (cerror "Enter new value for ~*~S"
	      "~S is invalid for ~S. Must be one of~{ ~S~}"
	      item
	      what
	      list)
      (format *query-io* "Enter new value for ~S: " what)
      (force-output *query-io*)
      (setf item (read *query-io*))
      (when (member item list)
	(return))))
  item)

;;; OPEN -- public
;;;
;;;   Open the given file.
;;;
(defun open (filename
	     &key
	     (direction :input)
	     (element-type 'base-character)
	     (if-exists nil if-exists-given)
	     (if-does-not-exist nil if-does-not-exist-given))
  "Return a stream which reads from or writes to Filename.
  Defined keywords:
   :direction - one of :input, :output, :io, or :probe
   :element-type - Type of object to read or write, default BASE-CHARACTER
   :if-exists - one of :error, :new-version, :rename, :rename-and-delete,
                       :overwrite, :append, :supersede or nil
   :if-does-not-exist - one of :error, :create or nil
  See the manual for details."
  ;; First, make sure that DIRECTION is valid. Allow it to be changed if not.
  (setf direction
	(assure-one-of direction
		       '(:input :output :io :probe)
		       :direction))

  ;; Calculate useful stuff.
  (multiple-value-bind
      (input output mask)
      (case direction
	(:input (values t nil mach:o_rdonly))
	(:output (values nil t mach:o_wronly))
	(:io (values t t mach:o_rdwr))
	(:probe (values nil nil mach:o_rdonly)))
    (let* ((pathname (pathname filename))
	   (namestring (predict-name pathname input)))
      
      ;; Process if-exists argument if we are doing any output.
      (cond (output
	     (unless if-exists-given
	       (setf if-exists
		     (if (eq (pathname-version pathname) :newest)
		       :new-version
		       :error)))
	     (setf if-exists
		   (assure-one-of if-exists
				  '(:error :new-version :rename
				    :rename-and-delete :overwrite
				    :append :supersede nil)
				  :if-exists))
	     (case if-exists
	       ((:error nil)
		(setf mask (logior mask mach:o_excl)))
	       ((:rename :rename-and-delete)
		(setf mask (logior mask mach:o_creat)))
	       ((:new-version :supersede)
		(setf mask (logior mask mach:o_trunc)))
	       (:append
		(setf mask (logior mask mach:o_append)))))
	    (t
	     (setf if-exists :ignore-this-arg)))
      
      (unless if-does-not-exist-given
	(setf if-does-not-exist
	      (cond ((eq direction :input) :error)
		    ((and output
			  (member if-exists '(:overwrite :append)))
		     :error)
		    ((eq direction :probe)
		     nil)
		    (t
		     :create))))
      (setf if-does-not-exist
	    (assure-one-of if-does-not-exist
			   '(:error :create nil)
			   :if-does-not-exist))
      (if (eq if-does-not-exist :create)
	(setf mask (logior mask mach:o_creat)))
       
      (let ((original (if (member if-exists
				  '(:rename :rename-and-delete))
			(pick-backup-name namestring)))
	    (delete-original (eq if-exists :rename-and-delete))
	    (mode #o666))
	(when original
	  ;; We are doing a :rename or :rename-and-delete.
	  ;; Determine if the file already exists, make sure the original
	  ;; file is not a directory and keep the mode
	  (let ((exists
		 (multiple-value-bind
		     (okay err/dev inode orig-mode)
		     (mach:unix-stat namestring)
		   (declare (ignore inode))
		   (cond (okay
			  (when (and output (= (logand orig-mode #o170000)
					       #o40000))
			    (error "Cannot open ~S for output: Is a directory."
				   namestring))
			  (setf mode (logand orig-mode #o777))
			  t)
			 ((eql err/dev mach:enoent)
			  nil)
			 (t
			  (error "Cannot find ~S: ~A"
				 namestring
				 (mach:get-unix-error-msg err/dev)))))))
	    (when (or (not exists)
		      ;; Do the rename.
		      (multiple-value-bind
			  (okay err)
			  (mach:unix-rename namestring original)
			(unless okay
			  (cerror "Use :SUPERSEDE instead."
				  "Could not rename ~S to ~S: ~A."
				  namestring
				  original
				  (mach:get-unix-error-msg err))
			  t)))
	      (setf original nil)
	      (setf delete-original nil)
	      ;; In order to use SUPERSEDE instead, we have
	      ;; to make sure mach:o_creat corresponds to
	      ;; if-does-not-exist.  mach:o_creat was set
	      ;; before because of if-exists being :rename.
	      (unless (eq if-does-not-exist :create)
		(setf mask (logior (logandc2 mask mach:o_creat) mach:o_trunc)))
	      (setf if-exists :supersede))))
	
	;; Okay, now we can try the actual open.
	(multiple-value-bind
	    (fd errno)
	    (mach:unix-open namestring mask mode)
	  (cond ((numberp fd)
		 (case direction
		   ((:input :output :io)
		    (make-fd-stream fd
				    :input input
				    :output output
				    :element-type element-type
				    :file namestring
				    :original original
				    :delete-original delete-original))
		   (:probe
		    (let ((stream (%make-fd-stream :name namestring
						   :fd fd
						   :element-type element-type)))
		      (close stream)
		      stream))))
		((eql errno mach:enoent)
		 (case if-does-not-exist
		   (:error
		    (cerror "Return NIL."
			    "Error opening ~S, ~A."
			    pathname
			    (mach:get-unix-error-msg errno)))
		   (:create
		    (cerror "Return NIL."
			    "Error creating ~S, path does not exist."
			    pathname)))
		 nil)
		((eql errno mach:eexist)
		 (unless (eq nil if-exists)
		   (cerror "Return NIL."
			   "Error opening ~S, ~A."
			   pathname
			   (mach:get-unix-error-msg errno)))
		 nil)
		(t
		 (cerror "Return NIL."
			 "Error opening ~S, ~A."
			 pathname
			 (mach:get-unix-error-msg errno))
		 nil)))))))

;;;; Initialization.

(defvar *tty* nil
  "The stream connected to the controlling terminal or NIL if there is none.")
(defvar *stdin* nil
  "The stream connected to the standard input (file descriptor 0).")
(defvar *stdout* nil
  "The stream connected to the standard output (file descriptor 1).")
(defvar *stderr* nil
  "The stream connected to the standard error output (file descriptor 2).")

;;; STREAM-INIT -- internal interface
;;;
;;; Called when the cold load is first started up.
;;; 
(defun stream-init ()
  (stream-reinit)
  (setf *terminal-io* (make-synonym-stream '*tty*))
  (setf *standard-input* (make-synonym-stream '*stdin*))
  (setf *standard-output* (make-synonym-stream '*stdout*))
  (setf *error-output* (make-synonym-stream '*stderr*))
  (setf *query-io* (make-synonym-stream '*terminal-io*))
  (setf *debug-io* *query-io*)
  (setf *trace-output* *standard-output*)
  nil)

;;; STREAM-REINIT -- internal interface
;;;
;;; Called whenever a saved core is restarted.
;;; 
(defun stream-reinit ()
  (setf *available-buffers* nil)
  (setf *stdin*
	(make-fd-stream 0 :name "Standard Input" :input t :buffering :line))
  (setf *stdout*
	(make-fd-stream 1 :name "Standard Output" :output t :buffering :line))
  (setf *stderr*
	(make-fd-stream 2 :name "Standard Error" :output t :buffering :line))
  (let ((tty (mach:unix-open "/dev/tty" mach:o_rdwr #o666)))
    (if tty
	(setf *tty*
	      (make-fd-stream tty :name "the Terminal" :input t :output t
			      :buffering :line))
	(setf *tty* (make-two-way-stream *stdin* *stdout*))))
  nil)


;;;; Beeping.

(defun default-beep-function (stream)
  (write-char #\bell stream)
  (finish-output stream))

(defvar *beep-function* #'default-beep-function
  "This is called in BEEP to feep the user.  It takes a stream.")

(defun beep (&optional (stream *terminal-io*))
  (funcall *beep-function* stream))


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
  (unless (streamp stream)
    (error "Argument ~S is not a stream." stream))
  (funcall (stream-misc stream) stream :file-position position))

;;; File-Length  --  Public
;;;
;;;    Like File-Position, only use :file-length.
;;;
(defun file-length (stream)
  "This function returns the length of the file that File-Stream is open to."
  (unless (streamp stream)
    (error "Argument ~S is not a stream." stream))
  (funcall (stream-misc stream) stream :file-length))

;;; File-Name  --  internal interface
;;;
;;;    Kind of like File-Position, but is an internal hack used by the filesys
;;; stuff to get and set the file name.
;;;
(defun file-name (stream &optional new-name)
  (when (fd-stream-p stream)
    (if new-name
	(setf (fd-stream-file stream) new-name)
	(fd-stream-file stream))))
