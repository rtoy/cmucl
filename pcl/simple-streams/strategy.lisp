;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/strategy.lisp,v 1.2 2003/06/07 17:56:28 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Strategy functions for base simple-stream classes

(in-package "STREAM")

;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext:define-function-name-syntax sc (name)
    ;; (sc <name> <external-format> [<access>])
    (if (and (<= 3 (length name) 4)
	     (symbolp (second name))
	     (keywordp (third name)))
        (values t (second name))
        (values nil nil)))

  (ext:define-function-name-syntax dc (name)
    ;; (dc <name> <external-format>)
    (if (and (= (length name) 3)
	     (symbolp (second name))
	     (keywordp (third name)))
        (values t (second name))
        (values nil nil)))

  (ext:define-function-name-syntax str (name)
    ;; (str <name> [<composing-format>])
    (if (and (<= 2 (length name) 3)
	     (symbolp (second name))
	     (or (null (third name))
	         (keywordp (third name))))
        (values t (second name))
        (values nil nil))))


;;;; Helper functions
(defun sc-refill-buffer (stream blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (let* ((unread (sm last-char-read-size stream))
           (buffer (sm buffer stream)))
      (unless (zerop unread)
        (buffer-copy buffer (- (sm buffer-ptr stream) unread) buffer 0 unread))
      (let ((bytes (device-read stream nil unread nil blocking)))
        (declare (type fixnum bytes))
        (setf (sm buffpos stream) unread
              (sm buffer-ptr stream) (if (plusp bytes)
                                         (+ bytes unread)
                                         unread))
        bytes))))

(defun sc-flush-buffer (stream blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (let ((ptr 0)
          (bytes (sm buffpos stream)))
      (declare (type fixnum ptr bytes))
      (loop
        (when (>= ptr bytes) (setf (sm buffpos stream) 0) (return 0))
        (let ((bytes-written (device-write stream nil ptr nil blocking)))
          (declare (fixnum bytes-written))
          (when (minusp bytes-written)
            (error "DEVICE-WRITE error."))
          (incf ptr bytes-written))))))

(defun dc-refill-buffer (stream blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let* ((unread (sm last-char-read-size stream))
           (buffer (sm buffer stream)))
      (unless (zerop unread)
        (buffer-copy buffer (- (sm buffer-ptr stream) unread) buffer 0 unread))
      (let ((bytes (device-read stream nil unread nil blocking)))
        (declare (type fixnum bytes))
        (setf (sm buffpos stream) unread
              (sm buffer-ptr stream) (if (plusp bytes)
                                         (+ bytes unread)
                                         unread))
        bytes))))

(defun dc-flush-buffer (stream blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((ptr 0)
          (bytes (sm outpos stream)))
      (declare (type fixnum ptr bytes))
      (loop
        (when (>= ptr bytes) (setf (sm outpos stream) 0) (return 0))
        (let ((bytes-written (device-write stream nil ptr nil blocking)))
          (declare (fixnum bytes-written))
          (when (minusp bytes-written)
            (error "DEVICE-WRITE error."))
          (incf ptr bytes-written))))))


;;;; Single-Channel-Simple-Stream strategy functions

(declaim (ftype j-listen-fn (sc listen :iso8859-1)))
(defun (sc listen :iso8859-1) (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (or (< (sm buffpos stream) (sm buffer-ptr stream))
        (case (device-read stream nil 0 0 nil)
          ((0 -2) nil)
          (-1 #| latch EOF |# nil)
          (-3 t)
          (t (error "DEVICE-READ error."))))))

(declaim (ftype j-read-char-fn (sc read-char :iso8859-1)))
(defun (sc read-char :iso8859-1) (stream eof-error-p eof-value blocking)
  #|(declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))|#
  (with-stream-class (single-channel-simple-stream stream)
    ;; if stream is open for read-write, may need to flush the buffer
    (let* ((buffer (sm buffer stream))
           (ptr (sm buffpos stream))
           (code (if (< ptr (sm buffer-ptr stream))
                     (progn
                       (setf (sm buffpos stream) (1+ ptr))
                       (bref buffer ptr))
                     (let ((bytes (sc-refill-buffer stream blocking)))
                       (declare (type fixnum bytes))
                       (unless (minusp bytes)
                         (let ((ptr (sm buffpos stream)))
                           (setf (sm buffpos stream) (1+ ptr))
                           (bref buffer ptr))))))
           (char (if code (code-char code) nil))
           (ctrl (sm control-in stream)))
      (when code
        (setf (sm last-char-read-size stream) 1)
        (when (and (< code 32) ctrl (svref ctrl code))
          ;; Does this have to be a function, or can it be a symbol?
          (setq char (funcall (the (or symbol function) (svref ctrl code))
                              stream char))))
      (if (null char)
          (cl::eof-or-lose stream eof-error-p eof-value)
          char))))

(declaim (ftype j-read-char-fn (sc read-char :iso8859-1 buffer)))
(defun (sc read-char :iso8859-1 buffer) (stream eof-error-p eof-value blocking)
  (declare (ignore blocking)) ;; everything is already in the buffer
  #|(declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))|#
  (with-stream-class (single-channel-simple-stream stream)
    (let* ((buffer (sm buffer stream))
           (ptr (sm buffpos stream))
           (code (when (< ptr (sm buffer-ptr stream))
                   (setf (sm buffpos stream) (1+ ptr))
                   (bref buffer ptr)))
           (char (if code (code-char code) nil))
           (ctrl (sm control-in stream)))
      (when code
        (setf (sm last-char-read-size stream) 1)
        (when (and (< code 32) ctrl (svref ctrl code))
          ;; Does this have to be a function, or can it be a symbol?
          (setq char (funcall (the (or symbol function) (svref ctrl code))
                              stream char))))
      (if (null char)
          (cl::eof-or-lose stream eof-error-p eof-value)
          char))))

(declaim (ftype j-read-chars-fn (sc read-chars :iso8859-1)))
(defun (sc read-chars :iso8859-1) (stream string search start end blocking)
  ;; string is filled from START to END, or until SEARCH is found
  ;; Return two values: count of chars read and
  ;;  NIL if SEARCH was not found
  ;;  T if SEARCH was found
  ;;  :EOF if eof encountered before end
  (declare (type simple-stream stream)
           (type string string)
           (type (or null character) search)
           (type fixnum start end)
           (type boolean blocking)
	   #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (single-channel-simple-stream stream)
    (setf (sm last-char-read-size stream) 0)
    ;; Should arrange for the last character to be unreadable
    (do ((buffer (sm buffer stream))
         (ptr (sm buffpos stream))
         (max (sm buffer-ptr stream))
	 (ctrl (sm control-in stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((= posn end) (setf (sm buffpos stream) ptr) (values count nil))
      (declare (type fixnum ptr max posn count))
      (let* ((code (if (< ptr max)
                       (prog1
                           (bref buffer ptr)
                         (incf ptr))
                       (let ((bytes (sc-refill-buffer stream blocking)))
                         (declare (type fixnum bytes))
                         (setf ptr (sm buffpos stream)
                               max (sm buffer-ptr stream))
                         (when (plusp bytes)
                           (prog1
                               (bref buffer ptr)
                             (incf ptr))))))
             (char (if code (code-char code) nil)))
        (when (and code (< code 32) ctrl (svref ctrl code))
          (setq char (funcall (the (or symbol function) (svref ctrl code))
                              stream char)))
        (cond ((null char)
               (setf (sm buffpos stream) ptr)
               (return (values count :eof)))
              ((and search (char= char search))
               (setf (sm buffpos stream) ptr)
               (return (values count t)))
              (t
               (setf (char string posn) char)))))))

(declaim (ftype j-read-chars-fn (sc read-chars :iso8859-1 buffer)))
(defun (sc read-chars :iso8859-1 buffer) (stream string search start end
						 blocking)
  (declare (type simple-stream stream)
           (type string string)
           (type (or null character) search)
           (type fixnum start end)
           (type boolean blocking)
	   #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (single-channel-simple-stream stream)
    (do ((buffer (sm buffer stream))
         (ptr (sm buffpos stream))
         (max (sm buffer-ptr stream))
	 (ctrl (sm control-in stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((= posn end)
         (setf (sm buffpos stream) ptr)
         (unless (zerop count) (setf (sm last-char-read-size stream) 1))
         (values count nil))
      (declare (type fixnum ptr max posn count))
      (let* ((code (when (< ptr max)
                     (prog1
                         (bref buffer ptr)
                       (incf ptr))))
             (char (if code (code-char code) nil)))
        (when (and code (< code 32) ctrl (svref ctrl code))
          (setq char (funcall (the (or symbol function) (svref ctrl code))
                              stream char)))
        (cond ((null char)
               (setf (sm buffpos stream) ptr)
               (unless (zerop count) (setf (sm last-char-read-size stream) 1))
               (return (values count :eof)))
              ((and search (char= char search))
               (setf (sm buffpos stream) ptr)
               ;; Unread of last char must unread the search character, too
               ;; If no characters were read, just add the length of the
               ;; search char to that of the previously read char.
               (if (zerop count)
                   (incf (sm last-char-read-size stream))
                   (setf (sm last-char-read-size stream) 2))
               (return (values count t)))
              (t
               (setf (char string posn) char)))))))

(declaim (ftype j-unread-char-fn (sc unread-char :iso8859-1)))
(defun (sc unread-char :iso8859-1) (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (single-channel-simple-stream stream)
    (let ((unread (sm last-char-read-size stream)))
      (if (>= (sm buffpos stream) unread)
          (decf (sm buffpos stream) unread)
          (error "Unreading needs work"))
      (setf (sm last-char-read-size stream) 0))))

(declaim (ftype j-write-char-fn (sc write-char :iso8859-1)))
(defun (sc write-char :iso8859-1) (character stream)
  (when character
    (with-stream-class (single-channel-simple-stream stream)
      (let ((code (char-code character))
	    (ctrl (sm control-out stream)))
	(when (and (< code 32) ctrl (svref ctrl code)
		   (funcall (the (or symbol function) (svref ctrl code))
			    stream character))
	  (return-from write-char character))
	(let ((buffer (sm buffer stream))
	      (ptr (sm buffpos stream)))
	  (when (>= ptr (sm buf-len stream))
	    (setf ptr (sc-flush-buffer stream t)))
	  (setf (bref buffer ptr) code)
	  (setf (sm buffpos stream) (1+ ptr))))))
  character)

(declaim (ftype j-write-chars-fn (sc write-chars :iso8859-1)))
(defun (sc write-chars :iso8859-1) (string stream start end)
  (with-stream-class (single-channel-simple-stream stream)
    (do ((buffer (sm buffer stream))
         (ptr (sm buffpos stream))
         (max (sm buf-len stream))
         (ctrl (sm control-out stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end) (setf (sm buffpos stream) ptr) count)
      (declare (type fixnum ptr max posn count))
      (let* ((char (char string posn))
             (code (char-code char)))
        (unless (and (< code 32) ctrl (svref ctrl code)
                     (funcall (the (or symbol function) (svref ctrl code))
                              stream char))
          (when (>= ptr max)
	    (setf (sm buffpos stream) ptr)
	    (setf ptr (sc-flush-buffer stream t)))
	  (setf (bref buffer ptr) code)
	  (incf ptr))))))


;;; SC-READ-BYTE doesn't actually live in a strategy slot
(defun sc-read-byte (stream eof-error-p eof-value blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
        (let ((bytes (device-read stream nil 0 nil blocking)))
          (declare (type fixnum bytes))
          (if (plusp bytes)
              (setf (sm buffer-ptr stream) bytes
                    ptr 0)
              (return-from sc-read-byte
                (cl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))

;;;; Dual-Channel-Simple-Stream strategy functions

(declaim (ftype j-listen-fn (dc listen :iso8859-1)))
(defun (dc listen :iso8859-1) (stream)
  (with-stream-class (dual-channel-simple-stream stream)
    (or (< (sm buffpos stream) (sm buffer-ptr stream))
        (case (device-read stream nil 0 0 nil)
          ((0 -2) nil)
          (-1 #| latch EOF |# nil)
          (-3 t)
          (t (error "DEVICE-READ error."))))))

(declaim (ftype j-read-char-fn (dc read-char :iso8859-1)))
(defun (dc read-char :iso8859-1) (stream eof-error-p eof-value blocking)
  #|(declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))|#
  (with-stream-class (dual-channel-simple-stream stream)
    (when (and (any-stream-instance-flags stream :interactive)
	       (any-stream-instance-flags stream :output))
      (finish-output stream))
    (let* ((buffer (sm buffer stream))
           (ptr (sm buffpos stream))
           (code (if (< ptr (sm buffer-ptr stream))
                     (progn
                       (setf (sm buffpos stream) (1+ ptr))
                       (bref buffer ptr))
                     (let ((bytes (dc-refill-buffer stream blocking)))
                       (declare (type fixnum bytes))
                       (unless (minusp bytes)
                         (let ((ptr (sm buffpos stream)))
                           (setf (sm buffpos stream) (1+ ptr))
                           (bref buffer ptr))))))
           (char (if code (code-char code) nil))
           (ctrl (sm control-in stream)))
      (when code
        (setf (sm last-char-read-size stream) 1)
        (when (and (< code 32) ctrl (svref ctrl code))
          ;; Does this have to be a function, or can it be a symbol?
          (setq char (funcall (the (or symbol function) (svref ctrl code))
                              stream char))))
      (if (null char)
          (cl::eof-or-lose stream eof-error-p eof-value)
          char))))

(declaim (ftype j-read-chars-fn (dc read-chars :iso8859-1)))
(defun (dc read-chars :iso8859-1) (stream string search start end blocking)
  (declare (type dual-channel-simple-stream stream)
           (type string string)
           (type (or null character) search)
           (type fixnum start end)
           (type boolean blocking)
           #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (dual-channel-simple-stream stream)
    ;; if interactive flag is set, finish-output first
    (setf (sm last-char-read-size stream) 0)
    ;; Should arrange for the last character to be unreadable
    (do ((buffer (sm buffer stream))
         (ptr (sm buffpos stream))
         (max (sm buffer-ptr stream))
	 (ctrl (sm control-in stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end) (setf (sm buffpos stream) ptr) (values count nil))
      (declare (type fixnum ptr max posn count))
      (let* ((code (if (< ptr max)
                       (prog1
                           (bref buffer ptr)
                         (incf ptr))
                       (let ((bytes (dc-refill-buffer stream blocking)))
                         (declare (type fixnum bytes))
                         (setf ptr (sm buffpos stream)
                               max (sm buffer-ptr stream))
                         (when (plusp bytes)
                           (prog1
                               (bref buffer ptr)
                             (incf ptr))))))
             (char (if code (code-char code) nil)))
        (when (and code (< code 32) ctrl (svref ctrl code))
          (setq char (funcall (the (or symbol function) (svref ctrl code))
                              stream char)))
        #|(let ((column (sm charpos stream)))
          (declare (type (or null fixnum) column))
          (when column
            (setf (sm charpos stream) (1+ column))))|#
        (cond ((null char)
               (setf (sm buffpos stream) ptr)
               (return (values count :eof)))
              ((and search (char= char search))
               (setf (sm buffpos stream) ptr)
               (return (values count t)))
              (t
               (setf (char string posn) char)))))))

(declaim (ftype j-unread-char-fn (dc unread-char :iso8859-1)))
(defun (dc unread-char :iso8859-1) (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((unread (sm last-char-read-size stream)))
      (if (>= (sm buffpos stream) unread)
          (decf (sm buffpos stream) unread)
          (error "Unreading needs work"))
      (setf (sm last-char-read-size stream) 0))))

(declaim (ftype j-write-char-fn (dc write-char :iso8859-1)))
(defun (dc write-char :iso8859-1) (character stream)
  (when character
    (with-stream-class (dual-channel-simple-stream stream)
      (let ((code (char-code character))
	    (ctrl (sm control-out stream)))
	(when (and (< code 32) ctrl (svref ctrl code)
		   (funcall (the (or symbol function) (svref ctrl code))
			    stream character))
	  (return-from write-char character))
	(let ((buffer (sm out-buffer stream))
	      (ptr (sm outpos stream)))
	  (when (>= ptr (sm max-out-pos stream))
	    (setq ptr (dc-flush-buffer stream t)))
	  (setf (bref buffer ptr) code)
	  (setf (sm outpos stream) (1+ ptr))))))
  character)

(declaim (ftype j-write-chars-fn (dc write-chars :iso8859-1)))
(defun (dc write-chars :iso8859-1) (string stream start end)
  (with-stream-class (dual-channel-simple-stream stream)
    (do ((buffer (sm out-buffer stream))
         (ptr (sm outpos stream))
         (max (sm max-out-pos stream))
         (ctrl (sm control-out stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end) (setf (sm outpos stream) ptr) count)
      (declare (type fixnum ptr max posn count))
      (let* ((char (char string posn))
             (code (char-code char)))
        (unless (and (< code 32) ctrl (svref ctrl code)
                     (funcall (the (or symbol function) (svref ctrl code))
                              stream char))
          (when (>= ptr max)
	    (setf (sm outpos stream) ptr)
	    (setf ptr (dc-flush-buffer stream ptr t)))
	  (setf (bref buffer ptr) code)
	  (incf ptr))))))


;;; DC-READ-BYTE doesn't actually live in a strategy slot
(defun dc-read-byte (stream eof-error-p eof-value blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
        (let ((bytes (device-read stream nil 0 nil blocking)))
          (declare (type fixnum bytes))
          (if (plusp bytes)
              (setf (sm buffer-ptr stream) bytes
                    ptr 0)
              (return-from dc-read-byte
                (cl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))

;;;; String-Simple-Stream strategy functions

(declaim (ftype j-read-char-fn (str read-char)))
#+(or)
(defun (str read-char) (stream eof-error-p eof-value blocking)
  (declare (type string-input-simple-stream stream) (ignore blocking)
           #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (string-input-simple-stream stream)
    (when (any-stream-instance-flags stream :eof)
      (cl::eof-or-lose stream eof-error-p eof-value))
    (let* ((ptr (sm buffpos stream))
           (char (if (< ptr (sm buffer-ptr stream))
                     (schar (sm buffer stream) ptr)
                     nil)))
      (if (null char)
          (cl::eof-or-lose stream eof-error-p eof-value)
          (progn
            (setf (sm last-char-read-size stream) 1)
            ;; do string-streams do control-in processing?
            #|(let ((column (sm charpos stream)))
              (declare (type (or null fixnum) column))
              (when column
                (setf (sm charpos stream) (1+ column))))|#
            char)))))



(declaim (ftype j-read-char-fn (str read-char :e-crlf)))
(defun (str read-char :e-crlf) (stream eof-error-p eof-value blocking)
  (with-stream-class (composing-stream stream)
    (let* ((melded-stream (sm melded-stream stream))
           (char (funcall-stm-handler j-read-char melded-stream nil stream
                                      blocking)))
      ;; if CHAR is STREAM, we hit EOF; if NIL, blocking is NIL and no
      ;; character was available...
      (when (eql char #\Return)
        (let ((next (funcall-stm-handler j-read-char melded-stream
                                         nil stream blocking)))
          ;; if NEXT is STREAM, we hit EOF, so we should just return the
          ;; #\Return (and mark the stream :EOF?  At least unread if we
          ;; got a soft EOF, from a terminal, etc.
          ;; if NEXT is NIL, blocking is NIL and there's a CR but no
          ;; LF available on the stream: have to unread the CR and
          ;; return NIL, letting the CR be reread later.
          ;;
          ;; If we did get a linefeed, adjust the last-char-read-size
          ;; so that an unread of the resulting newline will unread both
          ;; the linefeed _and_ the carriage return.
          (if (eql next #\Linefeed)
              (setq char #\Newline)
              (funcall-stm-handler j-unread-char melded-stream nil))))
      ;; do control-in processing on whatever character we've got
      char)))

(declaim (ftype j-unread-char-fn (str unread-char :e-crlf)))
(defun (str unread-char :e-crlf) (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (composing-stream stream)
    (funcall-stm-handler j-unread-char (sm melded-stream stream) nil)))


;;;; Functions to install the strategy functions in the appropriate slots

(defun %find-topmost-stream (stream)
  ;; N.B.: the topmost stream in the chain of encapsulations is actually
  ;; the bottommost in the "melding" chain
  (with-stream-class (simple-stream)
    (loop
      (when (eq (sm melded-stream stream) (sm melding-base stream))
	(return stream))
      (setq stream (sm melded-stream stream)))))

(defun install-single-channel-character-strategy (stream external-format
                                                         access)
  (find-external-format external-format)
  (let ((stream (%find-topmost-stream stream)))
    ;; ACCESS is usually NIL
    ;; May be "undocumented" values: stream::buffer, stream::mapped
    ;;   to install strategies suitable for direct buffer streams
    ;;   (i.e., ones that call DEVICE-EXTEND instead of DEVICE-READ)
    ;; (Avoids checking "mode" flags by installing special strategy)
    (with-stream-class (simple-stream stream)
      (if (or (eq access 'buffer) (eq access 'mapped))
	  (setf (sm j-read-char stream) #'(sc read-char :iso8859-1 buffer)
		(sm j-read-chars stream) #'(sc read-chars :iso8859-1 buffer)
		(sm j-unread-char stream) #'(sc unread-char :iso8859-1)
		(sm j-write-char stream) #'(sc write-char :iso8859-1)
		(sm j-write-chars stream) #'(sc write-chars :iso8859-1)
		(sm j-listen stream) #'(sc listen :iso8859-1))
	  (setf (sm j-read-char stream) #'(sc read-char :iso8859-1)
		(sm j-read-chars stream) #'(sc read-chars :iso8859-1)
		(sm j-unread-char stream) #'(sc unread-char :iso8859-1)
		(sm j-write-char stream) #'(sc write-char :iso8859-1)
		(sm j-write-chars stream) #'(sc write-chars :iso8859-1)
		(sm j-listen stream) #'(sc listen :iso8859-1)))))
  stream)

(defun install-dual-channel-character-strategy (stream external-format)
  (find-external-format external-format)
  (let ((stream (%find-topmost-stream stream)))
    (with-stream-class (simple-stream stream)
      (setf (sm j-read-char stream) #'(dc read-char :iso8859-1)
	    (sm j-read-chars stream) #'(dc read-chars :iso8859-1)
	    (sm j-unread-char stream) #'(dc unread-char :iso8859-1)
	    (sm j-write-char stream) #'(dc write-char :iso8859-1)
	    (sm j-write-chars stream) #'(dc write-chars :iso8859-1)
	    (sm j-listen stream) #'(dc listen :iso8859-1))))
  stream)

;; Deprecated -- use install-string-{input,output}-character-strategy instead!
(defun install-string-character-strategy (stream)
  (install-string-input-character-strategy stream)
  (when (any-stream-instance-flags stream :output)
    (install-string-output-character-strategy stream))
  stream)

(defun install-string-input-character-strategy (stream)
  #| implement me |#
  (let ((stream (%find-topmost-stream stream)))
    (with-stream-class (simple-stream stream)
      (setf (sm j-read-char stream) #'(str read-char))))
  stream)

(defun install-string-output-character-strategy (stream)
  #| implement me |#)

(defun compose-encapsulating-streams (stream external-format)
  (when (consp external-format)
    (with-stream-class (simple-stream)
      (dolist (fmt (butlast external-format))
	(let ((encap (make-instance 'composing-stream :composing-format fmt)))
	  (setf (sm melding-base encap) stream)
	  (setf (sm melded-stream encap) (sm melded-stream stream))
	  (setf (sm melded-stream stream) encap)
	  (rotatef (sm j-listen encap) (sm j-listen stream))
	  (rotatef (sm j-read-char encap) (sm j-read-char stream))
	  (rotatef (sm j-read-chars encap) (sm j-read-chars stream))
	  (rotatef (sm j-unread-char encap) (sm j-unread-char stream))
	  (rotatef (sm j-write-char encap) (sm j-write-char stream))
	  (rotatef (sm j-write-chars encap) (sm j-write-chars stream)))))))
