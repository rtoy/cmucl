;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/impl.lisp,v 1.3 2003/06/18 09:23:08 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementations of standard Common Lisp functions for simple-streams

(in-package "STREAM")

(defun %uninitialized (stream)
  (error "~S has not been initialized." stream))

(defun %check (stream kind)
  (declare (type simple-stream stream))
  (cond ((not (any-stream-instance-flags stream :simple))
	 (%uninitialized stream))
	((and (eq kind :open)
	      (not (any-stream-instance-flags stream :input :output)))
	 (lisp::closed-flame stream))
	((and (or (eq kind :input) (eq kind :io))
	      (not (any-stream-instance-flags stream :input)))
	 (lisp::ill-in-any stream))
	((and (or (eq kind :output) (eq kind :io))
	      (not (any-stream-instance-flags stream :output)))
	 (lisp::ill-out-any stream))))

#+count-sm
(progn
  (defvar *sm-r-count* (make-hash-table))
  (defvar *sm-w-count* (make-hash-table))
  (defun %sm (slot object)
    (incf (gethash slot *sm-r-count* 0))
    (slot-value object slot))
  (defun (setf %sm) (value slot object)
    (incf (gethash slot *sm-w-count* 0))
    (setf (slot-value object slot) value)))

(defun %input-stream-p (stream)
  (declare (type simple-stream stream))
  (%check stream nil)
  (any-stream-instance-flags stream :input))

(defun %output-stream-p (stream)
  (declare (type simple-stream stream))
  (%check stream nil)
  (any-stream-instance-flags stream :output))

(defun %open-stream-p (stream)
  (declare (type simple-stream stream))
  (%check stream nil)
  (any-stream-instance-flags stream :input :output))

(defun %interactive-stream-p (stream)
  (declare (type simple-stream stream))
  (%check stream :open)
  (any-stream-instance-flags stream :interactive))

(defun %interactive-stream-y (stream)
  (declare (type simple-stream stream))
  (%check stream :open)
  (add-stream-instance-flags stream :interactive))

(defun %interactive-stream-n (stream)
  (declare (type simple-stream stream))
  (%check stream :open)
  (remove-stream-instance-flags stream :interactive))

(defun %stream-external-format (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (sm external-format stream)))

(defun %charpos (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (sm charpos stream)))

(defun %file-position (stream position)
  (declare (type simple-stream stream)
	   (type (or (integer 0 *) (member nil :start :end)) position))
  (%check stream :open)
  (cond (position
	 (let ((position (case position (:start 0) (:end -1)
			       (otherwise position))))
	   ;; set unread to zero
	   ;; if position is within buffer, just move pointer; else
	   ;; flush output, if necessary
	   ;; set buffer pointer to 0, to force a read
	   (setf (device-file-position stream) position)))
	(t
	 (let ((posn (device-file-position stream)))
	   ;; adjust for buffer position
	   posn
	   ))))

(defun %file-length (stream)
  (declare (type simple-stream stream))
  (%check stream :open)
  (device-file-length stream)
  ;; implement me
  )

(defun %file-name (stream)
  (declare (type simple-stream stream))
  (if (typep stream 'file-simple-stream)
      (with-stream-class (file-simple-stream stream)
	(%check stream nil)
	(sm pathname stream))
      nil))

(defun %file-rename (stream new-name)
  (declare (type simple-stream stream))
  (if (typep stream 'file-simple-stream)
      (with-stream-class (file-simple-stream stream)
	(%check stream nil)
	(setf (sm pathname stream) new-name)
	(setf (sm filename stream) (ext:unix-namestring new-name nil))
	t)
      nil))

(defun %file-string-length (stream object)
  (declare (type simple-stream stream))
  (%check stream :open)
  (etypecase object
    (character 1)
    (string (length object))))

(defun %read-line (stream eof-error-p eof-value recursive-p)
  (declare (type simple-stream stream)
	   (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (let* ((encap (sm melded-stream stream)) ; encapsulating stream
	   (cbuf (make-string 80))	; current buffer
	   (bufs (list cbuf))		; list of buffers
	   (tail bufs)			; last cons of bufs list
	   (index 0)			; current index in current buffer
	   (total 0))			; total characters
      (declare (type simple-stream encap)
	       (type simple-base-string cbuf)
	       (type cons bufs tail)
	       (type fixnum index total))
      (loop
	(multiple-value-bind (chars done)
	    (funcall-stm-handler j-read-chars encap cbuf
				 #\Newline index (length cbuf) t)
	  (declare (type fixnum chars))
	  (incf index chars)
	  (incf total chars)
	  (when (and (eq done :eof) (zerop index))
	    (if eof-error-p
		(error 'end-of-file :stream stream)
		(return (values eof-value t))))
	  (when done
	    ;; If there's only one buffer in use, return it directly
	    (when (null (cdr bufs))
	      (return (values (lisp::shrink-vector cbuf index)
			      (eq done :eof))))
	    ;; If total fits in final buffer, use it
	    #-(or)
	    (when (<= total (length cbuf))
	      (replace cbuf cbuf :start1 (- total index) :end2 index)
	      (let ((idx 0))
		(declare (type fixnum idx))
		(dolist (buf bufs)
		  (declare (type simple-base-string buf))
		  (replace cbuf buf :start1 idx)
		  (incf idx (length buf))))
	      (return (values (lisp::shrink-vector cbuf index)
			      (eq done :eof))))
	    ;; Allocate new string of appropriate length
	    (let ((string (make-string total))
		  (index 0))
	      (declare (type fixnum index))
	      (dolist (buf bufs)
		(declare (type simple-base-string buf))
		(replace string buf :start1 index)
		(incf index (length buf)))
	      (return  (values string (eq done :eof)))))
	  (when (>= index (length cbuf))
	    (setf cbuf (make-string (the fixnum (* 2 index))))
	    (setf index 0)
	    (setf (cdr tail) (cons cbuf nil))
	    (setf tail (cdr tail))))))))

(defun %read-char (stream eof-error-p eof-value recursive-p blocking-p)
  (declare (type simple-stream stream)
	   (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (funcall-stm-handler j-read-char (sm melded-stream stream)
			 eof-error-p eof-value blocking-p)))

(defun %unread-char (stream character)
  (declare (type simple-stream stream) (ignore character))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (zerop (sm last-char-read-size stream))
	(error "Nothing to unread.")
	(funcall-stm-handler j-unread-char (sm melded-stream stream) nil))))

(defun %peek-char (stream peek-type eof-error-p eof-value recursive-p)
  (declare (type simple-stream stream)
	   (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (let* ((encap (sm melded-stream stream))
	   (char (funcall-stm-handler j-read-char encap
				     eof-error-p stream t)))
      (cond ((eq char stream) eof-value)
	    ((characterp peek-type)
	     (do ((char char (funcall-stm-handler j-read-char encap
						  eof-error-p
						  stream t)))
		 ((or (eq char stream) (char= char peek-type))
		  (unless (eq char stream)
		    (funcall-stm-handler j-unread-char encap t))
		  (if (eq char stream) eof-value char))))
	    ((eq peek-type t)
	     (do ((char char (funcall-stm-handler j-read-char encap
						  eof-error-p
						  stream t)))
		 ((or (eq char stream)
		      (not (lisp::whitespace-char-p char)))
		  (unless (eq char stream)
		    (funcall-stm-handler j-unread-char encap t))
		  (if (eq char stream) eof-value char))))
	    (t
	     (funcall-stm-handler j-unread-char encap t)
	     char)))))

(defun %listen (stream width)
  (declare (type simple-stream stream))
  ;; WIDTH is number of octets which must be available; any value
  ;; other than 1 is treated as 'character.
  (%check stream :input)
  (simple-stream-dispatch stream
    ;; single-channel-simple-stream
    (with-stream-class (single-channel-simple-stream stream)
      (if (not (eql width 1))
	  (funcall-stm-handler j-listen stream)
	  (or (< (sm buffpos stream) (sm buffer-ptr stream))
	      ;; Note: should try DEVICE-EXTEND for more on buffer streams
	      (when (>= (sm mode stream) 0) ;; device-connected
		(incf (sm last-char-read-size stream))
		(let ((ok (sc-refill-buffer stream nil)))
		  (decf (sm last-char-read-size stream))
		  (plusp ok))))))
    ;; dual-channel-simple-stream
    (error "Implement %LISTEN")
    ;; string-simple-stream
    (error "Implement %LISTEN")))

(defun %clear-input (stream buffer-only)
  (declare (type simple-stream stream))
  (%check stream :input)
  (simple-stream-dispatch stream
    ;; single-channel-simple-stream
    (with-stream-class (single-channel-simple-stream stream)
      (setf (sm buffpos stream) 0
	    (sm buffer-ptr stream) 0
	    (sm last-char-read-size stream) 0))
    ;; dual-channel-simple-stream
    (with-stream-class (dual-channel-simple-stream stream)
      (setf (sm buffpos stream) 0
	    (sm buffer-ptr stream) 0
	    (sm last-char-read-size stream) 0))
    ;; string-simple-stream
    nil)
  (device-clear-input stream buffer-only))

(defun %read-byte (stream eof-error-p eof-value)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (any-stream-instance-flags stream :eof)
	(lisp::eof-or-lose stream eof-error-p eof-value)
	(simple-stream-dispatch stream
	  ;; single-channel-simple-stream
	  (sc-read-byte stream eof-error-p eof-value t)
	  ;; dual-channel-simple-stream
	  (dc-read-byte stream eof-error-p eof-value t)
	  ;; string-simple-stream
	  (with-stream-class (string-simple-stream stream)
	    (let ((encap (sm input-handle stream)))
	      (unless encap
		(error 'simple-type-error
		       :datum stream
		       :expected-type 'stream
		       :format-control "Can't read-byte on string streams"
		       :format-arguments '()))
	      (prog1
		  (locally (declare (notinline read-byte))
		    (read-byte encap eof-error-p eof-value))
		(setf (sm last-char-read-size stream) 0
		      (sm encapsulated-char-read-size stream) 0))))))))

(defun %write-char (stream character)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-char character (sm melded-stream stream))))

(defun %fresh-line (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (/= (or (sm charpos stream) 1) 0)
      (funcall-stm-handler-2 j-write-char #\Newline (sm melded-stream stream))
      t)))

(defun %write-string (stream string start end)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-chars string (sm melded-stream stream)
			   start end)))

(defun %line-length (stream)
  (declare (type simple-stream stream))
  (%check stream :output)
  ;; implement me
  )

(defun %finish-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (sc-flush-buffer stream t)
      ;; dual-channel-simple-stream
      (dc-flush-buffer stream t)
      ;; string-simple-stream
      nil)))

(defun %force-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (sc-flush-buffer stream nil)
      ;; dual-channel-simple-stream
      (dc-flush-buffer stream nil)
      ;; string-simple-stream
      nil)))

(defun %clear-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    ;; clear output buffer
    (device-clear-output stream)))

(defun %write-byte (stream integer)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (with-stream-class (single-channel-simple-stream stream)
	(let ((ptr (sm buffpos stream)))
	  (when (>= ptr (sm buffer-ptr stream))
	    (setf ptr (sc-flush-buffer stream t)))
	  (setf (sm buffpos stream) (1+ ptr))
	  (setf (bref (sm buffer stream) ptr) integer)))
      ;; dual-channel-simple-stream
      (with-stream-class (dual-channel-simple-stream stream)
	(let ((ptr (sm outpos stream)))
	  (when (>= ptr (sm max-out-pos stream))
	    (setf ptr (dc-flush-buffer stream t)))
	  (setf (sm outpos stream) (1+ ptr))
	  (setf (bref (sm out-buffer stream) ptr) integer)))
      ;; string-simple-stream
      (error 'simple-type-error
	     :datum stream
	     :expected-type 'stream
	     :format-control "Can't write-byte on string streams."
	     :format-arguments '()))))

(defun %read-sequence (stream seq start end partial-fill)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (etypecase seq
      (string
       (funcall-stm-handler j-read-chars (sm melded-stream stream) seq nil
			    start (or end (length seq))
			    (if partial-fill :bnb t)))
      ((or (simple-array (unsigned-byte 8) (*))
	   (simple-array (signed-byte 8) (*)))
       ;; "read-vector" equivalent, but blocking if partial-fill is NIL
       (error "implement me")
       ))))

(defun %write-sequence (stream seq start end)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (etypecase seq
      (string
       (funcall-stm-handler-2 j-write-chars seq (sm melded-stream stream)
			      start (or end (length seq))))
      ((or (simple-array (unsigned-byte 8) (*))
	   (simple-array (signed-byte 8) (*)))
       ;; "write-vector" equivalent
       (error "implement me")
       ))))

(defun read-no-hang-p (stream)
  (declare (type (or integer stream) stream))
  (etypecase stream
    (integer (sys:wait-until-fd-usable stream :input 0))
    (simple-stream (%check stream :input)
		   (cond ((any-stream-instance-flags stream :dual)
			  ;; if record-end is -1, call device-finish-record
			  ;; return NIL if it returns NIL, T if it returns :EOF
			  ;;
			  ;;
			  )
			 ((any-stream-instance-flags stream :string)
			  ;; ...
			  )
			 (t
			  ;; ...
			  )))
    (stream (listen stream))))

(defun write-no-hang-p (stream)
  (declare (type (or integer stream) stream))
  (etypecase stream
    (integer (sys:wait-until-fd-usable stream :output 0))
    (simple-stream #| ... |#)
    (stream #| ... |#)))
