;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sysmacs.lisp,v 1.17 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Miscellaneous system hacking macros.
;;;
(in-package "LISP")

(in-package "SYSTEM")
(export '(without-gcing without-hemlock))

(in-package "LISP")


;;; WITH-ARRAY-DATA  --  Interface
;;;
;;;    Checks to see if the array is simple and the start and end are in
;;; bounds.  If so, it proceeds with those values.  Otherwise, it calls
;;; %WITH-ARRAY-DATA.  Note that there is a derive-type method for
;;; %WITH-ARRAY-DATA.
;;;
(defmacro with-array-data (((data-var array &key (offset-var (gensym)))
			    (start-var &optional (svalue 0))
			    (end-var &optional (evalue nil)))
			   &rest forms)
  "Given any Array, binds Data-Var to the array's data vector and Start-Var and
  End-Var to the start and end of the designated portion of the data vector.
  Svalue and Evalue are any start and end specified to the original operation,
  and are factored into the bindings of Start-Var and End-Var.  Offset-Var is
  the cumulative offset of all displacements encountered, and does not
  include Svalue."
  (once-only ((n-array array)
	      (n-svalue `(the index ,svalue))
	      (n-evalue `(the (or index null) ,evalue)))
    `(multiple-value-bind
	 (,data-var ,start-var ,end-var ,offset-var)
	 (if (not (array-header-p ,n-array))
	     (let ((,n-array ,n-array))
	       (declare (type (simple-array * (*)) ,n-array))
	       ,(once-only ((n-len `(length ,n-array))
			    (n-end `(or ,n-evalue ,n-len)))
		  `(if (<= ,n-svalue ,n-end ,n-len)
		       (values ,n-array ,n-svalue ,n-end 0)
		       (%with-array-data ,n-array ,n-svalue ,n-evalue))))
	     (%with-array-data ,n-array ,n-svalue ,n-evalue))
       (declare (ignorable ,offset-var))
       ,@forms)))

#-gengc
(defmacro without-gcing (&rest body)
  "Executes the forms in the body without doing a garbage collection."
  `(unwind-protect
       (let ((*gc-inhibit* t))
	 ,@body)
     (when (and *need-to-collect-garbage* (not *gc-inhibit*))
       (maybe-gc nil))))

#+gengc
(defmacro without-gcing (&rest body)
  "Executes the forms in the body without doing a garbage collection."
  `(without-interrupts ,@body))

(defmacro without-hemlock (&body body)
  `(progn
     (when (and hi::*in-the-editor* (null debug::*in-the-debugger*))
       (let ((device (hi::device-hunk-device
		      (hi::window-hunk (hi::current-window)))))
	 (funcall (hi::device-exit device) device)))
     ,@body
     (when (and hi::*in-the-editor* (null debug::*in-the-debugger*))
       (let ((device (hi::device-hunk-device
		      (hi::window-hunk (hi::current-window)))))
	 (funcall (hi::device-init device) device)))))



;;; Eof-Or-Lose is a useful macro that handles EOF.

(defmacro eof-or-lose (stream eof-errorp eof-value)
  `(if ,eof-errorp
       (error 'end-of-file :stream ,stream)
       ,eof-value))

;;; These macros handle the special cases of t and nil for input and
;;; output streams.
;;;
(defmacro in-synonym-of (stream)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-input*)
	     ((eq ,svar t) *terminal-io*)
	     (t (check-type ,svar stream)
		,svar)))))

(defmacro out-synonym-of (stream)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-output*)
	     ((eq ,svar t) *terminal-io*)
	     (T (check-type ,svar stream)
		,svar)))))

;;; With-Mumble-Stream calls the function in the given Slot of the Stream with
;;; the Args.
;;;
(defmacro with-in-stream (stream slot &rest args)
  `(let ((stream (in-synonym-of ,stream)))
     (funcall (,slot stream) stream ,@args)))

(defmacro with-out-stream (stream slot &rest args)
  `(let ((stream (out-synonym-of ,stream)))
     (funcall (,slot stream) stream ,@args)))


;;;; These are hacks to make the reader win.

;;; Prepare-For-Fast-Read-Char  --  Internal
;;;
;;;    This macro sets up some local vars for use by the Fast-Read-Char
;;; macro within the enclosed lexical scope.
;;;
(defmacro prepare-for-fast-read-char (stream &body forms)
  `(let* ((%frc-stream% (in-synonym-of ,stream))
	  (%frc-method% (stream-in %frc-stream%))
	  (%frc-buffer% (stream-in-buffer %frc-stream%))
	  (%frc-index% (stream-in-index %frc-stream%)))
     (declare (type index %frc-index%))
     ,@forms))

;;; Done-With-Fast-Read-Char  --  Internal
;;;
;;;    This macro must be called after one is done with fast-read-char
;;; inside it's scope to decache the stream-in-index.
;;;
(defmacro done-with-fast-read-char ()
  `(setf (stream-in-index %frc-stream%) %frc-index%))

;;; Fast-Read-Char  --  Internal
;;;
;;;    This macro can be used instead of Read-Char within the scope of
;;; a Prepare-For-Fast-Read-Char.
;;;
(defmacro fast-read-char (&optional (eof-errorp t) (eof-value ()))
  `(cond
    ((not %frc-buffer%)
     (funcall %frc-method% %frc-stream% ,eof-errorp ,eof-value))
    ((= %frc-index% in-buffer-length)
     (prog1 (fast-read-char-refill %frc-stream% ,eof-errorp ,eof-value)
	    (setq %frc-index% (stream-in-index %frc-stream%))))
    (t
     (prog1 (code-char (aref %frc-buffer% %frc-index%))
	    (incf %frc-index%)))))

;;;; And these for the fasloader...

;;; Prepare-For-Fast-Read-Byte  --  Internal
;;;
;;;    Just like Prepare-For-Fast-Read-Char except that we get the Bin
;;; method.
;;;
(defmacro prepare-for-fast-read-byte (stream &body forms)
  `(let* ((%frc-stream% (in-synonym-of ,stream))
	  (%frc-method% (stream-bin %frc-stream%))
	  (%frc-buffer% (stream-in-buffer %frc-stream%))
	  (%frc-index% (stream-in-index %frc-stream%)))
     (declare (type index %frc-index%))
     ,@forms))

;;; Fast-Read-Byte, Done-With-Fast-Read-Byte  --  Internal
;;;
;;;    Similar to fast-read-char, but we use a different refill routine & don't
;;; convert to characters.  If ANY-TYPE is true, then this can be used on any
;;; integer streams, and we don't assert the result type.
;;;
(defmacro fast-read-byte (&optional (eof-errorp t) (eof-value ()) any-type)
  `(truly-the
    ,(if (and (eq eof-errorp 't) (not any-type)) '(unsigned-byte 8) 't)
    (cond
     ((not %frc-buffer%)
      (funcall %frc-method% %frc-stream% ,eof-errorp ,eof-value))
     ((= %frc-index% in-buffer-length)
      (prog1 (fast-read-byte-refill %frc-stream% ,eof-errorp ,eof-value)
	(setq %frc-index% (stream-in-index %frc-stream%))))
     (t
      (prog1 (aref %frc-buffer% %frc-index%)
	(incf %frc-index%))))))
;;;
(defmacro done-with-fast-read-byte ()
  `(done-with-fast-read-char))
