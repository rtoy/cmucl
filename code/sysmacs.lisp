;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sysmacs.lisp,v 1.9 1991/04/24 23:39:34 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Miscellaneous system hacking macros.
;;;
(in-package "LISP" :use '("SYSTEM" "DEBUG"))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* t))

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
	 (if (typep ,n-array '(simple-array * (*)))
	     ,(once-only ((n-len `(length ,n-array))
			  (n-end `(or ,n-evalue ,n-len)))
		`(if (<= ,n-svalue ,n-end ,n-len)
		     (values ,n-array ,n-svalue ,n-end 0)
		     (%with-array-data ,n-array ,n-svalue ,n-evalue)))
	     (%with-array-data ,n-array ,n-svalue ,n-evalue))
       (declare (ignorable ,offset-var))
       ,@forms)))


(defmacro without-gcing (&rest body)
  "Executes the forms in the body without doing a garbage collection."
  `(unwind-protect
       (let ((*gc-inhibit* t))
	 ,@body)
     (when (and *need-to-collect-garbage* (not *gc-inhibit*))
       (maybe-gc nil))))


(defmacro with-enabled-interrupts (interrupt-list &body body)
  "With-enabled-interrupts ({(interrupt function [character])}*) {form}*
  Establish function as a handler for the Unix signal interrupt which
  should be a number between 1 and 31 inclusive.  For the signals that
  can be generated from the keyboard, the optional character specifies
  the character to use to generate the signal."
  (let ((il (gensym))
	(fn (gensym))
	(ch (gensym))
	(it (gensym)))
    `(let ((,il NIL))
       (unwind-protect
	   (progn
	     ,@(do* ((item interrupt-list (cdr item))
		     (intr (caar item) (caar item))
		     (ifcn (cadar item) (cadar item))
		     (ichr (caddar item) (caddar item))
		     (forms NIL))
		    ((null item) (nreverse forms))
		 (if (symbolp intr)
		     (setq intr (symbol-value intr)))
		 (push `(multiple-value-bind (,fn ,ch)
					     (enable-interrupt ,intr ,ifcn
							       ,ichr)
			  (push `(,,intr ,,fn ,,ch) ,il)) forms))
	     ,@body)
	 (dolist (,it (nreverse ,il))
	   (funcall #'enable-interrupt (car ,it) (cadr ,it) (caddr ,it)))))))


(defvar hi::*in-the-editor* nil)

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


;;; With-Reply-Port  --  Public    
;;;
;;;    If we find that the number of ports in use (as indicated by
;;; *reply-port-pointer*) disagrees with our dynamic depth in
;;; With-Reply-Port forms (as indicated by *reply-port-depth*),
;;; then we must have been unwound at some point in the past.
;;; We reallocate the ports that were in use when we were
;;; unwound, since they may have random messages hanging on them.
;;;
#+nil
(defmacro with-reply-port ((var) &body body)
  "With-Reply-Port (Var) {Form}*
  Binds Var to a port during the evaluation of the Forms."
  (let ((index (gensym))
	(old-flag (gensym))
	(res (gensym)))
    `(let ((,old-flag %sp-interrupts-inhibited)
	   ,res)
       (without-interrupts
	(let* ((,index *reply-port-depth*)
	       (*reply-port-depth* (1+ ,index))
	       ,var)
	  (unless (eql ,index *reply-port-pointer*)
	    (reallocate-reply-ports ,index))
	  (setq ,var (svref *reply-port-stack* ,index))
	  (setq *reply-port-pointer* (1+ ,index))
	  (unless ,var (setq ,var (allocate-new-reply-ports)))
	  (setq %sp-interrupts-inhibited ,old-flag)
	  (setq ,res (multiple-value-list (progn ,@body)))
	  (when (eql (car ,res) mach:rcv-timed-out)
	    (gr-call mach:port_deallocate *task-self* ,var)
	    (setf (svref *reply-port-stack* ,index)
		  (gr-call* mach:port_allocate *task-self*)))
	  (setq %sp-interrupts-inhibited (or ,old-flag T))
	  (if (eql ,index (1- *reply-port-pointer*))
	      (setq *reply-port-pointer* ,index)
	      (reallocate-reply-ports (1+ ,index)))
	  (values-list ,res))))))


;;; Eof-Or-Lose is a useful macro that handles EOF.

(defmacro eof-or-lose (stream eof-errorp eof-value)
  `(if ,eof-errorp
       (error "~S: Stream hit EOF unexpectedly." ,stream)
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
     (declare (type (or simple-string null) %frc-buffer%) (fixnum %frc-index%))
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
    ((= %frc-index% in-buffer-length)
     (setf (stream-in-index %frc-stream%) %frc-index%)
     (prog1 (funcall %frc-method% %frc-stream% ,eof-errorp ,eof-value)
	    (setq %frc-index% (stream-in-index %frc-stream%))))
    (t
     (prog1 (aref %frc-buffer% %frc-index%)
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
     (declare (type (or simple-array null) %frc-buffer%) (fixnum %frc-index%))
     ,@forms))

;;; Fast-Read-Byte, Done-With-Fast-Read-Byte  --  Internal
;;;
;;;    Identical to the text versions, but we get some gratuitous
;;; psuedo-generality by having different names.
;;;
(defmacro done-with-fast-read-byte ()
  `(done-with-fast-read-char))
;;;
(defmacro fast-read-byte (&rest stuff)
  `(fast-read-char ,@stuff))


#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* nil))
