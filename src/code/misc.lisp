;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/misc.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Environment query functions, documentation and dribble.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
(in-package "LISP")
(intl:textdomain "cmucl")

(export '(*features* variable room
	  lisp-implementation-type lisp-implementation-version machine-type
	  machine-version machine-instance software-type software-version
	  short-site-name long-site-name dribble compiler-macro))

(in-package "SYSTEM")
(export '(*software-type* *short-site-name* *long-site-name*))

(in-package "EXT")
(export 'featurep)

(in-package "LISP")

;;; Register various Lisp features
#+sparc-v7
(sys:register-lisp-runtime-feature :sparc-v7)

#+sparc-v8
(sys:register-lisp-runtime-feature :sparc-v8)

#+sparc-v9
(sys:register-lisp-runtime-feature :sparc-v9)

#+complex-fp-vops
(sys:register-lisp-feature :complex-fp-vops)

#+unicode
(sys:register-lisp-runtime-feature :unicode)

#+cmu
(sys:register-lisp-feature :cmu)

#+(or cmu cmucl)
(sys:register-lisp-feature :cmucl)

(defun featurep (x)
  "If X is an atom, see if it is present in *FEATURES*.  Also
  handle arbitrary combinations of atoms using NOT, AND, OR."
  (if (consp x)
      (case (car x)
	((:not not) (not (featurep (cadr x))))
	((:and and) (every #'featurep (cdr x)))
	((:or or) (some #'featurep (cdr x)))
	(t
	 (error (intl:gettext "Unknown operator in feature expression: ~S.") x)))
      (not (null (memq x *features*)))))


;;; Other Environment Inquiries.

(defun lisp-implementation-type ()
  "Returns a string describing the implementation type."
  "CMU Common Lisp")

(defun lisp-implementation-version ()
  "Returns a string describing the implementation version."
  (format nil "~A (~X~A)" *lisp-implementation-version* c:byte-fasl-file-version
	  #+unicode _" Unicode" #-unicode ""))

(defun machine-instance ()
  "Returns a string giving the name of the local machine."
  (unix:unix-gethostname))

(defvar *software-type* "Unix"
  _N"The value of SOFTWARE-TYPE.  Set in FOO-os.lisp.")

(defun software-type ()
  "Returns a string describing the supporting software."
  *software-type*)

(defvar *software-version* nil
  _N"Version string for supporting software")

(defun software-version ()
  _N"Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
	  (let (version)
	    (unwind-protect
		 (progn
		   (setf version
			 (alien:alien-funcall
			  (alien:extern-alien "os_software_version"
					      (function (alien:* c-call:c-string)))))
		   (alien:cast version c-call:c-string)))))
    *software-version*))

(defvar *short-site-name* nil
  "The value of SHORT-SITE-NAME.  Set in library:site-init.lisp.")

(defun short-site-name ()
  "Returns a string with the abbreviated site name."
  *short-site-name*)

(defvar *long-site-name* nil
  "The value of LONG-SITE-NAME.  Set in library:site-init.lisp.")

(defun long-site-name ()
  "Returns a string with the long form of the site name."
  *long-site-name*)


;;;; Dribble stuff:

;;; Each time we start dribbling to a new stream, we put it in
;;; *dribble-stream*, and push a list of *dribble-stream*, *standard-input*,
;;; *standard-output* and *error-output* in *previous-streams*.
;;; *standard-output* and *error-output* is changed to a broadcast stream that
;;; broadcasts to *dribble-stream* and to the old values of the variables.
;;; *standard-input* is changed to an echo stream that echos input from the old
;;; value of standard input to *dribble-stream*.
;;;
;;; When dribble is called with no arguments, *dribble-stream* is closed,
;;; and the values of *dribble-stream*, *standard-input*, and
;;; *standard-output* are poped from *previous-streams*.

(defvar *previous-streams* nil)
(defvar *dribble-stream* nil)

(defun dribble (&optional pathname &key (if-exists :append))
  "With a file name as an argument, dribble opens the file and
   sends a record of further I/O to that file.  Without an
   argument, it closes the dribble file, and quits logging."
  (cond (pathname
	 (let* ((new-dribble-stream
		 (open pathname :direction :output :if-exists if-exists
		       :if-does-not-exist :create))
		(new-standard-output
		 (make-broadcast-stream *standard-output* new-dribble-stream))
		(new-error-output
		 (make-broadcast-stream *error-output* new-dribble-stream))
		(new-standard-input
		 (make-echo-stream *standard-input* new-dribble-stream)))
	   (push (list *dribble-stream* *standard-input* *standard-output*
		       *error-output*)
		 *previous-streams*)
	   (setf *dribble-stream* new-dribble-stream)
	   (setf *standard-input* new-standard-input)
	   (setf *standard-output* new-standard-output)
	   (setf *error-output* new-error-output)))
	((null *dribble-stream*)
	 (error (intl:gettext "Not currently dribbling.")))
	(t
	 (let ((old-streams (pop *previous-streams*)))
	   (close *dribble-stream*)
	   (setf *dribble-stream* (first old-streams))
	   (setf *standard-input* (second old-streams))
	   (setf *standard-output* (third old-streams))
	   (setf *error-output* (fourth old-streams)))))
  (values))

(defun ed (&optional x)
  "Default implementation of ed.  This does nothing.  If hemlock is
  loaded, ed can be used to edit a file"
  (declare (ignorable x))
  (values))

(defun disassemble (object)
  "Disassemble the machine code associated with OBJECT, which can be a
  function, a lambda expression, or a symbol with a function definition.  If
  it is not already compiled, the compiler is called to produce something to
  disassemble."
  (disassem:disassemble object))
