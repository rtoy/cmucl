;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/misc.lisp,v 1.5 1990/10/23 14:44:57 wlott Exp $
;;;
;;; Assorted miscellaneous functions for Spice Lisp.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
(in-package "LISP")
(export '(documentation *features* common variable room
	  lisp-implementation-type lisp-implementation-version machine-type
	  machine-version machine-instance software-type software-version
	  short-site-name long-site-name dribble))


(defun documentation (name doc-type)
  "Returns the documentation string of Doc-Type for Name, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  and SETF."
  (case doc-type
    (variable (info variable documentation name))
    (function (info function documentation name))
    (structure
     (when (eq (info type kind name) :structure)
       (info type documentation name)))
    (type
     (info type documentation name))
    (setf (info setf documentation name))
    (t
     (cdr (assoc doc-type (info random-documentation stuff name))))))

(defun %set-documentation (name doc-type string)
  (case doc-type
    (variable (setf (info variable documentation name) string))
    (function (setf (info function documentation name) string))
    (structure
     (unless (eq (info type kind name) :structure)
       (error "~S is not the name of a structure type." name))
     (setf (info type documentation name) string))
    (type (setf (info type documentation name) string))
    (setf (setf (info setf documentation name) string))
    (t
     (let ((pair (assoc doc-type (info random-documentation stuff name))))
       (if pair
	   (setf (cdr pair) string)
	   (push (cons doc-type string)
		 (info random-documentation stuff name))))))
  string)

(defvar *features* '(:common :cmu :mach :new-compiler)
  "Holds a list of symbols that describe features provided by the
   implementation.")

(defun featurep (x)
  "If X is an atom, see if it is present in *FEATURES*.  Also
  handle arbitrary combinations of atoms using NOT, AND, OR."
  (cond ((atom x) (memq x *features*))
	((eq (car x) ':not) (not (featurep (cadr x))))
	((eq (car x) ':and)
	 (every #'featurep (cdr x)))
	((eq (car x) ':or)
	 (some #'featurep (cdr x)))
	(t nil)))



;;; Other Environment Inquiries.

(defun lisp-implementation-type ()
  "Returns a string describing the implementation type."
  "CMU Common Lisp")

(defun lisp-implementation-version ()
  "Returns a string describing the implementation version."
  *lisp-implementation-version*)

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "DECstation 3100")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "DECstation 3100")

(defun machine-instance ()
  "Returns a string giving the name of the local machine."
  (mach::unix-gethostname))

(defun software-type ()
  "Returns a string describing the supporting software."
  "MACH/4.3BSD")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (string-trim
   '(#\newline)
   (with-output-to-string (stream)
     (run-program "/usr/cs/etc/version" nil :output stream))))

(defun short-site-name ()
  "Returns a string with the abbreviated site name."
  "CMU-SCS")

(defun long-site-name ()
  "Returns a string with the long form of the site name."
  "Carnegie-Mellon University School of Computer Science")



;;;; Dribble stuff:

(defun dribble (&optional pathname &key (if-exists :append))
  "With a file name as an argument, dribble opens the file and
   sends a record of the output to that file.  Without an
   argument, it closes the open dribble file."
  (if pathname
      (with-open-file (f pathname :direction :output  :if-exists if-exists
			 :if-does-not-exist :create)
	(catch 'dribble-punt
	  (let ((*terminal-io*
		 (make-two-way-stream
		  (make-echo-stream *terminal-io* f)
		  (make-broadcast-stream *terminal-io* f))))
	    (%top-level))))
      (throw 'dribble-punt nil)))
