;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/bsd-os.lisp,v 1.7 2004/07/25 19:32:37 pmai Exp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMU CL under BSD Unix.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
;;; Hacked into (Free)bsd-os.lisp by Paul Werkowski.
;;; Generalized a bit for OpenBSD by Pierre R. Mai.
;;; Support for NetBSD by Pierre R. Mai.
;;; Support for Darwin by Pierre R. Mai.

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(register-lisp-feature :bsd)

(register-lisp-feature #+OpenBSD :OpenBSD
                       #+NetBSD :NetBSD
		       #+FreeBSD :FreeBSD
		       #+Darwin :Darwin
		       #-(or FreeBSD NetBSD OpenBSD Darwin) :bsd)

(setq *software-type* #+OpenBSD "OpenBSD"
                      #+NetBSD "NetBSD"
                      #+FreeBSD "FreeBSD"
		      #+Darwin "Darwin"
		      #-(or FreeBSD NetBSD OpenBSD Darwin) "BSD")

(defvar *software-version* nil "Version string for supporting software")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
	  (string-trim '(#\newline)
		       (with-output-to-string (stream)
			 (run-program "/usr/bin/uname"
				      '("-r")
				      :output stream)))))
  *software-version*)


;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defun os-init ()
  (setf *software-version* nil))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err?
      (error "Unix system call getrusage failed: ~A."
	     (unix:get-unix-error-msg utime)))
    
    (values utime stime majflt)))


;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (multiple-value-bind (val err)
      (unix:unix-getpagesize)
    (unless val
      (error "Getpagesize failed: ~A" (unix:get-unix-error-msg err)))
    val))

