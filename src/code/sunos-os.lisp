;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/sunos-os.lisp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMUCL under SunOS.  From Miles Bader and David
;;; Axmark.
;;;

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(intl:textdomain "cmucl-sunos-os")

(export '(get-system-info get-page-size os-init))

(pushnew :sunos *features*)

#+solaris
(progn
  (register-lisp-feature :solaris)
  (register-lisp-feature :elf))
#+svr4
(register-lisp-feature :svr4)

#+executable
(register-lisp-runtime-feature :executable)

;;; OS-INIT -- interface.
;;;
;;; Other OS dependent initializations.
;;; 
(defun os-init ()
  ;; Decache version on save, because it might not be the same when we restart.
  (setf *software-version* nil))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime maxrss ixrss idrss isrss minflt majflt)
      (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (cond ((null err?)
	   (error (intl:gettext "Unix system call getrusage failed: ~A.")
		  (unix:get-unix-error-msg utime)))
	  (T
	   (values utime stime majflt)))))

;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (multiple-value-bind (val err)
		       (unix:unix-getpagesize)
    (unless val
      (error (intl:gettext "Getpagesize failed: ~A") (unix:get-unix-error-msg err)))
    val))
