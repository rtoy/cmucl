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

(setq *software-type* "SunOS")

(defvar *software-version* nil "Version string for supporting software")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
	  (multiple-value-bind (sysname nodename release version)
	      (unix:unix-uname)
	    (declare (ignore sysname nodename))
	    (concatenate 'string release " " version))))
  *software-version*)


;;; OS-INIT -- interface.
;;;
;;; Other OS dependent initializations.
;;; 
(defun os-init ()
  ;; Decache version on save, because it might not be the same when we restart.
  (setf *software-version* nil))

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
