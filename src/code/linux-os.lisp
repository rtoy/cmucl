;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/linux-os.lisp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMUCL under Linux.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
;;; Derived from mach-os.lisp by Paul Werkowski

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(intl:textdomain "cmucl-linux-os")
 
(export '(get-system-info get-page-size os-init))

(register-lisp-feature :linux)
(register-lisp-feature :elf)
(register-lisp-runtime-feature :executable)

(setq *software-type* "Linux")

(defvar *software-version* nil
  "Version string for supporting software")

;;; Instead of reading /proc/version (which has some bugs with
;;; select() in Linux kernel 2.6.x) and instead of running uname -r,
;;; let's just get the info from uname().
(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
	  (multiple-value-bind (sysname nodename release version)
	      (unix:unix-uname)
	    (declare (ignore sysname nodename))
	    (concatenate 'string release " " version))))
  *software-version*)

#+nil
(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
	  (let (version)
	    (unwind-protect
		 (progn
		   (setf version
			 (alien:alien-funcall
			  (alien:extern-alien "os_software_version"
					      (function (alien:* c-call:c-string)))))
		   (unless (zerop (sap-int (alien:alien-sap version)))
		     (alien:cast version c-call:c-string)))
	      (when version
		(alien:free-alien version)))))
    *software-version*))


;;; OS-Init initializes our operating-system interface.
;;;
(defun os-init () nil)


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
      (error (intl:gettext "Unix system call getrusage failed: ~A.")
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
      (error (intl:gettext "Getpagesize failed: ~A") (unix:get-unix-error-msg err)))
    val))

