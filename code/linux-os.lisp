;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/linux-os.lisp,v 1.2 2002/10/07 14:31:04 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMU CL under Mach.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
;;; Hacked into Linux-os.lisp /Werkowski

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(pushnew :linux *features*)

(setq *software-type* "Linux")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  #+nil
  (string-trim '(#\newline)
	       (with-output-to-string (stream)
		 (run-program "/usr/cs/etc/version" ; Site dependent???
			      nil :output stream)))
  "n/a")


;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defvar *task-self*)

(defun os-init ()			; don't know what to do here
  #+sparc ;; Can't use #x20000000 thru #xDFFFFFFF, but mach tries to let us.
  (system:allocate-system-memory-at (system:int-sap #x20000000) #xc0000000))


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
  ;; probably should call getpagesize()
  4096)

