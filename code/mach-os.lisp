;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/mach-os.lisp,v 1.7 1992/02/29 02:29:53 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMU CL under Mach.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(pushnew :mach *features*)
(setq *software-type* "MACH/4.3BSD")

(defconstant foreign-segment-start #x00C00000)
(defconstant foreign-segment-size  #x00400000)
 
(defun software-version ()
  "Returns a string describing version of the supporting software."
  (string-trim '(#\newline)
	       (with-output-to-string (stream)
		 (run-program "/usr/cs/etc/version" ; Site dependent???
			      nil :output stream))))

;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defun os-init ()
  (setf *task-self* (mach:mach-task_self))
  (setf *task-data* (mach:mach-task_data))
  (setf *task-notify* (mach:mach-task_notify)))


;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.  For
;;; page-faults, we add pagein and pageout, since that is a somewhat more
;;; interesting number than the total faults.
;;;
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt majflt))
    (unless err?
      (error "Unix system call getrusage failed: ~A."
	     (unix:get-unix-error-msg utime)))
    
    (multiple-value-bind (gr ps fc ac ic wc zf ra in ot)
			 (mach:vm_statistics *task-self*)
      (declare (ignore ps fc ac ic wc zf ra))
      (mach:gr-error 'mach:vm_statistics gr)
      
      (values utime stime (+ in ot)))))


;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (mach:gr-call* mach:vm_statistics *task-self*))

