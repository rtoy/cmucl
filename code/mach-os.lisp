;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/mach-os.lisp,v 1.2 1991/09/04 13:30:27 ram Exp $")
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
(export '(get-system-info get-page-size))

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

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.  For
;;; page-faults, we add pagein and pageout, since that is a somewhat more
;;; interesting number than the total faults.
;;;
(defun get-system-info ()
  (let (run-utime run-stime page-faults)
    (multiple-value-bind (err? utime stime maxrss ixrss idrss
                               isrss minflt majflt)
        (mach:unix-getrusage mach:rusage_self)
      (declare (ignore maxrss ixrss idrss isrss minflt))
      (unless err?
	(error "Unix system call getrusage failed: ~A."
	       (mach:get-unix-error-msg utime)))

      (multiple-value-bind (gr ps fc ac ic wc zf ra in ot)
			   (mach:vm_statistics *task-self*)
	(declare (ignore ps fc ac ic wc zf ra))
	(gr-error 'mach:vm_statistics gr)
	
	(values utime stime (+ in ot))))))


;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (gr-call* mach:vm_statistics *task-self*))
