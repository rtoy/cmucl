;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project and
;;; has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/os.lisp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMUCL.
;;;
;;; The code here is for OS functions that don't depend on the OS.

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(intl:textdomain "cmucl-os")

(export '(get-page-size))

;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  _N"Return the system page size"
  (let ((maybe-page-size (alien:alien-funcall
			  (alien:extern-alien "os_get_page_size"
					      (function c-call:long)))))
    (when (minusp maybe-page-size)
      (error (intl:gettext "get-page-size failed: ~A") (get-unix-error-msg err)))
    maybe-page-size))


;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time (in usec) and number of page
;;;    faults.
;;;
(defun get-system-info ()
  _N"Get system information consisting of the user time (in usec), the
  system time (in usec) and the number of major page faults."
  (alien:with-alien ((utime unix:int64-t 0)
		     (stime unix:int64-t 0)
		     (major-fault c-call:long 0))
    (let ((rc (alien:alien-funcall
	       (alien:extern-alien "os_get_system_info"
				   (function c-call:int
					     (* unix:int64-t)
					     (* unix:int64-t)
					     (* c-call:long)))
	       (alien:addr utime)
	       (alien:addr stime)
	       (alien:addr major-fault))))
      (when (minusp rc)
	(error (intl:gettext "Unix system call getrusage failed: ~A.")
	       (unix:get-unix-error-msg utime)))
      (values utime stime major-fault))))
