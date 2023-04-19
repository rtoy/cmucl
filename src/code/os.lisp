;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; and has been placed in the public domain.
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
(intl:textdomain "cmucl-linux-os")

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


