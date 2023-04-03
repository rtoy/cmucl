;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/hpux-os.lisp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMU CL under Mach.  From Miles Bader and David
;;; Axmark.
;;;
(in-package "SYSTEM")
(use-package "EXTENSIONS")
(intl:textdomain "cmucl")

(export '(get-system-info get-page-size os-init))

(pushnew :hpux *features*)
(setq *software-type* "HPUX")

(defvar *software-version* nil "Version string for supporting software")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
          (let ((version-line
                 (with-output-to-string (stream)
                   (run-program "/usr/bin/uname"
                                '("-r" )
                                :output stream
                                :pty nil
                                :error nil))))
            (subseq version-line 0 (1- (length version-line))))))
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
      (error "Getpagesize failed: ~A" (unix:get-unix-error-msg err)))
    val))
