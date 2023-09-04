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

;;; OS-Init initializes our operating-system interface.
;;;
(defun os-init ()
  (setf *software-version* nil))
