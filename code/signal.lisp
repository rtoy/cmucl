;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/signal.lisp,v 1.1 1990/06/04 05:34:33 wlott Exp $
;;;
;;; Code for handling UNIX signals.
;;; 
;;; Written by William Lott.
;;;

(in-package "KERNEL")

(export '(signal-init))


(def-c-routine ("install_handler" install-handler)
	       (int)
  (signal int)
  (handler unsigned-long))

(defun signal-init ()
  (install-handler #.(mach:unix-signal-number :sigtrap)
		   (di::get-lisp-obj-address #'internal-error))
  nil)
