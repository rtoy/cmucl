;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/signal.lisp,v 1.3 1990/07/02 04:37:34 wlott Exp $
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


(defmacro define-signal-handler (name what &optional (function 'error))
  `(defun ,name (signal code scp)
     (declare (ignore signal code))
     (alien-bind ((sc
		   (make-alien 'mach:sigcontext
			       #.(c-sizeof 'mach:sigcontext)
			       scp)
		   mach:sigcontext
		   t))
       (,function ,(concatenate 'simple-string what " at #x~x.")
		  (alien-access (mach:sigcontext-pc (alien-value sc)))))))

(define-signal-handler sigint-handler "Interrupted" break)
(define-signal-handler sigill-handler "Illegal Instruction")
(define-signal-handler sigiot-handler "SIGIOT")
(define-signal-handler sigemt-handler "SIGEMT")
(define-signal-handler sigfpe-handler "SIGFPE")
(define-signal-handler sigbus-handler "Bus Error")
(define-signal-handler sigsegv-handler "Segmentation Violation")
(define-signal-handler sigsys-handler "Bad Argument to a System Call")
(define-signal-handler sigpipe-handler "SIGPIPE")
(define-signal-handler sigalrm-handler "SIGALRM")

(defun sigquit-handler (signal code scp)
  (declare (ignore signal code scp))
  (throw 'lisp::top-level-catcher nil))


(defun signal-init ()
  (macrolet ((frob (signal handler)
	       `(install-handler ,(mach:unix-signal-number signal)
				 (di::get-lisp-obj-address ,handler))))
    #+nil (frob :sigint #'sigint-handler)
    (frob :sigquit #'sigquit-handler)
    (frob :sigill #'sigill-handler)
    (frob :sigtrap #'internal-error)
    (frob :sigiot #'sigiot-handler)
    (frob :sigemt #'sigemt-handler)
    #+nil (frob :sigfpe #'sigfpe-handler)
    (frob :sigbus #'sigbus-handler)
    (frob :sigsegv #'sigsegv-handler)
    (frob :sigsys #'sigsys-handler)
    (frob :sigpipe #'sigpipe-handler)
    (frob :sigalrm #'sigalrm-handler))
  nil)
