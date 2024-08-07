;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/bsd-os.lisp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMU CL under BSD Unix.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
;;; Hacked into (Free)bsd-os.lisp by Paul Werkowski.
;;; Generalized a bit for OpenBSD by Pierre R. Mai.
;;; Support for NetBSD by Pierre R. Mai.
;;; Support for Darwin by Pierre R. Mai.

(in-package "SYSTEM")
(use-package "EXTENSIONS")

(intl:textdomain "cmucl-bsd-os")

(export '(get-system-info get-page-size os-init))

(register-lisp-feature :bsd)

(register-lisp-feature #+OpenBSD :OpenBSD
                       #+NetBSD :NetBSD
		       #+freebsd :freebsd
		       #+Darwin :Darwin
		       #-(or freebsd NetBSD OpenBSD Darwin) :bsd)

#+elf
(register-lisp-runtime-feature :elf)
#+mach-o
(register-lisp-runtime-feature :mach-o)

#+executable
(register-lisp-runtime-feature :executable)

(setq *software-type* #+OpenBSD "OpenBSD"
                      #+NetBSD "NetBSD"
                      #+freebsd "FreeBSD"
		      #+Darwin "Darwin"
		      #-(or freebsd NetBSD OpenBSD Darwin) "BSD")


;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defun os-init ()
  (setf *software-version* nil))
