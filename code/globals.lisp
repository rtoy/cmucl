;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/globals.lisp,v 1.7 1993/08/25 01:13:31 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains special proclamations for variables that are
;;; referenced in the code sources before they are defined.  There is also a
;;; function proclamation to make some common functions be known, avoiding
;;; large amounts of work in recording the calls that are done before the
;;; definition.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "LISP")

(proclaim '(special *keyword-package* *lisp-package* *package* *query-io*
		    *terminal-io* *error-output* *trace-output* *debug-io*
		    *standard-input* *standard-output* *hemlock-version*
		    *evalhook* *applyhook* *task-self* *command-line-switches*
		    *command-switch-demons* ext::temporary-foreign-files
		    *display-event-handlers* original-lisp-environment
		    *environment-list* *read-default-float-format*
		    *read-suppress* *readtable* *print-base* *print-radix*
		    *print-length* *print-level* *print-pretty* *print-escape*
		    *print-case* *print-circle* *print-gensym* *print-array*
		    defmacro-error-string defsetf-error-string
		    std-lisp-readtable hi::*in-the-editor*
		    debug::*in-the-debugger*
		    conditions::*handler-clusters*
		    conditions::*restart-clusters* alloctable-address
		    *gc-inhibit* *need-to-collect-garbage*
		    defmacro-error-string deftype-error-string
		    defsetf-error-string %sp-interrupts-inhibited
		    *software-interrupt-vector* *load-verbose*
		    *load-print-stuff* *in-compilation-unit*
		    *aborted-compilation-units* char-name-alist
		    *default-pathname-defaults* *beep-function*
		    *gc-notify-before* *gc-notify-after*))


(proclaim '(ftype (function (&rest t) *)
		  c::%%defun c::%%defmacro c::%%defconstant c::%defstruct
		  c::%%compiler-defstruct c::%proclaim c::get-info-value
		  c::set-info-value find-keyword keyword-test assert-error
		  assert-prompt check-type-error case-body-error))
