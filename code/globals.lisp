;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/globals.lisp,v 1.5 1991/02/08 13:33:06 ram Exp $")
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
(in-package 'lisp)

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
		    debug::*in-the-debugger* mach::*free-trap-arg-blocks*
		    conditions::*handler-clusters*
		    conditions::*restart-clusters* alloctable-address
		    ext::*c-type-names* *gc-inhibit* *need-to-collect-garbage*
		    defmacro-error-string deftype-error-string
		    defsetf-error-string %sp-interrupts-inhibited
		    *software-interrupt-vector* *load-verbose*
		    *load-print-stuff* *in-compilation-unit*
		    *aborted-compilation-units* char-name-alist
		    *default-pathname-defaults* *beep-function*
		    *gc-notify-before* *gc-notify-after*

		    ;; hack to get these args to with-trap-arg-block to work in
		    ;; the bootstrapping env, since the var must be known to be
		    ;; special, in addition to being known to be an alien var.
		    mach::timeval mach::timezone mach::int1 mach::int2
		    mach::int3 mach::tchars mach::ltchars))


(proclaim '(ftype (function (&rest t) *)
		  c::%%defun c::%%defmacro c::%%defconstant c::%defstruct
		  c::%%compiler-defstruct c::%proclaim c::get-info-value
		  c::set-info-value find-keyword keyword-test assert-error
		  assert-prompt check-type-error case-body-error))
