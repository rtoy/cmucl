;;; -*- Mode: Lisp; Package: ED; Log: hemlock.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
#|
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/hemlock/debug.lisp,v 1.1 1991/05/27 12:35:29 chiles Exp $")
|#
;;;
;;; **********************************************************************
;;;
;;; This contains commands for sending debugger commands to slaves in the
;;; debugger.
;;;
;;; Written by Bill Chiles.
;;;

(in-package "HEMLOCK")



;;;; Commands for slave buffers.

(defmacro define-debugger-command (name doc cmd &key uses-argument)
  `(defcommand ,(concatenate 'simple-string "Debug " name) (p)
     ,doc ,doc
     ,@(if uses-argument
	   nil
	   '((declare (ignore p))))
     (let* ((server-info (get-current-eval-server t))
	    (wire (server-info-wire server-info)))
       (wire:remote wire
	 (ts-stream-accept-input
	  (ts-data-stream (server-info-slave-info server-info))
	  ,(if uses-argument
	       `(list ,cmd p)
	       cmd)))
       (wire:wire-force-output wire))))

;;;
;;; Frame changing commands.
;;;

(define-debugger-command "Up"
  "Moves the \"Current Eval Server\" up one debugger frame."
  :up)

(define-debugger-command "Down"
  "Moves the \"Current Eval Server\" down one debugger frame."
  :down)

(define-debugger-command "Top"
  "Moves the \"Current Eval Server\" to the top of the debugging stack."
  :top)

(define-debugger-command "Bottom"
  "Moves the \"Current Eval Server\" to the bottom of the debugging stack."
  :bottom)

(define-debugger-command "Frame"
  "Moves the \"Current Eval Server\" to the absolute debugger frame number
   indicated by the prefix argument."
  :frame
  :uses-argument t)

;;;
;;; In and Out commands.
;;;

(define-debugger-command "Quit"
  "In the \"Current Eval Server\", throws to top level out of the debugger."
  :quit)

(define-debugger-command "Go"
  "In the \"Current Eval Server\", tries the CONTINUE restart."
  :go)

(define-debugger-command "Abort"
  "In the \"Current Eval Server\", execute the previous ABORT restart."
  :abort)

(define-debugger-command "Restart"
  "In the \"Current Eval Server\", executes the restart indicated by the
   prefix argument."
  :restart
  :uses-argument t)

;;;
;;; Information commands.
;;;

(define-debugger-command "Help"
  "In the \"Current Eval Server\", prints the debugger's help text."
  :help)

(define-debugger-command "Error"
  "In the \"Current Eval Server\", print the error condition and restart cases
   upon entering the debugger."
  :error)

(define-debugger-command "Backtrace"
  "Executes the previous abort restart."
  :backtrace)

(define-debugger-command "Print"
  "In the \"Current Eval Server\", prints a representation of the debugger's
   current frame."
  :print)

(define-debugger-command "Verbose Print"
  "In the \"Current Eval Server\", prints a representation of the debugger's
   current frame without elipsis."
  :vprint)

(define-debugger-command "List Locals"
  "In the \"Current Eval Server\", prints the local variables for the debugger's
   current frame."
  :list-locals)

;;; This should probably take you to the source in the editor.
;;; Maybe split the windows?
;;;
(define-debugger-command "Source"
  "In the \"Current Eval Server\", prints the source form for the debugger's
   current frame."
  :source)

(define-debugger-command "Verbose Source"
  "In the \"Current Eval Server\", prints the source form for the debugger's
   current frame with surrounding forms for context."
  :vsource)

;;;
;;; Miscellaneous commands.
;;;

(define-debugger-command "Flush Errors"
  "In the \"Current Eval Server\", toggles whether the debugger ignores errors
   or recursively enters itself."
  :flush)
