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
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/hemlock/debug.lisp,v 1.2 1991/05/29 11:40:48 chiles Exp $")
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

;;; Okay, the :edit-source command in the debugger initiates a synchronous RPC
;;; into the editor via the wire in *termina-io*, a typescript stream.  This
;;; routine takes the necessary values, a file and source-path, and changes
;;; the editor's state to edit that.
;;;
;;; This command has to wait on SERVE-EVENT until some special is set by the
;;; RPC routine saying it is okay to return to the editor's top level.
;;;
(defcommand "Debug Edit Source" (p)
  "Give the \"Current Eval Server\"'s current debugger frame, place the user
   at the location's source in the editor."
  "Give the \"Current Eval Server\"'s current debugger frame, place the user
   at the location's source in the editor."
  (declare (ignore p))
  (let* ((server-info (get-current-eval-server t))
	 (wire (server-info-wire server-info)))
    (wire:remote wire
      (ts-stream-accept-input
       (ts-data-stream (server-info-slave-info server-info))
       :edit-source))
    (wire:wire-force-output wire))))


;;; Okay, the :edit-source command in the slave debugger initiates a
;;; synchronous RPC into the editor via the wire in *termina-io*, a typescript
;;; stream.  This routine takes the necessary values, a file and source-path,
;;; and changes the editor's state to edit that.
;;;
;;; This command has to wait on SERVE-EVENT until some special is set by the
;;; RPC routine saying it is okay to return to the editor's top level.
;;;
(defvar *debug-editor-source-data* nil)

(defcommand "Debug Edit Source" (p)
  "Give the \"Current Eval Server\"'s current debugger frame, place the user
   at the location's source in the editor."
  "Give the \"Current Eval Server\"'s current debugger frame, place the user
   at the location's source in the editor."
  (declare (ignore p))
  (let* ((server-info (get-current-eval-server t))
	 (wire (server-info-wire server-info)))
    ;;
    ;; Tell the slave to tell the editor some source info.
    (wire:remote wire
      (ts-stream-accept-input
       (ts-data-stream (server-info-slave-info server-info))
       :edit-source))
    (wire:wire-force-output wire)
    ;;
    ;; Wait for the source info.
    (let ((*debug-editor-source-data* nil))
      (loop
	(system:serve-event)
	(when *debug-editor-source-data* (return))))))

;;; EDIT-SOURCE-LOCATION -- Internal Interface.
;;;
;;; The slave calls this in the editor when the debugger gets an :edit-source
;;; command.  This receives the information necessary to take the user in
;;; Hemlock to the source location, and does it.
;;;
(defun edit-source-location (name source-created-date tlf-offset
			     local-tlf-offset char-offset form-number)
  (let ((pn (pathname name)))
    (unless (probe-file pn)
      (editor-error "Source file no longer exists: ~A." name))
    (multiple-value-bind (buffer newp) (find-file-buffer pn)
      (let ((date (buffer-write-date buffer))
	    (point (buffer-point buffer)))
	(when newp (push-buffer-mark (copy-mark point) nil))
	(buffer-start point)
	;;
	;; Get to the top-level form in the buffer.
	(cond ((buffer-modified buffer)
	       (loud-message "Buffer has been modified.  Using form offset ~
			      instead of character position.")
	       (dotimes (i local-tlf-offset) 
		 (pre-command-parse-check point)
		 (form-offset point 1)))
	      ((not date)
	       (loud-message "Cannot compare write dates.  Assuming source ~
			      has not been modified -- ~A."
			     name)
	       (character-offset point char-offset))
	      ((= source-created-date date)
	       (character-offset point char-offset))
	      (t
	       (loud-message "File has been modified since reading the source.  ~
			      Using form offset instead of character position.")
	       (dotimes (i local-tlf-offset) 
		 (pre-command-parse-check point)
		 (form-offset point 1))))
	;;
	;; Read our form, get form-number translations, get the source-path,
	;; and make it usable.
	(let ((path (nreverse
		     (butlast
		      (cdr
		       (svref (di:form-number-translations
			       (with-input-from-region
				   (s (region point (buffer-end-mark buffer)))
				 (read s))
			       tlf-offset)
			      form-number)))))
	      (quote-or-function nil))
	  ;;
	  ;; Walk down to the form.
	  (change-to-buffer buffer)
	  (pre-command-parse-check point)
	  (dolist (n path)
	    (when quote-or-function
	      (editor-error
	       "Apparently settled on the symbol QUOTE or FUNCTION via their ~
		read macros, which is odd, but furthermore there seems to be ~
		more source-path left."))
	    (unless (form-offset point 1)
	      ;; Want to use the following and delete the next FORM-OFFSET -1.
	      ;; (scan-direction-valid point t (or :open-paren :prefix))
	      (editor-error
	       "Ran out of text in buffer with more source-path remaining."))
	    (form-offset point -1)
	    (ecase (next-character point)
	      (#\(
	       (mark-after point)
	       (form-offset point n))
	      (#\'
	       (case n
		 (0 (setf quote-or-function t))
		 (1 (mark-after point))
		 (t (editor-error "Next form is QUOTE, but source-path index ~
				   is other than zero or one."))))
	      (#\#
	       (case (next-character (mark-after point))
		 (#\'
		  (case n
		    (0 (setf quote-or-function t))
		    (1 (mark-after point))
		    (t (editor-error "Next form is FUNCTION, but source-path ~
				      index is other than zero or one."))))
		 (t (editor-error
		     "Can only parse ' and #' read macros."))))))
	  ;; Get to the beginning of the form.
	  (form-offset point 1)
	  (form-offset point -1)))))
  (setf *debug-editor-source-data* t)
  ;;
  ;; While Hemlock was setting up the source edit, the user could have typed
  ;; while looking at a buffer no longer current when the commands execute.
  (clear-editor-input *editor-input*))

;;; CANNOT-EDIT-SOURCE-LOCATION -- Interface.
;;;
;;; The slave calls this when the debugger command "EDIT-SOURCE" runs, and the
;;; slave cannot give the editor source information.
;;;
(defun cannot-edit-source-location ()
  (throw 'editor-top-level nil))

;;;
;;; Miscellaneous commands.
;;;

(define-debugger-command "Flush Errors"
  "In the \"Current Eval Server\", toggles whether the debugger ignores errors
   or recursively enters itself."
  :flush)
