;;; -*- Package: HEMLOCK -*-
;;;
;;; **********************************************************************
;;; Copyright (c) 1993 Carnegie Mellon University, all rights reserved.
;;; 
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/hemlock/dylan.lisp,v 1.1 1993/07/22 11:42:52 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains stuff that lives in the editor to make slaves and dylan
;;; interact better.
;;;
(in-package "HEMLOCK")

(defmode "Dylan" :major-p nil)

(define-file-type-hook ("dylan") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp")
  (setf (buffer-minor-mode buffer "Dylan") t))

(define-file-option "Module" (buffer value)
  (defhvar "Current Package"
    "The package used for evaluation of Lisp in this buffer."
    :buffer buffer
    :value
    (let* ((eof (list nil))
	   (thing (read-from-string value nil eof)))
      (when (eq thing eof) (error "Bad module file option value."))
      (cond
       ((stringp thing)
	thing)
       ((symbolp thing)
	(symbol-name thing))
       ((characterp thing)
	(string thing))
       (t
	(message
	 "Ignoring \"module\" file option -- cannot convert to a string."))))))

(defcommand "Set Buffer Module" (p)
  "Set the module to be used by Dylan evaluation and compilation commands
   while in this buffer.  When in a slave's interactive buffers, do NOT
   set the editor's module variable, but changed the slave's *current-module*."
  "Prompt for a module to make into a buffer-local variable current-package."
  (declare (ignore p))
  (let* ((name (string (prompt-for-expression
			:prompt "Module name: "
			:help "Name of module to associate with this buffer.")))
	 (buffer (current-buffer))
	 (info (value current-eval-server)))
    (cond ((and info
		(or (eq (server-info-slave-buffer info) buffer)
		    (eq (server-info-background-buffer info) buffer)))
	   (wire:remote (server-info-wire info)
	     (server-set-module name))
	   (wire:wire-force-output (server-info-wire info)))
	  (t
	   (defhvar "Current Package"
	     "The package used for evaluation of Lisp in this buffer."
	     :buffer buffer  :value name)))
    (when (buffer-modeline-field-p buffer :package)
      (dolist (w (buffer-windows buffer))
	(update-modeline-field buffer w :package)))))


;;; DYLAN-MODE-P -- interface.
;;;
;;; Called by various functions if they want their behavior to be different
;;; under dylan.
;;; 
(defun dylan-mode-p (&optional (buffer (current-buffer)))
  (buffer-minor-mode buffer "Dylan"))

(defcommand "Dylan Mode" (p)
  "Toggles the Dylan minor mode in the current buffer."
  "Toggles the Dylan minor mode in the current buffer."
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Dylan")
	(not (dylan-mode-p (current-buffer)))))

