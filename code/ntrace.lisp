;;; -*- Package: debug -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/ntrace.lisp,v 1.1 1991/10/13 14:29:30 chiles Exp $")
;;;
;;; **********************************************************************
;;;
;;; This is a tracing facility.
;;;
;;; THIS IS CURRENTLY UNDER DEVELOPMENT AND TESTING.
;;;
;;; Written by Bill Chiles.
;;;
;;; **********************************************************************
;;;

(in-package "DEBUG")

;;; BECAUSE SOMEONE SLOPPILY MADE THE "DEBUG" PACKAGE USE "EXT", I HAVE TO BEND
;;; OVER BACKWARDS NOW TO PREVENT STEPPING ON STUFF EXPORTED BY THE OLD TRACE
;;; CODE FROM "EXT".  THAT'S WHY THERE ARE ALL THE FUNNY NAMES.  NO, I DON'T
;;; FEEL LIKE CLEANING UP THE "DEBUG" PACKAGE ONCE AGAIN.
;;;

(export '(*n-trace-print-level* *n-trace-print-length* *n-traced-function-list*
	  n-trace n-untrace))

(defvar *n-traced-function-list* ()
  "A list of function names which are traced.")

(defvar *n-trace-print-length* ()
  "*Print-length* will be bound to this value when trace is printing.")

(defvar *n-trace-print-level* ()
  "*Print-level* will be bound to this value when trace is printing.")



;;;; TRACE.

;;; WITH-KEYWORDS -- Internal.
;;;
;;; This takes an options list of the following form:
;;;    (<option-name> <value> ...)
;;; It also takes a keyword binding spec of the following form:
;;;    ((<keyword> <binding-var> <default>)
;;;     ...)
;;; This returns a form that binds the variables to any provided value in
;;; options-list or to the default.
;;;
(defmacro with-keywords (option-list key-list &rest body)
  `(let ,(mapcar #'(lambda (kl)
		     `(,(cadr kl)		;var
		       (or (getf ,option-list ,(car kl))
			   ,(caddr kl))))	;default
		 key-list)
     ,@body))

;;; N-TRACE -- Public.
;;;
(defmacro n-trace (&rest specs)
  "Establishes tracing for specified functions and pushes their names on
   *n-traced-function-list*.  Each specification is either the name of a function
   or a list of the form:
      (function-name <trace-option> <value> <trace-option> <value> ...)
   If you supply no specifications, TRACE returns the list of traced functions.
   The following options are valid:
      :condition
         A form to EVAL to determine whether TRACE should display anything.
      :break
         A form to EVAL to determine whether to call BREAK before the call.
      :break-after
         Like :break, but takes effect after the call.
      :break-all
         Like :break, but takes effect before and after call.
      :wherein
         A function name or list of names in which TRACE will display a call.
      :print
         A list of forms for EVAL whose results TRACE will display in addition
         to other information before the call.
      :print-after
         Like :print, but takes effect after the call.
      :print-all
         Like :print, but takes effect before and after the call."
  (cond
   ((not specs) '*n-traced-function-list*)
   (t
    (let ((name-list nil)
	  (trace-1-forms nil))
      (dolist (spec specs `(progn
			     ;; Make sure every name has a definition.
			     ,@(mapcar #'(lambda (x) `#',x) name-list)
			     ,@trace-1-forms
			     ',(nreverse name-list)))
	(multiple-value-bind
	    (name options)
	    (typecase spec
	      (symbol
	       (values spec nil))
	      (list
	       (unless (symbolp (car spec))
		 (error "Illegal function name:  ~S." (car spec)))
	       (when (eq (car spec) 'quote)
		 (error "I bet you don't want to trace QUOTE."))
	       (values (car spec) (cdr spec)))
	      (t (error "Illegal trace spec:  ~S." spec)))
	  (push name name-list)
	  (with-keywords options
	    ((:condition condition nil)
	     (:break break nil)
	     (:break-after break-after nil)
	     (:break-all break-all nil)
	     (:wherein wherein nil)
	     (:print print nil)
	     (:print-after print-after nil)
	     (:print-all print-all nil))
	    (when break-all
	      (setf break (setf break-after break-all)))
	    (when print-all
	      (setf print (setf print-after print-all)))
	    ;; Wherein must be a list of symbols or nil.
	    (setf wherein
		  (typecase wherein
		    (null nil)
		    (symbol (list wherein))
		    (list (dolist (fun wherein wherein)
			    (unless (symbolp fun)
			      (error "Illegal function name, ~S, in :wherein."
				     fun))))
		    (t (error "Illegal :wherein option:  ~S." wherein))))
	    ;; Print and print-after must be lists.
	    (unless (listp print)
	      (error "Illegal form list, ~S, for :print." print))
	    (unless (listp print-after)
	      (error "Illegal form list, ~S, for :print-after." print-after))
	    (push `(n-trace-1 ',name ',condition ',break ',break-after
			    ',wherein ',print ',print-after)
		  trace-1-forms))))))))

;;; This is a list of function-end-cookies, which we use to note distinct
;;; dynamic entries into functions.
;;;
;;; The length of this list tells us the indentation to use for printing TRACE
;;; messages.
;;;
;;; This list also helps us synchronize the TRACE facility dynamically for
;;; detecting non-local flow of control that affects TRACE'ing.  Whenever
;;; execution hits a :function-end breakpoint used for TRACE'ing, we look for
;;; the function-end-cookie at the top of *traced-entries*.  If it is not
;;; there, we can adjust our indentation and the contents of the list
;;; accordingly, printing a warning that some TRACE'd entries have been fouled.
;;;
(defvar *traced-entries* nil)

;;; This maps function names to the two breakpoints created in TRACE-1, so we
;;; can get rid of them in UNTRACE-1.
;;;
(defvar *trace-breakpoints* (make-hash-table :test #'eq))

;;; N-TRACE-1 -- Internal.
;;;
;;; This establishes :function-start and :function-end breakpoints with
;;; appropriate hook functions to TRACE function-name as described by the user.
;;;
(defun n-trace-1 (function-name condition break break-after wherein print
		 print-after)
  (declare (ignore condition break break-after wherein print print-after))
  (cond
   ((member function-name *n-traced-function-list*)
    (warn "Function ~S already TRACE'd, ignoring this request."
	  function-name))
   (t
    (let* ((debug-fun (di:function-debug-function (fdefinition function-name)))
	   (start (di:make-breakpoint #'print-trace-start
				      debug-fun
				      :kind :function-start))
	   (end (di:make-breakpoint #'print-trace-end
				    debug-fun
				    :kind :function-end
				    :function-end-cookie
				    #'(lambda (x) (push x *traced-entries*)))))
      (setf (gethash function-name *trace-breakpoints*) (cons start end))
      ;; This works TOTALLY SLIMY by the order of the next two calls.
      (di:activate-breakpoint start)
      (di:activate-breakpoint end))
    (push function-name *n-traced-function-list*))))

;;; PRINT-TRACE-START -- Internal.
;;;
;;; This prints a representation of the call establishing frame.
;;;
(defun print-trace-start (frame bpt)
  (declare (ignore bpt))
  (let ((*print-length* (or *n-trace-print-length* *print-length*))
	(*print-level* (or *n-trace-print-level* *print-level*)))
    (fresh-line)
    (print-trace-indentation)
    (print-frame-call-1 frame nil)
    (terpri)))

;;; PRINT-TRACE-END -- Internal.
;;;
;;; This prints a representation of the return values delivered to frame by the
;;; function for which bpt is a :function-end breakpoint.  First, this checks
;;; to see that cookie is at the top of *traced-entries*; if it is not, then we
;;; need to adjust this list to determine the correct indentation for output.
;;;
(defun print-trace-end (frame bpt values cookie)
  (declare (ignore frame bpt))
  (unless (eq cookie (car *traced-entries*))
    (setf *traced-entries* (member cookie *traced-entries*))
    (fresh-line)
    (write-line "WARNING: dynamic flow of control occurred while TRACE'ing."))
  (print-trace-indentation)
  (pop *traced-entries*)
  (write-string "returned ")
  (dolist (v values)
    (prin1 v)
    (write-char #\space))
  (terpri))

(defun print-trace-indentation ()
  (let ((len (length (the list *traced-entries*))))
    (dotimes (i len) (write-string "  "))
    (prin1 len)
    (write-string ": ")))



;;;; N-UNTRACE.

(defmacro n-untrace (&rest names)
  "Removes tracing from the functions named.  With no args, untraces all
   functions."
  (let ((names (or names *n-traced-function-list*))
	(untrace-1-forms nil))
    (dolist (name names `(progn ,@(nreverse untrace-1-forms) t))
      (if (symbolp name)
	  (push `(n-untrace-1 ',name) untrace-1-forms)
	  (error "Illegal function name -- ~S." name)))))

(defun n-untrace-1 (name)
  (cond ((member name *n-traced-function-list*)
	 (let ((breakpoints (gethash name *trace-breakpoints*)))
	   (di:delete-breakpoint (car breakpoints))
	   (di:delete-breakpoint (cdr breakpoints))
	   (setf (gethash name *trace-breakpoints*) nil))
	 (setf *n-traced-function-list* (delete name *n-traced-function-list*)))
	(t (warn "Function is not TRACE'd -- ~S." name))))
