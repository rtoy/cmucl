;;; -*- Package: debug -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/ntrace.lisp,v 1.2 1991/11/03 17:21:16 chiles Exp $")
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

(in-package "LISP")

(export '(trace untrace))


(in-package "DEBUG")

(export '(*trace-print-level* *trace-print-length* *traced-function-list*
	  *trace-frame* *max-trace-indentation*))

(defvar *traced-function-list* nil
  "A list of function names which are traced.")

(defvar *trace-print-length* nil
  "Tracing output occurs with *print-length* bound to this value.")

(defvar *trace-print-level* nil
  "Tracing output occurs with *print-level* bound to this value.")

(defvar *trace-frame* nil
  "TRACE causes expressions for its switches to evaluate within a context
   where this is bound to the appropriate control stack frame.")

(defvar *max-trace-indentation* nil
  "This is currently unused.")


;;;; TRACE.

(eval-when (compile eval)

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

) ;EVAL-WHEN


;;; TRACE -- Public.
;;;
(defmacro trace (&rest specs)
  "Establishes tracing for specified functions and pushes their names on
   *traced-function-list*.  Each specification is either the name of a function
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
         Like :print, but takes effect before and after the call.

   While the provided expression evaluate, debug:*trace-frame* is bound to the
   appropriate frame for accessing locals of the TRACE'd function.  Therefore,
   you can use DI:PREPROCESS-FOR-EVAL (and DI:DEBUG-FUNCTION-START-LOCATION if
   necessary) and with its resulting function, provide an expression including
   a FUNCALL of the function and debug:*trace-frame*."
  (cond
   ((not specs) '*traced-function-list*)
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
	       (let ((fun (car spec)))
		 (cond ((eq fun 'quote)
			(error "Do NOT quote function names."))
		       ((symbolp fun)
			(values fun (cdr spec)))
		       ((not (and (consp fun) (= (length fun) 2)))
			(error "Illegal function name:  ~S." fun))
		       ((eq (car fun) 'setf)
			(values fun (cdr spec)))
		       (t (error "Illegal function name:  ~S." fun)))))
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
			    (unless (or (symbolp fun)
					(and (consp fun)
					     (= (length fun) 2)
					     (eq (car fun) 'setf)))
			      (error "Illegal function name, ~S, in :wherein."
				     fun))))
		    (t (error "Illegal :wherein option:  ~S." wherein))))
	    ;; Print and print-after must be lists.
	    (unless (listp print)
	      (error "Illegal form list, ~S, for :print." print))
	    (unless (listp print-after)
	      (error "Illegal form list, ~S, for :print-after." print-after))
	    (push `(trace-1 ',name ',condition ',break ',break-after
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
;;;
(defun clear-trace-breakpoint-record (fname new-value)
  (declare (ignore new-value))
  (let ((bpts (gethash fname *trace-breakpoints*)))
    (when bpts
      ;; Free breakpoint bookkeeping data.
      (di:delete-breakpoint (car bpts))
      (di:delete-breakpoint (cdr bpts))
      ;; Free TRACE bookkeeping data.
      (setf (gethash fname *trace-breakpoints*) nil))))
;;;
(push #'clear-trace-breakpoint-record ext:*setf-fdefinition-hook*)

;;; TRACE-1 -- Internal.
;;;
;;; This establishes :function-start and :function-end breakpoints with
;;; appropriate hook functions to TRACE function-name as described by the user.
;;;
(defun trace-1 (function-name condition break break-after where-in print
		 print-after)
  (cond
   ((member function-name *traced-function-list*)
    (warn "Function ~S already TRACE'd, ignoring this request."
	  function-name))
   (t
    (when where-in
      (dolist (f where-in)
	(unless (fboundp f)
	  (error "Undefined :where-in name -- ~S." f))))
    (let* ((debug-fun (di:function-debug-function (fdefinition function-name)))
	   ;; The start and end hooks use conditionp for communication.
	   (conditionp nil)
	   (start (di:make-breakpoint
		   #'(lambda (frame bpt)
		       (let ((*trace-frame* frame))
			 (cond ((and (or (not condition) ;Save a call to EVAL
					 (eval condition))
				     (or (not where-in)
					 (trace-where-in-p frame where-in)))
				(setf conditionp t)
				(print-trace-start frame bpt print))
			       (t (setf conditionp nil)))
			 (when (and break (eval break))
			   (break "Breaking before TRACE'd call to ~S."
				  (di:debug-function-name
				   (di:frame-debug-function frame))))))
		   debug-fun :kind :function-start))
	   (end (di:make-breakpoint
		 #'(lambda (frame bpt values cookie)
		     (let ((*trace-frame* frame))
		       (when conditionp
			 (print-trace-end frame bpt values cookie print-after))
		       (pop *traced-entries*)
		       (when (and break-after (eval break-after))
			 (break "Breaking after TRACE'd call to ~S."
				(di:debug-function-name
				 (di:frame-debug-function frame))))))
		 debug-fun :kind :function-end
		 :function-end-cookie
		 #'(lambda (frame x)
		     (when (and *traced-entries*
				(not (di:function-end-cookie-valid-p
				      frame (car *traced-entries*))))
		       (format t "~&WARNING: dynamic flow of control occurred ~
				  while TRACE'ing.~%")
		       (loop
			 (pop *traced-entries*)
			 (when (or (not *traced-entries*)
				   (di:function-end-cookie-valid-p
				    frame (car *traced-entries*)))
			   (return))))
		     (push x *traced-entries*)))))
      (setf (gethash function-name *trace-breakpoints*) (cons start end))
      ;; The next two forms must be in the order in which they appear.  They
      ;; rely on a documented property that later activated breakpoint hooks
      ;; run first, and the end breakpoint establishes a starting helper bpt.
      (di:activate-breakpoint start)
      (di:activate-breakpoint end))
    (push function-name *traced-function-list*))))

;;; PRINT-TRACE-START -- Internal.
;;;
;;; This prints a representation of the call establishing frame.
;;;
(defun print-trace-start (frame bpt &optional print)
  (declare (ignore bpt))
  (let ((*print-length* (or *trace-print-length* *print-length*))
	(*print-level* (or *trace-print-level* *print-level*)))
    (fresh-line)
    (print-trace-indentation)
    (print-frame-call-1 frame nil)
    (dolist (ele print)
      (fresh-line)
      (print-trace-indentation)
      (prin1 (eval ele)))
    (terpri)))

;;; PRINT-TRACE-END -- Internal.
;;;
;;; This prints a representation of the return values delivered to frame by the
;;; function for which bpt is a :function-end breakpoint.  First, this checks
;;; to see that cookie is at the top of *traced-entries*; if it is not, then we
;;; need to adjust this list to determine the correct indentation for output.
;;;
(defun print-trace-end (frame bpt values cookie &optional print-after)
  (declare (ignore frame bpt))
  (unless (eq cookie (car *traced-entries*))
    (setf *traced-entries* (member cookie *traced-entries*))
    (fresh-line)
    (write-line "WARNING: dynamic flow of control occurred while TRACE'ing."))
  (print-trace-indentation)
  (write-string "returned ")
  (dolist (v values)
    (prin1 v)
    (write-char #\space))
  (dolist (ele print-after)
    (terpri)
    (print-trace-indentation)
    (prin1 (eval ele)))
  (terpri))

(defun print-trace-indentation ()
  (let ((len (length (the list *traced-entries*))))
    (dotimes (i len) (write-string "  "))
    (prin1 len)
    (write-string ": ")))

;;; TRACE-WHERE-IN-P -- Internal.
;;;
;;; The TRACE hooks use this for the :where-in arg.
;;;
(defun trace-where-in-p (frame names)
  (do ((frame (di:frame-down frame) (di:frame-down frame)))
      ((not frame) nil)
    (when (member (di:debug-function-name (di:frame-debug-function frame))
		  names :test #'equal)
      (return t))))



;;;; N-UNTRACE.

(defmacro untrace (&rest names)
  "Removes tracing from the functions named.  With no args, untraces all
   functions."
  (let ((names (or names *traced-function-list*))
	(untrace-1-forms nil))
    (dolist (name names `(progn ,@(nreverse untrace-1-forms) t))
      (if (symbolp name)
	  (push `(untrace-1 ',name) untrace-1-forms)
	  (error "Illegal function name -- ~S." name)))))

(defun untrace-1 (name)
  (cond ((member name *traced-function-list*)
	 (let ((breakpoints (gethash name *trace-breakpoints*)))
	   (di:delete-breakpoint (car breakpoints))
	   (di:delete-breakpoint (cdr breakpoints))
	   (setf (gethash name *trace-breakpoints*) nil))
	 (setf *traced-function-list* (delete name *traced-function-list*)))
	(t (warn "Function is not TRACE'd -- ~S." name))))
