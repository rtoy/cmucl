;;; -*- Package: debug -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/ntrace.lisp,v 1.7 1992/05/15 18:31:05 ram Exp $")
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
	  *trace-frame* *trace-values* *max-trace-indentation*))

(defvar *traced-function-list* nil
  "A list of functions which are traced.")

(defvar *trace-print-length* nil
  "Tracing output occurs with *print-length* bound to this value.")

(defvar *trace-print-level* nil
  "Tracing output occurs with *print-level* bound to this value.")

(defvar *trace-frame* nil
  "TRACE causes expressions for its switches to evaluate within a context
   where this is bound to the appropriate control stack frame.")

(defvar *trace-values* nil
  "This is bound to the returned values when evaluating :BREAK-AFTER and
   :PRINT-AFTER forms.")

(defvar *max-trace-indentation* nil
  "This is currently unused.")


;;; TRACE -- Public.
;;;
(defmacro trace (&rest specs)
  "Establishes tracing for specified functions and pushes them on
   *traced-function-list*.  Each specification is one of the following:
      function
      the name of a function
      a list form
   If it is a list form, it has the following structure:
      (function-or-name <trace-option> <value> <trace-option> <value> ...)
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
			     ,@trace-1-forms
			     ',(reverse name-list)))
	(multiple-value-bind
	    (name options)
	    (typecase spec
	      ((or symbol function)
	       (values spec nil))
	      (list
	       (let ((fun (car spec)))
		 (cond ((eq fun 'quote)
			(error "Do NOT quote function names."))
		       ((or (symbolp fun) (functionp fun))
			(values fun (cdr spec)))
		       ((not (and (consp fun) (= (length fun) 2)))
			(error "Illegal function name:  ~S." fun))
		       ((eq (car fun) 'setf)
			(values fun (cdr spec)))
		       (t (error "Illegal function name:  ~S." fun)))))
	      (t (error "Illegal trace spec:  ~S." spec)))
	  (push name name-list)
	  (destructuring-bind (&key condition break break-after break-all
				    wherein print print-after print-all)
			      options
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


;;; TRACE-FDEFINITION  --  Internal
;;;
;;;    Given a function or macro name, return the definition.  Error if a
;;; special form.  If already a function, just return it.
;;;
(defun trace-fdefinition (x)
  (typecase x
    (symbol
     (cond ((special-form-p x)
	    (error "Can't trace special form ~S." x))
	   ((macro-function x))
	   (t
	    (fdefinition x))))
    (function x)
    (t (fdefinition x))))


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

;;; This maps functions to the two breakpoints created in TRACE-1, so we can
;;; get rid of them in UNTRACE-1.
;;;
(defvar *trace-breakpoints* (make-hash-table :test #'eq))
;;;
(defun clear-trace-breakpoint-record (fname new-value)
  (declare (ignore new-value))
  (when (fboundp fname)
    (let* ((fun (trace-fdefinition fname))
	   (bpts (gethash fun *trace-breakpoints*)))
      (when bpts
	;; Free breakpoint bookkeeping data.
	(di:delete-breakpoint (car bpts))
	(di:delete-breakpoint (cdr bpts))
	;; Free TRACE bookkeeping data.
	(setf (gethash fun *trace-breakpoints*) nil)))))
;;;
(push #'clear-trace-breakpoint-record ext:*setf-fdefinition-hook*)

;;; TRACE-1 -- Internal.
;;;
;;; This establishes :function-start and :function-end breakpoints with
;;; appropriate hook functions to TRACE function-name as described by the user.
;;;
(defun trace-1 (function-or-name condition break break-after wherein print
		 print-after)
  (let ((fun (trace-fdefinition function-or-name)))
    (when (member fun *traced-function-list*)
      (warn "Function ~S already TRACE'd, retracing it." function-or-name)
      (untrace-1 fun))
    
    (when wherein
      (dolist (f wherein)
	(unless (fboundp f)
	  (error "Undefined :wherein name -- ~S." f))))
    (let* ((debug-fun (di:function-debug-function fun))
	   ;; The start and end hooks use conditionp for communication.
	   (conditionp nil)
	   (start (di:make-breakpoint
		   #'(lambda (frame bpt)
		       (let ((*trace-frame* frame))
			 (cond ((and (or (not condition) ;Save a call to EVAL
					 (eval condition))
				     (or (not wherein)
					 (trace-wherein-p frame wherein)))
				(setf conditionp t)
				(print-trace-start frame bpt print))
			       (t (setf conditionp nil)))
			 (when (and break (eval break))
			   (di:flush-frames-above frame)
			   (let ((*stack-top-hint* frame))
			     (break "Breaking before TRACE'd call to ~S."
				    function-or-name)))))
		   debug-fun :kind :function-start))
	   (end (di:make-breakpoint
		 #'(lambda (frame bpt *trace-values* cookie)
		     (if (member fun *traced-function-list*)
			 (let ((*trace-frame* frame))
			   (when conditionp
			     (print-trace-end frame bpt *trace-values* cookie
					      print-after))
			   (pop *traced-entries*)
			   (when (and break-after (eval break-after))
			     (di:flush-frames-above frame)
			     (let ((*stack-top-hint* frame))
			       (break "Breaking after TRACE'd call to ~S."
				      function-or-name))))
			 (pop *traced-entries*)))
		 debug-fun :kind :function-end
		 :function-end-cookie
		 #'(lambda (frame x)
		     (when (and *traced-entries*
				(not (di:function-end-cookie-valid-p
				      frame (car *traced-entries*))))
		       (loop
			 (pop *traced-entries*)
			 (when (or (not *traced-entries*)
				   (di:function-end-cookie-valid-p
				    frame (car *traced-entries*)))
			   (return))))
		     (push x *traced-entries*)))))
      (assert (not (gethash fun *trace-breakpoints*)))
      (setf (gethash fun *trace-breakpoints*) (cons start end))
      ;; The next two forms must be in the order in which they appear.  They
      ;; rely on a documented property that later activated breakpoint hooks
      ;; run first, and the end breakpoint establishes a starting helper bpt.
      (di:activate-breakpoint start)
      (di:activate-breakpoint end))
    (push fun *traced-function-list*)))

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
    (setf *traced-entries* (member cookie *traced-entries*)))
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

;;; TRACE-WHEREIN-P -- Internal.
;;;
;;; The TRACE hooks use this for the :wherein arg.
;;;
(defun trace-wherein-p (frame names)
  (do ((frame (di:frame-down frame) (di:frame-down frame)))
      ((not frame) nil)
    (when (member (di:debug-function-name (di:frame-debug-function frame))
		  names :test #'equal)
      (return t))))



;;;; N-UNTRACE.

(defmacro untrace (&rest specs)
  "Removes tracing from the specified functions.  With no args, untraces all
   functions."
  (let ((specs (or specs *traced-function-list*)))
    `(progn
       ,@(mapcar #'(lambda (spec)
		     `(untrace-1 ',(if (consp spec) (car spec) spec)))
		 specs)
       t)))

(defun untrace-1 (function-or-name)
  (let ((fun (trace-fdefinition function-or-name)))
    (cond ((member fun *traced-function-list*)
	   (let ((breakpoints (gethash fun *trace-breakpoints*)))
	     (di:delete-breakpoint (car breakpoints))
	     (di:delete-breakpoint (cdr breakpoints))
	     (setf (gethash fun *trace-breakpoints*) nil))
	   (setf *traced-function-list* (delete fun *traced-function-list*)))
	  (t (warn "Function is not TRACE'd -- ~S." fun)))))
