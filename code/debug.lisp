;;; -*- Mode: Lisp; Package: Debug; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/debug.lisp,v 1.19 1991/07/14 02:48:36 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; CMU Common Lisp Debugger.  This includes a basic command-line oriented
;;; debugger interface as well as support for Hemlock to deliver debugger
;;; commands to a slave Lisp.
;;;
;;; Written by Bill Chiles.
;;;

(in-package "DEBUG")

(export '(internal-debug *in-the-debugger* backtrace *flush-debug-errors*
	  *debug-print-level* *debug-print-length* *debug-prompt*
	  *help-line-scroll-count*

	  *auto-eval-in-frame* var arg
	  *only-block-start-locations* *print-location-kind*

	  do-debug-command))

(in-package "LISP")
(export '(invoke-debugger *debugger-hook*))

(in-package "DEBUG")

;;;
;;; Used to communicate to debug-loop that we are at a step breakpoint.
;;;
(define-condition step-condition (simple-condition))


;;;; Variables, parameters, and constants.

(defparameter *debug-print-level* 3
  "*PRINT-LEVEL* is bound to this value when debug prints a function call.  If
  null, use *PRINT-LEVEL*")

(defparameter *debug-print-length* 5
  "*PRINT-LENGTH* is bound to this value when debug prints a function call.  If
  null, use *PRINT-LENGTH*.")

(defvar *in-the-debugger* nil
  "If this is bound before the debugger is invoked, it is used as the stack
(defvar *stack-top* nil)

;;;; Breakpoint state:

(defvar *only-block-start-locations* nil
  "When true, the LIST-LOCATIONS command only displays block start locations.
   Otherwise, all locations are displayed.")

(defvar *print-location-kind* nil
  "If true, list the code location type in the LIST-LOCATIONS command.")

;;; A list of the types of code-locations that should not be stepped to and
;;; should not be listed when listing breakpoints.
;;;
(defvar *bad-code-location-types* '(:call-site :internal-error))
(declaim (type list *bad-code-location-types*))

;;; Code locations of the possible breakpoints
;;;
(defvar *possible-breakpoints*)
(declaim (type list *possible-breakpoints*))

;;; A list of the made and active breakpoints, each is a breakpoint-info
;;;
(defvar *breakpoints* nil)
(declaim (type list *breakpoints*))

;;; A list of breakpoint-info structures of the made and active step
;;; breakpoints.
  ABORT    returns to the previous abort restart case.
;;;
(defvar *step-breakpoints* nil)  
;;;
(defvar *number-of-steps* 1)
(declaim (type integer *number-of-steps*))

;;; Used when listing and setting breakpoints.
;;;
(defvar *default-breakpoint-debug-function* nil)
  L              lists locals in current function.


;;;; Code location utilities:

      (unless found 
	(setf first-code-location code-location)
	(setf found t)))
    first-code-location))

;;; NEXT-CODE-LOCATIONS -- Internal.
;;;
;;; Returns a list of the next code-locations following the one passed.  One of
;;; the *bad-code-location-types* will not be returned.
;;;
(defun next-code-locations (code-location)
    (di:do-debug-block-locations (block-code-location debug-block)
    (setf kind (di:breakpoint-kind (breakpoint-info-breakpoint place)))
    (format t "Cannot step, in elsewhere code~%"))
   (t
    (let* ((code-location (di:frame-code-location frame))
	   (next-code-locations (next-code-locations code-location)))
      (cond
       (next-code-locations
	(dolist (code-location next-code-locations)
	  (let ((bp-info (location-in-list code-location *breakpoints*)))
	      (di:deactivate-breakpoint (breakpoint-info-breakpoint bp-info))))
	  (let ((bp (di:make-breakpoint #'main-hook-function code-location
					:kind :code-location)))
	    (di:activate-breakpoint bp)
	    (push (create-breakpoint-info code-location bp 0)
		  *step-breakpoints*))))
       (t
	(let* ((debug-function (di:frame-debug-function *current-frame*))
	       (bp (di:make-breakpoint #'main-hook-function debug-function
      (print-frame-call frame))))
	  (di:activate-breakpoint bp)
	  (push (create-breakpoint-info debug-function bp 0)
		*step-breakpoints*))))))))



;;;; Backtrace:

;;; BACKTRACE -- Public.
;;;
(defun backtrace (&optional (count most-positive-fixnum)
			    (*standard-output* *debug-io*))
  "Show a listing of the call stack going down from the current frame.  In the
   debugger, the current frame is indicated by the prompt.  Count is how many
   frames to show."
  (let ((*print-length* (or *debug-print-length* *print-length*))
	(*print-level* (or *debug-print-level* *print-level*)))
    (fresh-line *standard-output*)
    (do ((frame (if *in-the-debugger* *current-frame* (di:top-frame))
		(di:frame-down frame))
	 (count count (1- count)))
	((or (null frame) (zerop count))
	 (values))
      (print-frame-call frame :number t))))


;;;; Frame printing:

(eval-when (compile eval)

;;; LAMBDA-LIST-ELEMENT-DISPATCH -- Internal.
(defstruct (unprintable-object
	    (:constructor make-unprintable-object (string))
	    (:print-function (lambda (x s d)
			       (declare (ignore d))
			       (format s "#<~A>"
				       (unprintable-object-string x)))))
  string)

;;;
;;; This is a convenient way to express what to do for each type of lambda-list
;;; element.
;;;
(defmacro lambda-list-element-dispatch (element &key required optional rest
						keyword deleted)
  `(etypecase ,element
(defun print-frame-call (frame &optional
			       (*print-length* (or *debug-print-length*
						   *print-length*))
			       (*print-level* (or *debug-print-level*
						  *print-level*))
			       (verbosity 1))
  (ecase verbosity
    (0 (print frame))
    (1 (print-frame-call-1 frame))
    ((2 3 4 5))))
	     (t ,other)))))
			       (format s "#<~A>"
				       (unprintable-object-string x)))))
  string)

;;; PRINT-FRAME-CALL-1 -- Internal.
;;;
;;; This prints frame with verbosity level 1.  If we hit a rest-arg, 
(defun print-frame-call-1 (frame)
;;; punting the loop over lambda-list variables since any other arguments
;;; will be in the rest-arg's list of values.
;;;
(defun print-frame-call-1 (frame)
  (let* ((d-fun (di:frame-debug-function frame))
	 (loc (di:frame-code-location frame))
	 (results (list (di:debug-function-name d-fun))))
    (handler-case
	(dolist (ele (di:debug-function-lambda-list d-fun))
	  (lambda-list-element-dispatch ele
	    :required ((push (frame-call-arg ele loc frame) results))
	    :optional ((push (frame-call-arg (second ele) loc frame) results))
	    :keyword ((push (second ele) results)
		      (push (frame-call-arg (third ele) loc frame) results))
	    :deleted ((push (frame-call-arg ele loc frame) results))
	    :rest ((lambda-var-dispatch (second ele) loc
		     nil
		     (progn
		       (setf results
			     (append (reverse (di:debug-variable-value
					       (second ele) frame))
				     results))
		       (return))
		     (push (make-unprintable-object "unavaliable-rest-arg")
    (print (nreverse results))
       ()
       (push (make-unprintable-object "lambda-list-unavailable") results)))
    (prin1 (nreverse results))
    (when (di:debug-function-kind d-fun)

      (prin1 (di:debug-function-kind d-fun))
      (write-char #\]))))
;;;
(defun frame-call-arg (var location frame)
  (lambda-var-dispatch var location
    (make-unprintable-object "unused-arg")
    (di:debug-variable-value var frame)
    (make-unprintable-object "unavailable-arg")))
;;;; INVOKE-DEBUGGER.
;;; PRINT-FRAME-CALL -- Interface
;;;
;;; This prints a representation of the function call causing frame to exist.
;;; Verbosity indicates the level of information to output; zero indicates just
;;; printing the debug-function's name, and one indicates displaying call-like,
;;; one-liner format with argument values.
;;;
(defun print-frame-call (frame &key (print-length *print-length*)
			       (print-level *print-level*)
			       (verbosity 1)
			       (number nil))
  (let ((*print-length* (or *debug-print-length* print-length))
(defvar *debug-abort*)
	(*print-level* (or *debug-print-level* print-level)))
    (cond
     ((zerop verbosity)
      (when number
	(format t "~&~S: " (di:frame-number frame)))
      (format t "~S" frame))
     (t
      (when number
	(format t "~&~S: " (di:frame-number frame)))
  (mach:unix-sigsetmask 0)
    (when (>= verbosity 2)
      (let ((loc (di:frame-code-location frame)))
	 (*debug-abort* (find-restart 'abort))
	(handler-case
	    (progn
	 (*error-output* *debug-io*))

;;;; Invoke-debugger.

(defvar *debugger-hook* nil
  "This is either nil or a function of two arguments, a condition and the value
   of *debugger-hook*.  This function can either handle the condition or return
   which causes the standard debugger to execute.  The system passes the value
   of this variable to the function because it binds *debugger-hook* to nil
   around the invocation.")
    (do ((p restarts (cdr p))
	 (i 0 (1+ i)))
	((endp p))
      (format s "~&  ~D: ~A~%" i (car p)))))
	 ;; Rebind some printer control variables.
	 (kernel:*current-level* 0)
	 (*print-readably* nil)
	 (*read-eval* t))
    (format *error-output* "~2&~A~2&" *debug-condition*)
    (unless (typep condition 'step-condition)
      (show-restarts *debug-restarts* *error-output*))
    (internal-debug)))

;;; SHOW-RESTARTS -- Internal.
;;;
(defun show-restarts (restarts &optional (s *error-output*))
  (when restarts
    (format s "~&Restarts:~%")
    (let ((count 0)
	  (names-used '(nil))
	  (max-name-len 0))
      (dolist (restart restarts)
;;;; DEBUG-LOOP.
	  (when name
	    (let ((len (length (princ-to-string name))))
	      (when (> len max-name-len)
		(setf max-name-len len))))))
      (unless (zerop max-name-len)
	(incf max-name-len 3))
  (let ((*debug-command-level* (1+ *debug-command-level*))
	(*current-frame* (di:top-frame)))
			 count (- max-name-len 3) name restart)
		 (push name names-used))))
	(incf count)))))

;;; INTERNAL-DEBUG -- Internal Interface.
;;;
;;; This calls DEBUG-LOOP, performing some simple initializations before doing
;;; so.  INVOKE-DEBUGGER calls this to actually get into the debugger.
;;; CONDITIONS::ERROR-ERROR calls this in emergencies to get into a debug
;;; prompt as quickly as possible with as little risk as possible for stepping
;;; on whatever is causing recursive errors.
				      (throw 'debug-loop-catcher nil))
				    (invoke-debugger condition))))
(defun internal-debug ()
  (let ((*in-the-debugger* t)
	    (let ((level *debug-command-level*))
      (clear-input *debug-io*)
      (format *debug-io* "~2&Debug  (type H for help)~2%"))
    (debug-loop)))


					 (stream-command-name input))))
			   (cond ((not cmd-fun)
				  (error "Unknown stream-command -- ~S." input))
				 ((consp cmd-fun)
				  (error "Ambiguous debugger command: ~S."
					 cmd-fun))
				 (t
				  (apply cmd-fun (stream-command-args input))))))
	 (*real-stack-top* (di:top-frame))
	 (*stack-top* (or *stack-top-hint* *real-stack-top*))
				(cmd-fun (debug-command-p exp)))
	 (*current-frame* *stack-top*))
    (handler-bind ((di:debug-condition #'(lambda (condition)
					   (princ condition *debug-io*)
					   (throw 'debug-loop-catcher nil))))
      (fresh-line)
      (print-frame-call *current-frame* :verbosity 2)
      (loop
	(catch 'debug-loop-catcher
	  (handler-bind ((error #'(lambda (condition)
				    (when *flush-debug-errors*
				      (clear-input *debug-io*)
				      (princ condition)
				      (format t "~&Error flushed ...")
				      (throw 'debug-loop-catcher nil)))))
	    ;; Must bind level for restart function created by
	    ;; WITH-SIMPLE-RESTART.
	    (let ((level *debug-command-level*)
		  (restart-commands (make-restart-commands)))
	      (with-simple-restart (abort "Return to debug level ~D." level)
		(funcall *debug-prompt*)
		(let ((input (ext:get-stream-command *debug-io*)))
		  (cond (input
			 (let ((cmd-fun (debug-command-p
					 (ext:stream-command-name input)
					 restart-commands)))
			   (cond
			    ((not cmd-fun)
			     (error "Unknown stream-command -- ~S." input))
			    ((consp cmd-fun)
			     (error "Ambiguous debugger command: ~S." cmd-fun))
			    (t
			     (apply cmd-fun (ext:stream-command-args input))))))
			(t
			 (let* ((exp (read))
				(cmd-fun (debug-command-p exp restart-commands)))
			   (cond ((not cmd-fun)
				  (debug-eval-print exp))
				 ((consp cmd-fun)
				  (format t "~&Your command, ~S, is ambiguous:~%"
					  exp)
				  (dolist (ele cmd-fun)
				    (format t "   ~A~%" ele)))
				 (t
				  (funcall cmd-fun)))))))))))))))

(defvar *auto-eval-in-frame* t
  "When set (the default), evaluations in the debugger's command loop occur
   relative to the current frame's environment without the need of debugger
   forms that explicitly control this kind of evaluation.")

(defun debug-eval-print (exp)
  (setq +++ ++ ++ + + - - exp)
  (let* ((values (multiple-value-list
		  (if (and (fboundp 'eval:internal-eval) *auto-eval-in-frame*)
		      (di:eval-in-frame *current-frame* -)
		      (eval -))))
	 (*standard-output* *debug-io*))
    (fresh-line)
    (if values (prin1 (car values)))
    (dolist (x (cdr values))
      (fresh-line)
      (prin1 x))
    (setq /// // // / / values)
    (setq *** ** ** * * (car values))
    ;; Make sure nobody passes back an unbound marker.
    (unless (boundp '*)
      (setq * nil)
      (fresh-line)
      (princ "Setting * to NIL -- was unbound marker."))))



;;;; Debug loop functions.

;;; These commands are function, not really commands, so users can get their
;;; hands on the values returned.
;;;

(eval-when (eval compile)

(defmacro define-var-operation (ref-or-set &optional value-var)
  `(let* ((temp (etypecase name
		  (symbol (di:debug-function-symbol-variables
			   (di:frame-debug-function *current-frame*)
			   name))
		  (simple-string (di:ambiguous-debug-variables
				  (di:frame-debug-function *current-frame*)
				  name))))
	  (location (di:frame-code-location *current-frame*))
	  ;; Let's only deal with valid variables.
	  (vars (remove-if-not #'(lambda (v)
				   (eq (di:debug-variable-validity v location)
				       :valid))
			       temp)))
     (declare (list vars))
     (cond ((null vars)
	    (error "No known valid variables match ~S." name))
	   ((= (length vars) 1)
	    ,(ecase ref-or-set
	       (:ref
		'(di:debug-variable-value (car vars) *current-frame*))
	       (:set
		`(setf (di:debug-variable-value (car vars) *current-frame*)
		       ,value-var))))
	   (t
	    ;; Since we have more than one, first see if we have any
	    ;; variables that exactly match the specification.
	    (let* ((name (etypecase name
			   (symbol (symbol-name name))
			   (simple-string name)))
		   (exact (remove-if-not #'(lambda (v)
					     (string= (di:debug-variable-name v)
						      name))
					 vars))
		   (vars (or exact vars)))
	      (declare (simple-string name)
		       (list exact vars))
	      (cond
	       ;; Check now for only having one variable.
	       ((= (length vars) 1)
		,(ecase ref-or-set
		   (:ref
		    '(di:debug-variable-value (car vars) *current-frame*))
		   (:set
		    `(setf (di:debug-variable-value (car vars) *current-frame*)
			   ,value-var))))
	       ;; If there weren't any exact matches, flame about ambiguity
	       ;; unless all the variables have the same name.
	       ((and (not exact)
		     (find-if-not
		      #'(lambda (v)
			  (string= (di:debug-variable-name v)
				   (di:debug-variable-name (car vars))))
		      (cdr vars)))
		(error "Specification ambiguous:~%~{   ~A~%~}"
		       (mapcar #'di:debug-variable-name
			       (delete-duplicates
				vars :test #'string=
				:key #'di:debug-variable-name))))
	       ;; All names are the same, so see if the user ID'ed one of them.
	       (id-supplied
		(let ((v (find id vars :key #'di:debug-variable-id)))
		  (unless v
		    (error "Invalid variable ID, ~D, should have been one of ~S."
			   id (mapcar #'di:debug-variable-id vars)))
		  ,(ecase ref-or-set
		     (:ref
		      '(di:debug-variable-value v *current-frame*))
		     (:set
		      `(setf (di:debug-variable-value v *current-frame*)
			     ,value-var)))))
	       (t
		(error "Specify variable ID to disambiguate ~S.  Use one of ~S."
		       name (mapcar #'di:debug-variable-id vars)))))))))

) ;EVAL-WHEN

;;; VAR -- Public.
;;;
(defun var (name &optional (id 0 id-supplied))
  "Returns a variable's value if possible.  Name is a simple-string or symbol.
   If it is a simple-string, it is an initial substring of the variable's name.
   If name is a symbol, it has the same name and package as the variable whose
   value this function returns.  If the symbol is uninterned, then the variable
   has the same name as the symbol, but it has no package.

   If name is the initial substring of variables with different names, then
   this return no values after displaying the ambiguous names.  If name
   determines multiple variables with the same name, then you must use the
   optional id argument to specify which one you want.  If you left id
   unspecified, then this returns no values after displaying the distinguishing
   id values.

   The result of this function is limited to the availability of variable
   information.  This is SETF'able."
  (define-var-operation :ref))
;;;
(defun (setf var) (value name &optional (id 0 id-supplied))
  (define-var-operation :set value))



;;; ARG -- Public.
;;;
(defun arg (n)
  "Returns the n'th argument's value if possible.  Argument zero is the first
   argument in a frame's default printed representation.  Count keyword/value
   pairs as separate arguments."
  (multiple-value-bind
      (var lambda-var-p)
      (nth-arg n (handler-case (di:debug-function-lambda-list
				(di:frame-debug-function *current-frame*))
		   (di:lambda-list-unavailable ()
		     (error "No argument values are available."))))
    (if lambda-var-p
	(lambda-var-dispatch var (di:frame-code-location *current-frame*)
	  (error "Unused arguments have no values.")
	  (di:debug-variable-value var *current-frame*)
	  (error "Invalid argument value."))
	var)))

;;; NTH-ARG -- Internal.
;;;
;;; This returns the n'th arg as the user sees it from args, the result of
;;; DI:DEBUG-FUNCTION-LAMBDA-LIST.  If this returns a potential debug-variable
;;; from the lambda-list, then the second value is t.  If this returns a
;;; keyword symbol or a value from a rest arg, then the second value is nil.
;;;
(defun nth-arg (count args)
  (let ((n count))
    (dolist (ele args (error "Argument specification out of range -- ~S." n))
      (lambda-list-element-dispatch ele
	:required ((if (zerop n) (return (values ele t))))
	 (warn "Redefining ~S debugger command." ,name))
			((zerop (decf n))
			 (return (values (third ele) t)))))
	:deleted ((if (zerop n) (return (values ele t))))
	:rest ((let ((var (second ele)))
		 (lambda-var-dispatch var
				      (di:frame-code-location *current-frame*)
		   (error "Unused rest-arg before n'th argument.")
		   (dolist (value
			    (di:debug-variable-value var *current-frame*)
			    (error "Argument specification out of range -- ~S."
				   n))
		     (if (zerop n)
			 (return-from nth-arg (values value nil))
			 (decf n)))
		   (error "Invalid rest-arg before n'th argument.")))))
      (decf n))))



;;;; Debug loop command definition:

(defvar *debug-commands* nil)

;;; DEF-DEBUG-COMMAND -- Internal.
(defun debug-command-p (form)
  (if (symbolp form)
      (let* ((name (symbol-name form))
    `(progn
       (when (assoc ,name *debug-commands* :test #'string=)
	 (warn "Redefining ~S debugger command." ,name)
	 (setf *debug-commands*
	       (remove ,name *debug-commands* :key #'car :test #'string=)))
       (defun ,fun-name ,args
	 (unless *in-the-debugger*
	(dolist (ele *debug-commands*)
	  (let* ((str (car ele))
		 (str-len (length str)))
	    (declare (simple-string str)
		     (fixnum str-len))
	    (cond ((< str-len len))
		  ((= str-len len)
		   (when (string= name str :end1 len :end2 len)
		     (return-from debug-command-p (cdr ele))))
		  ((string= name str :end1 len :end2 len)
		   (push ele res)))))
;;; DEBUG-COMMAND-P -- Internal.
;;;
;;; This takes a symbol and uses its name to find a debugger command, using
;;; initial substring matching.  It returns the command function if form
;;; identifies only one command, but if form is ambiguous, this returns a list
;;; of the command names.  If there are no matches, this returns nil.  Whenever
;;; the loop that looks for a set of possibilities encounters an exact name
;;; match, we return that command function immediately.
;;;
(defun debug-command-p (form &optional other-commands)
      (let* ((name
	  (mapc #'match-command other-commands))
	;;
	;; Return the right value.
	(cond ((not res) nil)
	      ((= (length res) 1)
    (if next
	(print-frame-call (setf *current-frame* next))
	(format t "~&Top of stack."))))

;;;
;;; Returns a list of debug commands (in the same format as *debug-commands*)
    (if next
	(print-frame-call (setf *current-frame* next))
	(format t "~&Bottom of stack."))))
(defun make-restart-commands (&optional (restarts *debug-restarts*))
    (dolist (restart restarts)
  (print-frame-call
   (setf *current-frame*
	 (do ((prev *current-frame* lead)
	      (lead (di:frame-up *current-frame*) (di:frame-up lead)))
	     ((null lead) prev)))))
	    (push (cons (format nil "~d" num) restart-fun) commands))))
      (incf num))
  (print-frame-call
   (setf *current-frame*
	 (do ((prev *current-frame* lead)
	      (lead (di:frame-down *current-frame*) (di:frame-down lead)))
	     ((null lead) prev)))))

  (let ((next (di:frame-up *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
  
(def-debug-command "DOWN" ()
  (let ((next (di:frame-down *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Bottom of stack.")))))

(def-debug-command-alias "D" "DOWN")

(def-debug-command "TOP" ()
  (do ((prev *current-frame* lead)
       (lead (di:frame-up *current-frame*) (di:frame-up lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))

(def-debug-command "BOTTOM" ()
  (do ((prev *current-frame* lead)
       (lead (di:frame-down *current-frame*) (di:frame-down lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))


(def-debug-command-alias "B" "BOTTOM")

(def-debug-command "FRAME" (&optional
			    (n (read-prompting-maybe "Frame number: ")))
  (let ((current (di:frame-number *current-frame*)))
    (cond ((= n current)
	   (princ "You are here."))
	  ((> n current)
	   (print-frame-call
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)

(def-debug-command "ABORT" ()
  ;; There's always at least one abort restart due to the top-level one.
  (invoke-restart *debug-abort*))
		       (lead (di:frame-down *current-frame*)
			     (di:frame-down lead)))
		      ((null lead)
		       (princ "Bottom of stack encountered.")
		       prev)
      (write-string "Restart number: ")
		      (return prev))))))
	  (t
    (invoke-restart-interactively (nth num *debug-restarts*))))
;;;
;;; In and Out commands.
;;;

(def-debug-command "QUIT" ()
  (throw 'lisp::top-level-catcher nil))

(def-debug-command "GO" ()
  (continue)
  (error "No restart named continue."))

(def-debug-command "RESTART" ()
  (let ((num (read-if-available :prompt)))
    (when (eq num :prompt)
      (show-restarts *debug-restarts*)
      (write-string "Restart: ")
      (force-output)
      (setf num (read *standard-input*)))
    (let ((restart (typecase num
		     (unsigned-byte
		      (nth num *debug-restarts*))
		     (symbol
		      (find num *debug-restarts* :key #'restart-name
			    :test #'(lambda (sym1 sym2)
				      (string= (symbol-name sym1)
					       (symbol-name sym2)))))
		     (t
		      (format t "~S is invalid as a restart name.~%" num)
		      (return-from restart-debug-command nil)))))
      (if restart
	  (invoke-restart-interactively restart)
	  (princ "No such restart.")))))


;;;
;;; Information commands.
;;;
 
(defvar *help-line-scroll-count* 20
  "This controls how many lines the debugger's help command prints before
   printing a prompting line to continue with output.")

(def-debug-command "HELP" ()
  (let* ((end -1)
	 (len (length debug-help-string))
	 (len-1 (1- len)))
    (loop
  (print-frame-call *current-frame* nil nil))
	    (count *help-line-scroll-count*))
	(loop
	  (setf end (position #\newline debug-help-string :start (1+ end)))
	  (cond ((or (not end) (= end len-1))
		 (setf end len)
		 (return))
		((or (zerop (decf count)) (= end len))
		 (return))))
	(write-string debug-help-string *standard-output*
		      :start start :end end))
      (when (= end len) (return))
      (format t "~%[RETURN FOR MORE, Q TO QUIT HELP TEXT]: ")
      (force-output)
      (let ((res (read-line)))
	(when (or (string= res "q") (string= res "Q"))
	  (return))))))

(def-debug-command-alias "?" "HELP")

(def-debug-command "ERROR" ()
  (format t "~A~%" *debug-condition*)
  (show-restarts *debug-restarts*))

(def-debug-command "BACKTRACE" ()
  (backtrace (read-if-available most-positive-fixnum)))

(def-debug-command "PRINT" ()
  (print-frame-call *current-frame*))

(def-debug-command-alias "P" "PRINT")

(def-debug-command "VPRINT" ()
  (print-frame-call *current-frame* :print-level nil :print-length nil
		    :verbosity (read-if-available 2)))

(def-debug-command-alias "PP" "VPRINT")

  (print-frame-source-form *current-frame* (read-if-available 0)))
	      (*print-length* (or *debug-print-length* *print-level*))
	      (*standard-output* *debug-io*)
  (print-frame-source-form *current-frame* (read-if-available 0) t))
			d-fun

;;; PRINT-FRAME-SOURCE-FORM -- Internal.
	    (setf any-p t)
(defun print-frame-source-form (frame context &optional verbose)
  (let* ((location (maybe-block-start-location (di:frame-code-location frame)))
	      (format t "~A~:[#~D~;~*~]  =  ~S~%"
		      (di:debug-variable-name v)
		      (zerop (di:debug-variable-id v))
		      (di:debug-variable-id v)
		      (di:debug-variable-value v *current-frame*))))

	  (cond
	   ((not any-p)
	    (format t "No local variables ~@[starting with ~A ~]~
	               in function."
		    prefix))
	   ((not any-valid-p)
	    (format t "All variables ~@[starting with ~A ~]currently ~
	               have invalid values."
		    prefix))))
	(write-line "No variable information available."))))

(def-debug-command-alias "L" "LIST-LOCALS")

(def-debug-command "SOURCE" ()
  (fresh-line)
  (print-code-location-source-form (di:frame-code-location *current-frame*)
				   (read-if-available 0)))

(def-debug-command "VSOURCE" ()
  (fresh-line)
  (print-code-location-source-form (di:frame-code-location *current-frame*)
				   (read-if-available 0)
				   t))


;;;; Source location printing:

;;; We cache a stream to the last valid file debug source so that we won't have
;;; to repeatedly open the file.
;;;
(defvar *cached-debug-source* nil)
(declaim (type (or di:debug-source null) *cached-debug-source*))
(defvar *cached-source-stream* nil)
(declaim (type (or stream null) *cached-source-stream*))

(pushnew #'(lambda ()
	     (setq *cached-debug-source* nil *cached-source-stream* nil))
	 ext:*before-save-initializations*)


;;; We also cache the last top-level form that we printed a source for so that
;;; we don't have to do repeated reads and calls to FORM-NUMBER-TRANSLATIONS.
;;;
(defvar *cached-top-level-form-offset* nil)
(declaim (type (or kernel:index null) *cached-top-level-form-offset*))
(defvar *cached-top-level-form*)
(defvar *cached-form-number-translations*)


;;; GET-TOP-LEVEL-FORM  --  Internal
;;;
;;;    Given a code location, return the associated form-number translations
;;; and the actual top-level form.  We check our cache --- if there is a miss,
;;; we dispatch on the kind of the debug source.
;;;
(defun get-top-level-form (location)
  (let ((d-source (di:code-location-debug-source location)))
    (if (and (eq d-source *cached-debug-source*)
	     (eql (di:code-location-top-level-form-offset location)
		  *cached-top-level-form-offset*))
	(values *cached-form-number-translations* *cached-top-level-form*)
	(let* ((offset (di:code-location-top-level-form-offset location))
	       (res
		(ecase (di:debug-source-from d-source)
		  (:file (get-file-top-level-form location))
		  ((:lisp :stream)
		   (svref (di:debug-source-name d-source) offset)))))
	  (setq *cached-top-level-form-offset* offset)
	  (values (setq *cached-form-number-translations*
			(di:form-number-translations res offset))
		  (setq *cached-top-level-form* res))))))

		  (setf *possible-breakpoints*
			(possible-breakpoints
			 *default-breakpoint-debug-function*))))))
	   (setup-function-start ()
	     (let ((code-loc (di:debug-function-start-location place)))
	       (setf bp (di:make-breakpoint #'main-hook-function place
					    :kind :function-start))
	       (setf break (di:preprocess-for-eval break code-loc))
	       (setf condition (di:preprocess-for-eval condition code-loc))
	       (dolist (form print)
					(declare (ignore dummy)) ,condition)
				     'function))
	     (dolist (form print)
	       (push (cons
		      (coerce `(lambda (dummy)
				 (declare (ignore dummy)) ,form) 'function)
		      form)
		     print-functions)))
	   (setup-code-location ()
	     (setf place (nth index *possible-breakpoints*))
		    (di:frame-code-location *current-frame*)))
	     (dolist (form print)
	       (push (cons
		      (di:preprocess-for-eval form place)
		      form)
		     print-functions))
	     (setf break (di:preprocess-for-eval break place))
	     (setf condition (di:preprocess-for-eval condition place))))
      (set-vars-from-command-line (get-command-line))
      (cond
       ((or (eq index :start) (eq index :s))
	(setup-function-start))
       ((or (eq index :end) (eq index :e))
	(setup-function-end))
       (t
	(setup-code-location)))
      (di:activate-breakpoint bp)
      (let* ((new-bp-info (create-breakpoint-info place bp index
						  :break break
						  :print print-functions
						  :condition condition))
	     (old-bp-info (location-in-list new-bp-info *breakpoints*)))
	(when old-bp-info
	  (di:deactivate-breakpoint (breakpoint-info-breakpoint old-bp-info))
	  (setf *breakpoints* (remove old-bp-info *breakpoints*))
	  (format t "Note: previous breakpoint removed.~%"))
	(push new-bp-info *breakpoints*))
      (print-breakpoint-info (first *breakpoints*))
      (format t "~&Added."))))

(def-debug-command-alias "BP" "BREAKPOINT")

;;; list all breakpoints set
(def-debug-command "LIST-BREAKPOINTS" ()
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (dolist (info *breakpoints*)
    (print-breakpoint-info info)))

(def-debug-command-alias "LB" "LIST-BREAKPOINTS")
(def-debug-command-alias "LBP" "LIST-BREAKPOINTS")

;;; remove breakpoint n or all if none given
(def-debug-command "DELETE-BREAKPOINT" ()
  (let* ((index (read-if-available nil))
	 (bp-info
	  (find index *breakpoints* :key #'breakpoint-info-breakpoint-number)))
    (cond (bp-info
	   (di:delete-breakpoint (breakpoint-info-breakpoint bp-info))
	   (setf *breakpoints* (remove bp-info *breakpoints*))
	   (format t "Breakpoint ~S removed.~%" index))
	  (index (format t "Breakpoint doesn't exist."))
	  (t
	   (dolist (ele *breakpoints*)
	     (di:delete-breakpoint (breakpoint-info-breakpoint ele)))
	   (setf *breakpoints* nil)
	   (format t "All breakpoints deleted.~%")))))

(def-debug-command-alias "DBP" "DELETE-BREAKPOINT")


;;;
;;; Miscellaneous commands.
;;;

(def-debug-command "FLUSH-ERRORS" ()
  (if (setf *flush-debug-errors* (not *flush-debug-errors*))
      (write-line "Errors now flushed.")
      (write-line "Errors now create nested debug levels.")))


(def-debug-command "DESCRIBE" ()
  (let* ((curloc (di:frame-code-location *current-frame*))
	 (debug-fun (di:code-location-debug-function curloc))
	 (function (di:debug-function-function debug-fun)))
    (if function
	(describe function)
	(format t "Can't figure out the function for this frame."))))


;;;
;;; Editor commands.
;;;

(def-debug-command "EDIT-SOURCE" ()
  (unless (typep *terminal-io* 'ed::ts-stream)
    (error "The debugger's EDIT-SOURCE command only works in slave Lisps ~
	    connected to a Hemlock editor."))
  (let* ((wire (ed::ts-stream-wire *terminal-io*))
	 (location (maybe-block-start-location
		    (di:frame-code-location *current-frame*)))
	 (d-source (di:code-location-debug-source location))
	 (name (di:debug-source-name d-source)))
    (ecase (di:debug-source-from d-source)
      (:file
       (let* ((tlf-offset (di:code-location-top-level-form-offset location))
	      (local-tlf-offset (- tlf-offset
				   (di:debug-source-root-number d-source)))
	      (char-offset (aref (or (di:debug-source-start-positions d-source)
				     (error "No start positions map."))
				 local-tlf-offset)))
	 (wire:remote wire
	   (ed::edit-source-location (namestring name)
				     (di:debug-source-created d-source)
				     tlf-offset local-tlf-offset char-offset
				     (di:code-location-form-number location)))
	 (wire:wire-force-output wire)))
      ((:lisp :stream)
       (wire:remote wire
	 (ed::cannot-edit-source-location))
       (wire:wire-force-output wire)))))



;;;; Debug loop command utilities.

(defun read-prompting-maybe (prompt &optional (in *standard-input*)
				    (out *standard-output*))
  (unless (ext:listen-skip-whitespace in)
    (princ prompt out)
    (force-output out))
  (read in))

(defun read-if-available (default &optional (stream *standard-input*))
  (if (ext:listen-skip-whitespace stream)
      (read stream)
      default))
