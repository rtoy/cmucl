;;; -*- Mode: Lisp; Package: Debug; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/debug.lisp,v 1.14 1991/01/31 02:08:21 wlott Exp $
;;;
;;; CMU Common Lisp Debugger.  This is a very basic command-line oriented
;;; debugger.
;;;
;;; Written by Bill Chiles.
;;;

(in-package "DEBUG")

(export '(internal-debug *in-the-debugger* backtrace *flush-debug-errors*
	  *debug-print-level* *debug-print-length* *debug-prompt*
	  
	  *auto-eval-in-frame* var arg))


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
	    (funcall *debug-prompt*)
	    (let* ((exp (read))
		   (cmd-fun (debug-command-p exp))
		   ;; Must bind level for restart function created by
		   ;; WITH-SIMPLE-RESTART.
		   (level *debug-command-level*))
      (clear-input *debug-io*)
		(if cmd-fun
		    (funcall cmd-fun)
		    (debug-eval-print exp))))))))))
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
   information."
  (let* ((temp (etypecase name
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
	   (di:debug-variable-value (car vars) *current-frame*))
	  (t
	   ;; Since we have more than one, first see if we have any variables
	   ;; that exactly match the specification.
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
	       (di:debug-variable-value (car vars) *current-frame*))
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
		 (di:debug-variable-value v *current-frame*)))
	      (t
	       (error "Specify variable ID to disambiguate ~S.  Use one of ~S."
		      name (mapcar #'di:debug-variable-id vars)))))))))
			   id (mapcar #'di:debug-variable-id vars)))
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
;;; Interface to *debug-commands*.
;;; 
(defmacro def-debug-command (name &rest body)
    (dolist (ele args (error "Argument specification out of range -- ~S." n))
      (lambda-list-element-dispatch ele
      (defun ,fun-name () ,@body)
      (push (cons ,name #',fun-name) *debug-commands*)
				      (di:frame-code-location *current-frame*)
		   (error "Unused rest-arg before n'th argument.")
(defun debug-command-p (form)
  (and (symbolp form)
       (cdr (assoc (symbol-name form) *debug-commands* :test #'string=))))
(defun debug-command-p (form &optional other-commands)
      (let* ((name
	  (mapc #'match-command other-commands))
	;;
	;; Return the right value.
(def-debug-command "U"
	      ((= (length res) 1)
    (if next
	(print-frame-call (setf *current-frame* next))
	(princ "Top of stack."))))

(def-debug-command "D"
;;; Returns a list of debug commands (in the same format as *debug-commands*)
    (if next
	(print-frame-call (setf *current-frame* next))
	(princ "Bottom of stack."))))
(defun make-restart-commands (&optional (restarts *debug-restarts*))
(def-debug-command "T"
  (print-frame-call
   (setf *current-frame*
	 (do ((prev *current-frame* lead)
	      (lead (di:frame-up *current-frame*) (di:frame-up lead)))
	     ((null lead) prev)))))
	    (push (cons (format nil "~d" num) restart-fun) commands))))
(def-debug-command "B"
  (print-frame-call
   (setf *current-frame*
	 (do ((prev *current-frame* lead)
	      (lead (di:frame-down *current-frame*) (di:frame-down lead)))
	     ((null lead) prev)))))

(def-debug-command "F"
  (let ((n (read-prompting-maybe "Frame number: "))
	(current (di:frame-number *current-frame*)))
  
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


(def-debug-command "FRAME" (&optional
			    (n (read-prompting-maybe "Frame number: ")))
  (let ((current (di:frame-number *current-frame*)))
(def-debug-command "Q"
	   (princ "You are here."))
	  ((> n current)
(def-debug-command "GO"
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)

(def-debug-command "ABORT"
  ;; There's always at least one abort restart due to the top-level one.
  (invoke-restart *debug-abort*))
		       (lead (di:frame-down *current-frame*)
(def-debug-command "RESTART"
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
(defvar *help-line-scroll-count* 20)
  (continue)
(def-debug-command "H"

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
      (format t "~%Q for quit: ")
		      (format t "~S is invalid as a restart name.~%" num)
		      (return-from restart-debug-command nil)))))
      (if restart
	  (invoke-restart-interactively restart)
	  (princ "No such restart.")))))
(def-debug-command "ERROR"
;;; Information commands.
;;;
 
(def-debug-command "BACKTRACE"
  "This controls how many lines the debugger's help command prints before
   printing a prompting line to continue with output.")
(def-debug-command "P"
(def-debug-command "HELP" ()
  (let* ((end -1)
(def-debug-command "PP"
  (print-frame-call *current-frame* nil nil))
	    (count *help-line-scroll-count*))
(def-debug-command "L"
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

	    (format t "All variables ~@[starting with ~A~ ]currently ~
  (print-frame-call *current-frame* :print-level nil :print-length nil
		    :verbosity (read-if-available 2)))

(def-debug-command-alias "PP" "VPRINT")
(def-debug-command "SOURCE"
  (print-frame-source-form *current-frame* (read-if-available 0)))
	      (*print-length* (or *debug-print-length* *print-level*))
(def-debug-command "VSOURCE"
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
	 (let* ((tlf-offset (di:code-location-top-level-form-offset
			     location))
		(char-offset (aref (or (di:debug-source-start-positions
					d-source)
;;; We also cache the last top-level form that we printed a source for so that
				   tlf-offset)))
;;;
(defvar *cached-top-level-form-offset* nil)
(declaim (type (or kernel:index null) *cached-top-level-form-offset*))
(defvar *cached-top-level-form*)
(defvar *cached-form-number-translations*)


;;; GET-TOP-LEVEL-FORM  --  Internal
;;;
	       (dotimes (i tlf-offset)
		 (read f))))
;;;
(defun get-top-level-form (location)
  (let ((d-source (di:code-location-debug-source location)))
    (if (and (eq d-source *cached-debug-source*)
	     (eql (di:code-location-top-level-form-offset location)
		  *cached-top-level-form-offset*))
	(values *cached-form-number-translations* *cached-top-level-form*)
  (let ((translations (di:form-number-translations
		       tlf tlf-offset))
	(*print-level* (if verbose
	       (res
		(ecase (di:debug-source-from d-source)
		  (:file (get-file-top-level-form location))
		  ((:lisp :stream)
		   (svref (di:debug-source-name d-source) offset)))))
	  (setq *cached-top-level-form-offset* offset)
	  (values (setq *cached-form-number-translations*
	    (svref translations
		  (setq *cached-top-level-form* res))))))

		  (setf *possible-breakpoints*
			(possible-breakpoints
			 *default-breakpoint-debug-function*))))))
	   (setup-function-start ()
	     (let ((code-loc (di:debug-function-start-location place)))
(def-debug-command "FLUSH"
					    :kind :function-start))
	       (setf break (di:preprocess-for-eval break code-loc))
      (write-line "Errors now not flushed.")))
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
