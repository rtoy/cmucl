;;; -*- Mode: Lisp; Package: Debug; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; CMU Common Lisp Debugger.  This is a very basic command-line oriented
;;; debugger.
;;;
;;; Written by Bill Chiles.
;;;

(in-package "DEBUG")

(export '(internal-debug *in-the-debugger* backtrace *flush-debug-errors*
	  *debug-print-level* *debug-print-length* *debug-prompt*

	  var arg))


(in-package "LISP")
(export '(invoke-debugger *debugger-hook*))

(in-package "DEBUG")

;;;
;;; Used to communicate to debug-loop that we are at a step breakpoint.
;;;
(define-condition step-condition (simple-condition))
  "*Print-level* is bound to this value when debug prints a function call.")
;;;; Variables, parameters, and constants.

  "*Print-length* is bound to this value when debug prints a function call.")
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
  PUSH     rebinds things in another command level.  Good for hide/show.
  ABORT    returns to the previous abort restart case.
;;;
(defvar *step-breakpoints* nil)  
;;;
(defvar *number-of-steps* 1)
(declaim (type integer *number-of-steps*))
;;;
(defvar *default-breakpoint-debug-function* nil)
  L              lists locals in current function.
  P, PP          displays current function call.  

Functions/macros for your enjoyment:
 (DEBUG:DEBUG-RETURN expression [frame])  returns with values from an active frame.
 (DEBUG:ARGUMENT n [frame])              shows the nth  supplied argument.
 (DEBUG:PC [frame])                 shows the next pc to be executed.
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
  (let ((*print-length* *debug-print-length*)
	(*print-level* *debug-print-level*))
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

;;; LAMBDA-LIST-ELEMENT-DISPATCH -- Internal.

;;;
;;; This is a convenient way to express what to do for each type of lambda-list
;;; element.
;;;
(defmacro lambda-list-element-dispatch (element &key required optional rest
						keyword deleted)
  `(etypecase ,element
(defun print-frame-call (frame &optional
			       (*print-length* *debug-print-length*)
			       (*print-level* *debug-print-level*)
			       (verbosity 1))
  (ecase verbosity
    (0 (print frame))
    (1 (handler-case
	   (let* ((d-fun (di:frame-debug-function frame))
		  (loc (di:frame-code-location frame))
		  (args (di:debug-function-lambda-list d-fun)))
	     (terpri)
	     (write-char #\()
	     (prin1 (di:debug-function-name d-fun))
	     (dolist (ele args)
	       (write-char #\space)
	       (lambda-list-element-dispatch ele
		 :required ((print-frame-call-arg ele loc frame))
		 :optional ((print-frame-call-arg (second ele) loc frame))
		 :keyword ((prin1 (second ele))
			   (write-char #\space)
			   (print-frame-call-arg (third ele) loc frame))
		 :deleted ((print-frame-call-arg ele loc frame))))
	     (write-char #\))
	     (when (di:debug-function-kind d-fun)
	       (write-string " [")
	       (prin1 (di:debug-function-kind d-fun))
	       (write-char #\])))
	 (di:lambda-list-unavailable ()
	  (let ((d-fun (di:frame-debug-function frame)))
	    (format t "(~S <lambda-list-unavailable>)) ~S)"
		    (di:debug-function-name d-fun)
		    (di:debug-function-kind d-fun))))))
    ((2 3 4 5))))
	     (t ,other)))))
(defun print-frame-call-arg (var location frame)
  (cond ((eq var :deleted)
	 (write-string "<unused-arg>"))
	((eq (di:debug-variable-validity var location) :valid)
	 (prin1 (di:debug-variable-value var frame)))
	(t (write-string "<unavailable-arg>"))))



;;;; ROBS-BACKTRACE.

(defun robs-backtrace (&optional (frames most-positive-fixnum)
			    (*standard-output* *debug-io*))
  "Show a listing of the call stack going down from the current frame.  Frames
  is how many frames to show."
  (do ((callee (system:%primitive current-fp)
	       (di::stack-ref callee c::old-fp-save-offset))
       (n 0 (1+ n)))
      ((or (not (di::cstack-pointer-valid-p callee))
	   (>= n frames))
       (values))
    (let* ((caller (di::stack-ref callee c::old-fp-save-offset))
	   (pc (di::stack-ref callee c::return-pc-save-offset)))
      (unless (di::cstack-pointer-valid-p caller)
	(return (values)))
      (let ((env (di::stack-ref caller c::env-save-offset)))
	(cond 
	 ((eql env 0)
	  (let ((env (di::escape-register caller c::env-offset)))
	    (cond ((eql (system:%primitive get-type env) system:%trap-type)
		   (format t "~%<undefined> ~S"
			   (di::escape-register caller c::call-name-offset))
		   (setq callee
			 (check-valid
			  (di::escape-register caller c::old-fp-offset))))
		  ((di::env-valid-p env)
		   (format t "~%<escape frame> ")
		   (print-code-and-stuff
		    env
		    (di::escape-register caller c::return-pc-offset))
		   (setq callee
			 (check-valid
			  (di::stack-ref callee c::old-fp-save-offset))))
		  (t
		   (error "Escaping frame ENV invalid?")))))
	 ((di::env-valid-p env)
	  (terpri)
	  (print-code-and-stuff env pc))
	 (t
	  (format t "~%<invalid frame>")))))))

(defun print-code-and-stuff (env pc)
  (let* ((code (system:%primitive header-ref env system:%function-code-slot))
	 (code-int (system:%primitive make-fixnum code)))
    (format t "~A, Code = #x~X, PC = ~D"
	    (system:%primitive header-ref env system:%function-name-slot)
	    (logior code-int (ash system:%code-type 27))
	    (- (system:%primitive make-fixnum pc) code-int))))

(defun check-valid (x)
  (unless (di::cstack-pointer-valid-p x)
    (error "Invalid control stack pointer."))
  x)
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
	    ;; WITH-SIMPLE-RESTART.
	    (let ((level *debug-command-level*)
  (let* ((values (multiple-value-list (eval -)))
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

;;; VARS -- Public.
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
  (let* ((vars (etypecase name
		 (symbol (di:debug-function-symbol-variables
			  (di:frame-debug-function *current-frame*)
			  name))
		 (simple-string (di:ambiguous-debug-variables
				 (di:frame-debug-function *current-frame*)
				 name))))
	 (location (di:frame-code-location *current-frame*)))
    (declare (list vars))
    (setf vars
	  (remove-if-not #'(lambda (v)
			     (eq (di:debug-variable-validity v location) :valid))
			 vars))
    (cond ((null vars)
	   (error "No known valid variables match ~S." name))
	  ((= (length vars) 1)
	   (di:debug-variable-value (car vars) *current-frame*))
	  ((find-if-not #'(lambda (v)
			    (string= (di:debug-variable-name v)
				     (di:debug-variable-name (car vars))))
			vars)
	   (error "Specification ambiguous:~%~{   ~A~%~}"
		  (mapcar #'di:debug-variable-name
			  (delete-duplicates vars
					     :test #'string=
					     :key #'di:debug-variable-name))))
	  (id-supplied
	   (let ((v (find id vars :key #'di:debug-variable-id)))
	     (unless v
	       (error "Invalid variable ID, ~D, should have been one of ~S."
		      id (mapcar #'di:debug-variable-id vars)))
	     (di:debug-variable-value v *current-frame*)))
	  (t
	   (error "Specify variable id to disambiguate ~S.  Use one of ~S."
		   name (mapcar #'di:debug-variable-id vars))))))
			   id (mapcar #'di:debug-variable-id vars)))
		      '(di:debug-variable-value v *current-frame*))
		     (:set
		      `(setf (di:debug-variable-value v *current-frame*)
			     ,value-var)))))
   argument in a frame's default printed representation.  Keyword/value pairs
   count as one."
  (let* ((lambda-list (handler-case (di:debug-function-lambda-list
				     (di:frame-debug-function *current-frame*))
			(di:lambda-list-unavailable ()
			  (error "No argument values are available."))))
	 (rest-pos (position-if #'(lambda (x)
				    (and (consp x) (eq (car x) :rest)))
				lambda-list))
	 (arg (car (nthcdr (if (and rest-pos (<= rest-pos n))
			       (1+ n)
			       n)
			   lambda-list))))
    (flet ((variable-value-if-valid (var frame)
	     (cond ((eq var :deleted)
		    (error "Unused arguments have no values."))
		   ((eq (di:debug-variable-validity
			 var (di:frame-code-location frame))
			:valid)
		    (di:debug-variable-value var frame))
		   (t (error "Invalid argument value.")))))
      (lambda-list-element-dispatch arg
	:required ((variable-value-if-valid arg *current-frame*))
	:optional ((variable-value-if-valid (second arg) *current-frame*))
	:keyword ((variable-value-if-valid (third arg) *current-frame*))
	:deleted ((variable-value-if-valid arg *current-frame*))))))

	  (di:debug-variable-value var *current-frame*)
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


(def-debug-command "FRAME" (&optional
			    (n (read-prompting-maybe "Frame number: ")))
  (let ((current (di:frame-number *current-frame*)))
(def-debug-command "Q"
	   (princ "You are here."))
	  ((> n current)
(def-debug-command "GO"
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)

(def-debug-command "PUSH"
  (invoke-debugger *debug-condition*))

(def-debug-command "ABORT"
  ;; There's always at least one abort restart due to the top-level one.
  (invoke-restart *debug-abort*))
		       (lead (di:frame-down *current-frame*)
(def-debug-command "RESTART"
		      ((null lead)
		       (princ "Bottom of stack encountered.")
		       prev)
      (write-string "Restart number: ")
	  (t
    (invoke-restart-interactively (nth num *debug-restarts*))))
;;;
;;; In and Out commands.
;;;

(def-debug-command "QUIT" ()
(def-debug-command "H"
  (princ debug-help-string))
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
  (let ((*print-level* *debug-print-level*)
	(*print-length* *debug-print-length*)
	(*standard-output* *debug-io*)
	(location (di:frame-code-location *current-frame*))
	(any-p nil))
    (di:do-debug-function-variables (v (di:frame-debug-function *current-frame*))
      (setf any-p t)
      (if (eq (di:debug-variable-validity v location) :valid)
	  (format t "~A~:[#~D~;~*~]  =  ~S~%"
		  (di:debug-variable-name v)
		  (zerop (di:debug-variable-id v))
		  (di:debug-variable-id v)
		  (di:debug-variable-value v *current-frame*))
	  #|(format t "~A has an invalid value currently.~%"
		  (di:debug-variable-name v))|#))
    (unless any-p (write-line "No variable information available."))))
(def-debug-command-alias "PP" "VPRINT")
(def-debug-command "SOURCE"
  (print-frame-source-form *current-frame* (read-if-available 0)))
	      (*print-length* (or *debug-print-length* *print-level*))
(def-debug-command "VSOURCE"
  (print-frame-source-form *current-frame* (read-if-available 0) t))
			d-fun
(defun print-frame-source-form (frame context &optional verbose)
  (let* ((location (di:frame-code-location *current-frame*))
	      (format t "~A~:[#~D~;~*~]  =  ~S~%"
		      (di:debug-variable-name v)
    (cond ((not (eq :file (di:debug-source-from d-source)))
	   (format t "~%Source did not come from a file."))
	  ((not (probe-file name))
	   (format t "~%Source file no longer exists -- ~A."
		   (namestring name)))
	  ((<= (di:debug-source-created d-source)
	       (file-write-date name))
	   (let* ((tlf-offset (di:code-location-top-level-form-offset
			       location))
		  (char-offset (aref (di:debug-source-start-positions
				      d-source)
				     tlf-offset)))
	     (with-open-file (f name)
	       (file-position f char-offset)
	       (let* ((tlf (read f))
		      (translations (di:form-number-translations
				     tlf tlf-offset))
		      (*print-level* (if verbose nil *debug-print-level*))
		      (*print-length* (if verbose nil *debug-print-length*)))
		 (print (di:source-path-context
			 tlf
			 (svref translations
				(di:code-location-form-number location))
			 context))))))
	    (t
	     (format t "~%File has been modified since compilation -- ~A."
		     (namestring name))))))
		  (setf *possible-breakpoints*
			(possible-breakpoints
			 *default-breakpoint-debug-function*))))))
	   (setup-function-start ()

(defvar *flush-debug-errors* t
  "Don't recursively call DEBUG on errors while within the debugger if non-nil.")
	     (let ((code-loc (di:debug-function-start-location place)))
(def-debug-command "FLUSH"
  (setf *flush-debug-errors* (not *flush-debug-errors*)))

	(when old-bp-info
	  (di:deactivate-breakpoint (breakpoint-info-breakpoint old-bp-info))
	  (setf *breakpoints* (remove old-bp-info *breakpoints*))
	  (format t "Note: previous breakpoint removed.~%"))
	(push new-bp-info *breakpoints*))
      (print-breakpoint-info (first *breakpoints*))
  (if (not (ext:listen-skip-whitespace in))
      (princ prompt out))

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
