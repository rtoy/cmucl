;;; -*- Mode: Lisp; Package: Debug; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Spice Lisp Debugger.
;;;
;;; Written by Steve Handerson
;;; Pages 6 through 9 rewritten by Bill Chiles.
;;;
;;; **********************************************************************
;;;
(in-package "DEBUG" :use '("LISP" "SYSTEM"))


(export '(internal-debug *flush-debug-errors* backtrace debug-function
	  show-all debug-return local show hide argument pc
	  function-name hide-defaults *debug-print-level*
	  *debug-print-length* *debug-hidden-functions*))



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
(defvar *inside-debugger-p* nil
  "This is T while evaluating expressions in the debugger.")


(defparameter *debug-print-length* 5
  "*PRINT-LENGTH* is bound to this value when debug prints a function call.  If
  null, use *PRINT-LENGTH*.")

(defvar *in-the-debugger* nil
  "When true, the LIST-LOCATIONS command only displays block start locations.
  "The default contents of *debug-prompt*."
   Otherwise, all locations are displayed.")

  "If true, list the code location type in the LIST-LOCATIONS command.")

;;; A list of the types of code-locations that should not be stepped to and
;;; should not be listed when listing breakpoints.
;;;
  "A lambda of no args that prints the debugger prompt on *debug-io*.")

;;; Code locations of the possible breakpoints
;;;
Prompt is <stack-level>':'<frame-number>(<command-level>*']').
Frames look like calls, C signifying a catch frame.
Expressions get evaluated in the frame's lexical environment,
  setting * and friends like the top level read-eval-print loop.
Debug commands do not affect * and friends.
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
  F  go to numbered frame (prompts if not given).
  S  search for a specified function (prompts), 
     an optional number of times (does not prompt).
  R  searches up the stack, optional times.
(declaim (type integer *number-of-steps*))
;;;
(defvar *default-breakpoint-debug-function* nil)
  ?              prints all kinds of groovy things.
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
(proclaim '(inline pointer+ stack-ref valid-env-p cstack-pointer-valid-p))
	  (di:activate-breakpoint bp)
(defun pointer+ (x y)
  (%primitive sap+ x (ash y 2)))
	     (t ,other)))))
(defun stack-ref (s n)
  (%primitive read-control-stack (pointer+ s n)))

(defun escape-reg (f n)
  (stack-ref f (+ n %escape-frame-general-register-start-slot)))

(defun valid-env-p (env)
  (and (functionp env)
       (eql (%primitive get-vector-subtype env)
	    %function-constants-subtype)))

(defun cstack-pointer-valid-p (x)
  (and (%primitive pointer< x (%primitive current-sp))
       (not (%primitive pointer< x
			(%primitive make-immediate-type 0
				    %control-stack-type)))))

(defun check-valid (x)
  (unless (cstack-pointer-valid-p x)
    (error "Invalid control stack pointer."))
  x)

(defun print-code-and-stuff (env pc)
  (let* ((code (%primitive header-ref env %function-code-slot))
	 (code-int (%primitive make-fixnum code)))
    (format t "~A, Code = #x~X, PC = ~D"
	    (%primitive header-ref env %function-name-slot)
	    (logior code-int (ash %code-type 27))
	    (- (%primitive make-fixnum pc) code-int))))

;;; Backtrace prints a history of calls on the stack.

(defun backtrace (&optional (frames most-positive-fixnum)
			    (*standard-output* *debug-io*))
  "Show a listing of the call stack going down from the current frame.  Frames
  is how many frames to show."
  (do ((callee (%primitive current-fp)
	       (stack-ref callee c::old-fp-save-offset))
       (n 0 (1+ n)))
      ((or (not (cstack-pointer-valid-p callee))
	   (>= n frames))
       (values))
    (let* ((caller (stack-ref callee c::old-fp-save-offset))
	   (pc (stack-ref callee c::return-pc-save-offset)))
      (unless (cstack-pointer-valid-p caller)
	(return (values)))
      (let ((env (stack-ref caller c::env-save-offset)))
	(cond 
	 ((eql env 0)
	  (let ((env (escape-reg caller c::env-offset)))
	    (cond ((eql (%primitive get-type env) %trap-type)
		   (format t "~%<undefined> ~S"
			   (escape-reg caller c::call-name-offset))
		   (setq callee
			 (check-valid
			  (escape-reg caller c::old-fp-offset))))
		  ((valid-env-p env)
		   (format t "~%<escape frame> ")
		   (print-code-and-stuff
		    env
		    (escape-reg caller c::return-pc-offset))
		   (setq callee
			 (check-valid
			  (stack-ref callee c::old-fp-save-offset))))
		  (t
		   (error "Escaping frame ENV invalid?")))))
	 ((valid-env-p env)
	  (terpri)
	  (print-code-and-stuff env pc))
	 (t
	  (format t "~%<invalid frame>")))))))

    (make-unprintable-object "unavailable-arg")))
;;;; DEBUG
;;; PRINT-FRAME-CALL -- Interface
;;;
;;; This prints a representation of the function call causing frame to exist.
;;; Verbosity indicates the level of information to output; zero indicates just
   which causes the standard debugger to execute.")
(defun print-frame-call (frame &key (print-length *print-length*)
			       (number nil))
  (let ((*print-length* (or *debug-print-length* print-length))
(defvar *debug-abort*)
	(*print-level* (or *debug-print-level* print-level)))
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
   which causes the standard debugger to execute.  The system passes the value
   of this variable to the function because it binds *debugger-hook* to nil
   around the invocation.")
    (do ((p restarts (cdr p))
	 (i 0 (1+ i)))
	((endp p))
      (format s "~&  ~D: ~A~%" i (car p)))))
	 ;; Rebind some printer control variables.
;;; INTERNAL-DEBUG calls the debug loop.  This is used in DEBUG and
;;; CONDITIONS::ERROR-ERROR.
	 (*print-readably* nil)
;;; SHOW-RESTARTS -- Internal.
  (let ((*in-the-debugger* T)
	(*read-suppress* NIL))
  (when restarts
    (format s "~&Restarts:~%")
    (let ((count 0)
	  (names-used '(nil))
	  (max-name-len 0))
      (dolist (restart restarts)

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
  (if (symbolp form)
      (cdr (assoc (symbol-name form) *debug-commands* :test #'string=))))
(defun debug-command-p (form &optional other-commands)
	;; Return the right value.
(defun make-restart-commands (&optional (restarts *debug-restarts*))
(def-debug-command "FRAME" (&optional
;;; 
(def-debug-command "Q"
	   (princ "You are here."))
	  ((> n current)
(def-debug-command "GO"
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)

(def-debug-command "PUSH"
  (invoke-debugger *debug-condition*))

(def-debug-command "ABORT"
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

;;;
;;; 
(def-debug-command "H"
  (princ debug-help-string))
	  (princ "No such restart.")))))
(def-debug-command "ERROR"
;;; Information commands.
;;;
 
;;; BACKTRACE-DEBUG-COMMAND binds *inside-debugger-p*, so BACKTRACE will
;;; not reparse the stack.  *inside-debugger-p* is only bound to non-nil
;;; when doing evaluations in the debug loop.
;;; 
(def-debug-command "BACKTRACE"
  (let ((*inside-debugger-p* t))
    (backtrace (read-if-available most-positive-fixnum))))
   printing a prompting line to continue with output.")
  (let* ((end -1)
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
  (if (not (listen-skip-whitespace in))
      (princ prompt out))

;;; list all breakpoints set
(def-debug-command "LIST-BREAKPOINTS" ()
  (if (listen-skip-whitespace stream)
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (dolist (info *breakpoints*)



;;;; Debug-Loop.

(defun debug-loop ()
  (let ((*debug-command-level* (1+ *debug-command-level*)))
    (loop
     (catch 'debug-loop-catcher
       (handler-bind ((error #'(lambda (condition)
				 (when *flush-debug-errors*
				   (clear-input *debug-io*)
				   (princ condition)
				   (format t "~&Error flushed ...")
				   (throw 'debug-loop-catcher nil))
				 (invoke-debugger condition))))
	 (funcall *debug-prompt*)
	 (let* ((exp (read))
		(cmd-fun (debug-command-p exp))
		;; Must bind level for restart function created for this abort.
		(level *debug-command-level*))
	   (with-simple-restart (abort "Return to debug level ~D." level)
	     (if cmd-fun
		 (funcall cmd-fun)
		 (debug-eval-print exp)))))))))

(defun debug-eval-print (exp)
  (setq +++ ++ ++ + + - - exp)
  (let* ((values (multiple-value-list (eval -)))
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
