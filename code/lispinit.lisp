;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/lispinit.lisp,v 1.16 1990/11/28 17:11:04 wlott Exp $
;;;
;;; Initialization stuff for CMU Common Lisp, plus some other random functions
;;; that we don't have any better place for.
;;; 
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "LISP" :use '("SYSTEM" "DEBUG"))

(export '(most-positive-fixnum most-negative-fixnum sleep
			       ++ +++ ** *** // ///))


(in-package "SYSTEM" :nicknames '("SYS"))
(export '(add-port-death-handler remove-port-death-handler sap-int
	  int-sap sap-ref-8 sap-ref-16 sap-ref-32 without-gcing
	  *in-the-compiler* compiler-version *pornography-of-death*
	  *port-receive-rights-handlers* *port-ownership-rights-handlers*
	  without-interrupts with-reply-port map-port add-port-object
	  remove-port-object make-object-set object-set-operation
	  server-message *xwindow-table* map-xwindow add-xwindow-object
	  remove-xwindow-object server-event coerce-to-key-event
	  coerce-to-motion-event coerce-to-expose-event
	  coerece-to-exposecopy-event coerce-to-focuschange-event server
	  *task-self* *task-data* *task-notify* with-interrupts
	  with-enabled-interrupts enable-interrupt ignore-interrupt
	  default-interrupt))

(in-package "EXTENSIONS")
(export '(quit *prompt* save-lisp gc-on gc-off *clx-server-displays*))

(in-package "LISP")

;;; Make the error system enable interrupts.

(defconstant most-positive-fixnum #.vm:target-most-positive-fixnum
  "The fixnum closest in value to positive infinity.")

(defconstant most-negative-fixnum #.vm:target-most-negative-fixnum
  "The fixnum closest in value to negative infinity.")


;;; Random information:

(defvar *lisp-implementation-version* "4.0(?)")

(defvar *in-the-compiler* nil
  "Bound to T while running code inside the compiler.  Macros may test this to
  see where they are being expanded.")


;;; Must be initialized in %INITIAL-FUNCTION before the DEFVAR runs...
(proclaim '(special *gc-inhibit* *already-maybe-gcing*
		    *need-to-collect-garbage* *gc-verbose*
		    *before-gc-hooks* *after-gc-hooks*
		    mach::*interrupts-enabled*
		    mach::*interrupt-pending*
		    c::*type-system-initialized*))


;;;; Random magic specials.


;;; These are filled in by Genesis.

(defvar *current-catch-block*)
(defvar *current-unwind-block*)
(defvar *free-interrupt-context-index*)



;;;; Global ports:
 
(defvar *task-self* nil
  "Port that refers to the current task.")

(defvar *task-data* nil
  "Port used to receive data for the current task.")



;;;; Reply port allocation.
;;;
;;;    We maintain a global stack of reply ports which is shared among
;;; all matchmaker interfaces, and could be used by other people as well.
;;;

#| More stuff that will probably be drastically different.

;;;    The stack is represented by a vector, and a pointer to the first
;;; free port.  The stack grows upward.  There is always at least one
;;; NIL entry in the stack after the last allocated port.
;;;
(defvar *reply-port-stack* (make-array 16)) ; Vector of reply ports.
(defvar *reply-port-pointer* 0)	; Index of first free port.
(defvar *reply-port-depth* 0)	; Dynamic depth in With-Reply-Port forms.

;;; We use this as the reply port when allocating or deallocating reply
;;; ports to get around potentially nasty interactions.  Interrupts
;;; are always off when we are doing this, so we don't have to have
;;; more than one of these, or worry about unwinding.
(defvar *allocate-reply-port* (mach:mach-task_data))

;;; Reset-Reply-Port-Stack  --  Internal
;;;
;;;    This is a before-save initialization which Nil's out the reply
;;; port stack and sets *allocate-reply-port* back to DataPort so that
;;; things initialize right at OS-Init time.
;;;
(defun reset-reply-port-stack ()
  (setf *reply-port-pointer* 0  *reply-port-depth* 0)
  (fill (the simple-vector *reply-port-stack*) nil)
  (setf *allocate-reply-port* (mach:mach-task_data)))
(pushnew 'reset-reply-port-stack *before-save-initializations*)

;;; Allocate-New-Reply-Ports  --  Internal
;;;
;;;    If we run out of reply ports, we allocate another one, possibly
;;; growing the stack.
;;;
(defun allocate-new-reply-ports ()
  (let* ((stack *reply-port-stack*)
	 (pointer *reply-port-pointer*)
	 (len (length stack)))
    (declare (simple-vector stack) (fixnum len))
    (when (eql pointer (1- len))
      (let ((new (make-array (* len 2))))
	(replace new stack :end1 len :end2 len)
	(setf stack new  *reply-port-stack* new)))
    (setf (svref stack pointer) *allocate-reply-port*)
    (let ((port (gr-call* mach:port_allocate (mach:mach-task_self))))
      (gr-call mach:port_disable (mach:mach-task_self) port)
      ;;
      ;; Nil out the allocate reply port so it isn't used for mundane purposes.
      (setf (svref stack pointer) nil)
      (setf (svref stack (1- pointer)) port)
      port)))

;;; Reallocate-Reply-Ports  --  Internal
;;;
;;;    This function is called when With-Reply-Port finds the stack pointer
;;; to be other than what it expected when it finishes.  Reallocates all
;;; of the ports on the stack from Start to *reply-port-pointer*.  We
;;; stick the *allocate-reply-port* out at *reply-port-pointer*, and
;;; bind *reply-port-depth*, so that the allocation functions are happy.
;;;
(defun reallocate-reply-ports (start)
  (let* ((pointer *reply-port-pointer*)
	 (*reply-port-depth* pointer)
	 (stack *reply-port-stack*)
	 (save-port (svref stack pointer)))
    (when (> start pointer)
      (error "More ports in use than allocated???"))
    (setf (svref stack pointer) *allocate-reply-port*)
    (do ((i start (1+ i)))
	((= i pointer)
	 (setf (svref stack pointer) save-port))
      (let ((port (svref stack i)))
	(gr-call mach:port_deallocate *task-self* port)
	(setf (svref stack i)
	      (gr-call* mach:port_allocate *task-self*))))))
|#


;;;; Server stuff:
;;;
;;;    There is a fair amount of stuff to support Matchmaker RPC servers
;;; and asynchonous message service.  RPC message service needs to be
;;; centralized since a server must receive on all ports, and there is
;;; no way for a particular server to know about all other servers
;;; in the same lisp.
;;;
;;;    The idea is that you receive the message, and then dispatch off
;;; of the port received on and the message ID received.  Ports correspond
;;; to objects that the server manages.  Message ID's correspond to the
;;; operations on the objects.  Objects are grouped into object sets, which
;;; are sets of objects having the same operations defined.
;;; 
;;;    The same mechanism is used for handling asynchronous messages.
;;;

;;;    The current implementation uses standard eq[l] hashtables for both
;;; levels of dispatching.  Special purpose data structures would be more
;;; efficient, but the ~1ms overhead will probably be lost in the noise.

;;;
;;;    Hashtable from ports to objects.  Each entry is a cons (object . set).
;;;
(defvar *port-table* (make-hash-table :test #'eql))

;;; Hashtable from windows to objects.  Each entry is a cons (object . set).
;;;
(defvar *xwindow-table* (make-hash-table :test #'eql))


(defstruct (object-set
	    (:constructor make-object-set
			  (name &optional
				(default-handler #'default-default-handler)))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Object Set ~S>" (object-set-name s)))))
  name					; Name, for descriptive purposes.
  (table (make-hash-table :test #'eq))  ; Message-ID or xevent-type --> handler fun.
  default-handler)

(setf (documentation 'make-object-set 'function)
      "Make an object set for use by a RPC/xevent server.  Name is for
      descriptive purposes only.")

;;; Default-Default-Handler  --  Internal
;;;
;;;    If no such operation defined, signal an error.
;;;
(defun default-default-handler (object)
  #+nil
  (alien-bind ((msg (server-message-msg server-message)))
    (error "No operation for ID ~D on ~S in ~S."
	   (alien-access (mach:msg-id (alien-value msg))) object
	   (car (gethash (alien-access (mach:msg-localport (alien-value msg)))
			 *port-table*))))
  (error "You lose, object: ~S" object))


;;; MAP-XWINDOW and MAP-PORT return as multiple values the object and
;;; object set mapped to by a xwindow or port in *xwindow-table* or
;;; *port-table*.
;;; 
(macrolet ((defmapper (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "MAP-" (symbol-name name)))
		      (,name)
		 ,(format nil "Return as multiple values the object and ~
		               object-set mapped to by ~A."
			  (string-downcase (symbol-name name)))
		 (let ((temp (gethash ,name ,table)))
		   (if temp
		       (values (car temp) (cdr temp))
		       (values nil nil))))))
  (defmapper port *port-table*)
  (defmapper xwindow *xwindow-table*))


;;; ADD-PORT-OBJECT and ADD-XWINDOW-OBJECT store an object/object-set pair
;;; mapped to by a port or xwindow in either *port-table* or *xwindow-table*.
;;; 
(macrolet ((def-add-object (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "ADD-" (symbol-name name)
					    "-OBJECT"))
		      (,name object object-set)
		 ,(format nil "Add a new ~A/object/object-set association."
			  (string-downcase (symbol-name name)))
		 (check-type object-set object-set)
		 (setf (gethash ,name ,table) (cons object object-set))
		 object)))
  (def-add-object port *port-table*)
  (def-add-object xwindow *xwindow-table*))


;;; REMOVE-PORT-OBJECT and REMOVE-XWINDOW-OBJECT remove a port or xwindow and
;;; its associated object/object-set pair from *port-table* or *xwindow-table*.
;;; 
(macrolet ((def-remove-object (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "REMOVE-" (symbol-name name)
					    "-OBJECT"))
		      (,name)
		 ,(format nil
			  "Remove ~A and its associated object/object-set pair."
			  (string-downcase (symbol-name name)))
		 (remhash ,name ,table))))
  (def-remove-object port *port-table*)
  (def-remove-object xwindow *xwindow-table*))


;;; Object-Set-Operation  --  Public
;;;
;;;    Look up the handler function for a given message ID.
;;;
(defun object-set-operation (object-set message-id)
  "Return the handler function in Object-Set for the operation specified by
  Message-ID, if none, NIL is returned.  The handler function is passed
  the object.  The received message is in server-Message."
  (check-type object-set object-set)
  (check-type message-id fixnum)
  (values (gethash message-id (object-set-table object-set))))

;;; %Set-Object-Set-Operation  --  Internal
;;;
;;;    The setf inverse for Object-Set-Operation.
;;;
(defun %set-object-set-operation (object-set message-id new-value)
  (check-type object-set object-set)
  (check-type message-id fixnum)
  (setf (gethash message-id (object-set-table object-set)) new-value))
;;;
(defsetf object-set-operation %set-object-set-operation
  "Sets the handler function for an object set operation.")



;;;; Emergency Message Handling:
;;;
;;; We use the same mechanism for asynchronous messages as is used for
;;; normal server messages.   The only tricky part is that we don't want
;;; some random server function being called when we really want to
;;; receive an emergency message, so we can't receive on all ports.
;;; Instead, we use MessagesWaiting to find the ports with emergency
;;; messages.

#| still more noise that will be different.

(defalien waiting-ports nil (long-words 128))

;;; Service-Emergency-Message-Interrupt  --  Internal
;;;
;;;    This is a lot like the server function, but we only receive on
;;; ports with one emergency message.  We only receive one message because
;;; the handler function might have caused any other messages to be received.
;;; When we re-enable interrupts, if any emergency messages are left, we
;;; should be interrupted again.
;;;
(defun service-emergency-message-interrupt ()
  (grab-message-loop))

;;;
;;; This object set is used for DataPort, which is the port various magical
;;; message from the kernel are received on...
(defvar *kernel-messages* (make-object-set "Kernel Messages"))

(compiler-let ((*alien-eval-when* '(compile eval)))
(defrecord port-death-msg
  (msg mach:msg #.(record-size 'mach:msg))
  (ex-port-tt pad (long-words 1))
  (ex-port (signed-byte 32) (long-words 1)))

(defoperator (server-message-port-death-msg port-death-msg)
	     ((msg server-message))
  `(alien-index (alien-value ,msg) 0 (record-size 'port-death-msg)))
); Compiler-Let


;;; *Port-Death-Handlers* is an EQ hash table of lists of functions that are
;;; called upon port death.  If a port dies that is not in the table, we print
;;; out a message on *Trace-Output* describing its death.  If
;;; *Pornography-Of-Death* is true, we don't even print that message.

(defvar *port-death-handlers* (make-hash-table :test #'eql)
  "Don't use this --- use Add-Port-Death-Handler instead.")

;;; Add-Port-Death-Handler, Remove-Port-Death-Handler  --  Public
;;;
(defun add-port-death-handler (port function)
  "Make Function a handler for port death on Port.  When the port dies,
  Function is called with the port and an argument.  See also
  Remove-Port-Death-Handler."
  (pushnew function (gethash port *port-death-handlers*))
  nil)
;;;
(defun remove-port-death-handler (port function)
  "Undoes the effect of Add-Port-Death-Handler."
  (setf (gethash port *port-death-handlers*)
	(delete function (gethash port *port-death-handlers*)))
  nil)

(setf (object-set-operation *kernel-messages* mach:notify-port-deleted)
      #'(lambda (obj)
	  (declare (ignore obj))
	  (let* ((ex-port (alien-access
			   (port-death-msg-ex-port
			    (server-message-port-death-msg server-message))))
		 (handlers (gethash ex-port *port-death-handlers*)))
	    (remhash ex-port *port-table*)
	    (remhash ex-port *port-death-handlers*)
	    (if (null handlers)
		(handle-unclaimed-port-death ex-port)
		(dolist (fun handlers) (funcall fun ex-port))))
	  mach:kern-success))

(defvar *pornography-of-death* t
  "If true, nothing is said about port deaths.")

(defun handle-unclaimed-port-death (port)
  (unless *pornography-of-death*
    (format *trace-output* "~&[Port ~S just bit the dust.]~%" port)))

;;; Port receive and ownership rights messages are handled simlarly, but
;;; by default we deallocate the port to make sure it's really dead.  This
;;; gets around problems with ports being exhausted because some servers
;;; don't really nuke the port when the deallocate the object.
;;;

(defvar *port-receive-rights-handlers* (make-hash-table :test #'eql)
  "This is a hashtable from ports to functions.  The function is called with
  the port as its argument when a port receive rights message for that port
  is received from the kernel.")

(defvar *port-ownership-rights-handlers* (make-hash-table :test #'eql)
  "This is a hashtable from ports to functions.  The function is called with
  the port as its argument when a port ownership rights message for that port
  is received from the kernel.")

(setf (object-set-operation *kernel-messages* mach:notify-receive-rights)
      #'(lambda (obj)
	  (declare (ignore obj))
	  (let ((ex-port (alien-access
			  (port-death-msg-ex-port
			   (server-message-port-death-msg server-message)))))
	    (funcall (gethash ex-port *port-receive-rights-handlers*
			      #'handle-unclaimed-port-rights)
		     ex-port))
	  mach:kern-success))

(setf (object-set-operation *kernel-messages* mach:notify-ownership-rights)
      #'(lambda (obj)
	  (declare (ignore obj))
	  (let ((ex-port (alien-access
			  (port-death-msg-ex-port
			   (server-message-port-death-msg server-message)))))
	    (funcall (gethash ex-port *port-ownership-rights-handlers*
			      #'handle-unclaimed-port-rights)
		     ex-port))
	  mach:kern-success))

(defun handle-unclaimed-port-rights (port)
  (unless *pornography-of-death*
    (format *trace-output* "~&[Rights received for port ~D, deallocating it.]~%"
	    port))
  (mach:port_deallocate *task-self* port)
  (remhash port *port-receive-rights-handlers*)
  (remhash port *port-ownership-rights-handlers*)
  (remhash port *port-table*))

(add-port-object *task-data* nil *kernel-messages*)

;;; Clear-Port-Tables  --  Internal
;;;
;;;    A before-save initialization which clears all of the port hashtables.
;;;
(defun clear-port-tables ()
  (clrhash *port-table*)
  (clrhash *port-death-handlers*)
  (clrhash *port-receive-rights-handlers*)
  (clrhash *port-ownership-rights-handlers*))

(pushnew 'clear-port-tables *before-save-initializations*)

|#



;;; %Initial-Function is called when a cold system starts up.  First we zoom
;;; down the *Lisp-Initialization-Functions* doing things that wanted to happen
;;; at "load time."  Then we initialize the various subsystems and call the
;;; read-eval-print loop.  The top-level Read-Eval-Print loop is executed until
;;; someone (most likely the Quit function) throws to the tag
;;; %End-Of-The-World.  We quit this way so that all outstanding cleanup forms
;;; in Unwind-Protects will get executed.

(proclaim '(special *lisp-initialization-functions*))

(eval-when (compile)
  (defmacro print-and-call (name)
    `(progn
       (%primitive print ,(symbol-name name))
       (,name))))

(def-c-variable "internal_errors_enabled" boolean)

(defun %initial-function ()
  "Gives the world a shove and hopes it spins."
  (setf *already-maybe-gcing* t)
  (setf *gc-inhibit* t)
  (setf *need-to-collect-garbage* nil)
  (setf *gc-verbose* t)
  (setf *before-gc-hooks* nil)
  (setf *after-gc-hooks* nil)
  (setf mach::*interrupts-enabled* t)
  (setf mach::*interrupt-pending* nil)
  (setf c::*type-system-initialized* nil)
  (%primitive print "In initial-function, and running.")

  ;; Many top-level forms call INFO, (SETF INFO).
  (print-and-call c::globaldb-init)

  ;; Some of the random top-level forms call Make-Array, which calls Subtypep...
  (print-and-call type-init)

  (setf *lisp-initialization-functions*
	(nreverse *lisp-initialization-functions*))
  (%primitive print "Calling top-level forms.")
  (dolist (fun *lisp-initialization-functions*)
    (funcall fun))
  (makunbound '*lisp-initialization-functions*)	; So it gets GC'ed.

  ;; Only do this after top level forms have run, 'cause thats where
  ;; deftypes are.
  (setf c::*type-system-initialized* t)

  (print-and-call os-init)
  (print-and-call filesys-init)
  (print-and-call conditions::error-init)

  (print-and-call reader-init)
  (print-and-call backq-init)
  (print-and-call sharp-init)
  ;; After the various reader subsystems have done their thing to the standard
  ;; readtable, copy it to *readtable*.
  (setf *readtable* (copy-readtable std-lisp-readtable))

  (print-and-call stream-init)
  (print-and-call loader-init)
  (print-and-call format-init)
  (print-and-call package-init)
  (print-and-call kernel::signal-init)
  (setf (alien-access (alien-value internal_errors_enabled)) t)

  (%primitive print "Done initializing.")

  (setf *already-maybe-gcing* nil)
  (terpri)
  (princ "CMU Common Lisp kernel core image ")
  (princ (lisp-implementation-version))
  (princ ".")
  (terpri)
  (princ "[You are in the LISP package.]")
  (terpri)
  (catch '%end-of-the-world
    (loop
     (%top-level)
     (write-line "You're certainly a clever child.")))
  (mach:unix-exit 0))


;;;; Initialization functions:

;;; Reinit is called to reinitialize the world when a saved core image
;;; is resumed.
(defvar *task-notify* NIL)

(defun reinit ()
  (without-interrupts
   (setf *already-maybe-gcing* t)
   (os-init)
   (stream-reinit)
   (kernel::signal-init)
   (setf (alien-access (alien-value internal_errors_enabled)) t)
   (setf *already-maybe-gcing* nil))
  #+nil
  (mach:port_enable (mach:mach-task_self) *task-notify*)
  #+nil
  (add-port-object *task-notify* nil *kernel-messages*))

;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defun os-init ()
  (setf *task-self* (mach:mach-task_self))
  (setf *task-data* (mach:mach-task_data))
  (setf *task-notify* (mach:mach-task_notify)))


;;; Setup-path-search-list returns a list of the directories that are
;;; in the unix path environment variable.  This is so that run-program
;;; can be smarter about where to find a program to run.
(defun setup-path-search-list ()
  (let ((path (cdr (assoc :path ext::*environment-list*))))
    (when path
      (do* ((i 0 (1+ p))
	    (p (position #\: path :start i)
	       (position #\: path :start i))
	    (pl ()))
	   ((null p)
	    (let ((s (subseq path i)))
	      (if (string= s "")
		  (push "default:" pl)
		  (push (concatenate 'simple-string s "/") pl)))
	    (nreverse pl))
	(let ((s (subseq path i p)))
	  (if (string= s "")
	      (push "default:" pl)
	      (push (concatenate 'simple-string s "/") pl)))))))


;;;; Miscellaneous external functions:

;;; Quit gets us out, one way or another.

(defun quit (&optional recklessly-p)
  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
  non-Nil."
  (if recklessly-p
      (mach:unix-exit 0)
      (throw '%end-of-the-world nil)))


(defun sleep (n)
  "This function causes execution to be suspended for N seconds.  N may
  be any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
            Must be a non-negative, non-complex number."
	   n))
  (multiple-value-bind (sec usec)
		       (if (integerp n)
			   (values n 0)
			   (values (truncate n)
				   (truncate (* n 1000000))))
    (mach:unix-select 0 0 0 0 sec usec))
  nil)


;;;; TOP-LEVEL loop.

(defvar / nil
  "Holds a list of all the values returned by the most recent top-level EVAL.")
(defvar // nil "Gets the previous value of / when a new value is computed.")
(defvar /// nil "Gets the previous value of // when a new value is computed.")
(defvar * nil "Holds the value of the most recent top-level EVAL.")
(defvar ** nil "Gets the previous value of * when a new value is computed.")
(defvar *** nil "Gets the previous value of ** when a new value is computed.")
(defvar + nil "Holds the value of the most recent top-level READ.")
(defvar ++ nil "Gets the previous value of + when a new value is read.")
(defvar +++ nil "Gets the previous value of ++ when a new value is read.")
(defvar - nil "Holds the form curently being evaluated.")
(defvar *prompt* "* "
  "The top-level prompt string.  This also may be a function of no arguments
   that returns a simple-string.")
(defvar *in-top-level-catcher* nil
  "True if we are within the Top-Level-Catcher.  This is used by interrupt
  handlers to see whether it is o.k. to throw.")

(defun interactive-eval (form)
  "Evaluate FORM, returning whatever it returns but adjust ***, **, *, +++, ++,
  +, ///, //, /, and -."
  (setf +++ ++
	++ +
	+ -
	- form)
  (let ((results (multiple-value-list (eval form))))
    (setf /// //
	  // /
	  / results
	  *** **
	  ** *
	  * (car results)))
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    (setf * nil)
    (cerror "Go on with * set to NIL."
	    "EVAL returned an unbound marker."))
  (values-list /))

(defconstant eofs-before-quit 10)

(defun %top-level ()
  "Top-level READ-EVAL-PRINT loop.  Do not call this."
  (let  ((* nil) (** nil) (*** nil)
	 (- nil) (+ nil) (++ nil) (+++ nil)
	 (/// nil) (// nil) (/ nil)
	 (magic-eof-cookie (cons :eof nil))
	 (number-of-eofs 0))
    (loop
     (with-simple-restart (abort "Return to Top-Level.")
       (catch 'top-level-catcher
	 (let ((*in-top-level-catcher* t))
	   (loop
	     (fresh-line)
	     (princ (if (functionp *prompt*)
			(funcall *prompt*)
			*prompt*))
	     (force-output)
	     (let ((form (read *standard-input* nil magic-eof-cookie)))
	       (cond ((not (eq form magic-eof-cookie))
		      (let ((results
			     (multiple-value-list (interactive-eval form))))
			(dolist (result results)
			  (fresh-line)
			  (prin1 result)))
		      (setf number-of-eofs 0))
		     ((eql (incf number-of-eofs) 1)
		      (let ((stream (make-synonym-stream '*terminal-io*)))
			(setf *standard-input* stream)
			(setf *standard-output* stream)
			(format t "~&Received EOF on *standard-input*, ~
			          switching to *terminal-io*.~%")))
		     ((> number-of-eofs eofs-before-quit)
		      (format t "~&Received more than ~D EOFs; Aborting.~%"
			      eofs-before-quit)
		      (quit))
		     (t
		      (format t "~&Received EOF.~%")))))))))))



;;; %Halt  --  Interface
;;;
;;;    A convenient way to get into the assembly level debugger.
;;;
(defun %halt ()
  (%primitive halt))
