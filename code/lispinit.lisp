;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Initialization and low-level interrupt support for the Spice Lisp system.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "LISP" :use '("SYSTEM" "DEBUG"))

(in-package "XLIB")

(in-package "LISP")

(export '(most-positive-fixnum most-negative-fixnum sleep
			       ++ +++ ** *** // ///))


(in-package "SYSTEM")
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
	  *nameserverport* *usertypescript* *userwindow* *typescriptport*
	  *task-self* *task-data* *task-notify* *file-input-handlers*
	  with-interrupts with-enabled-interrupts enable-interrupt
	  ignore-interrupt default-interrupt serve-all))

(in-package "EXTENSIONS")
(export '(quit *prompt* print-herald save-lisp gc-on gc-off
	       *before-save-initializations* *after-save-initializations*
	       *editor-lisp-p* *clx-server-displays* *display-event-handlers*))

(in-package "LISP")

;;; These go here so that we can refer to them in top-level forms.

(defvar *before-save-initializations* ()
  "This is a list of functions which are called before creating a saved core
  image.  These functions are executed in the child process which has no ports,
  so they cannot do anything that tries to talk to the outside world.")

(defvar *after-save-initializations* ()
  "This is a list of functions which are called when a saved core image starts
  up.  The system itself should be initialized at this point, but applications
  might not be.")

;;; Make the error system enable interrupts.

(defconstant most-positive-fixnum 134217727
  "The fixnum closest in value to positive infinity.")

(defconstant most-negative-fixnum -134217728
  "The fixnum closest in value to negative infinity.")


;;; Random information:

(defvar compiler-version "???")
(defvar *lisp-implementation-version* "3.0(?)")

(defvar *in-the-compiler* ()
  "Bound to T while running code inside the compiler.  Macros may test this to
  see where they are being expanded.")

(defparameter %fasl-code-format 6)


;;;; Global ports:
 
(defvar *task-self* 1
  "Port that refers to the current task.")

(defvar *task-data* 2
  "Port used to receive data for the current task.")

(defvar *nameserverport* ()
  "Port to the name server.")


;;; GC stuff.

(defvar *gc-inhibit* nil)	; Inhibits GC's.

(defvar *already-maybe-gcing* nil) ; Inhibits recursive GC's.

(defvar *need-to-collect-garbage* nil
  "*Need-to-collect-garbage* is set to T when GC is disabled, but the system
  needs to do a GC.  When GC is enabled again, the GC is done then.")


;;; Software interrupt stuff.

(defvar *in-server* NIL
  "*In-server* is set to T when the SIGMSG interrupt has been enabled
  in Server.")

(defvar server-unique-object (cons 1 2))

(defconstant lockout-interrupts (logior (mach:sigmask mach:sigint)
					(mach:sigmask mach:sigquit)
					(mach:sigmask mach:sigfpe)
					(mach:sigmask mach:sigsys)
					(mach:sigmask mach:sigpipe)
					(mach:sigmask mach:sigalrm)
					(mach:sigmask mach:sigurg)
					(mach:sigmask mach:sigstop)
					(mach:sigmask mach:sigtstp)
					(mach:sigmask mach:sigcont)
					(mach:sigmask mach:sigchld)
					(mach:sigmask mach:sigttin)
					(mach:sigmask mach:sigttou)
					(mach:sigmask mach:sigio)
					(mach:sigmask mach:sigxcpu)
					(mach:sigmask mach:sigxfsz)
					(mach:sigmask mach:sigvtalrm)
					(mach:sigmask mach:sigprof)
					(mach:sigmask mach:sigwinch)
					(mach:sigmask mach:sigmsg)
					(mach:sigmask mach:sigemsg)))

(defconstant interrupt-stack-size 4096
  "Size of stack for Unix interrupts.")

(defvar software-interrupt-stack NIL
  "Address of the stack used by Mach to send signals to Lisp.")

(defvar %sp-interrupts-inhibited nil
  "True if emergency message interrupts should be inhibited, false otherwise.")

(defvar *software-interrupt-vector*
  (make-array mach::maximum-interrupts)
  "A vector that associates Lisp functions with Unix interrupts.")

(defun enable-interrupt (interrupt function &optional character)
  "Enable one Unix interrupt and associate a Lisp function with it.
  Interrupt should be the number of the interrupt to enable.  Function
  should be a funcallable object that will be called with three
  arguments: the signal code, a subcode, and the context of the
  interrupt.  The optional character should be an ascii character or
  an integer that causes the interrupt from the keyboard.  This argument
  is only used for SIGINT, SIGQUIT, and SIGTSTP interrupts and is ignored
  for any others.  Returns the old function associated with the interrupt
  and the character that generates it if the interrupt is one of SIGINT,
  SIGQUIT, SIGTSTP and character was specified."
  (unless (< 0 interrupt mach::maximum-interrupts)
    (error "Interrupt number ~D is not between 1 and ~D."
	   mach::maximum-interrupts))
  (let ((old-fun (svref *software-interrupt-vector* interrupt))
	(old-char ()))
    (when (and character
	       (or (eq interrupt mach:sigint)
		   (eq interrupt mach:sigquit)
		   (eq interrupt mach:sigtstp)))
      (when (characterp character)
	(setq character (char-code character)))
      (when (mach:unix-isatty 0)
	(if (or (eq interrupt mach:sigint)
		(eq interrupt mach:sigquit))
	    (mach:with-trap-arg-block mach:tchars tc
	      (multiple-value-bind
		  (val err)
		  (mach:unix-ioctl 0 mach:TIOCGETC
				   (alien-value-sap mach:tchars))
		(if (null val)
		    (error "Failed to get tchars information, unix error ~S."
			   (mach:get-unix-error-msg err))))
	      (cond ((eq interrupt mach:sigint)
		     (setq old-char
			   (alien-access (mach::tchars-intrc (alien-value tc))))
		     (setf (alien-access (mach::tchars-intrc (alien-value tc)))
			   character))
		    (T
		     (setq old-char
			   (alien-access (mach::tchars-quitc (alien-value tc))))
		     (setf (alien-access (mach::tchars-quitc (alien-value tc)))
			   character)))
	      (multiple-value-bind
		  (val err)
		  (mach:unix-ioctl 0 mach:tiocsetc
				   (alien-value-sap mach:tchars))
		(if (null val)
		    (error "Failed to set tchars information, unix error ~S."
			   (mach:get-unix-error-msg err)))))
	    (mach:with-trap-arg-block mach:ltchars tc
	      (multiple-value-bind
		  (val err)
		  (mach:unix-ioctl 0 mach:TIOCGLTC
				   (alien-value-sap mach:ltchars))
		(if (null val)
		    (error "Failed to get ltchars information, unix error ~S."
			   (mach:get-unix-error-msg err))))
	      (setq old-char
		    (alien-access (mach::ltchars-suspc (alien-value tc))))
	      (setf (alien-access (mach::ltchars-suspc (alien-value tc)))
		    character)
	      (multiple-value-bind
		  (val err)
		  (mach:unix-ioctl 0 mach:TIOCSLTC
				   (alien-value-sap mach:ltchars))
		(if (null val)
		    (error "Failed to set ltchars information, unix error ~S."
			   (mach:get-unix-error-msg err))))))))
    (setf (svref *software-interrupt-vector* interrupt) function)
    (if (null function)
	(mach:unix-sigvec interrupt mach:sig_dfl 0 0)
	(let ((diha (+ (ash clc::romp-data-base 16)
		       clc::software-interrupt-offset)))
	  (mach:unix-sigvec interrupt diha lockout-interrupts 1)))
    (if old-char
	(values old-fun old-char)
	old-fun)))

(defun ignore-interrupt (interrupt)
  "The Unix interrupt handling mechanism is set up so that interrupt is
  ignored."
  (unless (< 0 interrupt mach::maximum-interrupts)
    (error "Interrupt number ~D is not between 1 and 31."))
  (let ((old-fun (svref *software-interrupt-vector* interrupt)))
    (mach:unix-sigvec interrupt mach:sig_ign 0 0)
    (setf (svref *software-interrupt-vector* interrupt) NIL)
    old-fun))

(defun default-interrupt (interrupt)
  "The Unix interrupt handling mechanism is set up to do the default action
  under mach.  Lisp will not get control of the interrupt."
  (unless (< 0 interrupt mach::maximum-interrupts)
    (error "Interrupt number ~D is not between 1 and 31."))
  (let ((old-fun (svref *software-interrupt-vector* interrupt)))
    (mach:unix-sigvec interrupt mach:sig_dfl 0 0)
    (setf (svref *software-interrupt-vector* interrupt) NIL)
    old-fun))


;;; %SP-Software-Interrupt-Handler is called by the miscops when a Unix
;;; signal arrives.  The three arguments correspond to the information
;;; passed to a normal Unix signal handler, i.e.:
;;;	signal -- the Unix signal number.
;;;	code -- a code for those signals which can be caused by more
;;;		than one kind of event.  This code specifies the sub-event.
;;;	scp -- a pointer to the context of the signal.

;;; Because of the way %sp-software-interrupt-handler returns, it doesn't
;;; unwind the binding stack properly.  The only variable affected by this
;;; is software-interrupt-stack, so it must be handled specially.

(defun %sp-software-interrupt-handler (signal code scp stack)
  (declare (optimize (speed 3) (safety 0)))
  (if (and %sp-interrupts-inhibited
	   (not (memq signal '(#.mach:sigill #.mach:sigbus #.mach:sigsegv))))
      (progn
	(let ((iin %sp-interrupts-inhibited))
	  (setq %sp-interrupts-inhibited
		(nconc (if (consp iin) iin)
		       (list `(,signal ,code ,scp))))
	  (mach:unix-sigsetmask 0)))
      (let* ((old-stack software-interrupt-stack)
	     (new-stack ())
	     (%sp-interrupts-inhibited T))
	(unwind-protect
	    (progn
	      (when *in-server*
		(mach:unix-sigvec mach:sigmsg mach::sig_dfl 0 0))
	      (multiple-value-bind (gr addr)
				   (mach:vm_allocate *task-self* 0
						     interrupt-stack-size t)
		(gr-error 'mach:vm_allocate gr '%sp-software-interrupt-handler)
		(setq software-interrupt-stack
		      (int-sap (+ addr interrupt-stack-size))))
	      (setq new-stack software-interrupt-stack)
	      (mach:unix-sigstack new-stack 0)
	      (mach:unix-sigsetmask 0)
	      (funcall (svref *software-interrupt-vector* signal)
		       signal code scp)
	      (mach:unix-sigsetmask lockout-interrupts))
	  (mach:vm_deallocate *task-self*
			      (- (sap-int new-stack)
				 interrupt-stack-size)
			      interrupt-stack-size)
	  (setq software-interrupt-stack old-stack)
	  (mach:unix-sigstack old-stack 0)
	  (when *in-server*
	    (let ((diha (+ (ash clc::romp-data-base 16)
			   clc::software-interrupt-offset)))
	      (mach:unix-sigvec mach:sigmsg diha lockout-interrupts 1)))
	  (mach:unix-sigsetmask 0))))
  (%primitive break-return stack))


(defun ih-sigint (signal code scp)
  (declare (ignore signal code scp))
  (without-hemlock
   (with-interrupts
    (break "Software Interrupt" t))))

(defun ih-sigquit (signal code scp)
  (declare (ignore signal code scp))
  (throw 'top-level-catcher nil))

(defun ih-sigtstp (signal code scp)
  (declare (ignore signal code scp))
  (without-hemlock
;   (reset-keyboard 0)
   (mach:unix-kill (mach:unix-getpid) mach:sigstop)))

(defun ih-sigill (signal code scp)
  (declare (ignore signal code))
  (alien-bind ((context (make-alien-value scp 0 (record-size 'mach:sigcontext)
					  'mach:sigcontext)
			mach:sigcontext T))
    (error "Illegal instruction encountered at IAR ~X."
	   (alien-access (mach::sigcontext-iar (alien-value context))))))

(defun ih-sigbus (signal code scp)
  (declare (ignore signal code))
  (alien-bind ((context (make-alien-value scp 0 (record-size 'mach:sigcontext)
					  'mach:sigcontext)
			mach:sigcontext T))
    (with-interrupts
     (error "Bus error encountered at IAR ~X."
	    (alien-access (mach::sigcontext-iar (alien-value context)))))))

(defun ih-sigsegv (signal code scp)
  (declare (ignore signal code))
  (alien-bind ((context (make-alien-value scp 0 (record-size 'mach:sigcontext)
					  'mach:sigcontext)
			mach:sigcontext T))
    (with-interrupts
     (error "Segment violation encountered at IAR ~X."
	    (alien-access (mach::sigcontext-iar (alien-value context)))))))

(defun ih-sigfpe (signal code scp)
  (declare (ignore signal code))
  (alien-bind ((context (make-alien-value scp 0 (record-size 'mach:sigcontext)
					  'mach:sigcontext)
			mach:sigcontext T))
    (with-interrupts
     (error "Floating point exception encountered at IAR ~X."
	    (alien-access (mach::sigcontext-iar (alien-value context)))))))

;;; When we're in server then throw back to server.  If we're not
;;; in server then just ignore the sigmsg interrupt.  We can't handle
;;; it and we should never get it anyway.  But of course we do -- it's
;;; dealing with interrupts and there funny at best.
(defun ih-sigmsg (signal code scp)
  (declare (ignore signal code scp))
  (mach:unix-sigsetmask (mach:sigmask mach:sigmsg))
  (default-interrupt mach:sigmsg)
  (when *in-server*
    (setq *in-server* nil)
    (throw 'server-catch server-unique-object)))

(defun ih-sigemsg (signal code scp)
  (declare (ignore signal code scp))
  (service-emergency-message-interrupt))

(defun init-mach-signals ()
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (gr addr)
		       (mach:vm_allocate *task-self* 0 interrupt-stack-size t)
    (gr-error 'mach:vm_allocate gr 'enable-interrupt)
    (setq software-interrupt-stack
	  (int-sap (+ addr interrupt-stack-size))))
  (let ((iha (get 'clc::interrupt-handler '%loaded-address))
	(diha (+ (ash clc::romp-data-base 16) clc::software-interrupt-offset)))
    (%primitive pointer-system-set diha 0 iha))
  (mach:unix-sigstack software-interrupt-stack 0)
  (enable-interrupt mach:sigint #'ih-sigint)
  (enable-interrupt mach:sigquit #'ih-sigquit)
  (enable-interrupt mach:sigtstp #'ih-sigtstp)
  (enable-interrupt mach:sigill #'ih-sigill)
  (enable-interrupt mach:sigbus #'ih-sigbus)
  (enable-interrupt mach:sigsegv #'ih-sigsegv)
  (enable-interrupt mach:sigemsg #'ih-sigemsg)
  (enable-interrupt mach:sigfpe #'ih-sigfpe)
;  (reset-keyboard 0)
  )


;;;; Reply port allocation.
;;;
;;;    We maintain a global stack of reply ports which is shared among
;;; all matchmaker interfaces, and could be used by other people as well.
;;;
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
  (setq *reply-port-pointer* 0  *reply-port-depth* 0)
  (fill (the simple-vector *reply-port-stack*) nil)
  (setq *allocate-reply-port* (mach:mach-task_data)))
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
	(setq stack new  *reply-port-stack* new)))
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

;;;; Server function:
;;;
;;; SERVER makes use of a defined alien, server-event, that lives at address 0.
;;; This is a bogus alien used just as a dynamic variable that is declared
;;; appropriately for the compiler.  This alien variable is bound to stuff in
;;; an alien stack by the same name, server-event, which contains elements much
;;; bigger than necessary to accommodate whatever will come back in the future
;;; from waiting across ports, sockets, file descriptors, etc.  The defined
;;; alien operators allow easy access to server-event as different types of
;;; event by declaring the necessary type for the compiler when the operator
;;; is used.


;;;    Currently the server message is 4k bytes, thus serving larger requests
;;; is impossible.  If anyone is bothered by this, the size can be increased.
;;; X events are only 24 bytes.
;;; 

(defconstant server-message-size 4096)
(defalien server-message server-message (bytes server-message-size) 0)

(define-alien-stack server-message server-message (bytes server-message-size))

(defrecord server-message
  (msg mach:msg #.(record-size 'mach:msg)))

(defvar *file-input-handlers* ()
  "Is an association list of file descriptors and functions to call when
  input is available on the particular file descriptor.")

(defvar *clx-server-displays* ()
  "Is a list of CLX displays that may have some activity on them.")

(defvar *display-event-handlers* nil
  "This is an alist mapping displays to user functions to be called when
   SYSTEM:SERVER notices input on a display connection.  Do not modify this
   directly; use EXT:ENABLE-CLX-EVENT-HANDLING.  A given display should be
   represented here only once.")


;;; Default-Default-Handler  --  Internal
;;;
;;;    If no such operation defined, signal an error.
;;;
(defun default-default-handler (object)
  (alien-bind ((msg (server-message-msg server-message)))
    (error "No operation for ID ~D on ~S in ~S."
	   (alien-access (mach:msg-id (alien-value msg))) object
	   (car (gethash (alien-access (mach:msg-localport (alien-value msg)))
			 *port-table*)))))


;;; Server  --  Public
;;;
(defun server (&optional (timeout 0 todef))
  "Receive on all ports and Xevents and dispatch to the appropriate handler
  function.  If timeout is specified, server will wait the specified time
  and then return, otherwise it will wait until something happens.  Server
  returns T if something happened and NIL otherwise."
  (cond ((dolist (d/h ext::*display-event-handlers* nil)
	   (let ((d (car d/h)))
	     (when (xlib::event-listen d)
	       (handler-bind ((error #'(lambda (condx)
					 (declare (ignore condx))
					 (flush-display-events d))))
		 (funcall (cdr d/h) d))
	       (return t))))
	 T)
	(T
	 (let* ((to (if todef (round (* timeout 1000000))))
		(fd-mask 0)
		(omask 0)
		(value (catch 'server-catch
			 (unwind-protect
			     (progn
			       (setq omask (mach:unix-sigsetmask
					    (mach:sigmask mach:sigmsg)))
			       (unless (grab-message-loop)
				 (let ((*in-server* T))
				   (enable-interrupt mach:sigmsg #'ih-sigmsg)
				   (multiple-value-bind
				       (to1 to2)
				       (if todef (truncate to 1000000))
				     (multiple-value-bind
					 (nfd fdm)
					 (get-fd-info)
				       (mach:unix-sigsetmask 0)
				       (multiple-value-bind
					   (nfnd rfdm)
					   (mach:unix-select nfd fdm 0 0
							     to1 to2)
					 (mach:unix-sigsetmask
					  (mach:sigmask mach:sigmsg))
					 (default-interrupt mach:sigmsg)
					 (setq fd-mask rfdm)
					 nfnd))))))
			   (default-interrupt mach:sigmsg)
			   (mach:unix-sigsetmask omask)))))
	   (cond ((or (null value) (and todef (eq value 0))) NIL)
		 ((eq value server-unique-object)
		  (grab-message-loop)
		  T)
		 ((file-descriptor-ready fd-mask) T))))))

;;; Get-fd-info turns the association list in *file-input-handlers*
;;; into information that unix-select can be called with.
(defun Get-fd-info ()
  (do* ((fdl *file-input-handlers* (cdr fdl)) ; 
	(FD (caar fdl) (caar fdl))
	(mfd 0)
	(fdm 0))
       ((null fdl)
	(values (1+ mfd) fdm))
    (setq mfd (max mfd fd))
    (setq fdm (logior fdm (ash 1 fd)))))

;;; File-descriptor-ready is called when server determines that a file
;;; descriptor has input ready on one ore more of them.  It calls the
;;; appropriate handler with the file-descriptor as its argument.
;;; It checks for an xevent first, so they are handled as quickly as
;;; possible.
(defun file-descriptor-ready (rfdm)
  (do ((fd 0 (1+ fd))
       (ms rfdm (ash ms -1)))
      ((eq ms 0))
    (when (/= (the fixnum (logand ms 1)) 0)
      (let ((info (assoc fd *file-input-handlers* :test #'eq)))
	(when info
	  (funcall (cdr info) fd)))))
  T)

;;; Grab-message-loop calls the appropiate handler for an IPC message.
(defun grab-message-loop ()
  (do* ((gr (server-grab-message) (server-grab-message))
	(flag (/= gr mach:rcv-timed-out)
	      (if (/= gr mach:rcv-timed-out) t flag)))
       ((= gr mach:rcv-timed-out) flag)))

(defun server-grab-message ()
  (with-stack-alien (sm server-message)
    (alien-bind ((msg (server-message-msg (alien-value sm))))
      (setf (alien-access (mach:msg-msgsize (alien-value msg)))
	    server-message-size)
      (setf (alien-access (mach:msg-localport (alien-value msg)))
	    mach::port-enabled)
      (let ((gr (mach:msg-receive (alien-value sm) mach::rcv-timeout 0)))
	(when (eql gr mach:rcv-timed-out)
	  (return-from server-grab-message gr))
	(unless (eql gr mach:rcv-success)
	  (gr-error 'mach:msg-receive gr))
	(let* ((server-message (alien-value sm))
	       (port (alien-access (mach:msg-localport (alien-value msg))))
	       (id (alien-access (mach:msg-id (alien-value msg))))
	       (x (gethash port *port-table*))
	       (set (cdr x)))
	  (unless x
	    (error "~D is not known to server (operation: ~D)." port id))
	  (let ((gr (funcall (gethash id (object-set-table set)
				      (object-set-default-handler set))
			     (car x))))
	    (unless (eql gr mach:kern-success)
	      (gr-error 'server gr)))))))
  mach:kern-success)

(defun serve-all (&optional (timeout 0))
  "Serve-all calls server with the specified timeout.  If server does
  something (returns T) it loops over server with timeout 0 until all
  events have been served.  Serve-all returns T if server did something
  and other NIL."
  (do ((res NIL)
       (sval (server timeout) (server 0)))
      ((null sval) res)
    (setq res T)))


;;;; Emergency Message Handling:
;;;
;;; We use the same mechanism for asynchronous messages as is used for
;;; normal server messages.   The only tricky part is that we don't want
;;; some random server function being called when we really want to
;;; receive an emergency message, so we can't receive on all ports.
;;; Instead, we use MessagesWaiting to find the ports with emergency
;;; messages.

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
       (%primitive print ',name)
       (,name))))

(defun %initial-function ()
  "Gives the world a shove and hopes it spins."
  (setq *already-maybe-gcing* t)
  (setf *gc-inhibit* t)
  (setf *need-to-collect-garbage* nil)
  (%primitive print "In initial-function, and running.")

  ;; Many top-level forms call INFO, (SETF INFO).
  (print-and-call c::globaldb-init)

  ;; Some of the random top-level forms call Make-Array, which calls Subtypep...
  (print-and-call subtypep-init)

  (setq *lisp-initialization-functions*
	(nreverse *lisp-initialization-functions*))
  (%primitive print "Calling top-level forms.")
  (dolist (fun *lisp-initialization-functions*)
    (funcall fun))
  (makunbound '*lisp-initialization-functions*)	; So it gets GC'ed.

  (print-and-call os-init)
  (print-and-call filesys-init)
  (print-and-call conditions::error-init)

  (print-and-call reader-init)
  (print-and-call backq-init)
  (print-and-call sharp-init)
  ;; After the various reader subsystems have done their thing to the standard
  ;; readtable, copy it to *readtable*.
  (setq *readtable* (copy-readtable std-lisp-readtable))

  (print-and-call stream-init)
  (print-and-call random-init)
  (print-and-call format-init)
  (print-and-call package-init)
  (print-and-call pprint-init)

  (setq *already-maybe-gcing* nil)
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
   (setq *already-maybe-gcing* t)
   (os-init)
   (stream-reinit)
   (setq *already-maybe-gcing* nil))
  (setq *task-notify* (mach:mach-task_notify))
  (mach:port_enable (mach:mach-task_self) *task-notify*)
  (add-port-object *task-notify* nil *kernel-messages*)
  (init-mach-signals))


;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defun os-init ()
  (setq *task-self* (mach:mach-task_self))
  (setq *task-data* (mach:mach-task_data)))


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

(defun print-herald ()
  (write-string "CMU Common Lisp ")
  (write-line (lisp-implementation-version))
  (write-string "Hemlock ") (write-string *hemlock-version*)
  (write-string ", Compiler ") (write-line compiler-version)
  (write-line "Send bug reports and questions to Gripe.")
  (values))

(defvar *editor-lisp-p* nil
  "This is true if and only if the lisp was started with the -edit switch.")

(defun save-lisp (core-file-name &key
				 (purify t)
				 (root-structures ())
				 (init-function
				  #'(lambda ()
				      (throw 'top-level-catcher nil)))
				 (load-init-file t)
				 (print-herald t)
				 (process-command-line t))
  "Saves a Spice Lisp core image in the file of the specified name.  The
  following keywords are defined:
  
  :purify
      If true, do a purifying GC which moves all dynamically allocated
  objects into static space so that they stay pure.  This takes somewhat
  longer than the normal GC which is otherwise done, but GC's will done
  less often and take less time in the resulting core file.

  :root-structures
      This should be a list of the main entry points in any newly loaded
  systems.  This need not be supplied, but locality will be better if it
  is.  This is meaningless if :purify is Nil.
  
  :init-function
      This is a function which is called when the created core file is
  resumed.  The default function simply aborts to the top level
  read-eval-print loop.  If the function returns it will be the value
  of Save-Lisp.
  
  :load-init-file
      If true, then look for an init.lisp or init.fasl file when the core
  file is resumed.
  
  :print-herald
      If true, print out the lisp system herald when starting."
  
  (if purify
      (purify :root-structures root-structures)
      (gc))
  (unless (save core-file-name)
    (setf (search-list "default:") (list (default-directory)))
    (setf (search-list "path:") (setup-path-search-list))
    (when process-command-line (ext::process-command-strings))
    (setf *editor-lisp-p* nil)
    (macrolet ((find-switch (name)
		 `(find ,name *command-line-switches*
			:key #'cmd-switch-name
			:test #'(lambda (x y)
				  (declare (simple-string x y))
				  (string-equal x y)))))
      (when (and process-command-line (find-switch "edit"))
	(setf *editor-lisp-p* t))
      (when (and load-init-file
		 (not (and process-command-line (find-switch "noinit"))))
	(let* ((cl-switch (find-switch "init"))
	       (name (or (and cl-switch
			      (or (cmd-switch-value cl-switch)
				  (car (cmd-switch-words cl-switch))
				  "init"))
			 "init")))
	  (load (merge-pathnames name (user-homedir-pathname))
		:if-does-not-exist nil))))
    (when print-herald
      (print-herald))
    (when process-command-line
      (ext::invoke-switch-demons *command-line-switches*
				 *command-switch-demons*))
    (funcall init-function)))


;;; Quit gets us out, one way or another.

(defun quit (&optional recklessly-p)
  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
  non-Nil."
;  (reset-keyboard 0)
  (dolist (x (if (boundp 'extensions::temporary-foreign-files)
		 extensions::temporary-foreign-files))
    (mach:unix-unlink x))
  (if recklessly-p
      (mach:unix-exit 0)
      (throw '%end-of-the-world nil)))



(defalien sleep-msg mach:msg (record-size 'mach:msg))
(setf (alien-access (mach:msg-simplemsg sleep-msg)) T)
(setf (alien-access (mach:msg-msgtype sleep-msg)) 0)
(setf (alien-access (mach:msg-msgsize sleep-msg))
      (/ (record-size 'mach:msg) 8))

;;; Currently there is a bug in the Mach timeout code that if the timeout
;;; period is too short the receive never returns.

(defun sleep (n)
  "This function causes execution to be suspended for N seconds.  N may
  be any non-negative, non-complex number."
  (with-reply-port (sleep-port)
    (let ((m (round (* 1000 n))))
      (cond ((minusp m)
	     (error "Argument to Sleep, ~S, is a negative number." n))
	    ((zerop m))
	    (t
	     (setf (alien-access (mach:msg-localport sleep-msg)) sleep-port)
	     (let ((gr (mach:msg-receive sleep-msg mach:rcv-timeout m)))
	       (unless (eql gr mach:rcv-timed-out)
		 (gr-error 'mach:receive gr)))))))
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
