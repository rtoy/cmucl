;;; -*- Package: Extensions; Log: code.log  -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/run-program.lisp,v 1.4 1991/06/17 17:35:28 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; RUN-PROGRAM and friends.  Facility for running unix programs from inside
;;; a lisp.
;;; 
;;; Written by Jim Healy and Bill Chiles, November 1987, using an earlier
;;; version written by David McDonald.
;;;
;;; Completely re-written by William Lott, July 1989 - January 1990.
;;;

(in-package "EXTENSIONS")

(export '(run-program process-status process-exit-code process-core-dumped
	  process-wait process-kill process-input process-output process-plist
	  process-pty process-error process-status-hook process-alive-p
	  process-close process-pid process-p))


;;;; Import WAIT3 from unix.

(ext:def-c-pointer *wait (unsigned-byte 32))

(ext:def-c-routine ("wait3" c-wait3)
		   (int)
    (status *wait :out)
    (options int)
    (rusage int))

(eval-when (load eval compile)
  (defconstant wait-wstopped #o177)
  (defconstant wait-wnohang 1)
  (defconstant wait-wuntraced 2))

(defun wait3 (&optional do-not-hang check-for-stopped)
  "Return any available status information on child processed. "
  (multiple-value-bind (pid status)
		       (c-wait3 (logior (if do-not-hang
					  wait-wnohang
					  0)
					(if check-for-stopped
					  wait-wuntraced
					  0))
				0)
    (cond ((or (minusp pid)
	       (zerop pid))
	   nil)
	  ((eql (ldb (byte 8 0) status)
		wait-wstopped)
	   (values pid
		   :stopped
		   (ldb (byte 8 8) status)))
	  ((zerop (ldb (byte 7 0) status))
	   (values pid
		   :exited
		   (ldb (byte 8 8) status)))
	  (t
	   (let ((signal (ldb (byte 7 0) status)))
	     (values pid
		     (if (or (eql signal mach:sigstop)
			     (eql signal mach:sigtstp)
			     (eql signal mach:sigttin)
			     (eql signal mach:sigttou))
		       :stopped
		       :signaled)
		     signal
		     (not (zerop (ldb (byte 1 7) status)))))))))



;;;; Process control stuff.

(defvar *active-processes* nil
  "List of process structures for all active processes.")

(defstruct (process (:print-function %print-process))
  pid			    ; PID of child process.
  %status		    ; Either :RUNNING, :STOPPED, :EXITED, or :SIGNALED.
  exit-code		    ; Either exit code or signal
  core-dumped		    ; T if a core image was dumped.
  pty			    ; Stream to child's pty or nil.
  input			    ; Stream to child's input or nil.
  output		    ; Stream from child's output or nil.
  error			    ; Stream from child's error output or nil.
  status-hook		    ; Closure to call when PROC changes status.
  plist			    ; Place for clients to stash tings.
  cookie		    ; List of the number of pipes from the subproc.
  )

(defun %print-process (proc stream depth)
  (declare (ignore depth))
  (format stream "#<process ~D ~S>"
	  (process-pid proc)
	  (process-status proc)))

;;; PROCESS-STATUS -- Public.
;;;
(defun process-status (proc)
  "Return the current status of process.  The result is one of :running,
   :stopped, :exited, :signaled."
  (get-processes-status-changes)
  (process-%status proc))


;;; PROCESS-WAIT -- Public.
;;;
(defun process-wait (proc &optional check-for-stopped)
  "Wait for PROC to quit running for some reason.  Returns PROC."
  (loop
    (case (process-status proc)
      (:running)
      (:stopped
       (when check-for-stopped
	 (return)))
      (t
       (when (zerop (car (process-cookie proc)))
	 (return))))
    (system:serve-all-events 1))
  proc)


;;; FIND-CURRENT-FOREGROUND-PROCESS -- internal
;;;
;;; Finds the current foreground process group id.
;;; 
(defun find-current-foreground-process (proc)
  (system:with-stack-alien (result (unsigned-byte 32) (long-words 1))
    (multiple-value-bind
	(wonp error)
	(mach:unix-ioctl (system:fd-stream-fd (ext:process-pty proc))
			 mach:TIOCGPGRP
			 (system:alien-sap (system:alien-value result)))
      (unless wonp
	(error "TIOCPGRP ioctl failed: ~S"
	       (mach:get-unix-error-msg error)))
      (system:alien-access (system:alien-value result)))))

;;; PROCESS-KILL -- public
;;;
;;; Hand a process a signal.
;;;
(defun process-kill (proc signal &optional (whom :pid))
  "Hand SIGNAL to PROC.  If whom is :pid, use the kill Unix system call.  If
  whom is :process-group, use the killpg Unix system call.  If whom is
  :pty-process-group deliver the signal to whichever process group is currently
  in the foreground."
  (let ((pid (ecase whom
	       ((:pid :process-group)
		(process-pid proc))
	       (:pty-process-group
		(find-current-foreground-process proc)))))
    (multiple-value-bind (okay errno)
			 (if (eq whom :pty-process-group)
			   (mach:unix-killpg pid signal)
			   (mach:unix-kill pid signal))
      (cond ((not okay)
	     (values nil errno))
	    ((and (eql pid (process-pid proc))
		  (= (unix-signal-number signal) mach:sigcont))
	     (setf (process-%status proc) :running)
	     (setf (process-exit-code proc) nil)
	     (when (process-status-hook proc)
	       (funcall (process-status-hook proc) proc))
	     t)
	    (t
	     t)))))

;;; PROCESS-ALIVE-P -- public
;;;
;;; Returns T if the process is still alive, NIL otherwise.
;;; 
(defun process-alive-p (proc)
  "Returns T if the process is still alive, NIL otherwise."
  (let ((status (process-status proc)))
    (if (or (eq status :running)
	    (eq status :stopped))
      t
      nil)))

;;; PROCESS-CLOSE -- public
;;;
;;; Close all the streams held open by PROC.
;;; 
(defun process-close (proc)
  "Close all streams connected to PROC and stop maintaining the status slot."
  (macrolet ((frob (stream)
	       `(when ,stream (close ,stream))))
    (frob (process-pty proc))
    (frob (process-input proc))
    (frob (process-output proc))
    (frob (process-error proc))
    (system:without-interrupts
      (setf *active-processes* (delete proc *active-processes*)))
    proc))

;;; SIGCHLD-HANDLER -- Internal.
;;;
;;; This is the handler for sigchld signals that RUN-PROGRAM establishes.
;;;
(defun sigchld-handler (ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (get-processes-status-changes))

;;; GET-PROCESSES-STATUS-CHANGES -- Internal.
;;;
(defun get-processes-status-changes ()
  (loop
    (multiple-value-bind (pid what code core)
			 (wait3 t t)
      (unless pid
	(return))
      (let ((proc (find pid *active-processes* :key #'process-pid)))
	(when proc
	  (setf (process-%status proc) what)
	  (setf (process-exit-code proc) code)
	  (setf (process-core-dumped proc) core)
	  (when (process-status-hook proc)
	    (funcall (process-status-hook proc) proc))
	  (when (or (eq what :exited)
		    (eq what :signaled))
	    (system:without-interrupts
	      (setf *active-processes*
		    (delete proc *active-processes*)))))))))



;;;; RUN-PROGRAM and close friends.

(defvar *close-on-error* nil
  "List of file descriptors to close when RUN-PROGRAM exits due to an error.")
(defvar *close-in-parent* nil
  "List of file descriptors to close when RUN-PROGRAM returns in the parent.")
(defvar *handlers-installed* nil
  "List of handlers installed by RUN-PROGRAM.")


;;; FIND-A-PTY -- internal
;;;
;;;   Finds a pty that is not in use. Returns three values: the file descriptor
;;; for the master side of the pty, the file descriptor for the slave side of
;;; the pty, and the name of the tty device for the slave side.
;;; 
(defun find-a-pty ()
  "Returns the master fd, the slave fd, and the name of the tty"
  (dolist (char '(#\p #\q))
    (dotimes (digit 16)
      (let* ((master-name (format nil "/dev/pty~C~X" char digit))
	     (master-fd (mach:unix-open master-name
					mach:o_rdwr
					#o666)))
	(when master-fd
	  (let* ((slave-name (format nil "/dev/tty~C~X" char digit))
		 (slave-fd (mach:unix-open slave-name
					   mach:o_rdwr
					   #o666)))
	    (when slave-fd
	      ; Maybe put a vhangup here?
	      (with-stack-alien (stuff mach:sgtty (record-size 'mach:sgtty))
		(let ((sap (system:alien-sap (system:alien-value stuff))))
		  (mach:unix-ioctl slave-fd mach:TIOCGETP sap)
		  (setf (system:alien-access
			 (mach::sgtty-flags
			  (system:alien-value stuff)))
			#o6300) ; XTABS|EVENP|ODDP
		  (mach:unix-ioctl slave-fd mach:TIOCSETP sap)
		  (mach:unix-ioctl master-fd mach:TIOCGETP sap)
		  (setf (system:alien-access
			 (mach::sgtty-flags
			  (system:alien-value stuff)))
			(logand (system:alien-access
				 (mach::sgtty-flags
				  (system:alien-value stuff)))
				(lognot 8))) ; ~ECHO
		  (mach:unix-ioctl master-fd mach:TIOCSETP sap)))
	      (return-from find-a-pty
			   (values master-fd
				   slave-fd
				   slave-name)))
	  (mach:unix-close master-fd))))))
  (error "Could not find a pty."))

;;; OPEN-PTY -- internal
;;;
(defun open-pty (pty cookie)
  (when pty
    (multiple-value-bind
	(master slave name)
	(find-a-pty)
      (push master *close-on-error*)
      (push slave *close-in-parent*)
      (when (streamp pty)
	(multiple-value-bind (won new-fd) (mach:unix-dup master)
	  (unless won
	    (error "Could not MACH:UNIX-DUP ~D: ~A"
		   master (mach:get-unix-error-msg new-fd)))
	  (push new-fd *close-on-error*)
	  (copy-descriptor-to-stream new-fd pty cookie)))
      (values name
	      (system:make-fd-stream master :input t :output t)))))

;;; SETUP-CHILD -- internal
;;;
;;;   Execs the program after setting up the environment correctly. This
;;; routine never returns under any condition.
;;;
(defun setup-child (pfile args env stdin stdout stderr pty-name before-execve)
  (unwind-protect
      (handler-bind ((error #'(lambda (condition)
				(declare (ignore condition))
				(mach:unix-exit 2))))
	;; Put us in our own pgrp.
	(mach:unix-setpgrp 0 (mach:unix-getpid))
	;; If we want a pty, set it up.
	(when pty-name
	  (let ((old-tty (mach:unix-open "/dev/tty" mach:o_rdwr 0)))
	    (when old-tty
	      (mach:unix-ioctl old-tty mach:TIOCNOTTY 0)
	      (mach:unix-close old-tty)))
	  (let ((new-tty (mach:unix-open pty-name mach:o_rdwr 0)))
	    (when new-tty
	      (mach:unix-dup2 new-tty 0)
	      (mach:unix-dup2 new-tty 1)
	      (mach:unix-dup2 new-tty 2))))
	;; Setup the three standard descriptors.
	(when stdin
	  (mach:unix-dup2 stdin 0))
	(when stdout
	  (mach:unix-dup2 stdout 1))
	(when stderr
	  (mach:unix-dup2 stderr 2))
	;; Close all other descriptors.
	(do ((fd (1- (mach:unix-getdtablesize))
		 (1- fd)))
	    ((= fd 3))
	  (mach:unix-close fd))
	;; Do the before-execve
	(when before-execve
	  (funcall before-execve))
	;; Exec the program
	(multiple-value-bind
	    (okay errno)
	    (mach:unix-execve pfile args env)
	  (declare (ignore okay))
	  ;; If the magic number if bogus, try just a shell script.
	  (when (eql errno mach:ENOEXEC)
	    (mach:unix-execve "/bin/sh" (cons pfile args) env))))
    ;; If exec returns, we lose.
    (mach:unix-exit 1)))

;;; RUN-PROGRAM -- public
;;;
;;;   RUN-PROGRAM uses fork and execve to run a different program. Strange
;;; stuff happens to keep the unix state of the world coherent.
;;;
;;; The child process needs to get it's input from somewhere, and send it's
;;; output (both standard and error) to somewhere. We have to do different
;;; things depending on where these somewheres really are.
;;;
;;; For input, there are five options:
;;; - T: Just leave fd 0 alone. Pretty simple.
;;; - "file": Read from the file. We need to open the file and pull the
;;; descriptor out of the stream. The parent should close this stream after
;;; the child is up and running to free any storage used in the parent.
;;; - NIL: Same as "file", but use "/dev/null" as the file.
;;; - :STREAM: Use unix-pipe to create two descriptors. Use system:make-fd-stream
;;; to create the output stream on the writeable descriptor, and pass the
;;; readable descriptor to the child. The parent must close the readable
;;; descriptor for EOF to be passed up correctly.
;;; - a stream: If it's a fd-stream, just pull the descriptor out of it.
;;; Otherwise make a pipe as in :STREAM, and copy everything across.
;;;
;;; For output, there are n options:
;;; - T: Leave descriptor 1 alone.
;;; - "file": dump output to the file.
;;; - NIL: dump output to /dev/null.
;;; - :STREAM: return a stream that can be read from.
;;; - a stream: if it's a fd-stream, use the descriptor in it. Otherwise, copy
;;; stuff from output to stream.
;;;
;;; For error, there are all the same options as output plus:
;;; - :OUTPUT: redirect to the same place as output.
;;;
;;; RUN-PROGRAM returns a process struct for the process if the fork worked,
;;; and NIL if it did not.
;;;
(defun run-program (program args
		    &key (env *environment-list*) (wait t) pty input
		    if-input-does-not-exist output (if-output-exists :error)
		    (error :output) (if-error-exists :error) status-hook
		    before-execve)
  "Run-program creates a new process and runs the unix progam in the
   file specified by the simple-string program.  Args are the standard
   arguments that can be passed to a Unix program, for no arguments
   use NIL (which means just the name of the program is passed as arg 0).

   Run program will either return NIL or a PROCESS structure.  See the CMU
   Common Lisp Users Manual for details about the PROCESS structure.

   The keyword arguments have the following meanings:
     :env -
        An A-LIST mapping keyword environment variables to simple-string
	values.
     :wait -
        If non-NIL (default), wait until the created process finishes.  If
        NIL, continue running Lisp until the program finishes.
     :pty -
        Either T, NIL, or a stream.  Unless NIL, the subprocess is established
	under a PTY.  If :pty is a stream, all output to this pty is sent to
	this stream, otherwise the PROCESS-PTY slot is filled in with a stream
	connected to pty that can read output and write input.
     :input -
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
	input for the current process is inherited.  If NIL, /dev/null
	is used.  If a pathname, the file so specified is used.  If a stream,
	all the input is read from that stream and send to the subprocess.  If
	:STREAM, the PROCESS-INPUT slot is filled in with a stream that sends 
	its output to the process. Defaults to NIL.
     :if-input-does-not-exist (when :input is the name of a file) -
        can be one of:
           :error - generate an error.
           :create - create an empty file.
           nil (default) - return nil from run-program.
     :output -
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
	input for the current process is inherited.  If NIL, /dev/null
	is used.  If a pathname, the file so specified is used.  If a stream,
	all the output from the process is written to this stream. If
	:STREAM, the PROCESS-OUTPUT slot is filled in with a stream that can
	be read to get the output. Defaults to NIL.
     :if-output-exists (when :input is the name of a file) -
        can be one of:
           :error (default) - generates an error if the file already exists.
           :supersede - output from the program supersedes the file.
           :append - output from the program is appended to the file.
           nil - run-program returns nil without doing anything.
     :error and :if-error-exists - 
        Same as :output and :if-output-exists, except that :error can also be
	specified as :output in which case all error output is routed to the
	same place as normal output.
     :status-hook -
        This is a function the system calls whenever the status of the
        process changes.  The function takes the process as an argument.
     :before-execve -
        This is a function, without arguments, RUN-PROGRAM runs in the child
        process just before turning it into the specified program."

  ;; Make sure the interrupt handler is installed.
  (system:enable-interrupt mach:sigchld #'sigchld-handler)
  ;; Make sure all the args are okay.
  (unless (every #'simple-string-p args)
    (error "All args to program must be simple strings -- ~S." args))
  ;; Pre-pend the program to the argument list.
  (push (namestring program) args)
  ;; Clear random specials used by GET-DESCRIPTOR-FOR to communicate cleanup
  ;; info.  Also, establish proc at this level so we can return it.
  (let (*close-on-error* *close-in-parent* *handlers-installed* proc)
    (unwind-protect
	(let ((pfile (namestring (truename (merge-pathnames program "path:"))))
	      (cookie (list 0)))
	  (multiple-value-bind
	      (stdin input-stream)
	      (get-descriptor-for input cookie :direction :input
				  :if-does-not-exist if-input-does-not-exist)
	    (multiple-value-bind
		(stdout output-stream)
		(get-descriptor-for output cookie :direction :output
				    :if-exists if-output-exists)
	      (multiple-value-bind
		  (stderr error-stream)
		  (if (eq error :output)
		      (values stdout output-stream)
		      (get-descriptor-for error cookie :direction :output
					  :if-exists if-error-exists))
		(multiple-value-bind (pty-name pty-stream)
				     (open-pty pty cookie)
		  ;; Make sure we are not notified about the child death before
		  ;; we have installed the process struct in *active-processes*
		  (system:without-interrupts
		    (multiple-value-bind
			(child-pid errno)
			(mach:unix-fork)
		      (cond ((zerop child-pid)
			     ;; We are the child. Note: setup-child NEVER returns
			     (setup-child pfile args env stdin stdout stderr
					  pty-name before-execve))
			    ((minusp child-pid)
			     ;; This should only happen if the bozo has too
			     ;; many running procs.
			     (error "Could not fork child process: ~A"
				    (mach:get-unix-error-msg errno)))
			    (t
			     ;; We are the parent.
			     (setf proc (make-process :pid child-pid
						      :%status :running
						      :pty pty-stream
						      :input input-stream
						      :output output-stream
						      :error error-stream
						      :status-hook status-hook
						      :cookie cookie))
			     (push proc *active-processes*))))))))))
      (dolist (fd *close-in-parent*)
	(mach:unix-close fd))
      (unless proc
	(dolist (fd *close-on-error*)
	  (mach:unix-close fd))
	(dolist (handler *handlers-installed*)
	  (system:remove-fd-handler handler))))
    (when (and wait proc)
      (process-wait proc))
    proc))

;;; COPY-DESCRIPTOR-TO-STREAM -- internal
;;;
;;;   Installs a handler for any input that shows up on the file descriptor.
;;; The handler reads the data and writes it to the stream.
;;; 
(defun copy-descriptor-to-stream (descriptor stream cookie)
  (incf (car cookie))
  (let ((string (make-string 256))
	handler)
    (setf handler
	  (system:add-fd-handler descriptor :input
	    #'(lambda (fd)
		(declare (ignore fd))
		(loop
		  (multiple-value-bind
		      (result readable/errno)
		      (mach:unix-select (1+ descriptor) (ash 1 descriptor)
					0 0 0)
		    (cond ((null result)
			   (error "Could not select on sub-process: ~A"
				  (mach:get-unix-error-msg readable/errno)))
			  ((zerop result)
			   (return))))
		  (multiple-value-bind
		      (count errno)
		      (mach:unix-read descriptor
				      string
				      (length string))
		    (cond ((or (and (null count)
				    (eql errno mach:eio))
			       (eql count 0))
			   (system:remove-fd-handler handler)
			   (decf (car cookie))
			   (mach:unix-close descriptor)
			   (return))
			  ((null count)
			   (system:remove-fd-handler handler)
			   (decf (car cookie))
			   (error "Could not read input from sub-process: ~A"
				  (mach:get-unix-error-msg errno)))
			  (t
			   (write-string string stream
					 :end count))))))))))

;;; GET-DESCRIPTOR-FOR -- internal
;;;
;;;   Find a file descriptor to use for object given the direction. Returns
;;; the descriptor. If object is :STREAM, returns the created stream as the
;;; second value.
;;; 
(defun get-descriptor-for (object cookie &rest keys &key direction
				  &allow-other-keys)
  (cond ((eq object t)
	 ;; No new descriptor is needed.
	 (values nil nil))
	((eq object nil)
	 ;; Use /dev/null.
	 (multiple-value-bind
	     (fd errno)
	     (mach:unix-open "/dev/null"
			     (case direction
			       (:input mach:o_rdonly)
			       (:output mach:o_wronly)
			       (t mach:o_rdwr))
			     #o666)
	   (unless fd
	     (error "Could not open \"/dev/null\": ~A"
		    (mach:get-unix-error-msg errno)))
	   (push fd *close-in-parent*)
	   (values fd nil)))
	((eq object :stream)
	 (multiple-value-bind
	     (read-fd write-fd)
	     (mach:unix-pipe)
	   (unless read-fd
	     (error "Could not create pipe: ~A"
		    (mach:get-unix-error-msg write-fd)))
	   (case direction
	     (:input
	      (push read-fd *close-in-parent*)
	      (push write-fd *close-on-error*)
	      (let ((stream (system:make-fd-stream write-fd :output t)))
		(values read-fd stream)))
	     (:output
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (let ((stream (system:make-fd-stream read-fd :input t)))
		(values write-fd stream)))
	     (t
	      (mach:unix-close read-fd)
	      (mach:unix-close write-fd)
	      (error "Direction must be either :INPUT or :OUTPUT, not ~S"
		     direction)))))
	((or (pathnamep object) (stringp object))
	 (with-open-stream (file (apply #'open object keys))
	   (multiple-value-bind (won fd)
				(mach:unix-dup (system:fd-stream-fd file))
	     (cond (won
		    (push fd *close-in-parent*)
		    (values fd nil))
		   (t
		    (error "Could not duplicate file descriptor: ~A"
			   (mach:get-unix-error-msg fd)))))))
	((system:fd-stream-p object)
	 (values (system:fd-stream-fd object) nil))
	((streamp object)
	 (ecase direction
	   (:input
	    (dotimes (count
		      256
		      (error "Could not open a temporary file in /tmp"))
	      (let* ((name (format nil "/tmp/.run-program-~D" count))
		     (fd (mach:unix-open name
					 (logior mach:o_rdwr
						 mach:o_creat
						 mach:o_excl)
					 #o666)))
		(mach:unix-unlink name)
		(when fd
		  (let ((newline (string #\Newline)))
		    (loop
		      (multiple-value-bind
			  (line no-cr)
			  (read-line object nil nil)
			(unless line
			  (return))
			(mach:unix-write fd line 0 (length line))
			(if no-cr
			  (return)
			  (mach:unix-write fd newline 0 1)))))
		  (mach:unix-lseek fd 0 mach:l_set)
		  (push fd *close-in-parent*)
		  (return (values fd nil))))))
	   (:output
	    (multiple-value-bind (read-fd write-fd)
				 (mach:unix-pipe)
	      (unless read-fd
		(error "Cound not create pipe: ~A"
		       (mach:get-unix-error-msg write-fd)))
	      (copy-descriptor-to-stream read-fd object cookie)
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (values write-fd nil)))))
	(t
	 (error "Invalid option to run-program: ~S" object))))

