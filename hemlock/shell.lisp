;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Hemlock command level support for processes.
;;;
;;; Written by Blaine Burks.
;;;

(in-package "HEMLOCK")


(defun setup-process-buffer (buffer)
  (let ((mark (copy-mark (buffer-point buffer) :right-inserting)))
    (defhvar "Buffer Input Mark"
      "The buffer input mark for this buffer."
      :buffer buffer
      :value mark)
    (defhvar "Process Output Stream"
      "The process structure for this buffer."
      :buffer buffer
      :value (make-hemlock-output-stream mark))
    (defhvar "Interactive History"
      "A ring of the regions input to an interactive mode (Eval or Typescript)."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))
    (defhvar "Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)
    (defhvar "Searching Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)
    (unless (buffer-modeline-field-p buffer :process-status)
      (setf (buffer-modeline-fields buffer)
	    (nconc (buffer-modeline-fields buffer)
		   (list (modeline-field :process-status)))))))

(defmode "Process" :major-p nil :setup-function #'setup-process-buffer)


;;;; Support for handling input before the prompt in process buffers.

(defun unwedge-process-buffer ()
  (buffer-end (current-point))
  (deliver-signal-to-process :SIGINT (value process))
  (editor-error "Aborted."))

(defhvar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-process-buffer
  :mode "Process")

(defhvar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Interrupt and throw to end of buffer?"
  :mode "Process")


;;;; Some Global Variables.

(defhvar "Current Shell"
  "The shell that  \"Select Shell\" will zap you to.")

(defhvar "Kill Process Confirm"
  "When non-nil, ask the user whether he really wants to blow away the shell. ~
   Otherwise, just blow it away."
  :value T)

(defhvar "Shell Utility"
  "The shell command uses this as the default command line."
  :value "/bin/csh")

(defhvar "Shell Utility Switches"
  "This is list of strings that are the default command line arguments to the
   utility in \"Shell Utility\"."
  :value nil)



;;;; The Shell and New Shell Commands.

(defcommand "Shell" (p)
  "If a shell buffer exists, pop to it.  Otherwise creates a new one and pop to
   it.  With an argument, prompt for a command and buffer to execute it in."
  "If a shell buffer exists, pop to it.  Otherwise creates a new one and pop to
   it.  With an argument, prompt for a command and buffer to execute it in."
  (let ((shell (value current-shell)))
    (if shell (change-to-buffer shell) (make-new-shell p))))

(defcommand "New Shell" (p)
  "Creates a new shell and puts you in it."
  "Creates a new shell and puts you in it."
  (make-new-shell p))

(defun make-new-shell (prompt-for-command-p
		       &optional (command-line (get-command-line) clp))
  (let* ((command (or (and clp command-line)
		      (if prompt-for-command-p
			  (prompt-for-string
			   :default command-line :trim t
			   :prompt "Command to execute: "
			   :help "Shell command line to execute.")
			  command-line)))
	 (buffer-name (if prompt-for-command-p
			  (prompt-for-string
			   :default
			   (concatenate 'simple-string command " process")
			   :trim t
			   :prompt `("Buffer in which to execute ~A? "
				     ,command)
			   :help "Where output from this process will appear.")
			  (new-shell-name)))
	 (buffer (make-buffer
		  buffer-name
		  :modes '("Fundamental" "Process")
		  :delete-hook
		  (list #'(lambda (buffer)
			    (when (eq (value current-shell) buffer)
			      (setf (value current-shell) nil))
			    (kill-process (variable-value 'process
							  :buffer buffer)))))))
    (unless buffer
      (setf buffer (getstring buffer-name *buffer-names*))
      (buffer-end (buffer-point buffer)))
    (defhvar "Process"
      "The process for Shell and Process buffers."
      :buffer buffer
      :value (ext::run-program "/bin/sh" (list "-c" command)
			       :wait nil
			       :pty (variable-value 'process-output-stream
						    :buffer buffer)
			       :env (frob-environment-list
				     (car (buffer-windows buffer)))
			       :status-hook #'(lambda (process)
						(declare (ignore process))
						(update-process-buffer buffer))
			       :input t :output t))
    (update-process-buffer buffer)
    (unless (value current-shell)
      (setf (value current-shell) buffer))
    (change-to-buffer buffer)))

;;; GET-COMMAND-LINE -- Internal.
;;;
;;; This just conses up a string to feed to the shell.
;;;
(defun get-command-line ()
  (concatenate 'simple-string (value shell-utility) " "
	       (value shell-utility-switches)))

;;; FROB-ENVIRONMENT-LIST -- Internal.
;;;
;;; This sets some environment variables so the shell will be in the proper
;;; state when it comes up.
;;;
(defun frob-environment-list (window)
  (list* (cons :termcap  (concatenate 'simple-string
				      "emacs:co#"
				      (if window
					  (lisp::quick-integer-to-string
					   (window-width window))
					  "")
				      ":tc=unkown:"))
	 (cons :emacs "t") (cons :term "emacs")
	 (remove-if #'(lambda (keyword)
			(member keyword '(:termcap :emacs :term)
				:test #'(lambda (cons keyword)
					  (eql (car cons) keyword))))
		    ext:*environment-list*)))

;;; NEW-SHELL-NAME -- Internal.
;;;
;;; This returns a unique buffer name for a shell by incrementing the value of
;;; *process-number* until "Process <*process-number*> is not already the name
;;; of a buffer.  Perhaps this is being overly cautious, but I've seen some
;;; really stupid users.
;;;
(defvar *process-number* 0)
;;;
(defun new-shell-name ()
  (loop
    (let ((buffer-name (format nil "Shell ~D" (incf *process-number*))))
      (unless (getstring buffer-name *buffer-names*) (return buffer-name)))))


;;;; Modeline support.

(defun modeline-process-status (buffer window)
  (declare (ignore window))
  (when (hemlock-bound-p 'process :buffer buffer)
    (let ((process (variable-value 'process :buffer buffer)))
      (ecase (ext:process-status process)
	(:running "running")
	(:stopped "stopped")
	(:signaled "killed by signal ~D" (mach:unix-signal-name
					  (ext:process-exit-code process)))
	(:exited (format nil "exited with status ~D"
			 (ext:process-exit-code process)))))))
			 

(make-modeline-field :name :process-status
		     :function #'modeline-process-status)

(defun update-process-buffer (buffer)
  (when (buffer-modeline-field-p buffer :process-status)
    (dolist (window (buffer-windows buffer))
      (update-modeline-field buffer window :process-status)))
  (let ((process (variable-value 'process :buffer buffer)))
    (unless (ext:process-alive-p process)
      (ext:process-close process)
      (setf (value current-shell) nil))))


;;;; Supporting Commands.

(defcommand "Confirm Process Input" (p)
  "Evaluate Process Mode input between the point and last prompt."
  "Evaluate Process Mode input between the point and last prompt."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (let* ((process (value process))
	 (stream (ext:process-pty process)))
    (case (ext:process-status process)
      (:running)
      (:stopped (editor-error "The process has been stopped."))
      (t (editor-error "The process is dead.")))
    (let ((input-region (get-interactive-input)))
      (write-line (region-to-string input-region) stream)
      (force-output (ext:process-pty process))
      (insert-character (current-point) #\newline)
      ;; Move "Buffer Input Mark" to end of buffer.
      (move-mark (region-start input-region) (region-end input-region)))))

(defcommand "Kill Main Process" (p)
  "Kills the process in the current buffer."
  "Kills the process in the current buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (when (or (not (value kill-process-confirm))
	    (prompt-for-y-or-n :default nil
			       :prompt "Really blow away shell? "
			       :default nil
			       :default-string "no"))
    (kill-process (value process))))

(defcommand "Stop Main Process" (p)
  "Stops the process in the current buffer.  With an argument use :SIGSTOP
   instead of :SIGTSTP."
  "Stops the process in the current buffer.  With an argument use :SIGSTOP
  instead of :SIGTSTP."
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (deliver-signal-to-process (if p :SIGSTOP :SIGTSTP) (value process)))

(defcommand "Continue Main Process" (p)
  "Continues the process in the current buffer."
  "Continues the process in the current buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (deliver-signal-to-process :SIGCONT (value process)))
  
(defun kill-process (process)
  "Self-explanatory."
  (deliver-signal-to-process :SIGKILL process))

(defun deliver-signal-to-process (signal process)
  "Delivers a signal to a process."
  (ext:process-kill process signal :process-group))

(defcommand "Send EOF to Process" (p)
  "Sends a Ctrl-D to the process in the current buffer."
  "Sends a Ctrl-D to the process in the current buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (let ((stream (ext:process-pty (value process))))
    (write-char (int-char 4) stream)
    (force-output stream)))

(defcommand "Interrupt Buffer Subprocess" (p)
  "Stop the subprocess currently executing in this shell."
  "Stop the subprocess currently executing in this shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (buffer-end (current-point))
  (buffer-end (value buffer-input-mark))
  (deliver-signal-to-subprocess :SIGINT (value process)))

(defcommand "Kill Buffer Subprocess" (p)
  "Kill the subprocess currently executing in this shell."
  "Kill the subprocess currently executing in this shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))  
  (deliver-signal-to-subprocess :SIGKILL (value process)))

(defcommand "Quit Buffer Subprocess" (p)
  "Quit the subprocess currently executing int his shell."
  "Quit the subprocess currently executing int his shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (deliver-signal-to-subprocess :SIGQUIT (value process)))

(defcommand "Stop Buffer Subprocess" (p)
  "Stop the subprocess currently executing in this shell."
  "Stop the subprocess currently executing in this shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))  
  (deliver-signal-to-subprocess (if p :SIGSTOP :SIGTSTP) (value process)))

(defun deliver-signal-to-subprocess (signal process)
  "Delivers a signal to a subprocess of a shell."
  (ext:process-kill process signal :pty-process-group))
