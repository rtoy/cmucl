;;; -*- Package: HEMLOCK; Mode: Lisp -*-
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/hemlock/rcs.lisp,v 1.16 1990/05/30 16:27:22 ch Exp $
;;;
;;; Various commands for dealing with RCS under Hemlock.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 
(in-package "HEMLOCK")


;;;;

(defun current-buffer-pathname ()
  (let ((pathname (buffer-pathname (current-buffer))))
    (unless pathname
      (editor-error "The buffer has no pathname."))
    pathname))


(defmacro in-directory (directory &body forms)
  (let ((cwd (gensym)))
    `(let ((,cwd (ext:default-directory)))
       (unwind-protect
	   (progn
	     (setf (ext:default-directory) (directory-namestring ,directory))
	     ,@forms)
	 (setf (ext:default-directory) ,cwd)))))


(defvar *last-rcs-command-name* nil)
(defvar *last-rcs-command-output-string* nil)
(defvar *rcs-output-stream* (make-string-output-stream))

(defmacro do-command (command &rest args)
  `(progn
     (setf *last-rcs-command-name* ',command)
     (get-output-stream-string *rcs-output-stream*)
     (let ((process (ext:run-program ',command ,@args
				     :error *rcs-output-stream*)))
       (setf *last-rcs-command-output-string*
	     (get-output-stream-string *rcs-output-stream*))
       (case (ext:process-status process)
	 (:exited
	  (unless (zerop (ext:process-exit-code process))
	    (editor-error "~A aborted with an error; ~
	    use the ``RCS Last Command Output'' command for more ~
	    information" ',command)))
	 (:signaled
	  (editor-error "~A killed with signal ~A~@[ (core dumped)]."
			',command
			(ext:process-exit-code process)
			(ext:process-core-dumped process)))
	 (t
	  (editor-error "~S still alive?" process))))))

(defun buffer-different-from-file (buffer filename)
  (with-open-file (file filename)
    (do ((buffer-line (mark-line (buffer-start-mark buffer))
		      (line-next buffer-line))
	 (file-line (read-line file nil nil)
		    (read-line file nil nil)))
	((and (or (null buffer-line)
		  (zerop (line-length buffer-line)))
	      (null file-line))
	 nil)
      (when (or (null buffer-line)
		(null file-line)
		(string/= (line-string buffer-line) file-line))
	(return t)))))

(defun turn-auto-save-off (buffer)
  (setf (buffer-minor-mode buffer "Save") nil)
  ;;
  ;; William's personal hack
  (when (getstring "Ckp" *mode-names*)
    (setf (buffer-minor-mode buffer "Ckp") nil)))


(defhvar "RCS Lock File Hook"
  "RCS Lock File Hook"
  :value nil)

(defun rcs-lock-file (buffer pathname)
  (message "Locking ~A ..." (namestring pathname))
  (in-directory pathname
    (let ((file (file-namestring pathname)))
      (do-command "rcs" `("-l" ,file))
      (multiple-value-bind (won dev ino mode) (mach:unix-stat file)
	(declare (ignore ino))
	(cond (won
	       (mach:unix-chmod file (logior mode mach:writeown)))
	      (t
	       (editor-error "MACH:UNIX-STAT lost in RCS-LOCK-FILE: ~A"
			     (mach:get-unix-error-msg dev)))))))
  (invoke-hook rcs-lock-file-hook buffer pathname))


(defhvar "RCS Unlock File Hook"
  "RCS Unlock File Hook"
  :value nil)

(defun rcs-unlock-file (buffer pathname)
  (message "Unlocking ~A ..." (namestring pathname))
  (in-directory pathname
    (do-command "rcs" `("-u" ,(file-namestring pathname))))
  (invoke-hook rcs-unlock-file-hook buffer pathname))


;;;; Check In

(defhvar "RCS Check In File Hook"
  "RCS Check In File Hook"
  :value nil)

(defun rcs-check-in-file (buffer pathname keep-lock)
  (let ((old-buffer (current-buffer))
	(allow-delete nil)
	(log-buffer nil))
    (unwind-protect
	(when (block in-recursive-edit
		(do ((i 0 (1+ i)))
		    ((not (null log-buffer)))
		  (setf log-buffer
			(make-buffer
			 (format nil "RCS Log Entry ~D for ~S" i
				 (file-namestring pathname))
			 :modes '("Text")
			 :delete-hook
			 (list #'(lambda (buffer)
				   (declare (ignore buffer))
				   (unless allow-delete
				     (return-from in-recursive-edit t)))))))
		(turn-auto-save-off log-buffer)
		(change-to-buffer log-buffer)
		(do-recursive-edit)
	  
		(message "Checking in ~A~:[~; keeping the lock~] ..."
			 (namestring pathname) keep-lock)
		(let ((log-stream (make-hemlock-region-stream
				   (buffer-region log-buffer))))
		  (sub-check-in-file pathname keep-lock log-stream))
		(invoke-hook rcs-check-in-file-hook buffer pathname)
		nil)
	  (editor-error "Someone deleted the RCS Log Entry buffer."))
      (change-to-buffer old-buffer)
      (setf allow-delete t)
      (delete-buffer-if-possible log-buffer))))

(defun sub-check-in-file (pathname keep-lock log-stream)
  (let* ((filename (file-namestring pathname))
	 (rcs-filename (concatenate 'simple-string
				    "./RCS/" filename ",v")))
    (in-directory pathname
      (do-command "rcsci" `(,@(if keep-lock '("-l"))
			      "-u"
			      ,filename)
		  :input log-stream)
      ;; 
      ;; Set the times on the user's file to be equivalent to that of
      ;; the rcs file.
      (multiple-value-bind
	  (dev ino mode nlink uid gid rdev size atime mtime)
	  (mach:unix-stat rcs-filename)
	(declare (ignore mode nlink uid gid rdev size))
	(cond (dev
	       (multiple-value-bind
		   (wonp errno)
		   (mach:unix-utimes filename (list atime 0 mtime 0))
		 (unless wonp
		   (editor-error "MACH:UNIX-UTIMES failed: ~A"
				 (mach:get-unix-error-msg errno)))))
	      (t
	       (editor-error "MACH:UNIX-STAT failed: ~A"
			     (mach:get-unix-error-msg ino))))))))


;;;; Check Out

(defhvar "RCS Check Out File Hook"
  "RCS Check Out File Hook"
  :value nil)

(defun maybe-rcs-check-out-file (buffer pathname lock always-overwrite-p)
  (sub-maybe-rcs-check-out-files buffer (list pathname)
				 lock always-overwrite-p))

(defun maybe-rcs-check-out-files (pathnames lock always-overwrite-p)
  (sub-maybe-rcs-check-out-files nil pathnames lock always-overwrite-p))

(defun sub-maybe-rcs-check-out-files (buffer pathnames lock always-overwrite-p)
  (let ((check-out-count 0))
    (macrolet ((frob ()
		 `(progn
		    (rcs-check-out-file buffer pathname lock)
		    (incf check-out-count))))
      (dolist (pathname pathnames)
	(cond
	 ((and (not always-overwrite-p)
	       (probe-file pathname) (ext:file-writable pathname))
	  ;; File exists and is writable so check and see if the user really
	  ;; wants to check it out.
	  (command-case (:prompt
			 (format nil "The file ~A is writable.  Overwrite? "
				 (file-namestring pathname))
			 :help
			 "Type one of the following single-character commands:")
	    ((:yes :confirm)
	     "Overwrite the file."
	     (frob))
	    (:no
	     "Skip checking out this file.")
	    ((#\r #\R)
	     "Rename the file before checking it out."
	     (let ((new-pathname (prompt-for-file
				  :prompt "New Filename: "
				  :default (buffer-default-pathname
					      (current-buffer))
				  :must-exist nil)))
	       (rename-file pathname new-pathname)
	       (frob)))
	    (:do-all
	     "Overwrite this file and all remaining files."
	     (setf always-overwrite-p t)
	     (frob))
	    (:do-once
	     "Overwrite this file and then exit."
	     (frob)
	     (return))))
	 (t
	  (frob)))))
    check-out-count))

(defun rcs-check-out-file (buffer pathname lock)
  (message "Checking out ~A~:[~; with a lock~] ..." (namestring pathname) lock)
  (in-directory pathname
    (let ((backup
	   (if (probe-file pathname)
	       (lisp::pick-backup-name (namestring pathname))
	       nil)))
      (when backup (rename-file pathname backup))
      (do-command "rcsco" `(,@(if lock '("-l")) ,(file-namestring pathname)))
      (invoke-hook rcs-check-out-file-hook buffer pathname)
      (when backup (delete-file backup)))))

(defun pick-temp-file (defaults)
  (let ((index 0))
    (loop
      (let ((name (merge-pathnames (format nil ",rcstmp-~D" index) defaults)))
	(cond ((probe-file name)
	       (incf index))
	      (t
	       (return name)))))))


;;;; Last Command Output

(defcommand "RCS Last Command Output" (p)
  "Print the full output of the last RCS command."
  "Print the full output of the last RCS command."
  (declare (ignore p))
  (unless (and *last-rcs-command-name* *last-rcs-command-output-string*)
    (editor-error "No RCS commands have executed!"))
  (with-pop-up-display (s :buffer-name "*RCS Command Output*")
    (format s "Output from ``~A'':~%~%" *last-rcs-command-name*)
    (write-line *last-rcs-command-output-string* s)))


;;;; Checking In / Checking Out and Locking / Unlocking 

(defcommand "RCS Lock Buffer File" (p)
  "Attempt to lock the file in the current buffer."
  "Attempt to lock the file in the current buffer."
  (declare (ignore p))
  (let ((file (current-buffer-pathname))
	(buffer (current-buffer))
	(name (pick-temp-file "/tmp/")))
    (rcs-lock-file buffer file)
    (unwind-protect
	(progn
	  (in-directory file
  	    (do-command "rcsco" `("-p" ,(file-namestring file))
			:output (namestring name)))
	  (when (buffer-different-from-file buffer name)
	    (message
	     "RCS file is different; be sure to merge in your changes."))
	  (setf (buffer-writable buffer) t)
	  (message "Buffer is now writable."))
      (when (probe-file name)
	(delete-file name)))))

(defcommand "RCS Lock File" (p)
  "Prompt for a file, and attempt to lock it."
  "Prompt for a file, and attempt to lock it."
  (declare (ignore p))
  (rcs-lock-file nil (prompt-for-file :prompt "File to lock: "
				      :default (buffer-default-pathname
						(current-buffer))
				      :must-exist nil)))

(defcommand "RCS Unlock Buffer File" (p)
  "Unlock the file in the current buffer."
  "Unlock the file in the current buffer."
  (declare (ignore p))
  (rcs-unlock-file (current-buffer) (current-buffer-pathname))
  (setf (buffer-writable (current-buffer)) nil)
  (message "Buffer is no longer writable."))

(defcommand "RCS Unlock File" (p)
  "Prompt for a file, and attempt to unlock it."
  "Prompt for a file, and attempt to unlock it."
  (declare (ignore p))
  (rcs-unlock-file nil (prompt-for-file :prompt "File to unlock: "
					:default (buffer-default-pathname
						  (current-buffer))
					:must-exist nil)))

(defcommand "RCS Check In Buffer File" (p)
  "Checkin the file in the current buffer.  With an argument, do not
  release the lock."
  "Checkin the file in the current buffer.  With an argument, do not
  release the lock."
  (let ((buffer (current-buffer))
	(pathname (current-buffer-pathname)))
    (when (buffer-modified buffer)
      (save-file-command nil))
    (rcs-check-in-file buffer pathname p)
    (visit-file-command nil pathname buffer)))

(defcommand "RCS Check In File" (p)
  "Prompt for a file, and attempt to check it in.  With an argument, do
  not release the lock."
  "Prompt for a file, and attempt to check it in.  With an argument, do
  not release the lock."
  (rcs-check-in-file nil (prompt-for-file :prompt "File to lock: "
					  :default
					  (buffer-default-pathname
					   (current-buffer))
					  :must-exist nil)
		     p))

(defcommand "RCS Check Out Buffer File" (p)
  "Checkout the file in the current buffer.  With an argument, lock the
  file."
  "Checkout the file in the current buffer.  With an argument, lock the
  file."
  (let* ((buffer (current-buffer))
	 (pathname (current-buffer-pathname))
	 (point (current-point))
	 (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (when (buffer-modified buffer)
      (when (not (prompt-for-y-or-n :prompt "Buffer is modified, overwrite? "))
	(editor-error "Aborted.")))
    (maybe-rcs-check-out-file buffer pathname p nil)
    (setf (buffer-modified buffer) nil)
    (when p
      (setf (buffer-writable buffer) t)
      (message "Buffer is now writable."))
    (visit-file-command nil pathname)
    (unless (line-offset point lines)
      (buffer-end point))))

(defcommand "RCS Check Out File" (p)
  "Prompt for a file and attempt to check it out.  With an argument,
  lock the file."
  "Prompt for a file and attempt to check it out.  With an argument,
  lock the file."
  (let ((pathname (prompt-for-file :prompt "File to check out: "
				   :default (buffer-default-pathname
					     (current-buffer))
				   :must-exist nil)))
    (maybe-rcs-check-out-file nil pathname p nil)
    (find-file-command nil pathname)))


;;;; Log File

(defhvar "RCS Log Entry Buffer"
  "Name of the buffer to put RCS log entries into."
  :value "RCS Log")

(defhvar "RCS Log Buffer Hook"
  "RCS Log Buffer Hook"
  :value nil)

(defun get-log-buffer ()
  (let ((buffer (getstring (value rcs-log-entry-buffer) *buffer-names*)))
    (unless buffer
      (setf buffer (make-buffer (value rcs-log-entry-buffer)))
      (turn-auto-save-off buffer)
      (invoke-hook rcs-log-buffer-hook buffer))
    buffer))

(defcommand "RCS Buffer File Log Entry" (p)
  "Get the RCS Log for the file in the current buffer in a buffer."
  "Get the RCS Log for the file in the current buffer in a buffer."
  (declare (ignore p))
  (let ((buffer (get-log-buffer))
	(pathname (current-buffer-pathname)))
    (delete-region (buffer-region buffer))
    (message "Extracting log info ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (in-directory pathname
	(do-command "rlog" (list (file-namestring pathname))
		    :output (make-hemlock-output-stream mark))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))

(defcommand "RCS File Log Entry" (p)
  "Prompt for a file and get its RCS log entry in a buffer."
  "Prompt for a file and get its RCS log entry in a buffer."
  (declare (ignore p))
  (let ((file (prompt-for-file :prompt "File to get log of: "
			       :default (buffer-default-pathname
					 (current-buffer))
			       :must-exist nil))
	(buffer (get-log-buffer)))
    (delete-region (buffer-region buffer))
    (message "Extracing log info ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (in-directory file
	(do-command "rlog" (list (file-namestring file))
		    :output (make-hemlock-output-stream mark))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))


;;;; Directory Support

(defun list-out-of-date-files (dir)
  (let ((rcsdir (make-pathname :host (pathname-host dir)
			       :device (pathname-device dir)
			       :directory (concatenate 'simple-vector
						       (pathname-directory dir)
						       (vector "RCS"))))
	(out-of-date-files nil))
    (unless (directoryp rcsdir)
      (editor-error "Could not find the RCS directory."))
    (dolist (rcsfile (directory rcsdir))
      (let ((rcsname (file-namestring rcsfile)))
	(when (string= rcsname ",v" :start1 (- (length rcsname) 2))
	  (let* ((name (subseq rcsname 0 (- (length rcsname) 2)))
		 (file (merge-pathnames (parse-namestring name) dir)))
	    (unless (and (probe-file file)
			 (>= (file-write-date file) (file-write-date rcsfile)))
	      (push file out-of-date-files))))))
    out-of-date-files))

(defun rcs-prompt-for-directory (prompt)
  (let* ((default (buffer-default-pathname (current-buffer)))
	 (dir (prompt-for-file :prompt prompt
			       :default (make-pathname
					 :host (pathname-host default)
					 :device (pathname-device default)
					 :directory (pathname-directory default)
					 :defaults nil)
			       :must-exist nil)))
    (unless (directoryp dir)
      (let ((with-slash (parse-namestring (concatenate 'simple-string
						       (namestring dir)
						       "/"))))
	(unless (directoryp with-slash)
	  (editor-error "~S is not a directory" (namestring dir)))
	(setf dir with-slash)))
    dir))

(defcommand "RCS Update Directory" (p)
  "Prompt for a directory and check out all files that are older than
  their corresponding RCS files.  With an argument, never ask about
  overwriting writable files."
  "Prompt for a directory and check out all files that are older than
  the corresponding RCS file.  With an argument, never ask about
  overwriting writable files."
  (let* ((directory (rcs-prompt-for-directory "Directory to update: "))
	 (out-of-date-files (list-out-of-date-files directory))
	 (n-out-of-date (length out-of-date-files)))
    (cond ((zerop n-out-of-date)
	   (message "All RCS files in ~A are up to date."
		    (namestring directory)))
	  (t
	   (let ((n-checked-out
		  (maybe-rcs-check-out-files out-of-date-files nil p)))
	     (message "Number of files out of date: ~D; ~
	     number of files checked out: ~D"
		      n-out-of-date n-checked-out)))))))

(defcommand "RCS List Out Of Date Files" (p)
  "Prompt for a directory and list all of the files that are older than
  their corresponding RCS files."
  "Prompt for a directory and list all of the files that are older than
  their corresponding RCS files."
  (declare (ignore p))
  (let* ((directory (rcs-prompt-for-directory "Directory: "))
	 (out-of-date-files (list-out-of-date-files directory)))
    (cond ((null out-of-date-files)
	   (message "All RCS files in ~A are up to date."
		    (namestring directory)))
	  (t
	   (with-pop-up-display (s :buffer-name "*RCS Out of Date Files*")
	     (format s "Directory: ~A~%~%" (namestring directory))
	     (dolist (file out-of-date-files)
	       (format s "~A~%" (file-namestring file))))))))


;;;; Status and Modeline Frobs.

(defhvar "RCS Status"
  "RCS status of this buffer.  Either nil, :locked, :out-of-date, or
  :unlocked."
  :value nil)

;;;
;;; Note: This doesn't behave correctly w/r/t to branched files.
;;; 
(defun rcs-file-status (pathname)
  (let* ((directory (directory-namestring pathname))
	 (filename (file-namestring pathname))
	 (rcs-file (concatenate 'simple-string directory
				"RCS/" filename ",v")))
    (if (probe-file rcs-file)
	;; This is an RCS file
	(let ((probe-file (probe-file pathname)))
	  (cond ((and probe-file (file-writable pathname))
		 :locked)
		((or (not probe-file)
		     (< (file-write-date pathname)
			(file-write-date rcs-file)))
		 :out-of-date)
		(t
		 :unlocked))))))

(defun rcs-update-buffer-status (buffer &optional tn)
  (unless (hemlock-bound-p 'rcs-status :buffer buffer)
    (defhvar "RCS Status"
      "RCS Status of this buffer."
      :buffer buffer
      :value nil))
  (let ((tn (or tn (buffer-pathname buffer))))
    (setf (variable-value 'rcs-status :buffer buffer)
	  (if tn (rcs-file-status tn))))
  (hi::update-modelines-for-buffer buffer))
;;; 
(add-hook read-file-hook 'rcs-update-buffer-status)
(add-hook write-file-hook 'rcs-update-buffer-status)

(defcommand "RCS Update All RCS Status Variables" (p)
  "Update the ``RCS Status'' variable for all buffers."
  "Update the ``RCS Status'' variable for all buffers."
  (declare (ignore p))
  (dolist (buffer *buffer-list*)
    (rcs-update-buffer-status buffer))
  (dolist (window *window-list*)
    (update-modeline-fields (window-buffer window) window)))

;;; 
;;; Action Hooks
(defun rcs-action-hook (buffer pathname)
  (cond (buffer
	 (rcs-update-buffer-status buffer))
	(t
	 (let ((pathname (probe-file pathname)))
	   (when pathname
	     (dolist (buffer *buffer-list*)
	       (let ((buffer-pathname (buffer-pathname buffer)))
		 (when (equal pathname buffer-pathname)
		   (rcs-update-buffer-status buffer)))))))))
;;; 
(add-hook rcs-check-in-file-hook 'rcs-action-hook)
(add-hook rcs-check-out-file-hook 'rcs-action-hook)
(add-hook rcs-lock-file-hook 'rcs-action-hook)
(add-hook rcs-unlock-file-hook 'rcs-action-hook)


;;;
;;; RCS Modeline Field
(make-modeline-field
 :name :rcs-status
 :function #'(lambda (buffer window)
	       (declare (ignore buffer window))
	       (ecase (value rcs-status)
		 (:out-of-date "[OLD]  ")
		 (:locked "[LOCKED]  ")
		 (:unlocked "[RCS]  ")
		 ((nil) ""))))
