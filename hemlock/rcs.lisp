;;; -*- Package: HEMLOCK -*-
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/hemlock/rcs.lisp,v 1.4 1990/02/09 21:08:09 wlott Exp $
;;;
;;; Various commands for dealing with RCS under hemlock.
;;; 

(in-package "HEMLOCK")


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


(defvar *error-stream* (make-string-output-stream))

(defmacro do-command (&rest args)
  (let ((proc (gensym)))
    `(progn
       (get-output-stream-string *error-stream*)
       (let ((,proc (ext:run-program ,@args :error *error-stream*)))
	 (case (ext:process-status ,proc)
	   (:exited
	    (unless (zerop (ext:process-exit-code ,proc))
	      (editor-error "~A" (get-output-stream-string *error-stream*))))
	   (:signaled
	    (editor-error "~A killed with signal ~A ~@[core dumped]"
			  ',(car args)
			  (ext:process-exit-code ,proc)
			  (ext:process-core-dumped ,proc)))
	   (t
	    (editor-error "~S still alive?" ,proc)))))))

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

(defun rcs-lock-file (pathname)
  (message "Locking ~A ..." (namestring pathname))
  (in-directory pathname
    (let ((file (file-namestring pathname)))
      (do-command "rcs" `("-l" ,file))
      (multiple-value-bind
	  (won dev ino mode)
	  (mach:unix-stat file)
	(declare (ignore dev ino))
	(when won
	  (mach:unix-chmod file (logior mode mach:writeown)))))))

(defun rcs-unlock-file (pathname)
  (message "Unlocking ~A ..." (namestring pathname))
  (in-directory pathname
    (do-command "rcs" `("-u" ,(file-namestring pathname)))))

(defun rcs-check-in-file (pathname keep-lock)
  (let ((old-buffer (current-buffer))
	(allow-delete nil)
	(buffer nil))
    (unwind-protect
	(when (block in-recursive-edit
		(do ((i 0 (1+ i)))
		    ((not (null buffer)))
		  (setf buffer
			(make-buffer (format nil "RCS Log Entry ~D for ~S"
					     i (file-namestring pathname))
				     :modes '("Text")
				     :delete-hook
				     (list #'(lambda (buffer)
					       (declare (ignore buffer))
					       (unless allow-delete
						 (return-from in-recursive-edit
							      t)))))))
		(turn-auto-save-off buffer)
		(change-to-buffer buffer)
		(do-recursive-edit)
	  
		(message "Checking in ~A ..." (namestring pathname))
		(in-directory pathname
		  (do-command "rcsci" `(,@(if keep-lock '("-l"))
					"-u"
					,(file-namestring pathname))
			      :input (make-hemlock-region-stream
				      (buffer-region buffer))))
		nil)
	  (editor-error "Someone deleted the RCS Log Entry buffer."))
      (change-to-buffer old-buffer)
      (setf allow-delete t)
      (delete-buffer buffer))))

(defun rcs-check-out-file (pathname lock)
  (message "Checking out ~A ..." (namestring pathname))
  (in-directory pathname
    (let ((backup
	   (if (probe-file pathname)
	       (lisp::pick-backup-name (namestring pathname))
	       nil)))
      (when backup
	(rename-file pathname backup))
      (do-command "rcsco" `(,@(if lock '("-l")) ,(file-namestring pathname)))
      (when backup
	(delete-file backup)))))



(defun pick-temp-file (defaults)
  (let ((index 0))
    (loop
      (let ((name (merge-pathnames (format nil ",rcstmp-~D" index) defaults)))
	(cond ((probe-file name)
	       (incf index))
	      (t
	       (return name)))))))

(defcommand "RCS Lock Buffer File" (p)
  "Attempt to lock the file in the current buffer."
  "Attempt to lock the file in the current buffer."
  (declare (ignore p))
  (let ((file (current-buffer-pathname))
	(buffer (current-buffer))
	(name (pick-temp-file "/tmp/")))
    (rcs-lock-file file)
    (unwind-protect
	(progn
	  (in-directory file
	    (do-command "rcsco" `("-p" ,(file-namestring file))
			:output (namestring name)))
	  (when (buffer-different-from-file buffer name)
	    (message
	     "RCS file is different: be sure to merge in your changes."))
	  (setf (buffer-writable buffer) t)
	  (message "Buffer is now writable."))
      (when (probe-file name)
	(delete-file name)))))

(defcommand "RCS Lock File" (p)
  "Prompt for a file, and attempt to lock it."
  "Prompt for a file, and attempt to lock it."
  (declare (ignore p))
  (rcs-lock-file (prompt-for-file :prompt "File to lock: "
				  :default (buffer-default-pathname
					    (current-buffer))
				  :must-exist nil)))

(defcommand "RCS Unlock Buffer File" (p)
  "Unlock the file in the current buffer."
  "Unlock the file in the current buffer."
  (declare (ignore p))
  (rcs-unlock-file (current-buffer-pathname))
  (setf (buffer-writable (current-buffer)) nil)
  (message "Buffer is no longer writable."))

(defcommand "RCS Unlock File" (p)
  "Prompt for a file, and attempt to unlock it."
  "Prompt for a file, and attempt to unlock it."
  (declare (ignore p))
  (rcs-unlock-file (prompt-for-file :prompt "File to unlock: "
				    :default (buffer-default-pathname
					      (current-buffer))
				    :must-exist nil)))

(defcommand "RCS Check In Buffer File" (p)
  "Checkin the file in the current buffer.  With an argument, do not release
  the lock."
  "Checkin the file in the current buffer.  With an argument, do not release
  the lock."
  (let ((buffer (current-buffer))
	(pathname (current-buffer-pathname)))
    (when (buffer-modified buffer)
      (save-file-command nil))
    (rcs-check-in-file pathname p)
    (visit-file-command nil pathname buffer)))

(defcommand "RCS Check In File" (p)
  "Prompt for a file, and attempt to check it in.  With an argument, do not
  release the lock."
  "Prompt for a file, and attempt to check it in.  With an argument, do not
  release the lock."
  (rcs-check-in-file (prompt-for-file :prompt "File to lock: "
				      :default
				         (buffer-default-pathname
					  (current-buffer))
				      :must-exist nil)
		     p))

(defcommand "RCS Check Out Buffer File" (p)
  "Checkout the file in the current buffer.  With an argument, lock the file."
  "Checkout the file in the current buffer.  With an argument, lock the file."
  (let* ((buffer (current-buffer))
	 (pathname (current-buffer-pathname))
	 (point (current-point))
	 (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (when (buffer-modified buffer)
      (when (not (prompt-for-y-or-n :prompt "Buffer is modified, overwrite? "))
	(editor-error "Aborted.")))
    (rcs-check-out-file pathname p)
    (setf (buffer-modified buffer) nil)
    (when p
      (setf (buffer-writable buffer) t)
      (message "Buffer is now writable."))
    (visit-file-command nil pathname)
    (unless (line-offset point lines)
      (buffer-end point))))

(defcommand "RCS Check Out File" (p)
  "Prompt for a file and attempt to check it out.  With an argument, lock the
  file."
  "Prompt for a file and attempt to check it out.  With an argument, lock the
  file."
  (let ((pathname (prompt-for-file :prompt "File to check out: "
				   :default (buffer-default-pathname
					     (current-buffer))
				   :must-exist nil)))
    (rcs-check-out-file pathname p)
    (find-file-command nil pathname)))

(defhvar "RCS Log Entry Buffer"
  "Name of the buffer to put RCS log entries into."
  :value "RCS Log")

(defun get-log-buffer ()
  (let ((buffer (getstring (value rcs-log-entry-buffer) *buffer-names*)))
    (unless buffer
      (setf buffer (make-buffer (value rcs-log-entry-buffer)))
      (turn-auto-save-off buffer))
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


(defcommand "RCS Update Directory" (p)
  "Prompt for a directory and check out all files that are older than the
  corresponding RCS file.  With an argument, never ask about overwriting
  writable files."
  "Prompt for a directory and check out all files that are older than the
  corresponding RCS file.  With an argument, never ask about overwriting
  writable files."
  (let* ((def (buffer-default-pathname (current-buffer)))
	 (dir (prompt-for-file :prompt "Directory to update: "
			       :default (make-pathname
					 :host (pathname-host def)
					 :device (pathname-device def)
					 :directory (pathname-directory def)
					 :defaults nil)
			       :must-exist nil)))
    (unless (directoryp dir)
      (let ((with-slash (parse-namestring (concatenate 'simple-string
						       (namestring dir)
						       "/"))))
	(unless (directoryp with-slash)
	  (editor-error "~S is not a directory" (namestring dir)))
	(setf dir with-slash)))
    (let ((rcsdir
	   (make-pathname :host (pathname-host dir)
			  :device (pathname-device dir)
			  :directory (concatenate 'simple-vector
						  (pathname-directory dir)
						  (vector "RCS"))))
	  (did-any nil))
      (unless (directoryp rcsdir)
	(editor-error "Could not find the RCS directory."))
      (dolist (rcsfile (directory rcsdir))
	(let ((rcsname (file-namestring rcsfile)))
	  (when (string= rcsname ",v" :start1 (- (length rcsname) 2))
	    (let* ((name (subseq rcsname 0 (- (length rcsname) 2)))
		   (file (merge-pathnames (parse-namestring name)
					  dir)))
	      (when (< (file-write-date file) (file-write-date rcsfile))
		(multiple-value-bind
		    (won dev inode mode)
		    (mach:unix-stat (namestring file))
		  (declare (ignore dev inode))
		  (when (and won (not (zerop (logand mode mach:writeown))))
		    (cond ((or p
			       (not (prompt-for-y-or-n
				     :prompt
				     (format nil
					     "~S is writable, overwrite? "
					     (namestring file))
				     :default nil
				     :default-string "n")))
			   (let ((private
				  (merge-pathnames (concatenate
						    'simple-string
						    (file-namestring file)
						    ".private")
						   file)))
			     (message "Renaming ~S to ~S"
				      (namestring file)
				      (namestring private))
			     (rename-file file private)))
			  (t
			   (delete-file file))))
		  (setf did-any t)
		  (rcs-check-out-file file nil)))))))
      (unless did-any
	(message "No files are out of date.")))))
