;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/hemlock/display.lisp,v 1.6 1991/03/18 13:23:49 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Bill Chiles.
;;;
;;; This is the device independent redisplay entry points for Hemlock.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(redisplay redisplay-all))



;;;; Main redisplay entry points.

(defvar *things-to-do-once* ()
  "This is a list of lists of functions and args to be applied to.  The 
  functions are called with args supplied at the top of the command loop.")

(defvar *screen-image-trashed* ()
  "This variable is set to true if the screen has been trashed by some screen
   manager operation, and thus should be totally refreshed.  This is currently
   only used by tty redisplay.")

;;; True if we are in redisplay, and thus don't want to enter it recursively.
;;;
(defvar *in-redisplay* nil)

(proclaim '(special *window-list*))

(eval-when (compile eval)

;;; REDISPLAY-LOOP binds win-var to each window that is not the
;;; *current-window*, and calls the executes the general-form after executing
;;; the current-window-form.  Then we put the cursor in the appropriate place
;;; and force output.  Routines such as REDISPLAY and REDISPLAY-ALL want to
;;; invoke the after-redisplay method to make sure we've handled any events
;;; generated from redisplaying.  This is in case some user loops over one of
;;; these for a long time without going through Hemlock's input loop and event
;;; handling.  Routines such as INTERNAL-REDISPLAY don't want to worry about
;;; this since they are called from the input/event-handling loop.
;;;
;;; We establish the REDISPLAY-CATCHER, and return T if any of the forms
;;; returns true, otherwise NIL.  People throw :EDITOR-INPUT to indicate an
;;; abort.  A return of T in effect indicates that redisplay should be called
;;; again to make sure that it has converged.
;;;
;;; When we go to position the cursor, it is possible that we will find that it
;;; doesn't lie within the window after all (due to buffer modifications during
;;; output for previous redisplays.)  If so, we just make sure to return T.
;;;
(defmacro redisplay-loop ((win-var) general-form current-window-form
			  &optional (afterp t))
  (let ((device (gensym)) (point (gensym)) (hunk (gensym))
	(n-res (gensym)))
    `(let ((,n-res nil)
	   (*in-redisplay* t))
       (catch 'redisplay-catcher
	 (when (listen-editor-input *real-editor-input*)
	   (throw 'redisplay-catcher :editor-input))
	 (when ,current-window-form (setq ,n-res t))
	 (dolist (,win-var *window-list*)
	   (unless (eq ,win-var *current-window*)
	     (when (listen-editor-input *real-editor-input*)
	       (throw 'redisplay-catcher :editor-input))
	     (when ,general-form (setq ,n-res t))))
	 (let* ((,hunk (window-hunk *current-window*))
		(,device (device-hunk-device ,hunk))
		(,point (window-point *current-window*)))
	   (move-mark ,point (buffer-point (window-buffer *current-window*)))
	   (multiple-value-bind (x y)
				(mark-to-cursorpos ,point *current-window*)
	     (if x
		 (funcall (device-put-cursor ,device) ,hunk x y)
		 (setq ,n-res t)))
	   (when (device-force-output ,device)
	     (funcall (device-force-output ,device)))
	   ,@(if afterp
		 `((when (device-after-redisplay ,device)
		     (funcall (device-after-redisplay ,device) ,device))))
	   ,n-res)))))

) ;eval-when


;;; REDISPLAY  --  Public
;;;
;;;    This function updates the display of all windows which need it.
;;; it assumes it's internal representation of the screen is accurate 
;;; and attempts to do the minimal amount of output to bring the screen
;;; into correspondence.  *screen-image-trashed* is only used by terminal
;;; redisplay.
;;;
(defun redisplay ()
  "The main entry into redisplay; updates any windows that seem to need it."
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setq *things-to-do-once* nil))
  (cond (*in-redisplay* t)
	(*screen-image-trashed*
	 (when (eq (redisplay-all) t)
	   (setq *screen-image-trashed* nil)
	   t))
	(t
	 (redisplay-loop (w)
	   (redisplay-window w)
	   (redisplay-window-recentering *current-window*)))))


;;; REDISPLAY-ALL  --  Public
;;;
;;;    Update the screen making no assumptions about what is on it.
;;; useful if the screen (or redisplay) gets trashed.  Since windows
;;; potentially may be on different devices, we have to go through the
;;; list clearing all possible devices.  Always returns T or :EDITOR-INPUT,
;;; never NIL.
;;;
(defun redisplay-all ()
  "An entry into redisplay; causes all windows to be fully refreshed."
  (let ((cleared-devices nil))
    (dolist (w *window-list*)
      (let* ((hunk (window-hunk w))
	     (device (device-hunk-device hunk)))
	(unless (member device cleared-devices :test #'eq)
	  (when (device-clear device)
	    (funcall (device-clear device) device))
	  ;;
	  ;; It's cleared whether we did clear it or there was no method.
	  (push device cleared-devices)))))
  (redisplay-loop (w)
    (redisplay-window-all w)
    (progn
      (setf (window-tick *current-window*) (tick))
      (update-window-image *current-window*)
      (maybe-recenter-window *current-window*)
      (funcall (device-dumb-redisplay
		(device-hunk-device (window-hunk *current-window*)))
	       *current-window*)
      t)))


;;;; Internal redisplay entry points.

(defun internal-redisplay ()
  "The main internal entry into redisplay.  This is just like REDISPLAY, but it
   doesn't call the device's after-redisplay method."
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setq *things-to-do-once* nil))
  (cond (*in-redisplay* t)
	(*screen-image-trashed*
	 (when (eq (redisplay-all) t)
	   (setq *screen-image-trashed* nil)
	   t))
	(t
	 (redisplay-loop (w)
	   (redisplay-window w)
	   (redisplay-window-recentering *current-window*)))))

;;; REDISPLAY-WINDOWS-FROM-MARK is called from the hemlock-output-stream
;;; methods to bring the screen up to date.  It only redisplays windows which
;;; are displaying the buffer concerned, and doesn't deal with making the
;;; cursor track the point.  *screen-image-trashed* is only used by terminal
;;; redisplay.  This must call the device after-redisplay method since stream
;;; output may be done repeatedly without ever returning to the main Hemlock
;;; read loop and event servicing.
;;;
(defun redisplay-windows-from-mark (mark)
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setq *things-to-do-once* nil))
  (cond ((or *in-redisplay* (not *in-the-editor*)) t)
	((listen-editor-input *real-editor-input*) :editor-input)
	(*screen-image-trashed*
	 (when (eq (redisplay-all) t)
	   (setq *screen-image-trashed* nil)
	   t))
	(t
	 (catch 'redisplay-catcher
	   (let ((buffer (line-buffer (mark-line mark))))
	     (when buffer
	       (flet ((frob (win)
			(let* ((device (device-hunk-device (window-hunk win)))
			       (force (device-force-output device))
			       (after (device-after-redisplay device)))
			  (when force (funcall force))
			  (when after (funcall after device)))))
		 (let ((windows (buffer-windows buffer)))
		   (when (member *current-window* windows :test #'eq)
		     (redisplay-window-recentering *current-window*)
		     (frob *current-window*))
		   (dolist (window windows)
		     (unless (eq window *current-window*)
		       (redisplay-window window)
		       (frob window)))))))))))

;;; We return T if there are any changed lines, NIL otherwise.
;;;
(defun redisplay-window (window)
  "Maybe updates the window's image and calls the device's smart redisplay
   method.  NOTE: the smart redisplay method may throw to
   'hi::redisplay-catcher to abort redisplay."
  (maybe-update-window-image window)
  (prog1
      (not (eq (window-first-changed window) the-sentinel))
    (funcall (device-smart-redisplay (device-hunk-device (window-hunk window)))
	     window)))

(defun redisplay-window-all (window)
  "Updates the window's image and calls the device's dumb redisplay method."
  (setf (window-tick window) (tick))
  (update-window-image window)
  (funcall (device-dumb-redisplay (device-hunk-device (window-hunk window)))
	   window)
  t)

(defun random-typeout-redisplay (window)
  (catch 'redisplay-catcher
    (maybe-update-window-image window)
    (let* ((device (device-hunk-device (window-hunk window)))
	   (force (device-force-output device)))
      (funcall (device-smart-redisplay device) window)
      (when force (funcall force)))))


;;;; Support for redisplay entry points.

;;; REDISPLAY-WINDOW-RECENTERING tries to be clever about updating the window
;;; image unnecessarily, recenters the window if the window's buffer's point
;;; moved off the window, and does a smart redisplay.  We call the redisplay
;;; method even if we didn't update the image or recenter because someone
;;; else may have modified the window's image and already have updated it;
;;; if nothing happened, then the smart method shouldn't do anything anyway.
;;; NOTE: the smart redisplay method may throw to 'hi::redisplay-catcher to
;;; abort redisplay.
;;;
;;; We return T if there are any changed lines, NIL otherwise.
;;; 
(defun redisplay-window-recentering (window)
  (setup-for-recentering-redisplay window)
  (invoke-hook ed::redisplay-hook window)
  (setup-for-recentering-redisplay window)
  (prog1
      (not (eq (window-first-changed window) the-sentinel))
    (funcall (device-smart-redisplay (device-hunk-device (window-hunk window)))
	     window)))

(defun setup-for-recentering-redisplay (window)
  (let* ((display-start (window-display-start window))
	 (old-start (window-old-start window)))
    ;;
    ;; If the start is in the middle of a line and it wasn't before,
    ;; then move the start there.
    (when (and (same-line-p display-start old-start)
	       (not (start-line-p display-start))
	       (start-line-p old-start))
      (line-start display-start))
    (maybe-update-window-image window)
    (maybe-recenter-window window)))


;;; MAYBE-UPDATE-WINDOW-IMAGE only updates if the text has changed or the
;;; display start.
;;; 
(defun maybe-update-window-image (window)
  (when (or (> (buffer-modified-tick (window-buffer window))
	       (window-tick window))
	    (mark/= (window-display-start window)
		    (window-old-start window)))
    (setf (window-tick window) (tick))
    (update-window-image window)
    t))
