;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles.
;;;

(in-package "HEMLOCK-INTERNALS")


;;;; Terminal init and exit methods.

(defvar *hemlock-input-handler*)

(defun init-tty-device (device)
  (setf *hemlock-input-handler*
	(system:add-fd-handler 0 :input #'get-editor-tty-input))
  (standard-device-init)
  (device-write-string (tty-device-init-string device))
  (redisplay-all))

(defun exit-tty-device (device)
  (cursor-motion device 0 (1- (tty-device-lines device)))
  ;; Can't call the clear-to-eol method since we don't have a hunk to
  ;; call it on, and you can't count on the bottom hunk being the echo area.
  ;; 
  (if (tty-device-clear-to-eol-string device)
      (device-write-string (tty-device-clear-to-eol-string device))
      (dotimes (i (tty-device-columns device)
		  (cursor-motion device 0 (1- (tty-device-lines device))))
	(tty-write-char #\space)))
  (device-write-string (tty-device-cm-end-string device))
  (when (device-force-output device)
    (funcall (device-force-output device)))
  (when *hemlock-input-handler*
    (system:remove-fd-handler *hemlock-input-handler*)
    (setf *hemlock-input-handler* nil))
  (standard-device-exit))



;;;; Output routines and buffering.

(defconstant redisplay-output-buffer-length 256)

(defvar *redisplay-output-buffer*
  (make-string redisplay-output-buffer-length))
(proclaim '(simple-string *redisplay-output-buffer*))

(defvar *redisplay-output-buffer-index* 0)
(proclaim '(fixnum *redisplay-output-buffer-index*))


;;; TTY-WRITE-STRING blasts the string into the redisplay output buffer.
;;; If the string overflows the buffer, then segments of the string are
;;; blasted into the buffer, dumping the buffer, until the last piece of
;;; the string is stored in the buffer.  The buffer is always dumped if
;;; it is full, even if the last piece of the string just fills the buffer.
;;; 
(defun tty-write-string (string start length)
  (declare (fixnum start length))
  (let ((buffer-space (- redisplay-output-buffer-length
			 *redisplay-output-buffer-index*)))
    (declare (fixnum buffer-space))
    (cond ((<= length buffer-space)
	   (let ((dst-index (+ *redisplay-output-buffer-index* length)))
	     (%primitive byte-blt string start *redisplay-output-buffer*
			 *redisplay-output-buffer-index* dst-index)
	     (cond ((= length buffer-space)
		    (mach:unix-write 1 *redisplay-output-buffer* 0 
				      redisplay-output-buffer-length)
		    (setf *redisplay-output-buffer-index* 0))
		   (t
		    (setf *redisplay-output-buffer-index* dst-index)))))
	  (t
	   (let ((remaining (- length buffer-space)))
	     (declare (fixnum remaining))
	     (loop
	      (%primitive byte-blt string start *redisplay-output-buffer*
			  *redisplay-output-buffer-index*
			  redisplay-output-buffer-length)
	      (mach:unix-write 1 *redisplay-output-buffer* 0 
				redisplay-output-buffer-length)
	      (when (< remaining redisplay-output-buffer-length)
		(%primitive byte-blt string (+ start buffer-space)
			    *redisplay-output-buffer* 0 remaining)
		(setf *redisplay-output-buffer-index* remaining)
		(return t))
	      (incf start buffer-space)
	      (setf *redisplay-output-buffer-index* 0)
	      (setf buffer-space redisplay-output-buffer-length)
	      (decf remaining redisplay-output-buffer-length)))))))


;;; TTY-WRITE-CHAR stores a character in the redisplay output buffer,
;;; dumping the buffer if it becomes full.
;;; 
(defun tty-write-char (char)
  (setf (schar *redisplay-output-buffer* *redisplay-output-buffer-index*)
	char)
  (incf *redisplay-output-buffer-index*)
  (when (= *redisplay-output-buffer-index* redisplay-output-buffer-length)
    (mach:unix-write 1 *redisplay-output-buffer* 0
		     redisplay-output-buffer-length)
    (setf *redisplay-output-buffer-index* 0)))


;;; TTY-FORCE-OUTPUT dumps the redisplay output buffer.  This is called
;;; out of terminal device structures in multiple places -- the device
;;; exit method, random typeout methods, out of tty-hunk-stream methods,
;;; after calls to REDISPLAY or REDISPLAY-ALL.
;;; 
(defun tty-force-output ()
  (unless (zerop *redisplay-output-buffer-index*)
    (mach:unix-write 1 *redisplay-output-buffer* 0
		     *redisplay-output-buffer-index*)
    (setf *redisplay-output-buffer-index* 0)))


;;; TTY-FINISH-OUTPUT simply dumps output.
;;;
(defun tty-finish-output (device window)
  (declare (ignore window))
  (let ((force-output (device-force-output device)))
    (when force-output
      (funcall force-output))))



;;;; Screen image line hacks.

(defmacro replace-si-line (dst-string src-string src-start dst-start dst-end)
  `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end))
