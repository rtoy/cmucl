;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package 'mach)
(export '(msg-simplemsg msg-msgsize msg-msgtype msg-localport msg-remoteport
			msg-id sigmask with-trap-arg-block))

(defrecord Msg
  (Reserved1 (unsigned-byte 8) 8)
  (Reserved2 (unsigned-byte 8) 8)
  (Reserved3 (unsigned-byte 8) 8)
  (Reserved4 (unsigned-byte 7) 7)
  (SimpleMsg boolean 1)
  (MsgSize (signed-byte 32) 32)
  (MsgType (signed-byte 32) 32)
  (LocalPort port 32)
  (RemotePort port 32)
  (ID (signed-byte 32) 32))

(defrecord timeval
  (seconds (unsigned-byte 32) (long-words 1))
  (useconds (signed-byte 32) (long-words 1)))

(defalien timeval timeval (record-size 'timeval))

(defrecord timezone
  (minuteswest (signed-byte 32) (long-words 1))
  (dsttime (signed-byte 32) (long-words 1)))

(defalien timezone timezone (record-size 'timezone))

(eval-when (compile load eval)
(defrecord int1
  (int (signed-byte 32) (long-words 1)))

(defalien int1 int1 (record-size 'int1))

(defrecord int2
  (int (signed-byte 32) (long-words 1)))

(defalien int2 int2 (record-size 'int2))

(defrecord int3
  (int (signed-byte 32) (long-words 1)))

(defalien int3 int3 (record-size 'int3))

(defrecord sigcontext
  (onstack (unsigned-byte 32) (long-words 1))
  (mask (unsigned-byte 32) (long-words 1))
  (sctx-fpa (unsgined-byte 32) (long-words 1))
  (sp (unsigned-byte 32) (long-words 1))
  (fp (unsigned-byte 32) (long-words 1))
  (ap (unsigned-byte 32) (long-words 1))
  (iar (unsigned-byte 32) (long-words 1))
  (icscs (unsigned-byte 32) (long-words 1)))
(defalien sigcontext sigcontext (record-size 'sigcontext))


(defrecord tchars
  (intrc (signed-byte 8) (bytes 1))
  (quitc (signed-byte 8) (bytes 1))
  (startc (signed-byte 8) (bytes 1))
  (stopc (signed-byte 8) (bytes 1))
  (eofc (signed-byte 8) (bytes 1))
  (brkc (signed-byte 8) (bytes 1)))
(defalien tchars tchars (record-size 'tchars))

(defrecord ltchars
  (suspc (signed-byte 8) (bytes 1))
  (dsuspc (signed-byte 8) (bytes 1))
  (rprntc (signed-byte 8) (bytes 1))
  (flushc (signed-byte 8) (bytes 1))
  (werasc (signed-byte 8) (bytes 1))
  (lnextc (signed-byte 8) (bytes 1)))
(defalien ltchars ltchars (record-size 'ltchars))

); eval-when (compile load eval)


#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* t))

(defmacro sigmask (signal)
  "Returns a mask given a signal." 
  `(ash 1 (1- ,signal)))


(defmacro with-trap-arg-block (arg-var alien-var &body forms)
  `(progn (unless *free-trap-arg-blocks* (alloc-trap-arg-block))
	  (let ((*free-trap-arg-blocks* (cdr *free-trap-arg-blocks*))
		(,arg-var (car *free-trap-arg-blocks*)))
	    (alien-bind ((,alien-var ,arg-var ,arg-var T))
			,@forms))))
#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* nil))
