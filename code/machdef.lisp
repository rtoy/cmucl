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

(export '(int-array int-array-ref))

(export '(sigcontext-onstack sigcontext-mask sigcontext-pc sigcontext-regs
	  sigcontext-mdlo sigcontext-mdhi sigcontext-ownedfp sigcontext-fpregs
	  sigcontext-fpc_csr sigcontext-fpc_eir sigcontext-cause
	  sigcontext-badvaddr sigcontext-badpaddr sigcontext *sigcontext
	  indirect-*sigcontext))


(def-c-type c-string (pointer simple-base-string))

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

(defrecord timezone
  (minuteswest (signed-byte 32) (long-words 1))
  (dsttime (signed-byte 32) (long-words 1)))

#+new-compiler
(def-c-array int-array unsigned-long 32)

#+new-compiler
(def-c-record sigcontext
  (onstack unsigned-long)
  (mask unsigned-long)
  (pc system-area-pointer)
  (regs int-array)
  (mdlo unsigned-long)
  (mdhi unsigned-long)
  (ownedfp unsigned-long)
  (fpregs int-array)
  (fpc_csr unsigned-long)
  (fpc_eir unsigned-long)
  (cause unsigned-long)
  (badvaddr system-area-pointer)
  (badpaddr system-area-pointer))

(eval-when (compile load eval)

(defrecord tchars
  (intrc (signed-byte 8) (bytes 1))
  (quitc (signed-byte 8) (bytes 1))
  (startc (signed-byte 8) (bytes 1))
  (stopc (signed-byte 8) (bytes 1))
  (eofc (signed-byte 8) (bytes 1))
  (brkc (signed-byte 8) (bytes 1)))

(defrecord ltchars
  (suspc (signed-byte 8) (bytes 1))
  (dsuspc (signed-byte 8) (bytes 1))
  (rprntc (signed-byte 8) (bytes 1))
  (flushc (signed-byte 8) (bytes 1))
  (werasc (signed-byte 8) (bytes 1))
  (lnextc (signed-byte 8) (bytes 1)))

); eval-when (compile load eval)


#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* t))

(defmacro with-trap-arg-block (type var &body forms)
  `(with-stack-alien (,var ,type (record-size ',type))
     ,@forms))

;;; SIGMASK -- Public
;;;
#-new-compiler
(defmacro sigmask (&rest signals)
  "Returns a mask given a set of signals."
  (apply #'logior
	 (mapcar #'(lambda (signal)
		     (ash 1 (1- (unix-signal-number signal))))
		 signals)))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* nil))
