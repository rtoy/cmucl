;;; -*- Log: code.log; Package: extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file contains an interface to internet domain sockets.
;;;
;;; Written by William Lott.
;;;

(in-package "EXTENSIONS")

(export '(htonl ntohl htons ntohs lookup-host-entry host-entry host-entry-name
	  host-entry-aliases host-entry-addr-list host-entry-addr
	  create-inet-socket connect-to-inet-socket create-inet-listener
	  accept-tcp-connection close-socket ipproto-tcp ipproto-udp inaddr-any
	  add-oob-handler remove-oob-handler remove-all-oob-handlers
	  send-character-out-of-band))


(defvar *internet-protocols*
  (list (list :stream 6 sock-stream)
	(list :data-gram 17 sock-dgram))
  "AList of socket kinds and protocol values.")

(defun internet-protocol (kind)
  (let ((entry (assoc kind *internet-protocols*)))
    (unless entry
      (error "Invalid kind (~S) for internet domain sockets." kind))
    (values (cadr entry)
	    (caddr entry))))



(eval-when (compile load eval)
  #+ :IBM-RT-PC
  (pushnew :NETWORK-BYTE-ORDER *features*)
  )

#+ :NETWORK-BYTE-ORDER
(progn
  (defmacro htonl (x) x)
  (defmacro ntohl (x) x)
  (defmacro htons (x) x)
  (defmacro ntohs (x) x))

#- :NETWORK-BYTE-ORDER
(progn
  (defmacro htonl (x)
    (let ((val (gensym)))
      `(let ((,val ,x))
	 (logior (ash (ldb (byte 8 0)
			   ,val)
		      24)
		 (ash (ldb (byte 8 8)
			   ,val)
		      16)
		 (ash (ldb (byte 8 16)
			   ,val)
		      8)
		 (ldb (byte 8 24)
		      ,val)))))
  (defmacro ntohl (x)
    `(htonl ,x))
  (defmacro htons (x)
    (let ((val (gensym)))
      `(let ((,val ,x))
	 (logior (ash (ldb (byte 8 0)
			   ,val)
		      8)
		 (ldb (byte 8 8)
		      ,val)))))
  (defmacro ntohs (x)
    `(htons ,x)))



;;;; Host entry operations.

(defstruct host-entry
  name
  aliases
  addr-type
  addr-list)

(defun host-entry-addr (host)
  (car (host-entry-addr-list host)))


(def-c-pointer *char (null-terminated-string 256))
(def-c-type pointer (unsigned-byte 32))

(def-c-record inet-sockaddr
  (family short)
  (port unsigned-short)
  (addr unsigned-long)
  (zero (unsigned-byte 64)))

(def-c-record hostent
  (name *char)
  (aliases pointer)
  (addrtype int)
  (length int)
  (addr_list pointer))

(def-c-routine "gethostbyname" (*hostent)
  (name *char))
(def-c-routine "gethostbyaddr" (*hostent)
  (addr pointer)
  (len int)
  (type int))

(defalien *alien-ulong* (unsigned-byte 32) 32)
(defalien *alien-sockaddr* inet-sockaddr (c-sizeof 'inet-sockaddr))

(defoperator (my-hostent-aliases pointer)
	     ((hostent hostent))
  `(alien-index (alien-value ,hostent)
		(long-words 1)
		(long-words 1)))

(defoperator (my-hostent-addr_list pointer)
	     ((hostent hostent))
  `(alien-index (alien-value ,hostent)
		(long-words 4)
		(long-words 1)))

(defoperator (pointer-index pointer)
	     ((pointer pointer)
	      index)
  `(alien-index (alien-value ,pointer)
		(long-words ,index)
		(long-words 1)))

(defoperator (pointer-indirect pointer)
	     ((pointer pointer)
	      index)
  `(alien-indirect (alien-index (alien-value ,pointer)
				0
				(long-words 1))
		   (long-words (1+ ,index))))

(defun mumble (array)
  (alien-bind ((foo array pointer t)
	       (bar (pointer-indirect (alien-value foo) 0) pointer t)
	       (baz (pointer-index (alien-value bar) 0) pointer t))
    (alien-value baz)))

(defoperator (deref-string-ptr (null-terminated-string 512))
	     ((alien pointer))
  `(alien-indirect (alien-value ,alien)
		   (bytes 512)))

(defoperator (deref-ulong-ptr (unsigned-byte 32))
	     ((alien pointer))
  `(alien-indirect (alien-value ,alien)
		   (long-words 1)))

(defmacro listify-c-array (array derefer)
  (let ((results (gensym))
	(index (gensym))
	(p1 (gensym))
	(p2 (gensym))
	(p3 (gensym)))
    `(let ((,results nil)
	   (,index 0))
       (loop
	 (alien-bind
	     ((,p1 (pointer-indirect ,array ,index) pointer t)
	      (,p2 (pointer-index (alien-value ,p1) ,index) pointer t)
	      (,p3 (,derefer (alien-value ,p2))))
	   (when (zerop (alien-address (alien-value ,p3)))
	     (return (nreverse ,results)))
	   (push (alien-access (alien-value ,p3))
		 ,results))
	 (incf ,index)))))

(defun lookup-host-entry (host)
  (if (typep host 'host-entry)
    host
    (let ((hostent
	   (typecase host
	     (string
	      (gethostbyname host))
	     ((unsigned-byte 32)
	      (setf (system:alien-access *alien-ulong*) host)
	      (gethostbyaddr (system:alien-sap *alien-ulong*)
			     4 ; bytes per ulong
			     af-inet))
	     (t
	      (error "Invalid host ~S for ~S -- must be either a string or (unsigned-byte 32)")))))
      (if (not (null hostent))
	(alien-bind ((alien hostent hostent t)
		     (name
		      (alien-access (hostent-name (alien-value alien))
				    '(alien (null-terminated-string 256)
					    2048))
		      (null-terminated-string 256)
		      t)
		     (aliases
		      (my-hostent-aliases (alien-value alien))
		      pointer
		      t)
		     (addr-list
		      (my-hostent-addr_list (alien-value alien))
		      pointer
		      t))
	  (make-host-entry
	   :name (alien-access (alien-value name))
	   :aliases (listify-c-array (alien-value aliases) deref-string-ptr)
	   :addr-type (alien-access (hostent-addrtype (alien-value alien)))
	   :addr-list (listify-c-array (alien-value addr-list)
				       deref-ulong-ptr)))))))

(defun fill-in-sockaddr (addr port)
  (setf (alien-access (inet-sockaddr-family (alien-value *alien-sockaddr*)))
	af-inet)
  (setf (alien-access (inet-sockaddr-port (alien-value *alien-sockaddr*)))
	port)
  (setf (alien-access (inet-sockaddr-addr (alien-value *alien-sockaddr*)))
	addr)
  (values))

(defun create-inet-socket (&optional (kind :stream))
  (multiple-value-bind (proto type)
		       (internet-protocol kind)
    (multiple-value-bind (socket err)
			 (unix-socket af-inet type proto)
      (when (null socket)
	(error "Error creating socket: ~A"
	       (get-unix-error-msg err)))
      socket)))

(defun connect-to-inet-socket (host port &optional (kind :stream))
  (let ((socket (create-inet-socket kind))
	(hostent (lookup-host-entry host)))
    (fill-in-sockaddr (host-entry-addr hostent) port)
    (multiple-value-bind (ok err)
			 (unix-connect socket
				       *alien-sockaddr*)
      (unless ok
	(unix-close socket)
	(error "Error connecting socket to [~A:~A]: ~A"
	       (host-entry-name hostent)
	       port
	       (get-unix-error-msg err)))
      socket)))

(defun create-inet-listener (port &optional (kind :stream))
  (let ((socket (create-inet-socket kind)))
    (fill-in-sockaddr 0 port)
    (multiple-value-bind (ok err)
			 (unix-bind socket
				    *alien-sockaddr*)
      (unless ok
	(unix-close socket)
	(error "Error binding socket to port ~a: ~a"
	       port
	       (get-unix-error-msg err))))
    (when (eq kind :stream)
      (multiple-value-bind (ok err)
			   (unix-listen socket 5)
	(unless ok
	  (unix-close socket)
	  (error "Error listening to socket: ~a"
		 (get-unix-error-msg err)))))
    socket))

(defun accept-tcp-connection (unconnected)
  (declare (fixnum unconnected))
  (multiple-value-bind (connected err)
		       (unix-accept unconnected
				    *alien-sockaddr*)
    (when (null connected)
      (error "Error accepting a connection: ~a"
	     (get-unix-error-msg err)))
    (values connected
	    (alien-access (inet-sockaddr-addr *alien-sockaddr*)))))

(defun close-socket (socket)
  (multiple-value-bind (ok err)
		       (unix-close socket)
    (unless ok
      (error "Error closing socket: ~a"
	     (get-unix-error-msg err))))
  (values))



;;;; Out of Band Data.


;;; Two level AList. First levels key is the file descriptor, second levels
;;; key is the character. The datum is the handler to call.

(defvar *oob-handlers* nil)

;;; SIGURG-HANDLER -- internal
;;;
;;;   Routine that gets called whenever out-of-band data shows up. Checks each
;;; file descriptor for any oob data. If there is any, look for a handler for
;;; that character. If any are found, funcall them.

(defun sigurg-handler (signo code scp)
  (declare (ignore signo code scp))
  (let ((buffer (make-string 1))
	(handled nil))
    (declare (simple-string buffer))
    (dolist (handlers *oob-handlers*)
      (declare (list handlers))
      (multiple-value-bind (value err)
			   (mach:unix-recv (car handlers)
					   buffer
					   1
					   mach:msg-oob)
	(cond ((null value)
	       (cerror "Ignore it"
		       "Error recving oob data on ~A: ~A"
		       (car handlers)
		       (mach:get-unix-error-msg err)))
	      (t
	       (setf handled t)
	       (let ((char (schar buffer 0))
		     (handled nil))
		 (declare (string-char char))
		 (dolist (handler (cdr handlers))
		   (declare (list handler))
		   (when (eql (car handler) char)
		     (funcall (cdr handler))
		     (setf handled t)))
		 (unless handled
		   (cerror "Ignore it"
			   "No oob handler defined for ~S on ~A"
			   char
			   (car handlers))))))))
    (unless handled
      (cerror "Ignore it"
	      "Got a SIGURG, but couldn't find any out-of-band data.")))
  (values))

;;; ADD-OOB-HANDLER -- public
;;;
;;;   First, check to see if we already have any handlers for this file
;;; descriptor. If so, just add this handler to them. If not, add this
;;; file descriptor to *oob-handlers*, make sure our interupt handler is
;;; installed, and that the given file descriptor is "owned" by us (so sigurg
;;; will be delivered.)

(defun add-oob-handler (fd char handler)
  "Arange to funcall HANDLER when CHAR shows up out-of-band on FD."
  (declare (integer fd)
	   (string-char char))
  (let ((handlers (assoc fd *oob-handlers*)))
    (declare (list handlers))
    (cond (handlers
	   (push (cons char handler)
		 (cdr handlers)))
	  (t
	   (push (list fd
		       (cons char
			     handler))
		 *oob-handlers*)
	   (system:enable-interrupt mach:sigurg #'sigurg-handler)
	   (mach:unix-fcntl fd mach::f-setown (mach:unix-getpid)))))
  (values))

;;; REMOVE-OOB-HANDLER -- public
;;;
;;;   Delete any handlers for the given char from the list of handlers for the
;;; given file descriptor. If there are no more, nuke the entry for the file
;;; descriptor.

(defun remove-oob-handler (fd char)
  "Remove any handlers for CHAR on FD."
  (declare (integer fd)
	   (string-char char))
  (let ((handlers (assoc fd *oob-handlers*)))
    (declare (list handlers))
    (when handlers
      (let ((remaining (delete char handlers
			       :test #'eql
			       :key #'car)))
	(declare (list remaining))
	(if remaining
	  (setf (cdr handlers) remaining)
	  (setf *oob-handlers*
		(delete fd *oob-handlers*
			:test #'eql
			:key #'car))))))
  (values))

;;; REMOVE-ALL-OOB-HANDLERS -- public
;;;
;;;   Delete the entry for the given file descriptor.

(defun remove-all-oob-handlers (fd)
  "Remove all handlers for FD."
  (declare (integer fd))
  (setf *oob-handlers*
	(delete fd *oob-handlers*
		:test #'eql
		:key #'car))
  (values))

;;; SEND-CHARACTER-OUT-OF-BAND -- public
;;;
;;;   Sends CHAR across FD out of band.

(defun send-character-out-of-band (fd char)
  (declare (integer fd)
	   (string-char char))
  (let ((buffer (make-string 1 :initial-element char)))
    (declare (simple-string buffer))
    (multiple-value-bind (value err)
			 (mach:unix-send fd buffer 1 mach:msg-oob)
      (unless value
	(error "Error sending ~S OOB to across ~A: ~A"
	       char
	       fd
	       (mach:get-unix-error-msg err))))))
