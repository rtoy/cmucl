;;; -*- Log: code.log; Package: extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/internet.lisp,v 1.6 1991/11/09 02:47:15 wlott Exp $")
;;;
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


(defconstant sock-stream 1)
(defconstant sock-dgram 2)
(defconstant sock-raw 3)

(defconstant af-unix 1)
(defconstant af-inet 2)

(defconstant msg-oob 1)
(defconstant msg-peek 2)
(defconstant msg-dontroute 4)

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


(defmacro maybe-byte-swap (var bytes)
  (ecase (c:backend-byte-order c:*backend*)
    (:big-endian
     var)
    (:little-endian
     (let ((ldbs nil))
       (dotimes (i bytes `(logior ,@ldbs))
	 (push `(ash (ldb (byte 8 ,(* i 8)) ,var)
		     ,(* (- bytes 1 i) 8))
	       ldbs))))))

(proclaim '(inline htonl ntohl htons ntohs))

(defun htonl (x)
  (maybe-byte-swap x 4))
(defun ntohl (x)
  (maybe-byte-swap x 4))
(defun htons (x)
  (maybe-byte-swap x 2))
(defun ntohs (x)
  (maybe-byte-swap x 2))


;;;; Host entry operations.

(defstruct host-entry
  name
  aliases
  addr-type
  addr-list)

(defun host-entry-addr (host)
  (declare (type host-entry host))
  (car (host-entry-addr-list host)))


(def-c-pointer *char (null-terminated-string 256))
(def-c-pointer *int int)
(def-c-pointer *ulong unsigned-long)

(def-c-record inet-sockaddr
  (family short)
  (port unsigned-short)
  (addr unsigned-long)
  (zero (unsigned-byte 64)))

(def-c-record hostent
  (name *char)
  (aliases system-area-pointer)
  (addrtype int)
  (length int)
  (addr-list system-area-pointer))

(def-c-routine "gethostbyname" (*hostent)
  (name null-terminated-string))

(def-c-routine "gethostbyaddr" (*hostent)
  (addr *ulong :copy)
  (len int)
  (type int))

(def-c-routine ("socket" unix-socket) (int)
  (domain int)
  (type int)
  (protocol int))

(def-c-routine ("connect" unix-connect) (int)
  (socket int)
  (sockaddr system-area-pointer)
  (len int))

(def-c-routine ("bind" unix-bind) (int)
  (socket int)
  (sockaddr system-area-pointer)
  (len int))

(def-c-routine ("listen" unix-listen) (int)
  (socket int)
  (backlog int))

(def-c-routine ("accept" unix-accept) (int)
  (socket int)
  (sockaddr system-area-pointer)
  (len *int :in-out))


(defmacro listify-c-array (array alien-type)
  `(do* ((array ,array)
	 (results nil
		  (alien-bind ((alien
				(make-alien ',alien-type
					    ,(c-sizeof alien-type)
					    sap)
				,alien-type
				t))
		    (cons (alien-access alien)
			  results)))
	 (index 0 (1+ index))
	 (sap (sap-ref-sap array index) (sap-ref-sap array index)))
	((zerop (sap-int sap)) (nreverse results))))

(defun lookup-host-entry (host)
  (if (typep host 'host-entry)
    host
    (let ((hostent
	   (typecase host
	     (string
	      (gethostbyname host))
	     ((unsigned-byte 32)
	      (gethostbyaddr host 4 af-inet))
	     (t
	      (error "Invalid host ~S for ~S -- must be either a string ~
	              or (unsigned-byte 32)"
		     host 'lookup-host-entry)))))
      (when hostent
	(alien-bind ((alien hostent hostent t))
	  (make-host-entry
	   :name (alien-access
		  (indirect-*char (hostent-name (alien-value alien))))
	   :aliases (listify-c-array
		     (alien-access (hostent-aliases (alien-value alien)))
		     (null-terminated-string 256))
	   :addr-type (alien-access (hostent-addrtype (alien-value alien)))
	   :addr-list (listify-c-array
		       (alien-access (hostent-addr-list (alien-value alien)))
		       (unsigned-byte 32))))))))

(defun create-inet-socket (&optional (kind :stream))
  (multiple-value-bind (proto type)
		       (internet-protocol kind)
    (let ((socket (unix-socket af-inet type proto)))
      (when (minusp socket)
	(error "Error creating socket: ~A" (get-unix-error-msg)))
      socket)))

(defun connect-to-inet-socket (host port &optional (kind :stream))
  (let ((socket (create-inet-socket kind))
	(hostent (or (lookup-host-entry host)
		     (error "Unknown host: ~S." host))))
    (with-stack-alien (sockaddr inet-sockaddr (c-sizeof 'inet-sockaddr))
      (setf (alien-access (inet-sockaddr-family (alien-value sockaddr)))
	    af-inet)
      (setf (alien-access (inet-sockaddr-port (alien-value sockaddr)))
	    (htons port))
      (setf (alien-access (inet-sockaddr-addr (alien-value sockaddr)))
	    (host-entry-addr hostent))
      (when (minusp (unix-connect socket
				  (alien-sap (alien-value sockaddr))
				  #.(truncate (c-sizeof 'inet-sockaddr)
					      (c-sizeof 'char))))
	(unix-close socket)
	(error "Error connecting socket to [~A:~A]: ~A"
	       (host-entry-name hostent)
	       port
	       (get-unix-error-msg)))
      socket)))

(defun create-inet-listener (port &optional (kind :stream))
  (let ((socket (create-inet-socket kind)))
    (with-stack-alien (sockaddr inet-sockaddr (c-sizeof 'inet-sockaddr))
      (setf (alien-access (inet-sockaddr-family (alien-value sockaddr)))
	    af-inet)
      (setf (alien-access (inet-sockaddr-port (alien-value sockaddr)))
	    (htons port))
      (setf (alien-access (inet-sockaddr-addr (alien-value sockaddr)))
	    0)
      (when (minusp (unix-bind socket
			       (alien-sap (alien-value sockaddr))
			       #.(truncate (c-sizeof 'inet-sockaddr)
					   (c-sizeof 'char))))
	(unix-close socket)
	(error "Error binding socket to port ~a: ~a"
	       port
	       (get-unix-error-msg))))
    (when (eq kind :stream)
      (when (minusp (unix-listen socket 5))
	(unix-close socket)
	(error "Error listening to socket: ~A" (get-unix-error-msg))))
    socket))

(defun accept-tcp-connection (unconnected)
  (declare (fixnum unconnected))
  (with-stack-alien (sockaddr inet-sockaddr (c-sizeof 'inet-sockaddr))
    (let ((connected (unix-accept unconnected
				  (alien-sap (alien-value sockaddr))
				  #.(truncate (c-sizeof 'inet-sockaddr)
					      (c-sizeof 'char)))))
      (when (minusp connected)
	(error "Error accepting a connection: ~A" (get-unix-error-msg)))
      (values connected
	      (alien-access (inet-sockaddr-addr (alien-value sockaddr)))))))

(defun close-socket (socket)
  (multiple-value-bind (ok err)
		       (unix-close socket)
    (unless ok
      (error "Error closing socket: ~A" (get-unix-error-msg err))))
  (undefined-value))



;;;; Out of Band Data.

(def-c-routine ("recv" unix-recv) (int)
  (fd int)
  (buffer null-terminated-string)
  (length int)
  (flags int))

(def-c-routine ("send" unix-send) (int)
  (fd int)
  (buffer null-terminated-string)
  (length int)
  (flags int))


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
      (cond ((minusp (mach:unix-recv (car handlers) buffer 1 msg-oob))
	     (cerror "Ignore it"
		     "Error recving oob data on ~A: ~A"
		     (car handlers)
		     (mach:get-unix-error-msg)))
	    (t
	     (setf handled t)
	     (let ((char (schar buffer 0))
		   (handled nil))
	       (declare (base-char char))
	       (dolist (handler (cdr handlers))
		 (declare (list handler))
		 (when (eql (car handler) char)
		   (funcall (cdr handler))
		   (setf handled t)))
	       (unless handled
		 (cerror "Ignore it"
			 "No oob handler defined for ~S on ~A"
			 char
			 (car handlers)))))))
    (unless handled
      (cerror "Ignore it"
	      "Got a SIGURG, but couldn't find any out-of-band data.")))
  (undefined-value))

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
	   (base-char char))
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
	   (base-char char))
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
	   (base-char char))
  (let ((buffer (make-string 1 :initial-element char)))
    (declare (simple-string buffer))
    (when (minusp (mach:unix-send fd buffer 1 msg-oob))
      (error "Error sending ~S OOB to across ~A: ~A"
	     char
	     fd
	     (mach:get-unix-error-msg)))))
