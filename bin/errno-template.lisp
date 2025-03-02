;;; -*- Package: ERRNO -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/errno.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the UNIX low-level support, just enough to run
;;; CMUCL.
;;;
(in-package "ERRNO")
(intl:textdomain "cmucl-unix")

;;;; Errno stuff.

(eval-when (compile eval)

(defparameter *compiler-unix-errors* nil)

(defmacro def-unix-error (name number &optional description)
  (declare (ignore description))
  `(progn
     (eval-when (compile eval)
       (push (cons ,number ,description) *compiler-unix-errors*))
     (defconstant ,name ,number)
     (export ',name)))

(defmacro emit-unix-errors ()
  (let* ((max (apply #'max (mapcar #'car *compiler-unix-errors*)))
	 (array (make-array (1+ max) :initial-element nil)))
    (dolist (error *compiler-unix-errors*)
      (setf (svref array (car error)) (cdr error)))
    `(progn
       (defvar *unix-errors* ',array)
       (declaim (simple-vector *unix-errors*)))))

) ;eval-when

;;; 
;;; From <errno.h>
;;; 
(def-unix-error ESUCCESS 0 _N"Successful")

;;; Do NOT modify the ESUCCESS form above.  bin/create-errno.sh
;;; depends on it.

;;; Default errno values.  These are used only if we could not
;;; auto-generate these forms.
(def-unix-error EPERM 1 _N"Operation not permitted")
(def-unix-error ENOENT 2 _N"No such file or directory")
(def-unix-error ESRCH 3 _N"No such process")
(def-unix-error EINTR 4 _N"Interrupted system call")
(def-unix-error EIO 5 _N"I/O error")
(def-unix-error ENXIO 6 _N"Device not configured")
(def-unix-error E2BIG 7 _N"Arg list too long")
(def-unix-error ENOEXEC 8 _N"Exec format error")
(def-unix-error EBADF 9 _N"Bad file descriptor")
(def-unix-error ECHILD 10 _N"No child process")
#+bsd(def-unix-error EDEADLK 11 _N"Resource deadlock avoided")
#-bsd(def-unix-error EAGAIN 11 #-linux _N"No more processes" #+linux _N"Try again")
(def-unix-error ENOMEM 12 _N"Out of memory")
(def-unix-error EACCES 13 _N"Permission denied")
(def-unix-error EFAULT 14 _N"Bad address")
(def-unix-error ENOTBLK 15 _N"Block device required")
(def-unix-error EBUSY 16 _N"Device or resource busy")
(def-unix-error EEXIST 17 _N"File exists")
(def-unix-error EXDEV 18 _N"Cross-device link")
(def-unix-error ENODEV 19 _N"No such device")
(def-unix-error ENOTDIR 20 _N"Not a director")
(def-unix-error EISDIR 21 _N"Is a directory")
(def-unix-error EINVAL 22 _N"Invalid argument")
(def-unix-error ENFILE 23 _N"File table overflow")
(def-unix-error EMFILE 24 _N"Too many open files")
(def-unix-error ENOTTY 25 _N"Inappropriate ioctl for device")
(def-unix-error ETXTBSY 26 _N"Text file busy")
(def-unix-error EFBIG 27 _N"File too large")
(def-unix-error ENOSPC 28 _N"No space left on device")
(def-unix-error ESPIPE 29 _N"Illegal seek")
(def-unix-error EROFS 30 _N"Read-only file system")
(def-unix-error EMLINK 31 _N"Too many links")
(def-unix-error EPIPE 32 _N"Broken pipe")
;;; 
;;; Math
(def-unix-error EDOM 33 _N"Numerical argument out of domain")
(def-unix-error ERANGE 34 #-linux _N"Result too large" #+linux _N"Math result not representable")

;;; non-blocking and interrupt i/o
(def-unix-error EWOULDBLOCK 35 _N"Operation would block")
#-bsd(def-unix-error EDEADLK 35 _N"Operation would block") ; Ditto
#+bsd(def-unix-error EAGAIN 35 _N"Resource temporarily unavailable")
(def-unix-error EINPROGRESS 36 _N"Operation now in progress")
(def-unix-error EALREADY 37 _N"Operation already in progress")
;;;
;;; ipc/network software
(def-unix-error ENOTSOCK 38 _N"Socket operation on non-socket")
(def-unix-error EDESTADDRREQ 39 _N"Destination address required")
(def-unix-error EMSGSIZE 40 _N"Message too long")
(def-unix-error EPROTOTYPE 41 _N"Protocol wrong type for socket")
(def-unix-error ENOPROTOOPT 42 _N"Protocol not available")
(def-unix-error EPROTONOSUPPORT 43 _N"Protocol not supported")
(def-unix-error ESOCKTNOSUPPORT 44 _N"Socket type not supported")
(def-unix-error EOPNOTSUPP 45 _N"Operation not supported on socket")
(def-unix-error EPFNOSUPPORT 46 _N"Protocol family not supported")
(def-unix-error EAFNOSUPPORT 47 _N"Address family not supported by protocol family")
(def-unix-error EADDRINUSE 48 _N"Address already in use")
(def-unix-error EADDRNOTAVAIL 49 _N"Can't assign requested address")
;;;
;;; operational errors
(def-unix-error ENETDOWN 50 _N"Network is down")
(def-unix-error ENETUNREACH 51 _N"Network is unreachable")
(def-unix-error ENETRESET 52 _N"Network dropped connection on reset")
(def-unix-error ECONNABORTED 53 _N"Software caused connection abort")
(def-unix-error ECONNRESET 54 _N"Connection reset by peer")
(def-unix-error ENOBUFS 55 _N"No buffer space available")
(def-unix-error EISCONN 56 _N"Socket is already connected")
(def-unix-error ENOTCONN 57 _N"Socket is not connected")
(def-unix-error ESHUTDOWN 58 _N"Can't send after socket shutdown")
(def-unix-error ETOOMANYREFS 59 _N"Too many references: can't splice")
(def-unix-error ETIMEDOUT 60 _N"Connection timed out")
(def-unix-error ECONNREFUSED 61 _N"Connection refused")
;;; 
(def-unix-error ELOOP 62 _N"Too many levels of symbolic links")
(def-unix-error ENAMETOOLONG 63 _N"File name too long")
;;; 
(def-unix-error EHOSTDOWN 64 _N"Host is down")
(def-unix-error EHOSTUNREACH 65 _N"No route to host")
(def-unix-error ENOTEMPTY 66 _N"Directory not empty")
;;; 
;;; quotas & resource 
(def-unix-error EPROCLIM 67 _N"Too many processes")
(def-unix-error EUSERS 68 _N"Too many users")
(def-unix-error EDQUOT 69 _N"Disc quota exceeded")
;;;
;;; CMU RFS
(def-unix-error ELOCAL 126 _N"namei should continue locally")
(def-unix-error EREMOTE 127 _N"namei was handled remotely")
;;;
;;; VICE
(def-unix-error EVICEERR 70 _N"Remote file system error _N")
(def-unix-error EVICEOP 71 _N"syscall was handled by Vice")

;;; Do NOT modify the line below.  bin/create-errno.sh depends on it.

;;; End of default def-unix-error forms

;;; And now for something completely different ...
(emit-unix-errors)

