#! /bin/sh

# Generates the contents of the file code/unix-errno.lisp.

# For each supported OS, ERRNO_FILES should be set to a list of all
# the files that contain the definitions of the errno values.
case `uname -s` in
    Linux) ERRNO_FILES=/usr/include/asm-generic/errno*.h
	   ;;
esac


# The header was copied from code/unix.lisp.  This includes all the
# support code for DEF-UNIX-ERROR and for all OSes that don't use the
# awk script to create the DEF-UNIX-ERROR forms.
#
cat <<EOF
;;; -*- Package: UNIX -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/unix-errno.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the UNIX low-level support, just enough to run
;;; CMUCL.
;;;
(in-package "UNIX")
(intl:textdomain "cmucl-unix")

;;;; Errno stuff.
(eval-when (compile eval)

(defparameter *compiler-unix-errors* nil)

(defmacro def-unix-error (name number &optional description)
  "Define a constant named Name corresponding to the Unix errno value
  Number.  A description of the errno is optional in Description."
  \`(progn
     (eval-when (compile eval)
       (push (cons ,number ,description) *compiler-unix-errors*))
     (defconstant ,name ,number ,description)
     (export ',name)))

(defmacro emit-unix-errors ()
  (let* ((max (apply #'max (mapcar #'car *compiler-unix-errors*)))
	 (array (make-array (1+ max) :initial-element nil)))
    (dolist (error *compiler-unix-errors*)
      (setf (svref array (car error)) (cdr error)))
    \`(progn
       (defvar *unix-errors* ',array)
       (declaim (simple-vector *unix-errors*)))))

) ;eval-when

;;; 
;;; From <errno.h>
;;; 
(def-unix-error ESUCCESS 0 _N"Successful")
#-linux
(progn
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
)
#+bsd(def-unix-error EDEADLK 11 _N"Resource deadlock avoided")
#-(or bsd linux) (def-unix-error EAGAIN 11 #-linux _N"No more processes" #+linux _N"Try again")
#-linux
(progn
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
)
;;; 
#-(or linux svr4)
(progn
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
)
#+svr4
(progn
(def-unix-error ENOMSG 35 _N"No message of desired type")
(def-unix-error EIDRM 36 _N"Identifier removed")
(def-unix-error ECHRNG 37 _N"Channel number out of range")
(def-unix-error EL2NSYNC 38 _N"Level 2 not synchronized")
(def-unix-error EL3HLT 39 _N"Level 3 halted")
(def-unix-error EL3RST 40 _N"Level 3 reset")
(def-unix-error ELNRNG 41 _N"Link number out of range")
(def-unix-error EUNATCH 42 _N"Protocol driver not attached")
(def-unix-error ENOCSI 43 _N"No CSI structure available")
(def-unix-error EL2HLT 44 _N"Level 2 halted")
(def-unix-error EDEADLK 45 _N"Deadlock situation detected/avoided")
(def-unix-error ENOLCK 46 _N"No record locks available")
(def-unix-error ECANCELED 47 _N"Error 47")
(def-unix-error ENOTSUP 48 _N"Error 48")
(def-unix-error EBADE 50 _N"Bad exchange descriptor")
(def-unix-error EBADR 51 _N"Bad request descriptor")
(def-unix-error EXFULL 52 _N"Message tables full")
(def-unix-error ENOANO 53 _N"Anode table overflow")
(def-unix-error EBADRQC 54 _N"Bad request code")
(def-unix-error EBADSLT 55 _N"Invalid slot")
(def-unix-error EDEADLOCK 56 _N"File locking deadlock")
(def-unix-error EBFONT 57 _N"Bad font file format")
(def-unix-error ENOSTR 60 _N"Not a stream device")
(def-unix-error ENODATA 61 _N"No data available")
(def-unix-error ETIME 62 _N"Timer expired")
(def-unix-error ENOSR 63 _N"Out of stream resources")
(def-unix-error ENONET 64 _N"Machine is not on the network")
(def-unix-error ENOPKG 65 _N"Package not installed")
(def-unix-error EREMOTE 66 _N"Object is remote")
(def-unix-error ENOLINK 67 _N"Link has been severed")
(def-unix-error EADV 68 _N"Advertise error")
(def-unix-error ESRMNT 69 _N"Srmount error")
(def-unix-error ECOMM 70 _N"Communication error on send")
(def-unix-error EPROTO 71 _N"Protocol error")
(def-unix-error EMULTIHOP 74 _N"Multihop attempted")
(def-unix-error EBADMSG 77 _N"Not a data message")
(def-unix-error ENAMETOOLONG 78 _N"File name too long")
(def-unix-error EOVERFLOW 79 _N"Value too large for defined data type")
(def-unix-error ENOTUNIQ 80 _N"Name not unique on network")
(def-unix-error EBADFD 81 _N"File descriptor in bad state")
(def-unix-error EREMCHG 82 _N"Remote address changed")
(def-unix-error ELIBACC 83 _N"Can not access a needed shared library")
(def-unix-error ELIBBAD 84 _N"Accessing a corrupted shared library")
(def-unix-error ELIBSCN 85 _N".lib section in a.out corrupted")
(def-unix-error ELIBMAX 86 _N"Attempting to link in more shared libraries than system limit")
(def-unix-error ELIBEXEC 87 _N"Can not exec a shared library directly")
(def-unix-error EILSEQ 88 _N"Error 88")
(def-unix-error ENOSYS 89 _N"Operation not applicable")
(def-unix-error ELOOP 90 _N"Number of symbolic links encountered during path name traversal exceeds MAXSYMLINKS")
(def-unix-error ERESTART 91 _N"Error 91")
(def-unix-error ESTRPIPE 92 _N"Error 92")
(def-unix-error ENOTEMPTY 93 _N"Directory not empty")
(def-unix-error EUSERS 94 _N"Too many users")
(def-unix-error ENOTSOCK 95 _N"Socket operation on non-socket")
(def-unix-error EDESTADDRREQ 96 _N"Destination address required")
(def-unix-error EMSGSIZE 97 _N"Message too long")
(def-unix-error EPROTOTYPE 98 _N"Protocol wrong type for socket")
(def-unix-error ENOPROTOOPT 99 _N"Option not supported by protocol")
(def-unix-error EPROTONOSUPPORT 120 _N"Protocol not supported")
(def-unix-error ESOCKTNOSUPPORT 121 _N"Socket type not supported")
(def-unix-error EOPNOTSUPP 122 _N"Operation not supported on transport endpoint")
(def-unix-error EPFNOSUPPORT 123 _N"Protocol family not supported")
(def-unix-error EAFNOSUPPORT 124 _N"Address family not supported by protocol family")
(def-unix-error EADDRINUSE 125 _N"Address already in use")
(def-unix-error EADDRNOTAVAIL 126 _N"Cannot assign requested address")
(def-unix-error ENETDOWN 127 _N"Network is down")
(def-unix-error ENETUNREACH 128 _N"Network is unreachable")
(def-unix-error ENETRESET 129 _N"Network dropped connection because of reset")
(def-unix-error ECONNABORTED 130 _N"Software caused connection abort")
(def-unix-error ECONNRESET 131 _N"Connection reset by peer")
(def-unix-error ENOBUFS 132 _N"No buffer space available")
(def-unix-error EISCONN 133 _N"Transport endpoint is already connected")
(def-unix-error ENOTCONN 134 _N"Transport endpoint is not connected")
(def-unix-error ESHUTDOWN 143 _N"Cannot send after socket shutdown")
(def-unix-error ETOOMANYREFS 144 _N"Too many references: cannot splice")
(def-unix-error ETIMEDOUT 145 _N"Connection timed out")
(def-unix-error ECONNREFUSED 146 _N"Connection refused")
(def-unix-error EHOSTDOWN 147 _N"Host is down")
(def-unix-error EHOSTUNREACH 148 _N"No route to host")
(def-unix-error EWOULDBLOCK 11 _N"Resource temporarily unavailable")
(def-unix-error EALREADY 149 _N"Operation already in progress")
(def-unix-error EINPROGRESS 150 _N"Operation now in progress")
(def-unix-error ESTALE 151 _N"Stale NFS file handle")
)

;;; Auto-generated forms, if any.
EOF

# Create appropriate DEF-UNIX-ERROR forms by reading header files
# containing the C definitions.
awk '/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    printf "(def-unix-error %s %s)\n", $2, $3;
}' ${ERRNO_FILES}

# The tail was also copied from code/unix.lisp.  It's needed to tell
# Lisp about the errno values.
cat <<EOF
;;; End auto-generated forms, if any.

;;;
;;; And now for something completely different ...
(emit-unix-errors)
EOF
