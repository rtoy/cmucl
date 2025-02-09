BEGIN {
    print ";;; -*- Package: UNIX -*-\n\
;;;\n\
;;; **********************************************************************\n\
;;; This code was written as part of the CMU Common Lisp project at\n\
;;; Carnegie Mellon University, and has been placed in the public domain.\n\
;;;\n\
(ext:file-comment\n\
  \"$Header: src/code/unix-errno.lisp $\")\n\
;;;\n\
;;; **********************************************************************\n\
;;;\n\
;;; This file contains the UNIX low-level support, just enough to run\n\
;;; CMUCL.\n\
;;;\n\
(in-package \"UNIX\")\n\
(intl:textdomain \"cmucl-unix\")\n\
\n\
;;;; Errno stuff.\n\
(eval-when (compile eval)\n\
\n\
(defparameter *compiler-unix-errors* nil)\n\
\n\
(defmacro def-unix-error (name number &optional description)\n\
  \"Define a constant named Name corresponding to the Unix errno value\n\
  Number.  A description of the errno is optional in Description.\"\n\
  `(progn\n\
     (eval-when (compile eval)\n\
       (push (cons ,number ,description) *compiler-unix-errors*))\n\
     (defconstant ,name ,number ,description)\n\
     (export ',name)))\n\
\n\
(defmacro emit-unix-errors ()\n\
  (let* ((max (apply #'max (mapcar #'car *compiler-unix-errors*)))\n\
	 (array (make-array (1+ max) :initial-element nil)))\n\
    (dolist (error *compiler-unix-errors*)\n\
      (setf (svref array (car error)) (cdr error)))\n\
    `(progn\n\
       (defvar *unix-errors* ',array)\n\
       (declaim (simple-vector *unix-errors*)))))\n\
\n\
) ;eval-when\n\
\n\
;;; \n\
;;; From <errno.h>\n\
;;; \n\
(def-unix-error ESUCCESS 0 _N\"Successful\")\n\
#-linux\n\
(progn\n\
(def-unix-error EPERM 1 _N\"Operation not permitted\")\n\
(def-unix-error ENOENT 2 _N\"No such file or directory\")\n\
(def-unix-error ESRCH 3 _N\"No such process\")\n\
(def-unix-error EINTR 4 _N\"Interrupted system call\")\n\
(def-unix-error EIO 5 _N\"I/O error\")\n\
(def-unix-error ENXIO 6 _N\"Device not configured\")\n\
(def-unix-error E2BIG 7 _N\"Arg list too long\")\n\
(def-unix-error ENOEXEC 8 _N\"Exec format error\")\n\
(def-unix-error EBADF 9 _N\"Bad file descriptor\")\n\
(def-unix-error ECHILD 10 _N\"No child process\")\n\
)\n\
#+bsd(def-unix-error EDEADLK 11 _N\"Resource deadlock avoided\")\n\
#-bsd(def-unix-error EAGAIN 11 #-linux _N\"No more processes\" #+linux _N\"Try again\")\n\
#-linux\n\
(progn\n\
(def-unix-error ENOMEM 12 _N\"Out of memory\")\n\
(def-unix-error EACCES 13 _N\"Permission denied\")\n\
(def-unix-error EFAULT 14 _N\"Bad address\")\n\
(def-unix-error ENOTBLK 15 _N\"Block device required\")\n\
(def-unix-error EBUSY 16 _N\"Device or resource busy\")\n\
(def-unix-error EEXIST 17 _N\"File exists\")\n\
(def-unix-error EXDEV 18 _N\"Cross-device link\")\n\
(def-unix-error ENODEV 19 _N\"No such device\")\n\
(def-unix-error ENOTDIR 20 _N\"Not a director\")\n\
(def-unix-error EISDIR 21 _N\"Is a directory\")\n\
(def-unix-error EINVAL 22 _N\"Invalid argument\")\n\
(def-unix-error ENFILE 23 _N\"File table overflow\")\n\
(def-unix-error EMFILE 24 _N\"Too many open files\")\n\
(def-unix-error ENOTTY 25 _N\"Inappropriate ioctl for device\")\n\
(def-unix-error ETXTBSY 26 _N\"Text file busy\")\n\
(def-unix-error EFBIG 27 _N\"File too large\")\n\
(def-unix-error ENOSPC 28 _N\"No space left on device\")\n\
(def-unix-error ESPIPE 29 _N\"Illegal seek\")\n\
(def-unix-error EROFS 30 _N\"Read-only file system\")\n\
(def-unix-error EMLINK 31 _N\"Too many links\")\n\
(def-unix-error EPIPE 32 _N\"Broken pipe\")\n\
;;; \n\
;;; Math\n\
(def-unix-error EDOM 33 _N\"Numerical argument out of domain\")\n\
(def-unix-error ERANGE 34 #-linux _N\"Result too large\" #+linux _N\"Math result not representable\")\n\
)\n\
;;; \n\
#-(or linux svr4)\n\
(progn\n\
;;; non-blocking and interrupt i/o\n\
(def-unix-error EWOULDBLOCK 35 _N\"Operation would block\")\n\
#-bsd(def-unix-error EDEADLK 35 _N\"Operation would block\") ; Ditto\n\
#+bsd(def-unix-error EAGAIN 35 _N\"Resource temporarily unavailable\")\n\
(def-unix-error EINPROGRESS 36 _N\"Operation now in progress\")\n\
(def-unix-error EALREADY 37 _N\"Operation already in progress\")\n\
;;;\n\
;;; ipc/network software\n\
(def-unix-error ENOTSOCK 38 _N\"Socket operation on non-socket\")\n\
(def-unix-error EDESTADDRREQ 39 _N\"Destination address required\")\n\
(def-unix-error EMSGSIZE 40 _N\"Message too long\")\n\
(def-unix-error EPROTOTYPE 41 _N\"Protocol wrong type for socket\")\n\
(def-unix-error ENOPROTOOPT 42 _N\"Protocol not available\")\n\
(def-unix-error EPROTONOSUPPORT 43 _N\"Protocol not supported\")\n\
(def-unix-error ESOCKTNOSUPPORT 44 _N\"Socket type not supported\")\n\
(def-unix-error EOPNOTSUPP 45 _N\"Operation not supported on socket\")\n\
(def-unix-error EPFNOSUPPORT 46 _N\"Protocol family not supported\")\n\
(def-unix-error EAFNOSUPPORT 47 _N\"Address family not supported by protocol family\")\n\
(def-unix-error EADDRINUSE 48 _N\"Address already in use\")\n\
(def-unix-error EADDRNOTAVAIL 49 _N\"Can't assign requested address\")\n\
;;;\n\
;;; operational errors\n\
(def-unix-error ENETDOWN 50 _N\"Network is down\")\n\
(def-unix-error ENETUNREACH 51 _N\"Network is unreachable\")\n\
(def-unix-error ENETRESET 52 _N\"Network dropped connection on reset\")\n\
(def-unix-error ECONNABORTED 53 _N\"Software caused connection abort\")\n\
(def-unix-error ECONNRESET 54 _N\"Connection reset by peer\")\n\
(def-unix-error ENOBUFS 55 _N\"No buffer space available\")\n\
(def-unix-error EISCONN 56 _N\"Socket is already connected\")\n\
(def-unix-error ENOTCONN 57 _N\"Socket is not connected\")\n\
(def-unix-error ESHUTDOWN 58 _N\"Can't send after socket shutdown\")\n\
(def-unix-error ETOOMANYREFS 59 _N\"Too many references: can't splice\")\n\
(def-unix-error ETIMEDOUT 60 _N\"Connection timed out\")\n\
(def-unix-error ECONNREFUSED 61 _N\"Connection refused\")\n\
;;; \n\
(def-unix-error ELOOP 62 _N\"Too many levels of symbolic links\")\n\
(def-unix-error ENAMETOOLONG 63 _N\"File name too long\")\n\
;;; \n\
(def-unix-error EHOSTDOWN 64 _N\"Host is down\")\n\
(def-unix-error EHOSTUNREACH 65 _N\"No route to host\")\n\
(def-unix-error ENOTEMPTY 66 _N\"Directory not empty\")\n\
;;; \n\
;;; quotas & resource \n\
(def-unix-error EPROCLIM 67 _N\"Too many processes\")\n\
(def-unix-error EUSERS 68 _N\"Too many users\")\n\
(def-unix-error EDQUOT 69 _N\"Disc quota exceeded\")\n\
;;;\n\
;;; CMU RFS\n\
(def-unix-error ELOCAL 126 _N\"namei should continue locally\")\n\
(def-unix-error EREMOTE 127 _N\"namei was handled remotely\")\n\
;;;\n\
;;; VICE\n\
(def-unix-error EVICEERR 70 _N\"Remote file system error _N\")\n\
(def-unix-error EVICEOP 71 _N\"syscall was handled by Vice\")\n\
)\n\
#+svr4\n\
(progn\n\
(def-unix-error ENOMSG 35 _N\"No message of desired type\")\n\
(def-unix-error EIDRM 36 _N\"Identifier removed\")\n\
(def-unix-error ECHRNG 37 _N\"Channel number out of range\")\n\
(def-unix-error EL2NSYNC 38 _N\"Level 2 not synchronized\")\n\
(def-unix-error EL3HLT 39 _N\"Level 3 halted\")\n\
(def-unix-error EL3RST 40 _N\"Level 3 reset\")\n\
(def-unix-error ELNRNG 41 _N\"Link number out of range\")\n\
(def-unix-error EUNATCH 42 _N\"Protocol driver not attached\")\n\
(def-unix-error ENOCSI 43 _N\"No CSI structure available\")\n\
(def-unix-error EL2HLT 44 _N\"Level 2 halted\")\n\
(def-unix-error EDEADLK 45 _N\"Deadlock situation detected/avoided\")\n\
(def-unix-error ENOLCK 46 _N\"No record locks available\")\n\
(def-unix-error ECANCELED 47 _N\"Error 47\")\n\
(def-unix-error ENOTSUP 48 _N\"Error 48\")\n\
(def-unix-error EBADE 50 _N\"Bad exchange descriptor\")\n\
(def-unix-error EBADR 51 _N\"Bad request descriptor\")\n\
(def-unix-error EXFULL 52 _N\"Message tables full\")\n\
(def-unix-error ENOANO 53 _N\"Anode table overflow\")\n\
(def-unix-error EBADRQC 54 _N\"Bad request code\")\n\
(def-unix-error EBADSLT 55 _N\"Invalid slot\")\n\
(def-unix-error EDEADLOCK 56 _N\"File locking deadlock\")\n\
(def-unix-error EBFONT 57 _N\"Bad font file format\")\n\
(def-unix-error ENOSTR 60 _N\"Not a stream device\")\n\
(def-unix-error ENODATA 61 _N\"No data available\")\n\
(def-unix-error ETIME 62 _N\"Timer expired\")\n\
(def-unix-error ENOSR 63 _N\"Out of stream resources\")\n\
(def-unix-error ENONET 64 _N\"Machine is not on the network\")\n\
(def-unix-error ENOPKG 65 _N\"Package not installed\")\n\
(def-unix-error EREMOTE 66 _N\"Object is remote\")\n\
(def-unix-error ENOLINK 67 _N\"Link has been severed\")\n\
(def-unix-error EADV 68 _N\"Advertise error\")\n\
(def-unix-error ESRMNT 69 _N\"Srmount error\")\n\
(def-unix-error ECOMM 70 _N\"Communication error on send\")\n\
(def-unix-error EPROTO 71 _N\"Protocol error\")\n\
(def-unix-error EMULTIHOP 74 _N\"Multihop attempted\")\n\
(def-unix-error EBADMSG 77 _N\"Not a data message\")\n\
(def-unix-error ENAMETOOLONG 78 _N\"File name too long\")\n\
(def-unix-error EOVERFLOW 79 _N\"Value too large for defined data type\")\n\
(def-unix-error ENOTUNIQ 80 _N\"Name not unique on network\")\n\
(def-unix-error EBADFD 81 _N\"File descriptor in bad state\")\n\
(def-unix-error EREMCHG 82 _N\"Remote address changed\")\n\
(def-unix-error ELIBACC 83 _N\"Can not access a needed shared library\")\n\
(def-unix-error ELIBBAD 84 _N\"Accessing a corrupted shared library\")\n\
(def-unix-error ELIBSCN 85 _N\".lib section in a.out corrupted\")\n\
(def-unix-error ELIBMAX 86 _N\"Attempting to link in more shared libraries than system limit\")\n\
(def-unix-error ELIBEXEC 87 _N\"Can not exec a shared library directly\")\n\
(def-unix-error EILSEQ 88 _N\"Error 88\")\n\
(def-unix-error ENOSYS 89 _N\"Operation not applicable\")\n\
(def-unix-error ELOOP 90 _N\"Number of symbolic links encountered during path name traversal exceeds MAXSYMLINKS\")\n\
(def-unix-error ERESTART 91 _N\"Error 91\")\n\
(def-unix-error ESTRPIPE 92 _N\"Error 92\")\n\
(def-unix-error ENOTEMPTY 93 _N\"Directory not empty\")\n\
(def-unix-error EUSERS 94 _N\"Too many users\")\n\
(def-unix-error ENOTSOCK 95 _N\"Socket operation on non-socket\")\n\
(def-unix-error EDESTADDRREQ 96 _N\"Destination address required\")\n\
(def-unix-error EMSGSIZE 97 _N\"Message too long\")\n\
(def-unix-error EPROTOTYPE 98 _N\"Protocol wrong type for socket\")\n\
(def-unix-error ENOPROTOOPT 99 _N\"Option not supported by protocol\")\n\
(def-unix-error EPROTONOSUPPORT 120 _N\"Protocol not supported\")\n\
(def-unix-error ESOCKTNOSUPPORT 121 _N\"Socket type not supported\")\n\
(def-unix-error EOPNOTSUPP 122 _N\"Operation not supported on transport endpoint\")\n\
(def-unix-error EPFNOSUPPORT 123 _N\"Protocol family not supported\")\n\
(def-unix-error EAFNOSUPPORT 124 _N\"Address family not supported by protocol family\")\n\
(def-unix-error EADDRINUSE 125 _N\"Address already in use\")\n\
(def-unix-error EADDRNOTAVAIL 126 _N\"Cannot assign requested address\")\n\
(def-unix-error ENETDOWN 127 _N\"Network is down\")\n\
(def-unix-error ENETUNREACH 128 _N\"Network is unreachable\")\n\
(def-unix-error ENETRESET 129 _N\"Network dropped connection because of reset\")\n\
(def-unix-error ECONNABORTED 130 _N\"Software caused connection abort\")\n\
(def-unix-error ECONNRESET 131 _N\"Connection reset by peer\")\n\
(def-unix-error ENOBUFS 132 _N\"No buffer space available\")\n\
(def-unix-error EISCONN 133 _N\"Transport endpoint is already connected\")\n\
(def-unix-error ENOTCONN 134 _N\"Transport endpoint is not connected\")\n\
(def-unix-error ESHUTDOWN 143 _N\"Cannot send after socket shutdown\")\n\
(def-unix-error ETOOMANYREFS 144 _N\"Too many references: cannot splice\")\n\
(def-unix-error ETIMEDOUT 145 _N\"Connection timed out\")\n\
(def-unix-error ECONNREFUSED 146 _N\"Connection refused\")\n\
(def-unix-error EHOSTDOWN 147 _N\"Host is down\")\n\
(def-unix-error EHOSTUNREACH 148 _N\"No route to host\")\n\
(def-unix-error EWOULDBLOCK 11 _N\"Resource temporarily unavailable\")\n\
(def-unix-error EALREADY 149 _N\"Operation already in progress\")\n\
(def-unix-error EINPROGRESS 150 _N\"Operation now in progress\")\n\
(def-unix-error ESTALE 151 _N\"Stale NFS file handle\")\n\
)\n\
";
}

/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    printf "(def-unix-error %s %s)\n", $2, $3;
}

END {
    print "\n\n;;;\n\
;;; And now for something completely different ...\n\
(emit-unix-errors)\n\
"
}

