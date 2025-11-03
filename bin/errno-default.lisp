;;; Default errno values.  These are used only if we could not
;;; auto-generate these forms.
(defconstant EPERM 1)
(defconstant ENOENT 2)
(defconstant ESRCH 3)
(defconstant EINTR 4)
(defconstant EIO 5)
(defconstant ENXIO 6)
(defconstant E2BIG 7)
(defconstant ENOEXEC 8)
(defconstant EBADF 9)
(defconstant ECHILD 10)
#+bsd(defconstant EDEADLK 11)
#-bsd(defconstant EAGAIN 11 #-linux)
(defconstant ENOMEM 12)
(defconstant EACCES 13)
(defconstant EFAULT 14)
(defconstant ENOTBLK 15)
(defconstant EBUSY 16)
(defconstant EEXIST 17)
(defconstant EXDEV 18)
(defconstant ENODEV 19)
(defconstant ENOTDIR 20)
(defconstant EISDIR 21)
(defconstant EINVAL 22)
(defconstant ENFILE 23)
(defconstant EMFILE 24)
(defconstant ENOTTY 25)
(defconstant ETXTBSY 26)
(defconstant EFBIG 27)
(defconstant ENOSPC 28)
(defconstant ESPIPE 29)
(defconstant EROFS 30)
(defconstant EMLINK 31)
(defconstant EPIPE 32)
;;; 
;;; Math
(defconstant EDOM 33)
(defconstant ERANGE 34 #-linux)

;;; non-blocking and interrupt i/o
(defconstant EWOULDBLOCK 35)
#-bsd(defconstant EDEADLK 35 _N"Operation would block") ; Ditto
#+bsd(defconstant EAGAIN 35)
(defconstant EINPROGRESS 36)
(defconstant EALREADY 37)
;;;
;;; ipc/network software
(defconstant ENOTSOCK 38)
(defconstant EDESTADDRREQ 39)
(defconstant EMSGSIZE 40)
(defconstant EPROTOTYPE 41)
(defconstant ENOPROTOOPT 42)
(defconstant EPROTONOSUPPORT 43)
(defconstant ESOCKTNOSUPPORT 44)
(defconstant EOPNOTSUPP 45)
(defconstant EPFNOSUPPORT 46)
(defconstant EAFNOSUPPORT 47)
(defconstant EADDRINUSE 48)
(defconstant EADDRNOTAVAIL 49)
;;;
;;; operational errors
(defconstant ENETDOWN 50)
(defconstant ENETUNREACH 51)
(defconstant ENETRESET 52)
(defconstant ECONNABORTED 53)
(defconstant ECONNRESET 54)
(defconstant ENOBUFS 55)
(defconstant EISCONN 56)
(defconstant ENOTCONN 57)
(defconstant ESHUTDOWN 58)
(defconstant ETOOMANYREFS 59)
(defconstant ETIMEDOUT 60)
(defconstant ECONNREFUSED 61)
;;; 
(defconstant ELOOP 62)
(defconstant ENAMETOOLONG 63)
;;; 
(defconstant EHOSTDOWN 64)
(defconstant EHOSTUNREACH 65)
(defconstant ENOTEMPTY 66)
;;; 
;;; quotas & resource 
(defconstant EPROCLIM 67)
(defconstant EUSERS 68)
(defconstant EDQUOT 69)
;;;
;;; CMU RFS
(defconstant ELOCAL 126)
(defconstant EREMOTE 127)
;;;
;;; VICE
(defconstant EVICEERR 70)
(defconstant EVICEOP 71)
