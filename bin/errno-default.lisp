;;; Default errno values.  These are used only if we could not
;;; auto-generate these forms.
(def-unix-error EPERM 1)
(def-unix-error ENOENT 2)
(def-unix-error ESRCH 3)
(def-unix-error EINTR 4)
(def-unix-error EIO 5)
(def-unix-error ENXIO 6)
(def-unix-error E2BIG 7)
(def-unix-error ENOEXEC 8)
(def-unix-error EBADF 9)
(def-unix-error ECHILD 10)
#+bsd(def-unix-error EDEADLK 11)
#-bsd(def-unix-error EAGAIN 11 #-linux)
(def-unix-error ENOMEM 12)
(def-unix-error EACCES 13)
(def-unix-error EFAULT 14)
(def-unix-error ENOTBLK 15)
(def-unix-error EBUSY 16)
(def-unix-error EEXIST 17)
(def-unix-error EXDEV 18)
(def-unix-error ENODEV 19)
(def-unix-error ENOTDIR 20)
(def-unix-error EISDIR 21)
(def-unix-error EINVAL 22)
(def-unix-error ENFILE 23)
(def-unix-error EMFILE 24)
(def-unix-error ENOTTY 25)
(def-unix-error ETXTBSY 26)
(def-unix-error EFBIG 27)
(def-unix-error ENOSPC 28)
(def-unix-error ESPIPE 29)
(def-unix-error EROFS 30)
(def-unix-error EMLINK 31)
(def-unix-error EPIPE 32)
;;; 
;;; Math
(def-unix-error EDOM 33)
(def-unix-error ERANGE 34 #-linux)

;;; non-blocking and interrupt i/o
(def-unix-error EWOULDBLOCK 35)
#-bsd(def-unix-error EDEADLK 35 _N"Operation would block") ; Ditto
#+bsd(def-unix-error EAGAIN 35)
(def-unix-error EINPROGRESS 36)
(def-unix-error EALREADY 37)
;;;
;;; ipc/network software
(def-unix-error ENOTSOCK 38)
(def-unix-error EDESTADDRREQ 39)
(def-unix-error EMSGSIZE 40)
(def-unix-error EPROTOTYPE 41)
(def-unix-error ENOPROTOOPT 42)
(def-unix-error EPROTONOSUPPORT 43)
(def-unix-error ESOCKTNOSUPPORT 44)
(def-unix-error EOPNOTSUPP 45)
(def-unix-error EPFNOSUPPORT 46)
(def-unix-error EAFNOSUPPORT 47)
(def-unix-error EADDRINUSE 48)
(def-unix-error EADDRNOTAVAIL 49)
;;;
;;; operational errors
(def-unix-error ENETDOWN 50)
(def-unix-error ENETUNREACH 51)
(def-unix-error ENETRESET 52)
(def-unix-error ECONNABORTED 53)
(def-unix-error ECONNRESET 54)
(def-unix-error ENOBUFS 55)
(def-unix-error EISCONN 56)
(def-unix-error ENOTCONN 57)
(def-unix-error ESHUTDOWN 58)
(def-unix-error ETOOMANYREFS 59)
(def-unix-error ETIMEDOUT 60)
(def-unix-error ECONNREFUSED 61)
;;; 
(def-unix-error ELOOP 62)
(def-unix-error ENAMETOOLONG 63)
;;; 
(def-unix-error EHOSTDOWN 64)
(def-unix-error EHOSTUNREACH 65)
(def-unix-error ENOTEMPTY 66)
;;; 
;;; quotas & resource 
(def-unix-error EPROCLIM 67)
(def-unix-error EUSERS 68)
(def-unix-error EDQUOT 69)
;;;
;;; CMU RFS
(def-unix-error ELOCAL 126)
(def-unix-error EREMOTE 127)
;;;
;;; VICE
(def-unix-error EVICEERR 70)
(def-unix-error EVICEOP 71)
