;;; -*- Package: UNIX -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/unix.lisp,v 1.35 1994/10/29 05:44:03 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the UNIX low-level support.
;;;
(in-package "UNIX")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "SYSTEM")
(use-package "EXT")

(export '(
	  daddr-t caddr-t ino-t swblk-t size-t time-t dev-t off-t uid-t gid-t
	  timeval tv-sec tv-usec timezone tz-minuteswest tz-dsttime
	  itimerval it-interval it-value tchars t-intrc t-quitc t-startc
	  t-stopc t-eofc t-brkc ltchars t-suspc t-dsuspc t-rprntc t-flushc
	  t-werasc t-lnextc sgttyb sg-ispeed sg-ospeed sg-erase sg-kill
	  sg-flags winsize ws-row ws-col ws-xpixel ws-ypixel
	  direct d-off d-ino d-reclen #-svr4 d-namlen d-name
	  stat st-dev st-mode st-nlink st-uid st-gid st-rdev st-size
	  st-atime st-mtime st-ctime st-blksize st-blocks
	  s-ifmt s-ifdir s-ifchr s-ifblk s-ifreg s-iflnk s-ifsock
	  s-isuid s-isgid s-isvtx s-iread s-iwrite s-iexec
	  ruseage ru-utime ru-stime ru-maxrss ru-ixrss ru-idrss
	  ru-isrss ru-minflt ru-majflt ru-nswap ru-inblock ru-oublock
	  ru-msgsnd ru-msgrcv ru-nsignals ru-nvcsw ru-nivcsw
	  rlimit rlim-cur rlim-max sc-onstack sc-mask sc-pc

	  unix-errno get-unix-error-msg

	  unix-pathname unix-file-mode unix-fd unix-pid unix-uid unix-gid

	  unix-access r_ok w_ok x_ok f_ok unix-chdir unix-chmod setuidexec
	  setgidexec savetext readown writeown execown readgrp writegrp
	  execgrp readoth writeoth execoth unix-fchmod unix-chown unix-fchown
	  unix-getdtablesize unix-close unix-creat unix-dup unix-dup2
	  unix-fcntl f-dupfd f-getfd f-setfd f-getfl f-setfl f-getown f-setown
	  fndelay fappend fasync fcreat ftrunc fexcl unix-link unix-lseek
	  l_set l_incr l_xtnd unix-mkdir unix-open o_rdonly o_wronly o_rdwr
	  #+svr4 o_ndelay
	  o_append o_creat o_trunc o_excl unix-pipe unix-read unix-readlink
	  unix-rename unix-rmdir unix-fast-select fd-setsize fd-set fd-clr
	  fd-isset fd-zero unix-select unix-sync unix-fsync unix-truncate
	  unix-ftruncate unix-symlink unix-unlink unix-write unix-ioctl
	  tcsetpgrp tcgetpgrp tty-process-group
	  terminal-speeds tty-raw tty-crmod tty-echo tty-lcase
	  #-(or hpux svr4) tty-cbreak #-(or hpux svr4) tty-tandem
	  #+(or hpux svr4) termios #+(or hpux svr4) c-lflag
	  #+(or hpux svr4) c-iflag #+(or hpux svr4) c-oflag
	  #+(or hpux svr4) tty-icrnl #+(or hpux svr4) tty-ocrnl
	  #+(or hpux svr4) vdsusp #+(or hpux svr4) veof
	  #+(or hpux svr4) vintr #+(or hpux svr4) vquit #+(or hpux svr4) vstart
	  #+(or hpux svr4) vstop #+(or hpux svr4) vsusp
	  #+(or hpux svr4) c-cflag
	  #+(or hpux svr4) c-cc #+(or hpux svr4) tty-icanon
	  #+(or hpux svr4) vmin #+(or hpux svr4) vtime
	  #+(or hpux svr4) tty-ixon
	  #+(or hpux svr4) tcsaflush #+(or hpux svr4)
	  unix-tcgetattr #+(or hpux svr4) unix-tcsetattr
	  TIOCGETP TIOCSETP TIOCFLUSH TIOCSETC TIOCGETC TIOCSLTC
	  TIOCGLTC TIOCNOTTY TIOCSPGRP TIOCGPGRP TIOCGWINSZ TIOCSWINSZ
	  KBDCGET KBDCSET KBDCRESET KBDCRST KBDCSSTD KBDSGET KBDGCLICK
	  KBDSCLICK FIONREAD #+hpux siocspgrp
	  unix-exit unix-stat unix-lstat unix-fstat
	  unix-getrusage unix-fast-getrusage rusage_self rusage_children
	  unix-gettimeofday
	  #-hpux unix-utimes #-(or svr4 hpux) unix-setreuid
	  #-(or svr4 hpux) unix-setregid
	  unix-getpid unix-getppid
	  unix-getgid unix-getegid unix-getpgrp unix-setpgrp unix-getuid
	  unix-getpagesize unix-gethostname unix-gethostid unix-fork
	  unix-current-directory unix-isatty unix-ttyname unix-execve
	  unix-socket unix-connect unix-bind unix-listen unix-accept
	  unix-recv unix-send))

(pushnew :unix *features*)


;;;; Common machine independent structures.

;;; From sys/types.h

(def-alien-type daddr-t #-alpha long #+alpha int)
(def-alien-type caddr-t (* char))
(def-alien-type ino-t #-alpha unsigned-long #+alpha unsigned-int)
(def-alien-type swblk-t long)
(def-alien-type size-t #-alpha long #+alpha unsigned-long)
(def-alien-type time-t #-alpha unsigned-long #+alpha unsigned-int)
(def-alien-type dev-t #-(or alpha svr4) short #+alpha int #+svr4 unsigned-long)
(def-alien-type off-t #-alpha long #+alpha unsigned-long)
(def-alien-type uid-t #-(or alpha svr4) unsigned-short #+alpha unsigned-int
  #+svr4 long)
(def-alien-type gid-t #-(or alpha svr4) unsigned-short #+alpha unsigned-int
  #+svr4 long)
(def-alien-type mode-t #-(or alpha svr4) unsigned-short
  #+(or alpha svr4) unsigned-long)
(def-alien-type nlink-t #-svr4 unsigned-short #+svr4 unsigned-long)

(defconstant FD-SETSIZE #-(or hpux alpha) 256 #+hpux 2048 #+alpha 4096)

(def-alien-type nil
  (struct fd-set
    (fds-bits (array #-alpha unsigned-long #+alpha int #.(/ fd-setsize 32)))))

(defmacro fd-set (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logior (truly-the (unsigned-byte 32) (ash 1 ,bit))
		     (deref (slot ,fd-set 'fds-bits) ,word))))))

(defmacro fd-clr (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logand (deref (slot ,fd-set 'fds-bits) ,word)
		     (32bit-logical-not
		      (truly-the (unsigned-byte 32) (ash 1 ,bit))))))))

(defmacro fd-isset (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (logbitp ,bit (deref (slot ,fd-set 'fds-bits) ,word)))))

(defmacro fd-zero (fd-set)
  `(progn
     ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	 collect `(setf (deref (slot ,fd-set 'fds-bits) ,index) 0))))

;;; From sys/time.h

(def-alien-type nil
  (struct timeval
    (tv-sec time-t)		; seconds
    (tv-usec time-t)))		; and microseconds

(def-alien-type nil
  (struct timezone
    (tz-minuteswest int)		; minutes west of Greenwich
    (tz-dsttime				; type of dst correction
     (enum nil :none :usa :aust :wet :met :eet :can))))

(def-alien-type nil
  (struct itimerval
    (it-interval (struct timeval))	; timer interval
    (it-value (struct timeval))))	; current value

#+svr4
; High-res time.  Actually posix definition under svr4 name.
(def-alien-type nil
  (struct timestruc-t
    (tv-sec time-t)
    (tv-nsec long)))

;;; From ioctl.h

(def-alien-type nil
  (struct tchars
    (t-intrc char)			; interrupt
    (t-quitc char)			; quit
    (t-startc char)			; start output
    (t-stopc char)			; stop output
    (t-eofc char)			; end-of-file
    (t-brkc char)))			; input delimiter (like nl)

(def-alien-type nil
  (struct ltchars
    (t-suspc char)			; stop process signal
    (t-dsuspc char)			; delayed stop process signal
    (t-rprntc char)			; reprint line
    (t-flushc char)			; flush output (toggles)
    (t-werasc char)			; word erase
    (t-lnextc char)))			; literal next character

(def-alien-type nil
  (struct sgttyb
    (sg-ispeed char)			; input speed.
    (sg-ospeed char)			; output speed
    (sg-erase char)			; erase character
    (sg-kill char)			; kill character
    (sg-flags #+mach short #-mach int))) ; mode flags

(def-alien-type nil
  (struct winsize
    (ws-row unsigned-short)		; rows, in characters
    (ws-col unsigned-short)		; columns, in characters
    (ws-xpixel unsigned-short)		; horizontal size, pixels
    (ws-ypixel unsigned-short)))	; veritical size, pixels


;;; From sys/termio.h

#+(or svr4 hpux)
(def-alien-type nil
  (struct termios
    (c-iflag unsigned-int)
    (c-oflag unsigned-int)
    (c-cflag unsigned-int)
    (c-lflag unsigned-int)
    #+hpux
    (c-reserved unsigned-int)
    (c-cc (array unsigned-char #+hpux 16 #+solaris 19 #+irix 23))))

;;; From sys/dir.h

(def-alien-type nil
  (struct direct
    #+(and sunos (not svr4)) (d-off long) ; offset of next disk directory entry
    (d-ino ino-t); inode number of entry
    #+svr4 (d-off long)
    (d-reclen unsigned-short)		; length of this record
    #-svr4
    (d-namlen unsigned-short)		; length of string in d-name
    (d-name (array char 256))))		; name must be no longer than this

;;; From sys/stat.h

#-svr4
(def-alien-type nil
  (struct stat
    (st-dev dev-t)
    (st-ino ino-t)
    (st-mode mode-t)
    (st-nlink nlink-t)
    (st-uid uid-t)
    (st-gid gid-t)
    (st-rdev dev-t)
    (st-size off-t)
    (st-atime time-t)
    (st-spare1 int)
    (st-mtime time-t)
    (st-spare2 int)
    (st-ctime time-t)
    (st-spare3 int)
    (st-blksize #-alpha long #+alpha unsigned-int)
    (st-blocks #-alpha long #+alpha int)
    (st-spare4 (array long 2))))

#+svr4
(def-alien-type nil
  (struct stat
    (st-dev dev-t)
    (st-pad1 (array long 3))
    (st-ino ino-t)
    (st-mode unsigned-long)
    (st-nlink short)
    (st-uid uid-t)
    (st-gid gid-t)
    (st-rdev dev-t)
    (st-pad2 (array long 2))
    (st-size off-t)
    (st-pad3 long)
    (st-atime (struct timestruc-t))
    (st-mtime (struct timestruc-t))
    (st-ctime (struct timestruc-t))
    (st-blksize long)
    (st-blocks long)
    (st-fstype (array char 16))
    (st-pad4 (array long 8))))

(defconstant s-ifmt #o0170000)
(defconstant s-ifdir #o0040000)
(defconstant s-ifchr #o0020000)
(defconstant s-ifblk #o0060000)
(defconstant s-ifreg #o0100000)
(defconstant s-iflnk #o0120000)
(defconstant s-ifsock #o0140000)
(defconstant s-isuid #o0004000)
(defconstant s-isgid #o0002000)
(defconstant s-isvtx #o0001000)
(defconstant s-iread #o0000400)
(defconstant s-iwrite #o0000200)
(defconstant s-iexec #o0000100)

;;; From sys/resource.h

(def-alien-type nil
  (struct rusage
    (ru-utime (struct timeval))		; user time used
    (ru-stime (struct timeval))		; system time used.
    (ru-maxrss long)
    (ru-ixrss long)			; integral sharded memory size
    (ru-idrss long)			; integral unsharded data "
    (ru-isrss long)			; integral unsharded stack "
    (ru-minflt long)			; page reclaims
    (ru-majflt long)			; page faults
    (ru-nswap long)			; swaps
    (ru-inblock long)			; block input operations
    (ru-oublock long)			; block output operations
    (ru-msgsnd long)			; messages sent
    (ru-msgrcv long)			; messages received
    (ru-nsignals long)			; signals received
    (ru-nvcsw long)			; voluntary context switches
    (ru-nivcsw long)))			; involuntary "

(def-alien-type nil
  (struct rlimit
    (rlim-cur #-alpha int #+alpha unsigned-int)	 ; current (soft) limit
    (rlim-max #-alpha int #+alpha unsigned-int))); maximum value for rlim-cur



;;;; Errno stuff.

(eval-when (compile eval)

(defparameter *compiler-unix-errors* nil)

(defmacro def-unix-error (name number description)
  `(progn
     (eval-when (compile eval)
       (push (cons ,number ,description) *compiler-unix-errors*))
     (defconstant ,name ,number ,description)
     (export ',name)))

(defmacro emit-unix-errors ()
  (let* ((max (apply #'max (mapcar #'car *compiler-unix-errors*)))
	 (array (make-array (1+ max) :initial-element nil)))
    (dolist (error *compiler-unix-errors*)
      (setf (svref array (car error)) (cdr error)))
    `(progn
       (defvar *unix-errors* ',array)
       (proclaim '(simple-vector *unix-errors*)))))

) ;eval-when

;;; 
;;; From <errno.h>
;;; 
(def-unix-error ESUCCESS 0 "Successful")
(def-unix-error EPERM 1 "Not owner")
(def-unix-error ENOENT 2 "No such file or directory")
(def-unix-error ESRCH 3 "No such process")
(def-unix-error EINTR 4 "Interrupted system call")
(def-unix-error EIO 5 "I/O error")
(def-unix-error ENXIO 6 "No such device or address")
(def-unix-error E2BIG 7 "Arg list too long")
(def-unix-error ENOEXEC 8 "Exec format error")
(def-unix-error EBADF 9 "Bad file number")
(def-unix-error ECHILD 10 "No children")
(def-unix-error EAGAIN 11 "No more processes")
(def-unix-error ENOMEM 12 "Not enough core")
(def-unix-error EACCES 13 "Permission denied")
(def-unix-error EFAULT 14 "Bad address")
(def-unix-error ENOTBLK 15 "Block device required")
(def-unix-error EBUSY 16 "Mount device busy")
(def-unix-error EEXIST 17 "File exists")
(def-unix-error EXDEV 18 "Cross-device link")
(def-unix-error ENODEV 19 "No such device")
(def-unix-error ENOTDIR 20 "Not a director")
(def-unix-error EISDIR 21 "Is a directory")
(def-unix-error EINVAL 22 "Invalid argument")
(def-unix-error ENFILE 23 "File table overflow")
(def-unix-error EMFILE 24 "Too many open files")
(def-unix-error ENOTTY 25 "Not a typewriter")
(def-unix-error ETXTBSY 26 "Text file busy")
(def-unix-error EFBIG 27 "File too large")
(def-unix-error ENOSPC 28 "No space left on device")
(def-unix-error ESPIPE 29 "Illegal seek")
(def-unix-error EROFS 30 "Read-only file system")
(def-unix-error EMLINK 31 "Too many links")
(def-unix-error EPIPE 32 "Broken pipe")
;;; 
;;; Math
(def-unix-error EDOM 33 "Argument too large")
(def-unix-error ERANGE 34 "Result too large")
;;; 
#-svr4
(progn
;;; non-blocking and interrupt i/o
(def-unix-error EWOULDBLOCK 35 "Operation would block")
(def-unix-error EDEADLK 35 "Operation would block") ; Ditto
(def-unix-error EINPROGRESS 36 "Operation now in progress")
(def-unix-error EALREADY 37 "Operation already in progress")
;;;
;;; ipc/network software
(def-unix-error ENOTSOCK 38 "Socket operation on non-socket")
(def-unix-error EDESTADDRREQ 39 "Destination address required")
(def-unix-error EMSGSIZE 40 "Message too long")
(def-unix-error EPROTOTYPE 41 "Protocol wrong type for socket")
(def-unix-error ENOPROTOOPT 42 "Protocol not available")
(def-unix-error EPROTONOSUPPORT 43 "Protocol not supported")
(def-unix-error ESOCKTNOSUPPORT 44 "Socket type not supported")
(def-unix-error EOPNOTSUPP 45 "Operation not supported on socket")
(def-unix-error EPFNOSUPPORT 46 "Protocol family not supported")
(def-unix-error EAFNOSUPPORT 47 "Address family not supported by protocol family")
(def-unix-error EADDRINUSE 48 "Address already in use")
(def-unix-error EADDRNOTAVAIL 49 "Can't assign requested address")
;;;
;;; operational errors
(def-unix-error ENETDOWN 50 "Network is down")
(def-unix-error ENETUNREACH 51 "Network is unreachable")
(def-unix-error ENETRESET 52 "Network dropped connection on reset")
(def-unix-error ECONNABORTED 53 "Software caused connection abort")
(def-unix-error ECONNRESET 54 "Connection reset by peer")
(def-unix-error ENOBUFS 55 "No buffer space available")
(def-unix-error EISCONN 56 "Socket is already connected")
(def-unix-error ENOTCONN 57 "Socket is not connected")
(def-unix-error ESHUTDOWN 58 "Can't send after socket shutdown")
(def-unix-error ETOOMANYREFS 59 "Too many references: can't splice")
(def-unix-error ETIMEDOUT 60 "Connection timed out")
(def-unix-error ECONNREFUSED 61 "Connection refused")
;;; 
(def-unix-error ELOOP 62 "Too many levels of symbolic links")
(def-unix-error ENAMETOOLONG 63 "File name too long")
;;; 
(def-unix-error EHOSTDOWN 64 "Host is down")
(def-unix-error EHOSTUNREACH 65 "No route to host")
(def-unix-error ENOTEMPTY 66 "Directory not empty")
;;; 
;;; quotas & resource 
(def-unix-error EPROCLIM 67 "Too many processes")
(def-unix-error EUSERS 68 "Too many users")
(def-unix-error EDQUOT 69 "Disc quota exceeded")
;;;
;;; CMU RFS
(def-unix-error ELOCAL 126 "namei should continue locally")
(def-unix-error EREMOTE 127 "namei was handled remotely")
;;;
;;; VICE
(def-unix-error EVICEERR 70 "Remote file system error ")
(def-unix-error EVICEOP 71 "syscall was handled by Vice")
;;;
;;; Mach Emulation
(def-unix-error ERESTART 72 "Mach Emulation Error (?)")
)
#+svr4
(progn
(def-unix-error ENOMSG 35 "No message of desired type")
(def-unix-error EIDRM 36 "Identifier removed")
(def-unix-error ECHRNG 37 "Channel number out of range")
(def-unix-error EL2NSYNC 38 "Level 2 not synchronized")
(def-unix-error EL3HLT 39 "Level 3 halted")
(def-unix-error EL3RST 40 "Level 3 reset")
(def-unix-error ELNRNG 41 "Link number out of range")
(def-unix-error EUNATCH 42 "Protocol driver not attached")
(def-unix-error ENOCSI 43 "No CSI structure available")
(def-unix-error EL2HLT 44 "Level 2 halted")
(def-unix-error EDEADLK 45 "Deadlock situation detected/avoided")
(def-unix-error ENOLCK 46 "No record locks available")
(def-unix-error ECANCELED 47 "Error 47")
(def-unix-error ENOTSUP 48 "Error 48")
(def-unix-error EBADE 50 "Bad exchange descriptor")
(def-unix-error EBADR 51 "Bad request descriptor")
(def-unix-error EXFULL 52 "Message tables full")
(def-unix-error ENOANO 53 "Anode table overflow")
(def-unix-error EBADRQC 54 "Bad request code")
(def-unix-error EBADSLT 55 "Invalid slot")
(def-unix-error EDEADLOCK 56 "File locking deadlock")
(def-unix-error EBFONT 57 "Bad font file format")
(def-unix-error ENOSTR 60 "Not a stream device")
(def-unix-error ENODATA 61 "No data available")
(def-unix-error ETIME 62 "Timer expired")
(def-unix-error ENOSR 63 "Out of stream resources")
(def-unix-error ENONET 64 "Machine is not on the network")
(def-unix-error ENOPKG 65 "Package not installed")
(def-unix-error EREMOTE 66 "Object is remote")
(def-unix-error ENOLINK 67 "Link has been severed")
(def-unix-error EADV 68 "Advertise error")
(def-unix-error ESRMNT 69 "Srmount error")
(def-unix-error ECOMM 70 "Communication error on send")
(def-unix-error EPROTO 71 "Protocol error")
(def-unix-error EMULTIHOP 74 "Multihop attempted")
(def-unix-error EBADMSG 77 "Not a data message")
(def-unix-error ENAMETOOLONG 78 "File name too long")
(def-unix-error EOVERFLOW 79 "Value too large for defined data type")
(def-unix-error ENOTUNIQ 80 "Name not unique on network")
(def-unix-error EBADFD 81 "File descriptor in bad state")
(def-unix-error EREMCHG 82 "Remote address changed")
(def-unix-error ELIBACC 83 "Can not access a needed shared library")
(def-unix-error ELIBBAD 84 "Accessing a corrupted shared library")
(def-unix-error ELIBSCN 85 ".lib section in a.out corrupted")
(def-unix-error ELIBMAX 86 "Attempting to link in more shared libraries than system limit")
(def-unix-error ELIBEXEC 87 "Can not exec a shared library directly")
(def-unix-error EILSEQ 88 "Error 88")
(def-unix-error ENOSYS 89 "Operation not applicable")
(def-unix-error ELOOP 90 "Number of symbolic links encountered during path name traversal exceeds MAXSYMLINKS")
(def-unix-error ERESTART 91 "Error 91")
(def-unix-error ESTRPIPE 92 "Error 92")
(def-unix-error ENOTEMPTY 93 "Directory not empty")
(def-unix-error EUSERS 94 "Too many users")
(def-unix-error ENOTSOCK 95 "Socket operation on non-socket")
(def-unix-error EDESTADDRREQ 96 "Destination address required")
(def-unix-error EMSGSIZE 97 "Message too long")
(def-unix-error EPROTOTYPE 98 "Protocol wrong type for socket")
(def-unix-error ENOPROTOOPT 99 "Option not supported by protocol")
(def-unix-error EPROTONOSUPPORT 120 "Protocol not supported")
(def-unix-error ESOCKTNOSUPPORT 121 "Socket type not supported")
(def-unix-error EOPNOTSUPP 122 "Operation not supported on transport endpoint")
(def-unix-error EPFNOSUPPORT 123 "Protocol family not supported")
(def-unix-error EAFNOSUPPORT 124 "Address family not supported by protocol family")
(def-unix-error EADDRINUSE 125 "Address already in use")
(def-unix-error EADDRNOTAVAIL 126 "Cannot assign requested address")
(def-unix-error ENETDOWN 127 "Network is down")
(def-unix-error ENETUNREACH 128 "Network is unreachable")
(def-unix-error ENETRESET 129 "Network dropped connection because of reset")
(def-unix-error ECONNABORTED 130 "Software caused connection abort")
(def-unix-error ECONNRESET 131 "Connection reset by peer")
(def-unix-error ENOBUFS 132 "No buffer space available")
(def-unix-error EISCONN 133 "Transport endpoint is already connected")
(def-unix-error ENOTCONN 134 "Transport endpoint is not connected")
(def-unix-error ESHUTDOWN 143 "Cannot send after socket shutdown")
(def-unix-error ETOOMANYREFS 144 "Too many references: cannot splice")
(def-unix-error ETIMEDOUT 145 "Connection timed out")
(def-unix-error ECONNREFUSED 146 "Connection refused")
(def-unix-error EHOSTDOWN 147 "Host is down")
(def-unix-error EHOSTUNREACH 148 "No route to host")
(def-unix-error EWOULDBLOCK 11 "Resource temporarily unavailable")
(def-unix-error EALREADY 149 "Operation already in progress")
(def-unix-error EINPROGRESS 150 "Operation now in progress")
(def-unix-error ESTALE 151 "Stale NFS file handle")
)
;;;
;;; And now for something completely different ...
(emit-unix-errors)

(def-alien-variable ("errno" unix-errno) int)

;;; GET-UNIX-ERROR-MSG -- public.
;;; 
(defun get-unix-error-msg (&optional (error-number unix-errno))
  "Returns a string describing the error number which was returned by a
  UNIX system call."
  (declare (type integer error-number))
  (if (array-in-bounds-p *unix-errors* error-number)
      (svref *unix-errors* error-number)
      (format nil "Unknown error [~d]" error-number)))



;;;; Lisp types used by syscalls.

(deftype unix-pathname () 'simple-string)
(deftype unix-fd () `(integer 0 ,most-positive-fixnum))

#+mach
(progn
  (deftype unix-file-mode () '(unsigned-byte 16))
  (deftype unix-pid () '(unsigned-byte 16))
  (deftype unix-uid () '(unsigned-byte 16))
  (deftype unix-gid () '(unsigned-byte 16)))

#-mach
(progn
  (deftype unix-file-mode () '(unsigned-byte 32))
  (deftype unix-pid () '(unsigned-byte 32))
  (deftype unix-uid () '(unsigned-byte 32))
  (deftype unix-gid () '(unsigned-byte 32)))


;;;; System calls.


(defmacro syscall ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (values nil unix-errno)
	 ,success-form)))

;;; Like syscall, but if it fails, signal an error instead of returing error
;;; codes.  Should only be used for syscalls that will never really get an
;;; error.
;;;
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (error "Syscall ~A failed: ~A" ,name (get-unix-error-msg))
	 ,success-form)))

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values result 0) ,@args))


;;; Unix-access accepts a path and a mode.  It returns two values the
;;; first is T if the file is accessible and NIL otherwise.  The second
;;; only has meaning in the second case and is the unix errno value.

(defconstant r_ok 4 "Test for read permission")
(defconstant w_ok 2 "Test for write permission")
(defconstant x_ok 1 "Test for execute permission")
(defconstant f_ok 0 "Test for presence of file")

(defun unix-access (path mode)
  "Given a file path (a string) and one of four constant modes,
   unix-access returns T if the file is accessible with that
   mode and NIL if not.  It also returns an errno value with
   NIL which determines why the file was not accessible.

   The access modes are:
	r_ok     Read permission.
	w_ok     Write permission.
	x_ok     Execute permission.
	f_ok     Presence of file."
  (declare (type unix-pathname path)
	   (type (mod 8) mode))
  (void-syscall ("access" c-string int) path mode))

;;; Unix-chdir accepts a directory name and makes that the
;;; current working directory.

(defun unix-chdir (path)
  "Given a file path string, unix-chdir changes the current working 
   directory to the one specified."
  (declare (type unix-pathname path))
  (void-syscall ("chdir" c-string) path))

;;; Unix-chmod accepts a path and a mode and changes the mode to the new mode.

(defconstant setuidexec #o4000 "Set user ID on execution")
(defconstant setgidexec #o2000 "Set group ID on execution")
(defconstant savetext #o1000 "Save text image after execution")
(defconstant readown #o400 "Read by owner")
(defconstant writeown #o200 "Write by owner")
(defconstant execown #o100 "Execute (search directory) by owner")
(defconstant readgrp #o40 "Read by group")
(defconstant writegrp #o20 "Write by group")
(defconstant execgrp #o10 "Execute (search directory) by group")
(defconstant readoth #o4 "Read by others")
(defconstant writeoth #o2 "Write by others")
(defconstant execoth #o1 "Execute (search directory) by others")

(defun unix-chmod (path mode)
  "Given a file path string and a constant mode, unix-chmod changes the
   permission mode for that file to the one specified. The new mode
   can be created by logically OR'ing the following:

      setuidexec        Set user ID on execution.
      setgidexec        Set group ID on execution.
      savetext          Save text image after execution.
      readown           Read by owner.
      writeown          Write by owner.
      execown           Execute (search directory) by owner.
      readgrp           Read by group.
      writegrp          Write by group.
      execgrp           Execute (search directory) by group.
      readoth           Read by others.
      writeoth          Write by others.
      execoth           Execute (search directory) by others.
  
  It returns T on successfully completion; NIL and an error number
  otherwise."
  (declare (type unix-pathname path)
	   (type unix-file-mode mode))
  (void-syscall ("chmod" c-string int) path mode))

;;; Unix-fchmod accepts a file descriptor ("fd") and a file protection mode
;;; ("mode") and changes the protection of the file described by "fd" to 
;;; "mode".

(defun unix-fchmod (fd mode)
  "Given an integer file descriptor and a mode (the same as those
   used for unix-chmod), unix-fchmod changes the permission mode
   for that file to the one specified. T is returned if the call
   was successful."
  (declare (type unix-fd fd)
	   (type unix-file-mode mode))
  (void-syscall ("fchmod" int int) fd mode))

(defun unix-chown (path uid gid)
  "Given a file path, an integer user-id, and an integer group-id,
   unix-chown changes the owner of the file and the group of the
   file to those specified.  Either the owner or the group may be
   left unchanged by specifying them as -1.  Note: Permission will
   fail if the caller is not the superuser."
  (declare (type unix-pathname path)
	   (type (or unix-uid (integer -1 -1)) uid)
	   (type (or unix-gid (integer -1 -1)) gid))
  (void-syscall ("chown" c-string int int) path uid gid))

;;; Unix-fchown is exactly the same as unix-chown except that the file
;;; is specified by a file-descriptor ("fd") instead of a pathname.

(defun unix-fchown (fd uid gid)
  "Unix-fchown is like unix-chown, except that it accepts an integer
   file descriptor instead of a file path name."
  (declare (type unix-fd fd)
	   (type (or unix-uid (integer -1 -1)) uid)
	   (type (or unix-gid (integer -1 -1)) gid))
  (void-syscall ("fchown" int int int) fd uid gid))

;;; Returns the maximum size (i.e. the number of array elements
;;; of the file descriptor table.

(defun unix-getdtablesize ()
  "Unix-getdtablesize returns the maximum size of the file descriptor
   table. (i.e. the maximum number of descriptors that can exist at
   one time.)"
  (int-syscall ("getdtablesize")))

;;; Unix-close accepts a file descriptor and attempts to close the file
;;; associated with it.

(defun unix-close (fd)
  "Unix-close takes an integer file descriptor as an argument and
   closes the file associated with it.  T is returned upon successful
   completion, otherwise NIL and an error number."
  (declare (type unix-fd fd))
  (void-syscall ("close" int) fd))

;;; Unix-creat accepts a file name and a mode.  It creates a new file
;;; with name and sets it mode to mode (as for chmod).

(defun unix-creat (name mode)
  "Unix-creat accepts a file name and a mode (same as those for
   unix-chmod) and creates a file by that name with the specified
   permission mode.  It returns T on success, or NIL and an error
   number otherwise."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("creat" c-string int) name mode))

;;; Unix-dup returns a duplicate copy of the existing file-descriptor
;;; passed as an argument.

(defun unix-dup (fd)
  "Unix-dup duplicates an existing file descriptor (given as the
   argument) and return it.  If FD is not a valid file descriptor, NIL
   and an error number are returned."
  (declare (type unix-fd fd))
  (int-syscall ("dup" int) fd))

;;; Unix-dup2 makes the second file-descriptor describe the same file
;;; as the first. If the second file-descriptor points to an open
;;; file, it is first closed. In any case, the second should have a 
;;; value which is a valid file-descriptor.

(defun unix-dup2 (fd1 fd2)
  "Unix-dup2 duplicates an existing file descriptor just as unix-dup
   does only the new value of the duplicate descriptor may be requested
   through the second argument.  If a file already exists with the
   requested descriptor number, it will be closed and the number
   assigned to the duplicate."
  (declare (type unix-fd fd1 fd2))
  (void-syscall ("dup2" int int) fd1 fd2))

;;; Unix-fcntl takes a file descriptor, an integer command
;;; number, and optional command arguments.  It performs
;;; operations on the associated file and/or returns inform-
;;; ation about the file.

;;; Operations performed on file descriptors:

(defconstant F-DUPFD    0  "Duplicate a file descriptor")
(defconstant F-GETFD    1  "Get file desc. flags")
(defconstant F-SETFD    2  "Set file desc. flags")
(defconstant F-GETFL    3  "Get file flags")
(defconstant F-SETFL    4  "Set file flags")
#-svr4
(defconstant F-GETOWN   5  "Get owner")
#+svr4
(defconstant F-GETOWN   23  "Get owner")
#-svr4
(defconstant F-SETOWN   6  "Set owner")
#+svr4
(defconstant F-SETOWN   24  "Set owner")


;;; File flags for F-GETFL and F-SETFL:

(defconstant FNDELAY  #-osf1 #o0004 #+osf1 #o100000 "Non-blocking reads")
(defconstant FAPPEND  #o0010   "Append on each write")
(defconstant FASYNC   #-svr4 #o0100 #+svr4 #o10000
  "Signal pgrp when data ready")
(defconstant FCREAT   #-(or hpux svr4) #o1000 #+(or hpux svr4) #o0400
  "Create if nonexistant")
(defconstant FTRUNC   #-(or hpux svr4) #o2000 #+(or hpux svr4) #o1000
  "Truncate to zero length")
(defconstant FEXCL    #-(or hpux svr4) #o4000 #+(or hpux svr4) #o2000
  "Error if already created")

(defun unix-fcntl (fd cmd arg)
  "Unix-fcntl manipulates file descriptors according to the
   argument CMD which can be one of the following:

   F-DUPFD         Duplicate a file descriptor.
   F-GETFD         Get file descriptor flags.
   F-SETFD         Set file descriptor flags.
   F-GETFL         Get file flags.
   F-SETFL         Set file flags.
   F-GETOWN        Get owner.
   F-SETOWN        Set owner.

   The flags that can be specified for F-SETFL are:

   FNDELAY         Non-blocking reads.
   FAPPEND         Append on each write.
   FASYNC          Signal pgrp when data ready.
   FCREAT          Create if nonexistant.
   FTRUNC          Truncate to zero length.
   FEXCL           Error if already created.
   "
  (declare (type unix-fd fd)
	   (type (integer 0 6) cmd)
	   (type (unsigned-byte 16) arg))
  (int-syscall ("fcntl" int int int) fd cmd arg))

;;; Unix-link creates a hard link from name2 to name1.

(defun unix-link (name1 name2)
  "Unix-link creates a hard link from the file with name1 to the
   file with name2."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("link" c-string c-string) name1 name2))

;;; Unix-lseek accepts a file descriptor, an offset, and whence value.

(defconstant l_set 0 "set the file pointer")
(defconstant l_incr 1 "increment the file pointer")
(defconstant l_xtnd 2 "extend the file size")

(defun unix-lseek (fd offset whence)
  "Unix-lseek accepts a file descriptor and moves the file pointer ahead
   a certain offset for that file.  Whence can be any of the following:

   l_set        Set the file pointer.
   l_incr       Increment the file pointer.
   l_xtnd       Extend the file size.
  "
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) offset)
	   (type (integer 0 2) whence))
  (int-syscall ("lseek" int off-t int) fd offset whence))

;;; Unix-mkdir accepts a name and a mode and attempts to create the
;;; corresponding directory with mode mode.

(defun unix-mkdir (name mode)
  "Unix-mkdir creates a new directory with the specified name and mode.
   (Same as those for unix-fchmod.)  It returns T upon success, otherwise
   NIL and an error number."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("mkdir" c-string int) name mode))

;;; Unix-open accepts a pathname (a simple string), flags, and mode and
;;; attempts to open file with name pathname.

(defconstant o_rdonly 0 "Read-only flag.") 
(defconstant o_wronly 1 "Write-only flag.")
(defconstant o_rdwr 2   "Read-write flag.")
(defconstant o_append #o10   "Append flag.")
#+svr4 (defconstant o_ndelay 4 "Non-blocking I/O")
#+(or hpux svr4)
(progn
  (defconstant o_creat #o400  "Create if nonexistant flag.") 
  (defconstant o_trunc #o1000  "Truncate flag.")
  (defconstant o_excl #o2000  "Error if already exists."))
#-(or hpux svr4)
(progn
  (defconstant o_creat #o1000  "Create if nonexistant flag.") 
  (defconstant o_trunc #o2000  "Truncate flag.")
  (defconstant o_excl #o4000  "Error if already exists."))

(defun unix-open (path flags mode)
  "Unix-open opens the file whose pathname is specified by path
   for reading and/or writing as specified by the flags argument.
   The flags argument can be:

     o_rdonly        Read-only flag.
     o_wronly        Write-only flag.
     o_rdwr          Read-and-write flag.
     o_append        Append flag.
     o_creat         Create-if-nonexistant flag.
     o_trunc         Truncate-to-size-0 flag.

   If the o_creat flag is specified, then the file is created with
   a permission of argument mode if the file doesn't exist.  An
   integer file descriptor is returned by unix-open."
  (declare (type unix-pathname path)
	   (type (unsigned-byte 16) flags)
	   (type unix-file-mode mode))
  (int-syscall ("open" c-string int int) path flags mode))

(defun unix-pipe ()
  "Unix-pipe sets up a unix-piping mechanism consisting of
  an input pipe and an output pipe.  Unix-Pipe returns two
  values: if no error occurred the first value is the pipe
  to be read from and the second is can be written to.  If
  an error occurred the first value is NIL and the second
  the unix error code."
  (with-alien ((fds (array int 2)))
    (syscall ("pipe" (* int))
	     (values (deref fds 0) (deref fds 1))
	     (cast fds (* int)))))

;;; Unix-read accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer.  It returns the actual number of
;;; bytes read.

(defun unix-read (fd buf len)
  "Unix-read attempts to read from the file described by fd into
   the buffer buf until it is full.  Len is the length of the buffer.
   The number of bytes actually read is returned or NIL and an error
   number if an error occured."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))
  #+sunos
  ;; Note: Under sunos we touch each page before doing the read to give
  ;; the segv handler a chance to fix the permissions.  Otherwise,
  ;; read will return EFAULT.  This also bypasses a bug in 4.1.1 in which
  ;; read fails with EFAULT if the page has never been touched even if
  ;; the permissions are okay.
  (without-gcing
   (let* ((page-size (get-page-size))
	  (1-page-size (1- page-size))
	  (sap (etypecase buf
		 (system-area-pointer buf)
		 (vector (vector-sap buf))))
	  (end (sap+ sap len)))
     (declare (type (and fixnum unsigned-byte) page-size 1-page-size)
	      (type system-area-pointer sap end)
	      (optimize (speed 3) (safety 0)))
     (do ((sap (int-sap (logand (the (unsigned-byte 32)
				     (+ (sap-int sap) 1-page-size))
				(logxor 1-page-size (ldb (byte 32 0) -1))))
	       (sap+ sap page-size)))
	 ((sap>= sap end))
       (declare (type system-area-pointer sap))
       (setf (sap-ref-8 sap 0) (sap-ref-8 sap 0)))))
  (int-syscall ("read" int (* char) int) fd buf len))

(defun unix-readlink (path)
  "Unix-readlink invokes the readlink system call on the file name
  specified by the simple string path.  It returns up to two values:
  the contents of the symbolic link if the call is successful, or
  NIL and the Unix error number."
  (declare (type unix-pathname path))
  (with-alien ((buf (array char 1024)))
    (syscall ("readlink" c-string (* char) int)
	     (let ((string (make-string result)))
	       (kernel:copy-from-system-area
		(alien-sap buf) 0
		string (* vm:vector-data-offset vm:word-bits)
		(* result vm:byte-bits))
	       string)
	     path (cast buf (* char)) 1024)))

;;; Unix-rename accepts two files names and renames the first to the second.

(defun unix-rename (name1 name2)
  "Unix-rename renames the file with string name1 to the string
   name2.  NIL and an error code is returned if an error occured."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("rename" c-string c-string) name1 name2))

;;; Unix-rmdir accepts a name and removes the associated directory.

(defun unix-rmdir (name)
  "Unix-rmdir attempts to remove the directory name.  NIL and
   an error number is returned if an error occured."
  (declare (type unix-pathname name))
  (void-syscall ("rmdir" c-string) name))


;;; UNIX-FAST-SELECT -- public.
;;;
(defmacro unix-fast-select (num-descriptors
			    read-fds write-fds exception-fds
			    timeout-secs &optional (timeout-usecs 0))
  "Perform the UNIX select(2) system call.
  (declare (type (integer 0 #.FD-SETSIZE) num-descriptors)
	   (type (or (alien (* (struct fd-set))) null)
		 read-fds write-fds exception-fds)
	   (type (or null (unsigned-byte 31)) timeout-secs)
	   (type (unsigned-byte 31) timeout-usecs)
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))"
  `(let ((timeout-secs ,timeout-secs))
     (with-alien ((tv (struct timeval)))
       (when timeout-secs
	 (setf (slot tv 'tv-sec) timeout-secs)
	 (setf (slot tv 'tv-usec) ,timeout-usecs))
       (int-syscall ("select" int (* (struct fd-set)) (* (struct fd-set))
		     (* (struct fd-set)) (* (struct timeval)))
		    ,num-descriptors ,read-fds ,write-fds ,exception-fds
		    (if timeout-secs (alien-sap (addr tv)) (int-sap 0))))))


;;; Unix-select accepts sets of file descriptors and waits for an event
;;; to happen on one of them or to time out.

(defmacro num-to-fd-set (fdset num)
  `(if (fixnump ,num)
       (progn
	 (setf (deref (slot ,fdset 'fds-bits) 0) ,num)
	 ,@(loop for index upfrom 1 below (/ fd-setsize 32)
	     collect `(setf (deref (slot ,fdset 'fds-bits) ,index) 0)))
       (progn
	 ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	     collect `(setf (deref (slot ,fdset 'fds-bits) ,index)
			    (ldb (byte 32 ,(* index 32)) ,num))))))

(defmacro fd-set-to-num (nfds fdset)
  `(if (<= ,nfds 32)
       (deref (slot ,fdset 'fds-bits) 0)
       (+ ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	      collect `(ash (deref (slot ,fdset 'fds-bits) ,index)
			    ,(* index 32))))))

(defun unix-select (nfds rdfds wrfds xpfds to-secs &optional (to-usecs 0))
  "Unix-select examines the sets of descriptors passed as arguments
   to see if they are ready for reading and writing.  See the UNIX
   Programmers Manual for more information."
  (declare (type (integer 0 #.FD-SETSIZE) nfds)
	   (type unsigned-byte rdfds wrfds xpfds)
	   (type (or (unsigned-byte 31) null) to-secs)
	   (type (unsigned-byte 31) to-usecs)
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (with-alien ((tv (struct timeval))
	       (rdf (struct fd-set))
	       (wrf (struct fd-set))
	       (xpf (struct fd-set)))
    (when to-secs
      (setf (slot tv 'tv-sec) to-secs)
      (setf (slot tv 'tv-usec) to-usecs))
    (num-to-fd-set rdf rdfds)
    (num-to-fd-set wrf wrfds)
    (num-to-fd-set xpf xpfds)
    (macrolet ((frob (lispvar alienvar)
		 `(if (zerop ,lispvar)
		      (int-sap 0)
		      (alien-sap (addr ,alienvar)))))
      (syscall ("select" int (* (struct fd-set)) (* (struct fd-set))
		(* (struct fd-set)) (* (struct timeval)))
	       (values result
		       (fd-set-to-num nfds rdf)
		       (fd-set-to-num nfds wrf)
		       (fd-set-to-num nfds xpf))
	       nfds (frob rdfds rdf) (frob wrfds wrf) (frob xpfds xpf)
	       (if to-secs (alien-sap (addr tv)) (int-sap 0))))))


;;; Unix-sync writes all information in core memory which has been modified
;;; to permanent storage (i.e. disk).

(defun unix-sync ()
  "Unix-sync writes all information in core memory which has been
   modified to disk.  It returns NIL and an error code if an error
   occured."
  (void-syscall ("sync")))

;;; Unix-fsync writes the core-image of the file described by "fd" to
;;; permanent storage (i.e. disk).

(defun unix-fsync (fd)
  "Unix-fsync writes the core image of the file described by
   fd to disk."
  (declare (type unix-fd fd))
  (void-syscall ("fsync" int) fd))

;;; Unix-truncate accepts a file name and a new length.  The file is
;;; truncated to the new length.

(defun unix-truncate (name len)
  "Unix-truncate truncates the named file to the length (in
   bytes) specified by len.  NIL and an error number is returned
   if the call is unsuccessful."
  (declare (type unix-pathname name)
	   (type (unsigned-byte 32) len))
  (void-syscall ("truncate" c-string int) name len))

(defun unix-ftruncate (fd len)
  "Unix-ftruncate is similar to unix-truncate except that the first
   argument is a file descriptor rather than a file name."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))
  (void-syscall ("ftruncate" int int) fd len))

(defun unix-symlink (name1 name2)
  "Unix-symlink creates a symbolic link named name2 to the file
   named name1.  NIL and an error number is returned if the call
   is unsuccessful."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("symlink" c-string c-string) name1 name2))

;;; Unix-unlink accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link.

(defun unix-unlink (name)
  "Unix-unlink removes the directory entry for the named file.
   NIL and an error code is returned if the call fails."
  (declare (type unix-pathname name))
  (void-syscall ("unlink" c-string) name))

;;; Unix-write accepts a file descriptor, a buffer, an offset, and the
;;; length to write.  It attempts to write len bytes to the device
;;; associated with fd from the the buffer starting at offset.  It returns
;;; the actual number of bytes written.

(defun unix-write (fd buf offset len)
  "Unix-write attempts to write a character buffer (buf) of length
   len to the file described by the file descriptor fd.  NIL and an
   error is returned if the call is unsuccessful."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) offset len))
  (int-syscall ("write" int (* char) int)
	       fd
	       (with-alien ((ptr (* char) (etypecase buf
					    ((simple-array * (*))
					     (vector-sap buf))
					    (system-area-pointer
					     buf))))
		 (addr (deref ptr offset)))
	       len))

;;; Unix-ioctl is used to change parameters of devices in a device
;;; dependent way.


(defconstant terminal-speeds
  '#(nil 50 75 110 nil 150 200 300 600 1200 1800 2400 4800 9600 nil nil))

(defconstant tty-raw #o40)
(defconstant tty-crmod #o20)
(defconstant tty-echo #o10)
(defconstant tty-lcase #o4)
#-hpux
(defconstant tty-cbreak #o2)
#-hpux
(defconstant tty-tandem #o1)

#+(or hpux svr4)
(progn
  (defconstant tty-icanon #o2)
  (defconstant tty-icrnl #o400)
  (defconstant tty-ocrnl #o10)
  (defconstant tty-ixon #o2000)
  (defconstant veof 4)
  (defconstant vintr 0)
  (defconstant vquit 1))

#+hpux
(progn
  (defconstant vdsusp 21)
  (defconstant vstart 14)
  (defconstant vstop 15)
  (defconstant vsusp 13)
  (defconstant vmin 11)
  (defconstant vtime 12)
  (defconstant tcsaflush 2))

#+svr4
(progn
  (defconstant vdsusp 11)
  (defconstant vstart 8)
  (defconstant vstop 9)
  (defconstant vsusp 10)
  (defconstant vmin 4)
  (defconstant vtime 5)
  (defconstant tcsaflush #x5410))


(eval-when (compile load eval)

(defconstant iocparm-mask #x7f)
(defconstant ioc_void #x20000000)
(defconstant ioc_out #x40000000)
(defconstant ioc_in #x80000000)
(defconstant ioc_inout (logior ioc_in ioc_out))

#-svr4
(defmacro define-ioctl-command (name dev cmd arg &optional (parm-type :void))
  (let* ((ptype (ecase parm-type
		  (:void ioc_void)
		  (:in ioc_in)
		  (:out ioc_out)
		  (:inout ioc_inout)))
	 (code (logior (ash (char-code dev) 8) cmd ptype)))
    (when arg
      (setf code
	    `(logior (ash (logand (alien-size ,arg :bytes)
				  ,iocparm-mask)
			  16)
		     ,code)))
    `(eval-when (eval load compile)
       (defconstant ,name ,code))))

#+svr4
(defmacro define-ioctl-command (name dev cmd arg &optional (parm-type :void))
  (declare (ignore dev arg parm-type))
  `(eval-when (eval load compile)
     (defconstant ,name ,(logior (ash (char-code #\t) 8) cmd))))

)

;;; TTY ioctl commands.

(define-ioctl-command TIOCGETP #\t 8 (struct sgttyb) :out)
(define-ioctl-command TIOCSETP #\t 9 (struct sgttyb) :in)
(define-ioctl-command TIOCFLUSH #\t 16 int :in)
(define-ioctl-command TIOCSETC #\t 17 (struct tchars) :in)
(define-ioctl-command TIOCGETC #\t 18 (struct tchars) :out)
(define-ioctl-command TIOCGWINSZ #\t #-hpux 104 #+hpux 107 (struct winsize)
  :out)
(define-ioctl-command TIOCSWINSZ #\t #-hpux 103 #+hpux 106 (struct winsize)
  :in)

(define-ioctl-command TIOCNOTTY #\t 113 nil :void)
#-hpux
(progn
  (define-ioctl-command TIOCSLTC #\t 117 (struct ltchars) :in)
  (define-ioctl-command TIOCGLTC #\t 116 (struct ltchars) :out)
  (define-ioctl-command TIOCSPGRP #\t #-svr4 118 #+svr4 21 int :in)
  (define-ioctl-command TIOCGPGRP #\t #-svr4 119 #+svr4 20 int :out))
#+hpux
(progn
  (define-ioctl-command TIOCSLTC #\T 23 (struct ltchars) :in)
  (define-ioctl-command TIOCGLTC #\T 24 (struct ltchars) :out)
  (define-ioctl-command TIOCSPGRP #\T 29 int :in)
  (define-ioctl-command TIOCGPGRP #\T 30 int :out))

;;; File ioctl commands.
(define-ioctl-command FIONREAD #\f 127 int :out)


(defun unix-ioctl (fd cmd arg)
  "Unix-ioctl performs a variety of operations on open i/o
   descriptors.  See the UNIX Programmer's Manual for more
   information."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) cmd))
  (void-syscall ("ioctl" int unsigned-int (* char)) fd cmd arg))

#+(or svr4 hpux)
(defun unix-tcgetattr (fd termios)
  "Get terminal attributes."
  (declare (type unix-fd fd))
  (void-syscall ("tcgetattr" int (* (struct termios))) fd termios))

#+(or svr4 hpux)
(defun unix-tcsetattr (fd opt termios)
  "Set terminal attributes."
  (declare (type unix-fd fd))
  (void-syscall ("tcsetattr" int int (* (struct termios))) fd opt termios))

(defun tcsetpgrp (fd pgrp)
  "Set the tty-process-group for the unix file-descriptor FD to PGRP."
  (alien:with-alien ((alien-pgrp c-call:int pgrp))
    (unix-ioctl fd
		tiocspgrp
		(alien:alien-sap (alien:addr alien-pgrp)))))

(defun tcgetpgrp (fd)
  "Get the tty-process-group for the unix file-descriptor FD."
  (alien:with-alien ((alien-pgrp c-call:int))
    (multiple-value-bind (ok err)
	(unix-ioctl fd
		     tiocgpgrp
		     (alien:alien-sap (alien:addr alien-pgrp)))
      (if ok
	  (values alien-pgrp nil)
	  (values nil err)))))

(defun tty-process-group (&optional fd)
  "Get the tty-process-group for the unix file-descriptor FD.  If not supplied,
  FD defaults to /dev/tty."
  (if fd
      (tcgetpgrp fd)
      (multiple-value-bind (tty-fd errno)
	  (unix-open "/dev/tty" o_rdwr 0)
	(cond (tty-fd
	       (multiple-value-prog1
		   (tcgetpgrp tty-fd)
		 (unix-close tty-fd)))
	      (t
	       (values nil errno))))))

(defun %set-tty-process-group (pgrp &optional fd)
  "Set the tty-process-group for the unix file-descriptor FD to PGRP.  If not
  supplied, FD defaults to /dev/tty."
  (let ((old-sigs
	 (unix-sigblock
	  (sigmask :sigttou :sigttin :sigtstp :sigchld))))
    (declare (type (unsigned-byte 32) old-sigs))
    (unwind-protect
	(if fd
	    (tcsetpgrp fd pgrp)
	    (multiple-value-bind (tty-fd errno)
		(unix-open "/dev/tty" o_rdwr 0)
	      (cond (tty-fd
		     (multiple-value-prog1
			 (tcsetpgrp tty-fd pgrp)
		       (unix-close tty-fd)))
		    (t
		     (values nil errno)))))
      (unix-sigsetmask old-sigs))))
  
(defsetf tty-process-group (&optional fd) (pgrp)
  "Set the tty-process-group for the unix file-descriptor FD to PGRP.  If not
  supplied, FD defaults to /dev/tty."
  `(%set-tty-process-group ,pgrp ,fd))


;;; Socket options.

#+hpux
(define-ioctl-command SIOCSPGRP #\s 8 int :in)

#+hpux
(defun siocspgrp (fd pgrp)
  "Set the socket process-group for the unix file-descriptor FD to PGRP."
  (alien:with-alien ((alien-pgrp c-call:int pgrp))
    (unix-ioctl fd
		siocspgrp
		(alien:alien-sap (alien:addr alien-pgrp)))))

;;; Unix-exit terminates a program.

(defun unix-exit (&optional (code 0))
  "Unix-exit terminates the current process with an optional
   error code.  If successful, the call doesn't return.  If
   unsuccessful, the call returns NIL and an error number."
  (declare (type (signed-byte 32) code))
  (void-syscall ("exit" int) code))

;;; STAT and friends.

(defmacro extract-stat-results (buf)
  `(values T
	   (slot ,buf 'st-dev)
	   (slot ,buf 'st-ino)
	   (slot ,buf 'st-mode)
	   (slot ,buf 'st-nlink)
	   (slot ,buf 'st-uid)
	   (slot ,buf 'st-gid)
	   (slot ,buf 'st-rdev)
	   (slot ,buf 'st-size)
	   #-svr4 (slot ,buf 'st-atime)
	   #+svr4 (slot (slot ,buf 'st-atime) 'tv-sec)
	   #-svr4 (slot ,buf 'st-mtime)
	   #+svr4 (slot (slot ,buf 'st-mtime) 'tv-sec)
	   #-svr4 (slot ,buf 'st-ctime)
	   #+svr4 (slot (slot ,buf 'st-ctime) 'tv-sec)
	   (slot ,buf 'st-blksize)
	   (slot ,buf 'st-blocks)))

(defun unix-stat (name)
  "Unix-stat retrieves information about the specified
   file returning them in the form of multiple values.
   See the UNIX Programmer's Manual for a description
   of the values returned.  If the call fails, then NIL
   and an error number is returned instead."
  (declare (type unix-pathname name))
  (when (string= name "")
    (setf name "."))
  (with-alien ((buf (struct stat)))
    (syscall ("stat" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     name (addr buf))))


(defun unix-lstat (name)
  "Unix-lstat is similar to unix-stat except the specified
   file must be a symbolic link."
  (declare (type unix-pathname name))
  (with-alien ((buf (struct stat)))
    (syscall ("lstat" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     name (addr buf))))

(defun unix-fstat (fd)
  "Unix-fstat is similar to unix-stat except the file is specified
   by the file descriptor fd."
  (declare (type unix-fd fd))
  (with-alien ((buf (struct stat)))
    (syscall ("fstat" int (* (struct stat)))
	     (extract-stat-results buf)
	     fd (addr buf))))


(defconstant rusage_self 0 "The calling process.")
(defconstant rusage_children -1 "Terminated child processes.")

(declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
  "Like call getrusage, but return only the system and user time, and returns
   the seconds and microseconds as separate values."
  (declare (values (member t)
		   (unsigned-byte 31) (mod 1000000)
		   (unsigned-byte 31) (mod 1000000)))
  (with-alien ((usage (struct rusage)))
    (syscall* ("getrusage" int (* (struct rusage)))
	      (values t
		      (slot (slot usage 'ru-utime) 'tv-sec)
		      (slot (slot usage 'ru-utime) 'tv-usec)
		      (slot (slot usage 'ru-stime) 'tv-sec)
		      (slot (slot usage 'ru-stime) 'tv-usec))
	      who (addr usage))))

(defun unix-getrusage (who)
  "Unix-getrusage returns information about the resource usage
   of the process specified by who.  Who can be either the
   current process (rusage_self) or all of the terminated
   child processes (rusage_children).  NIL and an error number
   is returned if the call fails."
  (with-alien ((usage (struct rusage)))
    (syscall* ("getrusage" int (* (struct rusage)))
	      (values t
		      (+ (* (slot (slot usage 'ru-utime) 'tv-sec) 1000000)
			 (slot (slot usage 'ru-utime) 'tv-usec))
		      (+ (* (slot (slot usage 'ru-stime) 'tv-sec) 1000000)
			 (slot (slot usage 'ru-stime) 'tv-usec))
		      (slot usage 'ru-maxrss)
		      (slot usage 'ru-ixrss)
		      (slot usage 'ru-idrss)
		      (slot usage 'ru-isrss)
		      (slot usage 'ru-minflt)
		      (slot usage 'ru-majflt)
		      (slot usage 'ru-nswap)
		      (slot usage 'ru-inblock)
		      (slot usage 'ru-oublock)
		      (slot usage 'ru-msgsnd)
		      (slot usage 'ru-msgrcv)
		      (slot usage 'ru-nsignals)
		      (slot usage 'ru-nvcsw)
		      (slot usage 'ru-nivcsw))
	      who (addr usage))))

;; Requires call to tzset() in main.
;; Don't use this now: we 
#+svr4
(progn
    (def-alien-variable ("daylight" unix-daylight) int)
    (def-alien-variable ("timezone" unix-timezone) time-t)
    (def-alien-variable ("altzone" unix-altzone) time-t)
    (def-alien-variable ("tzname" unix-tzname) (array c-string 2))
    (def-alien-routine get-timezone c-call:void
		       (when c-call:long :in)
		       (minutes-west c-call:int :out)
		       (daylight-savings-p alien:boolean :out))
    (defun unix-get-minutes-west (secs)
	   (multiple-value-bind (ignore minutes dst) (get-timezone secs)
				(declare (ignore ignore) (ignore dst))
				(values minutes))
	    )
    (defun unix-get-timezone (secs)
	   (multiple-value-bind (ignore minutes dst) (get-timezone secs)
				(declare (ignore ignore) (ignore minutes))
				(values (deref unix-tzname (if dst 1 0)))
	    ) )
)
(declaim (inline unix-gettimeofday))
(defun unix-gettimeofday ()
  "If it works, unix-gettimeofday returns 5 values: T, the seconds and
   microseconds of the current time of day, the timezone (in minutes west
   of Greenwich), and a daylight-savings flag.  If it doesn't work, it
   returns NIL and the errno."
  (with-alien ((tv (struct timeval))
	       #-svr4 (tz (struct timezone)))
    (syscall* ("gettimeofday" (* (struct timeval)) #-svr4 (* (struct timezone)))
	      (values T
		      (slot tv 'tv-sec)
		      (slot tv 'tv-usec)
		      #-svr4 (slot tz 'tz-minuteswest)
		      #+svr4 (unix-get-minutes-west (slot tv 'tv-sec))
		      #-svr4 (slot tz 'tz-dsttime)
		      #+svr4 (unix-get-timezone (slot tv 'tv-sec))
		      )
	      (addr tv)
	      #-svr4 (addr tz))))

;;; Unix-utimes changes the accessed and updated times on UNIX
;;; files.  The first argument is the filename (a string) and
;;; the second argument is a list of the 4 times- accessed and
;;; updated seconds and microseconds.

#-hpux
(defun unix-utimes (file atime-sec atime-usec mtime-sec mtime-usec)
  "Unix-utimes sets the 'last-accessed' and 'last-updated'
   times on a specified file.  NIL and an error number is
   returned if the call is unsuccessful."
  (declare (type unix-pathname file)
	   (type (alien unsigned-long)
		 atime-sec atime-usec
		 mtime-sec mtime-usec))
  (with-alien ((tvp (array (struct timeval) 2)))
    (setf (slot (deref tvp 0) 'tv-sec) atime-sec)
    (setf (slot (deref tvp 0) 'tv-usec) atime-usec)
    (setf (slot (deref tvp 1) 'tv-sec) mtime-sec)
    (setf (slot (deref tvp 1) 'tv-usec) mtime-usec)
    (void-syscall ("utimes" c-string (* (struct timeval)))
		  file
		  (cast tvp (* (struct timeval))))))

;;; Unix-setreuid sets the real and effective user-id's of the current
;;; process to the arguments "ruid" and "euid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "ruid" or
;;; "euid" to -1 makes the system use the current id instead.

#-(or svr4 hpux)
(defun unix-setreuid (ruid euid)
  "Unix-setreuid sets the real and effective user-id's of the current
   process to the specified ones.  NIL and an error number is returned
   if the call fails."
  (void-syscall ("setreuid" int int) ruid euid))

;;; Unix-setregid sets the real and effective group-id's of the current
;;; process to the arguments "rgid" and "egid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "rgid" or
;;; "egid" to -1 makes the system use the current id instead.

#-(or svr4 hpux)
(defun unix-setregid (rgid egid)
  "Unix-setregid sets the real and effective group-id's of the current
   process process to the specified ones.  NIL and an error number is
   returned if the call fails."
  (void-syscall ("setregid" int int) rgid egid))

(def-alien-routine ("getpid" unix-getpid) int
  "Unix-getpid returns the process-id of the current process.")

(def-alien-routine ("getppid" unix-getppid) int
  "Unix-getppid returns the process-id of the parent of the current process.")

(def-alien-routine ("getgid" unix-getgid) int
  "Unix-getgid returns the real group-id of the current process.")

(def-alien-routine ("getegid" unix-getegid) int
  "Unix-getegid returns the effective group-id of the current process.")

;;; Unix-getpgrp returns the group-id associated with the
;;; process whose process-id is specified as an argument.
;;; As usual, if the process-id is 0, it refers to the current
;;; process.

(defun unix-getpgrp (pid)
  "Unix-getpgrp returns the group-id of the process associated
   with pid."
  (int-syscall ("getpgrp" int) pid))

;;; Unix-setpgrp sets the group-id of the process specified by 
;;; "pid" to the value of "pgrp".  The process must either have
;;; the same effective user-id or be a super-user process.

(defun unix-setpgrp (pid pgrp)
  "Unix-setpgrp sets the process group on the process pid to
   pgrp.  NIL and an error number is returned upon failure."
  (void-syscall (#-svr4 "setpgrp" #+svr4 "setpgid" int int) pid pgrp))

(def-alien-routine ("getuid" unix-getuid) int
  "Unix-getuid returns the real user-id associated with the
   current process.")

;;; Unix-getpagesize returns the number of bytes in the system page.

(defun unix-getpagesize ()
  "Unix-getpagesize returns the number of bytes in a system page."
  (int-syscall ("getpagesize")))

(defun unix-gethostname ()
  "Unix-gethostname returns the name of the host machine as a string."
  (with-alien ((buf (array char 256)))
    (syscall ("gethostname" (* char) int)
	     (cast buf c-string)
	     (cast buf (* char)) 256)))

(def-alien-routine ("gethostid" unix-gethostid) unsigned-long
  "Unix-gethostid returns a 32-bit integer which provides unique
   identification for the host machine.")

(defun unix-fork ()
  "Executes the unix fork system call.  Returns 0 in the child and the pid
   of the child in the parent if it works, or NIL and an error number if it
   doesn't work."
  (int-syscall ("fork")))



;;; Operations on Unix Directories.

(export '(open-dir read-dir close-dir))

(defstruct (directory
	    (:print-function %print-directory))
  name
  (dir-struct (required-argument) :type system-area-pointer))

(defun %print-directory (dir stream depth)
  (declare (ignore depth))
  (format stream "#<Directory ~S>" (directory-name dir)))

(defun open-dir (pathname)
  (declare (type unix-pathname pathname))
  (when (string= pathname "")
    (setf pathname "."))
  (let ((kind (unix-file-kind pathname)))
    (case kind
      (:directory
       (let ((dir-struct
	      (alien-funcall (extern-alien "opendir"
					   (function system-area-pointer
						     c-string))
			     pathname)))
	 (if (zerop (sap-int dir-struct))
	     (values nil unix-errno)
	     (make-directory :name pathname :dir-struct dir-struct))))
      ((nil)
       (values nil enoent))
      (t
       (values nil enotdir)))))

(defun read-dir (dir)
  (declare (type directory dir))
  (let ((daddr (alien-funcall (extern-alien "readdir"
					    (function system-area-pointer
						      system-area-pointer))
			      (directory-dir-struct dir))))
    (declare (type system-area-pointer daddr))
    (if (zerop (sap-int daddr))
	nil
	(with-alien ((direct (* (struct direct)) daddr))
	  #-svr4
	  (let ((nlen (slot direct 'd-namlen))
		(ino (slot direct 'd-ino)))
	    (declare (type (unsigned-byte 16) nlen))
	    (let ((string (make-string nlen)))
	      (kernel:copy-from-system-area
	       (alien-sap (addr (slot direct 'd-name))) 0
	       string (* vm:vector-data-offset vm:word-bits)
	       (* nlen vm:byte-bits))
	      (values string ino)))
	  #+svr4
	  (values (cast (slot direct 'd-name) c-string)
		  (slot direct 'd-ino))))))

(defun close-dir (dir)
  (declare (type directory dir))
  (alien-funcall (extern-alien "closedir"
			       (function void system-area-pointer))
		 (directory-dir-struct dir))
  nil)


(defun unix-current-directory ()
  (with-alien ((buf (array char 1024)))
    (values (not (zerop (alien-funcall (extern-alien "getwd"
						     (function int (* char)))
				       (cast buf (* char)))))
	    (cast buf c-string))))



;;;; Support routines for dealing with unix pathnames.

(export '(unix-file-kind unix-maybe-prepend-current-directory
	  unix-resolve-links unix-simplify-pathname))

(defun unix-file-kind (name &optional check-for-links)
  "Returns either :file, :directory, :link, :special, or NIL."
  (declare (simple-string name))
  (multiple-value-bind (res dev ino mode)
		       (if check-for-links
			   (unix-lstat name)
			   (unix-stat name))
    (declare (type (or fixnum null) mode)
	     (ignore dev ino))
    (when res
      (let ((kind (logand mode s-ifmt)))
	(cond ((eql kind s-ifdir) :directory)
	      ((eql kind s-ifreg) :file)
	      ((eql kind s-iflnk) :link)
	      (t :special))))))

(defun unix-maybe-prepend-current-directory (name)
  (declare (simple-string name))
  (if (and (> (length name) 0) (char= (schar name 0) #\/))
      name
      (multiple-value-bind (win dir) (unix-current-directory)
	(if win
	    (concatenate 'simple-string dir "/" name)
	    name))))

(defun unix-resolve-links (pathname)
  "Returns the pathname with all symbolic links resolved."
  (declare (simple-string pathname))
  (let ((len (length pathname))
	(pending pathname))
    (declare (fixnum len) (simple-string pending))
    (if (zerop len)
	pathname
	(let ((result (make-string 1024 :initial-element (code-char 0)))
	      (fill-ptr 0)
	      (name-start 0))
	  (loop
	    (let* ((name-end (or (position #\/ pending :start name-start) len))
		   (new-fill-ptr (+ fill-ptr (- name-end name-start))))
	      (replace result pending
		       :start1 fill-ptr
		       :end1 new-fill-ptr
		       :start2 name-start
		       :end2 name-end)
	      (let ((kind (unix-file-kind (if (zerop name-end) "/" result) t)))
		(unless kind (return nil))
		(cond ((eq kind :link)
		       (multiple-value-bind (link err) (unix-readlink result)
			 (unless link
			   (error "Error reading link ~S: ~S"
				  (subseq result 0 fill-ptr)
				  (get-unix-error-msg err)))
			 (cond ((or (zerop (length link))
				    (char/= (schar link 0) #\/))
				;; It's a relative link
				(fill result (code-char 0)
				      :start fill-ptr
				      :end new-fill-ptr))
			       ((string= result "/../" :end1 4)
				;; It's across the super-root.
				(let ((slash (or (position #\/ result :start 4)
						 0)))
				  (fill result (code-char 0)
					:start slash
					:end new-fill-ptr)
				  (setf fill-ptr slash)))
			       (t
				;; It's absolute.
				(and (> (length link) 0)
				     (char= (schar link 0) #\/))
				(fill result (code-char 0) :end new-fill-ptr)
				(setf fill-ptr 0)))
			 (setf pending
			       (if (= name-end len)
				   link
				   (concatenate 'simple-string
						link
						(subseq pending name-end))))
			 (setf len (length pending))
			 (setf name-start 0)))
		      ((= name-end len)
		       (return (subseq result 0 new-fill-ptr)))
		      ((eq kind :directory)
		       (setf (schar result new-fill-ptr) #\/)
		       (setf fill-ptr (1+ new-fill-ptr))
		       (setf name-start (1+ name-end)))
		      (t
		       (return nil))))))))))

(defun unix-simplify-pathname (src)
  (declare (simple-string src))
  (let* ((src-len (length src))
	 (dst (make-string src-len))
	 (dst-len 0)
	 (dots 0)
	 (last-slash nil))
    (macrolet ((deposit (char)
			`(progn
			   (setf (schar dst dst-len) ,char)
			   (incf dst-len))))
      (dotimes (src-index src-len)
	(let ((char (schar src src-index)))
	  (cond ((char= char #\.)
		 (when dots
		   (incf dots))
		 (deposit char))
		((char= char #\/)
		 (case dots
		   (0
		    ;; Either ``/...' or ``...//...'
		    (unless last-slash
		      (setf last-slash dst-len)
		      (deposit char)))
		   (1
		    ;; Either ``./...'' or ``..././...''
		    (decf dst-len))
		   (2
		    ;; We've found ..
		    (cond
		     ((and last-slash (not (zerop last-slash)))
		      ;; There is something before this ..
		      (let ((prev-prev-slash
			     (position #\/ dst :end last-slash :from-end t)))
			(cond ((and (= (+ (or prev-prev-slash 0) 2)
				       last-slash)
				    (char= (schar dst (- last-slash 2)) #\.)
				    (char= (schar dst (1- last-slash)) #\.))
			       ;; The something before this .. is another ..
			       (deposit char)
			       (setf last-slash dst-len))
			      (t
			       ;; The something is some random dir.
			       (setf dst-len
				     (if prev-prev-slash
					 (1+ prev-prev-slash)
					 0))
			       (setf last-slash prev-prev-slash)))))
		     (t
		      ;; There is nothing before this .., so we need to keep it
		      (setf last-slash dst-len)
		      (deposit char))))
		   (t
		    ;; Something other than a dot between slashes.
		    (setf last-slash dst-len)
		    (deposit char)))
		 (setf dots 0))
		(t
		 (setf dots nil)
		 (setf (schar dst dst-len) char)
		 (incf dst-len))))))
    (when (and last-slash (not (zerop last-slash)))
      (case dots
	(1
	 ;; We've got  ``foobar/.''
	 (decf dst-len))
	(2
	 ;; We've got ``foobar/..''
	 (unless (and (>= last-slash 2)
		      (char= (schar dst (1- last-slash)) #\.)
		      (char= (schar dst (- last-slash 2)) #\.)
		      (or (= last-slash 2)
			  (char= (schar dst (- last-slash 3)) #\/)))
	   (let ((prev-prev-slash
		  (position #\/ dst :end last-slash :from-end t)))
	     (if prev-prev-slash
		 (setf dst-len (1+ prev-prev-slash))
		 (return-from unix-simplify-pathname "./")))))))
    (cond ((zerop dst-len)
	   "./")
	  ((= dst-len src-len)
	   dst)
	  (t
	   (subseq dst 0 dst-len)))))


;;;; Other random routines.

(def-alien-routine ("isatty" unix-isatty) boolean
  "Accepts a Unix file descriptor and returns T if the device
  associated with it is a terminal."
  (fd int))

(def-alien-routine ("ttyname" unix-ttyname) c-string
  (fd int))




;;;; UNIX-EXECVE

(defun unix-execve (program &optional arg-list
			    (environment *environment-list*))
  "Executes the Unix execve system call.  If the system call suceeds, lisp
   will no longer be running in this process.  If the system call fails this
   function returns two values: NIL and an error code.  Arg-list should be a
   list of simple-strings which are passed as arguments to the exec'ed program.
   Environment should be an a-list mapping symbols to simple-strings which this
   function bashes together to form the environment for the exec'ed program."
  (check-type program simple-string)
  (let ((env-list (let ((envlist nil))
		    (dolist (cons environment)
		      (push (if (cdr cons)
				(concatenate 'simple-string
					     (string (car cons)) "="
					     (cdr cons))
				(car cons))
			    envlist))
		    envlist)))
    (sub-unix-execve program arg-list env-list)))


(defmacro round-bytes-to-words (n)
  `(logand (the fixnum (+ (the fixnum ,n) 3)) (lognot 3)))

;;;
;;; STRING-LIST-TO-C-STRVEC	-- Internal
;;; 
;;; STRING-LIST-TO-C-STRVEC is a function which takes a list of
;;; simple-strings and constructs a C-style string vector (strvec) --
;;; a null-terminated array of pointers to null-terminated strings.
;;; This function returns two values: a sap and a byte count.  When the
;;; memory is no longer needed it should be deallocated with
;;; vm_deallocate.
;;; 
(defun string-list-to-c-strvec (string-list)
  ;;
  ;; Make a pass over string-list to calculate the amount of memory
  ;; needed to hold the strvec.
  (let ((string-bytes 0)
	(vec-bytes (* 4 (1+ (length string-list)))))
    (declare (fixnum string-bytes vec-bytes))
    (dolist (s string-list)
      (check-type s simple-string)
      (incf string-bytes (round-bytes-to-words (1+ (length s)))))
    ;;
    ;; Now allocate the memory and fill it in.
    (let* ((total-bytes (+ string-bytes vec-bytes))
	   (vec-sap (system:allocate-system-memory total-bytes))
	   (string-sap (sap+ vec-sap vec-bytes))
	   (i 0))
      (declare (type (and unsigned-byte fixnum) total-bytes i)
	       (type system:system-area-pointer vec-sap string-sap))
      (dolist (s string-list)
	(declare (simple-string s))
	(let ((n (length s)))
	  ;; 
	  ;; Blast the string into place
	  (kernel:copy-to-system-area (the simple-string s)
				      (* vm:vector-data-offset vm:word-bits)
				      string-sap 0
				      (* (1+ n) vm:byte-bits))
	  ;; 
	  ;; Blast the pointer to the string into place
	  (setf (sap-ref-sap vec-sap i) string-sap)
	  (setf string-sap (sap+ string-sap (round-bytes-to-words (1+ n))))
	  (incf i 4)))
      ;; Blast in last null pointer
      (setf (sap-ref-sap vec-sap i) (int-sap 0))
      (values vec-sap total-bytes))))

(defun sub-unix-execve (program arg-list env-list)
  (let ((argv nil)
	(argv-bytes 0)
	(envp nil)
	(envp-bytes 0)
	result error-code)
    (unwind-protect
	(progn
	  ;; Blast the stuff into the proper format
	  (multiple-value-setq
	      (argv argv-bytes)
	    (string-list-to-c-strvec arg-list))
	  (multiple-value-setq
	      (envp envp-bytes)
	    (string-list-to-c-strvec env-list))
	  ;;
	  ;; Now do the system call
	  (multiple-value-setq
	      (result error-code)
	    (int-syscall ("execve"
			  (* char) system-area-pointer system-area-pointer)
			 (vector-sap program) argv envp)))
      ;; 
      ;; Deallocate memory
      (when argv
	(system:deallocate-system-memory argv argv-bytes))
      (when envp
	(system:deallocate-system-memory envp envp-bytes)))
    (values result error-code)))



;;;; Socket support.

(def-alien-routine ("socket" unix-socket) int
  (domain int)
  (type int)
  (protocol int))

(def-alien-routine ("connect" unix-connect) int
  (socket int)
  (sockaddr (* t))
  (len int))

(def-alien-routine ("bind" unix-bind) int
  (socket int)
  (sockaddr (* t))
  (len int))

(def-alien-routine ("listen" unix-listen) int
  (socket int)
  (backlog int))

(def-alien-routine ("accept" unix-accept) int
  (socket int)
  (sockaddr (* t))
  (len int :in-out))

(def-alien-routine ("recv" unix-recv) int
  (fd int)
  (buffer c-string)
  (length int)
  (flags int))

(def-alien-routine ("send" unix-send) int
  (fd int)
  (buffer c-string)
  (length int)
  (flags int))
