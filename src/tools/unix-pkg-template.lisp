;;; -*- Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/exports-unix.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Defines the UNIX package and all the exported symbols.
;;;
;;; This file is auto-generated via bin/create-errno.sh.
;;;
;;; DO NOT EDIT!
;;;

(in-package "LISP")

(intl:textdomain "cmucl")

(if (find-package "UNIX")
    (rename-package "UNIX" "UNIX" 'nil)
    (make-package "UNIX" :nicknames 'nil :use nil))

(use-package '("LISP") "UNIX")

(defpackage "UNIX"
  (:export "UNIX-CURRENT-DIRECTORY"
	   "UNIX-OPEN"
	   "UNIX-READ"
	   "UNIX-WRITE"
	   "UNIX-GETPAGESIZE"
	   "UNIX-ERRNO"
	   "UNIX-MAYBE-PREPEND-CURRENT-DIRECTORY"
	   "UNIX-REALPATH"
	   "UNIX-CLOSE"
	   "UNIX-STAT"
	   "UNIX-LSTAT"
	   "UNIX-FSTAT"
	   "UNIX-GETHOSTNAME"
	   "UNIX-LSEEK"
	   "UNIX-EXIT"
	   "UNIX-CHDIR"
	   "UNIX-ACCESS"
	   "UNIX-DUP"
	   "UNIX-CHMOD"
	   "UNIX-READLINK"
	   "UNIX-RENAME"
	   "UNIX-SELECT"
	   "UNIX-FAST-GETRUSAGE"
	   "UNIX-GETRUSAGE"
	   "UNIX-GETTIMEOFDAY"
	   "UNIX-ISATTY"
	   "UNIX-MKDIR"
	   "UNIX-RMDIR"
	   "UNIX-UNLINK"
	   "TIMEZONE"
	   "TIMEVAL"
	   "SIZE-T"
	   "OFF-T"
	   "INO-T"
	   "DEV-T"
	   "TIME-T"
	   "USER-INFO-NAME"
	   "INT64-T"
	   "MODE-T"
	   "UNIX-FAST-SELECT"
	   "UNIX-PIPE"
	   "UNIX-GETPID"
	   "UNIX-GETHOSTID"
	   "UNIX-UID"
	   "UNIX-GID"
	   "GET-UNIX-ERROR-MSG"
	   "WINSIZE"
	   "TIMEVAL"
	   "CLOSE-DIR"
	   "OPEN-DIR"
	   "READ-DIR"

	   ;; filesys.lisp
	   "UNIX-GETPWUID"

	   ;; multi-proc.lisp
	   "UNIX-SETITIMER"

	   ;; run-program.lisp
	   "UNIX-TTYNAME"
	   "UNIX-IOCTL"
	   "UNIX-OPENPTY"

	   ;; alien-callback.lisp
	   "UNIX-MPROTECT"

	   ;; internet.lisp
	   "UNIX-SOCKET"
	   "UNIX-CONNECT"
	   "UNIX-BIND"
	   "UNIX-LISTEN"
	   "UNIX-ACCEPT"
	   "UNIX-GETSOCKOPT"
	   "UNIX-SETSOCKOPT"
	   "UNIX-GETPEERNAME"
	   "UNIX-GETSOCKNAME"
	   "UNIX-RECV"
	   "UNIX-SEND"
	   "UNIX-RECVFROM"
	   "UNIX-SENDTO"
	   "UNIX-SHUTDOWN"
	   "UNIX-FCNTL"

	   ;; serve-event.lisp
	   "FD-SETSIZE"
	   "FD-ISSET"
	   "FD-CLR"

	   ;; Simple streams
	   "PROT_READ"
	   "UNIX-MMAP"
	   "UNIX-MUNMAP"
	   "UNIX-MSYNC"

	   ;; Motif
	   "UNIX-GETUID"

	   ;; Hemlock
	   "UNIX-CFGETOSPEED"
	   "TERMIOS"
	   "UNIX-TCGETATTR"
	   "UNIX-TCSETATTR"
	   "UNIX-FCHMOD"
	   "UNIX-CREAT"
	   "UNIX-UTIMES"

	   ;; Tests
	   "UNIX-SYMLINK"

	   ;; Other symbols from structures, etc.
	   "C-CC" "C-CFLAG" "C-IFLAG" "C-ISPEED" "C-LFLAG" "C-OFLAG" "C-OSPEED"
	   "CHECK" "D-NAME" "D-RECLEN"    
	   "F-GETFL" "F-GETOWN" "F-SETFL" "F-SETOWN" "FAPPEND"
	   "FASYNC" "FD-SET" "FD-ZERO" "FNDELAY" "F_OK" "GID-T" "IT-INTERVAL"
	   "IT-VALUE" "ITIMERVAL" "L_INCR" "L_SET" "L_XTND" "MAP_ANONYMOUS"
	   "MAP_FIXED" "MAP_PRIVATE" "MAP_SHARED" "MS_ASYNC" "MS_INVALIDATE"
	   "MS_SYNC" "O_APPEND" "O_CREAT" "O_EXCL" "O_NDELAY" "O_NONBLOCK"
	   "O_RDONLY" "O_RDWR" "O_TRUNC" "O_WRONLY" "PROT_EXEC" "PROT_NONE"
	   "PROT_WRITE" "RU-IDRSS" "RU-INBLOCK" "RU-ISRSS" "RU-IXRSS" "RU-MAJFLT"
	   "RU-MAXRSS" "RU-MINFLT" "RU-MSGRCV" "RU-MSGSND" "RU-NIVCSW" "RU-NSIGNALS"
	   "RU-NSWAP" "RU-NVCSW" "RU-OUBLOCK" "RU-STIME" "RU-UTIME"
	   "RUSAGE_CHILDREN" "RUSAGE_SELF" "R_OK" "S-IFBLK" "S-IFCHR" "S-IFDIR"
	   "S-IFLNK" "S-IFMT" "S-IFREG" "S-IFSOCK" "SIGABRT" "SIGALRM" "SIGBUS"
	   "SIGCHLD" "SIGCONT" "SIGCONTEXT" "SIGFPE" "SIGHUP" "SIGILL" "SIGINT"
	   "SIGIO" "SIGIOT" "SIGKILL" "SIGMASK" "SIGPIPE" "SIGPROF" "SIGQUIT"
	   "SIGSEGV" "SIGSTOP" "SIGTERM" "SIGTRAP" "SIGTSTP" "SIGTTIN" "SIGTTOU"
	   "SIGURG" "SIGUSR1" "SIGUSR2" "SIGVTALRM" "SIGWINCH" "SIGXCPU" "SIGXFSZ"
	   "ST-ATIME" "ST-BLKSIZE" "ST-BLOCKS" "ST-CTIME" "ST-DEV" "ST-GID"
	   "ST-MODE" "ST-MTIME" "ST-NLINK" "ST-RDEV" "ST-SIZE" "ST-UID" "STAT"
	   "TCSADRAIN" "TCSAFLUSH" "TCSANOW" "TIOCGPGRP" "TIOCGWINSZ" "TIOCNOTTY"
	   "TIOCSPGRP" "TIOCSWINSZ" "TTY-BRKINT" "TTY-ECHO" "TTY-ECHOCTL"
	   "TTY-ECHOE" "TTY-ECHOK" "TTY-ECHOKE" "TTY-ECHONL" "TTY-ECHOPRT"
	   "TTY-FLUSHO" "TTY-ICANON" "TTY-ICRNL" "TTY-IEXTEN" "TTY-IGNBRK"
	   "TTY-IGNCR" "TTY-IGNPAR" "TTY-IMAXBEL" "TTY-INLCR" "TTY-INPCK" "TTY-ISIG"
	   "TTY-ISTRIP" "TTY-IXANY" "TTY-IXOFF" "TTY-IXON" "TTY-NOFLSH" "TTY-ONLCR"
	   "TTY-OPOST" "TTY-PARMRK" "TTY-PENDIN" "TTY-TOSTOP" "TV-SEC" "TV-USEC"
	   "TZ-DSTTIME" "TZ-MINUTESWEST" "UID-T" "UNIX-FD" "UNIX-FILE-KIND"
	   "UNIX-FILE-MODE" "UNIX-GETUID" "UNIX-KILL" "UNIX-KILLPG" "UNIX-PATHNAME"
	   "UNIX-SIGBLOCK" "UNIX-SIGNAL-DESCRIPTION" "UNIX-SIGNAL-NAME"
	   "UNIX-SIGNAL-NUMBER" "UNIX-SIGPAUSE" "UNIX-SIGSETMASK" "USER-INFO"
	   "USER-INFO-DIR" "USER-INFO-GECOS" "USER-INFO-GID" "USER-INFO-PASSWORD"
	   "USER-INFO-SHELL" "USER-INFO-UID" "VDSUSP" "VEOF" "VEOL" "VEOL2" "VERASE"
	   "VINTR" "VKILL" "VMIN" "VQUIT" "VSTART" "VSTOP" "VSUSP" "VTIME"
	   "WRITEOWN" "WS-COL" "WS-ROW" "WS-XPIXEL" "WS-YPIXEL" "W_OK" "X_OK"
	   "FIONREAD"
	   "TERMINAL-SPEEDS"
	   )
  (:export
   ;; For asdf
   "UNIX-GETENV"
   "UNIX-SETENV"
   "UNIX-PUTENV"
   "UNIX-UNSETENV"
   ;; For slime
   "UNIX-EXECVE"
   "UNIX-FORK")
  #-(or linux solaris)
  (:export "TCHARS"
	   "LTCHARS"
	   "D-NAMLEN"
	   
	   ;; run-program.lisp
	   "SGTTYB"

	   ;; Other symbols from structures, etc.
	   "DIRECT" "ELOCAL" "EPROCLIM" "EVICEERR" "EVICEOP" "EXECGRP" "EXECOTH"
	   "EXECOWN" "F-DUPFD" "F-GETFD" "F-SETFD" "FCREAT" "FEXCL"
	   "FTRUNC" "READGRP" "READOTH" "READOWN" "S-IEXEC" "S-IREAD" "S-ISGID"
	   "S-ISUID" "S-ISVTX" "S-IWRITE" "SAVETEXT" "SETGIDEXEC" "SETUIDEXEC"
	   "SG-ERASE" "SG-FLAGS" "SG-ISPEED" "SG-KILL" "SG-OSPEED" "SIGEMT" "SIGSYS"
	   "T-BRKC" "T-DSUSPC" "T-EOFC" "T-FLUSHC" "T-INTRC" "T-LNEXTC" "T-QUITC"
	   "T-RPRNTC" "T-STARTC" "T-STOPC" "T-SUSPC" "T-WERASC" "TCIFLUSH"
	   "TCIOFLUSH" "TCOFLUSH" "TIOCFLUSH" "TIOCGETC"
	   "TIOCGETP" "TIOCGLTC" "TIOCSETC" "TIOCSETP" "TIOCSLTC" "TTY-CBREAK"
	   "TTY-CLOCAL" "TTY-CREAD" "TTY-CRMOD" "TTY-CS5" "TTY-CS6" "TTY-CS7"
	   "TTY-CS8" "TTY-CSIZE" "TTY-CSTOPB" "TTY-HUPCL" "TTY-LCASE" "TTY-PARENB"
	   "TTY-PARODD" "TTY-RAW" "TTY-TANDEM" "WRITEGRP" "WRITEOTH"
	   )
  #+linux
  (:export "TCHARS"
	   "LTCHARS"
	   "D-NAMLEN"

	   ;; run-program.lisp
	   "SGTTYB"

	   ;; Other symbols
	   "BLKCNT-T" "D-INO" "D-OFF"     
	   "O_NOCTTY" "SIGSTKFLT"
	   "SG-FLAGS"
	   "TIOCGETP"
	   "TIOCSETP"
	   "TTY-IUCLC"
	   "TTY-OCRNL" "TTY-OFDEL" "TTY-OFILL" "TTY-OLCUC" "TTY-ONLRET" "TTY-ONOCR"
	   "TTY-XCASE" "UNIX-DUP2" "UNIX-GETITIMER" "UNIX-PID"
	   "UTSNAME"
	   )
  #+solaris
  (:export "D-INO"
	   "D-OFF"
	   "DIRECT"
	   "EXECGRP"
	   "EXECOTH"
	   "EXECOWN"
	   
	   "F-DUPFD"
	   "F-GETFD"
	   "F-SETFD"
	   "FCREAT"
	   "FEXCL"
	   "FTRUNC"
	   "LTCHARS"
	   "O_NOCTTY"
	   "RCV1EN"
	   "READGRP"
	   "READOTH"
	   "READOWN"
	   "S-IEXEC"
	   "S-IREAD"
	   "S-ISGID"
	   "S-ISUID"
	   "S-ISVTX"
	   "S-IWRITE"
	   "SAVETEXT"
	   "SETGIDEXEC"
	   "SETUIDEXEC"
	   "SG-ERASE"
	   "SG-FLAGS"
	   "SG-ISPEED"
	   "SG-KILL"
	   "SG-OSPEED"
	   "SGTTYB"
	   "SIGEMT"
	   "SIGSYS"
	   "SIGWAITING"
	   "T-BRKC"
	   "T-DSUSPC"
	   "T-EOFC"
	   "T-FLUSHC"
	   "T-INTRC"
	   "T-LNEXTC"
	   "T-QUITC"
	   "T-RPRNTC"
	   "T-STARTC"
	   "T-STOPC"
	   "T-SUSPC"
	   "T-WERASC"
	   "TCHARS"
	   "TCIFLUSH"
	   "TCIOFLUSH"
	   "TCOFLUSH"
	   "TIOCFLUSH"
	   "TIOCGETC"
	   "TIOCGETP"
	   "TIOCGLTC"
	   "TIOCSETC"
	   "TIOCSETP"
	   "TIOCSLTC"
	   "TTY-CBAUD"
	   "TTY-CBREAK"
	   "TTY-CLOCAL"
	   "TTY-CREAD"
	   "TTY-CRMOD"
	   "TTY-CS5"
	   "TTY-CS6"
	   "TTY-CS7"
	   "TTY-CS8"
	   "TTY-CSIZE"
	   "TTY-CSTOPB"
	   "TTY-DEFECHO"
	   "TTY-HUPCL"
	   "TTY-IUCLC"
	   "TTY-LCASE"
	   "TTY-LOBLK"
	   "TTY-OCRNL"
	   "TTY-OFDEL"
	   "TTY-OFILL"
	   "TTY-OLCUC"
	   "TTY-ONLRET"
	   "TTY-ONOCR"
	   "TTY-PARENB"
	   "TTY-PARODD"
	   "TTY-RAW"
	   "TTY-TANDEM"
	   "TTY-XCASE"
	   "UNIX-TIMES"
	   "UNIX-DUP2"
	   "UTSNAME"
	   "WRITEGRP"
	   "WRITEOTH"
	   "XMT1EN"
	   )
  ;; ERRNO symbols, auto-generated.
  (:export "ESUCCESS"
