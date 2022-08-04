;;; -*- Package: UNIX -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; This contains extra functionality for the UNIX package that is not
;;; needed by CMUCL core.
(ext:file-comment
  "$Header: src/contrib/unix/unix.lisp $")
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
(intl:textdomain "cmucl-unix")

(export '(daddr-t caddr-t ino-t swblk-t size-t time-t dev-t off-t uid-t gid-t
	  timeval tv-sec tv-usec timezone tz-minuteswest tz-dsttime
	  itimerval it-interval it-value tchars t-intrc t-quitc t-startc
	  t-stopc t-eofc t-brkc ltchars t-suspc t-dsuspc t-rprntc t-flushc
	  t-werasc t-lnextc sgttyb sg-ispeed sg-ospeed sg-erase sg-kill
	  sg-flags winsize ws-row ws-col ws-xpixel ws-ypixel
	  direct d-off d-ino d-reclen #-(or linux svr4) d-namlen d-name
	  stat st-dev st-mode st-nlink st-uid st-gid st-rdev st-size
	  st-atime st-mtime st-ctime st-blksize st-blocks
	  s-ifmt s-ifdir s-ifchr s-ifblk s-ifreg s-iflnk s-ifsock
	  s-isuid s-isgid s-isvtx s-iread s-iwrite s-iexec
	  ruseage ru-utime ru-stime ru-maxrss ru-ixrss ru-idrss
	  ru-isrss ru-minflt ru-majflt ru-nswap ru-inblock ru-oublock
	  ru-msgsnd ru-msgrcv ru-nsignals ru-nvcsw ru-nivcsw
	  rlimit rlim-cur rlim-max sc-onstack sc-mask sc-pc

	  unix-errno get-unix-error-msg

	  prot_read prot_write prot_exec prot_none
	  map_shared map_private map_fixed map_anonymous
	  ms_async ms_sync ms_invalidate
	  unix-mmap unix-munmap unix-msync
	  unix-mprotect

	  unix-pathname unix-file-mode unix-fd unix-pid unix-uid unix-gid
	  unix-setitimer unix-getitimer
	  unix-access r_ok w_ok x_ok f_ok unix-chdir unix-chmod setuidexec
	  setgidexec savetext readown writeown execown readgrp writegrp
	  execgrp readoth writeoth execoth unix-fchmod unix-chown unix-fchown
	  unix-getdtablesize unix-close unix-creat unix-dup unix-dup2
	  unix-fcntl f-dupfd f-getfd f-setfd f-getfl f-setfl f-getown f-setown
	  fndelay fappend fasync fcreat ftrunc fexcl unix-link unix-lseek
	  l_set l_incr l_xtnd unix-mkdir unix-open o_rdonly o_wronly o_rdwr
	  #+(or hpux svr4 bsd linux) o_ndelay
	  #+(or hpux svr4 bsd linux) o_noctty #+(or hpux svr4 bsd) o_nonblock
	  o_append o_creat o_trunc o_excl unix-pipe unix-read unix-readlink
	  unix-rename unix-rmdir unix-fast-select fd-setsize fd-set fd-clr
	  fd-isset fd-zero unix-select unix-sync unix-fsync unix-truncate
	  unix-ftruncate unix-symlink
	  #+(and sparc svr4) unix-times
	  unix-unlink unix-write unix-ioctl
	  tcsetpgrp tcgetpgrp tty-process-group
	  terminal-speeds tty-raw tty-crmod tty-echo tty-lcase
	  #-hpux tty-cbreak #-(or hpux linux) tty-tandem
	  #+(or hpux svr4 linux bsd) termios
          #+(or hpux svr4 linux bsd) c-lflag
	  #+(or hpux svr4 linux bsd) c-iflag
          #+(or hpux svr4 linux bsd) c-oflag
	  #+(or hpux svr4 linux bsd) tty-icrnl
          #+(or hpux svr4 linux) tty-ocrnl
	  #+(or hpux svr4 bsd) vdsusp #+(or hpux svr4 linux bsd) veof
	  #+(or hpux svr4 linux bsd) vintr
          #+(or hpux svr4 linux bsd) vquit
          #+(or hpux svr4 linux bsd) vstart
	  #+(or hpux svr4 linux bsd) vstop
          #+(or hpux svr4 linux bsd) vsusp
	  #+(or hpux svr4 linux bsd) c-cflag
	  #+(or hpux svr4 linux bsd) c-cc
	  #+(or bsd osf1) c-ispeed
	  #+(or bsd osf1) c-ospeed
          #+(or hpux svr4 linux bsd) tty-icanon
	  #+(or hpux svr4 linux bsd) vmin
          #+(or hpux svr4 linux bsd) vtime
	  #+(or hpux svr4 linux bsd) tty-ixon
          #+(or hpux svr4 linux bsd) tcsanow
          #+(or hpux svr4 linux bsd) tcsadrain
          #+(or hpux svr4 linux bsd) tciflush
          #+(or hpux svr4 linux bsd) tcoflush
          #+(or hpux svr4 linux bsd) tcioflush
	  #+(or hpux svr4 linux bsd) tcsaflush
          #+(or hpux svr4 linux bsd) unix-tcgetattr
          #+(or hpux svr4 linux bsd) unix-tcsetattr
          #+(or hpux svr4 bsd) unix-cfgetospeed
          #+(or hpux svr4 bsd) unix-cfsetospeed
          #+(or hpux svr4 bsd) unix-cfgetispeed
          #+(or hpux svr4 bsd) unix-cfsetispeed
          #+(or hpux svr4 linux bsd) tty-ignbrk
          #+(or hpux svr4 linux bsd) tty-brkint
          #+(or hpux svr4 linux bsd) tty-ignpar
          #+(or hpux svr4 linux bsd) tty-parmrk
          #+(or hpux svr4 linux bsd) tty-inpck
          #+(or hpux svr4 linux bsd) tty-istrip
          #+(or hpux svr4 linux bsd) tty-inlcr
          #+(or hpux svr4 linux bsd) tty-igncr
          #+(or hpux svr4 linux) tty-iuclc
          #+(or hpux svr4 linux bsd) tty-ixany
          #+(or hpux svr4 linux bsd) tty-ixoff
          #+hpux tty-ienqak
          #+(or hpux irix solaris linux bsd) tty-imaxbel
          #+(or hpux svr4 linux bsd) tty-opost
          #+(or hpux svr4 linux) tty-olcuc
          #+(or hpux svr4 linux bsd) tty-onlcr
          #+(or hpux svr4 linux) tty-onocr
          #+(or hpux svr4 linux) tty-onlret
          #+(or hpux svr4 linux) tty-ofill
          #+(or hpux svr4 linux) tty-ofdel
          #+(or hpux svr4 linux bsd) tty-isig
          #+(or hpux svr4 linux) tty-xcase
          #+(or hpux svr4 linux bsd) tty-echoe
          #+(or hpux svr4 linux bsd) tty-echok
          #+(or hpux svr4 linux bsd) tty-echonl
          #+(or hpux svr4 linux bsd) tty-noflsh
          #+(or hpux svr4 linux bsd) tty-iexten
          #+(or hpux svr4 linux bsd) tty-tostop
          #+(or hpux irix solaris linux bsd) tty-echoctl
          #+(or hpux irix solaris linux bsd) tty-echoprt
          #+(or hpux irix solaris linux bsd) tty-echoke
          #+(or hpux irix solaris) tty-defecho
          #+(or hpux irix solaris bsd) tty-flusho
          #+(or hpux irix solaris linux bsd) tty-pendin
          #+(or hpux svr4 linux bsd) tty-cstopb
          #+(or hpux svr4 linux bsd) tty-cread
          #+(or hpux svr4 linux bsd) tty-parenb
          #+(or hpux svr4 linux bsd) tty-parodd
          #+(or hpux svr4 linux bsd) tty-hupcl
          #+(or hpux svr4 linux bsd) tty-clocal
          #+(or irix solaris) rcv1en
          #+(or irix solaris) xmt1en
          #+(or hpux irix solaris) tty-loblk
          #+(or hpux svr4 linux bsd) vintr
          #+(or hpux svr4 linux bsd) verase
          #+(or hpux svr4 linux bsd) vkill
          #+(or hpux svr4 linux bsd) veol
          #+(or hpux irix solaris linux bsd) veol2
          #+(or hpux irix solaris) tty-cbaud
          #+(or hpux svr4 bsd) tty-csize #+(or hpux svr4 bsd) tty-cs5
          #+(or hpux svr4 bsd) tty-cs6 #+(or hpux svr4 bsd) tty-cs7
          #+(or hpux svr4 bsd) tty-cs8
          #+(or hpux svr4 bsd) unix-tcsendbreak
          #+(or hpux svr4 bsd) unix-tcdrain
          #+(or hpux svr4 bsd) unix-tcflush
          #+(or hpux svr4 bsd) unix-tcflow
          
	  TIOCGETP TIOCSETP TIOCFLUSH TIOCSETC TIOCGETC TIOCSLTC
	  TIOCGLTC TIOCNOTTY TIOCSPGRP TIOCGPGRP TIOCGWINSZ TIOCSWINSZ
	  TIOCSIGSEND

	  KBDCGET KBDCSET KBDCRESET KBDCRST KBDCSSTD KBDSGET KBDGCLICK
	  KBDSCLICK FIONREAD #+(or hpux bsd) siocspgrp
	  unix-exit 
	  unix-getrusage unix-fast-getrusage rusage_self rusage_children
	  unix-gettimeofday
	  #-hpux unix-utimes #-(or svr4 hpux) unix-setreuid
	  #-(or svr4 hpux) unix-setregid
	  unix-getpid unix-getppid
	  #+(or svr4 bsd)unix-setpgid
	  unix-getgid unix-getegid unix-getpgrp unix-setpgrp unix-getuid
	  unix-getpagesize unix-gethostname unix-gethostid unix-fork
	  unix-getenv unix-setenv unix-putenv unix-unsetenv
	  unix-current-directory unix-isatty unix-ttyname unix-execve
	  unix-socket unix-connect unix-bind unix-listen unix-accept
	  unix-recv unix-send unix-getpeername unix-getsockname
	  unix-getsockopt unix-setsockopt unix-openpty

	  unix-recvfrom unix-sendto unix-shutdown
	  
          unix-getpwnam unix-getpwuid unix-getgrnam unix-getgrgid
          user-info user-info-name user-info-password user-info-uid
          user-info-gid user-info-gecos user-info-dir user-info-shell
          group-info group-info-name group-info-gid group-info-members

	  unix-uname))


;;;; Common machine independent structures.

;;; From sys/types.h

(def-alien-type u-int64-t (unsigned 64))

(def-alien-type daddr-t
    #-(or linux alpha) long
    #+(or linux alpha) int)

(def-alien-type caddr-t (* char))

(def-alien-type swblk-t long)



;;; Large file support for Solaris.  Define some of the 64-bit types
;;; we need.  Unlike unix-glibc's large file support, Solaris's
;;; version is a little simpler because all of the 64-bit versions of
;;; the functions actually exist as functions.  So instead of calling
;;; the 32-bit versions of the functions, we call the 64-bit versions.
;;;
;;; These functions are: creat64, open64, truncate64, ftruncate64,
;;; stat64, lstat64, fstat64, readdir64.
;;;
;;; There are also some new structures for large file support:
;;; dirent64, stat64.
;;;
;;; FIXME: We should abstract this better, but I (rtoy) don't have any
;;; other system to test this out on, so it's a Solaris hack for now.
#+solaris
(progn
  (deftype file-offset64 () '(signed-byte 64))
  (def-alien-type off64-t int64-t)
  (def-alien-type ino64-t u-int64-t)
  (def-alien-type blkcnt64-t u-int64-t))

;;; From sys/time.h

;;; From ioctl.h


;;; From sys/dir.h
;;;



;;; From sys/resource.h

(def-alien-type nil
  (struct rlimit
    (rlim-cur #-(or linux alpha) int #+linux long #+alpha unsigned-int)	 ; current (soft) limit
    (rlim-max #-(or linux alpha) int #+linux long #+alpha unsigned-int))); maximum value for rlim-cur




(defun (setf unix-errno) (newvalue) (unix-set-errno newvalue))



;;;; User and group database structures



(defstruct group-info
  (name "" :type string)
  (password "" :type string)
  (gid 0 :type unix-gid)
  (members nil :type list))             ; list of logins as strings

;; see <grp.h>
(def-alien-type nil
  (struct group
      (gr-name (* char))                ; name of the group
      (gr-passwd (* char))              ; encrypted group password
      (gr-gid gid-t)                    ; numerical group ID
      (gr-mem (* (* char)))))           ; vector of pointers to member names




(defun unix-setuid (uid)
  _N"Set the user ID of the calling process to UID.
   If the calling process is the super-user, set the real
   and effective user IDs, and the saved set-user-ID to UID;
   if not, the effective user ID is set to UID."
  (int-syscall ("setuid" uid-t) uid))

(defun unix-setgid (gid)
  _N"Set the group ID of the calling process to GID.
   If the calling process is the super-user, set the real
   and effective group IDs, and the saved set-group-ID to GID;
   if not, the effective group ID is set to GID."
  (int-syscall ("setgid" gid-t) gid))



(defun unix-msync (addr length flags)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length)
	   (type (signed-byte 32) flags))
  (syscall ("msync" system-area-pointer size-t int) t addr length flags))



(defun unix-chown (path uid gid)
  _N"Given a file path, an integer user-id, and an integer group-id,
   unix-chown changes the owner of the file and the group of the
   file to those specified.  Either the owner or the group may be
   left unchanged by specifying them as -1.  Note: Permission will
   fail if the caller is not the superuser."
  (declare (type unix-pathname path)
	   (type (or unix-uid (integer -1 -1)) uid)
	   (type (or unix-gid (integer -1 -1)) gid))
  (void-syscall ("chown" c-string int int) (%name->file path) uid gid))

;;; Unix-fchown is exactly the same as unix-chown except that the file
;;; is specified by a file-descriptor ("fd") instead of a pathname.

(defun unix-fchown (fd uid gid)
  _N"Unix-fchown is like unix-chown, except that it accepts an integer
   file descriptor instead of a file path name."
  (declare (type unix-fd fd)
	   (type (or unix-uid (integer -1 -1)) uid)
	   (type (or unix-gid (integer -1 -1)) gid))
  (void-syscall ("fchown" int int int) fd uid gid))

;;; Returns the maximum size (i.e. the number of array elements
;;; of the file descriptor table.

(defun unix-getdtablesize ()
  _N"Unix-getdtablesize returns the maximum size of the file descriptor
   table. (i.e. the maximum number of descriptors that can exist at
   one time.)"
  (int-syscall ("getdtablesize")))

;;; Unix-dup2 makes the second file-descriptor describe the same file
;;; as the first. If the second file-descriptor points to an open
;;; file, it is first closed. In any case, the second should have a 
;;; value which is a valid file-descriptor.

(defun unix-dup2 (fd1 fd2)
  _N"Unix-dup2 duplicates an existing file descriptor just as unix-dup
   does only the new value of the duplicate descriptor may be requested
   through the second argument.  If a file already exists with the
   requested descriptor number, it will be closed and the number
   assigned to the duplicate."
  (declare (type unix-fd fd1 fd2))
  (void-syscall ("dup2" int int) fd1 fd2))


;;; Unix-link creates a hard link from name2 to name1.

(defun unix-link (name1 name2)
  _N"Unix-link creates a hard link from the file with name1 to the
   file with name2."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("link" c-string c-string)
		(%name->file name1) (%name->file name2)))


;;; Unix-sync writes all information in core memory which has been modified
;;; to permanent storage (i.e. disk).

(defun unix-sync ()
  _N"Unix-sync writes all information in core memory which has been
   modified to disk.  It returns NIL and an error code if an error
   occured."
  (void-syscall ("sync")))

;;; Unix-fsync writes the core-image of the file described by "fd" to
;;; permanent storage (i.e. disk).

(defun unix-fsync (fd)
  _N"Unix-fsync writes the core image of the file described by
   fd to disk."
  (declare (type unix-fd fd))
  (void-syscall ("fsync" int) fd))

;;; Unix-truncate accepts a file name and a new length.  The file is
;;; truncated to the new length.

(defun unix-truncate (name len)
  _N"Unix-truncate truncates the named file to the length (in
   bytes) specified by len.  NIL and an error number is returned
   if the call is unsuccessful."
  (declare (type unix-pathname name)
	   (type (unsigned-byte #+solaris 64 #-solaris 32) len))
  #-(and bsd x86)
  (void-syscall (#+solaris "truncate64" #-solaris "truncate" c-string int) name len)
  #+(and bsd x86)
  (void-syscall ("truncate" c-string unsigned-long unsigned-long) name len 0))

(defun unix-ftruncate (fd len)
  _N"Unix-ftruncate is similar to unix-truncate except that the first
   argument is a file descriptor rather than a file name."
  (declare (type unix-fd fd)
	   (type (unsigned-byte #+solaris 64 #-solaris 32) len))
  #-(and bsd x86)
  (void-syscall (#+solaris "ftruncate64" #-solaris "ftruncate" int int) fd len)
  #+(and bsd x86)
  (void-syscall ("ftruncate" int unsigned-long unsigned-long) fd len 0))

;;; TTY ioctl commands.



#+(or svr4 hpux bsd linux)
(progn
  #+bsd
  (defun unix-cfgetospeed (termios)
    _N"Get terminal output speed."
    (int-syscall ("cfgetospeed" (* (struct termios))) termios))

  #-bsd
  (defun unix-cfsetospeed (termios speed)
    _N"Set terminal output speed."
    (let ((baud (or (position speed terminal-speeds)
                    (error _"Bogus baud rate ~S" speed))))
      (void-syscall ("cfsetospeed" (* (struct termios)) int) termios baud)))
  
  #+bsd
  (defun unix-cfsetospeed (termios speed)
    _N"Set terminal output speed."
    (void-syscall ("cfsetospeed" (* (struct termios)) int) termios speed))
  
  #-bsd
  (defun unix-cfgetispeed (termios)
    _N"Get terminal input speed."
    (multiple-value-bind (speed errno)
        (int-syscall ("cfgetispeed" (* (struct termios))) termios)
      (if speed
          (values (svref terminal-speeds speed) 0)
          (values speed errno))))

  #+bsd
  (defun unix-cfgetispeed (termios)
    _N"Get terminal input speed."
    (int-syscall ("cfgetispeed" (* (struct termios))) termios))
  
  #-bsd
  (defun unix-cfsetispeed (termios speed)
    _N"Set terminal input speed."
    (let ((baud (or (position speed terminal-speeds)
                    (error _"Bogus baud rate ~S" speed))))
      (void-syscall ("cfsetispeed" (* (struct termios)) int) termios baud)))

  #+bsd
  (defun unix-cfsetispeed (termios speed)
    _N"Set terminal input speed."
    (void-syscall ("cfsetispeed" (* (struct termios)) int) termios speed))

  (defun unix-tcsendbreak (fd duration)
    _N"Send break"
    (declare (type unix-fd fd))
    (void-syscall ("tcsendbreak" int int) fd duration))

  (defun unix-tcdrain (fd)
    _N"Wait for output for finish"
    (declare (type unix-fd fd))
    (void-syscall ("tcdrain" int) fd))

  (defun unix-tcflush (fd selector)
    _N"See tcflush(3)"
    (declare (type unix-fd fd))
    (void-syscall ("tcflush" int int) fd selector))

  (defun unix-tcflow (fd action)
    _N"Flow control"
    (declare (type unix-fd fd))
    (void-syscall ("tcflow" int int) fd action)))

(defun tcsetpgrp (fd pgrp)
  _N"Set the tty-process-group for the unix file-descriptor FD to PGRP."
  (alien:with-alien ((alien-pgrp c-call:int pgrp))
    (unix-ioctl fd
		tiocspgrp
		(alien:alien-sap (alien:addr alien-pgrp)))))

(defun tcgetpgrp (fd)
  _N"Get the tty-process-group for the unix file-descriptor FD."
  (alien:with-alien ((alien-pgrp c-call:int))
    (multiple-value-bind (ok err)
	(unix-ioctl fd
		     tiocgpgrp
		     (alien:alien-sap (alien:addr alien-pgrp)))
      (if ok
	  (values alien-pgrp nil)
	  (values nil err)))))

(defun tty-process-group (&optional fd)
  _N"Get the tty-process-group for the unix file-descriptor FD.  If not supplied,
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
  _N"Set the tty-process-group for the unix file-descriptor FD to PGRP.  If not
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
  _N"Set the tty-process-group for the unix file-descriptor FD to PGRP.  If not
  supplied, FD defaults to /dev/tty."
  `(%set-tty-process-group ,pgrp ,fd))


;;; Socket options.

#+(or hpux bsd)
(define-ioctl-command SIOCSPGRP #\s 8 int :in)

#+linux
(define-ioctl-command SIOCSPGRP #\s #x8904 int :in)

#+(or hpux bsd linux)
(defun siocspgrp (fd pgrp)
  _N"Set the socket process-group for the unix file-descriptor FD to PGRP."
  (alien:with-alien ((alien-pgrp c-call:int pgrp))
    (unix-ioctl fd
		siocspgrp
		(alien:alien-sap (alien:addr alien-pgrp)))))

;;; Unix-setreuid sets the real and effective user-id's of the current
;;; process to the arguments "ruid" and "euid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "ruid" or
;;; "euid" to -1 makes the system use the current id instead.

#-(or svr4 hpux)
(defun unix-setreuid (ruid euid)
  _N"Unix-setreuid sets the real and effective user-id's of the current
   process to the specified ones.  NIL and an error number is returned
   if the call fails."
  (void-syscall ("setreuid" int int) ruid euid))

;;; Unix-setregid sets the real and effective group-id's of the current
;;; process to the arguments "rgid" and "egid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "rgid" or
;;; "egid" to -1 makes the system use the current id instead.

#-(or svr4 hpux)
(defun unix-setregid (rgid egid)
  _N"Unix-setregid sets the real and effective group-id's of the current
   process process to the specified ones.  NIL and an error number is
   returned if the call fails."
  (void-syscall ("setregid" int int) rgid egid))

(def-alien-routine ("getppid" unix-getppid) int
  _N"Unix-getppid returns the process-id of the parent of the current process.")

(def-alien-routine ("getgid" unix-getgid) int
  _N"Unix-getgid returns the real group-id of the current process.")

(def-alien-routine ("getegid" unix-getegid) int
  _N"Unix-getegid returns the effective group-id of the current process.")

;;; Unix-getpgrp returns the group-id associated with the
;;; current process.

(defun unix-getpgrp ()
  _N"Unix-getpgrp returns the group-id of the calling process."
  (int-syscall ("getpgrp")))

;;; Unix-setpgid sets the group-id of the process specified by 
;;; "pid" to the value of "pgrp".  The process must either have
;;; the same effective user-id or be a super-user process.

;;; setpgrp(int int)[freebsd] is identical to setpgid and is retained
;;; for backward compatibility. setpgrp(void)[solaris] is being phased
;;; out in favor of setsid().

(defun unix-setpgrp (pid pgrp)
  _N"Unix-setpgrp sets the process group on the process pid to
   pgrp.  NIL and an error number are returned upon failure."
  (void-syscall (#-svr4 "setpgrp" #+svr4 "setpgid" int int) pid pgrp))

(defun unix-setpgid (pid pgrp)
  _N"Unix-setpgid sets the process group of the process pid to
   pgrp. If pgid is equal to pid, the process becomes a process
   group leader. NIL and an error number are returned upon failure."
  (void-syscall ("setpgid" int int) pid pgrp))


;;;; Support routines for dealing with unix pathnames.

(export '(unix-file-kind unix-maybe-prepend-current-directory
	  unix-resolve-links unix-simplify-pathname))



;;;
;;; Support for the Interval Timer (experimental)
;;;


(defun unix-getitimer (which)
  _N"Unix-getitimer returns the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). On success,
   unix-getitimer returns 5 values,
   T, it-interval-secs, it-interval-usec, it-value-secs, it-value-usec."
  (declare (type (member :real :virtual :profile) which)
	   (values t
		   #+netbsd (unsigned-byte 63) #-netbsd (unsigned-byte 29)
		   (mod 1000000)
		   #+netbsd (unsigned-byte 63) #-netbsd (unsigned-byte 29)
		   (mod 1000000)))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itv (struct itimerval)))
      (syscall* (#-netbsd "getitimer" #+netbsd "__getitimer50" int (* (struct itimerval)))
		(values T
			(slot (slot itv 'it-interval) 'tv-sec)
			(slot (slot itv 'it-interval) 'tv-usec)
			(slot (slot itv 'it-value) 'tv-sec)
			(slot (slot itv 'it-value) 'tv-usec))
		which (alien-sap (addr itv))))))


;;;; User and group database access, POSIX Standard 9.2.2

#+solaris
(defun unix-getpwnam (login)
  _N"Return a USER-INFO structure for the user identified by LOGIN, or NIL if not found."
  (declare (type simple-string login))
  (with-alien ((buf (array c-call:char 1024))
	       (user-info (struct passwd)))
    (let ((result
	   (alien-funcall
	    (extern-alien "getpwnam_r"
			  (function (* (struct passwd))
				    c-call:c-string
				    (* (struct passwd))
				    (* c-call:char)
				    c-call:unsigned-int))
	    login
	    (addr user-info)
	    (cast buf (* c-call:char))
	    1024)))
      (when (not (zerop (sap-int (alien-sap result))))
	(make-user-info
	 :name (string (cast (slot result 'pw-name) c-call:c-string))
	 :password (string (cast (slot result 'pw-passwd) c-call:c-string))
	 :uid (slot result 'pw-uid)
	 :gid (slot result 'pw-gid)
	 :age (string (cast (slot result 'pw-age) c-call:c-string))
	 :comment (string (cast (slot result 'pw-comment) c-call:c-string))
	 :gecos (string (cast (slot result 'pw-gecos) c-call:c-string))
	 :dir (string (cast (slot result 'pw-dir) c-call:c-string))
	 :shell (string (cast (slot result 'pw-shell) c-call:c-string)))))))

#+bsd
(defun unix-getpwnam (login)
  _N"Return a USER-INFO structure for the user identified by LOGIN, or NIL if not found."
  (declare (type simple-string login))
  (let ((result
         (alien-funcall
          (extern-alien "getpwnam"
                        (function (* (struct passwd))
                                  c-call:c-string))
          login)))
    (when (not (zerop (sap-int (alien-sap result))))
      (make-user-info
       :name (string (cast (slot result 'pw-name) c-call:c-string))
       :password (string (cast (slot result 'pw-passwd) c-call:c-string))
       :uid (slot result 'pw-uid)
       :gid (slot result 'pw-gid)
       #-darwin :change #-darwin (slot result 'pw-change)
       :gecos (string (cast (slot result 'pw-gecos) c-call:c-string))
       :dir (string (cast (slot result 'pw-dir) c-call:c-string))
       :shell (string (cast (slot result 'pw-shell) c-call:c-string))))))


#+solaris
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; sysconf(_SC_GETGR_R_SIZE_MAX)
  (defconstant +sc-getgr-r-size-max+ 7296
    _N"The maximum size of the group entry buffer"))

#+solaris
(defun unix-getgrnam (name)
  _N"Return a GROUP-INFO structure for the group identified by NAME, or NIL if not found."
  (declare (type simple-string name))
  (with-alien ((buf (array c-call:char #.+sc-getgr-r-size-max+))
	       (group-info (struct group)))
    (let ((result
	   (alien-funcall
	    (extern-alien "getgrnam_r"
			  (function (* (struct group))
                                    c-call:c-string
                                    (* (struct group))
                                    (* c-call:char)
                                    c-call:unsigned-int))
	    name
	    (addr group-info)
	    (cast buf (* c-call:char))
	    #.+sc-getgr-r-size-max+)))
      (unless (zerop (sap-int (alien-sap result)))
	(make-group-info
	 :name (string (cast (slot result 'gr-name) c-call:c-string))
	 :password (string (cast (slot result 'gr-passwd) c-call:c-string))
	 :gid (slot result 'gr-gid)
         :members (loop :with members = (slot result 'gr-mem)
                        :for i :from 0
                        :for member = (deref members i)
                        :until (zerop (sap-int (alien-sap member)))
                        :collect (string (cast member c-call:c-string))))))))

#+bsd
(defun unix-getgrnam (name)
  _N"Return a GROUP-INFO structure for the group identified by NAME, or NIL if not found."
  (declare (type simple-string name))
  (let ((result
         (alien-funcall
          (extern-alien "getgrnam"
                        (function (* (struct group))
                                  c-call:c-string))
          name)))
    (unless (zerop (sap-int (alien-sap result)))
      (make-group-info
       :name (string (cast (slot result 'gr-name) c-call:c-string))
       :password (string (cast (slot result 'gr-passwd) c-call:c-string))
       :gid (slot result 'gr-gid)
       :members (loop :with members = (slot result 'gr-mem)
                      :for i :from 0
                      :for member = (deref members i)
                      :until (zerop (sap-int (alien-sap member)))
                      :collect (string (cast member c-call:c-string)))))))

#+solaris
(defun unix-getgrgid (gid)
  _N"Return a GROUP-INFO structure for the group identified by GID, or NIL if not found."
  (declare (type unix-gid gid))
  (with-alien ((buf (array c-call:char #.+sc-getgr-r-size-max+))
	       (group-info (struct group)))
    (let ((result
	   (alien-funcall
	    (extern-alien "getgrgid_r"
			  (function (* (struct group))
				     c-call:unsigned-int
				     (* (struct group))
				     (* c-call:char)
				     c-call:unsigned-int))
	    gid
	    (addr group-info)
	    (cast buf (* c-call:char))
	    #.+sc-getgr-r-size-max+)))
      (unless (zerop (sap-int (alien-sap result)))
	(make-group-info
	 :name (string (cast (slot result 'gr-name) c-call:c-string))
	 :password (string (cast (slot result 'gr-passwd) c-call:c-string))
	 :gid (slot result 'gr-gid)
	 :members (loop :with members = (slot result 'gr-mem)
		        :for i :from 0
		        :for member = (deref members i)
		        :until (zerop (sap-int (alien-sap member)))
		        :collect (string (cast member c-call:c-string))))))))

#+bsd
(defun unix-getgrgid (gid)
  _N"Return a GROUP-INFO structure for the group identified by GID, or NIL if not found."
  (declare (type unix-gid gid))
  (let ((result
         (alien-funcall
          (extern-alien "getgrgid"
                        (function (* (struct group))
                                  c-call:unsigned-int))
          gid)))
    (unless (zerop (sap-int (alien-sap result)))
      (make-group-info
       :name (string (cast (slot result 'gr-name) c-call:c-string))
       :password (string (cast (slot result 'gr-passwd) c-call:c-string))
       :gid (slot result 'gr-gid)
       :members (loop :with members = (slot result 'gr-mem)
                      :for i :from 0
                      :for member = (deref members i)
                      :until (zerop (sap-int (alien-sap member)))
                      :collect (string (cast member c-call:c-string)))))))

#+solaris
(defun unix-setpwent ()
  (void-syscall ("setpwent")))

#+solaris
(defun unix-endpwent ()
  (void-syscall ("endpwent")))

#+solaris
(defun unix-getpwent ()
  (with-alien ((buf (array c-call:char 1024))
	       (user-info (struct passwd)))
    (let ((result
	   (alien-funcall
	    (extern-alien "getpwent_r"
			  (function (* (struct passwd))
				    (* (struct passwd))
				    (* c-call:char)
				    c-call:unsigned-int))
	    (addr user-info)
	    (cast buf (* c-call:char))
	    1024)))
      (when (not (zerop (sap-int (alien-sap result))))
	(make-user-info
	 :name (string (cast (slot result 'pw-name) c-call:c-string))
	 :password (string (cast (slot result 'pw-passwd) c-call:c-string))
	 :uid (slot result 'pw-uid)
	 :gid (slot result 'pw-gid)
	 :age (string (cast (slot result 'pw-age) c-call:c-string))
	 :comment (string (cast (slot result 'pw-comment) c-call:c-string))
	 :gecos (string (cast (slot result 'pw-gecos) c-call:c-string))
	 :dir (string (cast (slot result 'pw-dir) c-call:c-string))
	 :shell (string (cast (slot result 'pw-shell) c-call:c-string)))))))

#+(and solaris svr4)
(export '(unix-sysinfo
	  si-sysname si-hostname si-release si-version si-machine
	  si-architecture si-hw-serial si-hw-provider si-srpc-domain
	  si-platform si-isalist si-dhcp-cache))

#+(and solaris svr4)
(progn
;; From sys/systeminfo.h.  We don't list the set values here.
(def-enum + 1
  si-sysname si-hostname si-release si-version si-machine
  si-architecture si-hw-serial si-hw-provider si-srpc-domain)

(def-enum + 513
  si-platform si-isalist si-dhcp-cache)


(defun unix-sysinfo (command)
  ;; Hope a buffer of length 2048 is long enough.
  (with-alien ((buf (array c-call:unsigned-char 2048)))
    (let ((result
	   (alien-funcall
	    (extern-alien "sysinfo"
			  (function c-call:int
				    c-call:int
				    c-call:c-string
				    c-call:int))
	    command
	    (cast buf (* c-call:char))
	    2048)))
      (when (>= result 0)
	(cast buf c-call:c-string)))))
)

#+solaris
(export '(rlimit_cpu rlimit_fsize rlimit_data rlimit_stack rlimit_core rlimit_nofile
	  rlimit_vmem rlimit_as))

#+solaris
(progn
(defconstant rlimit_cpu 0
  _N"CPU time per process (in milliseconds)")
(defconstant rlimit_fsize 1
  _N"Maximum file size")
(defconstant rlimit_data 2
  _N"Data segment size")
(defconstant rlimit_stack 3
  _N"Stack size")
(defconstant rlimit_core 4
  _N"Core file size")
(defconstant rlimit_nofile 5
  _N"Number of open files")
(defconstant rlimit_vmem 6
  _N"Maximum mapped memory")
(defconstant rlimit_as rlimit_vmem)
)

#+(and darwin x86)
(export '(rlimit_cpu rlimit_fsize rlimit_data rlimit_stack rlimit_core
	  rlimit_as rlimit_rss rlimit_memlock rlimit_nproc rlimit_nofile))

#+(and darwin x86)
(progn
(defconstant rlimit_cpu 0
  _N"CPU time per process")
(defconstant rlimit_fsize 1
  _N"File size")
(defconstant rlimit_data 2
  _N"Data segment size")
(defconstant rlimit_stack 3
  _N"Stack size")
(defconstant rlimit_core 4
  _N"Core file size")
(defconstant rlimit_as 5
  _N"Addess space (resident set size)")
(defconstant rlimit_rss rlimit_as)
(defconstant rlimit_memlock 6
  _N"Locked-in-memory address space")
(defconstant rlimit_nproc 7
  _N"Number of processes")
(defconstant rlimit_nofile 8
  _N"Number of open files")
)


#+(or solaris (and darwin x86))
(export '(unix-getrlimit))

#+(or solaris (and darwin x86))
(defun unix-getrlimit (resource)
  _N"Get the limits on the consumption of system resouce specified by
  Resource.  If successful, return three values: T, the current (soft)
  limit, and the maximum (hard) limit."
  
  (with-alien ((rlimit (struct rlimit)))
    (syscall ("getrlimit" c-call:int (* (struct rlimit)))
	     (values t
		     (slot rlimit 'rlim-cur)
		     (slot rlimit 'rlim-max))
	     resource (addr rlimit))))
;; EOF
