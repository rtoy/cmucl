;;; -*- Package: UNIX -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/unix-glibc2.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the UNIX low-level support for glibc2.  Based
;;; on unix.lisp 1.56, converted for glibc2 by Peter Van Eynde (1998).
;;; Alpha support by Julian Dolby, 1999.
;;;
;;; All the functions with #+(or) in front are work in progress,
;;; and mostly don't work.
;;;
;; Todo: #+(or)'ed stuff and ioctl's
;;
;;
;; Large File Support (LFS) added by Pierre Mai and Eric Marsden, Feb
;; 2003. This is necessary to be able to read/write/stat files that
;; are larger than 2GB on a 32-bit system. From a C program, defining
;; a preprocessor macro _LARGEFILE64_SOURCE makes the preproccessor
;; replace a call to open() by open64(), and similarly for stat,
;; fstat, lstat, lseek, readdir and friends. Furthermore, certain data
;; types, that are normally 32 bits wide, are replaced by 64-bit wide
;; equivalents: off_t -> off64_t etc. The libc.so fiddles around with
;; weak symbols to support this mess.
;;
;; From CMUCL, we make FFI calls to the xxx64 functions, and use the
;; 64-bit wide versions of the data structures. The most ugly aspect
;; is that some of the stat functions are not available via dlsym, so
;; we reference them explicitly from linux-stubs.S. Another amusing
;; fact is that on glibc 2.2, stat64() returns a struct stat with a
;; 32-bit ino_t, whereas readdir64() returns a struct dirent that
;; contains a 64-bit ino_t.  On glibc 2.1, OTOH, both stat64 and
;; readdir64 use structs with 32-bit ino_t.
;;
;; The current version deals with this by going with the glibc 2.2
;; definitions, unless the keyword :glibc2.1 also occurs on *features*,
;; in addition to :glibc2, in which case we go with the glibc 2.1
;; definitions.  Note that binaries compiled against glibc 2.1 do in
;; fact work fine on glibc 2.2, because readdir64 is available in both
;; glibc 2.1 and glibc 2.2 versions in glibc 2.2, disambiguated through
;; ELF symbol versioning.  We use an entry for readdir64 in linux-stubs.S
;; in order to force usage of the correct version of readdir64 at runtime.
;;
;; So in order to compile for glibc 2.2 and newer, just compile CMUCL
;; on a glibc 2.2 system, and make sure that :glibc2.1 doesn't appear
;; on the *features* list.  In order to compile for glibc 2.1 and newer,
;; compile CMUCL on a glibc 2.1 system, and make sure that :glibc2.1 does
;; appear on the *features* list.

(in-package "UNIX")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "SYSTEM")
(use-package "EXT")
(intl:textdomain "cmucl-unix-glibc2")

(export '(
	  daddr-t caddr-t ino-t swblk-t size-t time-t dev-t off-t uid-t gid-t
          blkcnt-t fsblkcnt-t fsfilcnt-t
	  unix-lockf f_ulock f_lock f_tlock f_test
	  timeval tv-sec tv-usec timezone tz-minuteswest tz-dsttime
	  itimerval it-interval it-value tchars t-intrc t-quitc t-startc
	  t-stopc t-eofc t-brkc ltchars t-suspc t-dsuspc t-rprntc t-flushc
	  t-werasc t-lnextc sgttyb sg-ispeed sg-ospeed sg-erase sg-kill
	  sg-flags winsize ws-row ws-col ws-xpixel ws-ypixel
	  direct d-off d-ino d-reclen  d-name
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
	  unix-mmap unix-munmap unix-msync unix-mprotect
	  unix-pathname unix-file-mode unix-fd unix-pid unix-uid unix-gid
	  unix-setitimer unix-getitimer
	  unix-access r_ok w_ok x_ok f_ok unix-chdir unix-chmod setuidexec
	  setgidexec savetext readown writeown execown readgrp writegrp
	  execgrp readoth writeoth execoth unix-fchmod unix-chown unix-fchown
	  unix-getdtablesize unix-close unix-creat unix-dup unix-dup2
	  unix-fcntl f-dupfd f-getfd f-setfd f-getfl f-setfl f-getown f-setown
	  fndelay fappend fasync fcreat ftrunc fexcl unix-link unix-lseek
	  l_set l_incr l_xtnd unix-mkdir unix-open o_rdonly o_wronly o_rdwr
	  o_ndelay
	  o_noctty
	  o_append o_creat o_trunc o_excl unix-pipe unix-read unix-readlink
	  unix-rename unix-rmdir unix-fast-select fd-setsize fd-set fd-clr
	  fd-isset fd-zero unix-select unix-sync unix-fsync unix-truncate
	  unix-ftruncate unix-symlink unix-unlink unix-write unix-ioctl
	  unix-uname utsname
	  tcsetpgrp tcgetpgrp tty-process-group
	  terminal-speeds tty-raw tty-crmod tty-echo tty-lcase
	  tty-cbreak
	   termios
           c-lflag
	   c-iflag
           c-oflag
	   tty-icrnl
           tty-ocrnl
	   veof
	   vintr
           vquit
           vstart
	   vstop
           vsusp
	   c-cflag
	   c-cc
           tty-icanon
	   vmin
           vtime
	   tty-ixon
           tcsanow
           tcsadrain
           tciflush
           tcoflush
           tcioflush
	   tcsaflush
           unix-tcgetattr
           unix-tcsetattr
           tty-ignbrk
           tty-brkint
           tty-ignpar
           tty-parmrk
           tty-inpck
           tty-istrip
           tty-inlcr
           tty-igncr
           tty-iuclc
           tty-ixany
           tty-ixoff
	  tty-imaxbel
           tty-opost
           tty-olcuc
           tty-onlcr
           tty-onocr
           tty-onlret
           tty-ofill
           tty-ofdel
           tty-isig
           tty-xcase
           tty-echoe
           tty-echok
           tty-echonl
           tty-noflsh
           tty-iexten
           tty-tostop
           tty-echoctl
           tty-echoprt
           tty-echoke
           tty-pendin
           tty-cstopb
           tty-cread
           tty-parenb
           tty-parodd
           tty-hupcl
           tty-clocal
           vintr
           verase
           vkill
           veol
           veol2
	  TIOCGETP TIOCSETP TIOCFLUSH TIOCSETC TIOCGETC TIOCSLTC
	  TIOCGLTC TIOCNOTTY TIOCSPGRP TIOCGPGRP TIOCGWINSZ TIOCSWINSZ
	  TIOCSIGSEND

	  KBDCGET KBDCSET KBDCRESET KBDCRST KBDCSSTD KBDSGET KBDGCLICK
	  KBDSCLICK FIONREAD	  unix-exit 
	  unix-getrusage unix-fast-getrusage rusage_self rusage_children
	  unix-gettimeofday
	  unix-utimes unix-sched-yield unix-setreuid
	  unix-setregid
	  unix-getpid unix-getppid
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
          group-info group-info-name group-info-gid group-info-members))

;;;; Common machine independent structures.

(defmacro def-enum (inc cur &rest names)
  (flet ((defform (name)
	     (prog1 (when name `(defconstant ,name ,cur))
	       (setf cur (funcall inc cur 1)))))
    `(progn ,@(mapcar #'defform names))))

;;;; User and group database structures: <pwd.h> and <grp.h>

(defstruct group-info
  (name "" :type string)
  (password "" :type string)
  (gid 0 :type unix-gid)
  (members nil :type list))             ; list of logins as strings

(def-alien-type nil
  (struct group
      (gr-name (* char))                ; name of the group
      (gr-passwd (* char))              ; encrypted group password
      (gr-gid gid-t)                    ; numerical group ID
      (gr-mem (* (* char)))))           ; vector of pointers to member names

;;; From stdio.h

;;; From sys/types.h
;;;         and
;;;      gnu/types.h

(defconstant +max-s-long+ 2147483647)

(def-alien-type quad-t #+alpha long #-alpha (array long 2))
(def-alien-type qaddr-t (* quad-t))
(def-alien-type daddr-t int)
(def-alien-type caddr-t (* char))
(def-alien-type swblk-t long)
(def-alien-type clock-t long)
(def-alien-type uid-t unsigned-int)
(def-alien-type ssize-t #-alpha int #+alpha long)
(def-alien-type key-t int)
(def-alien-type int8-t char)
(def-alien-type u-int8-t unsigned-char)
(def-alien-type int16-t short)
(def-alien-type u-int16-t unsigned-short)
(def-alien-type int32-t int)
(def-alien-type register-t #-alpha int #+alpha long)

(def-alien-type fsblkcnt-t u-int64-t)
(def-alien-type fsfilcnt-t u-int64-t)
(def-alien-type pid-t int)
;(def-alien-type ssize-t #-alpha int #+alpha long)

(def-alien-type fsid-t (array int 2))

(def-alien-type key-t int)

(def-alien-type ipc-pid-t unsigned-short)


;;; dlfcn.h -> in foreign.lisp

(defun unix-getdtablesize ()
  _N"Unix-getdtablesize returns the maximum size of the file descriptor
   table. (i.e. the maximum number of descriptors that can exist at
   one time.)"
  (int-syscall ("getdtablesize")))

;;; fcntlbits.h



(defconstant f-dupfd    0  _N"Duplicate a file descriptor")
(defconstant f-getfd    1  _N"Get file desc. flags")
(defconstant f-setfd    2  _N"Set file desc. flags")

(defconstant F-CLOEXEC 1 _N"for f-getfl and f-setfl")

#-alpha
(progn
  (defconstant F-RDLCK 0 _N"for fcntl and lockf")
  (defconstant F-WRLCK 1 _N"for fcntl and lockf")
  (defconstant F-UNLCK 2 _N"for fcntl and lockf")
  (defconstant F-EXLCK 4 _N"old bsd flock (depricated)")
  (defconstant F-SHLCK 8 _N"old bsd flock (depricated)"))
#+alpha
(progn
  (defconstant F-RDLCK 1 _N"for fcntl and lockf")
  (defconstant F-WRLCK 2 _N"for fcntl and lockf")
  (defconstant F-UNLCK 8 _N"for fcntl and lockf")
  (defconstant F-EXLCK 16 _N"old bsd flock (depricated)")
  (defconstant F-SHLCK 32 _N"old bsd flock (depricated)"))

(defconstant F-LOCK-SH 1 _N"Shared lock for bsd flock")
(defconstant F-LOCK-EX 2 _N"Exclusive lock for bsd flock")
(defconstant F-LOCK-NB 4 _N"Don't block. Combine with F-LOCK-SH or F-LOCK-EX")
(defconstant F-LOCK-UN 8 _N"Remove lock for bsd flock")

(def-alien-type nil
    (struct flock
	    (l-type short)
	    (l-whence short)
	    (l-start off-t)
	    (l-len off-t)
	    (l-pid pid-t)))

;;; grp.h 

;;;  POSIX Standard: 9.2.1 Group Database Access	<grp.h>

#+(or)
(defun unix-setgrend ()
  _N"Rewind the group-file stream."
  (void-syscall ("setgrend")))

#+(or)
(defun unix-endgrent ()
  _N"Close the group-file stream."
  (void-syscall ("endgrent")))

#+(or)
(defun unix-getgrent ()
  _N"Read an entry from the group-file stream, opening it if necessary."
  
  (let ((result (alien-funcall (extern-alien "getgrent"
					     (function (* (struct group)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

;;; ioctl-types.h

(defconstant +NCC+ 8
  _N"Size of control character vector.")

(def-alien-type nil
  (struct termio
    (c-iflag unsigned-int) ; input mode flags
    (c-oflag unsigned-int) ; output mode flags
    (c-cflag unsigned-int) ; control mode flags
    (c-lflag unsigned-int) ; local mode flags
    (c-line unsigned-char) ; line discipline
    (c-cc (array unsigned-char #.+NCC+)))) ; control characters

;;; modem lines 
(defconstant tiocm-le  1)
(defconstant tiocm-dtr 2)
(defconstant tiocm-rts 4)
(defconstant tiocm-st  8)
(defconstant tiocm-sr  #x10)
(defconstant tiocm-cts #x20)
(defconstant tiocm-car #x40)
(defconstant tiocm-rng #x80)
(defconstant tiocm-dsr #x100)
(defconstant tiocm-cd  tiocm-car)
(defconstant tiocm-ri  #x80)

;;; ioctl (fd, TIOCSERGETLSR, &result) where result may be as below 

;;; line disciplines 
(defconstant N-TTY    0)
(defconstant N-SLIP   1)
(defconstant N-MOUSE  2)
(defconstant N-PPP    3)
(defconstant N-STRIP  4)
(defconstant N-AX25   5)


;;; ioctls.h

;;; Routing table calls. 
(defconstant siocaddrt	#x890B) ;; add routing table entry	
(defconstant siocdelrt	#x890C) ;; delete routing table entry	
(defconstant siocrtmsg	#x890D) ;; call to routing system	

;;; Socket configuration controls.
(defconstant siocgifname #x8910) ;; get iface name		
(defconstant siocsiflink #x8911) ;; set iface channel		
(defconstant siocgifconf #x8912) ;; get iface list		
(defconstant siocgifflags #x8913) ;; get flags			
(defconstant siocsifflags #x8914) ;; set flags			
(defconstant siocgifaddr #x8915) ;; get PA address		
(defconstant siocsifaddr #x8916) ;; set PA address		
(defconstant siocgifdstaddr #x8917  ) ;; get remote PA address 
(defconstant siocsifdstaddr #x8918  ) ;; set remote PA address 
(defconstant siocgifbrdaddr #x8919  ) ;; get broadcast PA address 
(defconstant siocsifbrdaddr #x891a  ) ;; set broadcast PA address 
(defconstant siocgifnetmask #x891b  ) ;; get network PA mask  
(defconstant siocsifnetmask #x891c  ) ;; set network PA mask  
(defconstant siocgifmetric #x891d  ) ;; get metric   
(defconstant siocsifmetric #x891e  ) ;; set metric   
(defconstant siocgifmem #x891f  ) ;; get memory address (BSD) 
(defconstant siocsifmem #x8920  ) ;; set memory address (BSD) 
(defconstant siocgifmtu #x8921  ) ;; get MTU size   
(defconstant siocsifmtu #x8922  ) ;; set MTU size   
(defconstant siocsifhwaddr #x8924  ) ;; set hardware address  
(defconstant siocgifencap #x8925  ) ;; get/set encapsulations       
(defconstant siocsifencap #x8926)
(defconstant siocgifhwaddr #x8927  ) ;; Get hardware address  
(defconstant siocgifslave #x8929  ) ;; Driver slaving support 
(defconstant siocsifslave #x8930)
(defconstant siocaddmulti #x8931  ) ;; Multicast address lists 
(defconstant siocdelmulti #x8932)
(defconstant siocgifindex #x8933  ) ;; name -> if_index mapping 
(defconstant siogifindex SIOCGIFINDEX ) ;; misprint compatibility :-) 
(defconstant siocsifpflags #x8934  ) ;; set/get extended flags set 
(defconstant siocgifpflags #x8935)
(defconstant siocdifaddr #x8936  ) ;; delete PA address  
(defconstant siocsifhwbroadcast #x8937 ) ;; set hardware broadcast addr 
(defconstant siocgifcount #x8938  ) ;; get number of devices 

(defconstant siocgifbr #x8940  ) ;; Bridging support  
(defconstant siocsifbr #x8941  ) ;; Set bridging options  

(defconstant siocgiftxqlen #x8942  ) ;; Get the tx queue length 
(defconstant siocsiftxqlen #x8943  ) ;; Set the tx queue length  


;;; ARP cache control calls. 
;;  0x8950 - 0x8952  * obsolete calls, don't re-use 
(defconstant siocdarp #x8953  ) ;; delete ARP table entry 
(defconstant siocgarp #x8954  ) ;; get ARP table entry  
(defconstant siocsarp #x8955  ) ;; set ARP table entry  

;;; RARP cache control calls. 
(defconstant siocdrarp #x8960  ) ;; delete RARP table entry 
(defconstant siocgrarp #x8961  ) ;; get RARP table entry  
(defconstant siocsrarp #x8962  ) ;; set RARP table entry  

;;; Driver configuration calls 

(defconstant siocgifmap #x8970  ) ;; Get device parameters 
(defconstant siocsifmap #x8971  ) ;; Set device parameters 

;;; DLCI configuration calls 

(defconstant siocadddlci #x8980  ) ;; Create new DLCI device 
(defconstant siocdeldlci #x8981  ) ;; Delete DLCI device  

;;; Device private ioctl calls. 

;; These 16 ioctls are available to devices via the do_ioctl() device
;; vector.  Each device should include this file and redefine these
;; names as their own. Because these are device dependent it is a good
;; idea _NOT_ to issue them to random objects and hope. 

(defconstant siocdevprivate	#x89F0	) ;; to 89FF 


;;; netdb.h

;; All data returned by the network data base library are supplied in
;; host order and returned in network order (suitable for use in
;; system calls).

;;; Absolute file name for network data base files.
(defconstant path-hequiv "/etc/hosts.equiv")
(defconstant path-hosts "/etc/hosts")
(defconstant path-networks "/etc/networks")
(defconstant path-nsswitch_conf "/etc/nsswitch.conf")
(defconstant path-protocols "/etc/protocols")
(defconstant path-services "/etc/services")


;;; Possible values left in `h_errno'.
(defconstant netdb-internal -1 _N"See errno.")
(defconstant netdb-success 0 _N"No problem.")
(defconstant host-not-found 1 _N"Authoritative Answer Host not found.")
(defconstant try-again 2 _N"Non-Authoritative Host not found,or SERVERFAIL.")
(defconstant no-recovery 3 _N"Non recoverable errors, FORMERR, REFUSED, NOTIMP.")
(defconstant no-data 4	"Valid name, no data record of requested type.")
(defconstant no-address	no-data	"No address, look for MX record.")

;;; Description of data base entry for a single host.

(def-alien-type nil
    (struct hostent
	    (h-name c-string)        ; Official name of host.
	    (h-aliases (* c-string)) ; Alias list.
	    (h-addrtype int)         ; Host address type.
	    (h_length int)           ; Length of address.
	    (h-addr-list (* c-string)))) ; List of addresses from name server.

#+(or)
(defun unix-sethostent (stay-open)
  _N"Open host data base files and mark them as staying open even after
a later search if STAY_OPEN is non-zero."
  (void-syscall ("sethostent" int) stay-open))

#+(or)
(defun unix-endhostent ()
  _N"Close host data base files and clear `stay open' flag."
  (void-syscall ("endhostent")))

#+(or)
(defun unix-gethostent ()
  _N"Get next entry from host data base file.  Open data base if
necessary."
    (let ((result (alien-funcall (extern-alien "gethostent"
					     (function (* (struct hostent)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-gethostbyaddr(addr length type)
  _N"Return entry from host data base which address match ADDR with
length LEN and type TYPE."
    (let ((result (alien-funcall (extern-alien "gethostbyaddr"
					     (function (* (struct hostent))
						       c-string int int))
				 addr len type)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-gethostbyname (name)
  _N"Return entry from host data base for host with NAME."
    (let ((result (alien-funcall (extern-alien "gethostbyname"
					     (function (* (struct hostent))
						       c-string))
				 name)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-gethostbyname2 (name af)
  _N"Return entry from host data base for host with NAME.  AF must be
   set to the address type which as `AF_INET' for IPv4 or `AF_INET6'
   for IPv6."
    (let ((result (alien-funcall (extern-alien "gethostbyname2"
					     (function (* (struct hostent))
						       c-string int))
				 name af)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

;; Description of data base entry for a single network.  NOTE: here a
;; poor assumption is made.  The network number is expected to fit
;; into an unsigned long int variable.

(def-alien-type nil
    (struct netent
	    (n-name c-string) ; Official name of network.
	    (n-aliases (* c-string)) ; Alias list.
	    (n-addrtype int) ;  Net address type.
	    (n-net unsigned-long))) ; Network number.

#+(or)
(defun unix-setnetent (stay-open)
  _N"Open network data base files and mark them as staying open even
   after a later search if STAY_OPEN is non-zero."
  (void-syscall ("setnetent" int) stay-open))


#+(or)
(defun unix-endnetent ()
  _N"Close network data base files and clear `stay open' flag."
  (void-syscall ("endnetent")))


#+(or)
(defun unix-getnetent ()
  _N"Get next entry from network data base file.  Open data base if
   necessary."
    (let ((result (alien-funcall (extern-alien "getnetent"
					     (function (* (struct netent)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))


#+(or)
(defun unix-getnetbyaddr (net type)
  _N"Return entry from network data base which address match NET and
   type TYPE."
    (let ((result (alien-funcall (extern-alien "getnetbyaddr"
					     (function (* (struct netent))
						       unsigned-long int))
				 net type)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-getnetbyname (name)
  _N"Return entry from network data base for network with NAME."
    (let ((result (alien-funcall (extern-alien "getnetbyname"
					     (function (* (struct netent))
						       c-string))
				 name)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

;; Description of data base entry for a single service.
(def-alien-type nil
    (struct servent
	    (s-name c-string) ; Official service name.
	    (s-aliases (* c-string)) ; Alias list.
	    (s-port int) ; Port number.
	    (s-proto c-string))) ; Protocol to use.

#+(or)
(defun unix-setservent (stay-open)
  _N"Open service data base files and mark them as staying open even
   after a later search if STAY_OPEN is non-zero."
  (void-syscall ("setservent" int) stay-open))

#+(or)
(defun unix-endservent (stay-open)
  _N"Close service data base files and clear `stay open' flag."
  (void-syscall ("endservent")))


#+(or)
(defun unix-getservent ()
  _N"Get next entry from service data base file.  Open data base if
   necessary."
    (let ((result (alien-funcall (extern-alien "getservent"
					     (function (* (struct servent)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-getservbyname (name proto)
  _N"Return entry from network data base for network with NAME and
   protocol PROTO."
    (let ((result (alien-funcall (extern-alien "getservbyname"
					     (function (* (struct netent))
						       c-string (* char)))
				 name proto)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-getservbyport (port proto)
  _N"Return entry from service data base which matches port PORT and
   protocol PROTO."
    (let ((result (alien-funcall (extern-alien "getservbyport"
					     (function (* (struct netent))
						       int (* char)))
				 port proto)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

;;  Description of data base entry for a single service.

(def-alien-type nil
    (struct protoent
	    (p-name c-string) ; Official protocol name.
	    (p-aliases (* c-string)) ; Alias list.
	    (p-proto int))) ; Protocol number.

#+(or)
(defun unix-setprotoent (stay-open)
  _N"Open protocol data base files and mark them as staying open even
   after a later search if STAY_OPEN is non-zero."
  (void-syscall ("setprotoent" int) stay-open))

#+(or)
(defun unix-endprotoent ()
  _N"Close protocol data base files and clear `stay open' flag."
  (void-syscall ("endprotoent")))

#+(or)
(defun unix-getprotoent ()
  _N"Get next entry from protocol data base file.  Open data base if
   necessary."
    (let ((result (alien-funcall (extern-alien "getprotoent"
					     (function (* (struct protoent)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-getprotobyname (name)
  _N"Return entry from protocol data base for network with NAME."
    (let ((result (alien-funcall (extern-alien "getprotobyname"
					     (function (* (struct protoent))
						       c-string))
				 name)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-getprotobynumber (proto)
  _N"Return entry from protocol data base which number is PROTO."
    (let ((result (alien-funcall (extern-alien "getprotobynumber"
					     (function (* (struct protoent))
						       int))
				 proto)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-setnetgrent (netgroup)
  _N"Establish network group NETGROUP for enumeration."
  (int-syscall ("setservent" c-string) netgroup))

#+(or)
(defun unix-endnetgrent ()
  _N"Free all space allocated by previous `setnetgrent' call."
  (void-syscall ("endnetgrent")))

#+(or)
(defun unix-getnetgrent (hostp userp domainp)
  _N"Get next member of netgroup established by last `setnetgrent' call
   and return pointers to elements in HOSTP, USERP, and DOMAINP."
  (int-syscall ("getnetgrent" (* c-string) (* c-string) (* c-string))
	       hostp userp domainp))

#+(or)
(defun unix-innetgr (netgroup host user domain)
  _N"Test whether NETGROUP contains the triple (HOST,USER,DOMAIN)."
  (int-syscall ("innetgr" c-string c-string c-string c-string)
	       netgroup host user domain))

(def-alien-type nil
    (struct addrinfo
	    (ai-flags int)    ; Input flags.
	    (ai-family int)   ; Protocol family for socket.
	    (ai-socktype int) ; Socket type.
	    (ai-protocol int) ; Protocol for socket.
	    (ai-addrlen int)  ; Length of socket address.
	    (ai-addr (* (struct sockaddr)))
	                      ; Socket address for socket.
	    (ai-cononname c-string)
	                      ; Canonical name for service location.
	    (ai-net (* (struct addrinfo))))) ; Pointer to next in list.

;; Possible values for `ai_flags' field in `addrinfo' structure.

(defconstant ai_passive 1 _N"Socket address is intended for `bind'.")
(defconstant ai_canonname 2 _N"Request for canonical name.")

;; Error values for `getaddrinfo' function.
(defconstant eai_badflags -1 _N"Invalid value for `ai_flags' field.")
(defconstant eai_noname -2 _N"NAME or SERVICE is unknown.")
(defconstant eai_again -3 _N"Temporary failure in name resolution.")
(defconstant eai_fail -4 _N"Non-recoverable failure in name res.")
(defconstant eai_nodata -5 _N"No address associated with NAME.")
(defconstant eai_family -6 _N"ai_family not supported.")
(defconstant eai_socktype -7 _N"ai_socktype not supported.")
(defconstant eai_service -8 _N"SERVICE not supported for ai_socktype.")
(defconstant eai_addrfamily -9 _N"Address family for NAME not supported.")
(defconstant eai_memory -10 _N"Memory allocation failure.")
(defconstant eai_system -11 _N"System error returned in errno.")


#+(or)
(defun unix-getaddrinfo (name service req pai)
  _N"Translate name of a service location and/or a service name to set of
   socket addresses."
  (int-syscall ("getaddrinfo" c-string c-string (* (struct addrinfo))
			      (* (* struct addrinfo)))
	       name service req pai))


#+(or)
(defun unix-freeaddrinfo (ai)
  _N"Free `addrinfo' structure AI including associated storage."
  (void-syscall ("freeaddrinfo" (* struct addrinfo))
		ai))


#+(or)
(defun unix-forkpty (amaster name termp winp)
  _N"Create child process and establish the slave pseudo terminal as the
   child's controlling terminal."
  (int-syscall ("forkpty" (* int) c-string (* (struct termios))
			  (* (struct winsize)))
	       amaster name termp winp))


;; POSIX Standard: 9.2.2 User Database Access <pwd.h>

#+(or)
(defun unix-setpwent ()
  _N"Rewind the password-file stream."
  (void-syscall ("setpwent")))

#+(or)
(defun unix-endpwent ()
  _N"Close the password-file stream."
  (void-syscall ("endpwent")))

#+(or)
(defun unix-getpwent ()
  _N"Read an entry from the password-file stream, opening it if necessary."
    (let ((result (alien-funcall (extern-alien "getpwent"
					     (function (* (struct passwd)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
	result)))

;;; resourcebits.h

(def-alien-type nil
  (struct rlimit
    (rlim-cur long)	 ; current (soft) limit
    (rlim-max long))); maximum value for rlim-cur

;; Priority limits.

(defconstant prio-min -20 _N"Minimum priority a process can have")
(defconstant prio-max 20 _N"Maximum priority a process can have")


;;; The type of the WHICH argument to `getpriority' and `setpriority',
;;; indicating what flavor of entity the WHO argument specifies.

(defconstant priority-process 0 _N"WHO is a process ID")
(defconstant priority-pgrp 1 _N"WHO is a process group ID")
(defconstant priority-user 2 _N"WHO is a user ID")

;;; sched.h

#+(or)
(defun unix-sched_setparam (pid param)
  _N"Rewind the password-file stream."
  (int-syscall ("sched_setparam" pid-t (struct psched-param))
		pid param))

#+(or)
(defun unix-sched_getparam (pid param)
  _N"Rewind the password-file stream."
  (int-syscall ("sched_getparam" pid-t (struct psched-param))
		pid param))


#+(or)
(defun unix-sched_setscheduler (pid policy param)
  _N"Set scheduling algorithm and/or parameters for a process."
  (int-syscall ("sched_setscheduler" pid-t int (struct psched-param))
		pid policy param))

#+(or)
(defun unix-sched_getscheduler (pid)
  _N"Retrieve scheduling algorithm for a particular purpose."
  (int-syscall ("sched_getscheduler" pid-t)
		pid))

(defun unix-sched-yield ()
  _N"Retrieve scheduling algorithm for a particular purpose."
  (int-syscall ("sched_yield")))

#+(or)
(defun unix-sched_get_priority_max (algorithm)
  _N"Get maximum priority value for a scheduler."
  (int-syscall ("sched_get_priority_max" int)
		algorithm))

#+(or)
(defun unix-sched_get_priority_min (algorithm)
  _N"Get minimum priority value for a scheduler."
  (int-syscall ("sched_get_priority_min" int)
		algorithm))



#+(or)
(defun unix-sched_rr_get_interval (pid t)
  _N"Get the SCHED_RR interval for the named process."
  (int-syscall ("sched_rr_get_interval" pid-t (* (struct timespec)))
		pid t))

;;; schedbits.h

(defconstant scheduler-other 0)
(defconstant scheduler-fifo 1)
(defconstant scheduler-rr 2)


;; Data structure to describe a process' schedulability.

(def-alien-type nil
    (struct sched_param
	    (sched-priority int)))

;; Cloning flags.
(defconstant csignal       #x000000ff _N"Signal mask to be sent at exit.")
(defconstant clone_vm      #x00000100 _N"Set if VM shared between processes.")
(defconstant clone_fs      #x00000200 _N"Set if fs info shared between processes")
(defconstant clone_files   #x00000400 _N"Set if open files shared between processe")
(defconstant clone_sighand #x00000800 _N"Set if signal handlers shared.")
(defconstant clone_pid     #x00001000 _N"Set if pid shared.")


;;; shadow.h

;; Structure of the password file.

(def-alien-type nil
    (struct spwd
	    (sp-namp c-string) ; Login name.
	    (sp-pwdp c-string) ; Encrypted password.
	    (sp-lstchg long)   ; Date of last change.
	    (sp-min long)      ; Minimum number of days between changes.
	    (sp-max long)      ; Maximum number of days between changes.
	    (sp-warn long)     ; Number of days to warn user to change the password.
	    (sp-inact long)    ; Number of days the account may be inactive.
	    (sp-expire long)   ; Number of days since 1970-01-01 until account expires.
	    (sp-flags long)))  ; Reserved.

#+(or)
(defun unix-setspent ()
  _N"Open database for reading."
  (void-syscall ("setspent")))

#+(or)
(defun unix-endspent ()
  _N"Close database."
  (void-syscall ("endspent")))

#+(or)
(defun unix-getspent ()
  _N"Get next entry from database, perhaps after opening the file."
    (let ((result (alien-funcall (extern-alien "getspent"
					     (function (* (struct spwd)))))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-getspnam (name)
  _N"Get shadow entry matching NAME."
    (let ((result (alien-funcall (extern-alien "getspnam"
					     (function (* (struct spwd))
						       c-string))
				 name)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

#+(or)
(defun unix-sgetspent (string)
  _N"Read shadow entry from STRING."
    (let ((result (alien-funcall (extern-alien "sgetspent"
					     (function (* (struct spwd))
						       c-string))
				 string)))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))

;; 

#+(or)
(defun unix-lckpwdf ()
  _N"Protect password file against multi writers."
  (void-syscall ("lckpwdf")))


#+(or)
(defun unix-ulckpwdf ()
  _N"Unlock password file."
  (void-syscall ("ulckpwdf")))

;; Protection bits.

(defconstant s-isuid #o0004000 _N"Set user ID on execution.")
(defconstant s-isgid #o0002000 _N"Set group ID on execution.")
(defconstant s-isvtx #o0001000 _N"Save swapped text after use (sticky).")
(defconstant s-iread #o0000400 _N"Read by owner")
(defconstant s-iwrite #o0000200 _N"Write by owner.")
(defconstant s-iexec #o0000100 _N"Execute by owner.")

;;; statfsbuf.h

(def-alien-type nil
    (struct statfs
	    (f-type int)
	    (f-bsize int)
	    (f-blocks fsblkcnt-t)
	    (f-bfree fsblkcnt-t)
	    (f-bavail fsblkcnt-t)
	    (f-files fsfilcnt-t)
	    (f-ffree fsfilcnt-t)
	    (f-fsid fsid-t)
	    (f-namelen int)
	    (f-spare (array int 6))))


;;; termbits.h



(def-enum + 0 tciflush tcoflush tcioflush)

(defconstant tty-nl0 0)
(defconstant tty-nl1 #o400)

(defconstant tty-crdly	#o0003000)
(defconstant   tty-cr0	#o0000000)
(defconstant   tty-cr1	#o0001000)
(defconstant   tty-cr2	#o0002000)
(defconstant   tty-cr3	#o0003000)
(defconstant tty-tabdly	#o0014000)
(defconstant   tty-tab0	#o0000000)
(defconstant   tty-tab1	#o0004000)
(defconstant   tty-tab2	#o0010000)
(defconstant   tty-tab3	#o0014000)
(defconstant   tty-xtabs	#o0014000)
(defconstant tty-bsdly	#o0020000)
(defconstant   tty-bs0	#o0000000)
(defconstant   tty-bs1	#o0020000)
(defconstant tty-vtdly	#o0040000)
(defconstant   tty-vt0	#o0000000)
(defconstant   tty-vt1	#o0040000)
(defconstant tty-ffdly	#o0100000)
(defconstant   tty-ff0	#o0000000)
(defconstant   tty-ff1	#o0100000)

;; c-cflag bit meaning
(defconstant tty-cbaud	#o0010017)
(defconstant tty-b0	#o0000000) ;; hang up
(defconstant tty-b50	#o0000001)
(defconstant tty-b75	#o0000002)
(defconstant tty-b110	#o0000003)
(defconstant tty-b134	#o0000004)
(defconstant tty-b150	#o0000005)
(defconstant tty-b200	#o0000006)
(defconstant tty-b300	#o0000007)
(defconstant tty-b600	#o0000010)
(defconstant tty-b1200	#o0000011)
(defconstant tty-b1800	#o0000012)
(defconstant tty-b2400	#o0000013)
(defconstant tty-b4800	#o0000014)
(defconstant tty-b9600	#o0000015)
(defconstant tty-b19200	#o0000016)
(defconstant tty-b38400	#o0000017)
(defconstant tty-exta tty-b19200)
(defconstant tty-extb tty-b38400)
(defconstant tty-csize	#o0000060)
(defconstant tty-cs5	#o0000000)
(defconstant tty-cs6	#o0000020)
(defconstant tty-cs7	#o0000040)
(defconstant tty-cs8	#o0000060)
(defconstant tty-cstopb	#o0000100)
(defconstant tty-cread	#o0000200)
(defconstant tty-parenb	#o0000400)
(defconstant tty-parodd	#o0001000)
(defconstant tty-hupcl	#o0002000)
(defconstant tty-clocal	#o0004000)
(defconstant tty-cbaudex #o0010000)
(defconstant tty-b57600  #o0010001)
(defconstant tty-b115200 #o0010002)
(defconstant tty-b230400 #o0010003)
(defconstant tty-b460800 #o0010004)
(defconstant tty-cibaud	  #o002003600000) ; input baud rate (not used)
(defconstant tty-crtscts	  #o020000000000) ;flow control 

;;; tcflow() and TCXONC use these 
(def-enum + 0 tty-tcooff tty-tcoon tty-tcioff tty-tcion)

;; tcflush() and TCFLSH use these */
(def-enum + 0 tty-tciflush tty-tcoflush tty-tcioflush)

;; tcsetattr uses these
(def-enum + 0 tty-tcsanow tty-tcsadrain tty-tcsaflush)

;;; termios.h

(defun unix-cfsetospeed (termios speed)
  _N"Set terminal output speed."
  (let ((baud (or (position speed terminal-speeds)
		  (error _"Bogus baud rate ~S" speed))))
    (void-syscall ("cfsetospeed" (* (struct termios)) int) termios baud)))

(defun unix-cfgetispeed (termios)
  _N"Get terminal input speed."
  (multiple-value-bind (speed errno)
      (int-syscall ("cfgetispeed" (* (struct termios))) termios)
    (if speed
	(values (svref terminal-speeds speed) 0)
      (values speed errno))))

(defun unix-cfsetispeed (termios speed)
  _N"Set terminal input speed."
  (let ((baud (or (position speed terminal-speeds)
		  (error _"Bogus baud rate ~S" speed))))
    (void-syscall ("cfsetispeed" (* (struct termios)) int) termios baud)))

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
  (void-syscall ("tcflow" int int) fd action))

;;; timebits.h

;;; unistd.h


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

#+(or)
(defun unix-pathconf (path name)
  _N"Get file-specific configuration information about PATH."
  (int-syscall ("pathconf" c-string int) (%name->file path) name))

#+(or)
(defun unix-sysconf (name)
  _N"Get the value of the system variable NAME."
  (int-syscall ("sysconf" int) name))

#+(or)
(defun unix-confstr (name)
  _N"Get the value of the string-valued system variable NAME."
  (with-alien ((buf (array char 1024)))
    (values (not (zerop (alien-funcall (extern-alien "confstr"
						     (function int
							       c-string
							       size-t))
				       name buf 1024)))
	    (cast buf c-string))))


(def-alien-routine ("getppid" unix-getppid) int
  _N"Unix-getppid returns the process-id of the parent of the current process.")

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
  (void-syscall ("setpgid" int int) pid pgrp))

(defun unix-setpgid (pid pgrp)
  _N"Unix-setpgid sets the process group of the process pid to
   pgrp. If pgid is equal to pid, the process becomes a process
   group leader. NIL and an error number are returned upon failure."
  (void-syscall ("setpgid" int int) pid pgrp))

#+(or)
(defun unix-setsid ()
  _N"Create a new session with the calling process as its leader.
   The process group IDs of the session and the calling process
   are set to the process ID of the calling process, which is returned."
  (void-syscall ( "setsid")))

#+(or)
(defun unix-getsid ()
  _N"Return the session ID of the given process."
  (int-syscall ( "getsid")))

#+(or)
(def-alien-routine ("geteuid" unix-getuid) int
  _N"Get the effective user ID of the calling process.")

(def-alien-routine ("getgid" unix-getgid) int
  _N"Unix-getgid returns the real group-id of the current process.")

(def-alien-routine ("getegid" unix-getegid) int
  _N"Unix-getegid returns the effective group-id of the current process.")

;/* If SIZE is zero, return the number of supplementary groups
;   the calling process is in.  Otherwise, fill in the group IDs
;   of its supplementary groups in LIST and return the number written.  */
;extern int getgroups __P ((int __size, __gid_t __list[]));

#+(or)
(defun unix-group-member (gid)
  _N"Return nonzero iff the calling process is in group GID."
  (int-syscall ( "group-member" gid-t) gid))


(defun unix-setuid (uid)
  _N"Set the user ID of the calling process to UID.
   If the calling process is the super-user, set the real
   and effective user IDs, and the saved set-user-ID to UID;
   if not, the effective user ID is set to UID."
  (int-syscall ("setuid" uid-t) uid))

;;; Unix-setreuid sets the real and effective user-id's of the current
;;; process to the arguments "ruid" and "euid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "ruid" or
;;; "euid" to -1 makes the system use the current id instead.

(defun unix-setreuid (ruid euid)
  _N"Unix-setreuid sets the real and effective user-id's of the current
   process to the specified ones.  NIL and an error number is returned
   if the call fails."
  (void-syscall ("setreuid" int int) ruid euid))

(defun unix-setgid (gid)
  _N"Set the group ID of the calling process to GID.
   If the calling process is the super-user, set the real
   and effective group IDs, and the saved set-group-ID to GID;
   if not, the effective group ID is set to GID."
  (int-syscall ("setgid" gid-t) gid))


;;; Unix-setregid sets the real and effective group-id's of the current
;;; process to the arguments "rgid" and "egid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "rgid" or
;;; "egid" to -1 makes the system use the current id instead.

(defun unix-setregid (rgid egid)
  _N"Unix-setregid sets the real and effective group-id's of the current
   process process to the specified ones.  NIL and an error number is
   returned if the call fails."
  (void-syscall ("setregid" int int) rgid egid))

;;; Unix-link creates a hard link from name2 to name1.

(defun unix-link (name1 name2)
  _N"Unix-link creates a hard link from the file with name1 to the
   file with name2."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("link" c-string c-string)
		(%name->file name1) (%name->file name2)))

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

(defun tcsetpgrp (fd pgrp)
  _N"Set the tty-process-group for the unix file-descriptor FD to PGRP."
  (alien:with-alien ((alien-pgrp c-call:int pgrp))
    (unix-ioctl fd
		tiocspgrp
		(alien:alien-sap (alien:addr alien-pgrp)))))

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

#+(or)
(defun unix-getlogin ()
  _N"Return the login name of the user."
    (let ((result (alien-funcall (extern-alien "getlogin"
					     (function c-string)))))
    (declare (type system-area-pointer result))
    (if (zerop (sap-int result))
	nil
      result)))


#+(or)
(defun unix-sethostname (name len)
  (int-syscall ("sethostname" c-string size-t) name len))

#+(or)
(defun unix-sethostid (id)
  (int-syscall ("sethostid" long) id))

#+(or)
(defun unix-getdomainname (name len)
  (int-syscall ("getdomainname" c-string size-t) name len))

#+(or)
(defun unix-setdomainname (name len)
  (int-syscall ("setdomainname" c-string size-t) name len))

;;; Unix-fsync writes the core-image of the file described by "fd" to
;;; permanent storage (i.e. disk).

(defun unix-fsync (fd)
  _N"Unix-fsync writes the core image of the file described by
   fd to disk."
  (declare (type unix-fd fd))
  (void-syscall ("fsync" int) fd))


#+(or)
(defun unix-vhangup ()
 _N"Revoke access permissions to all processes currently communicating
  with the control terminal, and then send a SIGHUP signal to the process
  group of the control terminal." 
 (int-syscall ("vhangup")))

#+(or)
(defun unix-revoke (file)
 _N"Revoke the access of all descriptors currently open on FILE."
 (int-syscall ("revoke" c-string) (%name->file file)))


#+(or)
(defun unix-chroot (path)
 _N"Make PATH be the root directory (the starting point for absolute paths).
   This call is restricted to the super-user."
 (int-syscall ("chroot" c-string) (%name->file path)))

;;; Unix-sync writes all information in core memory which has been modified
;;; to permanent storage (i.e. disk).

(defun unix-sync ()
  _N"Unix-sync writes all information in core memory which has been
   modified to disk.  It returns NIL and an error code if an error
   occured."
  (void-syscall ("sync")))

;;; Unix-truncate accepts a file name and a new length.  The file is
;;; truncated to the new length.

(defun unix-truncate (name length)
  _N"Unix-truncate truncates the named file to the length (in
   bytes) specified by LENGTH.  NIL and an error number is returned
   if the call is unsuccessful."
  (declare (type unix-pathname name)
	   (type (unsigned-byte 64) length))
  (void-syscall ("truncate64" c-string off-t) (%name->file name) length))

(defun unix-ftruncate (fd length)
  _N"Unix-ftruncate is similar to unix-truncate except that the first
   argument is a file descriptor rather than a file name."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 64) length))
  (void-syscall ("ftruncate64" int off-t) fd length))

#+(or)
(defun unix-getdtablesize ()
  _N"Return the maximum number of file descriptors
   the current process could possibly have."
  (int-syscall ("getdtablesize")))

(defconstant f_ulock 0 _N"Unlock a locked region")
(defconstant f_lock 1 _N"Lock a region for exclusive use")
(defconstant f_tlock 2 _N"Test and lock a region for exclusive use")
(defconstant f_test 3 _N"Test a region for othwer processes locks")

(defun unix-lockf (fd cmd length)
  _N"Unix-locks can lock, unlock and test files according to the cmd
   which can be one of the following:

   f_ulock  Unlock a locked region
   f_lock   Lock a region for exclusive use
   f_tlock  Test and lock a region for exclusive use
   f_test   Test a region for othwer processes locks

   The lock is for a region from the current location for a length
   of length.

   This is a simpler version of the interface provided by unix-fcntl.
   "
  (declare (type unix-fd fd)
	   (type (unsigned-byte 64) length)
	   (type (integer 0 3) cmd))
  (int-syscall ("lockf64" int int off-t) fd cmd length))

;;; utime.h

;; Structure describing file times.

(def-alien-type nil
    (struct utimbuf
	    (actime time-t) ; Access time. 
	    (modtime time-t))) ; Modification time.

;;; waitflags.h

;; Bits in the third argument to `waitpid'.

(defconstant waitpid-wnohang 1 _N"Don't block waiting.")
(defconstant waitpid-wuntranced 2 _N"Report status of stopped children.")

(defconstant waitpid-wclone #x80000000 _N"Wait for cloned process.")


;;; sys/fsuid.h

#+(or)
(defun unix-setfsuid (uid)
  _N"Change uid used for file access control to UID, without affecting
   other priveledges (such as who can send signals at the process)."
  (int-syscall ("setfsuid" uid-t) uid))

#+(or)
(defun unix-setfsgid (gid)
  _N"Change gid used for file access control to GID, without affecting
   other priveledges (such as who can send signals at the process)."
  (int-syscall ("setfsgid" gid-t) gid))

;;; sys/poll.h

;; Data structure describing a polling request.

(def-alien-type nil
    (struct pollfd
	    (fd int)       ; File descriptor to poll.
	    (events short) ; Types of events poller cares about.
	    (revents short))) ; Types of events that actually occurred.

;; Event types that can be polled for.  These bits may be set in `events'
;; to indicate the interesting event types; they will appear in `revents'
;; to indicate the status of the file descriptor.  

(defconstant POLLIN  #o1 _N"There is data to read.")
(defconstant POLLPRI #o2 _N"There is urgent data to read.")
(defconstant POLLOUT #o4 _N"Writing now will not block.")

;; Event types always implicitly polled for.  These bits need not be set in
;;`events', but they will appear in `revents' to indicate the status of
;; the file descriptor.  */


(defconstant POLLERR  #o10 _N"Error condition.")
(defconstant POLLHUP  #o20 _N"Hung up.")
(defconstant POLLNVAL #o40 _N"Invalid polling request.")


(defconstant +npollfile+ 30 _N"Canonical number of polling requests to read
in at a time in poll.")

#+(or)
(defun unix-poll (fds nfds timeout)
 _N" Poll the file descriptors described by the NFDS structures starting at
   FDS.  If TIMEOUT is nonzero and not -1, allow TIMEOUT milliseconds for
   an event to occur; if TIMEOUT is -1, block until an event occurs.
   Returns the number of file descriptors with events, zero if timed out,
   or -1 for errors."
 (int-syscall ("poll" (* (struct pollfd)) long int)
	      fds nfds timeout))

;;; sys/resource.h

(defun unix-getrlimit (resource)
  _N"Get the soft and hard limits for RESOURCE."
  (with-alien ((rlimits (struct rlimit)))
    (syscall ("getrlimit" int (* (struct rlimit)))
	     (values t
		     (slot rlimits 'rlim-cur)
		     (slot rlimits 'rlim-max))
	     resource (addr rlimits))))

(defun unix-setrlimit (resource current maximum)
  _N"Set the current soft and hard maximum limits for RESOURCE.
   Only the super-user can increase hard limits."
  (with-alien ((rlimits (struct rlimit)))
    (setf (slot rlimits 'rlim-cur) current)
    (setf (slot rlimits 'rlim-max) maximum)
    (void-syscall ("setrlimit" int (* (struct rlimit)))
		  resource (addr rlimits))))


#+(or)
(defun unix-ulimit (cmd newlimit)
 _N"Function depends on CMD:
  1 = Return the limit on the size of a file, in units of 512 bytes.
  2 = Set the limit on the size of a file to NEWLIMIT.  Only the
      super-user can increase the limit.
  3 = Return the maximum possible address of the data segment.
  4 = Return the maximum number of files that the calling process can open.
  Returns -1 on errors."
 (int-syscall ("ulimit" int long) cmd newlimit))

#+(or)
(defun unix-getpriority (which who)
  _N"Return the highest priority of any process specified by WHICH and WHO
   (see above); if WHO is zero, the current process, process group, or user
   (as specified by WHO) is used.  A lower priority number means higher
   priority.  Priorities range from PRIO_MIN to PRIO_MAX (above)."
  (int-syscall ("getpriority" int int)
	       which who))

#+(or)
(defun unix-setpriority (which who)
  _N"Set the priority of all processes specified by WHICH and WHO (see above)
   to PRIO.  Returns 0 on success, -1 on errors."
  (int-syscall ("setpriority" int int)
	       which who))


(defun unix-umask (mask)
  _N"Set the file creation mask of the current process to MASK,
   and return the old creation mask."
  (int-syscall ("umask" mode-t) mask))

#+(or)
(defun unix-makedev (path mode dev)
 _N"Create a device file named PATH, with permission and special bits MODE
  and device number DEV (which can be constructed from major and minor
  device numbers with the `makedev' macro above)."
  (declare (type unix-pathname path)
	   (type unix-file-mode mode))
  (void-syscall ("makedev" c-string mode-t dev-t) (%name->file name) mode dev))


#+(or)
(defun unix-fifo (name mode)
  _N"Create a new FIFO named PATH, with permission bits MODE."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("mkfifo" c-string int) (%name->file name) mode))

;;; sys/statfs.h

#+(or)
(defun unix-statfs (file buf)
  _N"Return information about the filesystem on which FILE resides."
  (int-syscall ("statfs64" c-string (* (struct statfs)))
	       (%name->file file) buf))

;;; sys/swap.h

#+(or)
(defun unix-swapon (path flags)
 _N"Make the block special device PATH available to the system for swapping.
  This call is restricted to the super-user."
 (int-syscall ("swapon" c-string int) (%name->file path) flags))

#+(or)
(defun unix-swapoff (path)
 _N"Make the block special device PATH unavailable to the system for swapping.
  This call is restricted to the super-user."
 (int-syscall ("swapoff" c-string) (%name->file path)))

;;; sys/sysctl.h

#+(or)
(defun unix-sysctl (name nlen oldval oldlenp newval newlen)
  _N"Read or write system parameters."
  (int-syscall ("sysctl" int int (* void) (* void) (* void) size-t)
	       name nlen oldval oldlenp newval newlen))

;;; time.h

;; POSIX.4 structure for a time value.  This is like a `struct timeval' but
;; has nanoseconds instead of microseconds.

(def-alien-type nil
    (struct timespec
	    (tv-sec long)   ;Seconds
	    (tv-nsec long))) ;Nanoseconds

;; Used by other time functions. 

(def-alien-type nil
    (struct tm
	    (tm-sec int)   ; Seconds.	[0-60] (1 leap second)
	    (tm-min int)   ; Minutes.	[0-59]
	    (tm-hour int)  ; Hours.	[0-23]
	    (tm-mday int)  ; Day.		[1-31]
	    (tm-mon int)   ;  Month.	[0-11]
	    (tm-year int)  ; Year	- 1900.
	    (tm-wday int)  ; Day of week.	[0-6]
	    (tm-yday int)  ; Days in year.[0-365]
	    (tm-isdst int) ;  DST.		[-1/0/1]
	    (tm-gmtoff long) ;  Seconds east of UTC.
	    (tm-zone c-string))) ; Timezone abbreviation.  

#+(or)
(defun unix-clock ()
  _N"Time used by the program so far (user time + system time).
   The result / CLOCKS_PER_SECOND is program time in seconds."
  (int-syscall ("clock")))

#+(or)
(defun unix-time (timer)
  _N"Return the current time and put it in *TIMER if TIMER is not NULL."
  (int-syscall ("time" time-t) timer))

;; Requires call to tzset() in main.

(def-alien-variable ("daylight" unix-daylight) int)
(def-alien-variable ("timezone" unix-timezone) time-t)
;(def-alien-variable ("altzone" unix-altzone) time-t) doesn't exist
(def-alien-variable ("tzname" unix-tzname) (array c-string 2))

(def-alien-routine get-timezone c-call:void
  (when c-call:long :in)
  (minutes-west c-call:int :out)
  (daylight-savings-p alien:boolean :out))

(defun unix-get-minutes-west (secs)
  (multiple-value-bind (ignore minutes dst) (get-timezone secs)
    (declare (ignore ignore) (ignore dst))
    (values minutes)))
  
(defun unix-get-timezone (secs)
  (multiple-value-bind (ignore minutes dst) (get-timezone secs)
    (declare (ignore ignore) (ignore minutes))
    (values (deref unix-tzname (if dst 1 0)))))

;/* Set the current time of day and timezone information.
;   This call is restricted to the super-user.  */
;extern int __settimeofday __P ((__const struct timeval *__tv,
;    __const struct timezone *__tz));
;extern int settimeofday __P ((__const struct timeval *__tv,
;         __const struct timezone *__tz));

;/* Adjust the current time of day by the amount in DELTA.
;   If OLDDELTA is not NULL, it is filled in with the amount
;   of time adjustment remaining to be done from the last `adjtime' call.
;   This call is restricted to the super-user.  */
;extern int __adjtime __P ((__const struct timeval *__delta,
;      struct timeval *__olddelta));
;extern int adjtime __P ((__const struct timeval *__delta,
;    struct timeval *__olddelta));


;;; sys/timeb.h

;; Structure returned by the `ftime' function.

(def-alien-type nil
    (struct timeb
	    (time time-t)      ; Seconds since epoch, as from `time'.
	    (millitm short)    ; Additional milliseconds.
	    (timezone int)     ; Minutes west of GMT.
	    (dstflag short)))  ; Nonzero if Daylight Savings Time used. 

#+(or)
(defun unix-fstime (timebuf)
  _N"Fill in TIMEBUF with information about the current time."
  (int-syscall ("ftime" (* (struct timeb))) timebuf))


;;; sys/times.h

;; Structure describing CPU time used by a process and its children.

(def-alien-type nil
    (struct tms
	    (tms-utime clock-t) ; User CPU time.
	    (tms-stime clock-t) ; System CPU time.
	    (tms-cutime clock-t) ; User CPU time of dead children.
	    (tms-cstime clock-t))) ; System CPU time of dead children.

#+(or)
(defun unix-times (buffer)
  _N"Store the CPU time used by this process and all its
   dead children (and their dead children) in BUFFER.
   Return the elapsed real time, or (clock_t) -1 for errors.
   All times are in CLK_TCKths of a second."
  (int-syscall ("times" (* (struct tms))) buffer))

;;; sys/wait.h

#+(or)
(defun unix-wait (status)
  _N"Wait for a child to die.  When one does, put its status in *STAT_LOC
   and return its process ID.  For errors, return (pid_t) -1."
  (int-syscall ("wait" (* int)) status))

#+(or)
(defun unix-waitpid (pid status options)
  _N"Wait for a child matching PID to die.
   If PID is greater than 0, match any process whose process ID is PID.
   If PID is (pid_t) -1, match any process.
   If PID is (pid_t) 0, match any process with the
   same process group as the current process.
   If PID is less than -1, match any process whose
   process group is the absolute value of PID.
   If the WNOHANG bit is set in OPTIONS, and that child
   is not already dead, return (pid_t) 0.  If successful,
   return PID and store the dead child's status in STAT_LOC.
   Return (pid_t) -1 for errors.  If the WUNTRACED bit is
   set in OPTIONS, return status for stopped children; otherwise don't."
  (int-syscall ("waitpit" pid-t (* int) int)
	       pid status options))

;;; the ioctl's.
;;;
;;; I've deleted all the stuff that wasn't in the header files.
;;; This is what survived.


;;; asm/sockios.h

;;; Socket options.

(define-ioctl-command SIOCSPGRP #x89 #x02)

(defun siocspgrp (fd pgrp)
  _N"Set the socket process-group for the unix file-descriptor FD to PGRP."
  (alien:with-alien ((alien-pgrp c-call:int pgrp))
    (unix-ioctl fd
		siocspgrp
		(alien:alien-sap (alien:addr alien-pgrp)))))

;;; A few random constants and functions

(defconstant setuidexec #o4000 _N"Set user ID on execution")
(defconstant setgidexec #o2000 _N"Set group ID on execution")
(defconstant savetext #o1000 _N"Save text image after execution")
(defconstant readown #o400 _N"Read by owner")
(defconstant execown #o100 _N"Execute (search directory) by owner")
(defconstant readgrp #o40 _N"Read by group")
(defconstant writegrp #o20 _N"Write by group")
(defconstant execgrp #o10 _N"Execute (search directory) by group")
(defconstant readoth #o4 _N"Read by others")
(defconstant writeoth #o2 _N"Write by others")
(defconstant execoth #o1 _N"Execute (search directory) by others")

;;;; Support routines for dealing with unix pathnames.

(export '(unix-file-kind unix-maybe-prepend-current-directory))

;;; Stuff not yet found in the header files...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abandon all hope who enters here...


;;;; User and group database access, POSIX Standard 9.2.2

(defun unix-getpwnam (login)
  _N"Return a USER-INFO structure for the user identified by LOGIN, or NIL if not found."
  (declare (type simple-string login))
  (with-alien ((buf (array c-call:char 1024))
	       (user-info (struct passwd))
               (result (* (struct passwd))))
    (let ((returned
	   (alien-funcall
	    (extern-alien "getpwnam_r"
			  (function c-call:int
                                    c-call:c-string
                                    (* (struct passwd))
				    (* c-call:char)
                                    c-call:unsigned-int
                                    (* (* (struct passwd)))))
	    login
	    (addr user-info)
	    (cast buf (* c-call:char))
	    1024
            (addr result))))
      (when (zerop returned)
        (make-user-info
         :name (string (cast (slot result 'pw-name) c-call:c-string))
         :password (string (cast (slot result 'pw-passwd) c-call:c-string))
         :uid (slot result 'pw-uid)
         :gid (slot result 'pw-gid)
         :gecos (string (cast (slot result 'pw-gecos) c-call:c-string))
         :dir (string (cast (slot result 'pw-dir) c-call:c-string))
         :shell (string (cast (slot result 'pw-shell) c-call:c-string)))))))

(defun unix-getgrnam (name)
  _N"Return a GROUP-INFO structure for the group identified by NAME, or NIL if not found."
  (declare (type simple-string name))
  (with-alien ((buf (array c-call:char 2048))
	       (group-info (struct group))
               (result (* (struct group))))
    (let ((returned
	   (alien-funcall
	    (extern-alien "getgrnam_r"
			  (function c-call:int
                                    c-call:c-string
                                    (* (struct group))
                                    (* c-call:char)
                                    c-call:unsigned-int
                                    (* (* (struct group)))))
	    name
	    (addr group-info)
	    (cast buf (* c-call:char))
	    2048
            (addr result))))
      (when (zerop returned)
        (make-group-info
         :name (string (cast (slot result 'gr-name) c-call:c-string))
         :password (string (cast (slot result 'gr-passwd) c-call:c-string))
         :gid (slot result 'gr-gid)
         :members (loop :with members = (slot result 'gr-mem)
                        :for i :from 0
                        :for member = (deref members i)
                        :until (zerop (sap-int (alien-sap member)))
                        :collect (string (cast member c-call:c-string))))))))

(defun unix-getgrgid (gid)
  _N"Return a GROUP-INFO structure for the group identified by GID, or NIL if not found."
  (declare (type unix-gid gid))
  (with-alien ((buf (array c-call:char 2048))
	       (group-info (struct group))
               (result (* (struct group))))
    (let ((returned
	   (alien-funcall
	    (extern-alien "getgrgid_r"
			  (function c-call:int
                                    c-call:unsigned-int
                                    (* (struct group))
                                    (* c-call:char)
                                    c-call:unsigned-int
                                    (* (* (struct group)))))
	    gid
	    (addr group-info)
	    (cast buf (* c-call:char))
	    2048
            (addr result))))
      (when (zerop returned)
        (make-group-info
         :name (string (cast (slot result 'gr-name) c-call:c-string))
         :password (string (cast (slot result 'gr-passwd) c-call:c-string))
         :gid (slot result 'gr-gid)
         :members (loop :with members = (slot result 'gr-mem)
                        :for i :from 0
                        :for member = (deref members i)
                        :until (zerop (sap-int (alien-sap member)))
                        :collect (string (cast member c-call:c-string))))))))

(defun unix-uname ()
  _N"Unix-uname returns information from the uname(2) system call.
  The return values are

    Name of the operating system
    Name of this node within some implementation-defined network, if any
    Release level of this operating system
    Version level of this operating system release
    Name of the hardware type on which the system is running"
  (with-alien ((names (struct utsname)))
    (syscall* (#-(or freebsd (and x86 solaris)) "uname"
	       #+(and x86 solaris) "nuname"	; See /usr/include/sys/utsname.h
	       #+freebsd "__xuname" #+freebsd int
	       (* (struct utsname)))
	      (values (cast (slot names 'sysname) c-string)
		      (cast (slot names 'nodename) c-string)
		      (cast (slot names 'release) c-string)
		      (cast (slot names 'version) c-string)
		      (cast (slot names 'machine) c-string))
	      #+freebsd 256
	      (addr names))))

;; EOF
