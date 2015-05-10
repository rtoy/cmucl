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

;; Check the G_BROKEN_FILENAMES environment variable; if set the encoding
;; is locale-dependent...else use :utf-8 on Unicode Lisps.  On 8 bit Lisps
;; it must be set to :iso8859-1 (or left as NIL), making files with
;; non-Latin-1 characters "mojibake", but otherwise they'll be inaccessible.
;; Must be set to NIL initially to enable building Lisp!
(defvar *filename-encoding* nil)

(pushnew :unix *features*)
(pushnew :glibc2 *features*)

;; needed for bootstrap
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %name->file (string)
    `(if *filename-encoding*
	 (string-encode ,string *filename-encoding*)
	 ,string))
  (defmacro %file->name (string)
    `(if *filename-encoding*
	 (string-decode ,string *filename-encoding*)
	 ,string)))

(defconstant +max-u-long+ 4294967295)

(def-alien-type size-t #-alpha unsigned-int #+alpha long)
(def-alien-type time-t long)

(def-alien-type uquad-t #+alpha unsigned-long
		#-alpha (array unsigned-long 2))
(def-alien-type u-int32-t unsigned-int)
(def-alien-type int64-t (signed 64))
(def-alien-type u-int64-t (unsigned 64))

(def-alien-type dev-t #-amd64 uquad-t #+amd64 u-int64-t)
(def-alien-type uid-t unsigned-int)
(def-alien-type gid-t unsigned-int)
(def-alien-type ino-t #-amd64 u-int32-t #+amd64 u-int64-t)
(def-alien-type ino64-t u-int64-t)
(def-alien-type mode-t u-int32-t)
(def-alien-type nlink-t #-amd64 unsigned-int #+amd64 u-int64-t)
(def-alien-type off-t int64-t)
(def-alien-type blkcnt-t u-int64-t)

;;;; Common machine independent structures.


;; Needed early in bootstrap.
(defun unix-current-directory ()
  _N"Put the absolute pathname of the current working directory in BUF.
   If successful, return BUF.  If not, put an error message in
   BUF and return NULL.  BUF should be at least PATH_MAX bytes long."
  ;; 5120 is some randomly selected maximum size for the buffer for getcwd.
  (with-alien ((buf (array c-call:char 5120)))
    (let ((result (alien-funcall
		    (extern-alien "getcwd"
				  (function (* c-call:char)
					    (* c-call:char) c-call:int))
		    (cast buf (* c-call:char))
		    5120)))
      
      (values (not (zerop (sap-int (alien-sap result))))
	      (%file->name (cast buf c-call:c-string))))))

(defconstant o_read    o_rdonly _N"Open for reading")
(defconstant o_write   o_wronly _N"Open for writing")

(defconstant o_rdonly  0 _N"Read-only flag.") 
(defconstant o_wronly  1 _N"Write-only flag.")
(defconstant o_rdwr    2 _N"Read-write flag.")
(defconstant o_accmode 3 _N"Access mode mask.")

#-alpha
(progn
  (defconstant o_creat   #o100 _N"Create if nonexistant flag. (not fcntl)") 
  (defconstant o_excl    #o200 _N"Error if already exists. (not fcntl)")
  (defconstant o_noctty  #o400 _N"Don't assign controlling tty. (not fcntl)")
  (defconstant o_trunc   #o1000 _N"Truncate flag. (not fcntl)")
  (defconstant o_append  #o2000 _N"Append flag.")
  (defconstant o_ndelay  #o4000 _N"Non-blocking I/O")
  (defconstant o_nonblock #o4000 _N"Non-blocking I/O")
  (defconstant o_ndelay  o_nonblock)
  (defconstant o_sync    #o10000 _N"Synchronous writes (on ext2)")
  (defconstant o_fsync    o_sync)
  (defconstant o_async   #o20000 _N"Asynchronous I/O"))
#+alpha
(progn
  (defconstant o_creat   #o1000 _N"Create if nonexistant flag. (not fcntl)") 
  (defconstant o_trunc   #o2000 _N"Truncate flag. (not fcntl)")
  (defconstant o_excl    #o4000 _N"Error if already exists. (not fcntl)")
  (defconstant o_noctty  #o10000 _N"Don't assign controlling tty. (not fcntl)")
  (defconstant o_nonblock #o4 _N"Non-blocking I/O")
  (defconstant o_append  #o10 _N"Append flag.")
  (defconstant o_ndelay  o_nonblock)
  (defconstant o_sync    #o40000 _N"Synchronous writes (on ext2)")
  (defconstant o_fsync    o_sync)
  (defconstant o_async   #o20000 _N"Asynchronous I/O"))

#-alpha
(progn
  (defconstant f-getlk    5   _N"Get lock")
  (defconstant f-setlk    6   _N"Set lock")
  (defconstant f-setlkw   7   _N"Set lock, wait for release")
  (defconstant f-setown   8  _N"Set owner (for sockets)")
  (defconstant f-getown   9  _N"Get owner (for sockets)"))
#+alpha
(progn
  (defconstant f-getlk    7   _N"Get lock")
  (defconstant f-setlk    8   _N"Set lock")
  (defconstant f-setlkw   9   _N"Set lock, wait for release")
  (defconstant f-setown   5  _N"Set owner (for sockets)")
  (defconstant f-getown   6  _N"Get owner (for sockets)"))

(defconstant F-CLOEXEC 1 _N"for f-getfl and f-setfl")
(defun unix-open (path flags mode)
  _N"Unix-open opens the file whose pathname is specified by PATH
   for reading and/or writing as specified by the FLAGS argument.
   Returns an integer file descriptor.
   The flags argument can be:

     o_rdonly        Read-only flag.
     o_wronly        Write-only flag.
     o_rdwr          Read-and-write flag.
     o_append        Append flag.
     o_creat         Create-if-nonexistant flag.
     o_trunc         Truncate-to-size-0 flag.
     o_excl          Error if the file already exists
     o_noctty        Don't assign controlling tty
     o_ndelay        Non-blocking I/O
     o_sync          Synchronous I/O
     o_async         Asynchronous I/O

   If the o_creat flag is specified, then the file is created with
   a permission of argument MODE if the file doesn't exist."
  (declare (type unix-pathname path)
	   (type fixnum flags)
	   (type unix-file-mode mode))
  (int-syscall ("open64" c-string int int) (%name->file path) flags mode))

;;; asm/errno.h
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
       (declaim (simple-vector *unix-errors*)))))

)

(def-unix-error ESUCCESS 0 _N"Successful")
(def-unix-error EPERM 1 _N"Operation not permitted")
(def-unix-error ENOENT 2 _N"No such file or directory")
(def-unix-error ESRCH 3 _N"No such process")
(def-unix-error EINTR 4 _N"Interrupted system call")
(def-unix-error EIO 5 _N"I/O error")
(def-unix-error ENXIO 6 _N"No such device or address")
(def-unix-error E2BIG 7 _N"Arg list too long")
(def-unix-error ENOEXEC 8 _N"Exec format error")
(def-unix-error EBADF 9 _N"Bad file number")
(def-unix-error ECHILD 10 _N"No children")
(def-unix-error EAGAIN 11 _N"Try again")
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
(def-unix-error ENOTTY 25 _N"Not a typewriter")
(def-unix-error ETXTBSY 26 _N"Text file busy")
(def-unix-error EFBIG 27 _N"File too large")
(def-unix-error ENOSPC 28 _N"No space left on device")
(def-unix-error ESPIPE 29 _N"Illegal seek")
(def-unix-error EROFS 30 _N"Read-only file system")
(def-unix-error EMLINK 31 _N"Too many links")
(def-unix-error EPIPE 32 _N"Broken pipe")
;;; 
;;; Math
(def-unix-error EDOM 33 _N"Math argument out of domain")
(def-unix-error ERANGE 34 _N"Math result not representable")
;;; 
(def-unix-error  EDEADLK         35     _N"Resource deadlock would occur")
(def-unix-error  ENAMETOOLONG    36     _N"File name too long")
(def-unix-error  ENOLCK          37     _N"No record locks available")
(def-unix-error  ENOSYS          38     _N"Function not implemented")
(def-unix-error  ENOTEMPTY       39     _N"Directory not empty")
(def-unix-error  ELOOP           40     _N"Too many symbolic links encountered")
(def-unix-error  EWOULDBLOCK     11     _N"Operation would block")
(def-unix-error  ENOMSG          42     _N"No message of desired type")
(def-unix-error  EIDRM           43     _N"Identifier removed")
(def-unix-error  ECHRNG          44     _N"Channel number out of range")
(def-unix-error  EL2NSYNC        45     _N"Level 2 not synchronized")
(def-unix-error  EL3HLT          46     _N"Level 3 halted")
(def-unix-error  EL3RST          47     _N"Level 3 reset")
(def-unix-error  ELNRNG          48     _N"Link number out of range")
(def-unix-error  EUNATCH         49     _N"Protocol driver not attached")
(def-unix-error  ENOCSI          50     _N"No CSI structure available")
(def-unix-error  EL2HLT          51     _N"Level 2 halted")
(def-unix-error  EBADE           52     _N"Invalid exchange")
(def-unix-error  EBADR           53     _N"Invalid request descriptor")
(def-unix-error  EXFULL          54     _N"Exchange full")
(def-unix-error  ENOANO          55     _N"No anode")
(def-unix-error  EBADRQC         56     _N"Invalid request code")
(def-unix-error  EBADSLT         57     _N"Invalid slot")
(def-unix-error  EDEADLOCK       EDEADLK     _N"File locking deadlock error")
(def-unix-error  EBFONT          59     _N"Bad font file format")
(def-unix-error  ENOSTR          60     _N"Device not a stream")
(def-unix-error  ENODATA         61     _N"No data available")
(def-unix-error  ETIME           62     _N"Timer expired")
(def-unix-error  ENOSR           63     _N"Out of streams resources")
(def-unix-error  ENONET          64     _N"Machine is not on the network")
(def-unix-error  ENOPKG          65     _N"Package not installed")
(def-unix-error  EREMOTE         66     _N"Object is remote")
(def-unix-error  ENOLINK         67     _N"Link has been severed")
(def-unix-error  EADV            68     _N"Advertise error")
(def-unix-error  ESRMNT          69     _N"Srmount error")
(def-unix-error  ECOMM           70     _N"Communication error on send")
(def-unix-error  EPROTO          71     _N"Protocol error")
(def-unix-error  EMULTIHOP       72     _N"Multihop attempted")
(def-unix-error  EDOTDOT         73     _N"RFS specific error")
(def-unix-error  EBADMSG         74     _N"Not a data message")
(def-unix-error  EOVERFLOW       75     _N"Value too large for defined data type")
(def-unix-error  ENOTUNIQ        76     _N"Name not unique on network")
(def-unix-error  EBADFD          77     _N"File descriptor in bad state")
(def-unix-error  EREMCHG         78     _N"Remote address changed")
(def-unix-error  ELIBACC         79     _N"Can not access a needed shared library")
(def-unix-error  ELIBBAD         80     _N"Accessing a corrupted shared library")
(def-unix-error  ELIBSCN         81     _N".lib section in a.out corrupted")
(def-unix-error  ELIBMAX         82     _N"Attempting to link in too many shared libraries")
(def-unix-error  ELIBEXEC        83     _N"Cannot exec a shared library directly")
(def-unix-error  EILSEQ          84     _N"Illegal byte sequence")
(def-unix-error  ERESTART        85     _N"Interrupted system call should be restarted _N")
(def-unix-error  ESTRPIPE        86     _N"Streams pipe error")
(def-unix-error  EUSERS          87     _N"Too many users")
(def-unix-error  ENOTSOCK        88     _N"Socket operation on non-socket")
(def-unix-error  EDESTADDRREQ    89     _N"Destination address required")
(def-unix-error  EMSGSIZE        90     _N"Message too long")
(def-unix-error  EPROTOTYPE      91     _N"Protocol wrong type for socket")
(def-unix-error  ENOPROTOOPT     92     _N"Protocol not available")
(def-unix-error  EPROTONOSUPPORT 93     _N"Protocol not supported")
(def-unix-error  ESOCKTNOSUPPORT 94     _N"Socket type not supported")
(def-unix-error  EOPNOTSUPP      95     _N"Operation not supported on transport endpoint")
(def-unix-error  EPFNOSUPPORT    96     _N"Protocol family not supported")
(def-unix-error  EAFNOSUPPORT    97     _N"Address family not supported by protocol")
(def-unix-error  EADDRINUSE      98     _N"Address already in use")
(def-unix-error  EADDRNOTAVAIL   99     _N"Cannot assign requested address")
(def-unix-error  ENETDOWN        100    _N"Network is down")
(def-unix-error  ENETUNREACH     101    _N"Network is unreachable")
(def-unix-error  ENETRESET       102    _N"Network dropped connection because of reset")
(def-unix-error  ECONNABORTED    103    _N"Software caused connection abort")
(def-unix-error  ECONNRESET      104    _N"Connection reset by peer")
(def-unix-error  ENOBUFS         105    _N"No buffer space available")
(def-unix-error  EISCONN         106    _N"Transport endpoint is already connected")
(def-unix-error  ENOTCONN        107    _N"Transport endpoint is not connected")
(def-unix-error  ESHUTDOWN       108    _N"Cannot send after transport endpoint shutdown")
(def-unix-error  ETOOMANYREFS    109    _N"Too many references: cannot splice")
(def-unix-error  ETIMEDOUT       110    _N"Connection timed out")
(def-unix-error  ECONNREFUSED    111    _N"Connection refused")
(def-unix-error  EHOSTDOWN       112    _N"Host is down")
(def-unix-error  EHOSTUNREACH    113    _N"No route to host")
(def-unix-error  EALREADY        114    _N"Operation already in progress")
(def-unix-error  EINPROGRESS     115    _N"Operation now in progress")
(def-unix-error  ESTALE          116    _N"Stale NFS file handle")
(def-unix-error  EUCLEAN         117    _N"Structure needs cleaning")
(def-unix-error  ENOTNAM         118    _N"Not a XENIX named type file")
(def-unix-error  ENAVAIL         119    _N"No XENIX semaphores available")
(def-unix-error  EISNAM          120    _N"Is a named type file")
(def-unix-error  EREMOTEIO       121    _N"Remote I/O error")
(def-unix-error  EDQUOT          122    _N"Quota exceeded")

;;; And now for something completely different ...
(emit-unix-errors)

(def-alien-type nil
    (struct passwd
	    (pw-name (* char))          ; user's login name
	    (pw-passwd (* char))        ; no longer used
	    (pw-uid uid-t)              ; user id
	    (pw-gid gid-t)              ; group id
	    (pw-gecos (* char))         ; typically user's full name
	    (pw-dir (* char))           ; user's home directory
	    (pw-shell (* char))))       ; user's login shell

;;;; System calls.

(def-alien-routine ("os_get_errno" unix-get-errno) int)
(def-alien-routine ("os_set_errno" unix-set-errno) int (newvalue int))
(defun unix-errno () (unix-get-errno))
(defun (setf unix-errno) (newvalue) (unix-set-errno newvalue))

;;; GET-UNIX-ERROR-MSG -- public.
;;; 
(defun get-unix-error-msg (&optional (error-number (unix-errno)))
  _N"Returns a string describing the error number which was returned by a
  UNIX system call."
  (declare (type integer error-number))
  
  (if (array-in-bounds-p *unix-errors* error-number)
      (svref *unix-errors* error-number)
      (format nil (intl:gettext "Unknown error [~d]") error-number)))

(defmacro syscall ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (values nil (unix-errno))
	 ,success-form)))

;;; Like syscall, but if it fails, signal an error instead of returning error
;;; codes.  Should only be used for syscalls that will never really get an
;;; error.
;;;
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (error (intl:gettext "Syscall ~A failed: ~A") ,name (get-unix-error-msg))
	 ,success-form)))

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values result 0) ,@args))

;;; Unix-write accepts a file descriptor, a buffer, an offset, and the
;;; length to write.  It attempts to write len bytes to the device
;;; associated with fd from the the buffer starting at offset.  It returns
;;; the actual number of bytes written.

(defun unix-write (fd buf offset len)
  _N"Unix-write attempts to write a character buffer (buf) of length
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

(defun unix-pipe ()
  _N"Unix-pipe sets up a unix-piping mechanism consisting of
  an input pipe and an output pipe.  Unix-Pipe returns two
  values: if no error occurred the first value is the pipe
  to be read from and the second is can be written to.  If
  an error occurred the first value is NIL and the second
  the unix error code."
  (with-alien ((fds (array int 2)))
    (syscall ("pipe" (* int))
	     (values (deref fds 0) (deref fds 1))
	     (cast fds (* int)))))

;;; UNIX-READ accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer.  It returns the actual number of
;;; bytes read.

(defun unix-read (fd buf len)
  _N"UNIX-READ attempts to read from the file described by fd into
   the buffer buf until it is full.  Len is the length of the buffer.
   The number of bytes actually read is returned or NIL and an error
   number if an error occured."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))
  #+gencgc
  ;; With gencgc, the collector tries to keep raw objects like strings
  ;; in separate pages that are not write-protected.  However, this
  ;; isn't always true.  Thus, BUF will sometimes be write-protected
  ;; and the kernel doesn't like writing to write-protected pages.  So
  ;; go through and touch each page to give the segv handler a chance
  ;; to unprotect the pages.  (This is taken from unix.lisp.)
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
     ;; Touch the beginning of every page
     (do ((sap (int-sap (logand (sap-int sap)
				(logxor 1-page-size (ldb (byte 32 0) -1))))
	       (sap+ sap page-size)))
	 ((sap>= sap end))
       (declare (type system-area-pointer sap))
       (setf (sap-ref-8 sap 0) (sap-ref-8 sap 0)))))
  (int-syscall ("read" int (* char) int) fd buf len))

;;; Unix-getpagesize returns the number of bytes in the system page.

(defun unix-getpagesize ()
  _N"Unix-getpagesize returns the number of bytes in a system page."
  (int-syscall ("getpagesize")))

;;; sys/stat.h

(defmacro extract-stat-results (buf)
  `(values T
           #+(or alpha amd64)
	   (slot ,buf 'st-dev)
           #-(or alpha amd64)
           (+ (deref (slot ,buf 'st-dev) 0)
	      (* (+ +max-u-long+  1)
	         (deref (slot ,buf 'st-dev) 1)))   ;;; let's hope this works..
	   (slot ,buf 'st-ino)
	   (slot ,buf 'st-mode)
	   (slot ,buf 'st-nlink)
	   (slot ,buf 'st-uid)
	   (slot ,buf 'st-gid)
           #+(or alpha amd64)
	   (slot ,buf 'st-rdev)
           #-(or alpha amd64)
           (+ (deref (slot ,buf 'st-rdev) 0)
	      (* (+ +max-u-long+  1)
	         (deref (slot ,buf 'st-rdev) 1)))   ;;; let's hope this works..
	   (slot ,buf 'st-size)
	   (slot ,buf 'st-atime)
	   (slot ,buf 'st-mtime)
	   (slot ,buf 'st-ctime)
	   (slot ,buf 'st-blksize)
	   (slot ,buf 'st-blocks)))

;;; bits/stat.h

(def-alien-type nil
  (struct stat
    (st-dev dev-t)
    #-(or alpha amd64) (st-pad1 unsigned-short)
    (st-ino ino-t)
    #+alpha (st-pad1 unsigned-int)
    #-amd64 (st-mode mode-t)
    (st-nlink  nlink-t)
    #+amd64 (st-mode mode-t)
    (st-uid  uid-t)
    (st-gid  gid-t)
    (st-rdev dev-t)
    #-alpha (st-pad2  unsigned-short)
    (st-size off-t)
    #-alpha (st-blksize unsigned-long)
    #-alpha (st-blocks blkcnt-t)
    (st-atime time-t)
    #-alpha (unused-1 unsigned-long)
    (st-mtime time-t)
    #-alpha (unused-2 unsigned-long)
    (st-ctime time-t)
    #+alpha (st-blocks int)
    #+alpha (st-pad2 unsigned-int)
    #+alpha (st-blksize unsigned-int)
    #+alpha (st-flags unsigned-int)
    #+alpha (st-gen unsigned-int)
    #+alpha (st-pad3 unsigned-int)
    #+alpha (unused-1 unsigned-long)
    #+alpha (unused-2 unsigned-long)
    (unused-3 unsigned-long)
    (unused-4 unsigned-long)
    #-alpha (unused-5 unsigned-long)))

(defun unix-stat (name)
  _N"UNIX-STAT retrieves information about the specified
   file returning them in the form of multiple values.
   See the UNIX Programmer's Manual for a description
   of the values returned.  If the call fails, then NIL
   and an error number is returned instead."
  (declare (type unix-pathname name))
  (when (string= name "")
    (setf name "."))
  (with-alien ((buf (struct stat)))
    (syscall ("stat64" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     (%name->file name) (addr buf))))

(defun unix-fstat (fd)
  _N"UNIX-FSTAT is similar to UNIX-STAT except the file is specified
   by the file descriptor FD."
  (declare (type unix-fd fd))
  (with-alien ((buf (struct stat)))
    (syscall ("fstat64" int (* (struct stat)))
	     (extract-stat-results buf)
	     fd (addr buf))))

(defun unix-lstat (name)
  _N"UNIX-LSTAT is similar to UNIX-STAT except the specified
   file must be a symbolic link."
  (declare (type unix-pathname name))
  (with-alien ((buf (struct stat)))
    (syscall ("lstat64" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     (%name->file name) (addr buf))))

;; Encoding of the file mode.

(defconstant s-ifmt   #o0170000 _N"These bits determine file type.")

;; File types.

(defconstant s-ififo  #o0010000 _N"FIFO")
(defconstant s-ifchr  #o0020000 _N"Character device")
(defconstant s-ifdir  #o0040000 _N"Directory")
(defconstant s-ifblk  #o0060000 _N"Block device")
(defconstant s-ifreg  #o0100000 _N"Regular file")

;; These don't actually exist on System V, but having them doesn't hurt.

(defconstant s-iflnk  #o0120000 _N"Symbolic link.")
(defconstant s-ifsock #o0140000 _N"Socket.")
(defun unix-file-kind (name &optional check-for-links)
  _N"Returns either :file, :directory, :link, :special, or NIL."
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

;; Values for the second argument to access.

;;; Unix-access accepts a path and a mode.  It returns two values the
;;; first is T if the file is accessible and NIL otherwise.  The second
;;; only has meaning in the second case and is the unix errno value.

(defun unix-access (path mode)
  _N"Given a file path (a string) and one of four constant modes,
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
  (void-syscall ("access" c-string int) (%name->file path) mode))

(defconstant l_set 0 _N"set the file pointer")
(defconstant l_incr 1 _N"increment the file pointer")
(defconstant l_xtnd 2 _N"extend the file size")

(defun unix-lseek (fd offset whence)
  _N"UNIX-LSEEK accepts a file descriptor and moves the file pointer ahead
   a certain OFFSET for that file.  WHENCE can be any of the following:

   l_set        Set the file pointer.
   l_incr       Increment the file pointer.
   l_xtnd       Extend the file size.
  "
  (declare (type unix-fd fd)
	   (type (signed-byte 64) offset)
	   (type (integer 0 2) whence))
  (let ((result (alien-funcall
                 (extern-alien "lseek64" (function off-t int off-t int))
                 fd offset whence)))
    (if (minusp result)
        (values nil (unix-errno))
        (values result 0))))
;;; Unix-close accepts a file descriptor and attempts to close the file
;;; associated with it.

(defun unix-close (fd)
  _N"Unix-close takes an integer file descriptor as an argument and
   closes the file associated with it.  T is returned upon successful
   completion, otherwise NIL and an error number."
  (declare (type unix-fd fd))
  (void-syscall ("close" int) fd))

;;; Unix-creat accepts a file name and a mode.  It creates a new file
;;; with name and sets it mode to mode (as for chmod).

(defun unix-creat (name mode)
  _N"Unix-creat accepts a file name and a mode (same as those for
   unix-chmod) and creates a file by that name with the specified
   permission mode.  It returns a file descriptor on success,
   or NIL and an error  number otherwise.

   This interface is made obsolete by UNIX-OPEN."
  
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (int-syscall ("creat64" c-string int) (%name->file name) mode))

(defun unix-resolve-links (pathname)
  _N"Returns the pathname with all symbolic links resolved."
  (declare (simple-string pathname))
  (let ((len (length pathname))
	(pending pathname))
    (declare (fixnum len) (simple-string pending))
    (if (zerop len)
	pathname
	(let ((result (make-string 100 :initial-element (code-char 0)))
	      (fill-ptr 0)
	      (name-start 0))
	  (loop
	    (let* ((name-end (or (position #\/ pending :start name-start) len))
		   (new-fill-ptr (+ fill-ptr (- name-end name-start))))
	      ;; grow the result string, if necessary.  the ">=" (instead of
	      ;; using ">") allows for the trailing "/" if we find this
	      ;; component is a directory.
	      (when (>= new-fill-ptr (length result))
		(let ((longer (make-string (* 3 (length result))
					   :initial-element (code-char 0))))
		  (replace longer result :end1 fill-ptr)
		  (setq result longer)))
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
			   (error (intl:gettext "Error reading link ~S: ~S")
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
		       (when (eq kind :directory)
			 (setf (schar result new-fill-ptr) #\/)
			 (incf new-fill-ptr))
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

(defun unix-gethostname ()
  _N"Unix-gethostname returns the name of the host machine as a string."
  (with-alien ((buf (array char 256)))
    (syscall* ("gethostname" (* char) int)
	      (cast buf c-string)
	      (cast buf (* char)) 256)))

;;; Unix-dup returns a duplicate copy of the existing file-descriptor
;;; passed as an argument.

(defun unix-dup (fd)
  _N"Unix-dup duplicates an existing file descriptor (given as the
   argument) and return it.  If FD is not a valid file descriptor, NIL
   and an error number are returned."
  (declare (type unix-fd fd))
  (int-syscall ("dup" int) fd))

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

(defun unix-exit (&optional (code 0))
  _N"Unix-exit terminates the current process with an optional
   error code.  If successful, the call doesn't return.  If
   unsuccessful, the call returns NIL and an error number."
  (declare (type (signed-byte 32) code))
  (void-syscall ("exit" int) code))

(def-alien-routine ("getuid" unix-getuid) int
  _N"Unix-getuid returns the real user-id associated with the
   current process.")

;;; Unix-chdir accepts a directory name and makes that the
;;; current working directory.

(defun unix-chdir (path)
  _N"Given a file path string, unix-chdir changes the current working 
   directory to the one specified."
  (declare (type unix-pathname path))
  (void-syscall ("chdir" c-string) (%name->file path)))

;;; Unix-chmod accepts a path and a mode and changes the mode to the new mode.

(defun unix-chmod (path mode)
  _N"Given a file path string and a constant mode, unix-chmod changes the
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

  Thus #o444 and (logior unix:readown unix:readgrp unix:readoth)
  are equivalent for 'mode.  The octal-base is familar to Unix users.
  
  It returns T on successfully completion; NIL and an error number
  otherwise."
  (declare (type unix-pathname path)
	   (type unix-file-mode mode))
  (void-syscall ("chmod" c-string int) (%name->file path) mode))

;;; Unix-fchmod accepts a file descriptor ("fd") and a file protection mode
;;; ("mode") and changes the protection of the file described by "fd" to 
;;; "mode".

(defun unix-fchmod (fd mode)
  _N"Given an integer file descriptor and a mode (the same as those
   used for unix-chmod), unix-fchmod changes the permission mode
   for that file to the one specified. T is returned if the call
   was successful."
  (declare (type unix-fd fd)
	   (type unix-file-mode mode))
  (void-syscall ("fchmod" int int) fd mode))

(defun unix-readlink (path)
  _N"Unix-readlink invokes the readlink system call on the file name
  specified by the simple string path.  It returns up to two values:
  the contents of the symbolic link if the call is successful, or
  NIL and the Unix error number."
  (declare (type unix-pathname path))
  (with-alien ((buf (array char 1024)))
    (syscall ("readlink" c-string (* char) int)
	     (let ((string (make-string result)))
	       #-unicode
	       (kernel:copy-from-system-area
		(alien-sap buf) 0
		string (* vm:vector-data-offset vm:word-bits)
		(* result vm:byte-bits))
	       #+unicode
	       (let ((sap (alien-sap buf)))
		 (dotimes (k result)
		   (setf (aref string k) (code-char (sap-ref-8 sap k)))))
	       (%file->name string))
	     (%name->file path) (cast buf (* char)) 1024)))

;;; Unix-unlink accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link.

(defun unix-unlink (name)
  _N"Unix-unlink removes the directory entry for the named file.
   NIL and an error code is returned if the call fails."
  (declare (type unix-pathname name))
  (void-syscall ("unlink" c-string) (%name->file name)))

(defconstant r_ok 4 _N"Test for read permission")
(defconstant w_ok 2 _N"Test for write permission")
(defconstant x_ok 1 _N"Test for execute permission")
(defconstant f_ok 0 _N"Test for presence of file")

(defun unix-fcntl (fd cmd arg)
  _N"Unix-fcntl manipulates file descriptors accoridng to the
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
	   (type (unsigned-byte 32) cmd)
	   (type (unsigned-byte 32) arg))
  (int-syscall ("fcntl" int unsigned-int unsigned-int) fd cmd arg))

;;;; Memory-mapped files

(defconstant +null+ (sys:int-sap 0))

(defconstant prot_read 1)
(defconstant prot_write 2)
(defconstant prot_exec 4)
(defconstant prot_none 0)

(defconstant map_shared 1)
(defconstant map_private 2)
(defconstant map_fixed 16)
(defconstant map_anonymous 32)

(defconstant ms_async 1)
(defconstant ms_sync 4)
(defconstant ms_invalidate 2)

;; The return value from mmap that means mmap failed.
(defconstant map_failed (int-sap (1- (ash 1 vm:word-bits))))

(defun unix-mmap (addr length prot flags fd offset)
  (declare (type (or null system-area-pointer) addr)
	   (type (unsigned-byte 32) length)
           (type (integer 1 7) prot)
	   (type (unsigned-byte 32) flags)
	   (type (or null unix-fd) fd)
	   (type (signed-byte 32) offset))
  ;; Can't use syscall, because the address that is returned could be
  ;; "negative".  Hence we explicitly check for mmap returning
  ;; MAP_FAILED.
  (let ((result
	 (alien-funcall (extern-alien "mmap" (function system-area-pointer
						       system-area-pointer
						       size-t int int int off-t))
			(or addr +null+) length prot flags (or fd -1) offset)))
    (if (sap= result map_failed)
	(values nil (unix-errno))
	(values result 0))))

(defun unix-munmap (addr length)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length))
  (syscall ("munmap" system-area-pointer size-t) t addr length))

(defun unix-msync (addr length flags)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length)
	   (type (signed-byte 32) flags))
  (syscall ("msync" system-area-pointer size-t int) t addr length flags))

;;; Unix-rename accepts two files names and renames the first to the second.

(defun unix-rename (name1 name2)
  _N"Unix-rename renames the file with string name1 to the string
   name2.  NIL and an error code is returned if an error occured."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("rename" c-string c-string)
		(%name->file name1) (%name->file name2)))

;;; Unix-rmdir accepts a name and removes the associated directory.

(defun unix-rmdir (name)
  _N"Unix-rmdir attempts to remove the directory name.  NIL and
   an error number is returned if an error occured."
  (declare (type unix-pathname name))
  (void-syscall ("rmdir" c-string) (%name->file name)))

(def-alien-type fd-mask #-alpha unsigned-long #+alpha unsigned-int)

(defconstant fd-setsize 1024)
(defconstant nfdbits 32)
  
(def-alien-type nil
  (struct fd-set
	  (fds-bits (array fd-mask #.(/ fd-setsize nfdbits)))))

;; not checked for linux...
(defmacro fd-clr (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset nfdbits)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logand (deref (slot ,fd-set 'fds-bits) ,word)
		     (32bit-logical-not
		      (truly-the (unsigned-byte 32) (ash 1 ,bit))))))))

;; not checked for linux...
(defmacro fd-isset (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset nfdbits)
       (logbitp ,bit (deref (slot ,fd-set 'fds-bits) ,word)))))

;; not checked for linux...
(defmacro fd-set (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset nfdbits)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logior (truly-the (unsigned-byte 32) (ash 1 ,bit))
		     (deref (slot ,fd-set 'fds-bits) ,word))))))

;; not checked for linux...
(defmacro fd-zero (fd-set)
  `(progn
     ,@(loop for index upfrom 0 below (/ fd-setsize nfdbits)
	 collect `(setf (deref (slot ,fd-set 'fds-bits) ,index) 0))))

;;; TTY ioctl commands.

(defconstant iocparm-mask #x3fff)
(defconstant ioc_void #x00000000)
(defconstant ioc_out #x40000000)
(defconstant ioc_in #x80000000)
(defconstant ioc_inout (logior ioc_in ioc_out))

(defmacro define-ioctl-command (name dev cmd &optional arg parm-type)
  _N"Define an ioctl command. If the optional ARG and PARM-TYPE are given
  then ioctl argument size and direction are included as for ioctls defined
  by _IO, _IOR, _IOW, or _IOWR. If DEV is a character then the ioctl type
  is the characters code, else DEV may be an integer giving the type."
  (let* ((type (if (characterp dev)
		   (char-code dev)
		   dev))
	 (code (logior (ash type 8) cmd)))
    (when arg
      (setf code `(logior (ash (logand (alien-size ,arg :bytes) ,iocparm-mask)
			       16)
			  ,code)))
    (when parm-type
      (let ((dir (ecase parm-type
		   (:void ioc_void)
		   (:in ioc_in)
		   (:out ioc_out)
		   (:inout ioc_inout))))
	(setf code `(logior ,dir ,code))))
    `(eval-when (eval load compile)
       (defconstant ,name ,code))))

(define-ioctl-command TIOCGWINSZ #\T #x13)
(define-ioctl-command TIOCSWINSZ #\T #x14)
(define-ioctl-command TIOCNOTTY  #\T #x22)
(define-ioctl-command TIOCSPGRP  #\T #x10)
(define-ioctl-command TIOCGPGRP  #\T #x0F)

;;; File ioctl commands.
(define-ioctl-command FIONREAD #\T #x1B)

;;; ioctl-types.h

(def-alien-type nil
  (struct winsize
    (ws-row unsigned-short)		; rows, in characters
    (ws-col unsigned-short)		; columns, in characters
    (ws-xpixel unsigned-short)		; horizontal size, pixels
    (ws-ypixel unsigned-short)))	; veritical size, pixels

(defconstant f-getfl    3  _N"Get file flags")
(defconstant f-setfl    4  _N"Set file flags")

;;; Define some more compatibility macros to be backward compatible with
;;; BSD systems which did not managed to hide these kernel macros. 

(defconstant FAPPEND  o_append _N"depricated stuff")
(defconstant FFSYNC   o_fsync  _N"depricated stuff")
(defconstant FASYNC   o_async  _N"depricated stuff")
(defconstant FNONBLOCK  o_nonblock _N"depricated stuff")
(defconstant FNDELAY  o_ndelay _N"depricated stuff")

(defun unix-mprotect (addr length prot)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length)
           (type (integer 1 7) prot))
  (syscall ("mprotect" system-area-pointer size-t int)
	   t addr length prot))
  
;;;; Lisp types used by syscalls.

(deftype unix-pathname () 'simple-string)
(deftype unix-fd () `(integer 0 ,most-positive-fixnum))

(deftype unix-file-mode () '(unsigned-byte 32))
(deftype unix-pid () '(unsigned-byte 32))
(deftype unix-uid () '(unsigned-byte 32))
(deftype unix-gid () '(unsigned-byte 32))

;;; Operations on Unix Directories.

;;; direntry.h

(def-alien-type nil
  (struct dirent
    #+glibc2.1
    (d-ino ino-t)                       ; inode number of entry
    #-glibc2.1
    (d-ino ino64-t)                     ; inode number of entry
    (d-off off-t)                       ; offset of next disk directory entry
    (d-reclen unsigned-short)		; length of this record
    (d_type unsigned-char)
    (d-name (array char 256))))		; name must be no longer than this

(export '(open-dir read-dir close-dir))

(defstruct (%directory
	     (:constructor make-directory)
	     (:conc-name directory-)
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
			     (%name->file pathname))))
	 (if (zerop (sap-int dir-struct))
	     (values nil (unix-errno))
	     (make-directory :name pathname :dir-struct dir-struct))))
      ((nil)
       (values nil enoent))
      (t
       (values nil enotdir)))))

(defun read-dir (dir)
  (declare (type %directory dir))
  (let ((daddr (alien-funcall (extern-alien "readdir64"
					    (function system-area-pointer
						      system-area-pointer))
			      (directory-dir-struct dir))))
    (declare (type system-area-pointer daddr))
    (if (zerop (sap-int daddr))
	nil
	(with-alien ((dirent (* (struct dirent)) daddr))
	  (values (%file->name (cast (slot dirent 'd-name) c-string))
		  (slot dirent 'd-ino))))))

(defun close-dir (dir)
  (declare (type %directory dir))
  (alien-funcall (extern-alien "closedir"
			       (function void system-area-pointer))
		 (directory-dir-struct dir))
  nil)

(defconstant rusage_self 0 _N"The calling process.")
(defconstant rusage_children -1 _N"Terminated child processes.")
(defconstant rusage_both -2)

(def-alien-type nil
  (struct rusage
    (ru-utime (struct timeval))		; user time used
    (ru-stime (struct timeval))		; system time used.
    (ru-maxrss long)                    ; Maximum resident set size (in kilobytes)
    (ru-ixrss long)			; integral shared memory size
    (ru-idrss long)			; integral unshared data "
    (ru-isrss long)			; integral unshared stack "
    (ru-minflt long)			; page reclaims
    (ru-majflt long)			; page faults
    (ru-nswap long)			; swaps
    (ru-inblock long)			; block input operations
    (ru-oublock long)			; block output operations
    (ru-msgsnd long)			; messages sent
    (ru-msgrcv long)			; messages received
    (ru-nsignals long)			; signals received
    (ru-nvcsw long)			; voluntary context switches
    (ru-nivcsw long)))

(declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
  _N"Like call getrusage, but return only the system and user time, and returns
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
  _N"Unix-getrusage returns information about the resource usage
   of the process specified by who.  Who can be either the
   current process (rusage_self) or all of the terminated
   child processes (rusage_children).  NIL and an error number
   is returned if the call fails."
  (with-alien ((usage (struct rusage)))
    (syscall ("getrusage" int (* (struct rusage)))
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

;;;; Socket support.

;;; Looks a bit naked.

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

(def-alien-routine ("getpeername" unix-getpeername) int
  (socket int)
  (sockaddr (* t))
  (len (* unsigned)))

(def-alien-routine ("getsockname" unix-getsockname) int
  (socket int)
  (sockaddr (* t))
  (len (* unsigned)))

(def-alien-routine ("getsockopt" unix-getsockopt) int
  (socket int)
  (level int)
  (optname int)
  (optval (* t))
  (optlen unsigned :in-out))

(def-alien-routine ("setsockopt" unix-setsockopt) int
  (socket int)
  (level int)
  (optname int)
  (optval (* t))
  (optlen unsigned))

;; Datagram support

(def-alien-routine ("recvfrom" unix-recvfrom) int
  (fd int)
  (buffer c-string)
  (length int)
  (flags int)
  (sockaddr (* t))
  (len int :in-out))

(def-alien-routine ("sendto" unix-sendto) int
  (fd int)
  (buffer c-string)
  (length int)
  (flags int)
  (sockaddr (* t))
  (len int))

(def-alien-routine ("shutdown" unix-shutdown) int
  (socket int)
  (level int))

;;; sys/select.h

;;; UNIX-FAST-SELECT -- public.
;;;
(defmacro unix-fast-select (num-descriptors
			    read-fds write-fds exception-fds
			    timeout-secs &optional (timeout-usecs 0))
  _N"Perform the UNIX select(2) system call."
  (declare (type (integer 0 #.FD-SETSIZE) num-descriptors) 
	   (type (or (alien (* (struct fd-set))) null) 
		 read-fds write-fds exception-fds) 
	   (type (or null (unsigned-byte 31)) timeout-secs) 
	   (type (unsigned-byte 31) timeout-usecs) 
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
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
	 ,@(loop for index upfrom 1 below (/ fd-setsize nfdbits)
	     collect `(setf (deref (slot ,fdset 'fds-bits) ,index) 0)))
       (progn
	 ,@(loop for index upfrom 0 below (/ fd-setsize nfdbits)
	     collect `(setf (deref (slot ,fdset 'fds-bits) ,index)
			    (ldb (byte nfdbits ,(* index nfdbits)) ,num))))))

(defmacro fd-set-to-num (nfds fdset)
  `(if (<= ,nfds nfdbits)
       (deref (slot ,fdset 'fds-bits) 0)
       (+ ,@(loop for index upfrom 0 below (/ fd-setsize nfdbits)
	      collect `(ash (deref (slot ,fdset 'fds-bits) ,index)
			    ,(* index nfdbits))))))

(defun unix-select (nfds rdfds wrfds xpfds to-secs &optional (to-usecs 0))
  _N"Unix-select examines the sets of descriptors passed as arguments
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

(defun unix-symlink (name1 name2)
  _N"Unix-symlink creates a symbolic link named name2 to the file
   named name1.  NIL and an error number is returned if the call
   is unsuccessful."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("symlink" c-string c-string)
		(%name->file name1) (%name->file name2)))

(def-alien-routine ("gethostid" unix-gethostid) unsigned-long
  _N"Unix-gethostid returns a 32-bit integer which provides unique
   identification for the host machine.")

(def-alien-routine ("getpid" unix-getpid) int
  _N"Unix-getpid returns the process-id of the current process.")

(defstruct user-info
  (name "" :type string)
  (password "" :type string)
  (uid 0 :type unix-uid)
  (gid 0 :type unix-gid)
  (gecos "" :type string)
  (dir "" :type string)
  (shell "" :type string))

(defun unix-getpwuid (uid)
  _N"Return a USER-INFO structure for the user identified by UID, or NIL if not found."
  (declare (type unix-uid uid))
  (with-alien ((buf (array c-call:char 1024))
	       (user-info (struct passwd))
               (result (* (struct passwd))))
    (let ((returned
	   (alien-funcall
	    (extern-alien "getpwuid_r"
			  (function c-call:int
                                    c-call:unsigned-int
                                    (* (struct passwd))
                                    (* c-call:char)
                                    c-call:unsigned-int
                                    (* (* (struct passwd)))))
	    uid
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

(declaim (inline unix-gettimeofday))
(defun unix-gettimeofday ()
  _N"If it works, unix-gettimeofday returns 5 values: T, the seconds and
   microseconds of the current time of day, the timezone (in minutes west
   of Greenwich), and a daylight-savings flag.  If it doesn't work, it
   returns NIL and the errno."
  (with-alien ((tv (struct timeval))
	       (tz (struct timezone)))
    (syscall* ("gettimeofday" (* (struct timeval)) 
			      (* (struct timezone)))
	      (values T
		      (slot tv 'tv-sec)
		      (slot tv 'tv-usec)
		      (slot tz 'tz-minuteswest)
		      (slot tz 'tz-dsttime))
	      (addr tv)
	      (addr tz))))

;;; Unix-utimes changes the accessed and updated times on UNIX
;;; files.  The first argument is the filename (a string) and
;;; the second argument is a list of the 4 times- accessed and
;;; updated seconds and microseconds.

(defun unix-utimes (file atime-sec atime-usec mtime-sec mtime-usec)
  _N"Unix-utimes sets the 'last-accessed' and 'last-updated'
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

(def-alien-routine ("ttyname" unix-ttyname) c-string
  (fd int))

(def-alien-routine ("isatty" unix-isatty) boolean
  _N"Accepts a Unix file descriptor and returns T if the device
  associated with it is a terminal."
  (fd int))

;;; pty.h

(defun unix-openpty (name termp winp)
  _N"Create pseudo tty master slave pair with NAME and set terminal
   attributes according to TERMP and WINP and return handles for both
   ends in AMASTER and ASLAVE."
  (with-alien ((amaster int)
	       (aslave int))
    (values
     (int-syscall ("openpty" (* int) (* int) c-string (* (struct termios))
			     (* (struct winsize)))
		  (addr amaster) (addr aslave) name termp winp)
     amaster aslave)))

(def-alien-type nil
  (struct utsname
    (sysname (array char 65))
    (nodename (array char 65))
    (release (array char 65))
    (version (array char 65))
    (machine (array char 65))
    (domainname (array char 65))))

(defun unix-uname ()
  _N"Unix-uname returns the name and information about the current kernel. The
  values returned upon success are: sysname, nodename, release, version,
  machine, and domainname. Upon failure, 'nil and the 'errno are returned."
  (with-alien ((utsname (struct utsname)))
    (syscall* ("uname" (* (struct utsname)))
	      (values (cast (slot utsname 'sysname) c-string)
		      (cast (slot utsname 'nodename) c-string)
		      (cast (slot utsname 'release) c-string)
		      (cast (slot utsname 'version) c-string)
		      (cast (slot utsname 'machine) c-string)
		     (cast (slot utsname 'domainname) c-string))
	      (addr utsname))))

(defun unix-ioctl (fd cmd arg)
  _N"Unix-ioctl performs a variety of operations on open i/o
   descriptors.  See the UNIX Programmer's Manual for more
   information."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) cmd))
  (int-syscall ("ioctl" int unsigned-int (* char)) fd cmd arg))


;;; Unix-mkdir accepts a name and a mode and attempts to create the
;;; corresponding directory with mode mode.

(defun unix-mkdir (name mode)
  _N"Unix-mkdir creates a new directory with the specified name and mode.
   (Same as those for unix-chmod.)  It returns T upon success, otherwise
   NIL and an error number."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("mkdir" c-string int) (%name->file name) mode))

;; A time value that is accurate to the nearest
;; microsecond but also has a range of years.  
(def-alien-type nil
  (struct timeval
	  (tv-sec time-t)	; seconds
	  (tv-usec time-t)))	; and microseconds

;;; sys/time.h

;; Structure crudely representing a timezone.
;;   This is obsolete and should never be used. 
(def-alien-type nil
  (struct timezone
    (tz-minuteswest int)		; minutes west of Greenwich
    (tz-dsttime	int)))			; type of dst correction

;; Type of the second argument to `getitimer' and
;; the second and third arguments `setitimer'. 
(def-alien-type nil
  (struct itimerval
    (it-interval (struct timeval))	; timer interval
    (it-value (struct timeval))))	; current value

(defconstant ITIMER-REAL 0)
(defconstant ITIMER-VIRTUAL 1)
(defconstant ITIMER-PROF 2)

(defun unix-getitimer (which)
  _N"Unix-getitimer returns the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). On success,
   unix-getitimer returns 5 values,
   T, it-interval-secs, it-interval-usec, it-value-secs, it-value-usec."
  (declare (type (member :real :virtual :profile) which)
	   (values t
		   (unsigned-byte 29)(mod 1000000)
		   (unsigned-byte 29)(mod 1000000)))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itv (struct itimerval)))
      (syscall* ("getitimer" int (* (struct itimerval)))
		(values T
			(slot (slot itv 'it-interval) 'tv-sec)
			(slot (slot itv 'it-interval) 'tv-usec)
			(slot (slot itv 'it-value) 'tv-sec)
			(slot (slot itv 'it-value) 'tv-usec))
		which (alien-sap (addr itv))))))

(defun unix-setitimer (which int-secs int-usec val-secs val-usec)
  _N" Unix-setitimer sets the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). A SIGALRM signal
   will be delivered VALUE <seconds+microseconds> from now. INTERVAL,
   when non-zero, is <seconds+microseconds> to be loaded each time
   the timer expires. Setting INTERVAL and VALUE to zero disables
   the timer. See the Unix man page for more details. On success,
   unix-setitimer returns the old contents of the INTERVAL and VALUE
   slots as in unix-getitimer."
  (declare (type (member :real :virtual :profile) which)
	   (type (unsigned-byte 29) int-secs val-secs)
	   (type (integer 0 (1000000)) int-usec val-usec)
	   (values t
		   (unsigned-byte 29)(mod 1000000)
		   (unsigned-byte 29)(mod 1000000)))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itvn (struct itimerval))
		 (itvo (struct itimerval)))
      (setf (slot (slot itvn 'it-interval) 'tv-sec ) int-secs
	    (slot (slot itvn 'it-interval) 'tv-usec) int-usec
	    (slot (slot itvn 'it-value   ) 'tv-sec ) val-secs
	    (slot (slot itvn 'it-value   ) 'tv-usec) val-usec)
      (syscall* ("setitimer" int (* (struct timeval))(* (struct timeval)))
		(values T
			(slot (slot itvo 'it-interval) 'tv-sec)
			(slot (slot itvo 'it-interval) 'tv-usec)
			(slot (slot itvo 'it-value) 'tv-sec)
			(slot (slot itvo 'it-value) 'tv-usec))
		which (alien-sap (addr itvn))(alien-sap (addr itvo))))))


(def-alien-type cc-t unsigned-char)
(def-alien-type speed-t  unsigned-int)
(def-alien-type tcflag-t unsigned-int)

(defconstant +NCCS+ 32
  _N"Size of control character vector.")

(def-alien-type nil
  (struct termios
    (c-iflag tcflag-t)
    (c-oflag tcflag-t)
    (c-cflag tcflag-t)
    (c-lflag tcflag-t)
    (c-line cc-t)
    (c-cc (array cc-t #.+NCCS+))
    (c-ispeed speed-t)
    (c-ospeed speed-t)))

;; c_cc characters

(defmacro def-enum (inc cur &rest names)
  (flet ((defform (name)
	     (prog1 (when name `(defconstant ,name ,cur))
	       (setf cur (funcall inc cur 1)))))
    `(progn ,@(mapcar #'defform names))))

(def-enum + 0 vintr vquit verase
	  vkill veof vtime
	  vmin vswtc vstart
	  vstop vsusp veol
	  vreprint vdiscard vwerase
	  vlnext veol2)
(defvar vdsusp vsusp)

(def-enum + 0 tcsanow tcsadrain tcsaflush)

;; c_iflag bits
(def-enum ash 1 tty-ignbrk tty-brkint tty-ignpar tty-parmrk tty-inpck
	  tty-istrip tty-inlcr tty-igncr tty-icrnl tty-iuclc
	  tty-ixon tty-ixany tty-ixoff 
	  tty-imaxbel)

;; c_oflag bits
(def-enum ash 1 tty-opost tty-olcuc tty-onlcr tty-ocrnl tty-onocr
	  tty-onlret tty-ofill tty-ofdel tty-nldly)

;; c_lflag bits
(def-enum ash 1 tty-isig tty-icanon tty-xcase tty-echo tty-echoe
	  tty-echok tty-echonl tty-noflsh
	  tty-tostop tty-echoctl tty-echoprt
	  tty-echoke tty-flusho
	  tty-pendin tty-iexten)

(defun unix-tcgetattr (fd termios)
  _N"Get terminal attributes."
  (declare (type unix-fd fd))
  (void-syscall ("tcgetattr" int (* (struct termios))) fd termios))

(defun unix-tcsetattr (fd opt termios)
  _N"Set terminal attributes."
  (declare (type unix-fd fd))
  (void-syscall ("tcsetattr" int int (* (struct termios))) fd opt termios))

(defconstant writeown #o200 _N"Write by owner")

;;; termios.h

(defun unix-cfgetospeed (termios)
  _N"Get terminal output speed."
  (multiple-value-bind (speed errno)
      (int-syscall ("cfgetospeed" (* (struct termios))) termios)
    (if speed
	(values (svref terminal-speeds speed) 0)
      (values speed errno))))
