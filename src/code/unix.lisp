;;; -*- Package: UNIX -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/unix.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the UNIX low-level support, just enough to run
;;; CMUCL.
;;;
(in-package "UNIX")

(intl:textdomain "cmucl-unix")

(pushnew :unix *features*)
#+linux
(pushnew :glibc2 *features*)

;; Check the G_BROKEN_FILENAMES environment variable; if set the encoding
;; is locale-dependent...else use :utf-8 on Unicode Lisps.  On 8 bit Lisps
;; it must be set to :iso8859-1 (or left as NIL), making files with
;; non-Latin-1 characters "mojibake", but otherwise they'll be inaccessible.
;; Must be set to NIL initially to enable building Lisp!
(defvar *filename-encoding* :null
  "The encoding to use for converting a namestring to a string that can
  be used by the operations system.  It must be a valid
  external-format name or :NULL.  :NULL means the string
  is passed as is to the operating system.  The operating system will
  get the low 8 bits of each UTF-16 code unit of the string.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %name->file (string)
    `(if (eql *filename-encoding* :null)
	 ,string
	 (string-encode ,string *filename-encoding*)))
  (defmacro %file->name (string)
    `(if (eql *filename-encoding* :null)
	 ,string
	 (string-decode ,string *filename-encoding*))))


;;;; Common machine independent structures.
#+linux
(defconstant +max-u-long+ 4294967295)

(def-alien-type int64-t (signed 64))

(def-alien-type u-int64-t (unsigned 64))

(def-alien-type uquad-t
    #+alpha unsigned-long
    #-alpha (array unsigned-long 2))

(def-alien-type u-int32-t unsigned-int)

(def-alien-type ino-t
    #+(or netbsd linux darwin) u-int64-t
    #+alpha unsigned-int
    #-(or alpha netbsd linux darwin) unsigned-long)

(def-alien-type size-t
    #-(or linux alpha) long
    #+linux unsigned-int 
    #+alpha unsigned-long)

(def-alien-type time-t
    #-(or bsd linux alpha) unsigned-long
    #+linux long
    #+(and bsd (not netbsd)) long
    #+(and bsd netbsd) int64-t
    #+alpha unsigned-int)

#-BSD
(progn
  (deftype file-offset () '(signed-byte 32))
  (def-alien-type off-t
      #-(or alpha linux) long
      #+linux int64-t
      #+alpha unsigned-long)
  (def-alien-type uid-t
      #-(or alpha svr4 linux) unsigned-short
      #+alpha unsigned-int
      #+linux unsigned-int
      #+svr4 long)
  (def-alien-type gid-t
      #-(or alpha svr4 linux) unsigned-short
      #+alpha unsigned-int
      #+linux unsigned-int
      #+svr4 long)
  #+linux
  (def-alien-type blkcnt-t u-int64-t)
)

#+BSD
(progn
  (deftype file-offset () '(signed-byte 64))
  (def-alien-type off-t int64-t)
  (def-alien-type uid-t unsigned-long)
  (def-alien-type gid-t unsigned-long))

(def-alien-type mode-t
    #-(or alpha svr4 linux) unsigned-short
    #+alpha unsigned-int
    #+linux u-int32-t
    #+svr4 unsigned-long)

;; not checked for linux...
(defmacro fd-clr (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logand (deref (slot ,fd-set 'fds-bits) ,word)
		     (32bit-logical-not
		      (truly-the (unsigned-byte 32) (ash 1 ,bit))))))))

;; not checked for linux...
(defmacro fd-isset (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (logbitp ,bit (deref (slot ,fd-set 'fds-bits) ,word)))))

(defconstant fd-setsize
  #-(or hpux alpha linux FreeBSD) 256
  #+hpux 2048 #+alpha 4096 #+(or linux FreeBSD) 1024)

;; not checked for linux...
(def-alien-type nil
  (struct fd-set
    (fds-bits (array #-alpha unsigned-long #+alpha int #.(/ fd-setsize 32)))))

(def-alien-type nil
  (struct timeval
    (tv-sec #-linux time-t #+linux int)		; seconds
    (tv-usec int)))				; and microseconds

#+(or linux BSD)
(def-alien-type nil
  (struct timespec-t
    (ts-sec time-t)
    (ts-nsec long)))

;;; From ioctl.h
(def-alien-type nil
  (struct tchars
    (t-intrc char)			; interrupt
    (t-quitc char)			; quit
    #+linux (t-eofc char)
    (t-startc char)			; start output
    (t-stopc char)			; stop output
    #-linux (t-eofc char)			; end-of-file
    (t-brkc char)))			; input delimiter (like nl)

;; not found (semi) linux
(def-alien-type nil
  (struct ltchars
    #+linux (t-werasc char)			; word erase 	  
    (t-suspc char)			; stop process signal
    (t-dsuspc char)			; delayed stop process signal
    (t-rprntc char)			; reprint line
    (t-flushc char)			; flush output (toggles)
    #-linux (t-werasc char)			; word erase
    (t-lnextc char)))			; literal next character

(def-alien-type nil
  (struct sgttyb
    #+linux (sg-flags #+mach short #-mach int) ; mode flags 	  
    (sg-ispeed char)			; input speed.
    (sg-ospeed char)			; output speed
    (sg-erase char)			; erase character
    #-linux (sg-kill char)			; kill character
    #-linux (sg-flags #+mach short #-mach int) ; mode flags
    #+linux (sg-kill char)
    #+linux (t (struct termios))
    #+linux (check int)))

(def-alien-type nil
  (struct winsize
    (ws-row unsigned-short)		; rows, in characters
    (ws-col unsigned-short)		; columns, in characters
    (ws-xpixel unsigned-short)		; horizontal size, pixels
    (ws-ypixel unsigned-short)))	; veritical size, pixels


;;;; System calls.

(defmacro %syscall ((name (&rest arg-types) result-type)
		    success-form &rest args)
  `(let* ((fn (extern-alien ,name (function ,result-type ,@arg-types)))
	  (result (alien-funcall fn ,@args)))
     (if (eql -1 result)
	 (values nil (unix-errno))
	 ,success-form)))

(defmacro syscall ((name &rest arg-types) success-form &rest args)
  `(%syscall (,name (,@arg-types) int) ,success-form ,@args))

;;; Like syscall, but if it fails, signal an error instead of returing error
;;; codes.  Should only be used for syscalls that will never really get an
;;; error.
;;;
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (eql -1 result)
	 (error _"Syscall ~A failed: ~A" ,name (get-unix-error-msg))
	 ,success-form)))

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values result 0) ,@args))

(defmacro off-t-syscall ((name arg-types) &rest args)
  `(%syscall (,name ,arg-types off-t) (values result 0) ,@args))


;;; Operations on Unix Directories.

(export '(open-dir read-dir close-dir))

(defstruct (%directory
	     (:conc-name directory-)
	     (:constructor make-directory)
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

#-(or solaris (and bsd (not solaris)) linux)
(defun read-dir (dir)
  (declare (type %directory dir))
  (let ((daddr (alien-funcall (extern-alien "readdir"
					    (function system-area-pointer
						      system-area-pointer))
			      (directory-dir-struct dir))))
    (declare (type system-area-pointer daddr))
    (if (zerop (sap-int daddr))
	nil
	(with-alien ((direct (* (struct direct)) daddr))
	  #-(or linux svr4)
	  (let ((nlen (slot direct 'd-namlen))
		(ino (slot direct 'd-ino)))
	    (declare (type (unsigned-byte 16) nlen))
	    (let ((string (make-string nlen)))
	      #-unicode
	      (kernel:copy-from-system-area
	       (alien-sap (addr (slot direct 'd-name))) 0
	       string (* vm:vector-data-offset vm:word-bits)
	       (* nlen vm:byte-bits))
	      #+unicode
	      (let ((sap (alien-sap (addr (slot direct 'd-name)))))
		(dotimes (k nlen)
		  (setf (aref string k)
			(code-char (sap-ref-8 sap k)))))
	      (values (%file->name string) ino)))
	  #+(or linux svr4)
	  (values (%file->name (cast (slot direct 'd-name) c-string))
		  (slot direct 'd-ino))))))

;;; 64-bit readdir for Solaris
#+solaris
(defun read-dir (dir)
  (declare (type %directory dir))
  (let ((daddr (alien-funcall (extern-alien "readdir64"
					    (function system-area-pointer
						      system-area-pointer))
			      (directory-dir-struct dir))))
    (declare (type system-area-pointer daddr))
    (if (zerop (sap-int daddr))
	nil
	(with-alien ((direct (* (struct dirent64)) daddr))
	  #-(or linux svr4)
	  (let ((nlen (slot direct 'd-namlen))
		(ino (slot direct 'd-ino)))
	    (declare (type (unsigned-byte 16) nlen))
	    (let ((string (make-string nlen)))
	      #-unicode
	      (kernel:copy-from-system-area
	       (alien-sap (addr (slot direct 'd-name))) 0
	       string (* vm:vector-data-offset vm:word-bits)
	       (* nlen vm:byte-bits))
	      #+unicode
	      (let ((sap (alien-sap (addr (slot direct 'd-name)))))
		(dotimes (k nlen)
		  (setf (aref string k)
			(code-char (sap-ref-8 sap k)))))
	      (values (%file->name string) ino)))
	  #+(or linux svr4)
	  (values (%file->name (cast (slot direct 'd-name) c-string))
		  (slot direct 'd-ino))))))

#+(and bsd (not solaris))
(defun read-dir (dir)
  (declare (type %directory dir))
  (let ((daddr (alien-funcall (extern-alien "readdir"
					    (function system-area-pointer
						      system-area-pointer))
			      (directory-dir-struct dir))))
    (declare (type system-area-pointer daddr))
    (if (zerop (sap-int daddr))
	nil
	(with-alien ((direct (* (struct direct)) daddr))
	  (let ((nlen (slot direct 'd-namlen))
		(fino (slot direct 'd-fileno)))
	    (declare (type (unsigned-byte #+netbsd 16 #-netbsd 8) nlen)
		     (type (unsigned-byte #+netbsd 64 #-netbsd 32) fino))
	    (let ((string (make-string nlen)))
	      #-unicode
	      (kernel:copy-from-system-area
	       (alien-sap (addr (slot direct 'd-name))) 0
	       string (* vm:vector-data-offset vm:word-bits)
	       (* nlen vm:byte-bits))
	      #+unicode
	      (let ((sap (alien-sap (addr (slot direct 'd-name)))))
		(dotimes (k nlen)
		  (setf (aref string k)
			(code-char (sap-ref-8 sap k)))))
	      (values (%file->name string) fino)))))))

#+linux
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


;; Use getcwd instead of getwd.  But what should we do if the path
;; won't fit?  Try again with a larger size?  We don't do that right
;; now.
(defun unix-current-directory ()
  ;; 5120 is some randomly selected maximum size for the buffer for getcwd.
  (with-alien ((buf (array c-call:char 5120)))
    (let ((result
	   (alien-funcall 
	    (extern-alien "getcwd"
				(function (* c-call:char)
					  (* c-call:char) c-call:int))
	    (cast buf (* c-call:char))
	    5120)))
	
      (values (not (zerop
		    (sap-int (alien-sap result))))
	      (%file->name (cast buf c-call:c-string))))))

;;; Unix-access accepts a path and a mode.  It returns two values the
;;; first is T if the file is accessible and NIL otherwise.  The second
;;; only has meaning in the second case and is the unix errno value.

(defconstant r_ok 4 _N"Test for read permission")
(defconstant w_ok 2 _N"Test for write permission")
(defconstant x_ok 1 _N"Test for execute permission")
(defconstant f_ok 0 _N"Test for presence of file")

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

;;; Unix-chdir accepts a directory name and makes that the
;;; current working directory.

(defun unix-chdir (path)
  _N"Given a file path string, unix-chdir changes the current working 
   directory to the one specified."
  (declare (type unix-pathname path))
  (void-syscall ("chdir" c-string) (%name->file path)))

;;; Unix-chmod accepts a path and a mode and changes the mode to the new mode.

(defconstant setuidexec #o4000 _N"Set user ID on execution")
(defconstant setgidexec #o2000 _N"Set group ID on execution")
(defconstant savetext #o1000 _N"Save text image after execution")
(defconstant readown #o400 _N"Read by owner")
(defconstant writeown #o200 _N"Write by owner")
(defconstant execown #o100 _N"Execute (search directory) by owner")
(defconstant readgrp #o40 _N"Read by group")
(defconstant writegrp #o20 _N"Write by group")
(defconstant execgrp #o10 _N"Execute (search directory) by group")
(defconstant readoth #o4 _N"Read by others")
(defconstant writeoth #o2 _N"Write by others")
(defconstant execoth #o1 _N"Execute (search directory) by others")

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

;;; Unix-lseek accepts a file descriptor, an offset, and whence value.

(defconstant l_set 0 _N"set the file pointer")
(defconstant l_incr 1 _N"increment the file pointer")
(defconstant l_xtnd 2 _N"extend the file size")

#-linux
(defun unix-lseek (fd offset whence)
  _N"Unix-lseek accepts a file descriptor and moves the file pointer ahead
   a certain offset for that file.  Whence can be any of the following:

   l_set        Set the file pointer.
   l_incr       Increment the file pointer.
   l_xtnd       Extend the file size.
  _N"
  (declare (type unix-fd fd)
	   (type file-offset offset)
	   (type (integer 0 2) whence))
  (off-t-syscall ("lseek" (int off-t int)) fd offset whence))

#+linux
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
;;; Unix-mkdir accepts a name and a mode and attempts to create the
;;; corresponding directory with mode mode.

(defun unix-mkdir (name mode)
  _N"Unix-mkdir creates a new directory with the specified name and mode.
   (Same as those for unix-chmod.)  It returns T upon success, otherwise
   NIL and an error number."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("mkdir" c-string int) (%name->file name) mode))

;;; Unix-unlink accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link.

(defun unix-unlink (name)
  _N"Unix-unlink removes the directory entry for the named file.
   NIL and an error code is returned if the call fails."
  (declare (type unix-pathname name))
  (void-syscall ("unlink" c-string) (%name->file name)))

;;; Unix-open accepts a pathname (a simple string), flags, and mode and
;;; attempts to open file with name pathname.

(defconstant o_rdonly 0 _N"Read-only flag.") 
(defconstant o_wronly 1 _N"Write-only flag.")
(defconstant o_rdwr 2   _N"Read-write flag.")
#+(or hpux linux svr4)
(defconstant o_ndelay
  #+linux o_nonblock
  #-linux 4
  _N"Non-blocking I/O")
(defconstant o_append #-linux #o10 #+linux #o2000   _N"Append flag.")
#+(or hpux svr4 linux)
(progn
  (defconstant o_creat #-linux #o400 #+linux #o100 _N"Create if nonexistant flag.") 
  (defconstant o_trunc #o1000  _N"Truncate flag.")
  (defconstant o_excl #-linux #o2000 #+linux #o200 _N"Error if already exists.")
  (defconstant o_noctty #+linux #o400 #+hpux #o400000 #+(or irix solaris) #x800
               _N"Don't assign controlling tty"))
#+(or hpux linux svr4 BSD)
(defconstant o_nonblock
  #+hpux #o200000
  #+(or irix solaris) #x80
  #+BSD #x04
  #+linux #o4000
  _N"Non-blocking mode")
#+BSD
(defconstant o_ndelay o_nonblock) ; compatibility
#+linux
(progn
  (defconstant o_sync #o10000 _N"Synchronous writes (on ext2)")
  (defconstant o_fsync    o_sync)
  (defconstant o_async   #o20000 _N"Asynchronous I/O"))

#-(or hpux svr4 linux)
(progn
  (defconstant o_creat #o1000  _N"Create if nonexistant flag.") 
  (defconstant o_trunc #o2000  _N"Truncate flag.")
  (defconstant o_excl #o4000  _N"Error if already exists."))

(defun unix-open (path flags mode)
  _N"Unix-open opens the file whose pathname is specified by path
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
	   (type fixnum flags)
	   (type unix-file-mode mode))
  (int-syscall (#+(or linux solaris) "open64" #-(or linux solaris) "open"
		  c-string int int)
	       (%name->file path) flags mode))

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
  (int-syscall (#+(or linux solaris) "creat64" #-(or linux solaris) "creat" c-string int)
	       (%name->file name) mode))

;;; Unix-read accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer.  It returns the actual number of
;;; bytes read.

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

;;; Unix-fcntl takes a file descriptor, an integer command
;;; number, and optional command arguments.  It performs
;;; operations on the associated file and/or returns inform-
;;; ation about the file.

;;; Operations performed on file descriptors:

(defconstant F-DUPFD    0  _N"Duplicate a file descriptor")
(defconstant F-GETFD    1  _N"Get file desc. flags")
(defconstant F-SETFD    2  _N"Set file desc. flags")
(defconstant F-GETFL    3  _N"Get file flags")
(defconstant F-SETFL    4  _N"Set file flags")
#-(or linux svr4)
(defconstant F-GETOWN   5  _N"Get owner")
#+svr4
(defconstant F-GETOWN   23  _N"Get owner")
#+linux
(defconstant F-GETLK    5   _N"Get lock")
#-(or linux svr4)
(defconstant F-SETOWN   6  _N"Set owner")
#+svr4
(defconstant F-SETOWN   24  _N"Set owner")
#+linux 
(defconstant F-SETLK    6   _N"Set lock")
#+linux
(defconstant F-SETLKW   7   _N"Set lock, wait for release")
#+linux
(defconstant F-SETOWN   8  _N"Set owner")

;;; File flags for F-GETFL and F-SETFL:

(defconstant FNDELAY
  #+linux o_ndelay
  #+osf1 #o100000
  #-(or linux osf1) #o0004
  _N"Non-blocking reads")
(defconstant FAPPEND  #-linux #o0010 #+linux o_append  _N"Append on each write") 
(defconstant FASYNC   #-(or linux svr4) #o0100 #+svr4 #o10000 #+linux o_async
  _N"Signal pgrp when data ready")
;; doesn't exist in Linux ;-(
#-linux (defconstant FCREAT   #-(or hpux svr4) #o1000 #+(or hpux svr4) #o0400
   _N"Create if nonexistant")
#-linux (defconstant FTRUNC   #-(or hpux svr4) #o2000 #+(or hpux svr4) #o1000
  _N"Truncate to zero length")
#-linux (defconstant FEXCL    #-(or hpux svr4) #o4000 #+(or hpux svr4) #o2000
  _N"Error if already created")

(defun unix-fcntl (fd cmd arg)
  _N"Unix-fcntl manipulates file descriptors according to the
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

#-linux
(defun unix-read (fd buf len)
  _N"Unix-read attempts to read from the file described by fd into
   the buffer buf until it is full.  Len is the length of the buffer.
   The number of bytes actually read is returned or NIL and an error
   number if an error occured."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))
  #+(or sunos gencgc)
  ;; Note: Under sunos we touch each page before doing the read to give
  ;; the segv handler a chance to fix the permissions.  Otherwise,
  ;; read will return EFAULT.  This also bypasses a bug in 4.1.1 in which
  ;; read fails with EFAULT if the page has never been touched even if
  ;; the permissions are okay.
  ;;
  ;; (Is this true for Solaris?)
  ;;
  ;; Also, with gencgc, the collector tries to keep raw objects like
  ;; strings in separate pages that are not write-protected.  However,
  ;; this isn't always true.  Thus, BUF will sometimes be
  ;; write-protected and the kernel doesn't like writing to
  ;; write-protected pages.  So go through and touch each page to give
  ;; the segv handler a chance to unprotect the pages.
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

#+linux
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
		   (setf (aref string k)
			 (code-char (sap-ref-8 sap k)))))
	       (%file->name string))
	     (%name->file path) (cast buf (* char)) 1024)))

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

;;; Unix-write accepts a file descriptor, a buffer, an offset, and the
;;; length to write.  It attempts to write len bytes to the device
;;; associated with fd from the buffer starting at offset.  It returns
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

;;; Unix-ioctl is used to change parameters of devices in a device
;;; dependent way.


(defconstant terminal-speeds
  '#(0 50 75 110 134 150 200 300 600 #+hpux 900 1200 1800 2400 #+hpux 3600
     4800 #+hpux 7200 9600 19200 38400 57600 115200 230400
     #+hpux 460800))

;;; from /usr/include/bsd/sgtty.h (linux)

(defconstant tty-raw #-linux #o40 #+linux 1)
(defconstant tty-crmod #-linux #o20 #+linux 4)
#-(or hpux svr4 bsd linux) (defconstant tty-echo #o10) ;; 8
(defconstant tty-lcase #-linux #o4 #+linux 2)
#-hpux
(defconstant tty-cbreak #-linux #o2 #+linux 64)
#-(or linux hpux)
(defconstant tty-tandem #o1)

#+(or hpux svr4 bsd linux)
(progn
  (defmacro def-enum (inc cur &rest names)
    (flet ((defform (name)
               (prog1 (when name `(defconstant ,name ,cur))
                 (setf cur (funcall inc cur 1)))))
      `(progn ,@(mapcar #'defform names))))

  ;; Input modes. Linux: /usr/include/asm/termbits.h
  (def-enum ash 1 tty-ignbrk tty-brkint tty-ignpar tty-parmrk tty-inpck
            tty-istrip tty-inlcr tty-igncr tty-icrnl #-bsd tty-iuclc
            tty-ixon #-bsd tty-ixany tty-ixoff #+bsd tty-ixany
            #+hpux tty-ienqak #+bsd nil tty-imaxbel)

  ;; output modes
  #-bsd (def-enum ash 1 tty-opost tty-olcuc tty-onlcr tty-ocrnl tty-onocr
                      tty-onlret tty-ofill tty-ofdel #+linux tty-nldly)
  #+bsd (def-enum ash 1 tty-opost tty-onlcr)

  ;; local modes
  #-(or bsd linux) (def-enum ash 1 tty-isig tty-icanon tty-xcase tty-echo tty-echoe
                      tty-echok tty-echonl tty-noflsh #+irix tty-iexten
                      #+(or sunos linux) tty-tostop tty-echoctl tty-echoprt
                      tty-echoke #+(or sunos svr4) tty-defecho tty-flusho
                      #+linux nil tty-pendin #+irix tty-tostop
					     #+(or sunos linux) tty-iexten)
  #+linux
  (def-enum ash 1 tty-isig tty-icanon tty-xcase tty-echo tty-echoe
	  tty-echok tty-echonl tty-noflsh
	  tty-tostop tty-echoctl tty-echoprt
	  tty-echoke tty-flusho
	  tty-pendin tty-iexten)
  #+bsd (def-enum ash 1 tty-echoke tty-echoe tty-echok tty-echo tty-echonl
                      tty-echoprt tty-echoctl tty-isig tty-icanon nil
                      tty-iexten)
  #+bsd (defconstant tty-tostop #x00400000)
  #+bsd (defconstant tty-flusho #x00800000)
  #+bsd (defconstant tty-pendin #x20000000)
  #+bsd (defconstant tty-noflsh #x80000000)
  #+hpux (defconstant tty-tostop #o10000000000)
  #+hpux (defconstant tty-iexten #o20000000000)

  ;; control modes
  (def-enum ash #-bsd #o100 #+bsd #x400 #+hpux nil tty-cstopb
            tty-cread tty-parenb tty-parodd tty-hupcl tty-clocal
            #+svr4 rcv1en #+svr4 xmt1en #+(or hpux svr4) tty-loblk)

  ;; special control characters
  #+(or hpux svr4 linux) (def-enum + 0 vintr vquit verase vkill veof
                                   #-linux veol #-linux veol2)
  #+bsd (def-enum + 0 veof veol veol2 verase nil vkill nil nil vintr vquit)
  #+linux (defconstant veol 11)
  #+linux (defconstant veol2 16)
  
  (defconstant tciflush 0)
  (defconstant tcoflush 1)
  (defconstant tcioflush 2))

#+bsd
(progn
  (defconstant vmin 16)
  (defconstant vtime 17)
  (defconstant vsusp 10)
  (defconstant vstart 12)
  (defconstant vstop 13)
  (defconstant vdsusp 11))

#+hpux
(progn
  (defconstant vmin 11)
  (defconstant vtime 12)
  (defconstant vsusp 13)
  (defconstant vstart 14)
  (defconstant vstop 15)
  (defconstant vdsusp 21))

#+(or hpux bsd linux)
(progn
  (defconstant tcsanow 0)
  (defconstant tcsadrain 1)
  (defconstant tcsaflush 2))

#+(or linux svr4)
(progn
  #-linux (defconstant vdsusp 11)
  (defconstant vstart 8)
  (defconstant vstop 9)
  (defconstant vsusp 10)
  (defconstant vmin #-linux 4 #+linux 6)
  (defconstant vtime 5))

#+(or sunos svr4)
(progn
  ;; control modes
  (defconstant tty-cbaud #o17)
  (defconstant tty-csize #o60)
  (defconstant tty-cs5 #o0)
  (defconstant tty-cs6 #o20)
  (defconstant tty-cs7 #o40)
  (defconstant tty-cs8 #o60))

#+bsd
(progn
  ;; control modes
  (defconstant tty-csize #x300)
  (defconstant tty-cs5 #x000)
  (defconstant tty-cs6 #x100)
  (defconstant tty-cs7 #x200)
  (defconstant tty-cs8 #x300))

#+svr4
(progn
  (defconstant tcsanow #x540e)
  (defconstant tcsadrain #x540f)
  (defconstant tcsaflush #x5410))

(eval-when (compile load eval)

#-(or (and svr4 (not irix)) linux)
(progn
 (defconstant iocparm-mask #x7f) ; Freebsd: #x1fff ?
 (defconstant ioc_void #x20000000)
 (defconstant ioc_out #x40000000)
 (defconstant ioc_in #x80000000)
 (defconstant ioc_inout (logior ioc_in ioc_out)))

#-(or linux (and svr4 (not irix)))
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

#+(and svr4 (not irix))
(defmacro define-ioctl-command (name dev cmd arg &optional (parm-type :void))
  (declare (ignore dev arg parm-type))
  `(eval-when (eval load compile)
     (defconstant ,name ,(logior (ash (char-code #\t) 8) cmd))))

#+linux
(progn
  (defconstant iocparm-mask #x3fff)
  (defconstant ioc_void #x00000000)
  (defconstant ioc_out #x40000000)
  (defconstant ioc_in #x80000000)
  (defconstant ioc_inout (logior ioc_in ioc_out)))

#+linux
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

)

;;; TTY ioctl commands.

#-linux
(progn
  (define-ioctl-command TIOCGETP #\t #-linux 8 #+linux #x81 (struct sgttyb) :out)
  (define-ioctl-command TIOCSETP #\t #-linux 9 #+linux #x82 (struct sgttyb) :in)
  (define-ioctl-command TIOCFLUSH #\t #-linux 16 #+linux #x89 int :in)
  (define-ioctl-command TIOCSETC #\t #-linux 17 #+linux #x84 (struct tchars) :in)
  (define-ioctl-command TIOCGETC #\t #-linux 18 #+linux #x83 (struct tchars) :out)
  (define-ioctl-command TIOCGWINSZ #\t #-hpux 104 #+hpux 107 (struct winsize)
    :out)
  (define-ioctl-command TIOCSWINSZ #\t #-hpux 103 #+hpux 106 (struct winsize)
    :in)

  (define-ioctl-command TIOCNOTTY #\t #-linux 113 #+linux #x22 nil :void))
#-(or hpux linux)
(progn
  (define-ioctl-command TIOCSLTC #\t #-linux 117 #+linux #x84 (struct ltchars) :in)
  (define-ioctl-command TIOCGLTC #\t #-linux 116 #+linux #x85 (struct ltchars) :out)
  (define-ioctl-command TIOCSPGRP #\t #-svr4 118 #+svr4 21 int :in)
  (define-ioctl-command TIOCGPGRP #\t #-svr4 119 #+svr4 20 int :out))
#+hpux
(progn
  (define-ioctl-command TIOCSLTC #\T 23 (struct ltchars) :in)
  (define-ioctl-command TIOCGLTC #\T 24 (struct ltchars) :out)
  (define-ioctl-command TIOCSPGRP #\T 29 int :in)
  (define-ioctl-command TIOCGPGRP #\T 30 int :out)
  (define-ioctl-command TIOCSIGSEND #\t 93 nil))
#+linux
(progn
  (define-ioctl-command TIOCGWINSZ #\T #x13)
  (define-ioctl-command TIOCSWINSZ #\T #x14)
  (define-ioctl-command TIOCNOTTY  #\T #x22)
  (define-ioctl-command TIOCSPGRP  #\T #x10)
  (define-ioctl-command TIOCGPGRP  #\T #x0F))

;;; File ioctl commands.
#-linux
(define-ioctl-command FIONREAD #\f #-linux 127 #+linux #x1B int :out)
#+linux
(define-ioctl-command FIONREAD #\T #x1B)


(defun unix-ioctl (fd cmd arg)
  _N"Unix-ioctl performs a variety of operations on open i/o
   descriptors.  See the UNIX Programmer's Manual for more
   information."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) cmd))
  (int-syscall ("ioctl" int unsigned-int (* char)) fd cmd arg))

(defun unix-tcgetattr (fd termios)
  _N"Get terminal attributes."
  (declare (type unix-fd fd))
  (void-syscall ("tcgetattr" int (* (struct termios))) fd termios))

(defun unix-tcsetattr (fd opt termios)
  _N"Set terminal attributes."
  (declare (type unix-fd fd))
  (void-syscall ("tcsetattr" int int (* (struct termios))) fd opt termios))

;; XXX rest of functions in this progn probably are present in linux, but
;; not verified.
#-bsd
(defun unix-cfgetospeed (termios)
  _N"Get terminal output speed."
  (multiple-value-bind (speed errno)
      (int-syscall ("cfgetospeed" (* (struct termios))) termios)
    (if speed
        (values (svref terminal-speeds speed) 0)
        (values speed errno))))

#+bsd
(defun unix-cfgetospeed (termios)
  _N"Get terminal output speed."
  (int-syscall ("cfgetospeed" (* (struct termios))) termios))

(def-alien-routine ("getuid" unix-getuid) int
  _N"Unix-getuid returns the real user-id associated with the
   current process.")

(defun unix-gethostname ()
  _N"Unix-gethostname returns the name of the host machine as a string."
  (with-alien ((buf (array char 256)))
    (syscall* ("gethostname" (* char) int)
	      (cast buf c-string)
	      (cast buf (* char)) 256)))

(def-alien-routine ("gethostid" unix-gethostid) unsigned-long
  _N"Unix-gethostid returns a 32-bit integer which provides unique
   identification for the host machine.")

;;; Unix-exit terminates a program.

(defun unix-exit (&optional (code 0))
  _N"Unix-exit terminates the current process with an optional
   error code.  If successful, the call doesn't return.  If
   unsuccessful, the call returns NIL and an error number."
  (declare (type (signed-byte 32) code))
  (void-syscall ("exit" int) code))

;;; From sys/termios.h

;;; NOTE: There is both a  termio (SYSV) and termios (POSIX)
;;; structure with similar but incompatible definitions. It may be that
;;; the non-BSD variant of termios below is really a termio but I (pw)
;;; can't verify. The BSD variant uses the Posix termios def. Some systems
;;; (Ultrix and OSF1) seem to support both if used independently.
;;; The 17f version of this seems a bit confused wrt the conditionals.
;;; Please check these defs for your system.

;;; TSM: from what I can tell looking at the 17f definition, my guess is that it
;;; was originally a termio for sunos (nonsolaris) (because it had the c-line
;;; member for sunos only), and then was mutated into the termios definition for
;;; later systems. The definition here is definitely not an IRIX termio because
;;; it doesn't have c-line. In any case, the functions tcgetattr, etc.,
;;; definitely take a termios, and termios seems to be the more standard
;;; standard now, so my suggestion is to just go with termios and forget about
;;; termio. Note the SVID says NCCS not NCC for the constant here, so I've
;;; changed it (which means you need to bootstrap it to avoid a reader error).

;;; On top of all that, SGI decided to change the termios structure on irix
;;; 6.[34] (but NOT 6.2), left the old routines named the same in the library,
;;; but introduced static functions in termios.h to redirect new calls to the
;;; new library--which means it's important not to #include termios.h before
;;; undefineds.h when building lisp.

(defconstant +NCCS+
  #+hpux 16
  #+irix 23
  #+solaris 19
  #+(or bsd osf1) 20
  #+linux 32
  #+(and sunos (not svr4)) 17
  _N"Size of control character vector.")

#-linux
(def-alien-type nil
  (struct termios
    (c-iflag unsigned-int)
    (c-oflag unsigned-int)
    (c-cflag unsigned-int)
    (c-lflag unsigned-int)
    #+(or linux hpux (and sunos (not svr4)))
    (c-reserved #-(or linux (and sunos (not svr4))) unsigned-int
		#+(or linux (and sunos (not svr4))) unsigned-char)
    (c-cc (array unsigned-char #.+NCCS+))
    #+(or bsd osf1) (c-ispeed unsigned-int)
    #+(or bsd osf1) (c-ospeed unsigned-int)))

#+linux
(progn
  (def-alien-type cc-t unsigned-char)
  (def-alien-type speed-t  unsigned-int)
  (def-alien-type tcflag-t unsigned-int))

#+linux
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


;;; From sys/dir.h
;;;
;;; (For Solaris, this is not struct direct, but struct dirent!)
#-(or bsd linux netbsd)
(def-alien-type nil
  (struct direct
    #+(and sunos (not svr4)) (d-off long) ; offset of next disk directory entry
    (d-ino ino-t); inode number of entry
    #+(or linux svr4) (d-off long)
    (d-reclen unsigned-short)		; length of this record
    #-(or linux svr4)
    (d-namlen unsigned-short)		; length of string in d-name
    (d-name (array char 256))))		; name must be no longer than this

#+(and bsd (not netbsd))
(def-alien-type nil
  (struct direct
    (d-fileno unsigned-long)
    (d-reclen unsigned-short)
    (d-type unsigned-char)
    (d-namlen unsigned-char)		; length of string in d-name
    (d-name (array char 256))))		; name must be no longer than this

#+netbsd
(def-alien-type nil
  (struct direct
    (d-fileno ino-t)
    (d-reclen unsigned-short)
    (d-namlen unsigned-short)
    (d-type unsigned-char)
    (d-name (array char 512))))

#+linux
(def-alien-type nil
  (struct dirent
    #+glibc2.1
    (d-ino ino-t)                       ; inode number of entry
    #-glibc2.1
    (d-ino ino-t)                     ; inode number of entry
    (d-off off-t)                       ; offset of next disk directory entry
    (d-reclen unsigned-short)		; length of this record
    (d_type unsigned-char)
    (d-name (array char 256))))		; name must be no longer than this


#+(or linux svr4)
; High-res time.  Actually posix definition under svr4 name.
(def-alien-type nil
  (struct timestruc-t
    (tv-sec time-t)
    (tv-nsec long)))


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

;;; The 64-bit version of struct dirent.
#+solaris
(def-alien-type nil
  (struct dirent64
    (d-ino ino64-t); inode number of entry
    (d-off off64-t) ; offset of next disk directory entry
    (d-reclen unsigned-short)		; length of this record
    (d-name (array char 256))))		; name must be no longer than this


;; unix-stat and friends
(macrolet
    ((call-stat (c-func-name first-arg-type first-arg)
       ;; Call the stat function named C-FUNC-NAME.  The type of the
       ;; first arg is FIRST-ARG-TYPE and FIRST-ARG is the first arg
       ;; to the stat function.  fstat is different from stat and
       ;; lstat since it takes an fd for the first arg instead of
       ;; string.
       `(with-alien ((dev c-call:long-long)
		     (ino c-call:unsigned-long-long)
		     (mode c-call:unsigned-int)
		     (nlink c-call:unsigned-long-long)
		     (uid c-call:unsigned-int)
		     (gid c-call:unsigned-int)
		     (rdev c-call:unsigned-long-long)
		     (size c-call:long-long)
		     (atime c-call:long-long)
		     (mtime c-call:long-long)
		     (ctime c-call:long-long)
		     (blksize c-call:long)
		     (blocks c-call:long-long))
	  (let ((result
		  (alien-funcall
		   (extern-alien ,c-func-name
				 (function int
					   ,first-arg-type
					   (* c-call:long-long)
					   (* c-call:unsigned-long-long)
					   (* c-call:unsigned-int)
					   (* c-call:unsigned-long-long)
					   (* c-call:unsigned-int)
					   (* c-call:unsigned-int)
					   (* c-call:unsigned-long-long)
					   (* c-call:long-long)
					   (* c-call:long-long)
					   (* c-call:long-long)
					   (* c-call:long-long)
					   (* c-call:long)
					   (* c-call:long-long)))
		   ,first-arg
		   (addr dev)
		   (addr ino)
		   (addr mode)
		   (addr nlink)
		   (addr uid)
		   (addr gid)
		   (addr rdev)
		   (addr size)
		   (addr atime)
		   (addr mtime)
		   (addr ctime)
		   (addr blksize)
		   (addr blocks))))
	    (if (eql -1 result)
		(values nil (unix-errno))
		(values t
			dev
			ino
			mode
			nlink
			uid
			gid
			rdev
			size
			atime
			mtime
			ctime
			blksize
			blocks))))))
  (defun unix-stat (name)
    _N"Unix-stat retrieves information about the specified
   file returning them in the form of multiple values.  If the call
   fails, then NIL and an error number is returned.  If the call
   succeeds, then T is returned in addition to the following values
   from the stat struct st:

     st_dev        Device ID
     st_ino        File serial number
     st_mode       Mode of file
     st_nlink      Number of hard links to the file
     st_uid        User ID
     st_gid        Group ID
     st_rdev       Device ID (if file is character or block special)
     st_atime      Last data access time, in sec
     st_mtime      Last data modification time, in sec
     st_ctime      Last file status change time, in sec
     st_blksize    Preferred I/O block size
     st_blocks     Number of blocks allocated. (Block size is implementation dependent.)
"
    (declare (type unix-pathname name))
    (when (string= name "")
      (setf name "."))
    (call-stat "os_stat" c-call:c-string (%name->file name)))

  (defun unix-lstat (name)
    "Unix-lstat is similar to unix-stat except the specified
   file must be a symbolic link.  If the call fails, then NIL and an
   error number is returned.  If the call succeeds, then T is returned
   in addition to the following values from the stat struct st:

     st_dev        Device ID
     st_ino        File serial number
     st_mode       Mode of file
     st_nlink      Number of hard links to the file
     st_uid        User ID
     st_gid        Group ID
     st_rdev       Device ID (if file is character or block special)
     st_atime      Last data access time, in sec
     st_mtime      Last data modification time, in sec
     st_ctime      Last file status change time, in sec
     st_blksize    Preferred I/O block size
     st_blocks     Number of blocks allocated. (Block size is implementation dependent.)
"
    (declare (type unix-pathname name))
    (call-stat "os_lstat" c-call:c-string (%name->file name)))

  (defun unix-fstat (fd)
    _N"Unix-fstat is similar to unix-stat except the file is specified
   by the file descriptor fd.  If the call fails, then NIL and an
   error number is returned.  If the call succeeds, then T is returned
   in addition to the following values from the stat struct st:

     st_dev        Device ID
     st_ino        File serial number
     st_mode       Mode of file
     st_nlink      Number of hard links to the file
     st_uid        User ID
     st_gid        Group ID
     st_rdev       Device ID (if file is character or block special)
     st_atime      Last data access time, in sec
     st_mtime      Last data modification time, in sec
     st_ctime      Last file status change time, in sec
     st_blksize    Preferred I/O block size
     st_blocks     Number of blocks allocated. (Block size is implementation dependent.)
"
    (declare (type unix-fd fd))
    (call-stat "os_fstat" int fd)))

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

(defconstant rusage_self 0 _N"The calling process.")
(defconstant rusage_children -1 _N"Terminated child processes.")

(declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
  _N"Like call getrusage, but return only the system and user time, and returns
   the seconds and microseconds as separate values."
  (declare (values (member t)
		   (unsigned-byte 31) (mod 1000000)
		   (unsigned-byte 31) (mod 1000000)))
  (with-alien ((usage (struct rusage)))
    (syscall* (#-netbsd "getrusage" #+netbsd "__getrusage50" int (* (struct rusage)))
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
    (syscall (#-netbsd "getrusage" #+netbsd "__getrusage50" int (* (struct rusage)))
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

;;;; Support routines for dealing with unix pathnames.

(defconstant s-ifmt   #o0170000 _N"These bits determine file type.")
(defconstant s-ifdir  #o0040000 _N"Directory")
(defconstant s-ifchr  #o0020000 _N"Character device")
#+linux
(defconstant s-ififo  #o0010000 _N"FIFO")
(defconstant s-ifblk  #o0060000 _N"Block device")
(defconstant s-ifreg  #o0100000 _N"Regular file")
(defconstant s-iflnk  #o0120000 _N"Symbolic link.")
(defconstant s-ifsock #o0140000 _N"Socket.")
(defconstant s-isuid #o0004000)
(defconstant s-isgid #o0002000)
(defconstant s-isvtx #o0001000)
(defconstant s-iread #o0000400)
(defconstant s-iwrite #o0000200)
(defconstant s-iexec #o0000100)

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
       (declaim (simple-vector *unix-errors*)))))

) ;eval-when

;;; 
;;; From <errno.h>
;;; 
(def-unix-error ESUCCESS 0 _N"Successful")
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
#+linux
(progn
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
)

;;;
;;; And now for something completely different ...
(emit-unix-errors)

(def-alien-routine ("os_get_errno" unix-get-errno) int)
(def-alien-routine ("os_set_errno" unix-set-errno) int (newvalue int))
(defun unix-errno () (unix-get-errno))
#+linux
(defun (setf unix-errno) (newvalue) (unix-set-errno newvalue))

;;; GET-UNIX-ERROR-MSG -- public.
;;; 
(defun get-unix-error-msg (&optional (error-number (unix-errno)))
  _N"Returns a string describing the error number which was returned by a
  UNIX system call."
  (declare (type integer error-number))
  (if (array-in-bounds-p *unix-errors* error-number)
      (svref *unix-errors* error-number)
      (format nil _"Unknown error [~d]" error-number)))


;;;; Lisp types used by syscalls.

(deftype unix-pathname () 'simple-string)
(deftype unix-fd () `(integer 0 ,most-positive-fixnum))

(deftype unix-file-mode () '(unsigned-byte 32))
(deftype unix-uid () '(unsigned-byte 32))
(deftype unix-gid () '(unsigned-byte 32))


;;; UNIX-FAST-SELECT -- public.
;;;
(defmacro unix-fast-select (num-descriptors
			    read-fds write-fds exception-fds
			    timeout-secs &optional (timeout-usecs 0))
  _N"Perform the UNIX select(2) system call.
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
       (int-syscall (#-netbsd "select" #+netbsd "__select50" int (* (struct fd-set)) (* (struct fd-set))
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

;; not checked for linux...
(defmacro fd-set (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logior (truly-the (unsigned-byte 32) (ash 1 ,bit))
		     (deref (slot ,fd-set 'fds-bits) ,word))))))

;; not checked for linux...
(defmacro fd-zero (fd-set)
  `(progn
     ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	 collect `(setf (deref (slot ,fd-set 'fds-bits) ,index) 0))))

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
      (syscall (#-netbsd "select" #+netbsd "__select50" int (* (struct fd-set)) (* (struct fd-set))
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

(def-alien-type nil
  (struct timeval
    (tv-sec time-t)			; seconds
    (tv-usec #-linux int 
             #+linux time-t))) ; and microseconds

(def-alien-type nil
  (struct timezone
    (tz-minuteswest int)		; minutes west of Greenwich
    (tz-dsttime				; type of dst correction
     #-linux (enum nil :none :usa :aust :wet :met :eet :can)
     #+linux int)))

(declaim (inline unix-gettimeofday))
(defun unix-gettimeofday ()
  _N"If it works, unix-gettimeofday returns 5 values: T, the seconds and
   microseconds of the current time of day, the timezone (in minutes west
   of Greenwich), and a daylight-savings flag.  If it doesn't work, it
   returns NIL and the errno."
  (with-alien ((tv (struct timeval))
	       #-(or svr4 netbsd) (tz (struct timezone)))
    (syscall* (#-netbsd "gettimeofday"
	       #+netbsd  "__gettimeofday50"
	       (* (struct timeval)) #-svr4 (* (struct timezone)))
	      (values T
		      (slot tv 'tv-sec)
		      (slot tv 'tv-usec)
		      #-(or svr4 netbsd) (slot tz 'tz-minuteswest)
		      #+svr4 (unix-get-minutes-west (slot tv 'tv-sec))
		      #-(or svr4 netbsd) (slot tz 'tz-dsttime)
		      #+svr4 (unix-get-timezone (slot tv 'tv-sec))
		      )
	      (addr tv)
	      #-(or svr4 netbsd) (addr tz) #+netbsd nil)))

;;; Unix-utimes changes the accessed and updated times on UNIX
;;; files.  The first argument is the filename (a string) and
;;; the second argument is a list of the 4 times- accessed and
;;; updated seconds and microseconds.

#-hpux
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
    (void-syscall (#-netbsd "utimes" #+netbsd "__utimes50" c-string (* (struct timeval)))
		  file
		  (cast tvp (* (struct timeval))))))

(def-alien-routine ("getpid" unix-getpid) int
  _N"Unix-getpid returns the process-id of the current process.")


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

(defun unix-recvfrom (fd buffer length flags sockaddr len)
  (with-alien ((l c-call:int len))
    (values
     (alien-funcall (extern-alien "recvfrom"
				  (function c-call:int
					    c-call:int
					    system-area-pointer
					    c-call:int
					    c-call:int
					    (* t)
					    (* c-call:int)))
		    fd
		    (system:vector-sap buffer)
		    length
		    flags
		    sockaddr
		    (addr l))
     l)))

#-unicode
(def-alien-routine ("sendto" unix-sendto) int
  (fd int)
  (buffer c-string)
  (length int)
  (flags int)
  (sockaddr (* t))
  (len int))

(defun unix-sendto (fd buffer length flags sockaddr len)
  (alien-funcall (extern-alien "sendto"
			       (function c-call:int
					 c-call:int
					 system-area-pointer
					 c-call:int
					 c-call:int
					 (* t)
					 c-call:int))
		 fd
		 (system:vector-sap buffer)
		 length
		 flags
		 sockaddr
		 len))

(def-alien-routine ("shutdown" unix-shutdown) int
  (socket int)
  (level int))


;;;; Memory-mapped files

(defconstant +null+ (sys:int-sap 0))

(defconstant prot_read 1)		; Readable
(defconstant prot_write 2)		; Writable
(defconstant prot_exec 4)		; Executable
(defconstant prot_none 0)		; No access

(defconstant map_shared 1)		; Changes are shared
(defconstant map_private 2)		; Changes are private
(defconstant map_fixed 16)		; Fixed, user-defined address
(defconstant map_noreserve #x40)	; Don't reserve swap space
(defconstant map_anonymous
  #+solaris #x100			; Solaris
  #+linux 32				; Linux
  #+bsd #x1000)

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
	   (type file-offset offset))
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

(defun unix-mprotect (addr length prot)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length)
           (type (integer 1 7) prot))
  (syscall ("mprotect" system-area-pointer size-t int)
	   t addr length prot))
  
(defun unix-msync (addr length flags)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length)
	   (type (signed-byte 32) flags))
  (syscall ("msync" system-area-pointer size-t int) t addr length flags))


;;;; User and group database structures

(defstruct user-info
  (name "" :type string)
  (password "" :type string)
  (uid 0 :type unix-uid)
  (gid 0 :type unix-gid)
  #+solaris (age "" :type string)
  #+solaris (comment "" :type string)
  #+freebsd (change -1 :type fixnum)
  (gecos "" :type string)
  (dir "" :type string)
  (shell "" :type string))

;; see <pwd.h>
#+solaris
(def-alien-type nil
    (struct passwd
	    (pw-name (* char))          ; user's login name
	    (pw-passwd (* char))        ; no longer used
	    (pw-uid uid-t)              ; user id
	    (pw-gid gid-t)              ; group id
	    (pw-age (* char))           ; password age (not used)
	    (pw-comment (* char))       ; not used
	    (pw-gecos (* char))         ; typically user's full name
	    (pw-dir (* char))           ; user's home directory
	    (pw-shell (* char))))       ; user's login shell

#+bsd
(def-alien-type nil
    (struct passwd
	    (pw-name (* char))          ; user's login name
	    (pw-passwd (* char))        ; no longer used
	    (pw-uid uid-t)              ; user id
	    (pw-gid gid-t)              ; group id
            (pw-change int)             ; password change time
            (pw-class (* char))         ; user access class
	    (pw-gecos (* char))         ; typically user's full name
	    (pw-dir (* char))           ; user's home directory
	    (pw-shell (* char))         ; user's login shell
            (pw-expire int)             ; account expiration
            #+(or freebsd darwin)
	    (pw-fields int)))           ; internal
#+linux
(def-alien-type nil
    (struct passwd
	    (pw-name (* char))          ; user's login name
	    (pw-passwd (* char))        ; no longer used
	    (pw-uid uid-t)              ; user id
	    (pw-gid gid-t)              ; group id
	    (pw-gecos (* char))         ; typically user's full name
	    (pw-dir (* char))           ; user's home directory
	    (pw-shell (* char))))       ; user's login shell


;;;; Other random routines.
(def-alien-routine ("isatty" unix-isatty) boolean
  _N"Accepts a Unix file descriptor and returns T if the device
  associated with it is a terminal."
  (fd int))

(def-alien-routine ("ttyname" unix-ttyname) c-string
  (fd int))

(def-alien-routine ("openpty" unix-openpty) int
  (amaster int :out)
  (aslave int :out)
  (name c-string)
  (termp (* (struct termios)))
  (winp (* (struct winsize))))

(def-alien-type nil
  (struct itimerval
    (it-interval (struct timeval))	; timer interval
    (it-value (struct timeval))))	; current value

;;;
;;; Support for the Interval Timer (experimental)
;;;
(defconstant ITIMER-REAL 0)
(defconstant ITIMER-VIRTUAL 1)
(defconstant ITIMER-PROF 2)

#-linux
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
		   (unsigned-byte 29)
		   (mod 1000000)
		   (unsigned-byte 29)
		   (mod 1000000)))
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
      (syscall* (#-netbsd "setitimer" #+netbsd "__setitimer50" int (* (struct timeval))(* (struct timeval)))
		(values T
			(slot (slot itvo 'it-interval) 'tv-sec)
			(slot (slot itvo 'it-interval) 'tv-usec)
			(slot (slot itvo 'it-value) 'tv-sec)
			(slot (slot itvo 'it-value) 'tv-usec))
		which (alien-sap (addr itvn))(alien-sap (addr itvo))))))

#+linux
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

;;;; User and group database access, POSIX Standard 9.2.2

#+solaris
(defun unix-getpwuid (uid)
  _N"Return a USER-INFO structure for the user identified by UID, or NIL if not found."
  (declare (type unix-uid uid))
  (with-alien ((buf (array c-call:char 1024))
	       (user-info (struct passwd)))
    (let ((result
	   (alien-funcall
	    (extern-alien "getpwuid_r"
			  (function (* (struct passwd))
				    c-call:unsigned-int
				    (* (struct passwd))
				    (* c-call:char)
				    c-call:unsigned-int))
	    uid
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
(defun unix-getpwuid (uid)
  _N"Return a USER-INFO structure for the user identified by UID, or NIL if not found."
  (declare (type unix-uid uid))
  (let ((result
         (alien-funcall
          (extern-alien "getpwuid"
			  (function (* (struct passwd))
				    c-call:unsigned-int))
          uid)))
    (when (not (zerop (sap-int (alien-sap result))))
      (make-user-info
       :name (string (cast (slot result 'pw-name) c-call:c-string))
       :password (string (cast (slot result 'pw-passwd) c-call:c-string))
       :uid (slot result 'pw-uid)
       :gid (slot result 'pw-gid)
       :gecos (string (cast (slot result 'pw-gecos) c-call:c-string))
       :dir (string (cast (slot result 'pw-dir) c-call:c-string))
       :shell (string (cast (slot result 'pw-shell) c-call:c-string))))))

#+linux
(defun unix-getpwuid (uid)
  "Return a USER-INFO structure for the user identified by UID.  If
  not found, NIL is returned with a second value indicating the cause
  of the failure.  In particular, if the second value is 0 (or
  ENONENT, ESRCH, EBADF, etc.), then the uid was not found."
  (declare (type unix-uid uid))
  (with-alien ((buf (array c-call:char 16384))
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
      (if (not (zerop (sap-int (alien-sap result))))
          (make-user-info
           :name (string (cast (slot result 'pw-name) c-call:c-string))
           :password (string (cast (slot result 'pw-passwd) c-call:c-string))
           :uid (slot result 'pw-uid)
           :gid (slot result 'pw-gid)
           :gecos (string (cast (slot result 'pw-gecos) c-call:c-string))
           :dir (string (cast (slot result 'pw-dir) c-call:c-string))
           :shell (string (cast (slot result 'pw-shell) c-call:c-string)))
	  (values nil returned)))))

;;; Getrusage is not provided in the C library on Solaris 2.4, and is
;;; rather slow on later versions so the "times" system call is
;;; provided.
#+(and sparc svr4)
(progn
(def-alien-type nil
  (struct tms
    (tms-utime #-alpha long #+alpha int)	; user time used
    (tms-stime #-alpha long #+alpha int)	; system time used.
    (tms-cutime #-alpha long #+alpha int)	; user time, children
    (tms-cstime #-alpha long #+alpha int)))	; system time, children

(declaim (inline unix-times))
(defun unix-times ()
  _N"Unix-times returns information about the cpu time usage of the process
   and its children."
  (with-alien ((usage (struct tms)))
    (alien-funcall (extern-alien "times" (function int (* (struct tms))))
		   (addr usage))
    (values t
	    (slot usage 'tms-utime)
	    (slot usage 'tms-stime)
	    (slot usage 'tms-cutime)
	    (slot usage 'tms-cstime))))
) ; end progn

;; Requires call to tzset() in main.
;; Don't use this now: we 
#+(or linux svr4)
(progn
    (def-alien-variable ("daylight" unix-daylight) int)
    (def-alien-variable ("timezone" unix-timezone) time-t)
    (def-alien-variable ("altzone" unix-altzone) time-t)
    #-irix (def-alien-variable ("tzname" unix-tzname) (array c-string 2))
    #+irix (defvar unix-tzname-addr nil)
    #+irix (pushnew #'(lambda () (setq unix-tzname-addr nil))
                    ext:*after-save-initializations*)
    #+irix (declaim (notinline fakeout-compiler))
    #+irix (defun fakeout-compiler (name dst)
             (unless unix-tzname-addr
               (setf unix-tzname-addr (system:foreign-symbol-address
				       name
				       :flavor :data)))
              (deref (sap-alien unix-tzname-addr (array c-string 2)) dst))
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
                                (values #-irix (deref unix-tzname (if dst 1 0))
                                        #+irix (fakeout-compiler "tzname" (if dst 1 0)))
	    ) )
)

#-linux
(def-alien-type nil
  (struct utsname
    (sysname (array char #+svr4 257 #+bsd 256))
    (nodename (array char #+svr4 257 #+bsd 256))
    (release (array char #+svr4 257 #+bsd 256))
    (version (array char #+svr4 257 #+bsd 256))
    (machine (array char #+svr4 257 #+bsd 256))))

#+linux
(def-alien-type nil
  (struct utsname
    (sysname (array char 65))
    (nodename (array char 65))
    (release (array char 65))
    (version (array char 65))
    (machine (array char 65))
    (domainname (array char 65))))


;;; For asdf.  Well, only getenv, but might as well make it symmetric.

;; Environment manipulation; man getenv(3)
(def-alien-routine ("getenv" unix-getenv) c-call:c-string
  (name c-call:c-string) 
  _N"Get the value of the environment variable named Name.  If no such
  variable exists, Nil is returned.")

;; This doesn't exist in Solaris 8 but does exist in Solaris 10.
(def-alien-routine ("setenv" unix-setenv) c-call:int
  (name c-call:c-string)
  (value c-call:c-string)
  (overwrite c-call:int)
  _N"Adds the environment variable named Name to the environment with
  the given Value if Name does not already exist. If Name does exist,
  the value is changed to Value if Overwrite is non-zero.  Otherwise,
  the value is not changed.")


(def-alien-routine ("putenv" unix-putenv) c-call:int
  (name-value c-call:c-string)
  _N"Adds or changes the environment.  Name-value must be a string of
  the form \"name=value\".  If the name does not exist, it is added.
  If name does exist, the value is updated to the given value.")

(def-alien-routine ("unsetenv" unix-unsetenv) c-call:int
  (name c-call:c-string)
  _N"Removes the variable Name from the environment")


;;; For slime, which wants to use unix-execve.

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
	  #-unicode
	  (kernel:copy-to-system-area (the simple-string s)
				      (* vm:vector-data-offset vm:word-bits)
				      string-sap 0
				      (* (1+ n) vm:byte-bits))
	  #+unicode
	  (progn
	    ;; FIXME: Do we need to apply some kind of transformation
	    ;; to convert Lisp unicode strings to C strings?  Utf-8?
	    (dotimes (k n)
	      (setf (sap-ref-8 string-sap k)
		    (logand #xff (char-code (aref s k)))))
	    (setf (sap-ref-8 string-sap n) 0))
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
			  c-string system-area-pointer system-area-pointer)
			 program argv envp)))
      ;; 
      ;; Deallocate memory
      (when argv
	(system:deallocate-system-memory argv argv-bytes))
      (when envp
	(system:deallocate-system-memory envp envp-bytes)))
    (values result error-code)))

;;;; UNIX-EXECVE
(defun unix-execve (program &optional arg-list
			    (environment *environment-list*))
  _N"Executes the Unix execve system call.  If the system call suceeds, lisp
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
    (sub-unix-execve (%name->file program) arg-list env-list)))

(defun unix-fork ()
  _N"Executes the unix fork system call.  Returns 0 in the child and the pid
   of the child in the parent if it works, or NIL and an error number if it
   doesn't work."
  (int-syscall ("fork")))

(defun unix-setlocale ()
  _N"Set all the categories of the locale according to the values of
  the environment variables by calling setlocale(LC_ALL, \"\").

  Returns 0 on success and -1 if setlocale failed."
  (alien:alien-funcall
   (alien:extern-alien "os_setlocale"
		       (function c-call:int))))

(defun unix-get-lc-messages ()
  _N"Get LC_MESSAGES from the current locale.  If we can't, return
  NIL.  A call to UNIX-SETLOCALE must have been done previously before
  calling this so that the correct locale is returned."
  (with-alien ((buf (array c-call:char 256)))
    (let ((result
	    (alien-funcall
	     (extern-alien "os_get_lc_messages"
			   (function c-call:int
				     (* c-call:char)
				     c-call:int))
	     (cast buf (* c-call:char))
	     256)))
      (when (zerop result)
	(cast buf c-call:c-string)))))

(defun unix-get-locale-codeset ()
  _N"Get the codeset from the locale"
  (cast (alien-funcall
	    (extern-alien "os_get_locale_codeset"
			  (function (* char))))
	c-string))

(defun check-template (template)
  ;; Make sure the template ends with exactly 6 X's and no more.
  (let ((last-non-x (position-if-not #'(lambda (c)
					 (char= c #\X))
				     template
				     :from-end t)))
    (and last-non-x
	 (= last-non-x (- (length template) 7)))))

(defun unix-mkstemp (template)
  _N"Generates a unique temporary file name from TEMPLATE, and creates
  and opens the file.  On success, the corresponding file descriptor
  and name of the file is returned.  Otherwise, NIL and the UNIX error
  code is returned.

  The last six characters of the template must be \"XXXXXX\"."
  ;; Make sure the template is valid.
  (unless (check-template template)
    (return-from unix-mkstemp
      (values nil einval)))
  (let* ((format (if (eql *filename-encoding* :null)
		     :iso8859-1
		     *filename-encoding*))
	 ;; Convert the string to octets using the
	 ;; *FILENAME-ENCODING*.  Should we signal an error if the
	 ;; string can be encoded?
	 (octets (stream:string-to-octets template
					  :external-format format))
	 (length (length octets))
	 (buffer (make-array (1+ length)
			     :element-type '(unsigned-byte 8)
			     :initial-element 0)))
    ;; Copy the octets from OCTETS to the null-terminated array BUFFER.
    (replace buffer octets)
    (syscall ("mkstemp" (* c-call:char))
	     (values result
		     ;; Convert the array of octets in BUFFER back to
		     ;; a Lisp string.
		     (stream:octets-to-string buffer
					      :end length
					      :external-format format))
	     (sys:vector-sap buffer))))

(defun unix-mkdtemp (template)
  _N"Generate a uniquely named temporary directory from Template,
  which must have \"XXXXXX\" as the last six characters.  The
  directory is created with permissions 0700.  The name of the
  directory is returned.

  If the directory cannot be created NIL and the UNIX error code is
  returned."
  ;; Make sure the template is valid.
  (unless (check-template template)
    (return-from unix-mkdtemp
      (values nil einval)))
  (let* ((format (if (eql *filename-encoding* :null)
		     :iso8859-1
		     *filename-encoding*))
	 ;; Encode the string using the appropriate
	 ;; *filename-encoding*.  Should we signal an error if the
	 ;; string can't be encoded in that format?
	 (octets (stream:string-to-octets template
					  :external-format format))
	 (length (length octets))
	 (buffer (make-array (1+ length)
			     :element-type '(unsigned-byte 8)
			     :initial-element 0)))
    ;; Copy the octets from OCTETS to the null-terminated array BUFFER.
    (replace buffer octets)
    (let ((result (alien-funcall
		   (extern-alien "mkdtemp"
				 (function (* char)
					   (* char)))
		   (sys:vector-sap buffer))))
      ;; If mkdtemp worked, a non-NIL value is returned, return the
      ;; resulting name.  Otherwise, return NIL and the errno.
      (if (null-alien result)
	  (values nil (unix-errno))
	  (%file->name (cast result c-call:c-string))))))
