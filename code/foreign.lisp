;;; -*- Log: code.log; Package: Extensions -*-

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************

;;; Functions for dealing with foreign function calls in Common Lisp.

;;; Written by David B. McDonald, January 1987.

;;; *******************************************************************
(in-package "EXTENSIONS" :nicknames '("EXT") :use '("LISP" "SYSTEM"))

(export '(load-foreign get-code-pointer get-data-pointer))

(defconstant Unix-OMagic #x107)
(defconstant Unix-NMagic #x108)
(defconstant Unix-ZMagic #x10b)

(defconstant Unix-header-size 32)
(defconstant Symbol-Table-Entry-Size 12)

(defconstant n_undf #x0)
(defconstant n_abs #x2)
(defconstant n_text #x4)
(defconstant n_data #x6)
(defconstant n_bss #x8)
(defconstant n_comm #x12)
(defconstant n_fn #x1f)
(defconstant n_ext #x1)

(defvar file-count 0
  "Number of foreign function files loaded into the current Lisp.")

(defvar temporary-foreign-files NIL
  "List of dotted pairs containing location and size of object code
   loaded so that foreign functions can be called.")

(proclaim '(fixnum file-count))

(defstruct unix-ste
  (type 0 :type fixnum)
  (location 0))


(defvar foreign-symbols (make-hash-table :size 1000 :test #'equal))

(defmacro read-cword (sap offset)
  `(let ((rsap ,sap)
	 (roff ,offset))
     (declare (fixnum roff))
     (setq roff (the fixnum (ash roff 1)))
     (logior (ash (%primitive 16bit-system-ref rsap roff) 16)
	     (%primitive 16bit-system-ref rsap (the fixnum (1+ roff))))))

(defun read-miscop-free-pointer ()
  (let ((aa lisp::alloctable-address)
	(in (ash lisp::%assembler-code-type lisp::%alloc-ref-type-shift)))
    (declare (fixnum in))
    (logand (+ (logior (%primitive 16bit-system-ref aa (the fixnum (1+ in)))
		       (ash (%primitive 16bit-system-ref aa in) 16)) 3)
	    (lognot 3))))

(defun write-miscop-free-pointer (value)
  (let ((aa lisp::alloctable-address)
	(in (ash lisp::%assembler-code-type lisp::%alloc-ref-type-shift))
	(vl (logand value #xFFFF))
	(vh (logand (ash value -16) #xFFFF)))
    (declare (fixnum in))
    (%primitive 16bit-system-set aa (the fixnum (1+ in)) vl)
    (%primitive 16bit-system-set aa in vh)))

;;; Load-foreign accepts a file or a list of files to be loaded into the
;;; currently running Lisp core.  These files should be standard object
;;; files created by you favourite compiler (e.g., cc).  It accepts two
;;; optional parameters: libraries is a list of libraries to search
;;; for unresolved references (default is the standard C library), and
;;; env which is a list of Unix environment strings (default is what
;;; lisp started with).  Load-foreign runs ld creating an object file
;;; that has been linked so that it can be loaded into a predetermined
;;; location in memory.

(defun load-foreign (files &optional
			   (libraries '("-lc"))
			   (linker "/usr/cs/bin/ld")
			   (base-file "/usr/misc/.lisp/bin/lisp")
			   (env lisp::original-lisp-environment))
  "Load-foreign loads a list of C object files into a running Lisp.  The
  files argument should be a single file or a list of files.  The files
  may be specified as namestrings or as pathnames.  The libraries
  argument should be a list of library files as would be specified to
  ld.  They will be searched in the order given.  The default is just
  \"-lc\", i.e., the C library.  The linker argument is used to specifier
  the Unix linker to use to link the object files (the default is
  /usr/cs/bin/ld).  The base-file argument is used to specify a file to
  use as the starting place for defined symbols.  The default is the C
  start up code for Lisp.  The env argument is the Unix environment
  variable definitions for the invocation of the linker.  The default is
  the environment passed to Lisp."
  (if (null (listp files)) (setq files (list files)))
  (format t "[Loading foreign files ~A ...~%" files)
  (let ((tfl (if files (format nil "/tmp/L~d.~d"
			       (mach:unix-getuid)
			       (the fixnum (+ (the fixnum (mach:unix-getpid))
					      file-count)))
		 base-file))
	(ofl (get-last-loaded-file file-count base-file))
	(addr (read-miscop-free-pointer)))
    (when files
      (setq file-count (the fixnum (1+ file-count)))
      (format t "  [Running ld ...")
      (force-output t)
      (let ((nfiles ()))
	(dolist (f files)
	  (let* ((pn (merge-pathnames f *default-pathname-defaults*))
		 (tn (probe-file pn)))
	    (push (if tn (namestring tn) f) nfiles)))
	(setf files (nreverse nfiles)))
      (run-program linker `("-N" "-A" ,ofl "-T"
			    ,(format nil "~X" (+ addr unix-header-size))
			    "-o" ,tfl ,@files ,@libraries)
		   :env env :wait t :output t :error t)
      (push tfl temporary-foreign-files)
      (format t " done.]~%"))
    (multiple-value-bind (res dev ino mode nlnk uid gid rdev len)
			 (mach:unix-stat tfl)
      (declare (ignore ino mode nlnk uid gid rdev))
      (when (null res)
	(error "Could not stat intermediate file ~a, unix error: ~A."
	       tfl (mach:get-unix-error-msg dev)))
      (format t "  [Reading Unix object file ...")
      (force-output t)
      (multiple-value-bind (fd err) (mach:unix-open tfl mach:o_rdonly 0)
	(when (null fd)
	  (error "Failed to open intermediate file ~A, unix error: ~A."
		 (mach:get-unix-error-msg err)))
	(multiple-value-bind (bytes err2)
			     (mach:unix-read fd (int-sap addr)
					     len)
	  (when (or (null bytes) (not (eq bytes len)))
	    (if (null bytes)
		(error "Read of intermediate file ~A failed, unix error: ~A"
		       tfl (mach:get-unix-error-msg err2))
		(error "Read of intermediate file ~A only read ~d of ~d bytes."
		       tfl bytes len))))
	(mach:unix-close fd)))
    (format t " done.]~%")
    (let ((fsize (logand (+ (load-object-file tfl addr files) 4) (lognot 3))))
      (when files (write-miscop-free-pointer (+ addr fsize)))))
  (format t "done.]~%"))

;;; Get-last-loaded-file attempts to find the file that was last loaded into
;;; Lisp.  If one is found, load-foreign uses it as the bases for the initial
;;; symbol table.  Otherwise, it uses the lisp startup code.

(defun get-last-loaded-file (fc base-file)
  (declare (fixnum fc))
  (do ((i (the fixnum (1- fc)) (1- i)))
      ((< i 0) base-file)
    (declare (fixnum i))
    (let ((tfl (format nil "/tmp/L~d.~d" (mach:unix-getuid)
		       (the fixnum (+ (the fixnum (mach:unix-getpid)) i)))))
      (if (probe-file tfl) (return tfl)))))

;;; Load-object-file, actually loads the object file created by ld.
;;; It makes sure that it is a legal object file.

(defun load-object-file (file addr flag)
  (format t "  [Loading symbol table information ...")
  (force-output t)
  (let* ((sap (int-sap addr))
	 (magic (read-cword sap 0))
	 (text-size (read-cword sap 1))
	 (idata-size (read-cword sap 2))
	 (udata-size (read-cword sap 3))
	 (symtab-size (read-cword sap 4))
	 (epoint (read-cword sap 5))
	 (treloc-size (read-cword sap 6))
	 (dreloc-size (read-cword sap 7))
	 (load-size (+ text-size idata-size udata-size unix-header-size))
	 (symstart (+ text-size idata-size (if flag unix-header-size 2048)))
	 (strstart (+ symstart (the fixnum symtab-size))))
    (declare (fixnum magic text-size idata-size udata-size
		     symtab-size symstart strstart treloc-size
		     dreloc-size)
	     (ignore epoint))
    (unless (or (null flag)
		(and (= magic unix-OMagic) (= treloc-size 0) (= dreloc-size 0)))
      (error "File ~A is not a legal Unix object file." file))
    (read-symbol-table (the fixnum (+ (the fixnum sap) symstart))
		       symtab-size (the fixnum (+ (the fixnum sap) strstart)))
    (setq load-size (logand (the fixnum (+ load-size 8192)) (lognot 8191)))
    (do ((ind (truncate (+ text-size idata-size unix-header-size) 2)
	      (1+ ind))
	 (end (truncate udata-size 2)))
	((>= ind end))
      (%primitive 16bit-system-set sap ind 0))
    (format t " done.]~%")
    load-size))

;;; Read-symbol-table reads the symbol table out of the object, making
;;; external symbols available to Lisp, so that they can be used to
;;; link to the C routines.

(defun read-symbol-table (symstart symtab-size strstart)
  (let ((end (the fixnum (+ (the fixnum symstart) (the fixnum symtab-size)))))
    (do* ((se symstart (the fixnum (+ (the fixnum se) symbol-table-entry-size)))
	  (si (logior (ash (%primitive 16bit-system-ref se 0) 16)
		      (%primitive 16bit-system-ref se 1))
	      (logior (ash (%primitive 16bit-system-ref se 0) 16)
		      (%primitive 16bit-system-ref se 1)))
	  (st (%primitive 8bit-system-ref se 4)
	      (%primitive 8bit-system-ref se 4))
	  (sv (logior (ash (%primitive 16bit-system-ref se 4) 16)
		      (%primitive 16bit-system-ref se 5))
	      (logior (ash (%primitive 16bit-system-ref se 4) 16)
		      (%primitive 16bit-system-ref se 5))))
	 ((>= (the fixnum se) (the fixnum end)))
      (declare (fixnum st))
      (when (or (= st (logior n_text n_ext))
		(= st (logior n_data n_ext))
		(= st (logior n_bss n_ext)))
	(let* ((strend (%primitive find-character strstart si (+ si 512) 0)))
	  (when (null strend)
	    (error "Symbol table string didn't terminate."))
	  (let ((strlen (the fixnum (- (the fixnum strend) (the fixnum si))))
		(offset 0)
		(code NIL)
		(str NIL))
	    (declare (fixnum strlen offset))
	    (when (eq (%primitive 8bit-system-ref strstart si) (char-code #\_))
	      (setq offset (the fixnum (1+ offset)))
	      (when (eq (%primitive 8bit-system-ref strstart
				    (the fixnum (1+ (the fixnum si))))
			(char-code #\.))
		(setq code T)
 		(setq offset (the fixnum (1+ offset)))))
	    (setq str (make-string (the fixnum (- strlen offset))))
	    (%primitive byte-blt strstart
			(the fixnum (+ (the fixnum si) offset)) str 0 strlen)
	    (if (let ((x (ash sv (- (+ clc::type-shift-16 16)))))
		  (not (<= clc::first-pointer-type x clc::last-pointer-type)))
		(let ((ste (gethash str foreign-symbols))
		      (loc (int-sap sv)))
		  (cond ((null ste)
			 (setf (gethash str foreign-symbols)
			       (make-unix-ste :type (if code
							(logior n_text
								n_ext) st)
					      :location (if code nil loc))))
			(code
			 (setf (unix-ste-type ste) (logior n_text n_ext)))
			(T
			 (setf (unix-ste-location ste) loc)))))))))))

;;; Get-code-pointer accepts a simple string which should be the name
;;; of a C routine that has already been loaded into the Lisp core image.
;;; This name should use the correct capitalization of the C name without
;;; the default underscore.
(defun get-code-pointer (name)
  (let ((ste (gethash name foreign-symbols)))
    (when (null ste)
      (error "There is no foreign function named ~A loaded." name))
    (when (not (eq (unix-ste-type ste) (logior n_text n_ext)))
      (error "~A is a foreign external variable, not a foreign function." name))
    (unix-ste-location ste)))


;;; Get-data-pointer is similar to get-code-pointer, except it returns the
;;; address of a foreign global variable.

(defun get-data-pointer (name)
  (let ((ste (gethash name foreign-symbols)))
    (when (null ste)
      (error "There is no foreign variable named ~A loaded." name))
    (when (eq (unix-ste-type ste) (logior n_text n_ext))
      (error "~A is a foreign function, not a foreign variable." name))
    (unix-ste-location ste)))
