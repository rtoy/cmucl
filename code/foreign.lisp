;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/foreign.lisp,v 1.7 1991/08/30 17:23:40 ram Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "SYSTEM")

(in-package "EXT")
(export '(load-foreign))
(in-package "SYSTEM")
(import 'ext:load-foreign)

(defvar *previous-linked-object-file* nil)
(defvar *foreign-segment-free-pointer* foreign-segment-start)

(defun pick-temporary-file-name (&optional (base "/tmp/tmp~D~C"))
  (let ((code (char-code #\A)))
    (loop
      (let ((name (format nil base (mach:unix-getpid) (code-char code))))
	(multiple-value-bind
	    (fd errno)
	    (mach:unix-open name
			    (logior mach:o_wronly mach:o_creat mach:o_excl)
			    #o666)
	  (cond ((not (null fd))
		 (mach:unix-close fd)
		 (return name))
		((not (= errno mach:eexist))
		 (error "Could not create temporary file ~S: ~A"
			name (mach:get-unix-error-msg errno)))
		
		((= code (char-code #\Z))
		 (setf code (char-code #\a)))
		((= code (char-code #\z))
		 (return nil))
		(t
		 (incf code))))))))

#+sparc
(ext:def-c-record exec
  (magic ext:unsigned-long)
  (text ext:unsigned-long)
  (data ext:unsigned-long)
  (bss ext:unsigned-long)
  (syms ext:unsigned-long)
  (entry ext:unsigned-long)
  (trsize ext:unsigned-long)
  (drsize ext:unsigned-long))

(defun allocate-space-in-foreign-segment (bytes)
  (let* ((pagesize-1 (1- (get-page-size)))
	 (memory-needed (logandc2 (+ bytes pagesize-1) pagesize-1))
	 (addr (int-sap *foreign-segment-free-pointer*))
	 (new-ptr (+ *foreign-segment-free-pointer* bytes)))
    (when (> new-ptr (+ foreign-segment-start foreign-segment-size))
      (error "Not enough memory left."))
    (setf *foreign-segment-free-pointer* new-ptr)
    (gr-call* mach:vm_allocate *task-self* addr memory-needed nil)
    addr))

#+sparc
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (mach:unix-open name mach:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (mach:get-unix-error-msg errno)))
    (unwind-protect
	(with-stack-alien (header exec #.(ext:c-sizeof 'exec))
	  (mach:unix-read fd
			  (alien-sap (alien-value header))
			  (truncate (alien-size (alien-value header))
				    (bytes 1)))
	  (let* ((len-of-text-and-data
		  (+ (alien-access (exec-text (alien-value header)))
		     (alien-access (exec-data (alien-value header)))))
		 (memory-needed
		  (+ len-of-text-and-data
		     (alien-access (exec-bss (alien-value header)))))
		 (addr (allocate-space-in-foreign-segment memory-needed)))
	    (mach:unix-read fd addr len-of-text-and-data)))
      (mach:unix-close fd))))

#+pmax
(ext:def-c-record filehdr
  (magic ext:unsigned-short)
  (nscns ext:unsigned-short)
  (timdat ext:long)
  (symptr ext:long)
  (nsyms ext:long)
  (opthdr ext:unsigned-short)
  (flags ext:unsigned-short))

#+pmax
(ext:def-c-record aouthdr
  (magic ext:short)
  (vstamp ext:short)
  (tsize ext:long)
  (dsize ext:long)
  (bsize ext:long)
  (entry ext:long)
  (text_start ext:long)
  (data_start ext:long))

#+pmax
(defconstant filhsz 20)
#+pmax
(defconstant aouthsz 56)
#+pmax
(defconstant scnhsz 40)

#+pmax
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (mach:unix-open name mach:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (mach:get-unix-error-msg errno)))
    (unwind-protect
	(with-stack-alien (filehdr filehdr #.(ext:c-sizeof 'filehdr))
	  (with-stack-alien (aouthdr aouthdr #.(ext:c-sizeof 'aouthdr))
	    (mach:unix-read fd
			    (alien-sap (alien-value filehdr))
			    (truncate (alien-size (alien-value filehdr))
				      (bytes 1)))
	    (mach:unix-read fd
			    (alien-sap (alien-value aouthdr))
			    (truncate (alien-size (alien-value aouthdr))
				      (bytes 1)))
	    (let* ((len-of-text-and-data
		    (+ (alien-access (aouthdr-tsize (alien-value aouthdr)))
		       (alien-access (aouthdr-dsize (alien-value aouthdr)))))
		   (memory-needed
		    (+ len-of-text-and-data
		       (alien-access (aouthdr-bsize (alien-value aouthdr)))))
		   (addr (allocate-space-in-foreign-segment memory-needed))
		   (pad-size-1 (if (< (alien-access
				       (aouthdr-vstamp (alien-value aouthdr)))
				      23)
				   7 15)))
	      (mach:unix-lseek fd
			       (logandc2 (+ filhsz aouthsz
					    (* scnhsz
					       (alien-access
						(filehdr-nscns
						 (alien-value filehdr))))
					    pad-size-1)
					 pad-size-1)
			       mach:l_set)
	      (mach:unix-read fd addr len-of-text-and-data))))
      (mach:unix-close fd))))

(defun parse-symbol-table (name)
  (format t ";;; Parsing symbol table...~%")
  (let ((symbol-table (make-hash-table :test #'equal)))
    (with-open-file (file name)
      (loop
	(let ((line (read-line file nil nil)))
	  (unless line
	    (return))
	  (let* ((symbol (subseq line 11))
		 (address (parse-integer line :end 8 :radix 16))
		 (old-address (gethash symbol lisp::*foreign-symbols*)))
	    (unless (or (null old-address) (= address old-address))
	      (warn "~S moved from #x~8,'0X to #x~8,'0X.~%"
		    symbol old-address address))
	    (setf (gethash symbol symbol-table) address)))))
    (setf lisp::*foreign-symbols* symbol-table)))

(defun load-foreign (files &optional
			   (libraries '("-lc"))
			   (linker "library:load-foreign.csh")
			   (base-file "path:lisp")
			   (env ext:*environment-list*))
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
  (let ((output-file (pick-temporary-file-name))
	(symbol-table-file (pick-temporary-file-name))
	(error-output (make-string-output-stream)))

    (format t ";;; Running ~A...~%" linker)
    (force-output)
    (let ((proc (ext:run-program linker
				 (list* (or *previous-linked-object-file*
					    (namestring (truename base-file)))
					(format nil "~X"
						*foreign-segment-free-pointer*)
					output-file
					symbol-table-file
					(append (if (atom files)
						    (list files)
						    files)
						libraries))
				 :env env
				 :input nil
				 :output error-output
				 :error :output)))
      (unless proc
	(error "Could not run ~S" linker))
      (unless (zerop (ext:process-exit-code proc))
	(system:serve-all-events 0)
	(error "~S failed:~%~A"
	       linker (get-output-stream-string error-output)))
      (load-object-file output-file)
      (parse-symbol-table symbol-table-file)
      (mach:unix-unlink symbol-table-file)
      (let ((old-file *previous-linked-object-file*))
	(setf *previous-linked-object-file* output-file)
	(when old-file
	  (mach:unix-unlink old-file)))))
  (format t ";;; Done.~%"))
