;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/foreign.lisp,v 1.14 1993/07/05 01:47:44 wlott Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "SYSTEM")

(in-package "ALIEN")
(export '(load-foreign))
(in-package "SYSTEM")
(import 'alien:load-foreign)

#+sparc (defconstant foreign-segment-start #xe0000000)
#+sparc (defconstant foreign-segment-size  #x00100000)

#+pmax (defconstant foreign-segment-start #x00C00000)
#+pmax (defconstant foreign-segment-size  #x00400000)

(defvar *previous-linked-object-file* nil)
(defvar *foreign-segment-free-pointer* foreign-segment-start)

(defun pick-temporary-file-name (&optional (base "/tmp/tmp~D~C"))
  (let ((code (char-code #\A)))
    (loop
      (let ((name (format nil base (unix:unix-getpid) (code-char code))))
	(multiple-value-bind
	    (fd errno)
	    (unix:unix-open name
			    (logior unix:o_wronly unix:o_creat unix:o_excl)
			    #o666)
	  (cond ((not (null fd))
		 (unix:unix-close fd)
		 (return name))
		((not (= errno unix:eexist))
		 (error "Could not create temporary file ~S: ~A"
			name (unix:get-unix-error-msg errno)))
		
		((= code (char-code #\Z))
		 (setf code (char-code #\a)))
		((= code (char-code #\z))
		 (return nil))
		(t
		 (incf code))))))))

#+sparc
(alien:def-alien-type exec
  (alien:struct nil
    (magic c-call:unsigned-long)
    (text c-call:unsigned-long)
    (data c-call:unsigned-long)
    (bss c-call:unsigned-long)
    (syms c-call:unsigned-long)
    (entry c-call:unsigned-long)
    (trsize c-call:unsigned-long)
    (drsize c-call:unsigned-long)))

(defun allocate-space-in-foreign-segment (bytes)
  (let* ((pagesize-1 (1- (get-page-size)))
	 (memory-needed (logandc2 (+ bytes pagesize-1) pagesize-1))
	 (addr (int-sap *foreign-segment-free-pointer*))
	 (new-ptr (+ *foreign-segment-free-pointer* bytes)))
    (when (> new-ptr (+ foreign-segment-start foreign-segment-size))
      (error "Not enough memory left."))
    (setf *foreign-segment-free-pointer* new-ptr)
    (allocate-system-memory-at addr memory-needed)
    addr))

#+sparc
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (unix:unix-open name unix:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (unix:get-unix-error-msg errno)))
    (unwind-protect
	(alien:with-alien ((header exec))
	  (unix:unix-read fd
			  (alien:alien-sap header)
			  (alien:alien-size exec :bytes))
	  (let* ((len-of-text-and-data
		  (+ (alien:slot header 'text) (alien:slot header 'data)))
		 (memory-needed
		  (+ len-of-text-and-data (alien:slot header 'bss)))
		 (addr (allocate-space-in-foreign-segment memory-needed)))
	    (unix:unix-read fd addr len-of-text-and-data)))
      (unix:unix-close fd))))

#+pmax
(alien:def-alien-type filehdr
  (alien:struct nil
    (magic c-call:unsigned-short)
    (nscns c-call:unsigned-short)
    (timdat c-call:long)
    (symptr c-call:long)
    (nsyms c-call:long)
    (opthdr c-call:unsigned-short)
    (flags c-call:unsigned-short)))

#+pmax
(alien:def-alien-type aouthdr
  (alien:struct nil
    (magic c-call:short)
    (vstamp c-call:short)
    (tsize c-call:long)
    (dsize c-call:long)
    (bsize c-call:long)
    (entry c-call:long)
    (text_start c-call:long)
    (data_start c-call:long)))

#+pmax
(defconstant filhsz 20)
#+pmax
(defconstant aouthsz 56)
#+pmax
(defconstant scnhsz 40)

#+pmax
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (unix:unix-open name unix:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (unix:get-unix-error-msg errno)))
    (unwind-protect
	(alien:with-alien ((filehdr filehdr)
			   (aouthdr aouthdr))
	  (unix:unix-read fd
			  (alien:alien-sap filehdr)
			  (alien:alien-size filehdr :bytes))
	  (unix:unix-read fd
			  (alien:alien-sap aouthdr)
			  (alien:alien-size aouthdr :bytes))
	  (let* ((len-of-text-and-data
		  (+ (alien:slot aouthdr 'tsize) (alien:slot aouthdr 'dsize)))
		 (memory-needed
		  (+ len-of-text-and-data (alien:slot aouthdr 'bsize)))
		 (addr (allocate-space-in-foreign-segment memory-needed))
		 (pad-size-1 (if (< (alien:slot aouthdr 'vstamp) 23) 7 15)))
	    (unix:unix-lseek fd
			     (logandc2 (+ filhsz aouthsz
					  (* scnhsz
					     (alien:slot filehdr 'nscns))
					  pad-size-1)
				       pad-size-1)
			     unix:l_set)
	    (unix:unix-read fd addr len-of-text-and-data)))
      (unix:unix-close fd))))

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

(defun load-foreign (files &key
			   (libraries '("-lc"))
			   (base-file
			    (merge-pathnames *command-line-utility-name*
					     "path:"))
			   (env ext:*environment-list*))
  "Load-foreign loads a list of C object files into a running Lisp.  The files
  argument should be a single file or a list of files.  The files may be
  specified as namestrings or as pathnames.  The libraries argument should be a
  list of library files as would be specified to ld.  They will be searched in
  the order given.  The default is just \"-lc\", i.e., the C library.  The
  base-file argument is used to specify a file to use as the starting place for
  defined symbols.  The default is the C start up code for Lisp.  The env
  argument is the Unix environment variable definitions for the invocation of
  the linker.  The default is the environment passed to Lisp."
  (let ((output-file (pick-temporary-file-name))
	(symbol-table-file (pick-temporary-file-name))
	(error-output (make-string-output-stream)))

    (format t ";;; Running library:load-foreign.csh...~%")
    (force-output)
    (let ((proc (ext:run-program
		 "library:load-foreign.csh"
		 (list* (or *previous-linked-object-file*
			    (namestring (truename base-file)))
			(format nil "~X"
				*foreign-segment-free-pointer*)
			output-file
			symbol-table-file
			(append (mapcar #'(lambda (name)
					    (unix-namestring name nil))
					(if (atom files)
					    (list files)
					    files))
				libraries))
		 :env env
		 :input nil
		 :output error-output
		 :error :output)))
      (unless proc
	(error "Could not run library:load-foreign.csh"))
      (unless (zerop (ext:process-exit-code proc))
	(system:serve-all-events 0)
	(error "library:load-foreign.csh failed:~%~A"
	       (get-output-stream-string error-output)))
      (load-object-file output-file)
      (parse-symbol-table symbol-table-file)
      (unix:unix-unlink symbol-table-file)
      (let ((old-file *previous-linked-object-file*))
	(setf *previous-linked-object-file* output-file)
	(when old-file
	  (unix:unix-unlink old-file)))))
  (format t ";;; Done.~%"))
