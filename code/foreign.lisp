;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/foreign.lisp,v 1.27 1997/11/15 11:03:27 dtc Exp $")
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

#+hppa (defconstant foreign-segment-start #x10C00000)
#+hppa (defconstant foreign-segment-size  #x00400000)

#+(and (not linux) x86)
 (defconstant foreign-segment-start #x7000000) ; just an unused space
#+(and (not linux) x86) 
 (defconstant foreign-segment-size  #x00400000)

(defvar *previous-linked-object-file* nil)
#-(or linux irix)
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

#+(or FreeBSD (and sparc (not svr4)))
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

#-(or linux svr4)
(defun allocate-space-in-foreign-segment (bytes)
  (let* ((pagesize-1 (1- (get-page-size)))
	 (memory-needed (logandc2 (+ bytes pagesize-1) pagesize-1))
	 (addr (int-sap *foreign-segment-free-pointer*))
	 (new-ptr (+ *foreign-segment-free-pointer* memory-needed)))
    (when (> new-ptr (+ foreign-segment-start foreign-segment-size))
      (error "Not enough memory left."))
    (setf *foreign-segment-free-pointer* new-ptr)
    (allocate-system-memory-at addr memory-needed)
    addr))

;;; pw-- This seems to work for FreeBSD. The MAGIC field is not tested
;;; for correct file format so it may croak if ld fails to produce the
;;; expected results. It is probably good enough for now.
#+(or FreeBSD (and sparc (not svr4)))
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

#+hppa
(alien:def-alien-type nil
    (alien:struct sys_clock
                  (secs c-call:unsigned-int)
                  (nanosecs c-call:unsigned-int)))
#+hppa
(alien:def-alien-type nil
    (alien:struct header
                  (system_id c-call:short)
                  (a_magic c-call:short)
                  (version_id c-call:unsigned-int)
                  (file_time (alien:struct sys_clock))
                  (entry_space c-call:unsigned-int)
                  (entry_subspace c-call:unsigned-int)
                  (entry_offset c-call:unsigned-int)
                  (aux_header_location c-call:unsigned-int)
                  (aux_header_size c-call:unsigned-int)
                  (som_length c-call:unsigned-int)
                  (presumed_dp c-call:unsigned-int)
                  (space_location c-call:unsigned-int)
                  (space_total c-call:unsigned-int)
                  (subspace_location c-call:unsigned-int)
                  (subspace_total c-call:unsigned-int)
                  (loader_fixup_location c-call:unsigned-int)
                  (loader_fixup_total c-call:unsigned-int)
                  (space_strings_location c-call:unsigned-int)
                  (space_strings_size c-call:unsigned-int)
                  (init_array_location c-call:unsigned-int)
                  (init_array_total c-call:unsigned-int)
                  (compiler_location c-call:unsigned-int)
                  (compiler_total c-call:unsigned-int)
                  (symbol_location c-call:unsigned-int)
                  (symbol_total c-call:unsigned-int)
                  (fixup_request_location c-call:unsigned-int)
                  (fixup_request_total c-call:unsigned-int)
                  (symbol_strings_location c-call:unsigned-int)
                  (symbol_strings_size c-call:unsigned-int)
                  (unloadable_sp_location c-call:unsigned-int)
                  (unloadable_sp_size c-call:unsigned-int)
                  (checksum c-call:unsigned-int)))

#+hppa
(alien:def-alien-type nil
    (alien:struct aux_id
                  #|
                  (mandatory c-call:unsigned-int 1)
                  (copy c-call:unsigned-int 1)
                  (append c-call:unsigned-int 1)
                  (ignore c-call:unsigned-int 1)
                  (reserved c-call:unsigned-int 12)
                  (type c-call:unsigned-int 16)
                  |#
                  (dummy c-call:unsigned-int)
                  (length c-call:unsigned-int)))
#+hppa
(alien:def-alien-type nil
    (alien:struct som_exec_auxhdr
                  (som_auxhdr (alien:struct aux_id))
                  (exec_tsize c-call:long)
                  (exec_tmem c-call:long)
                  (exec_tfile c-call:long)
                  (exec_dsize c-call:long)
                  (exec_dmem c-call:long)
                  (exec_dfile c-call:long)
                  (exec_bsize c-call:long)
                  (exec_entry c-call:long)
                  (exec_flags c-call:long)
                  (exec_bfill c-call:long)))

#+hppa
(alien:def-alien-routine ("bzero" unix-bzero) c-call:void
  (s alien:system-area-pointer)
  (n c-call:unsigned-long))

#+hppa
(defconstant reloc-magic #x106)
#+hppa
(defconstant cpu-pa-risc1-0 #x20b)
#+hppa
(defconstant cpu-pa-risc1-1 #x210)
#+hppa
(defconstant cpu-pa-risc-max #x2ff)

#+hppa
(defun load-object-file (name)
  (format t ";;; Loading object file...~%")
  (multiple-value-bind (fd errno) (unix:unix-open name unix:o_rdonly 0)
    (unless fd
      (error "Could not open ~S: ~A" name (unix:get-unix-error-msg errno)))
    (unwind-protect
        (alien:with-alien ((header (alien:struct som_exec_auxhdr)))
          (unix:unix-lseek fd (alien:alien-size (alien:struct header) :bytes)
                           unix:l_set)
          (unix:unix-read fd
                          (alien:alien-sap header)
                          (alien:alien-size (alien:struct som_exec_auxhdr)
                                            :bytes))
          (let* ((tmem (alien:slot header 'exec_tmem))
                 (tsize (alien:slot header 'exec_tsize))
                 (dmem (alien:slot header 'exec_dmem))
                 (dsize (alien:slot header 'exec_dsize))
                 (bsize (alien:slot header 'exec_bsize))
                 (memory-needed (+ tsize dsize bsize (* 2 4096)))
                 (addr (allocate-space-in-foreign-segment memory-needed)))
            (unix-bzero addr memory-needed) ;force valid
            (unix:unix-lseek fd (alien:slot header 'exec_tfile) unix:l_set)
            (unix:unix-read fd (system:int-sap tmem) tsize)
            (unix:unix-lseek fd (alien:slot header 'exec_dfile) unix:l_set)
            (unix:unix-read fd (system:int-sap dmem) dsize)
            (unix-bzero (system:int-sap (+ dmem dsize)) bsize)
            ;;(format t "tmem ~X tsize ~X dmem ~X dsize ~X bsize ~X~%"
            ;;        tmem tsize dmem dsize bsize)
            ;;(format t "tfile ~X dfile ~X~%"
            ;;        (alien:slot header 'exec_tfile)
            ;;        (alien:slot header 'exec_dfile))
            (alien:alien-funcall (alien:extern-alien
                                  "sanctify_for_execution"
                                  (alien:function c-call:void
                                                  alien:system-area-pointer
                                                  c-call:unsigned-long))
                                 addr (+ (- dmem tmem) dsize bsize))
            ))
      (unix:unix-close fd))))

#-(or linux solaris irix)
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
		 #+FreeBSD (kind (aref line 9))	; filter out .o file names
		 (old-address (gethash symbol lisp::*foreign-symbols*)))
	    (unless (or (null old-address) (= address old-address)
			#+FreeBSD (char= kind #\F))
	      (warn "~S moved from #x~8,'0X to #x~8,'0X.~%"
		    symbol old-address address))
	    (setf (gethash symbol symbol-table) address)))))
    (setf lisp::*foreign-symbols* symbol-table)))

#-(or linux irix solaris)
(defun load-foreign (files &key
			   (libraries '("-lc"))
			   (base-file
			    #-hpux
			    (merge-pathnames *command-line-utility-name*
					     "path:")
			    #+hpux "library:cmucl.orig")
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
	(error-output (make-string-output-stream))
	(files (if (atom files) (list files) files)))

    (format t ";;; Running library:load-foreign.csh...~%")
    (force-output)
    #+hpux
    (dolist (f files)
      (with-open-file (stream f :element-type '(unsigned-byte 16))
	(unless (let ((sysid (read-byte stream)))
                  (or (eql sysid cpu-pa-risc1-0)
		      (and (>= sysid cpu-pa-risc1-1)
			   (<= sysid cpu-pa-risc-max))))
	  (error "Object file is wrong format, so can't load-foreign:~
		  ~%  ~S"
		 f))
	(unless (eql (read-byte stream) reloc-magic)
	  (error "Object file is not relocatable, so can't load-foreign:~
		  ~%  ~S"
		 f))))

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
					files)
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


(export '(alternate-get-global-address))

#-(or solaris linux irix)
(defun alternate-get-global-address (symbol)
  (declare (type simple-string symbol)
	   (ignore symbol))
  0)

#+(or linux solaris irix)
(progn

(defconstant rtld-lazy 1)
(defconstant rtld-now 2)
(defconstant rtld-global #-irix #o400 #+irix 4)

(defvar *global-table* nil)
;;; Dynamically loaded stuff isn't there upon restoring from a
;;; save--this is primarily for irix, which resolves tzname at
;;; runtime, resulting in *global-table* being set in the saved core
;;; image, resulting in havoc upon restart.
(pushnew #'(lambda () (setq *global-table* nil))
	 ext:*after-save-initializations*)

(defvar *dso-linker*
  #+solaris "/usr/ccs/bin/ld"
  #+(or linux irix) "/usr/bin/ld")

(alien:def-alien-routine dlopen system-area-pointer
  (str c-call:c-string) (i c-call:int))
(alien:def-alien-routine dlsym system-area-pointer
  (lib system-area-pointer)
  (str c-call:c-string))
(alien:def-alien-routine dlclose void (lib system-area-pointer))
(alien:def-alien-routine dlerror c-call:c-string)

;;; Ensure we've opened our own binary so can resolve global variables
;;; in the lisp image that come from libraries. This used to happen
;;; only in alternate-get-global-address, and only if no libraries
;;; were dlopened already, but that didn't work if something was
;;; dlopened before any problem global vars were used. So now we do
;;; this in any function that can add to the global-table, as well as
;;; in alternate-get-global-address.
(defun ensure-lisp-table-opened ()
  (unless *global-table*
    ;; Prevent recursive call if dlopen isn't defined
    (setf *global-table* (int-sap 0))
    (setf *global-table* (list (dlopen nil rtld-lazy)))
    (when (zerop (system:sap-int (car *global-table*)))
      (error "Can't open global symbol table: ~S" (dlerror)))))

(defun load-object-file (file)
  (ensure-lisp-table-opened)
  ; rtld global: so it can find all the symbols previously loaded
  ; rtld now: that way dlopen will fail if not all symbols are defined.
  (let ((sap (dlopen file (logior rtld-now rtld-global))))
       (if (zerop (sap-int sap))
	   (error "Can't open object ~S: ~S" file (dlerror))
	   (pushnew sap *global-table* :test #'sap=))))

(defun alternate-get-global-address (symbol)
  (ensure-lisp-table-opened)
  ;; find the symbol in any of the loaded obbjects,
  ;; search in reverse order of loading, later loadings
  ;; take precedence
  (let ((result 0))
       (do ((table *global-table* (cdr table)))
	   ((or (null (car table)) (not (zerop result))))
	   (setq result (sap-int (dlsym (car table) symbol))))
       (values result)))

(defun load-foreign (files &key
			   (libraries '("-lc"))
			   (base-file nil)
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
  ;; Note: dlopen remembers the name of an object, when dlopenin
  ;; the same name twice, the old objects is reused.
  (declare (ignore base-file))
  (let ((output-file (pick-temporary-file-name
		      (concatenate 'string "/tmp/~D~C" (string (gensym)))))
	(error-output (make-string-output-stream)))
 
    (format t ";;; Running ~A...~%" *dso-linker*)
    (force-output)
    (let ((proc (ext:run-program
		 *dso-linker*
		 (list*
		        #+(or solaris linux) "-G" #+irix "-shared"
			"-o"
			output-file
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
       (error "Could not run ~A" *dso-linker*))
      (unless (zerop (ext:process-exit-code proc))
	(system:serve-all-events 0)
        (error "~A failed:~%~A" *dso-linker*
	       (get-output-stream-string error-output)))
      (load-object-file output-file)
      (unix:unix-unlink output-file)
      ))
  (format t ";;; Done.~%"))
)
