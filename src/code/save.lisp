;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/save.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Dump the current lisp image into a core file.  All the real work is done
;;; be C.  Also contains various high-level initialization stuff: loading init
;;; files and parsing environment variables.
;;;
;;; Written by William Lott.
;;; 
;;;
(in-package "LISP")

(in-package "EXTENSIONS")
(intl:textdomain "cmucl")

(export '(print-herald *herald-items* save-lisp *before-save-initializations*
	  *after-save-initializations* *environment-list* *editor-lisp-p*))
(in-package "LISP")

(defvar *before-save-initializations* nil
  "This is a list of functions which are called before creating a saved core
  image.  These functions are executed in the child process which has no ports,
  so they cannot do anything that tries to talk to the outside world.")

(defvar *after-save-initializations* nil
  "This is a list of functions which are called when a saved core image starts
  up.  The system itself should be initialized at this point, but applications
  might not be.")

(defvar *environment-list* nil
  "An alist mapping each environment variable (as a keyword) to its
  value.")

(defvar *environment-list-initialized* nil
  "Non-NIL if environment-init has been called")

(defvar *editor-lisp-p* nil
  "This is true if and only if the lisp was started with the -edit switch.")



;;; Filled in by the startup code.
(defvar lisp-environment-list)
(defvar *cmucl-lib*)		; Essentially the envvar CMUCLLIB, if available

#+:executable
;; A dumped executable image won't have any reasonable default
;; library: search list.  Save the one from when it was dumped.
(defvar *old-cmucl-library-search-list*)

(defvar *cmucl-core-path*)	; Path to where the Lisp core file was found.

;;; Filled in by the save code.
(defvar *cmucl-core-dump-time*)	; Time the core was dumped
(defvar *cmucl-core-dump-host*) ; machine-instance the core was dumped on


;;; PARSE-UNIX-SEARCH-LIST  --  Internal
;;;
;;; Returns a list of the directories that are in the specified Unix
;;; environment variable.  Return NIL if the variable is undefined.
;;;
(defun parse-unix-search-path (path)
  (do* ((i 0 (1+ p))
	(p (position #\: path :start i)
	   (position #\: path :start i))
	(pl ()))
       ((null p)
	(let ((s (subseq path i)))
	  (if (string= s "")
	      (push "default:" pl)
	      (push (concatenate 'simple-string s "/") pl)))
	(nreverse pl))
    (let ((s (subseq path i p)))
      (if (string= s "")
	  (push "default:" pl)
	  (push (concatenate 'simple-string s "/") pl)))))

(defun parse-unix-search-list (var)
  (let ((path (cdr (assoc var ext::*environment-list*))))
    (when path
      (parse-unix-search-path path))))


;;; ENVIRONMENT-INIT  --  Internal
;;;
;;;    Parse the LISP-ENVIRONMENT-LIST into a keyword alist.  Set up default
;;; search lists.
;;;
(defun environment-init ()
  (setq *environment-list* ())
  (dolist (ele lisp-environment-list)
    (let ((=pos (position #\= (the simple-string ele))))
      (when =pos
	(push (cons (intern (subseq ele 0 =pos)
			    *keyword-package*)
		    (subseq ele (1+ =pos)))
	      *environment-list*))))
  (setf (search-list "default:") (list (default-directory)))
  (setf (search-list "path:") (parse-unix-search-list :path))
  (setf (search-list "home:")
	(or (parse-unix-search-list :home)
	    (list (default-directory))))

  (setf (search-list "library:")
	(if (and (boundp '*cmucl-lib*)
		 #+:executable
		 (not (boundp '*old-cmucl-library-search-list*)))
	    (parse-unix-search-path *cmucl-lib*)
	    (or
	     #+:executable
	     *old-cmucl-library-search-list*
	     '("/usr/local/lib/cmucl/lib/"))))
  (setf (search-list "modules:")
	'("library:contrib/" "library:subsystems/" "target:contrib/"))
  (setf (search-list "ld-library-path:")
	(parse-unix-search-list :ld_library_path))
  (setf (search-list "ext-formats:")
	'("library:ext-formats/"
	  "target:i18n/"
	  "target:pcl/simple-streams/external-formats/"))
  (setq *environment-list-initialized* t))


;;;; SAVE-LISP itself.

(alien:def-alien-routine "save" (alien:boolean)
  (file c-call:c-string)
  (initial-function (alien:unsigned #.vm:word-bits))
  (sse2-mode c-call:int))

#+:executable
(alien:def-alien-routine "save_executable" (alien:boolean)
  (file c-call:c-string)
  (initial-function (alien:unsigned #.vm:word-bits)))

(defun set-up-locale-external-format ()
  "Add external format alias for :locale to the format specified by
  the locale as set by setlocale(3C)."
  (let ((codeset (unix::unix-get-locale-codeset))
	(external-format nil))
    (cond ((zerop (length codeset))
	   (setq external-format *default-external-format*))
	  (t
	   (let ((name (intern codeset "KEYWORD")))
             (setq external-format
		   (stream::ef-name (stream::find-external-format name nil))))))
    (cond (external-format
	   (setf (gethash :locale stream::*external-format-aliases*)
		 external-format))
	  (t
	   (warn "No external format found for codeset \"~S\"; using ~S instead"
		 codeset
		 *default-external-format*)
	   (setf (gethash :locale stream::*external-format-aliases*)
		 *default-external-format*))))
  (values))

(defun decode-runtime-strings (locale file-locale)
  ;; The C runtime can initialize the following strings from the
  ;; command line or the environment.  We need to decode these into
  ;; the utf-16 strings that Lisp uses.
  (setf lisp-command-line-list
	(mapcar #'(lambda (s)
		    (stream:string-decode s locale))
		lisp-command-line-list))
  (setf lisp-environment-list
	(mapcar #'(lambda (s)
		    (stream:string-decode s locale))
		lisp-environment-list))
  ;; This needs more work..  *cmucl-lib* could be set from the the envvar
  ;; "CMUCLLIB" or from the "-lib" command-line option, and thus
  ;; should use the LOCALE to decode the string.
  (when *cmucl-lib*
    (setf *cmucl-lib*
	  (stream:string-decode *cmucl-lib* file-locale)))
  ;; This also needs more work since the core path could come from the
  ;; "-core" command-line option and should thus use LOCALE to decode
  ;; the string.  It could also come from the "CMUCLCORE" envvar.
  (setf *cmucl-core-path*
	(stream:string-decode *cmucl-core-path* file-locale))
  ;; *unidata-path* defaults to a pathname object, but the user can
  ;; specify a path, so we need to decode the string path if given.
  (when (and *unidata-path* (stringp *unidata-path*))
    (setf *unidata-path*
	  (stream:string-decode *unidata-path* file-locale))))

(defun save-lisp (core-file-name &key
				 (purify t)
				 (root-structures ())
				 (environment-name "Auxiliary")
				 (init-function #'%top-level)
				 (load-init-file t)
				 (site-init "library:site-init")
				 (print-herald t)
				 (process-command-line t)
		                 #+:executable
		                 (executable nil)
				 (batch-mode nil)
				 (quiet nil))
  "Saves a CMU Common Lisp core image in the file of the specified name.  The
  following keywords are defined:
  
  :purify
      If true (the default), do a purifying GC which moves all dynamically
  allocated objects into static space so that they stay pure.  This takes
  somewhat longer than the normal GC which is otherwise done, but GC's will
  be done less often and take less time in the resulting core file.  See
  EXT:PURIFY.

  :root-structures
      This should be a list of the main entry points in any newly loaded
  systems.  This need not be supplied, but locality and/or GC performance
  will be better if they are.  Meaningless if :purify is NIL.  See EXT:PURIFY.

  :environment-name
      Also passed to EXT:PURIFY when :PURIFY is T.  Rarely used.
  
  :init-function
      This is the function that starts running when the created core file is
  resumed.  The default function simply invokes the top level
  read-eval-print loop.  If the function returns the lisp will exit.
  
  :load-init-file
      If true, then look for an init file when the core file is resumed.
  Look for home:init first and then home:.cmucl-init.  No error if
  there is no init file.

  :site-init
      If true, then the name of the site init file to load.  The default is
  library:site-init if it exists.  If not, library:default-site-init
  is used if it exists.  No error if these files do not exist.

  :print-herald
      If true (the default), print out the lisp system herald when starting.

  :process-command-line
      If true (the default), process command-line switches via the normal
  mechanisms, otherwise ignore all switches (except those processed by
  the C startup code).  In either case, the command line switches are
  saved in *COMMAND-LINE-STRINGS* and
  *COMMAND-LINE-APPLICATION-ARGUMENTS*.

  :executable
      If nil (the default), save-lisp will save using the traditional
   core-file format.  If true, save-lisp will create an executable
   file that contains the lisp image built in. 
   (Not all architectures support this yet.)

  :batch-mode
      If nil (the default), then the presence of the -batch command-line
  switch will invoke batch-mode processing.  If true, the produced core
  will always be in batch-mode, regardless of any command-line switches.

  :quiet
     If non-NIL, loading, compiling, and GC messages are suppressed.
     This is equivalent to setting *load-verbose*, *compile-verbose*,
     *compile-print*, *compile-progress*, *require-verbose*, and
     *gc-verbose* all to NIL.  If NIL (the default), the default
     values of these variables are used."

  (unless (probe-file (directory-namestring core-file-name))
    (error 'simple-file-error
           :format-control (intl:gettext "Directory ~S does not exist")
           :format-arguments (list (directory-namestring core-file-name))))
  
  #+mp (mp::shutdown-multi-processing)
  (when (fboundp 'eval:flush-interpreted-function-cache)
    (eval:flush-interpreted-function-cache))
  (when (fboundp 'cancel-finalization)
    (cancel-finalization sys:*tty*))

  #+:executable
  (when executable 
    ;; Only do this when dumping an executable Lisp.  Otherwise
    ;; worldload will make us lose because it clears the search lists.
    ;; If we are dumping an executable lisp image, we want to keep
    ;; track of the library search list across dumps because the
    ;; normal way for figuring out the library paths from arg[0] is
    ;; almost guaranteed to be wrong for executables.
    (setf *old-cmucl-library-search-list* (search-list "library:")))
  
  (if purify
      (purify :root-structures root-structures
	      :environment-name environment-name)
      #-gencgc (gc) #+gencgc (gc :full t))
  (dolist (f *before-save-initializations*) (funcall f))
  (setq ext:*batch-mode* (if batch-mode t nil))
  (labels
      ((%restart-lisp ()
	 (with-simple-restart (abort (intl:gettext "Skip remaining initializations."))
	   (catch 'top-level-catcher
	     (reinit)
	     (environment-init)
	     (dolist (f *after-save-initializations*) (funcall f))
	     ;; Set the runtime locale
	     (unless (zerop (unix::unix-setlocale))
	       (warn "os_setlocale failed"))
	     ;; Load external format aliases now so we can aliases to
	     ;; specify the external format.
	     (stream::load-external-format-aliases)
	     ;; Set up :locale format
	     (set-up-locale-external-format)
	     ;; Set terminal encodings to :locale and filename encoding to :utf-8.
	     ;; (This needs more work on Darwin.)
	     (set-system-external-format :locale :utf-8)
	     (decode-runtime-strings :locale :utf-8)
	     ;; Need to reinitialize the environment again because
	     ;; we've possibly changed the environment variables and
	     ;; pathnames.
	     (environment-init)
	     ;; Set the locale for lisp
	     (intl::setlocale)
	     (ext::process-command-strings process-command-line)
	     (setf *editor-lisp-p* nil)
	     (macrolet ((find-switch (name)
			  `(find ,name *command-line-switches*
				 :key #'cmd-switch-name
				 :test #'(lambda (x y)
					   (declare (simple-string x y))
					   (string-equal x y)))))
               (when (or quiet
			 (and process-command-line (find-switch "quiet")))
                 (setq *load-verbose* nil
                       *compile-verbose* nil
                       *compile-print* nil
                       *compile-progress* nil
                       *require-verbose* nil
                       *gc-verbose* nil))
	       (when (and process-command-line
			  (or (find-switch "help")
			      (find-switch "-help")
			      (find-switch "version")
			      (find-switch "-version")))
		 ;; Don't load any init files if -help, --help,
		 ;; -version, or --version is given.  These exit right
		 ;; away, so loading the init file is wasteful.
		 (setf site-init nil)
		 (setf load-init-file nil))
	       (when (and site-init
			  (not (and process-command-line
				    (find-switch "nositeinit"))))
		 (or (load site-init :if-does-not-exist nil :verbose nil)
		     (load "library:default-site-init" :if-does-not-exist nil :verbose nil)))
	       (when (and process-command-line (find-switch "edit"))
		 (setf *editor-lisp-p* t))
	       (when (and load-init-file
			  (not (and process-command-line
				    (find-switch "noinit"))))
		 (let* ((cl-switch (find-switch "init"))
			(name (and cl-switch
				   (or (cmd-switch-value cl-switch)
				       (car (cmd-switch-words cl-switch))))))
		   (if name
		       (load (merge-pathnames name #p"home:")
			     :if-does-not-exist nil)
		       (or (load "home:init" :if-does-not-exist nil)
			   (load "home:.cmucl-init"
				 :if-does-not-exist nil)))))
	       (when process-command-line
		 (ext::invoke-switch-demons *command-line-switches*
					    *command-switch-demons*))
	       (when (and print-herald
			  (not (or quiet
				   (and process-command-line
					(find-switch "quiet")))))
		 ;; Don't print the herald if -quiet is given.
		 (print-herald)))))
	 (funcall init-function))
       (restart-lisp ()
	 (unix:unix-exit
	  (catch '%end-of-the-world
	    (unwind-protect
		(if *batch-mode*
		    (handler-case
			(%restart-lisp)
		      (error (cond)
			(format *error-output* (intl:gettext "Error in batch processing:~%~A~%")
				cond)
			(throw '%end-of-the-world 1)))
		    (%restart-lisp))
	      (finish-standard-output-streams))))))

    ;; Record dump time and host
    (setq *cmucl-core-dump-time* (get-universal-time))
    (setq *cmucl-core-dump-host* (machine-instance))

    (let ((initial-function (get-lisp-obj-address #'restart-lisp))
	  (core-name (unix-namestring core-file-name nil)))
      (without-gcing
	  #+:executable
	(if executable
	    (save-executable core-name initial-function)
	    (save core-name initial-function #+sse2 1 #-sse2 0))
	#-:executable
	(save core-name initial-function #+sse2 1 #-sse2 0))))
  nil)



;;;; PRINT-HERALD support.

(defvar *herald-items* ()
  "Determines what PRINT-HERALD prints (the system startup banner.)  This is a
   database which can be augmented by each loaded system.  The format is a
   property list which maps from subsystem names to the banner information for
   that system.  This list can be manipulated with GETF -- entries are printed
   in, reverse order, so the newest entry is printed last.  Usually the system
   feature keyword is used as the system name.  A given banner is a list of
   strings and functions (or function names).  Strings are printed, and
   functions are called with an output stream argument.")

(setf (getf *herald-items* :common-lisp)
      `("CMU Common Lisp "
	,#'(lambda (stream)
	     (write-string (lisp-implementation-version) stream))
	,#'(lambda (stream)
	     (write-string (intl:gettext ", running on ") stream))
	,#'(lambda (stream) (write-string (machine-instance) stream))
	terpri
	,#'(lambda (stream)
	     (let ((core (if (boundp '*cmucl-core-path*)
			     (truename *cmucl-core-path*)
			     nil))
		   (dump-time (if (boundp '*cmucl-core-dump-time*)
		                  *cmucl-core-dump-time*
				  nil)))
	       (when core
		 (write-string (intl:gettext "With core: ") stream)
		 (write-line (namestring core) stream))
	       (when dump-time
		 (write-string (intl:gettext "Dumped on: ") stream)
		 (ext:format-universal-time stream dump-time :style :iso8601)
		 (write-string (intl:gettext " on ") stream)
		 (write-line *cmucl-core-dump-host* stream))))
	))

(setf (getf *herald-items* :bugs)
      `(,#'(lambda (stream)
	     (write-string (intl:gettext "Please visit https://cmucl.org/bugs to report bugs and ask questions.")
			   stream))
	terpri
	,#'(lambda (stream)
	     (write-string (intl:gettext "Loaded subsystems:") stream))))

#+unicode
(setf (getf *herald-items* :unicode)
      `(,#'(lambda (stream)
	     (write-string _"    Unicode " stream))
	,(if (and (boundp 'lisp::*unidata-version*)
		  (>= (length lisp::*unidata-version*) 11))
	     (subseq lisp::*unidata-version* 11
		     (1- (length lisp::*unidata-version*)))
	     " ")
	,#'(lambda (stream)
	     (write-string _"with Unicode version " stream))
	,#'(lambda (stream)
	     (princ lisp::+unicode-major-version+ stream)
	     (write-char #\. stream)
	     (princ lisp::+unicode-minor-version+ stream)
	     (write-char #\. stream)
	     (princ lisp::+unicode-update-version+ stream))
	terpri))

;;; PRINT-HERALD  --  Public
;;;
(defun print-herald (&optional (stream *standard-output*))
  "Print some descriptive information about the Lisp system version and
   configuration."
  (let ((res ()))
    (do ((item *herald-items* (cddr item)))
	((null item))
      (push (second item) res))

    (fresh-line stream)
    (dolist (item res)
      (dolist (thing item)
	(typecase thing
	  (string
	   (write-string thing stream))
	  (function (funcall thing stream))
	  ((or symbol cons)
	   (funcall (fdefinition thing) stream))
	  (t
	   (error (intl:gettext "Unrecognized *HERALD-ITEMS* entry: ~S.") thing))))
      (fresh-line stream)))

  (values))


;;;; Random functions used by worldload.

(defun assert-user-package ()
  (unless (eq *package* (find-package "CL-USER"))
    (error (intl:gettext "Change *PACKAGE* to the USER package and try again."))))

;;; MAYBE-BYTE-LOAD  --  Interface
;;;
;;;    If Name has been byte-compiled, and :runtime is a feature, then load the
;;; byte-compiled version, otherwise just do normal load.
;;;
(defun maybe-byte-load (name &optional (load-native t))
  (let ((bname (make-pathname
		:defaults name
		:type #.(c:backend-byte-fasl-file-type c:*target-backend*))))
    (cond ((and (featurep :runtime)
		(probe-file bname))
	   (load bname))
	  (load-native
	   (load name)))))


;;; BYTE-LOAD-OVER  --  Interface
;;;
;;;    Replace a cold-loaded native object file with a byte-compiled one, if it
;;; exists.
;;;
(defun byte-load-over (name)
  (load (make-pathname
	 :defaults name
	 :type #.(c:backend-byte-fasl-file-type c:*target-backend*))
	:if-does-not-exist nil))
