;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/save.lisp,v 1.17 1993/04/28 01:56:52 wlott Exp $")
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
  "An alist mapping environment variables (as keywords) to either values")

(defvar *editor-lisp-p* nil
  "This is true if and only if the lisp was started with the -edit switch.")



;;; Filled in by the startup code.
(defvar lisp-environment-list)


;;; PARSE-UNIX-SEARCH-LIST  --  Internal
;;;
;;; Returns a list of the directories that are in the specified Unix
;;; environment variable.  Return NIL if the variable is undefined.
;;;
(defun parse-unix-search-list (var)
  (let ((path (cdr (assoc var ext::*environment-list*))))
    (when path
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
	      (push (concatenate 'simple-string s "/") pl)))))))


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
	(push (cons (intern (string-upcase (subseq ele 0 =pos))
			    *keyword-package*)
		    (subseq ele (1+ =pos)))
	      *environment-list*))))
  (setf (search-list "default:") (list (default-directory)))
  (setf (search-list "path:") (parse-unix-search-list :path))
  (setf (search-list "home:")
	(or (parse-unix-search-list :home)
	    (list (default-directory))))

  (setf (search-list "library:")
	(or (parse-unix-search-list :cmucllib)
	    '("/usr/misc/.cmucl/lib/"))))



;;;; SAVE-LISP itself.

(alien:def-alien-routine "save" (alien:boolean)
  (file c-call:c-string)
  (initial-function (alien:unsigned #.vm:word-bits)))

(defun save-lisp (core-file-name &key
				 (purify t)
				 (root-structures ())
				 (constants nil)
				 (init-function #'%top-level)
				 (load-init-file t)
				 (site-init "library:site-init")
				 (print-herald t)
				 (process-command-line t))
  "Saves a CMU Common Lisp core image in the file of the specified name.  The
  following keywords are defined:
  
  :purify
      If true, do a purifying GC which moves all dynamically allocated
  objects into static space so that they stay pure.  This takes somewhat
  longer than the normal GC which is otherwise done, but GC's will done
  less often and take less time in the resulting core file.

  :root-structures
  :constants
      These should be a list of the main entry points in any newly loaded
  systems and a list of any large data structures that will never again
  be changed.  These need not be supplied, but locality and/or GC performance
  will be better if they are.  They are meaningless if :purify is NIL.
  
  :init-function
      This is a function which is called when the created core file is
  resumed.  The default function simply invokes the top level
  read-eval-print loop.  If the function returns the lisp will exit.
  
  :load-init-file
      If true, then look for an init.lisp or init.fasl file when the core
  file is resumed.

  :site-init
      If true, then the name of the site init file to load.  The default is
      library:site-init.  No error if this does not exist.

  :print-herald
      If true, print out the lisp system herald when starting."

  (when (fboundp 'eval:flush-interpreted-function-cache)
    (eval:flush-interpreted-function-cache))
  (if purify
      (purify :root-structures root-structures :constants constants)
      (gc))
  (flet
      ((restart-lisp ()
	 (catch '%end-of-the-world
	   (with-simple-restart (abort "Skip remaining initializations.")
	     (catch 'top-level-catcher
	       (reinit)
	       (dolist (f *before-save-initializations*) (funcall f))
	       (dolist (f *after-save-initializations*) (funcall f))
	       (environment-init)
	       (when site-init
		 (load site-init :if-does-not-exist nil :verbose nil))
	       (when process-command-line
		 (ext::process-command-strings))
	       (setf *editor-lisp-p* nil)
	       (macrolet ((find-switch (name)
			    `(find ,name *command-line-switches*
				   :key #'cmd-switch-name
				   :test #'(lambda (x y)
					     (declare (simple-string x y))
					     (string-equal x y)))))
		 (when (and process-command-line (find-switch "edit"))
		   (setf *editor-lisp-p* t))
		 (when (and load-init-file
			    (not (and process-command-line
				      (find-switch "noinit"))))
		   (let* ((cl-switch (find-switch "init"))
			  (name (and cl-switch
				     (or (cmd-switch-value cl-switch)
					 (car (cmd-switch-words
					       cl-switch))))))
		     (if name
			 (load (merge-pathnames name #p"home:")
			       :if-does-not-exist nil)
			 (or (load "home:init" :if-does-not-exist nil)
			     (load "home:.cmucl-init"
				   :if-does-not-exist nil))))))
	       (when process-command-line
		 (ext::invoke-switch-demons *command-line-switches*
					    *command-switch-demons*))
	       (when print-herald
		 (print-herald))))
	   (funcall init-function))
	 (unix:unix-exit 0)))
    (let ((initial-function (get-lisp-obj-address #'restart-lisp)))
      (without-gcing
	(save (unix-namestring core-file-name nil) initial-function))))
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
	", running on "
	,#'(lambda (stream) (write-string (machine-instance) stream))))

(setf (getf *herald-items* :bugs)
      '("Send bug reports and questions to cmucl-bugs@cs.cmu.edu."
	terpri
	"Loaded subsystems:"))

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
	   (error "Unrecognized *HERALD-ITEMS* entry: ~S." thing))))
      (fresh-line stream)))

  (values))


;;;; Random functions used by worldload.

(defun assert-user-package ()
  (unless (eq *package* (find-package "USER"))
    (error "Change *PACKAGE* to the USER package and try again.")))
