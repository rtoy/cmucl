;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/save.lisp,v 1.6 1991/04/09 14:39:43 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/save.lisp,v 1.6 1991/04/09 14:39:43 ram Exp $
;;;
;;; Dump the current lisp image into a core file.  All the real work is done
;;; be C.
;;;
;;; Written by William Lott.
;;; 
;;;
(in-package "LISP")

(in-package "EXTENSIONS")
(export '(print-herald save-lisp *before-save-initializations*
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


(def-c-routine "save" (boolean)
  (file null-terminated-string))


(defun save-lisp (core-file-name &key
				 (purify t)
				 (root-structures ())
				 (constants nil)
				 (init-function
				  #'(lambda ()
				      (throw 'top-level-catcher nil)))
				 (load-init-file t)
				 (enable-gc t)
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
  resumed.  The default function simply aborts to the top level
  read-eval-print loop.  If the function returns it will be the value
  of Save-Lisp.
  
  :load-init-file
      If true, then look for an init.lisp or init.fasl file when the core
  file is resumed.
  
  :print-herald
      If true, print out the lisp system herald when starting.

  :enable-gc
      If true, turn GC on if it was off."

  (eval:flush-interpreted-function-cache)
  (if purify
      (purify :root-structures root-structures :constants constants)
      (gc))
  (unless (save (namestring core-file-name))
    (dolist (f *before-save-initializations*) (funcall f))
    (dolist (f *after-save-initializations*) (funcall f))
    (reinit)
    (dolist (ele lisp-environment-list)
      (let ((=pos (position #\= (the simple-string ele))))
	(when =pos
	  (push (cons (intern (string-upcase (subseq ele 0 =pos))
			      *keyword-package*)
		      (subseq ele (1+ =pos)))
		*environment-list*))))
    (setf (search-list "default:") (list (default-directory)))
    (setf (search-list "path:") (setup-path-search-list))
    (when process-command-line (ext::process-command-strings))
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
		 (not (and process-command-line (find-switch "noinit"))))
	(let* ((cl-switch (find-switch "init"))
	       (name (or (and cl-switch
			      (or (cmd-switch-value cl-switch)
				  (car (cmd-switch-words cl-switch))
				  "init"))
			 "init")))
	  (load (merge-pathnames name (user-homedir-pathname))
		:if-does-not-exist nil))))
    (when enable-gc
      (gc-on))
    (when print-herald
      (print-herald))
    (when process-command-line
      (ext::invoke-switch-demons *command-line-switches*
				 *command-switch-demons*))
    (funcall init-function)))


(defun print-herald ()
  (macrolet ((frob (variable)
	       `(if (boundp ',variable)
		    ,variable
		    "<not loaded>")))
    (write-string "CMU Common Lisp ")
    (write-string (lisp-implementation-version))
    (write-string ", running on ")
    (write-line (machine-instance))
    (write-string "Hemlock ")
    (write-string (frob *hemlock-version*))
    (write-string ", Compiler ")
    (write-line (frob compiler-version))
    (write-line "Send bug reports and questions to Gripe."))
  (values))


;;;; Random functions used by worldload.

(defun assert-user-package ()
  (unless (eq *package* (find-package "USER"))
    (error "Change *PACKAGE* to the USER package and try again.")))

(defun initial-init-function ()
  (gc-on)
  (throw 'top-level-catcher nil))

