;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/module.lisp,v 1.8 2003/05/12 16:30:41 emarsden Exp $")
;;;
;;; **********************************************************************

;;; Code written by Jim Muller.
;;; Rewritten by Bill Chiles.
;;;
;;; Note that this module file is based on the old system, and is being
;;; spliced into the current sources to reflect the last minute deprecated
;;; addition of modules to the X3J13 ANSI standard.
;;;
(in-package "LISP")

(export '(*modules* provide require))


(in-package "EXTENSIONS")
(export '(*require-verbose* defmodule))
(in-package "LISP")



;;;; Exported specials.

(defvar *modules* ()
  "This is a list of module names that have been loaded into Lisp so far.
   It is used by PROVIDE and REQUIRE.")

(defvar *require-verbose* t
  "*load-verbose* is bound to this before loading files.")

;;;; Defmodule.

(defvar *module-file-translations* (make-hash-table :test #'equal))
(defmacro defmodule (name &rest files)
  "Defines a module by registering the files that need to be loaded when
   the module is required.  If name is a symbol, its print name is used
   after downcasing it."
  `(%define-module ,name ',files))

(defun %define-module (name files)
  (setf (gethash (module-name-string name) *module-file-translations*)
        files))

(defun module-files (name)
  (gethash name *module-file-translations*))



;;;; Provide and Require.

(defun provide (module-name)
  "Adds a new module name to *modules* indicating that it has been loaded.
   Module-name may be any valid string designator.  All comparisons are
   done using string=, i.e. module names are case-sensitive."
  (pushnew (module-name-string module-name) *modules* :test #'string=)
  t)

(defun require (module-name &optional pathname)
  "Loads a module when it has not been already.  Pathname, if supplied,
   is a single pathname or list of pathnames to be loaded if the module
   needs to be.  If pathname is not supplied, then a list of files are
   looked for that were registered by a EXT:DEFMODULE form.  If the module
   has not been defined, then a file will be loaded whose name is formed
   by merging \"modules:\" and the concatenation of module-name with the
   suffix \"-LIBRARY\".  Note that both the module-name and the suffix are
   each, separately, converted from :case :common to :case :local.  This
   merged name will be probed with both a .lisp and .fasl extensions,
   calling LOAD if it exists.  While loading any files, *load-verbose* is
   bound to *require-verbose* which defaults to nil."
 (setf module-name (module-name-string module-name))
 (unless (member module-name *modules* :test #'string=)
   (let ((files (or (when pathname
		      (if (consp pathname) pathname (list pathname)))
		    (module-files module-name)
		    (list (module-default-pathname module-name))))
	 (*load-verbose* *require-verbose*))
     (dolist (file files t)
       (ext:without-package-locks
        (load file))))))



;;;; Misc.

(defun module-name-string (name)
  "Coerce a string designator to a module name."
  (string name))

(defun module-default-pathname (module-name)
  "Derive a default pathname to try to load for an undefined module
named module-name.  The default pathname is constructed from the
module-name by appending the suffix \"-LIBRARY\" to it, and merging
with \"modules:\".  Note that both the module-name and the suffix are
each, separately, converted from :case :common to :case :local."
  (let* ((module-pathname (make-pathname :name module-name :case :common))
         (library-pathname (make-pathname :name "-LIBRARY" :case :common)))
    (merge-pathnames
     "modules:"
     (make-pathname :name
		    (concatenate 'string
				 (pathname-name module-pathname :case :local)
				 (pathname-name library-pathname :case :local))
                    :case :local))))
