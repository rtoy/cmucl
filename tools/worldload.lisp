;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/worldload.lisp,v 1.41 1992/02/14 23:46:39 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file loads the parts of the system that aren't cold loaded and saves
;;; the resulting core image.  It writes "lisp.core" in the DEFAULT-DIRECTORY.
;;;
;;; ####### NOTE: HACK ALERT!!!!
;;; 
;;; This file must be loaded by:
;;;    (load (open "...worldload.lisp"))
;;;
;;; The OPEN call is needed to prevent the stream buffer from the previous
;;; incarnation (which is no longer valid) from being added to the stream
;;; buffer freelist.  If you don't do this, you will probably get a memory
;;; access violation when you first try to do file I/O in the new core.
;;;

;;; Define a bunch of search lists relative to lisp:
;;;
(setf (ext:search-list "code:") '("lisp:code/"))
(setf (ext:search-list "c:") '("lisp:compiler/"))
(setf (ext:search-list "vm:")
      '(#+pmax "c:mips/"
        #+sparc "c:sparc/"
	#+rt "c:rt/"
	"c:generic/"))
(setf (ext:search-list "assem:")
      '(#+pmax "lisp:assembly/mips/"
	#+sparc "lisp:assembly/sparc/"
	#+rt "lisp:assembly/rt/"
	"lisp:assembly/"))
(setf (ext:search-list "hem:") '("lisp:hemlock/"))
(setf (ext:search-list "clx:") '("lisp:clx/"))
(setf (ext:search-list "pcl:") '("lisp:pcl/"))
(setf (ext:search-list "tools:") '("lisp:tools/"))

;;; Make sure the core will start up in the user package.
(lisp::assert-user-package)

;;; We want to be in the LISP package for the rest of the file.
(in-package "LISP")

;;; Make sure the package structure is correct.
;;;
(load "code:exports")

;;; Get some data on this core.
;;;
(write-string "What is the current lisp-implementation-version? ")
(force-output)
(set '*lisp-implementation-version* (read-line))

;;; Keep us entertained...
(setq *load-verbose* t)

;;; Load random code sources.

(load "code:format-time")
(load "code:parse-time")
(load "code:purify")
(load "code:commandline")
(load "code:sort")
(load "code:time")
(load "code:tty-inspect")
(load "code:describe")
(load "code:rand")
(load "code:trace")
(load "code:ntrace")
(load "code:profile")
(load "code:weak")
(load "code:final")
(load "code:sysmacs")
(load "code:run-program")
(load "code:query")
(load "code:loop")
(load "code:internet")
(load "code:wire")
(load "code:remote")
(load "code:foreign")
(load "code:setf-funs")

(setq *info-environment*
      (list* (make-info-environment)
	     (compact-info-environment (first *info-environment*)
				       :name "Kernel")
	     (rest *info-environment*)))
(purify :root-structures
	`(lisp::%top-level extensions:save-lisp ,lisp::fop-codes))

;;; Load the compiler.
#-no-compiler
(load "c:loadcom.lisp")
#-no-compiler
;;; Depends on backend definition for object format info...
(load "code:room")
#-no-compiler
(set 'compiler-version
     (concatenate 'string compiler-version
		  "(" *lisp-implementation-version* ")"))
#-no-compiler
(progn
  (setq *info-environment*
	(list* (make-info-environment)
	       (compact-info-environment (first *info-environment*)
					 :name "Compiler")
	       (rest *info-environment*)))
  (purify :root-structures '(compile-file)))

;;; The pretty printer is part of the kernel core, but we can't turn in on
;;; until after the compiler is loaded because it compiles some lambdas
;;; to help with the dispatching.
;;; 
#-no-pp
(pp::pprint-init)

;;; CLX.
;;;
#-no-clx
(load "clx:defsystem")
#-no-clx
(xlib:load-clx (pathname "clx:"))
#-no-clx
(load "code:clx-ext")
#-no-clx
(load "code:inspect")

;;; Hemlock.
;;;
#-no-hemlock
(load "tools:hemload.lisp")
#-no-hemlock
(load "hem:rompsite") ;Contains site-init stuff called at load time.
#-no-hemlock
(hi::build-hemlock)
#-no-hemlock
(set '*hemlock-version*
     (concatenate 'string *hemlock-version* " "
		  "(" *lisp-implementation-version* ")"))

#-(and no-clx no-hemlock)
(purify :root-structures `(ed #-no-hemlock ,hi::*global-command-table*))

;;; PCL.
;;;
#-no-pcl (load "pcl:pclload")


;;; Don't include the search lists used for loading in the resultant core.
;;;
(lisp::clear-all-search-lists)

;;; Okay, build the thing!
;;;
(progn
  ;; We want to be in the USER package when the command line switches run.
  (in-package "USER")
  (setq - nil)
  (setq + nil)
  (setq * nil)
  (setq / nil)
  (setq ++ nil)
  (setq ** nil)
  (setq // nil)
  (setq +++ nil)
  (setq *** nil)
  (setq /// nil)
  (setq *load-verbose* nil)
  (setq *info-environment*
	(list* (make-info-environment :name "Working")
	       (compact-info-environment (first *info-environment*)
					 :name "Auxiliary")
	       (rest *info-environment*)))

  (save-lisp (namestring (merge-pathnames "lisp.core" (default-directory)))
	     :purify t
	     :init-function #'initial-init-function
	     #| :constants (cadr *info-environment*) |#
	     ))
