;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/worldload.lisp,v 1.51 1992/12/16 10:50:25 ram Exp $")
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
	#+hppa "c:hppa/"
	#+x86 "c:x86/"
	"c:generic/"))
(setf (ext:search-list "assem:")
      '(#+pmax "lisp:assembly/mips/"
	#+sparc "lisp:assembly/sparc/"
	#+rt "lisp:assembly/rt/"
	#+hppa "lisp:assembly/hppa/"
	#+x86 "lisp:assembly/x86/"
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
(load "code:module")

(setq *info-environment*
      (list* (make-info-environment)
	     (compact-info-environment (first *info-environment*)
				       :name "Kernel")
	     (rest *info-environment*)))
(purify :root-structures
	`(lisp::%top-level extensions:save-lisp ,lisp::fop-codes))

;;; Load the compiler.
#-no-compiler
(progn
  (load "c:loadcom.lisp")
  (setq *info-environment*
	(list* (make-info-environment)
	       (compact-info-environment (first *info-environment*)
					 :name "Compiler")
	       (rest *info-environment*)))
  (load "c:loadbackend.lisp")
  ;; If we want a small core, blow away the meta-compile time VOP info.
  #+small (setf (c::backend-parsed-vops c:*backend*)
		(make-hash-table :test #'eq))
  (setq *info-environment*
	(list* (make-info-environment)
	       (compact-info-environment
		(first *info-environment*)
		:name
		(concatenate 'string (c:backend-name c:*backend*) " backend"))
	       (rest *info-environment*)))
  (purify :root-structures '(compile-file)))

#-no-compiler
;;; Depends on backend definition for object format info...
(load "code:room")

;;; The pretty printer is part of the kernel core, but we can't turn in on
;;; until after the compiler is loaded because it compiles some lambdas
;;; to help with the dispatching.
;;; 
#-no-pp
(pp::pprint-init)

;;; CLX.
;;;
#-no-clx
(load "clx:clx-library")

;;; Hemlock.
;;;
#-no-hemlock
(load "lisp:hemlock/hemlock-library")

#-(and no-clx no-hemlock)
(purify :root-structures `(ed #-no-hemlock ,hi::*global-command-table*))

;;; PCL.
;;;
#-no-pcl (load "pcl:pclload")
#-(or no-pcl no-clx) (load "code:inspect")

;;; Don't include the search lists used for loading in the resultant core.
;;;
(lisp::clear-all-search-lists)

;; set up the initial info environment.
(setq *info-environment*
      (list* (make-info-environment :name "Working")
	     (compact-info-environment (first *info-environment*)
				       :name "Auxiliary")
	     (rest *info-environment*)))

;;; Okay, build the thing!
;;;
(progn
  ;; We want to be in the USER package when the command line switches run.
  (in-package "USER")
  ;; Clean random top-level specials.
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
  ;; 
  ;; Enable the garbage collector.  But first fake it into thinking that
  ;; we don't need to garbage collect.  The save-lisp is going to call purify
  ;; so any garbage will be collected then.
  (setf *need-to-collect-garbage* nil)
  (gc-on)
  ;;
  ;; Save the lisp.
  (save-lisp "lisp.core"))
