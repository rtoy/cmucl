;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/worldload.lisp,v 1.64 1993/08/19 13:18:59 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file loads the parts of the system that aren't cold loaded and saves
;;; the resulting core image.  It writes "lisp.core" in the DEFAULT-DIRECTORY.

;;; Load the rest of the reader (may be byte-compiled.)
(load "target:code/sharpm")
(sharp-init)
(load "target:code/backq")
(backq-init)
(setf *readtable* (copy-readtable std-lisp-readtable))

;;; Overwrite some cold-loaded stuff with byte-compiled versions, if any.
(load "target:code/debug.*bytef" :if-does-not-exist nil)
(load "target:code/defmacro.*bytef" :if-does-not-exist nil) 
(load "target:code/bc-error.fasl" :if-does-not-exist nil)

;;; Define a bunch of search lists relative to target:
;;;
(setf (ext:search-list "code:") '("target:code/"))
(setf (ext:search-list "c:") '("target:compiler/"))
(setf (ext:search-list "vm:")
      '(#+pmax "c:mips/"
        #+sparc "c:sparc/"
	#+rt "c:rt/"
	#+hppa "c:hppa/"
	#+x86 "c:x86/"
	"c:generic/"))
(setf (ext:search-list "assem:")
      '(#+pmax "target:assembly/mips/"
	#+sparc "target:assembly/sparc/"
	#+rt "target:assembly/rt/"
	#+hppa "target:assembly/hppa/"
	#+x86 "target:assembly/x86/"
	"target:assembly/"))
(setf (ext:search-list "hem:") '("target:hemlock/"))
(setf (ext:search-list "clx:") '("target:clx/"))
(setf (ext:search-list "pcl:") '("target:pcl/"))
(setf (ext:search-list "tools:") '("target:tools/"))

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
#-gengc (load "code:purify")
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
(load "code:room")

(defvar *old-ie*)

(setq *old-ie* (car *info-environment*))
(setq *info-environment*
      (list* (make-info-environment)
	     (compact-info-environment (first *info-environment*)
				       :name "Kernel")
	     (rest *info-environment*)))
(lisp::shrink-vector (c::volatile-info-env-table *old-ie*) 0)
(setq *old-ie* nil)

(purify :root-structures
	`(lisp::%top-level extensions:save-lisp ,lisp::fop-codes))

;;; Load the compiler.
#-no-compiler
(progn
  (load "c:loadcom.lisp")
  (setq *old-ie* (car *info-environment*))
  (setq *info-environment*
	(list* (make-info-environment)
	       (compact-info-environment (first *info-environment*)
					 :name "Compiler")
	       (rest *info-environment*)))
  (lisp::shrink-vector (c::volatile-info-env-table *old-ie*) 0)

  (load "c:loadbackend.lisp")
  ;; If we want a small core, blow away the meta-compile time VOP info.
  #+small (setf (c::backend-parsed-vops c:*backend*)
		(make-hash-table :test #'eq))

  (setq *old-ie* (car *info-environment*))
  (setq *info-environment*
	(list* (make-info-environment)
	       (compact-info-environment
		(first *info-environment*)
		:name
		(concatenate 'string (c:backend-name c:*backend*) " backend"))
	       (rest *info-environment*)))

  (lisp::shrink-vector (c::volatile-info-env-table *old-ie*) 0)
  (setq *old-ie* nil))

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
(load "clx:clx-library")

;;; Hemlock.
;;;
#-no-hemlock
(load "target:hemlock/hemlock-library")

;;; PCL.
;;;
#-no-pcl (load "pcl:pclload")
; #+(and no-clm (not (or no-pcl no-clx))) (load "code:inspect")

;;; CLM.
;;;
#-no-clm
(load "target:interface/clm-library")

(defvar *target-sl* (search-list "target:"))

;;; Don't include the search lists used for loading in the resultant core.
;;;
(lisp::clear-all-search-lists)

;;; Set up a default for modules and target:
;;; 
(setf (search-list "modules:") '("./"))
(setf (search-list "target:") *target-sl*)

;; set up the initial info environment.
(setq *old-ie* (car *info-environment*))
(setq *info-environment*
      (list* (make-info-environment :name "Working")
	     (compact-info-environment (first *info-environment*)
				       :name "Auxiliary")
	     (rest *info-environment*)))
(lisp::shrink-vector (c::volatile-info-env-table *old-ie*) 0)
(setq *old-ie* nil)

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
  (save-lisp "lisp.core" :root-structures
	     `(ed #-no-hemlock ,hi::*global-command-table*)))
