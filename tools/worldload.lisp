;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file loads the parts of the system that aren't cold loaded and saves
;;; the resulting core image.  It writes "lisp.core" in the DEFAULT-DIRECTORY.
;;;

;;; Define a bunch of search lists relative to lisp:
;;;
(setf (ext:search-list "code:") '("lisp:code/"))
(setf (ext:search-list "c:") '("lisp:compiler/"))
(setf (ext:search-list "mips:") '("c:mips/"))
(setf (ext:search-list "assem:") '("lisp:assembly/"))
(setf (ext:search-list "hem:") '("lisp:hemlock/"))

;;; This must be here, because it's where assert-user-package is defined.
(load "code:save")

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
(write-string "What is the compiler version? ")
(force-output)
(set 'compiler-version (read-line))
(write-string "What is the Hemlock version? ")
(force-output)
(set '*hemlock-version* (read-line))

;;; Keep us entertained...
(setq *load-verbose* t)

;;; Load random code sources.
;(load "code:lfloatcon")
;(load "code:spirrat")
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
(load "code:weak")
(load "code:sysmacs")
(load "code:pprint")
(load "code:run-program")

;;; Load the compiler.
(load "c:loadcom.lisp")

;;; Load the pretty printer after the compiler, 'cause it compiles stuff
;;; at load time.
(load "code:xp")
(pprint-init)

#|
;;; CLX.
;;;
(load "clx:defsystem")
(load-clx (pathname "clx:"))

;;; A hack to fix a bug in the X11 R3 server.  This should go away when
;;; the server is fixed.
;;;
(load "/afs/cs/project/clisp/systems-work/font-patch")
|#

;;; Stick these before Hemlock.
;;;
(load "code:internet")
(load "code:wire")
(load "code:remote")

;;; Hemlock.
;;;
(load "hem:load-hem.lisp")
(load "hem:rompsite") ;Contains site-init stuff called at load time.
(hi::build-hemlock)

#|
Don't install any dir translations, 'cause we want the real things.

;;; Setup definition editing defaults to look in the stable AFS directory.
;;; The first translation says what we want most clearly, but we require
;;; the others due to symbol links.
;;;
(ed::add-definition-dir-translation "/afs/cs/project/clisp/systems-work/"
				    "/afs/cs/project/clisp/systems/")
(ed::add-definition-dir-translation "/afs/cs/project/clisp-1/systems-work/"
				    "/afs/cs/project/clisp/systems/")
(ed::add-definition-dir-translation
 "/afs/cs.cmu.edu/project/clisp-1/systems-work/"
 "/afs/cs/project/clisp/systems/")
(ed::add-definition-dir-translation
 "/afs/cs.cmu.edu/project/clisp/systems-work/"
 "/afs/cs/project/clisp/systems/")

;;; For some interim time, translate old compilation directories to the new
;;; working directories.  Do it for symbolic links and actual paths.
;;;
(ed::add-definition-dir-translation "/usr/lisp/"
				    "/afs/cs/project/clisp/systems/")
(ed::add-definition-dir-translation "/usr1/lisp/"
				    "/afs/cs/project/clisp/systems/")
(ed::add-definition-dir-translation "/usr2/lisp/"
				    "/afs/cs/project/clisp/systems/")


;;; PCL.
;;;
(load "pcl:defsys")
(pcl::load-pcl)

|#

;;; Load these after PCL.
;;;
;(load "code:inspect")
;(load "code:tty-inspect")


;;; There should be no search lists defined in a full core.
;;;
(clrhash lisp::*search-list-table*)


;;; Okay, build the thing!
;;;
(progn
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
	(list (make-info-environment :name "Working")
	      (compact-info-environment (car *info-environment*))))
  (save-lisp (namestring (merge-pathnames "lisp.core" (default-directory)))
	     :purify t
	     :init-function #'initial-init-function
	     :root-structures `(ed
				,hi::*global-command-table*
				lisp::%top-level
				extensions:save-lisp
				,lisp::fop-codes
				compile-file)))
