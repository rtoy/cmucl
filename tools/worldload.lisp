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


#| Can't eval conditionals now...
;;; Setup some packages.
;;;
(unless (eq *package* (find-package "USER"))
  (error "Set *package* to the User package and try again."))
|#

(in-package "CLOS" :nicknames '("PCL"))
(in-package "USER" :use '("LISP" "EXTENSIONS" "CONDITIONS" "DEBUG" "CLOS"))
(in-package "HEMLOCK")
(in-package "LISP")
#|
;;; Must load this here, instead of before loading this file, otherwise
;;; SEARCH-LIST is unknown.
;;;
(load "/afs/cs/project/clisp/new-compiler/logical-names.lisp")
|#
;;; Get some data on this core.
;;;
(write-string "What is the current lisp-implementation-version? ")
(set '*lisp-implementation-version* (read-line))
(write-string "What is the compiler version? ")
(set 'compiler-version (read-line))
(write-string "What is the Hemlock version? ")
(set '*hemlock-version* (read-line))

;;;
;;; Keep us entertained...
(setq *load-verbose* t)

(export 'ed)

(load "code:run-program")
(load "code:lfloatcon")
(load "code:spirrat")
(load "code:foreign")
(load "code:format-time")
(load "code:parse-time")
;(load "code:xp-patch")
(load "assem:ropdefs")
(load "assem:rompconst")
(load "assem:disassemble")


(load "c:loadcom.lisp")

(setq lisp::original-lisp-environment NIL)


;;; Load the symbol table information for the Lisp start up code.
;;; Used by CLX for the C routine to connect to the X11 server.
;;;
(load-foreign nil)

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

;;; Stick these after LOAD-FORIEGN but before Hemlock.
;;;
(load "code:internet")
(load "code:wire")
(load "code:remote")

#|
;;; Hemlock.
;;;
(load "hem:rompsite") ;Contains site-init stuff called at load time.
(load "hem:load-hem.lisp")
(hi::build-hemlock)

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
(load "code:tty-inspect")


;;; There should be no search lists defined in a full core.
;;;
(clrhash lisp::*search-list-table*)


;;; Okay, build the thing!
;;;
(in-package "USER")
(progn 
  (setq + NIL)
  (setq * NIL)
  (setq ++ NIL)
  (setq ** NIL)
  (setq +++ NIL)
  (setq *** NIL)
  (setq *load-verbose* nil)
  (setq *info-environment*
	(list (make-info-environment :name "Working")
	      (compact-info-environment (car *info-environment*))))
  (save-lisp (namestring (merge-pathnames "lisp.core" (default-directory)))
	     :purify t
	     :root-structures `(ed
				#|,hi::*global-command-table*|#
				lisp::%top-level
				extensions:save-lisp
				,lisp::fop-codes
				compile-file)
	     :init-function #'(lambda ()
				(gc-on)
				(abort))))
