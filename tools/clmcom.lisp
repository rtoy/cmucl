;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/clmcom.lisp,v 1.5 1993/05/15 18:24:06 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; File for compiling the Motif toolkit and related interface
;;; stuff.
;;;

(in-package "USER")

#-clx
;;; If CLX has not been loaded, but has been compiled, then load it.
;;;
(when (probe-file (make-pathname :defaults "target:clx/clx-library"
				 :type (c:backend-fasl-file-type c:*backend*)))
  (load "target:clx/clx-library"))

(pushnew :motif-toolkit *features*)

(with-compiler-log-file
    ("target:compile-motif.log"
     :optimize '(optimize (speed 3) (safety 1) (ext:inhibit-warnings 3)))

 (comf "target:motif/lisp/initial" :load t)
 (comf "target:motif/lisp/internals" :load t)
 (comf "target:motif/lisp/transport" :load t)
 (comf "target:motif/lisp/events" :load t)
 (comf "target:motif/lisp/conversion" :load t))

(with-compiler-log-file
    ("target:compile-motif.log"
     :optimize '(optimize (speed 2) (ext:inhibit-warnings 2)))

  (comf "target:motif/lisp/interface-glue" :load t)
  (comf "target:motif/lisp/xt-types" :load t)
  (comf "target:motif/lisp/string-base" :load t)
  (comf "target:motif/lisp/prototypes" :load t)
  (comf "target:motif/lisp/interface-build" :load t)
  (comf "target:motif/lisp/callbacks" :load t)
  (comf "target:motif/lisp/widgets" :load t)
  (comf "target:motif/lisp/main" :load t))

(xt::build-toolkit-interface)

;;; Make sure that the INTERFACE package is defined before we start
;;; talking about it.
(with-compiler-log-file
    ("target:compile-motif.log")
  (comf "target:interface/initial" :load t))

(with-compiler-log-file
    ("target:compile-motif.log")

  (comf "target:interface/interface" :load t)
  (comf "target:interface/inspect" :load t)
  ;;
  ;; We don't want to fall into the Motif debugger while compiling.
  ;; It may be that the motifd server hasn't been (re)compiled yet.
  (let ((interface:*interface-style* :tty))
    (comf "target:interface/debug" :load t)))
