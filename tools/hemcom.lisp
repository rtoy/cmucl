;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/hemcom.lisp,v 1.1.1.9 1991/02/14 00:25:14 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file compiles all of Hemlock.
;;;

(c::%proclaim '(optimize (speed 1) (safety 1) (c::brevity 1) (debug-info 2)))
#|
(when (ext:get-command-line-switch "slave")
  (error "Cannot compile Hemlock in a slave due to its clobbering needed
	  typescript routines by renaming the package."))


;;; Blast the old packages in case they are around.  We do this solely to
;;; prove Hemlock can compile cleanly without its having to exist already.
;;;
(unless (find-package "OLD-ED")
  (when (find-package "ED")
    (rename-package (find-package "ED") "OLD-ED"))
  (when (find-package "HI")
    (rename-package (find-package "HI") "OLD-HI")))
|#

;;; Stuff to set up the packages Hemlock uses.
;;;
(in-package "HEMLOCK-INTERNALS"
	    :nicknames '("HI")
	    :use '("LISP" "EXTENSIONS" "SYSTEM"))

;;;
(in-package "HEMLOCK"
	    :nicknames '("ED")
	    :use '("LISP" "HEMLOCK-INTERNALS" "EXTENSIONS" "SYSTEM"))
;;;
(export 'c::compile-from-stream (find-package "C"))


(in-package "USER")

(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

(with-compiler-log-file ("target:compile-hemlock.log")

(comf "target:code/globals" :always-once t)
(comf "target:code/struct" :always-once t)
(comf "target:hemlock/charmacs" :always-once t)
(comf "target:hemlock/key-event" :load t)
(comf "target:hemlock/struct" :always-once t)
;(comf "target:hemlock/struct-ed" :always-once t)
(comf "target:hemlock/rompsite" :always-once t)
;;;
;;; This is necessary since all the #k uses in Hemlock will expand into
;;; EXT:MAKE-KEY-EVENT calls with keysyms and bits from the compiling Lisp, not
;;; for the Lisp new code will run in.  This destroys the compiling Lisp with
;;; respect to running code with #k's compiled for it, but it causes the
;;; compilation to see new keysyms, modifiers, and CLX modifier maps correctly
;;; for the new system.
;;;
(ext::re-initialize-key-events)
(comf "target:hemlock/keysym-defs" :load t)
(comf "target:hemlock/input")
(comf "target:hemlock/macros" :always-once t)
(comf "target:hemlock/line" :always-once t)
(comf "target:hemlock/ring")
(comf "target:hemlock/table")
(comf "target:hemlock/htext1")
(comf "target:hemlock/htext2")
(comf "target:hemlock/htext3")
(comf "target:hemlock/htext4")
(comf "target:hemlock/search1")
(comf "target:hemlock/search2")
(comf "target:hemlock/linimage")
(comf "target:hemlock/cursor")
(comf "target:hemlock/syntax")
(comf "target:hemlock/winimage")
#+clx (comf "target:hemlock/hunk-draw")
;(comf "target:hemlock/bit-stream")
(comf "target:hemlock/termcap")
(comf "target:hemlock/display")
#+clx (comf "target:hemlock/bit-display")
(comf "target:hemlock/tty-disp-rt")
(comf "target:hemlock/tty-display")
;(comf "target:hemlock/tty-stream")
(comf "target:hemlock/pop-up-stream")
(comf "target:hemlock/screen")
#+clx (comf "target:hemlock/bit-screen")
(comf "target:hemlock/tty-screen")
(comf "target:hemlock/window")
(comf "target:hemlock/font")
(comf "target:hemlock/interp")
(comf "target:hemlock/vars")
(comf "target:hemlock/buffer")
(comf "target:hemlock/files")
(comf "target:hemlock/streams")
(comf "target:hemlock/echo")
(comf "target:hemlock/main")
(comf "target:hemlock/echocoms")
(comf "target:hemlock/defsyn")
(comf "target:hemlock/command")
(comf "target:hemlock/morecoms")
(comf "target:hemlock/undo")
(comf "target:hemlock/killcoms")
(comf "target:hemlock/searchcoms")
(comf "target:hemlock/filecoms")
(comf "target:hemlock/indent")
(comf "target:hemlock/lispmode")
(comf "target:hemlock/comments")
(comf "target:hemlock/fill")
(comf "target:hemlock/text")
(comf "target:hemlock/doccoms")
(comf "target:hemlock/srccom")
(comf "target:hemlock/group")
(comf "target:hemlock/spell-rt")
(comf "target:hemlock/spell-corr")
(comf "target:hemlock/spell-aug")
(comf "target:hemlock/spell-build")
(comf "target:hemlock/spellcoms")
(comf "target:hemlock/abbrev")
(comf "target:hemlock/overwrite")
(comf "target:hemlock/gosmacs")
(comf "target:hemlock/ts-buf")
(comf "target:hemlock/ts-stream")
(comf "target:hemlock/eval-server")
(comf "target:hemlock/lispbuf")
(comf "target:hemlock/lispeval")
(comf "target:hemlock/kbdmac")
(comf "target:hemlock/icom")
(comf "target:hemlock/hi-integrity")
(comf "target:hemlock/ed-integrity")
(comf "target:hemlock/scribe")
(comf "target:hemlock/pascal")
(comf "target:hemlock/edit-defs")
(comf "target:hemlock/auto-save")
(comf "target:hemlock/register")
(comf "target:hemlock/xcoms")
(comf "target:hemlock/unixcoms")
(comf "target:hemlock/mh")
(comf "target:hemlock/highlight")
(comf "target:hemlock/dired")
(comf "target:hemlock/diredcoms")
(comf "target:hemlock/bufed")
(comf "target:hemlock/lisp-lib")
(comf "target:hemlock/completion")
(comf "target:hemlock/shell")
(comf "target:hemlock/bindings")
(comf "target:hemlock/hacks")

); With-Compiler-Log-File
