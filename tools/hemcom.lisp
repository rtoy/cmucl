;;;
;;; This file compiles all of Hemlock.
;;;

(c::%proclaim '(optimize (debug-info 2)))
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
(in-package "SYSTEM")
(export '(%sp-byte-blt %sp-find-character %sp-find-character-with-attribute
		       %sp-reverse-find-character-with-attribute))

(in-package "C")
(export 'compile-from-stream)

(in-package "USER")

(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

(with-compiler-log-file ("hem:lossage.log")

(comf "code:globals" :always-once t)
(comf "code:struct" :always-once t)
(comf "hem:struct" :always-once t)
(comf "hem:struct-ed" :always-once t)
(comf "hem:rompsite" :always-once t)
(comf "hem:charmacs" :always-once t)
(comf "hem:key-event" :load t)
;;;
;;; This is necessary since all the #k uses in Hemlock will expand into
;;; EXT:MAKE-KEY-EVENT calls with keysyms and bits from the compiling Lisp, not
;;; for the Lisp new code will run in.  This destroys the compiling Lisp with
;;; respect to running code with #k's compiled for it, but it causes the
;;; compilation to see new keysyms, modifiers, and CLX modifier maps correctly
;;; for the new system.
;;;
(ext::re-initialize-key-events)
(comf "hem:keysym-defs" :load t)
(comf "hem:input.lisp")
(comf "hem:macros" :always-once t)
(comf "hem:line" :always-once t)
(comf "hem:ring")
(comf "hem:table")
(comf "hem:htext1")
(comf "hem:htext2")
(comf "hem:htext3")
(comf "hem:htext4")
(comf "hem:search1")
(comf "hem:search2")
(comf "hem:linimage")
(comf "hem:cursor")
(comf "hem:syntax")
(comf "hem:winimage")
;(comf "hem:hunk-draw")
;(comf "hem:bit-stream")
(comf "hem:termcap")
(comf "hem:display")
;(comf "hem:bit-display")
(comf "hem:tty-disp-rt")
(comf "hem:tty-display")
;(comf "hem:tty-stream")
(comf "hem:pop-up-stream")
(comf "hem:screen")
;(comf "hem:bit-screen")
(comf "hem:tty-screen")
(comf "hem:window")
(comf "hem:font")
(comf "hem:interp")
(comf "hem:vars")
(comf "hem:buffer")
(comf "hem:files")
(comf "hem:streams")
(comf "hem:echo")
(comf "hem:main")
(comf "hem:echocoms")
(comf "hem:defsyn")
(comf "hem:command")
(comf "hem:morecoms")
(comf "hem:undo")
(comf "hem:killcoms")
(comf "hem:searchcoms")
(comf "hem:filecoms")
(comf "hem:indent")
(comf "hem:lispmode")
(comf "hem:comments")
(comf "hem:fill")
(comf "hem:text")
(comf "hem:doccoms")
(comf "hem:srccom")
(comf "hem:group")
(comf "hem:spell-rt")
(comf "hem:spell-corr")
(comf "hem:spell-aug")
(comf "hem:spell-build")
(comf "hem:spellcoms")
(comf "hem:abbrev")
(comf "hem:overwrite")
(comf "hem:gosmacs")
(comf "hem:ts-buf")
(comf "hem:ts-stream")
(comf "hem:eval-server")
(comf "hem:lispbuf")
(comf "hem:lispeval")
(comf "hem:kbdmac")
(comf "hem:icom")
(comf "hem:hi-integrity")
(comf "hem:ed-integrity")
(comf "hem:scribe")
(comf "hem:pascal")
(comf "hem:edit-defs")
(comf "hem:auto-save")
(comf "hem:register")
(comf "hem:xcoms")
(comf "hem:unixcoms")
(comf "hem:mh")
(comf "hem:highlight")
(comf "hem:dired")
(comf "hem:diredcoms")
(comf "hem:bufed")
(comf "hem:lisp-lib")
(comf "hem:completion")
(comf "hem:shell")
(comf "hem:bindings")
(comf "hem:hacks")

); With-Compiler-Log-File
