;;;
;;; This file compiles all of Hemlock.
;;;

(when (ext:get-command-line-switch "slave")
  (error "Cannot compile Hemlock in a slave due to its clobbering needed
	  typescript routines by renaming the package."))


;;; Blast the old packages in case they are around.  We do this solely to
;;; prove Hemlock can compile cleanly without its having to exist already.
;;;
(when (find-package "ED")
  (rename-package (find-package "ED") "OLD-ED"))
;;;
(when (find-package "HI")
  (rename-package (find-package "HI") "OLD-HI"))


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

(in-package "HEMLOCK-INTERNALS")


(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

(defparameter the-log-file "hem:lossage.log")

(when (probe-file the-log-file)
  (delete-file the-log-file))

(defun cf (file)
  (write-line file)
  (finish-output nil)
  (let ((*error-output* (open the-log-file
			      :direction :output 
			      :if-exists :append
			      :if-does-not-exist :create)))
    (unwind-protect
	(progn
	  (compile-file file :error-file nil)
	  (terpri *error-output*) (terpri *error-output*))
      (close *error-output*))))

(cf "hem:struct.lisp")
(cf "hem:struct-ed.lisp")
(cf "hem:rompsite.lisp")
(cf "hem:charmacs.lisp")
;; keytran and keytrandefs used to be in rompsite, but they are too big now.
;; They also need to go after charmacs due to the funny characters named.
(cf "hem:keytran.lisp")
(cf "hem:keytrandefs.lisp")
(cf "hem:macros.lisp")
(cf "hem:line.lisp")
(cf "hem:ring.lisp")
(cf "hem:table.lisp")
(cf "hem:htext1.lisp")
(cf "hem:htext2.lisp")
(cf "hem:htext3.lisp")
(cf "hem:htext4.lisp")
(cf "hem:search1.lisp")
(cf "hem:search2.lisp")
(cf "hem:linimage.lisp")
(cf "hem:cursor.lisp")
(cf "hem:syntax.lisp")
(cf "hem:winimage.lisp")
(cf "hem:hunk-draw.lisp")
;(cf "hem:bit-stream.lisp")
(cf "hem:termcap.lisp")
(cf "hem:display.lisp")
(cf "hem:bit-display.lisp")
(cf "hem:tty-disp-rt.lisp")
(cf "hem:tty-display.lisp")
;(cf "hem:tty-stream.lisp")
(cf "hem:pop-up-stream.lisp")
(cf "hem:screen.lisp")
(cf "hem:bit-screen.lisp")
(cf "hem:tty-screen.lisp")
(cf "hem:window.lisp")
(cf "hem:font.lisp")
(cf "hem:interp.lisp")
(cf "hem:vars.lisp")
(cf "hem:buffer.lisp")
(cf "hem:files.lisp")
(cf "hem:streams.lisp")
(cf "hem:echo.lisp")
(cf "hem:main.lisp")
(cf "hem:echocoms.lisp")
(cf "hem:defsyn.lisp")
(cf "hem:command.lisp")
(cf "hem:morecoms.lisp")
(cf "hem:undo.lisp")
(cf "hem:killcoms.lisp")
(cf "hem:searchcoms.lisp")
(cf "hem:filecoms.lisp")
(cf "hem:indent.lisp")
(cf "hem:lispmode.lisp")
(cf "hem:comments.lisp")
(cf "hem:fill.lisp")
(cf "hem:text.lisp")
(cf "hem:doccoms.lisp")
(cf "hem:srccom.lisp")
(cf "hem:group.lisp")
(cf "hem:spell-rt.lisp")
(cf "hem:spell-corr.lisp")
(cf "hem:spell-aug.lisp")
(cf "hem:spell-build.lisp")
(cf "hem:spellcoms.lisp")
(cf "hem:abbrev.lisp")
(cf "hem:overwrite.lisp")
(cf "hem:gosmacs.lisp")
(cf "hem:ts-buf.lisp")
(cf "hem:ts-stream.lisp")
(cf "hem:eval-server.lisp")
(cf "hem:lispbuf.lisp")
(cf "hem:lispeval.lisp")
(cf "hem:kbdmac.lisp")
(cf "hem:icom.lisp")
(cf "hem:hi-integrity.lisp")
(cf "hem:ed-integrity.lisp")
(cf "hem:scribe.lisp")
(cf "hem:pascal.lisp")
(cf "hem:edit-defs.lisp")
(cf "hem:auto-save.lisp")
(cf "hem:register.lisp")
(cf "hem:xcoms.lisp")
(cf "hem:unixcoms.lisp")
(cf "hem:mh.lisp")
(cf "hem:highlight.lisp")
(cf "hem:dired.lisp")
(cf "hem:diredcoms.lisp")
(cf "hem:bufed.lisp")
(cf "hem:lisp-lib.lisp")
(cf "hem:completion.lisp")
(cf "hem:shell.lisp")
(cf "hem:bindings.lisp")
