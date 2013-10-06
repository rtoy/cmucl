;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header: src/tools/hemcom.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file compiles all of Hemlock.
;;;

(when (boundp 'conditions::*make-condition-accessor-methods*)
  (setq conditions::*make-condition-accessor-methods* t))

#+bootstrap
(progn
  (when (ext:get-command-line-switch "slave")
    (error "Cannot compile Hemlock in a slave due to its clobbering needed
    typescript routines by renaming the package."))
  
  ;;; Blast the old packages in case they are around.  We do this solely to
  ;;; prove Hemlock can compile cleanly without its having to exist already.
  ;;;
  (copy-packages '("ED" "HI")))


;;; Stuff to set up the packages Hemlock uses.
;;;
(unless (find-package "HEMLOCK-INTERNALS")
  (make-package "HEMLOCK-INTERNALS"
		:nicknames '("HI")
		:use '("LISP" "EXTENSIONS" "SYSTEM")))

(defpackage "HEMLOCK-INTERNALS"
  (:nicknames "HI")
  (:export
   "*BUFFER-LIST*" "*BUFFER-NAMES*" "*CHARACTER-ATTRIBUTE-NAMES*"
   "*COMMAND-NAMES*" "*CREATE-INITIAL-WINDOWS-HOOK*" "*CREATE-WINDOW-HOOK*"
   "*DELETE-WINDOW-HOOK*" "*ECHO-AREA-BUFFER*" "*ECHO-AREA-STREAM*"
   "*ECHO-AREA-WINDOW*" "*EDITOR-INPUT*" "*GLOBAL-VARIABLE-NAMES*"
   "*INPUT-TRANSCRIPT*" "*INVOKE-HOOK*" "*KEY-EVENT-HISTORY*"
   "*LAST-KEY-EVENT-TYPED*" "*LOGICAL-KEY-EVENT-NAMES*" "*MODE-NAMES*"
   "*PARSE-DEFAULT*" "*PARSE-DEFAULT-STRING*" "*PARSE-HELP*"
   "*PARSE-INPUT-REGION*" "*PARSE-PROMPT*" "*PARSE-STARTING-MARK*"
   "*PARSE-STRING-TABLES*" "*PARSE-TYPE*" "*PARSE-VALUE-MUST-EXIST*"
   "*PARSE-VERIFICATION-FUNCTION*" "*PRINT-REGION*"
   "*RANDOM-TYPEOUT-BUFFERS*" "*RANDOM-TYPEOUT-HOOK*" "*REAL-EDITOR-INPUT*"
   "*WINDOW-LIST*" "ABORT-RECURSIVE-EDIT" "ADD-HOOK"
   "AFTER-EDITOR-INITIALIZATIONS" "BIND-KEY" "BLANK-AFTER-P"
   "BLANK-BEFORE-P" "BLANK-LINE-P" "BUFFER" "BUFFER-DELETE-HOOK"
   "BUFFER-END" "BUFFER-END-MARK" "BUFFER-MAJOR-MODE" "BUFFER-MINOR-MODE"
   "BUFFER-MODELINE-FIELD-P" "BUFFER-MODELINE-FIELDS" "BUFFER-MODES"
   "BUFFER-MODIFIED" "BUFFER-NAME" "BUFFER-PATHNAME" "BUFFER-POINT"
   "BUFFER-REGION" "BUFFER-SIGNATURE" "BUFFER-START" "BUFFER-START-MARK"
   "BUFFER-VARIABLES" "BUFFER-WINDOWS" "BUFFER-WRITABLE" "BUFFER-WRITE-DATE"
   "BUFFERP" "CENTER-WINDOW" "CHARACTER-ATTRIBUTE"
   "CHARACTER-ATTRIBUTE-DOCUMENTATION" "CHARACTER-ATTRIBUTE-HOOKS"
   "CHARACTER-ATTRIBUTE-NAME" "CHARACTER-ATTRIBUTE-P" "CHARACTER-OFFSET"
   "CLEAR-ECHO-AREA" "CLEAR-EDITOR-INPUT" "CLRSTRING" "COMMAND"
   "COMMAND-BINDINGS" "COMMAND-CASE" "COMMAND-DOCUMENTATION"
   "COMMAND-FUNCTION" "COMMAND-NAME" "COMMANDP" "COMPLETE-STRING"
   "COPY-MARK" "COPY-REGION" "COUNT-CHARACTERS" "COUNT-LINES"
   "CURRENT-BUFFER" "CURRENT-POINT" "CURRENT-VARIABLE-TABLES"
   "CURRENT-WINDOW" "CURSORPOS-TO-MARK" "DEFATTRIBUTE" "DEFAULT-FONT"
   "DEFCOMMAND" "DEFHVAR" "DEFINE-LOGICAL-KEY-EVENT" "DEFINE-TTY-FONT"
   "DEFMODE" "DELETE-AND-SAVE-REGION" "DELETE-BUFFER" "DELETE-CHARACTERS"
   "DELETE-FONT-MARK" "DELETE-KEY-BINDING" "DELETE-LINE-FONT-MARKS"
   "DELETE-MARK" "DELETE-REGION" "DELETE-STRING" "DELETE-VARIABLE"
   "DELETE-WINDOW" "DIRECTORYP" "DISPLAYED-P" "DO-ALPHA-CHARS" "DO-STRINGS"
   "EDITOR-DESCRIBE-FUNCTION" "EDITOR-ERROR" "EDITOR-ERROR-FORMAT-ARGUMENTS"
   "EDITOR-ERROR-FORMAT-STRING" "EDITOR-FINISH-OUTPUT" "EDITOR-SLEEP"
   "EMPTY-LINE-P" "END-LINE-P" "ENTER-WINDOW-AUTORAISE" "EXIT-HEMLOCK"
   "EXIT-RECURSIVE-EDIT" "FETCH-CUT-STRING" "FILTER-REGION" "FIND-AMBIGUOUS"
   "FIND-ATTRIBUTE" "FIND-CONTAINING" "FIND-PATTERN" "FIRST-LINE-P"
   "FONT-MARK" "FUN-DEFINED-FROM-PATHNAME" "GET-COMMAND" "GET-KEY-EVENT"
   "GETSTRING" "HANDLE-LISP-ERRORS" "HEMLOCK-BOUND-P"
   "HEMLOCK-OUTPUT-STREAM" "HEMLOCK-OUTPUT-STREAM-P" "HEMLOCK-REGION-STREAM"
   "HEMLOCK-REGION-STREAM-P" "HLET" "IN-RECURSIVE-EDIT" "INPUT-WAITING"
   "INSERT-CHARACTER" "INSERT-REGION" "INSERT-STRING" "INVOKE-HOOK"
   "KEY-TRANSLATION" "LAST-COMMAND-TYPE" "LAST-KEY-EVENT-CURSORPOS"
   "LAST-LINE-P" "LINE" "LINE-BUFFER" "LINE-CHARACTER" "LINE-END"
   "LINE-LENGTH" "LINE-NEXT" "LINE-OFFSET" "LINE-PLIST" "LINE-PREVIOUS"
   "LINE-SIGNATURE" "LINE-START" "LINE-STRING" "LINE-TO-REGION" "LINE<"
   "LINE<=" "LINE>" "LINE>=" "LINEP" "LINES-RELATED" "LISTEN-EDITOR-INPUT"
   "LOGICAL-KEY-EVENT-DOCUMENTATION" "LOGICAL-KEY-EVENT-KEY-EVENTS"
   "LOGICAL-KEY-EVENT-NAME" "LOGICAL-KEY-EVENT-P" "LOUD-MESSAGE"
   "MAKE-BUFFER" "MAKE-COMMAND" "MAKE-EMPTY-REGION"
   "MAKE-HEMLOCK-OUTPUT-STREAM" "MAKE-HEMLOCK-REGION-STREAM"
   "MAKE-KBDMAC-STREAM" "MAKE-MODELINE-FIELD" "MAKE-RING"
   "MAKE-STRING-TABLE" "MAKE-WINDOW" "MAKE-XWINDOW-LIKE-HWINDOW"
   "MAP-BINDINGS" "MARK" "MARK-AFTER" "MARK-BEFORE" "MARK-CHARPOS"
   "MARK-COLUMN" "MARK-KIND" "MARK-LINE" "MARK-TO-CURSORPOS" "MARK/="
   "MARK<" "MARK<=" "MARK=" "MARK>" "MARK>=" "MARKP"
   "MERGE-RELATIVE-PATHNAMES" "MESSAGE" "MODE-DOCUMENTATION" "MODE-MAJOR-P"
   "MODE-VARIABLES" "MODELINE-FIELD" "MODELINE-FIELD-FUNCTION"
   "MODELINE-FIELD-NAME" "MODELINE-FIELD-P" "MODELINE-FIELD-WIDTH"
   "MODIFY-KBDMAC-STREAM" "MOVE-FONT-MARK" "MOVE-MARK" "MOVE-TO-COLUMN"
   "MOVE-TO-POSITION" "NEW-SEARCH-PATTERN" "NEXT-CHARACTER" "NEXT-WINDOW"
   "NINSERT-REGION" "PAUSE-HEMLOCK" "PREFIX-ARGUMENT" "PREVIOUS-CHARACTER"
   "PREVIOUS-WINDOW" "PROMPT-FOR-BUFFER" "PROMPT-FOR-EXPRESSION"
   "PROMPT-FOR-FILE" "PROMPT-FOR-INTEGER" "PROMPT-FOR-KEY"
   "PROMPT-FOR-KEY-EVENT" "PROMPT-FOR-KEYWORD" "PROMPT-FOR-STRING"
   "PROMPT-FOR-VARIABLE" "PROMPT-FOR-Y-OR-N" "PROMPT-FOR-YES-OR-NO"
   "READ-FILE" "RECURSIVE-EDIT" "REDISPLAY" "REDISPLAY-ALL" "REGION"
   "REGION-BOUNDS" "REGION-END" "REGION-START" "REGION-TO-STRING" "REGIONP"
   "REMOVE-HOOK" "REMOVE-SCHEDULED-EVENT" "REPLACE-PATTERN" "REPROMPT"
   "REVERSE-FIND-ATTRIBUTE" "RING" "RING-LENGTH" "RING-POP" "RING-PUSH"
   "RING-REF" "RINGP" "ROTATE-RING" "SAME-LINE-P" "SCHEDULE-EVENT"
   "SCROLL-WINDOW" "SEARCH-CHAR-CODE-LIMIT" "SEARCH-PATTERN"
   "SEARCH-PATTERN-P" "SET-REGION-BOUNDS" "SETV" "SHADOW-ATTRIBUTE"
   "SHOW-MARK" "START-LINE-P" "STORE-CUT-STRING" "STRING-TABLE"
   "STRING-TABLE-P" "STRING-TABLE-SEPARATOR" "STRING-TO-REGION"
   "STRING-TO-VARIABLE" "SYNTAX-CHAR-CODE-LIMIT" "UNGET-KEY-EVENT"
   "UNSHADOW-ATTRIBUTE" "UPDATE-MODELINE-FIELD" "UPDATE-MODELINE-FIELDS"
   "USE-BUFFER" "VALUE" "VARIABLE-DOCUMENTATION" "VARIABLE-HOOKS"
   "VARIABLE-NAME" "VARIABLE-VALUE" "WINDOW" "WINDOW-BUFFER"
   "WINDOW-DISPLAY-END" "WINDOW-DISPLAY-RECENTERING" "WINDOW-DISPLAY-START"
   "WINDOW-FONT" "WINDOW-HEIGHT" "WINDOW-POINT" "WINDOW-WIDTH" "WINDOWP"
   "WITH-INPUT-FROM-REGION" "WITH-MARK" "WITH-OUTPUT-TO-MARK"
   "WITH-POP-UP-DISPLAY" "WITH-WRITABLE-BUFFER" "WRITE-FILE"))

(unless (find-package "HEMLOCK")
  (make-package "HEMLOCK"
		:nicknames '("ED")
		:use '("LISP" "HEMLOCK-INTERNALS" "EXTENSIONS" "SYSTEM")))
;;;
(export 'c::compile-from-stream (find-package "C"))


(in-package "CL-USER")

(defvar *byte-compile* #+small t #-small :maybe)

(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

#-clx
;;; If CLX has not been loaded, but has been compiled, then load it.
;;;
(when (probe-file (make-pathname :defaults "target:clx/clx-library"
				 :type (c:backend-fasl-file-type c:*backend*)))
  #+(and (not pcl) (not no-pcl-clx))
  (load "target:pcl/pclload")
  (load "target:clx/clx-library")
  #+gencgc (gc :full t)
  #-gencgc (ext:purify))
  
(with-compiler-log-file
    ("target:compile-hemlock.log"
     :optimize
     '(optimize (debug #-small 2 #+small .5) 
		(speed 2) (inhibit-warnings 2)
		(safety #-small 1 #+small 0))
     :optimize-interface
     '(optimize-interface (debug .5))
     :context-declarations
     '(((:or :external (:match "$%SET-"))
	(declare (optimize (safety 2))
		 (optimize-interface (debug 1))))
       (:macro (declare (optimize (speed 0))))))

(comf "target:code/globals")
(comf "target:code/struct")
(comf "target:hemlock/charmacs")
(comf "target:hemlock/key-event" :load t)
(comf "target:hemlock/struct")
;(comf "target:hemlock/struct-ed")
(comf "target:hemlock/rompsite")
;;;
;;; This is necessary since all the #k uses in Hemlock will expand into
;;; EXT:MAKE-KEY-EVENT calls with keysyms and bits from the compiling Lisp, not
;;; for the Lisp new code will run in.  This destroys the compiling Lisp with
;;; respect to running code with #k's compiled for it, but it causes the
;;; compilation to see new keysyms, modifiers, and CLX modifier maps correctly
;;; for the new system.
;;;
(ext::re-initialize-key-events)
(comf "target:hemlock/keysym-defs")
(comf "target:hemlock/input")
(comf "target:hemlock/macros" :byte-compile t)
(comf "target:hemlock/line")
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
(with-compilation-unit (:optimize '(optimize (safety 2) (debug 3)))
  (comf "target:hemlock/tty-display")) ; Buggy...
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
(comf "target:hemlock/echo" :byte-compile t)
(comf "target:hemlock/main" :byte-compile t)
(comf "target:hemlock/echocoms" :byte-compile t)
(comf "target:hemlock/defsyn")

(comf "target:hemlock/ts-buf")
(comf "target:hemlock/ts-stream")

(with-compilation-unit
    (:optimize
     '(optimize (safety 2) (speed 0))
     :context-declarations
     '(((:match "-COMMAND$")
	(declare (optimize (safety #+small 0 #-small 1))
		 (optimize-interface (safety 2))))))

(comf "target:hemlock/command" :byte-compile t)
(comf "target:hemlock/morecoms" :byte-compile t)
(comf "target:hemlock/undo" :byte-compile t)
(comf "target:hemlock/killcoms" :byte-compile t)
(comf "target:hemlock/searchcoms" :byte-compile t)
(comf "target:hemlock/filecoms" :byte-compile t)
(comf "target:hemlock/indent" :byte-compile t)
(comf "target:hemlock/lispmode")
(comf "target:hemlock/comments" :byte-compile t)
(comf "target:hemlock/fill")
(comf "target:hemlock/text" :byte-compile t)
(comf "target:hemlock/doccoms" :byte-compile t)
(comf "target:hemlock/srccom" :byte-compile t)
(comf "target:hemlock/abbrev" :byte-compile t)
(comf "target:hemlock/group" :byte-compile t)
(comf "target:hemlock/overwrite" :byte-compile t)
(comf "target:hemlock/gosmacs" :byte-compile t)
(comf "target:hemlock/eval-server" :byte-compile t)
(comf "target:hemlock/dylan" :byte-compile t)
(comf "target:hemlock/lispbuf" :byte-compile t)
(comf "target:hemlock/lispeval" :byte-compile t)
(comf "target:hemlock/icom" :byte-compile t)
(comf "target:hemlock/hi-integrity" :byte-compile t)
(comf "target:hemlock/ed-integrity" :byte-compile t)
(comf "target:hemlock/scribe" :byte-compile t)
(comf "target:hemlock/pascal" :byte-compile t)
(comf "target:hemlock/edit-defs" :byte-compile t)
(comf "target:hemlock/auto-save" :byte-compile t)
(comf "target:hemlock/register" :byte-compile t)
(comf "target:hemlock/xcoms" :byte-compile t)
(comf "target:hemlock/unixcoms" :byte-compile t)
(comf "target:hemlock/mh")
(comf "target:hemlock/highlight" :byte-compile t)
(comf "target:hemlock/dired" :byte-compile t)
(comf "target:hemlock/diredcoms" :byte-compile t)
(comf "target:hemlock/bufed" :byte-compile t)
(comf "target:hemlock/lisp-lib" :byte-compile t)
(comf "target:hemlock/completion" :byte-compile t)
(comf "target:hemlock/shell" :byte-compile t)
(comf "target:hemlock/debug" :byte-compile t)
(comf "target:hemlock/netnews" :byte-compile t)
(comf "target:hemlock/rcs" :byte-compile t)
(comf "target:hemlock/dabbrev" :byte-compile t)

) ;WITH-COMPILATION-UNIT for commands

;; Stuff we want compiled native:

(comf "target:hemlock/spell-rt")
(comf "target:hemlock/spell-corr")
(comf "target:hemlock/spell-aug")
(comf "target:hemlock/spell-build")
(comf "target:hemlock/spellcoms")
(comf "target:hemlock/kbdmac")

(comf "target:hemlock/bindings")
(comf "target:hemlock/hacks")

) ;WITH-COMPILER-LOG-FILE

(unless (probe-file "target:hemlock/spell-dictionary.bin")
  (load "target:hemlock/spell-rt")
  (load "target:hemlock/spell-corr")
  (load "target:hemlock/spell-aug")
  (load "target:hemlock/spell-build")
  (funcall (fdefinition (intern "BUILD-DICTIONARY" "SPELL"))
	   "target:hemlock/spell-dictionary.text"
	   "target:hemlock/spell-dictionary.bin"))

(cat-if-anything-changed
 "target:hemlock/hemlock-library"
 "target:hemlock/rompsite"
 "target:hemlock/struct"
 ; "target:hemlock/struct-ed"
 "target:hemlock/charmacs"
 "target:hemlock/input"
 "target:hemlock/line"
 "target:hemlock/ring"
 "target:hemlock/vars"
 "target:hemlock/buffer"
 "target:hemlock/macros"
 "target:hemlock/interp"
 "target:hemlock/syntax"
 "target:hemlock/htext1"
 "target:hemlock/htext2"
 "target:hemlock/htext3"
 "target:hemlock/htext4"
 "target:hemlock/files"
 "target:hemlock/search1"
 "target:hemlock/search2"
 "target:hemlock/table"
 #+clx "target:hemlock/hunk-draw"
 "target:hemlock/window"
 "target:hemlock/screen"
 "target:hemlock/winimage"
 "target:hemlock/linimage"
 "target:hemlock/display"
 "target:hemlock/termcap"
 #+clx "target:hemlock/bit-display"
 "target:hemlock/tty-disp-rt"
 "target:hemlock/tty-display"
 "target:hemlock/pop-up-stream"
 #+clx "target:hemlock/bit-screen"
 "target:hemlock/tty-screen"
 "target:hemlock/cursor"
 "target:hemlock/font"
 "target:hemlock/streams"
 "target:hemlock/hacks"
 "target:hemlock/main"
 "target:hemlock/echo"
 "target:hemlock/echocoms"
 "target:hemlock/command"
 "target:hemlock/indent"
 "target:hemlock/comments"
 "target:hemlock/morecoms"
 "target:hemlock/undo"
 "target:hemlock/killcoms"
 "target:hemlock/searchcoms"
 "target:hemlock/filecoms"
 "target:hemlock/doccoms"
 "target:hemlock/srccom"
 "target:hemlock/group"
 "target:hemlock/fill"
 "target:hemlock/text"
 "target:hemlock/lispmode"
 "target:hemlock/ts-buf"
 "target:hemlock/ts-stream"
 "target:hemlock/eval-server"
 "target:hemlock/lispbuf"
 "target:hemlock/lispeval"
 "target:hemlock/spell-rt"
 "target:hemlock/spell-corr"
 "target:hemlock/spell-aug"
 "target:hemlock/spellcoms"
 "target:hemlock/overwrite"
 "target:hemlock/abbrev"
 "target:hemlock/icom"
 "target:hemlock/kbdmac"
 "target:hemlock/defsyn"
 "target:hemlock/scribe"
 "target:hemlock/pascal"
 "target:hemlock/dylan"
 "target:hemlock/edit-defs"
 "target:hemlock/auto-save"
 "target:hemlock/register"
 "target:hemlock/xcoms"
 "target:hemlock/unixcoms"
 "target:hemlock/mh"
 "target:hemlock/highlight"
 "target:hemlock/dired"
 "target:hemlock/diredcoms"
 "target:hemlock/bufed"
 "target:hemlock/lisp-lib"
 "target:hemlock/completion"
 "target:hemlock/shell"
 "target:hemlock/debug"
 "target:hemlock/netnews"
 "target:hemlock/rcs"
 "target:hemlock/dabbrev"
 "target:hemlock/bindings")
