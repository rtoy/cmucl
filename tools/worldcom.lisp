;;; -*- Package: User; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************

(in-package "USER")

(with-compiler-log-file
    ("target:compile-lisp.log"
     :optimize '(optimize (speed 2) (space 2) (inhibit-warnings 2)
			  (debug-info #-small 2 #+small 1)
			  (safety #-small 1 #+small 0))
     :optimize-interface '(optimize-interface (safety #-small 2 #+small 1)
					      (debug-info 1))
     :context-declarations
     '(((:or :external (:and (:match "%") (:match "SET")))
	(declare (optimize-interface (safety 2))))
       ((:or (:and :external :macro)
	     (:match "$PARSE-"))
	(declare (optimize (safety 2))))
       ((:and :external (:match "LIST"))
	(declare (optimize (safety 1))))))

(let ((*features*
       (cons (intern (c:backend-name c:*backend*)
		     (find-package "KEYWORD"))
	     (remove-if #'(lambda (x)
			    (member x '(:pmax :sparc :rt)))
			*features*))))


;;; these guys need to be first.
(comf "target:code/struct") ; For structures.

;;; Assembly files.
(when (string= (c:backend-name c:*backend*) "PMAX")
  (comf "target:assembly/mips/assem-rtns" :assem t)
  (comf "target:assembly/mips/array" :assem t)
  (comf "target:assembly/mips/bit-bash" :assem t)
  (comf "target:assembly/mips/arith" :assem t)
  (comf "target:assembly/mips/alloc" :assem t))

(when (string= (c:backend-name c:*backend*) "SPARC")
  (comf "target:assembly/sparc/assem-rtns" :assem t)
  (comf "target:assembly/sparc/array" :assem t)
  (comf "target:assembly/sparc/bit-bash" :assem t)
  (comf "target:assembly/sparc/arith" :assem t)
  (comf "target:assembly/sparc/alloc" :assem t))

(when (string= (c:backend-name c:*backend*) "RT")
  (comf "target:assembly/rt/assem-rtns" :assem t)
  (comf "target:assembly/rt/array" :assem t)
  (comf "target:assembly/rt/arith" :assem t)
  (comf "target:assembly/rt/alloc" :assem t))

;;; these guys can supposedly come in any order, but not really.
;;; some are put at the end so macros don't run interpreted and stuff.

(comf "target:code/kernel")
(comf "target:code/lispinit")
(comf "target:code/fdefinition")

(comf "target:code/error")
(comf "target:code/salterror")

(comf "target:compiler/type")
(comf "target:compiler/generic/vm-type")
(comf "target:compiler/type-init")
(comf "target:code/pred")

(comf "target:code/alieneval")
(comf "target:code/c-call")

(comf "target:code/bit-bash")
(comf "target:code/array")
(comf "target:code/hash")

(with-compilation-unit
  (:optimize '(optimize (safety 1)))
  (comf "target:code/list")
  (comf "target:code/seq")) ; seq must come after list

(comf "target:code/string")
(comf "target:code/mipsstrops")

(comf "target:code/machdef")

(when (string= (c:backend-name c:*backend*) "PMAX")
  (comf "target:code/pmax-machdef")
  (comf "target:code/pmax-vm")
  (comf "target:code/pmax-disassem"))
(when (string= (c:backend-name c:*backend*) "SPARC")
  (comf "target:code/sparc-machdef")
  (comf "target:code/sparc-vm"))
(when (string= (c:backend-name c:*backend*) "RT")
  (comf "target:code/rt-machdef")
  (comf "target:code/rt-vm"))

(comf "target:code/symbol")
(comf "target:code/bignum")
(comf "target:code/numbers")
(comf "target:code/float-trap")
(comf "target:code/float")
(comf "target:code/irrat")

(comf "target:code/type-boot")

(comf "target:compiler/proclaim")

(comf "target:code/char")
(comf "target:code/misc")
(comf "target:code/extensions")
(comf "target:code/commandline")

(comf "target:code/room")
(comf "target:code/gc")
(comf "target:code/purify")
(comf "target:code/save")

(comf "target:code/stream")
(comf "target:code/print")
(comf "target:code/pprint")
(comf "target:code/format")

(comf "target:code/package")
(comf "target:code/reader")
(comf "target:code/sharpm")
(comf "target:code/backq")

(comf "target:code/syscall")
(comf "target:code/vm")
(comf "target:code/serve-event")
(pushnew :serve-event *features*)
(comf "target:code/fd-stream")
(comf "target:code/filesys")
(comf "target:code/search-list")
(comf "target:code/load")

(comf "target:code/eval")

(comf "target:code/signal")
(comf "target:code/interr")
(comf "target:code/debug-info")
(comf "target:code/debug-int")
(comf "target:code/debug")

(comf "target:code/query")
(comf "target:code/rand")
(comf "target:code/trace")
(comf "target:code/profile")
(comf "target:code/sort")
(comf "target:code/sysmacs")
(comf "target:code/time")
(comf "target:code/weak")

;;; Later so that miscellaneous structures are defined (not crucial, but nice.)
(comf "target:code/describe")
;(comf "target:code/inspect")
(comf "target:code/tty-inspect")

(comf "target:code/format-time")
(comf "target:code/parse-time")
(comf "target:code/run-program")

(comf "target:code/loop")

;;; XP won't cross-compile.
#+new-compiler
(comf "target:code/xp")

#+clx
(comf "target:code/clx-ext")

(comf "target:code/foreign")
(comf "target:code/internet")
(comf "target:code/wire")
(comf "target:code/remote")

(comf "target:code/setf-funs")

;;; Compile basic macros that we assume are already in the compilation
;;; environment.  We inhibit compile-time definition to prevent these functions
;;; from becoming interpreted.  In some cases, this is necessary for
;;; compilation to work at all, since the expander functions are lazily
;;; converted: we could go into an infinite recursion trying to convert the
;;; definition of a macro which uses itself.
;;;
(let ((c:*compile-time-define-macros* nil))
  (comf "target:code/defstruct")
  (comf "target:code/defmacro")
  (comf "target:code/defrecord")
  (comf "target:compiler/globaldb")
  ;; We can't compile anything after macros, 'cause it breaks the running lisp.
  (comf "target:code/macros"))


); let

); with-compiler-log-file
