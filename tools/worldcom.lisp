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

(setf *new-compile* t)

(with-compiler-log-file ("target:compile-lisp.log")

;;; these guys need to be first.

(comf "target:code/globals" :always-once t) ; For global variables.
(comf "target:code/struct" :always-once t) ; For structures.


;;; Assembly files.
(comf "target:assembly/assem-rtns" :assem t)
(comf "target:assembly/array" :assem t)
(comf "target:assembly/bit-bash" :assem t)

(comf "target:compiler/type")
(comf "target:compiler/mips/vm-type")
(comf "target:compiler/type-init")

(comf "target:code/serve-event")
(comf "target:code/lispinit")
(comf "target:code/error")
(comf "target:code/alieneval")
(comf "target:code/stream")
(comf "target:code/arith")
(comf "target:code/array")
(comf "target:code/backq")
(comf "target:code/c-call")
(comf "target:code/char")
(comf "target:code/list")
;(comf "target:code/clx-ext")
(comf "target:code/commandline")
(comf "target:code/eval")
(comf "target:code/debug")
(comf "target:code/trace")
(comf "target:code/extensions")
(comf "target:code/fd-stream")
(comf "target:code/fdefinition")
(comf "target:code/filesys")
(comf "target:code/format")
(comf "target:code/hash")
;(comf "target:code/lfloatcon")
(comf "target:code/load")
;(comf "target:code/miscop")
(comf "target:code/package")
(comf "target:code/mipsstrops")
(comf "target:code/pred")
(comf "target:code/print")
(comf "target:code/provide")
(comf "target:code/query")
(comf "target:code/rand")
(comf "target:code/reader")
(comf "target:code/mipsnum")
(comf "target:code/salterror")
;(comf "target:code/save")
(comf "target:code/search-list")
(comf "target:code/seq")
(comf "target:code/sharpm")
(comf "target:code/sort")
(comf "target:code/type-boot")
(comf "target:code/run-program")
;(comf "target:code/spirrat")
;(comf "target:code/xp")
;(comf "target:code/xp-patch")
;(comf "target:code/pprint")
(comf "target:code/string")
(comf "target:code/symbol")
(comf "target:code/syscall")
(comf "target:code/sysmacs")
(comf "target:code/time")
;(comf "target:code/foreign")
(comf "target:compiler/proclaim")
(comf "target:compiler/knownfun")
(comf "target:code/debug-info")

;;; Later so that miscellaneous structures are defined (not crucial, but nice.)
(comf "target:code/describe")
;(comf "target:code/inspect")
(comf "target:code/tty-inspect")

;(comf "target:code/purify")
;(comf "target:code/gc")
(comf "target:code/misc")
(comf "target:code/format-time")
(comf "target:code/parse-time")

(comf "target:code/internet")
(comf "target:code/wire")
(comf "target:code/remote")


;(comf "target:code/constants")

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
  (comf "target:code/macros")
  (comf "target:code/defrecord")
  
  (comf "target:compiler/globaldb"))

); with-compiler-log-file
