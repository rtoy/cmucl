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

(with-compiler-log-file ("code:compile-lisp.log")

;;; these guys need to be first.

(comf "code:globals" :always-once t) ; For global variables.
(comf "code:struct" :always-once t) ; For structures.

;;; these guys can supposedly come in any order, but not really.
;;; some are put at the end so macros don't run interpreted and stuff.

(comf "code:serve-event")
(comf "code:lispinit")
(comf "code:error")
(comf "code:alieneval")
(comf "code:stream")
(comf "code:arith")
(comf "code:array")
(comf "code:backq")
(comf "code:c-call")
(comf "code:char")
(comf "code:list")
;(comf "code:clx-ext")
(comf "code:commandline")
(comf "code:eval")
(comf "code:debug")
(comf "code:trace")
(comf "code:extensions")
(comf "code:fd-stream")
(comf "code:fdefinition")
(comf "code:filesys")
(comf "code:format")
(comf "code:hash")
(comf "code:lfloatcon")
(comf "code:load")
(comf "code:miscop")
(comf "code:package")
(comf "code:rompstrops")
(comf "code:pred")
(comf "code:print")
(comf "code:provide")
(comf "code:query")
(comf "code:rand")
(comf "code:reader")
(comf "code:rompnum")
(comf "code:salterror")
(comf "code:save")
(comf "code:search-list")
(comf "code:seq")
(comf "code:sharpm")
(comf "code:sort")
(comf "code:type-boot")
(comf "code:run-program")
(comf "code:spirrat")
(comf "code:xp")
(comf "code:xp-patch")
(comf "code:pprint")
(comf "code:string")
(comf "code:subtypep")
(comf "code:symbol")
(comf "code:syscall")
(comf "code:sysmacs")
(comf "code:time")
(comf "code:foreign")
(comf "c:proclaim")
(comf "c:knownfun")
(comf "code:debug-info")

;;; Later so that miscellaneous structures are defined (not crucial, but nice.)
(comf "code:describe")
;(comf "code:inspect")
(comf "code:tty-inspect")

(comf "code:purify")
(comf "code:gc")
(comf "code:misc")
(comf "code:format-time")
(comf "code:parse-time")

(comf "code:internet")
(comf "code:wire")
(comf "code:remote")

(comf "assem:ropdefs")
(comf "assem:rompconst")
(comf "assem:disassemble")
#+new-compiler
(comf "assem:assem")
#+new-compiler
(comf "assem:assembler")

(comf "code:machdef")
(comf "code:mmlispdefs")
(comf "icode:machdefs")
(comf "icode:netnamedefs")

(let ((system:*alien-eval-when* '(compile eval)))
  (unless (probe-file "icode:machuser.nfasl")
    (load "icode:machmsgdefs.lisp")
    (comf "icode:machuser"))
  
  (unless (probe-file "icode:netnameuser.nfasl")
    (load "icode:netnamemsgdefs.lisp")
    (comf "icode:netnameuser")))

(comf "code:constants")

;;; Compile basic macros that we assume are already in the compilation
;;; environment.  We inhibit compile-time definition to prevent these functions
;;; from becoming interpreted.  In some cases, this is necessary for
;;; compilation to work at all, since the expander functions are lazily
;;; converted: we could go into an infinite recursion trying to convert the
;;; definition of a macro which uses itself.
;;;
(let ((c:*compile-time-define-macros* nil))
  (comf "code:defstruct")
  (comf "code:defmacro")
  (comf "code:macros")
  (comf "code:defrecord")
  
  (comf "c:globaldb"))

); with-compiler-log-file
