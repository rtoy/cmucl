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

(with-compiler-log-file ("ncode:compile-lisp.log")

;;; these guys need to be first.

(comf "ncode:globals" :always-once t) ; For global variables.
(comf "ncode:struct" :always-once t) ; For structures.

;;; these guys can supposedly come in any order, but not really.
;;; some are put at the end so macros don't run interpreted and stuff.

(comf "ncode:lispinit")
(comf "ncode:error")
(comf "ncode:alieneval")
(comf "ncode:stream")
(comf "ncode:arith")
(comf "ncode:array")
(comf "ncode:backq")
(comf "ncode:c-call")
(comf "ncode:char")
(comf "ncode:list")
;(comf "ncode:clx-ext")
(comf "ncode:commandline")
(comf "ncode:eval")
(comf "ncode:debug")
(comf "ncode:trace")
(comf "ncode:extensions")
(comf "ncode:fdefinition")
(comf "ncode:filesys")
(comf "ncode:format")
(comf "ncode:hash")
(comf "ncode:lfloatcon")
(comf "ncode:load")
(comf "ncode:machio")
(comf "ncode:miscop")
(comf "ncode:package")
(comf "ncode:rompstrops")
(comf "ncode:pred")
(comf "ncode:print")
(comf "ncode:provide")
(comf "ncode:query")
(comf "ncode:rand")
(comf "ncode:reader")
(comf "ncode:rompnum")
(comf "ncode:salterror")
(comf "ncode:save")
(comf "ncode:search-list")
(comf "ncode:seq")
(comf "ncode:sharpm")
(comf "ncode:sort")
(comf "ncode:run-program")
(comf "ncode:spirrat")
(comf "ncode:xp")
(comf "ncode:xp-patch")
(comf "ncode:pprint")
(comf "ncode:string")
(comf "ncode:subtypep")
(comf "ncode:symbol")
(comf "ncode:syscall")
(comf "ncode:sysmacs")
(comf "ncode:time")
(comf "ncode:foreign")
(comf "c:proclaim")
(comf "c:knownfun")
(comf "ncode:debug-info")

;;; Later so that miscellaneous structures are defined (not crucial, but nice.)
(comf "ncode:describe")
;(comf "ncode:inspect")
(comf "ncode:tty-inspect")

(comf "ncode:purify")
(comf "ncode:gc")
(comf "ncode:misc")
(comf "ncode:format-time")
(comf "ncode:parse-time")

(comf "ncode:internet")
(comf "ncode:wire")
(comf "ncode:remote")

(comf "assem:ropdefs")
(comf "assem:rompconst")
(comf "assem:disassemble")

(comf "ncode:machdef")
(comf "ncode:mmlispdefs")
(comf "nicode:machdefs")
(comf "nicode:netnamedefs")

(let ((system:*alien-eval-when* '(compile)))
  (unless (probe-file "nicode:machuser.nfasl")
    (load "nicode:machmsgdefs.lisp")
    (comf "nicode:machuser"))
  
  (unless (probe-file "nicode:netnameuser.nfasl")
    (load "nicode:netnamemsgdefs.lisp")
    (comf "nicode:netnameuser")))

;;; Compile basic macros that we assume are already in the compilation
;;; environment.  We inhibit compile-time definition to prevent these functions
;;; from becoming interpreted.  In some cases, this is necessary for
;;; compilation to work at all, since the expander functions are lazily
;;; converted: we could go into an infinite recursion trying to convert the
;;; definition of a macro which uses itself.
;;;
(let ((c:compile-time-define-macros* nil))
  (comf "ncode:defstruct")
  (comf "ncode:defmacro")
  (comf "ncode:macros")
  (comf "ncode:defrecord")
  (comf "ncode:constants")
  
  (comf "c:globaldb"))

); with-compiler-log-file
