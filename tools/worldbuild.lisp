;;; -*- Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; When loaded, this file builds a core image from all the .fasl files that
;;; are part of the kernel CMU Common Lisp system.

(in-package "LISP")

(defparameter lisp-files
  `(,@(when (string= (c:backend-name c:*backend*) "PMAX")
	'("target:assembly/mips/assem-rtns.assem"
	  "target:assembly/mips/array.assem"
	  "target:assembly/mips/bit-bash.assem"
	  "target:assembly/mips/arith.assem"
	  "target:assembly/mips/alloc.assem"))
    ,@(when (string= (c:backend-name c:*backend*) "SPARC")
	'("target:assembly/sparc/assem-rtns.assem"
	  "target:assembly/sparc/array.assem"
	  "target:assembly/sparc/bit-bash.assem"
	  "target:assembly/sparc/arith.assem"
	  "target:assembly/sparc/alloc.assem"))

    "target:code/fdefinition"
    "target:code/eval"

    "target:code/type-boot"
    "target:code/struct"
    "target:code/error"
    "target:code/salterror"
    "target:compiler/type"
    "target:compiler/generic/vm-type"
    "target:compiler/type-init"

    "target:code/defstruct"
    "target:compiler/proclaim"
    "target:compiler/globaldb"
    "target:code/pred"

    "target:code/kernel"
    "target:code/bit-bash"
    "target:code/array"
    "target:code/char"
    "target:code/lispinit"
    "target:code/seq"
    "target:code/numbers"
    "target:code/float"
    "target:code/float-trap"
    "target:code/irrat"
    "target:code/bignum"
    "target:code/defmacro"
    "target:code/defrecord"
    "target:code/list"
    "target:code/hash"
    "target:code/macros"
    "target:code/sysmacs"
    "target:code/symbol"
    "target:code/string"
    "target:code/mipsstrops"
    "target:code/misc"
    "target:code/gc"
    "target:code/extensions"
    "target:code/alieneval"
    "target:code/c-call"
    "target:code/syscall"
    "target:code/vm"
    "target:code/serve-event"
    "target:code/stream"
    "target:code/fd-stream"
    "target:code/print"
    "target:code/format"
    "target:code/package"
    "target:code/reader"
    "target:code/backq"
    "target:code/sharpm"
    "target:code/load"
    "target:code/machdef"
    ,@(when (string= (c:backend-name c:*backend*) "PMAX")
	'("target:code/pmax-vm"
	  "target:code/pmax-machdef"))
    ,@(when (string= (c:backend-name c:*backend*) "SPARC")
	'("target:code/sparc-vm"
	  "target:code/sparc-machdef"))
    "target:code/filesys"
    "target:code/search-list"

    "target:code/signal"
    "target:code/interr"
    "target:code/debug-info"
    "target:code/debug-int"
    "target:code/debug"
    ))

(setf *genesis-core-name*
      #+mips "target:ldb/kernel.core"
      #+sparc "/usr/tmp/kernel.core")
(setf *genesis-c-header-name* "target:ldb/lisp.h")
(setf *genesis-map-name* "target:ldb/lisp.map")
(setf *genesis-symbol-table* "target:ldb/ldb.map")

(genesis lisp-files)
