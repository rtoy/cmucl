;;; -*- Package: User -*-
;;;
(in-package "USER")

(with-compiler-log-file ("c:compile-compiler.log")

(unless *new-compile*
  (comf "ncode:fdefinition")
  (load "ncode:extensions.lisp")
  (comf "c:globaldb" :load t)
  (unless (boundp 'ext::*info-environment*)
    (c::globaldb-init))

  (comf "c:patch")

  (comf "ncode:macros" :load t)
  (comf "ncode:extensions" :bootstrap-macros :both)
  (load "ncode:extensions.fasl")
  (comf "ncode:struct" :load t)
  (comf "c:macros" :load t :bootstrap-macros :both))

(when *new-compile*
  (comf "ncode:globals" :always-once t) ; For global variables.
  (comf "ncode:struct" :always-once t) ; For structures.
  (comf "c:globals" :always-once t)
  (comf "c:proclaim" :always-once t)) ; For COOKIE structure.

(comf "c:type" :always-once *new-compile*)
(comf "c:rt/vm-type")
(comf "c:type-init")
(comf "c:sset" :always-once *new-compile*)
(comf "c:node" :always-once *new-compile*)
(comf "c:ctype")
#-new-compiler
(comf "c:knownfun" :always-once *new-compile*)
(comf "c:fndb")
(comf "c:main")

#-new-compiler
(unless *new-compile*
  (comf "c:proclaim" :load t))

(comf "c:ir1tran")
(comf "c:ir1util" :bootstrap-macros :both)
(comf "c:ir1opt")
(comf "c:ir1final")
(comf "c:srctran")
(comf "c:seqtran")
(comf "c:typetran")
(comf "c:locall")
(comf "c:dfo")
(comf "c:checkgen")
(comf "c:constraint")
(comf "c:envanal")
(comf "c:rt/parms")
(comf "c:vop" :always-once *new-compile*)

(comf "c:vmdef" :load t :bootstrap-macros :both)

(comf "c:tn" :bootstrap-macros :both)
(comf "c:bit-util")
(comf "c:life")

(comf "c:assembler"
      :load t
      :bootstrap-macros :both
      :always-once *new-compile*)

(comf "ncode:debug-info"
      :load t
      :bootstrap-macros :both
      :always-once *new-compile*)

(comf "c:rt/assem-insts" :load t)

#+new-compiler
(when *new-compile*
  (comf "c:eval-comp")
  (comf "c:eval" :bootstrap-macros :both)
  (let ((c:*compile-time-define-macros* nil))
    (comf "c:macros" :load t)))


(comf "c:aliencomp")
(comf "c:debug-dump")

(unless *new-compile*
  (comf "ncode:constants" :load t :proceed t)
  (comf "assem:rompconst" :load t :proceed t)
  (comf "assem:assembler")
  (comf "c:fop"))

(comf "c:rt/assem-macs" :load t :bootstrap-macros :both)

(comf "c:rt/dump")

(when *new-compile*
  (comf "c:rt/core"))

(comf "c:rt/vm" :always-once *new-compile*)
(comf "c:rt/miscop")
(comf "c:rt/move")
(comf "c:rt/subprim")
(comf "c:rt/values")
(comf "c:rt/memory")
(comf "c:rt/cell")
(comf "c:rt/call")
(comf "c:rt/nlx")
(comf "c:rt/print")
(comf "c:rt/array")
(comf "c:rt/pred")
(comf "c:rt/type-vops")
(comf "c:rt/arith")
(comf "c:rt/system")
(comf "c:rt/char")
(comf "c:gtn")
(comf "c:ltn")
(comf "c:stack")
(comf "c:control")
(comf "c:entry")
(comf "c:ir2tran")
(comf "c:rt/vm-tran")
(comf "c:pack")
(comf "c:codegen")
(comf "c:debug")

#-new-compiler
(unless *new-compile* 
  (comf "c:rt/genesis"))

#+new-compiler
(comf "c:rt/genesis")

(unless *new-compile*
  (comf "ncode:defstruct")
  (comf "ncode:error")
  (comf "ncode:defrecord")
  (comf "ncode:defmacro")
  (comf "ncode:alieneval")
  (comf "ncode:c-call")
  (comf "ncode:salterror")
  (comf "ncode:sysmacs")
  (comf "ncode:machdef")
  (comf "ncode:mmlispdefs")
  (comf "nicode:machdefs")
  (comf "nicode:netnamedefs")
  (comf "c:globaldb" :output-file "c:boot-globaldb.fasl"
	:bootstrap-macros :both))


); with-compiler-error-log
