;;; -*- Package: C -*-
;;;
;;;    Load up the compiler.
;;;
(in-package "C")

(setf *load-verbose* t)

#-new-compiler
(progn
  (ext:gc-off)

  (load "code:fdefinition")
  (load "c:globaldb")
  (globaldb-init)

  (load "c:patch")
  (load "code:macros")
  (load "code:struct")
  (load "c:proclaim")
  (load "code:extensions")
  (load "code:defmacro")
  (load "code:sysmacs")
  (load "code:defrecord")
  (load "code:error")
  (load "code:debug-info")
  (load "code:defstruct")
  (load "code:c-call")
  (load "code:salterror")
  (load "code:machdef")

  (load "c:boot-globaldb"))

(load "c:macros")
#-pmax (load "c:type")
#-pmax (load "c:vm-type")
#-pmax (load "c:type-init")
#-pmax (setq *type-system-initialized* t)
#+pmax (load "mips:parms")
(load "c:sset")
(load "c:node")
(load "c:alloc")
(load "c:ctype")
(load "c:knownfun")
(load "c:fndb")
#+pmax (load "mips:vm-fndb")
(load "c:ir1util")
(load "c:ir1tran")
(load "c:ir1final")
(load "c:srctran")
(load "c:seqtran")
(load "c:typetran")
#+pmax (load "mips:vm-typetran")
#+pmax (load "c:float-tran")
(load "c:locall")
(load "c:dfo")
(load "c:ir1opt")
;(load "c:loop")
(load "c:checkgen")
(load "c:constraint")
(load "c:envanal")
#-pmax
(load "c:parms")
(load "c:vop")
(load "c:tn")
(load "c:bit-util")
(load "c:life")
(load "c:vmdef")
(load "c:gtn")
(load "c:ltn")
(load "c:stack")
(load "c:control")
(load "c:entry")
(load "c:ir2tran")
(load "c:pack")
(load "c:codegen")
(load "c:main")
(load "c:assembler")
(load "assem:assemfile")
#+pmax (load "mips:dismips")
#-pmax (load "c:assem-insts")
#+pmax (load "mips:mips-insts")
#-pmax (load "c:assem-macs")
#+pmax (load "mips:mips-macs")
(load "c:aliencomp")
(load "c:debug-dump")

#-new-compiler
(load "code:alieneval")

#+rt-target(progn
#-new-compiler
(handler-bind ((error #'(lambda (condition)
			  (format t "~%~A~%Continuing...~%" condition)
			  (continue))))
  (progn
    (load "code:constants")
    (load "assem:rompconst")))

#-new-compiler
(load "c:fop")

(load "c:dump")
#+new-compiler
(load "c:core")

(load "c:vm")
(load "c:move")
(load "c:char")
(load "c:miscop")
(load "c:subprim")
(load "c:print")
(load "c:memory")
(load "c:cell")
(load "c:call")
(load "c:nlx")
(load "c:values")
(load "c:array")
(load "c:pred")
(load "c:system")
(load "c:type-vops")
(load "c:arith")
); #+RT-TARGET PROGN

#+pmax(progn
(load "c:dump")
(load "mips:core")
;(load "mips:genesis")

(load "mips:vm")
(load "mips:move")
(load "mips:sap")
(load "mips:system")
(load "mips:char")
(load "mips:float")
(load "mips:memory")
(load "mips:static-fn")
(load "mips:arith")
(load "mips:cell")
(load "mips:subprim")
(load "mips:debug")
(load "mips:print")
(load "mips:alloc")
(load "mips:call")
(load "mips:nlx")
(load "mips:values")
(load "mips:array")
(load "mips:pred")
(load "mips:type-vops")
(load "mips:vm-tran")

#+pmax (load "assem:mips/assem-rtns")
#+pmax (load "assem:mips/bit-bash")
#+pmax (load "assem:mips/array")
#+pmax (load "assem:mips/arith")
); mips progn

(load "c:pseudo-vops")
#-pmax (load "c:vm-tran")
#+pmax (load "mips:vm-tran")
(load "c:debug")
(load "c:represent")

#+new-compiler
(load "c:eval-comp")
#+new-compiler
(load "c:eval")


#-new-compiler
(progn
  #+rt-target
  (load "assem:assembler")
  (%proclaim '(optimize (debug-info 2)))

  (setq *info-environment*
	(list (make-info-environment :name "Working")
	      (compact-info-environment (car *info-environment*))))
  (lisp::purify :root-structures '(ncompile-file))
  (ext:gc-on))

#+pmax (%proclaim '(optimize (speed 1) (safety 1)))
