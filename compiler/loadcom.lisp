;;; -*- Package: C -*-
;;;
;;;    Load up the compiler.
;;;
(in-package "C")

#-new-compiler
(progn
  (ext:gc-off)

  (load "code:fdefinition" :verbose t)
  (load "c:globaldb" :verbose t)
  (globaldb-init)

  (load "c:patch" :verbose t)
  (load "code:macros" :verbose t)
  (load "code:struct" :verbose t)
  (load "c:proclaim" :verbose t)
  (load "code:extensions" :verbose t)
  (load "code:defmacro" :verbose t)
  (load "code:sysmacs" :verbose t)
  (load "code:defrecord" :verbose t)
  (load "code:error" :verbose t)
  (load "code:debug-info" :verbose t)
  (load "code:defstruct" :verbose t)
  (load "code:c-call" :verbose t)
  (load "code:salterror" :verbose t)
  (load "code:machdef" :verbose t)

  (load "c:boot-globaldb" :verbose t))

(load "c:macros" :verbose t)
#-pmax (load "c:type" :verbose t)
#-pmax (load "c:vm-type" :verbose t)
#-pmax (load "c:type-init" :verbose t)
#-pmax (setq *type-system-initialized* t)
#+pmax (load "mips:parms")
(load "c:sset" :verbose t)
(load "c:node" :verbose t)
(load "c:alloc" :verbose t)
(load "c:ctype" :verbose t)
(load "c:knownfun" :verbose t)
(load "c:fndb" :verbose t)
#+pmax (load "mips:vm-fndb" :verbose t)
(load "c:ir1util" :verbose t)
(load "c:ir1tran" :verbose t)
(load "c:ir1final" :verbose t)
(load "c:srctran" :verbose t)
(load "c:seqtran" :verbose t)
(load "c:typetran" :verbose t)
#+pmax (load "mips:vm-typetran" :verbose t)
#+pmax (load "c:float-tran" :verbose t)
(load "c:locall" :verbose t)
(load "c:dfo" :verbose t)
(load "c:ir1opt" :verbose t)
;(load "c:loop" :verbose t)
(load "c:checkgen" :verbose t)
(load "c:constraint" :verbose t)
(load "c:envanal" :verbose t)
#-pmax
(load "c:parms" :verbose t)
(load "c:vop" :verbose t)
(load "c:tn" :verbose t)
(load "c:bit-util" :verbose t)
(load "c:life" :verbose t)
(load "c:vmdef" :verbose t)
(load "c:gtn" :verbose t)
(load "c:ltn" :verbose t)
(load "c:stack" :verbose t)
(load "c:control" :verbose t)
(load "c:entry" :verbose t)
(load "c:ir2tran" :verbose t)
(load "c:pack" :verbose t)
(load "c:codegen" :verbose t)
(load "c:main" :verbose t)
(load "c:assembler" :verbose t)
#+pmax (load "mips:dismips" :verbose t)
#+pmax (load "mips:assemfile" :verbose t)
#-pmax (load "c:assem-insts" :verbose t)
#+pmax (load "mips:mips-insts" :verbose t)
#-pmax (load "c:assem-macs" :verbose t)
#+pmax (load "mips:mips-macs" :verbose t)
(load "c:aliencomp" :verbose t)
(load "c:debug-dump" :verbose t)

#-new-compiler
(load "code:alieneval" :verbose t)

#+rt-target(progn
#-new-compiler
(handler-bind ((error #'(lambda (condition)
			  (format t "~%~A~%Continuing...~%" condition)
			  (continue))))
  (progn
    (load "code:constants" :verbose t)
    (load "assem:rompconst" :verbose t)))

#-new-compiler
(load "c:fop" :verbose t)

(load "c:dump" :verbose t)
#+new-compiler
(load "c:core" :verbose t)

(load "c:vm" :verbose t)
(load "c:move" :verbose t)
(load "c:char" :verbose t)
(load "c:miscop" :verbose t)
(load "c:subprim" :verbose t)
(load "c:print" :verbose t)
(load "c:memory" :verbose t)
(load "c:cell" :verbose t)
(load "c:call" :verbose t)
(load "c:nlx" :verbose t)
(load "c:values" :verbose t)
(load "c:array" :verbose t)
(load "c:pred" :verbose t)
(load "c:system" :verbose t)
(load "c:type-vops" :verbose t)
(load "c:arith" :verbose t)
); #+RT-TARGET PROGN

#+pmax(progn
(load "mips:dump")
;(load "c:core")
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
(load "assem:assem-rtns-vops")
(load "assem:bit-bash-vops")
(load "assem:array-vops")
); mips progn

(load "c:pseudo-vops" :verbose t)
#-pmax (load "c:vm-tran" :verbose t)
#+pmax (load "mips:vm-tran" :verbose t)
(load "c:debug" :verbose t)
(load "c:represent" :verbose t)

#+new-compiler
(load "c:eval-comp" :verbose t)
#+new-compiler
(load "c:eval" :verbose t)


#-new-compiler
(progn
  #+rt-target
  (load "assem:assembler" :verbose t)
  (%proclaim '(optimize (debug-info 2)))

  (setq *info-environment*
	(list (make-info-environment :name "Working")
	      (compact-info-environment (car *info-environment*))))
  (lisp::purify :root-structures '(ncompile-file))
  (ext:gc-on))

#+pmax (%proclaim '(optimize (speed 1) (safety 1)))
