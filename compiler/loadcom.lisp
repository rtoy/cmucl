;;; -*- Package: C -*-
;;;
;;;    Load up the compiler.
;;;
(in-package "C")

#-new-compiler
(progn
  (ext:gc-off)

  (load "ncode:fdefinition" :verbose t)
  (load "c:globaldb" :verbose t)
  (globaldb-init)

  (load "c:patch" :verbose t)
  (load "ncode:macros" :verbose t)
  (load "ncode:struct" :verbose t)
  (load "c:proclaim" :verbose t)
  (load "ncode:extensions" :verbose t)
  (load "ncode:defmacro" :verbose t)
  (load "ncode:sysmacs" :verbose t)
  (load "ncode:defrecord" :verbose t)
  (load "ncode:error" :verbose t)
  (load "ncode:debug-info" :verbose t)
  (load "ncode:defstruct" :verbose t)
  (load "ncode:c-call" :verbose t)
  (load "ncode:salterror" :verbose t)
  (load "ncode:machdef" :verbose t)

  (load "c:boot-globaldb" :verbose t))

(load "c:macros" :verbose t)
(load "c:type" :verbose t)
(load "c:vm-type" :verbose t)
(load "c:type-init" :verbose t)
(load "c:sset" :verbose t)
(load "c:node" :verbose t)
(load "c:ctype" :verbose t)
(load "c:knownfun" :verbose t)
(load "c:fndb" :verbose t)
(load "c:ir1util" :verbose t)
(load "c:ir1tran" :verbose t)
(load "c:ir1final" :verbose t)
(load "c:srctran" :verbose t)
(load "c:seqtran" :verbose t)
(load "c:typetran" :verbose t)
(load "c:vm-tran" :verbose t)
(load "c:locall" :verbose t)
(load "c:dfo" :verbose t)
(load "c:ir1opt" :verbose t)
;(load "c:loop" :verbose t)
(load "c:checkgen" :verbose t)
(load "c:constraint" :verbose t)
(load "c:envanal" :verbose t)
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
(load "c:assem-insts" :verbose t)
(load "c:assem-macs" :verbose t)
(load "c:aliencomp" :verbose t)
(load "c:debug-dump" :verbose t)

#-new-compiler
(load "ncode:alieneval" :verbose t)

#+rt-target(progn
#-new-compiler
(handler-bind ((error #'(lambda (condition)
			  (format t "~%~A~%Continuing...~%" condition)
			  (continue))))
  (progn
    (load "ncode:constants" :verbose t)
    (load "assem:rompconst" :verbose t)))

#-new-compiler
(load "c:fop" :verbose t)

(load "c:dump" :verbose t)
#+new-compiler
(load "c:core" :verbose t)

(load "c:vm" :verbose t)
(load "c:move" :verbose t)
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
(load "c:char" :verbose t)
(load "c:type-vops" :verbose t)
(load "c:arith" :verbose t)
); #+RT-TARGET PROGN

(load "c:debug" :verbose t)

#+new-compiler
(load "c:eval-comp" :verbose t)
#+new-compiler
(load "c:eval" :verbose t)


#-new-compiler
(progn
  #+rt-target
  (load "assem:assembler" :verbose t)
  (%proclaim '(optimize (debug-info 0)))

  (setq *info-environment*
	(list (make-info-environment :name "Working")
	      (compact-info-environment (car *info-environment*))))
  (lisp::purify :root-structures '(ncompile-file))
  (ext:gc-on))
