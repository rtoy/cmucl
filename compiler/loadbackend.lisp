;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/loadbackend.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Load the backend of the compiler.
;;;

(in-package "C")

(load "vm:vm-macs")
(if (target-featurep :rt)
    (load "vm:params")
    (load "vm:parms"))

(load "vm:objdef")
(load "vm:interr")
(load "assem:support")
(load "vm:macros")
(load "vm:utils")

(load "vm:vm")
(load "vm:insts")
(unless (target-featurep :rt)
  (load "vm:primtype"))
(load "vm:move")
(load "vm:sap")
(when (target-featurep :x86)
  (if (target-featurep :sse2)
      (load "vm:sse2-sap")
      (load "vm:x87-sap")))
(load "vm:system")
(load "vm:char")
(if (target-featurep :rt)
    (if (target-featurep :afpa)
	(load "vm:afpa")
	(load "vm:mc68881"))
    (if (target-featurep :sse2)
	(load "vm:float-sse2")
	(load "vm:float")))

(load "vm:memory")
(load "vm:static-fn")
(load "vm:arith")
(load "vm:cell")
(load "vm:subprim")
(load "vm:debug")
(load "vm:c-call")
(when (target-featurep :x86)
  (if (target-featurep :sse2)
      (load "vm:sse2-c-call")
      (load "vm:x87-c-call")))
(load "vm:print")
(load "vm:alloc")
(load "vm:call")
(load "vm:nlx")
(load "vm:values")
;; These need to be loaded before array because array wants to use
;; some vops as templates.
(when (target-featurep :x86)
  (load (if (target-featurep :sse2)
	    "vm:sse2-array"
	    "vm:x87-array")))
(load "vm:array")
(load "vm:pred")
(load "vm:type-vops")

(load "assem:assem-rtns")

(load "assem:array")
(load "assem:arith")
(load "assem:alloc")

(load "c:pseudo-vops")

(check-move-function-consistency)
