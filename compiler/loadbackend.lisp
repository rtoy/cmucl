;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/loadbackend.lisp,v 1.6 1992/03/21 19:44:30 wlott Exp $")
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
(when (target-featurep :gengc)
  (load "vm:vm-utils"))
(load "vm:utils")

(load "vm:vm")
(load "vm:insts")
(unless (target-featurep :rt)
  (load "vm:primtype"))
(load "vm:move")
(load "vm:sap")
(load "vm:system")
(load "vm:char")
(if (target-featurep :rt)
    (if (target-featurep :afpa)
	(load "vm:afpa")
	(load "vm:mc68881"))
    (load "vm:float"))

(load "vm:memory")
(load "vm:static-fn")
(load "vm:arith")
(load "vm:cell")
(load "vm:subprim")
(load "vm:debug")
(load "vm:c-call")
(load "vm:print")
(load "vm:alloc")
(load "vm:call")
(load "vm:nlx")
(load "vm:values")
(load "vm:array")
(load "vm:pred")
(load "vm:type-vops")

(load "assem:assem-rtns")

(unless (or (target-featurep :rt)
	    (target-featurep :gengc))
  (load "assem:bit-bash"))
(load "assem:array")
(load "assem:arith")
(unless (target-featurep :gengc)
  (load "assem:alloc"))

(load "c:pseudo-vops")

(check-move-function-consistency)
