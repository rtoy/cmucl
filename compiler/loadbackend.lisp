;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/loadbackend.lisp,v 1.3 1992/02/25 22:55:17 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Load the backend of the compiler.
;;;

(in-package "C")

(load "vm:vm-macs")
(if (string= (c:backend-name c:*target-backend*) "RT")
    (load "vm:params")
    (load "vm:parms"))
(load "vm:objdef")
(load "assem:support")
(load "vm:macros")
(load "vm:utils")

(load "vm:vm")
(load "vm:insts")
(unless (string= (c:backend-name c:*target-backend*) "RT")
  (load "vm:primtype"))
(load "vm:move")
(load "vm:sap")
(load "vm:system")
(load "vm:char")
(if (string= (c:backend-name c:*target-backend*) "RT")
    #+afpa (load "vm:afpa")
    #-afpa (load "vm:mc68881")
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
(load "vm:vm-tran")

(load "assem:assem-rtns")

(unless (string= (c:backend-name c:*target-backend*) "RT")
  (load "assem:bit-bash"))
(load "assem:array")
(load "assem:arith")
(load "assem:alloc")

(load "c:pseudo-vops")
(load "vm:vm-tran")

(check-move-function-consistency)
