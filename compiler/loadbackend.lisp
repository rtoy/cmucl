;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/loadbackend.lisp,v 1.1 1992/02/24 05:51:12 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Load the backend of the compiler.
;;;

(in-package "C")

(load "vm:vm-macs")
#-rt (load "vm:parms")
#+rt (load "vm:params")
(load "vm:objdef")
(load "vm:vm-fndb")
(load "vm:vm-typetran")
(load "assem:support")
(load "vm:macros")
(load "vm:utils")
(load "vm:core")

(load "vm:vm")
(load "vm:insts")
#-rt (load "vm:primtype")
(load "vm:move")
(load "vm:sap")
(load "vm:system")
(load "vm:char")
#-rt (load "vm:float")
#+(and rt afpa) (load "vm:afpa")
#+(and rt (not afpa)) (load "vm:mc68881")

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
#-rt (load "assem:bit-bash")
(load "assem:array")
(load "assem:arith")
(load "assem:alloc")

(load "c:pseudo-vops")
(load "vm:vm-tran")

(check-move-function-consistency)

#+small
;;;
;;; If we want a small core, blow away the meta-compile time VOP info.
(setf (backend-parsed-vops *backend*) (make-hash-table :test #'eq))
