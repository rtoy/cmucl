;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/pred.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of predicate VOPs for ARM64.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;;
;;; Ported to ARM64 from the SPARC backend.
;;;
;;; Key differences from SPARC:
;;;
;;;   - ARM64 has no branch delay slots, so the (inst nop) that follows
;;;     every SPARC branch is omitted throughout this file.
;;;
;;;   - The ARM64 conditional-branch instruction (defined in insts.lisp) is:
;;;
;;;         (define-instruction b (segment cond-or-target &optional target))
;;;
;;;     so the conditional form is:
;;;
;;;         (inst b :eq label)          ; condition first, label second
;;;
;;;     which matches the SPARC convention exactly; no change is needed
;;;     for the argument order of conditional branches.
;;;

(in-package "ARM64")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the
;;; desired destination.  Dest is the continuation we transfer control to.
;;;
;;; SPARC requires a delay-slot NOP after every branch instruction.
;;; ARM64 has no branch delay slots, so the NOP is omitted.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest)))


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
         (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst cmp x y)
    ;; ARM64 inst b: (inst b :cond label) -- condition first, label second.
    ;; This matches the SPARC convention; no argument reordering needed.
    (inst b (if not-p :ne :eq) target)))
