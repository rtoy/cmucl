;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains definitions of VOPs used as internal markers by the
;;; compiler.  Since they don't emit any code, they should be portable. 
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Notes the place at which the environment is properly initialized, for
;;; debug-info purposes.
;;;
(define-vop (note-environment-start)
  (:info start-lab)
  (:generator 0
    (emit-label start-lab)))


;;; Call a move function.  Used for register save/restore and spilling.
;;;
(define-vop (move-operand)
  (:args (x))
  (:results (y))
  (:info name)
  (:node-var node)
  (:generator 0
    (unassemble
      (funcall (symbol-function name) node x y))))


;;; Move the specified arg-vals into the argument passing locations for
;;; callee-2env.  FP and NFP are the callee's FP and NFP allocated by
;;; ALLOCATE-FRAME.  Representation selection inserts the appropriate
;;; move-argument VOPs before this VOP, changing the arg-val operands to be
;;; references to the passing locations.
;;;
(define-vop (move-arguments)
  (:args (args :more t))
  (:info fp nfp callee-2env)
  (:ignore args fp nfp callee-2env)
  (:generator 0))

;;; Similar to move-arguments, but emits the argument moves for a full call
;;; with NARGS args.  FP is the callee's frame pointer (allocated by
;;; ALLOCATE-FULL-CALL-FRAME.)
;;;
(define-vop (move-full-call-arguments move-arguments)
  (:info fp nargs))

;;; Used for known return.  Much like MOVE-ARGUMENTS, but the target frame
;;; pointers are obtained from the current environment.  OLD-CONT is used for
;;; FP and the old NFP is computed with COMPUTE-OLD-NFP.
;;;
(define-vop (move-results move-arguments)
  (:info locs))
