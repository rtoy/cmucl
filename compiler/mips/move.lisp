;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/move.lisp,v 1.10 1990/04/05 23:54:40 wlott Exp $
;;;
;;;    This file contains the RT VM definition of operand loading/saving and
;;; the Move VOP.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

;;; Load-Constant-TN  --  Internal
;;;
;;;    Load the Constant or immediate constant TN X into Y.  Temp is a
;;; descriptor register temp or NIL, if none is available.  Temp must be
;;; supplied if Y may be in memory.  Node is for souce context.
;;;
(defun load-constant-tn (x y temp node)
  (declare (type tn x y) (type (or tn null temp)) (type node node))
  (assemble node
    (sc-case x
      ((zero negative-immediate unsigned-immediate immediate
	     null random-immediate immediate-base-character immediate-sap)
       (let ((val (tn-value x))
	     (dest (sc-case y
		     ((any-reg descriptor-reg base-character-reg sap-reg) y)
		     ((control-stack number-stack sap-stack
				     base-character-stack)
		      temp))))
	 (etypecase val
	   (integer
	    (loadi dest (fixnum val)))
	   (null
	    (move dest null-tn))
	   (symbol
	    (load-symbol dest val))
	   (string-char
	    (loadi dest (logior (ash (char-code val) type-bits)
				base-character-type))))
	 (unless (eq dest y)
	   (store-stack-tn y temp))))
      (constant
       (sc-case y
	 ((any-reg descriptor-reg)
	  (loadw y code-tn (tn-offset x) other-pointer-type))
	 (control-stack
	  (loadw temp code-tn (tn-offset x) other-pointer-type)
	  (store-stack-tn y temp)))))))


;;;; The Move VOP:
;;;
;;;    The Move VOP is used for doing arbitrary moves when there is no
;;; type-specific move/coerce operation.

;;; We need a register to do a memory-memory move.
;;;
(define-vop (move)
  (:args (x :target y
	    :scs (any-reg descriptor-reg)
	    :load nil))
  (:results (y :scs (any-reg descriptor-reg)
	       :load nil))
  (:temporary (:scs (descriptor-reg)
	       :from :argument  :to :result)
	      temp)
  (:node-var node)
  (:effects)
  (:affected)
  (:generator 0
    (unless (location= x y)
      (sc-case x
	((any-reg descriptor-reg)
	 (sc-case y
	   ((any-reg descriptor-reg)
	    (move y x))
	   (control-stack
	    (store-stack-tn y x))))
	(control-stack
	 (sc-case y
	   ((any-reg descriptor-reg)
	    (load-stack-tn y x))
	   (control-stack
	    (load-stack-tn temp x)
	    (store-stack-tn y temp))))
	(t
	 (unassemble (load-constant-tn x y temp node)))))))


;;; Make Move the check VOP for T so that type check generation doesn't think
;;; it is a hairy type.  This also allows checking of a few of the values in a
;;; continuation to fall out.
;;;
(primitive-type-vop move (:check) t)


;;;; ILLEGAL-MOVE

;;; This VOP exists just to begin the lifetime of a TN that couldn't be written
;;; legally due to a type error.  An error is signalled before this VOP is
;;; so we don't need to do anything (not that there would be anything sensible
;;; to do anyway.)
;;;
(define-vop (illegal-move)
  (:args (x) (type))
  (:results (y))
  (:ignore y)
  (:generator 666
    (error-call di:object-not-type-error x type)))


;;;; Operand loading and saving:
;;;
;;;    These are the VOPs used for loading or saving Load-TNs.  They cannot
;;; allocate any temporaries since packing has already been done by the time
;;; that these VOPs are emitted.  It can be assumed that the loaded (saved) TN
;;; is free before (after) the load (save) (so may be used as a temporary.)
;;;

(define-vop (load-operand)
  (:args (x))
  (:results (y))
  (:node-var node)
  (:generator 5
    (sc-case x
      (control-stack
       (sc-case y
	 ((any-reg descriptor-reg)
	  (load-stack-tn y x))
	 (base-character-reg
	  (load-stack-tn y x)
	  (inst srl y y vm:type-bits))))
      (base-character-stack
       (sc-case y
	 (base-character-reg
	  (load-stack-tn y x))))
      (t
       (unassemble (load-constant-tn x y nil node))))))

(define-vop (store-operand)
  (:args (x))
  (:results (y))
  (:generator 5
    (sc-case y
      (control-stack
       (sc-case x
	 ((any-reg descriptor-reg)
	  (store-stack-tn y x))
	 (base-character-reg
	  (inst sll x x vm:type-bits)
	  (inst ori x x vm:base-character-type)
	  (store-stack-tn y x))))
      (base-character-stack
       (sc-case x
	 (base-character-reg
	  (store-stack-tn y x)))))))


;;;; Register saving and restoring VOPs.

(define-vop (save-reg)
  (:args (reg))
  (:results (stack))
  (:generator 5
    (store-stack-tn stack reg)))

(define-vop (restore-reg)
  (:args (stack))
  (:results (reg))
  (:generator 5
    (load-stack-tn reg stack)))
