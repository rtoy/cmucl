;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the RT VM definition of operand loading/saving and
;;; the Move VOP.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

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
      ((short-immediate unsigned-immediate immediate random-immediate
			immediate-string-char)
       (let ((val (tn-value x))
	     (dest (sc-case y
		     ((any-reg descriptor-reg string-char-reg) y)
		     (stack temp))))
	 (etypecase val
	   (integer
	    (loadi dest val))
	   (null
	    (inst cau dest zero-tn clc::nil-16))
	   ((member t)
	    (inst cau dest zero-tn clc::t-16))
	   (string-char
	    (let ((code (char-code val)))
	      (loadi dest code)
	      (unless (sc-is y string-char-reg)
		(inst oiu dest dest (ash system:%string-char-type
					 clc::type-shift-16))))))
	 (unless (eq dest y)
	   (store-stack-tn y temp))))
      (constant
       (sc-case y
	 ((any-reg descriptor-reg)
	  (load-slot y env-tn (tn-offset x)))
	 (stack
	  (load-slot temp env-tn (tn-offset x))
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
	    (inst lr y x))
	   (stack
	    (store-stack-tn y x))))
	(stack
	 (sc-case y
	   ((any-reg descriptor-reg)
	    (load-stack-tn y x))
	   (stack
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

;;; This VOP is emitted when we attempt to do a move between incompatible
;;; primitive types.  We signal an error, and ignore the result (which is
;;; specified only to terminate its lifetime.)
;;;
(define-vop (illegal-move three-arg-miscop)
  (:args (x :scs (any-reg descriptor-reg) :target a1)
	 (y-type :scs (any-reg descriptor-reg) :target a2))
  (:variant-vars)
  (:ignore r a3 nl0 nl1 misc-pc)
  (:generator 666
    (loadi a0 clc::error-object-not-type)
    (unless (location= x a1)
      (inst lr a1 x))
    (unless (location= y-type a2)
      (inst lr a2 y-type))
    (inst miscop 'clc::error2)
    (note-this-location vop :internal-error)))


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
      (stack
       (sc-case y
	 ((any-reg descriptor-reg)
	  (load-stack-tn y x))
	 (string-char-reg
	  (load-stack-tn y x)
	  (inst nilz y y system:%character-code-mask))))
      (string-char-stack
       (sc-case y
	 (string-char-reg
	  (load-stack-tn y x))))
      (t
       (unassemble (load-constant-tn x y nil node))))))

(define-vop (store-operand)
  (:args (x))
  (:results (y))
  (:generator 5
    (sc-case y
      (stack
       (sc-case x
	 ((any-reg descriptor-reg)
	  (store-stack-tn y x))
	 (string-char-reg
	  (inst oiu x x (ash system:%string-char-type clc::type-shift-16))
	  (store-stack-tn y x))))
      (string-char-stack
       (sc-case x
	 (string-char-reg
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
