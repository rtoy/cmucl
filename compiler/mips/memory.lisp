;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/memory.lisp,v 1.9 1990/06/18 14:47:14 wlott Exp $
;;;
;;;    This file contains the MIPS definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "C")


;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.  Cell-Setf is similar to
;;; Cell-Set, but delivers the new value as the result.
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))
;;;
(define-vop (cell-setf)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)
		:target result))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))

;;; Define-Cell-Accessors  --  Interface
;;;
;;;    Define accessor VOPs for some cells in an object.  If the operation name
;;; is NIL, then that operation isn't defined.  If the translate function is
;;; null, then we don't define a translation.
;;;
(defmacro define-cell-accessors (offset lowtag
					ref-op ref-trans set-op set-trans)
  `(progn
     ,@(when ref-op
	 `((define-vop (,ref-op cell-ref)
	     (:variant ,offset ,lowtag)
	     ,@(when ref-trans
		 `((:translate ,ref-trans))))))
     ,@(when set-op
	 `((define-vop (,set-op cell-setf)
	     (:variant ,offset ,lowtag)
	     ,@(when set-trans
		 `((:translate ,set-trans))))))))


;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the stardard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (storew value object (+ base offset) lowtag)))



;;;; Indexed references:

;;; Define-Indexer  --  Internal
;;;
;;;    Define some VOPs for indexed memory reference.  Unless the index is
;;; constant, we must compute an intermediate result in a boxed temporary,
;;; since the RT doesn't have any indexed addressing modes.  This means that GC
;;; has to adjust the "raw" pointer in Index-Temp by observing that Index-Temp
;;; points within Object-Temp.  After we are done, we clear Index-Temp so that
;;; we don't raw pointers lying around.
;;;
(defmacro define-indexer (name write-p op shift)
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg))
	    (index :scs (any-reg zero immediate negative-immediate))
	    ,@(when write-p
		'((value :scs (any-reg descriptor-reg) :target result))))
     (:arg-types * tagged-num ,@(when write-p '(*)))
     (:temporary (:scs (interior-reg) :type interior) lip)
     ,@(unless (zerop shift)
	 `((:temporary (:scs (non-descriptor-reg) :type random) temp)))
     (:results (,(if write-p 'result 'value)
		:scs (any-reg descriptor-reg)))
     (:result-types *)
     (:variant-vars offset lowtag)
     (:policy :fast-safe)
     (:generator 5
       (sc-case index
	 ((immediate zero negative-immediate)
	  (inst ,op value object
		(- (+ (if (sc-is index zero)
			  0
			  (ash (tn-value index) (- word-shift ,shift)))
		      (ash offset word-shift))
		   lowtag))
	  ,(if write-p
	       '(move result value)
	       '(inst nop)))
	 (t
	  ,@(if (zerop shift)
		`((inst addu lip object index))
		`((inst srl temp index ,shift)
		  (inst addu lip temp object)))
	  (inst ,op value lip (- (ash offset word-shift) lowtag))
	  ,(if write-p
	       '(move result value)
	       '(inst nop)))))))

(define-indexer word-index-ref nil lw 0)
(define-indexer word-index-set t sw 0)
(define-indexer halfword-index-ref nil lhu 1)
(define-indexer signed-halfword-index-ref nil lh 1)
(define-indexer halfword-index-set t sh 1)
(define-indexer byte-index-ref nil lbu 2)
(define-indexer signed-byte-index-ref nil lb 2)
(define-indexer byte-index-set t sb 2)

