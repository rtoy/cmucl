;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
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
    (ld value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (st value object offset lowtag)))
;;;
(define-vop (cell-setf)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)
		:target result))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (st value object offset lowtag)
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
    (ld value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (st value object (+ base offset) lowtag)))



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
     (:args (object :scs (descriptor-reg)
		    ,@(unless (zerop shift)
			'(:target object-temp)))
	    (index :scs (any-reg descriptor-reg immediate negative-immediate)
		   :target index-temp)
	    ,@(when write-p
		'((value :scs (any-reg descriptor-reg) :target result))))
     (:results (,(if write-p 'result 'value)
		:scs (any-reg descriptor-reg)))
     (:variant-vars offset)
     ,@(unless (zerop shift)
	 `((:temporary (:scs (descriptor-reg)
			     :from (:argument 0))
		       object-temp)))
     (:temporary (:scs (descriptor-reg)
		       :from (:argument 0))
		 index-temp)
     (:policy :fast-safe)
     (:generator 5
       (sc-case index
	 ((immediate negative-immediate)
	  (inst ,op value object
		;; ### Is this right?  Is index supposed to be a byte or word
		;; offset?
		(- (+ (tn-value index) offset) other-pointer-type))
	  ,(if write-p
	       '(move result value)
	       '(nop)))
	 (t
	  ;; This code assumes that object has other-pointer low tag bits.
	  ;; Note: the temporary reg causes GC problems.
	  ,@(if (zerop shift)
		`((inst add index-temp object index))
		`((move object-temp object)
		  (inst srl index-temp index ,shift)
		  (inst add index-temp object-temp index-temp)))
	  (inst ,op value index-temp (- offset other-pointer-type))
	  (move index-temp zero-tn)
	  ,@(when write-p
	      `((move result value))))))))

(define-indexer word-index-ref nil lw 0)
(define-indexer word-index-set t sw 0)
(define-indexer halfword-index-ref nil lhu 1)
(define-indexer signed-halfword-index-ref nil lh 1)
(define-indexer halfword-index-set t sh 1)
(define-indexer byte-index-ref nil lbu 2)
(define-indexer signed-byte-index-ref nil lb 2)
(define-indexer byte-index-set t sb 2)

