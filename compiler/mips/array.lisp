;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/array.lisp,v 1.12 1990/04/29 02:48:40 wlott Exp $
;;;
;;;    This file contains the MIPS definitions for array operations.
;;;
;;; Written by William Lott
;;;
(in-package "C")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:args (type :scs (any-reg descriptor-reg))
	 (rank :scs (any-reg descriptor-reg)))
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic (ndescr)
      (inst addu header alloc-tn vm:other-pointer-type)
      (inst addu alloc-tn alloc-tn
	    (* vm:array-dimensions-offset vm:word-bytes))
      (inst addu alloc-tn alloc-tn rank)
      (inst sll ndescr rank vm:type-bits)
      (inst or ndescr ndescr type)
      (inst srl ndescr ndescr 2)
      (storew ndescr header 0 vm:other-pointer-type))
    (move result header)))


;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t fixnum) fixnum
  (flushable))
(defknown ((setf lisp::%array-dimension))
	  (t fixnum fixnum) fixnum ())

(define-vop (%array-dimension word-index-ref)
  (:translate lisp::%array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))

(define-vop (%set-array-dimension word-index-set)
  (:translate (setf lisp::%array-dimension))
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))



(defknown lisp::%array-rank (t) fixnum (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 vm:other-pointer-type)
    (inst sra temp temp vm:type-bits)
    (inst sll res temp 2)
    (inst addu res res (fixnum (- 1 vm:array-dimensions-offset)))))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:generator 5
    (let ((error (generate-error-code di:invalid-array-index-error
				      array bound index)))
      (inst sltu temp index bound)
      (inst beq temp zero-tn error)
      (inst nop)
      (move result index))))



;;;; Accessors/Setters

(defmacro def-data-vector-frobs (type variant &optional (element-type t) sc)
  `(progn
     (define-vop (,(intern (concatenate 'simple-string
					"DATA-VECTOR-REF/"
					(string type)))
		  ,(intern (concatenate 'simple-string
					(string variant)
					"-REF")))
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-ref)
       (:arg-types ,type *)
       ,@(when sc
	   `((:results (value :scs (,sc)))
	     (:result-types ,element-type))))
     (define-vop (,(intern (concatenate 'simple-string
					"DATA-VECTOR-SET/"
					(string type)))
		  ,(intern (concatenate 'simple-string
					(string variant)
					"-SET")))
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-set)
       (:arg-types ,type * ,element-type)
       ,@(when sc
	   `((:args (object :scs (descriptor-reg))
		    (index :scs (any-reg descriptor-reg
					 immediate unsigned-immediate))
		    (value :scs (,sc)))
	     (:results (result :scs (,sc))))))))

(def-data-vector-frobs simple-string byte-index
  base-character base-character-reg)
(def-data-vector-frobs simple-vector word-index)

(def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
  unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-16 halfword-index
  unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-32 word-index
  unsigned-reg)



(define-vop (raw-bits word-index-ref)
  (:note "raw-bits VOP")
  (:translate %raw-bits)
  (:results (value :scs (unsigned-reg)))
  (:variant 0 vm:other-pointer-type))

(define-vop (set-raw-bits word-index-set)
  (:note "setf raw-bits VOP")
  (:translate (setf %raw-bits))
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg descriptor-reg immediate unsigned-immediate))
	 (value :scs (unsigned-reg)))
  (:results (result :scs (unsigned-reg)))
  (:variant 0 vm:other-pointer-type))




;;;; Misc. Array VOPs.


#+nil
(define-vop (vector-word-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec clc::g-vector-header-words)
    (inst niuo res res clc::g-vector-words-mask-16)))


(define-vop (get-vector-subtype)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 vm:other-pointer-type)
    (inst sra temp temp vm:type-bits)
    (inst sll res temp 2)))

(define-vop (set-vector-subtype)
  (:args (x :scs (descriptor-reg) :target res)
	 (subtype :scs (any-reg descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) t1 t2)
  (:generator 6
    (loadw t1 x 0 vm:other-pointer-type)
    (inst li t2 vm:type-mask)
    (inst and t1 t1 t2)
    (inst sll t2 subtype (- vm:type-bits 2))
    (inst or t1 t1 t2)
    (storew t1 x 0 vm:other-pointer-type)
    (move res x)))

