;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/array.lisp,v 1.6 1990/03/29 21:34:15 wlott Exp $
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
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic (ndescr)
      (inst addiu header alloc-tn vm:other-pointer-type)
      (inst addiu alloc-tn alloc-tn
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
    (inst addiu res res (fixnum (- 1 vm:array-dimensions-offset)))))



;;;; Various length translations.

(macrolet ((frob (type)
	     `(define-vop (,(intern (concatenate 'simple-string
						 "FAST-LENGTH/"
						 (string type)))
			   vector-length)
		(:translate length)
		(:arg-types ,type)
		(:policy :fast-safe))))
  (frob simple-string)
  (frob simple-bit-vector)
  (frob simple-vector)
  (frob simple-array-unsigned-byte-2)
  (frob simple-array-unsigned-byte-4)
  (frob simple-array-unsigned-byte-8)
  (frob simple-array-unsigned-byte-16)
  (frob simple-array-unsigned-byte-32)
  (frob simple-array-single-float)
  (frob simple-array-double-float))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:node-var node)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:generator 5
    (let ((error (generate-error-code node di:invalid-array-index-error
				      array bound index)))
      (inst sltu temp index bound)
      (inst beq temp zero-tn error)
      (nop)
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
       (:policy :fast-safe)
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
       (:policy :fast-safe)
       (:arg-types ,type * ,element-type)
       ,@(when sc
	   `((:args (object :scs (descriptor-reg))
		    (index :scs (any-reg descriptor-reg
					 immediate unsigned-immediate))
		    (value :scs (,sc)))
	     (:results (result :scs (,sc)))
	     (:result-types ,element-type))))))

(def-data-vector-frobs simple-string byte-index
  base-character base-character-reg)
(def-data-vector-frobs simple-vector word-index)
#|
(def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
  unsigned-32-reg)
(def-data-vector-frobs simple-array-unsigned-byte-16 halfword-index
  unsigned-32-reg)
(def-data-vector-frobs simple-array-unsigned-byte-32 word-index
  unsigned-32-reg)
|#



(define-vop (raw-bits halfword-index-ref)
  (:translate %raw-bits)
  (:variant 1 vm:other-pointer-type))

(define-vop (set-raw-bits halfword-index-set)
  (:translate (setf %raw-bits))
  (:variant 1 vm:other-pointer-type))




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
    (loadi t2 vm:type-mask)
    (inst and t1 t1 t2)
    (inst sra t2 subtype 2)
    (inst or t1 t1 t2)
    (storew t1 x 0 vm:other-pointer-type)
    (move res x)))

