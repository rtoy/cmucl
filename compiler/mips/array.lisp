;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/array.lisp,v 1.3 1990/03/07 18:38:35 wlott Exp $
;;;
;;;    This file contains the MIPS definitions for array operations.
;;;
;;; Written by William Lott
;;;
(in-package "C")

#|
(define-miscop aref1 (a i) :translate aref)
(define-miscop caref2 (a i j) :translate aref)
(define-miscop caref3 (a i j k) :translate aref)
(define-miscop aset1 (a i val) :translate %aset)
(define-miscop caset2 (a i j val) :translate %aset)
(define-miscop caset3 (a i j k val) :translate %aset)
(define-miscop svref (v i) :translate aref
  :arg-types (simple-vector t))
(define-miscop svset (v i val) :translate %aset
  :arg-types (simple-vector t t))
(define-miscop schar (v i) :translate aref
  :arg-types (simple-string t))
(define-miscop scharset (v i val) :translate %aset
  :arg-types (simple-string t t))
(define-miscop sbit (v i) :translate aref
  :arg-types (simple-bit-vector t))
(define-miscop sbitset (v i val) :translate %aset
  :arg-types (simple-bit-vector t t))
|#


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




(define-vop (fast-schar byte-index-ref)
  (:arg-types simple-string *)
  (:results (value :scs (base-character-reg)))
  (:result-types base-character)
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate aref)
  (:policy :fast))

(define-vop (fast-scharset byte-index-set)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg descriptor-reg immediate unsigned-immediate))
	 (value :scs (base-character-reg)))
  (:results (result :scs (base-character-reg)))
  (:result-types base-character)
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate %aset)
  (:policy :fast)
  (:arg-types simple-string * base-character))


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


(define-vop (fast-svref word-index-ref)
  (:variant vm:vector-base-size vm:other-pointer-type)
  (:translate aref)
  (:arg-types simple-vector *)
  (:policy :fast))

(define-vop (fast-svset word-index-set)
  (:variant vm:vector-base-size vm:other-pointer-type)
  (:translate %aset)
  (:arg-types simple-vector * *)
  (:policy :fast))
