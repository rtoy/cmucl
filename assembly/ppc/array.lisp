;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/ppc/array.lisp,v 1.3 2004/08/08 11:15:11 rtoy Exp $
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "PPC")


(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type any-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:arg words any-reg a2-offset)
			  (:res result descriptor-reg a0-offset)

			  (:temp ndescr non-descriptor-reg nl0-offset)
			  (:temp pa-flag non-descriptor-reg nl3-offset)
			  (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic (pa-flag)
    (inst addi ndescr words (* (1+ vm:vector-data-offset) vm:word-bytes))
    (inst clrrwi ndescr ndescr lowtag-bits)
    (allocation vector ndescr other-pointer-type)
    (inst srwi ndescr type vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  (move result vector))



;;;; Hash primitives

#+assembler
(defparameter sxhash-simple-substring-entry (gen-label))

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)

			  (:temp length any-reg a1-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))

  (declare (ignore result accum data temp offset))

  (loadw length string vm:vector-length-slot vm:other-pointer-type)
  (inst b sxhash-simple-substring-entry))


(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))
  (emit-label sxhash-simple-substring-entry)

  (inst li offset (- (* vector-data-offset word-bytes) other-pointer-type))
  (move accum zero-tn)
  (inst b test)

  LOOP

  (inst xor accum accum data)
  (inst slwi temp accum 27)
  (inst srwi accum accum 5)
  (inst or accum accum temp)
  (inst addi offset offset 4)

  TEST

  (inst subic. length length (fixnumize 4))
  (inst lwzx data string offset)
  (inst bge loop)

  (inst addic. length length (fixnumize 4))
  (inst neg length length)
  (inst beq done)
  (inst slwi length length 1)
  (inst srw data data length)
  (inst xor accum accum data)

  DONE

  (inst slwi result accum 5)
  (inst srwi result result 3))
