;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/array.lisp,v 1.2 1990/11/30 16:45:44 wlott Exp $
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "SPARC")


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
			  (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic (ndescr)
    (inst or vector alloc-tn vm:other-pointer-type)
    (inst add alloc-tn alloc-tn (+ (1- (ash 1 vm:lowtag-bits))
				   (* vm:vector-data-offset vm:word-bytes)))
    (inst add alloc-tn alloc-tn words)
    (inst and alloc-tn alloc-tn (lognot vm:lowtag-mask))
    (inst srl ndescr type vm:word-shift)
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

  (inst b sxhash-simple-substring-entry)
  (loadw length string vm:vector-length-slot vm:other-pointer-type))


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
  (inst b test)
  (move accum zero-tn)

  LOOP

  (inst xor accum data)
  (inst sll temp accum 27)
  (inst srl accum 5)
  (inst or accum temp)
  (inst add offset 4)

  TEST

  (inst subcc length (fixnum 4))
  (inst b :ge loop)
  (inst ld data string offset)

  (inst addcc length (fixnum 4))
  (inst b :eq done)
  (inst neg length)
  (inst sll length 1)
  (inst srl data length)
  (inst xor accum data)

  DONE

  (inst sll result accum 5)
  (inst srl result result 3))
