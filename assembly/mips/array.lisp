;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/array.lisp,v 1.18 1990/11/18 16:56:21 wlott Exp $
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "MIPS")


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
    (inst addu alloc-tn alloc-tn (+ (1- (ash 1 vm:lowtag-bits))
				    (* vm:vector-data-offset vm:word-bytes)))
    (inst addu alloc-tn alloc-tn words)
    (inst li ndescr (lognot vm:lowtag-mask))
    (inst and alloc-tn alloc-tn ndescr)
    (inst srl ndescr type vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  (move result vector))



;;;; Hash primitives

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp length any-reg a2-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp byte non-descriptor-reg nl2-offset)
			  (:temp retaddr non-descriptor-reg nl3-offset))

  ;; Save the return address.
  (inst subu retaddr lip-tn code-tn)

  (loadw length string vm:vector-length-slot vm:other-pointer-type)
  (inst addu lip string
	(- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type))
  (inst b test)
  (move accum zero-tn)

  loop

  (inst and byte data #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 8)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 16)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 24)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst addu lip lip 4)

  test

  (inst addu length length (fixnum -4))
  (inst lw data lip 0)
  (inst bgez length loop)
  (inst nop)

  (inst addu length length (fixnum 3))
  (inst beq length zero-tn one-more)
  (inst addu length length (fixnum -1))
  (inst beq length zero-tn two-more)
  (inst addu length length (fixnum -1))
  (inst bne length zero-tn done)
  (inst nop)

  (inst srl byte data 16)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  two-more

  (inst srl byte data 8)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  one-more

  (inst and byte data #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  done

  (inst sll result accum 5)
  (inst srl result result 3)

  ;; Restore the return address.
  (inst addu lip-tn retaddr code-tn))


(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp byte non-descriptor-reg nl2-offset)
			  (:temp retaddr non-descriptor-reg nl3-offset))
  ;; Save the return address.
  (inst subu retaddr lip-tn code-tn)

  (inst addu lip string
	(- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type))
  (inst b test)
  (move accum zero-tn)

  loop

  (inst and byte data #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 8)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 16)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 24)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst addu lip lip 4)

  test

  (inst addu length length (fixnum -4))
  (inst lw data lip 0)
  (inst bgez length loop)
  (inst nop)

  (inst addu length length (fixnum 3))
  (inst beq length zero-tn one-more)
  (inst addu length length (fixnum -1))
  (inst beq length zero-tn two-more)
  (inst addu length length (fixnum -1))
  (inst bne length zero-tn done)
  (inst nop)

  (inst srl byte data 16)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  two-more

  (inst srl byte data 8)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  one-more

  (inst and byte data #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  done

  (inst sll result accum 5)
  (inst srl result result 3)

  ;; Restore the return address.
  (inst addu lip-tn retaddr code-tn))

