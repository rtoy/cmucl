;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/array.lisp,v 1.7 1990/04/24 03:13:54 wlott Exp $
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "C")


(defmacro allocate-vector (type length words vector ndescr)
  `(pseudo-atomic (,ndescr)
     (inst or ,vector alloc-tn vm:other-pointer-type)
     (inst addu alloc-tn alloc-tn (+ (1- (ash 1 vm:lowtag-bits))
				      (* vm:vector-data-offset vm:word-bytes)))
     (inst addu alloc-tn alloc-tn ,words)
     (inst li ,ndescr (lognot vm:lowtag-mask))
     (inst and alloc-tn alloc-tn ,ndescr)
     ,(if (constantp type)
	  `(inst li ,ndescr ,type)
	  `(inst srl ,ndescr ,type vm:word-shift))
     (storew ,ndescr ,vector 0 vm:other-pointer-type)
     (storew ,length ,vector vm:vector-length-slot vm:other-pointer-type)))

(defmacro maybe-invoke-gc (vector words)
  "Assure that VECTOR lies entirely before the guard page.  WORDS is the
  number of words of data in the vector."
  (declare (ignore vector words))
  nil)


(define-assembly-routine (allocate-vector
			  (:arg type)
			  (:arg length)
			  (:arg words)
			  (:res result)

			  (:temp vector :sc descriptor-reg)
			  (:temp ndescr :sc non-descriptor-reg :type random))
  (allocate-vector type length words vector ndescr)
  (maybe-invoke-gc vector words)
  (move result vector))


(define-assembly-routine (alloc-g-vector
			  (:arg length)
			  (:arg fill)
			  (:res result)

			  (:temp ndescr :sc non-descriptor-reg :type random)
			  (:temp lip :sc interior-reg :type interior)
			  (:temp vector :sc descriptor-reg))

  (allocate-vector vm:simple-vector-type length length vector ndescr)
  (inst beq length zero-tn done)
  (inst addu lip vector (- (* vm:vector-data-offset vm:word-bytes)
			   vm:other-pointer-type))

  loop

  (storew fill lip)
  (inst addu length length (fixnum -1))
  (inst bne length zero-tn loop)
  (inst addu lip lip vm:word-bytes)

  done

  (move result vector))


(define-assembly-routine (alloc-string
			  (:arg length)
			  (:res result)

			  (:temp ndescr :sc non-descriptor-reg :type random)
			  (:temp vector :sc descriptor-reg)
			  (:temp words :sc non-descriptor-reg :type fixnum))
  ;; Note: When we calculate the number of words needed, we assure that there
  ;; will be at least one extra byte available.  This allows us to pass strings
  ;; to C land without having to tack an extra null on the end ourselves.
  ;;
  ;; To do this quickly, we divide the length by the number of bytes and strip
  ;; off the two fixnum lowtag bits, add 1 (binary 1, not fixnum 1), and
  ;; shift back to a fixnum:
  ;;
  ;; length   len>>4   1+      <<2
  ;; 	0	0	1	4
  ;;    1	0	1	4
  ;;    3	0	1	4
  ;;    4	1	2	8
  ;;    7	1	2	8
  ;;    8	2	3	12
  ;;
  (inst sra words length (+ vm:word-shift 2))
  (inst addu words words 1)
  (inst sll words words 2)
  (allocate-vector vm:simple-string-type length words vector ndescr)
  (maybe-invoke-gc vector words)
  (move result vector))





;;;; Hash primitives

(define-assembly-routine (sxhash-simple-string
			  (:arg string)
			  (:res result)

			  (:temp lip :sc interior-reg :type interior)
			  (:temp length :sc any-reg :type fixnum)
			  (:temp accum :sc non-descriptor-reg :type random)
			  (:temp data :sc non-descriptor-reg :type random)
			  (:temp byte :sc non-descriptor-reg :type random))
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
  (inst srl result result 3))


(define-assembly-routine (sxhash-simple-substring
			  (:arg string)
			  (:arg length)
			  (:res result)

			  (:temp lip :sc interior-reg :type interior)
			  (:temp accum :sc non-descriptor-reg :type random)
			  (:temp data :sc non-descriptor-reg :type random)
			  (:temp byte :sc non-descriptor-reg :type random))
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
  (inst srl result result 3))
