;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/array.lisp,v 1.7 2003/08/03 11:27:50 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/array.lisp,v 1.7 2003/08/03 11:27:50 gerd Exp $
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
  (pseudo-atomic ()
    (inst add ndescr words (* (1+ vm:vector-data-offset) vm:word-bytes))
    (inst andn ndescr 7)
    (allocation vector ndescr other-pointer-type)
    (inst srl ndescr type vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  ;; This makes sure the zero byte at the end of a string is paged in so
  ;; the kernel doesn't bitch if we pass it the string.
  ;;
  ;; This used to write to the word after the last allocated word.  I
  ;; (RLT) made it write to the last allocated word, which is where
  ;; the zero-byte of the string is.  Look at the deftransform for
  ;; make-array in array-tran.lisp.  For strings we always allocate
  ;; enough space to hold the zero-byte.
  (storew zero-tn alloc-tn -1)
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

  (inst subcc length (fixnumize 4))
  (inst b :ge loop)
  (inst ld data string offset)

  (inst addcc length (fixnumize 4))
  (inst b :eq done)
  (inst neg length)
  (inst sll length 1)
  (inst srl data length)
  (inst xor accum data)

  DONE

  (inst sll result accum 5)
  (inst srl result result 3))
