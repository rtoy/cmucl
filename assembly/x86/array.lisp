;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/x86/array.lisp,v 1.2 1997/02/10 17:03:44 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains various array operations that are too expensive
;;; (in space) to do inline.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski -- Spring 1995.
;;;
(in-package :x86)


;;;; Allocation

(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type unsigned-reg eax-offset)
			  (:arg length any-reg ebx-offset)
			  (:arg words any-reg ecx-offset)
			  (:res result descriptor-reg edx-offset)

			  (:temp alloc dword-reg edi-offset))
  #-cgc
  (with-allocation (alloc)
    (inst lea result (make-ea :byte :base alloc :disp other-pointer-type))
    (inst add alloc
	  (+ (1- (ash 1 lowtag-bits)) (* vector-data-offset word-bytes)))
    (inst add alloc words)
    (inst and alloc (lognot vm:lowtag-mask)))
  #+cgc
  (progn
    (inst mov alloc (+ (1- (ash 1 lowtag-bits))
		       (* vector-data-offset word-bytes)))
    (inst add alloc words)
    (inst and alloc (lognot vm:lowtag-mask))
    (with-cgc-allocation(alloc alloc)
      (inst lea result (make-ea :byte :base alloc :disp other-pointer-type))))
  (storew type result 0 other-pointer-type)
  (storew length result vector-length-slot other-pointer-type)
  (inst ret))



;;;; Hash primitives

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg ebx-offset)
			  (:res result any-reg edx-offset)

			  (:temp length any-reg edi-offset)
			  (:temp esi dword-reg esi-offset)
			  (:temp ecx dword-reg ecx-offset)
			  (:temp eax dword-reg eax-offset))
  (declare (ignore result esi ecx eax))
  (loadw length string vector-length-slot other-pointer-type)
  ;; zzzzz this appears to be busted
  ;; (inst jmp nil (make-fixup 'sxhash-simple-substring :assembly-routine))
  ;; just fall through???
  )

(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg ebx-offset)
			  (:arg length any-reg edi-offset)
			  (:res result any-reg edx-offset)

			  (:temp esi dword-reg esi-offset)
			  (:temp ecx dword-reg ecx-offset)
			  (:temp eax dword-reg eax-offset))
  ;; Compute a pointer to where we are going to be extracting the bits.
  (inst lea esi
	(make-ea :byte :base string
		 :disp (- (* vector-data-offset word-bytes)
			  other-pointer-type)))
  ;; Initialize the result.
  (inst xor result result)
  ;; Get the count.  If it's zero, blow out.
  (inst mov ecx length)
  (inst jecxz done)
  ;; Convert it into count of the number of full words.  If zero, then skip
  ;; to the part that handles the tail.
  (inst shr ecx 4)
  (inst jecxz do-extra)
  ;; Clear the direction flag, so we advance through memory.
  (inst cld)

  LOOP
  ;; Merge each successive word with the result.
  (inst lods eax)			; load 32-bits into eax and (+4 esi)

  (inst rol result 5)
  (inst xor result eax)
  (inst loop loop)

  DO-EXTRA
  ;; Now we have to take care of any bytes that don't make up a full word.
  ;; First, check to see how many of them there are.  If zero, blow out of
  ;; here.  Otherwise, multiply by 8.
  (inst mov ecx length)
  (inst and ecx (fixnum 3))
  (inst jecxz done)
  (inst shl ecx 1)

  ;; Grab the last word.
  (inst lods eax)

  ;; Convert the count into a mask.  The count is multiplied by 8, so we just
  ;; shift -1 left, which shifts count*8 zeros into the low order end.  We
  ;; then invert that, ending up with a mask of count*8 ones.
  (inst mov esi -1)
  (inst shl esi :cl)
  (inst not esi)
  ;; Use the mask to strip off the bits we arn't interested in, and merge
  ;; the remaining bits with the result.
  (inst and eax esi)
  (inst rol result 5)
  (inst xor result eax)

  DONE

  ;; Force result to be a positive fixnum.
  (inst and result #x7ffffffc)
  (inst ret))
