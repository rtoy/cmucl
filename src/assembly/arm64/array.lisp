;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm64/array.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; Ported to ARM64 from the SPARC backend.
;;;
;;; Key SPARC -> ARM64 translation notes:
;;;
;;;   SPARC inst ADD   -> ARM64 inst ADD / ADD.W
;;;   SPARC inst ANDN  -> ARM64 inst AND with (lognot mask)
;;;                        (BIC has no immediate form; AND accepts bitmask immediates)
;;;   SPARC inst SRL   -> ARM64 inst LSR  (logical shift right, final fixnum conversion)
;;;   SPARC inst SLLN  -> ARM64 inst ADD.W / EOR.W with shifted-reg, or LSL
;;;   SPARC inst SRLN  -> ARM64 inst EOR.W with shifted-reg (zero-extend replaces final LSR)
;;;   SPARC inst SUBCC -> ARM64 inst SUBS (SUB setting flags)
;;;   SPARC inst LDUB  -> ARM64 inst LDRB (load unsigned byte)
;;;   SPARC inst B     -> ARM64 inst B    (unconditional / conditional)
;;;   SPARC inst XOR   -> ARM64 inst EOR.W
;;;   SPARC zero-tn    -> ARM64 zero-tn   (XZR / WZR)
;;;   SPARC alloc-tn   -> ARM64 alloc-tn  (X14, heap frontier)
;;;   SPARC inst T :NE -> ARM64: (inst b.ne ...) / (inst udf ...)
;;;
;;; Register assignments (ARM64):
;;;   a0-offset = X22   (argument / result 0)
;;;   a1-offset = X23   (argument 1)
;;;   a2-offset = X24   (argument 2)
;;;   a3-offset = X25   (argument 3; used as :temp vector)
;;;   nl0-offset = X0   (non-descriptor scratch 0; ndescr / accum)
;;;   nl1-offset = X1   (non-descriptor scratch 1; gc-temp / data)
;;;   nl3-offset = X3   (non-descriptor scratch 3; offset)
;;;
(in-package "ARM64")


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
			  (:temp gc-temp non-descriptor-reg nl1-offset)
			  (:temp vector descriptor-reg a3-offset))
  (emit-not-implemented)
  (pseudo-atomic ()
    ;; ndescr = words + (vector-data-offset + 1) * word-bytes
    ;; This is the total allocation size in bytes, including the header word
    ;; and the length slot.
    (inst add ndescr words (* (1+ vm:vector-data-offset) vm:word-bytes))
    ;; Round up to the next lispobj-aligned boundary by clearing the lowtag
    ;; bits.  SPARC used ANDN (AND-NOT); BIC has no immediate form on ARM64,
    ;; so we use AND with the bitwise complement of lowtag-mask instead.
    ;; (lognot vm:lowtag-mask) is a bitmask immediate expressible in the
    ;; logical-immediate encoding (AND does accept bitmask immediates).
    (inst and ndescr ndescr (lognot vm:lowtag-mask))
    ;; Inline bump-pointer allocation.  VECTOR receives the raw pointer with
    ;; other-pointer-type lowtag applied.
    (allocation vector ndescr other-pointer-type :temp-tn gc-temp)
    #+gencgc
    (progn
      ;; ndescr currently holds the allocation size.  Compute the address
      ;; one word past the end of the allocated object, then zero that word.
      ;; (On SPARC this cleared the padding word; on ARM64 we do the same.)
      ;; Note: VECTOR already has other-pointer-type applied, so the raw
      ;; end address is (vector - other-pointer-type + ndescr).  We fold
      ;; the lowtag adjustment into STOREW's lowtag argument instead.
      (inst add ndescr vector ndescr)
      (storew zero-tn ndescr -1 vm:other-pointer-type))
    ;; Write the object header.  The header word encodes the type and,
    ;; for most vector types, is the widetag shifted up by word-shift.
    ;; SPARC used SRL (logical shift right); ARM64 uses LSR.
    (inst lsr ndescr type vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    ;; Write the length field.
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  ;; Ensure the zero byte at the end of a string is paged in.
  ;; The write target is the last allocated word (where the NUL terminator
  ;; lives for strings; see the deftransform for make-array in array-tran.lisp).
  ;;
  ;; On SPARC without gencgc this wrote through alloc-tn; on ARM64 we
  ;; replicate the same pattern using alloc-tn (X14).
  #-gencgc
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
			  (:temp offset non-descriptor-reg nl3-offset))

  (declare (ignore result accum data offset))

  (emit-not-implemented)
  ;; Jump directly into sxhash-simple-substring, passing the string length
  ;; loaded from the vector header as the byte count.
  ;; ARM64 has no branch-delay slot, so LOADW comes before the branch.
  (loadw length string vm:vector-length-slot vm:other-pointer-type)
  (inst b sxhash-simple-substring-entry))


;; Implement the one-at-a-time algorithm designed by Bob Jenkins
;; (see <http://burtleburtle.net/bob/hash/doobs.html> for some
;; more information).
;;
;; For completeness, here is the hash function, in C, from that web page.
;; ub4 is an unsigned 32-bit integer.

#||
ub4 one_at_a_time(char *key, ub4 len)
{
  ub4   hash, i;
  for (hash=0, i=0; i<len; ++i)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return (hash & mask);
}
||#


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
			  (:temp offset non-descriptor-reg nl3-offset))
  (emit-not-implemented)
  (emit-label sxhash-simple-substring-entry)

  #+unicode
  ;; Each character is two bytes in the Unicode (UTF-16) representation,
  ;; so the number of bytes to hash is twice the character count.
  ;; ARM64 uses LSL where SPARC used SLL.
  (inst lsl length length 1)

  ;; Initialise the byte-offset register.  The data pointer starts at
  ;; (vector-data-offset * word-bytes - other-pointer-type), i.e. the
  ;; first data byte of the vector object.
  (inst li offset (- (* vector-data-offset word-bytes) other-pointer-type))
  ;; Initialise the hash accumulator to zero.
  (move accum zero-tn)
  ;; Jump to the loop test before executing the loop body, so a zero-length
  ;; string correctly returns 0.
  (inst b test)

  LOOP

  ;; hash += key[i]
  (inst add.w accum accum data)
  ;; hash += (hash << 10)
  (inst add.w accum accum (shift accum :lsl 10))
  ;; hash ^= (hash >> 6)
  (inst eor.w accum accum (shift accum :lsr 6))
  ;; Advance byte pointer.
  (inst add offset offset 1)

  TEST

  ;; Decrement the remaining byte count and test.
  (inst subs length length (fixnumize 1))
  ;; Load the next byte (unsigned) from string[offset].
  (inst ldrb data (reg-offset string offset))
  (inst b :ge loop)

  ;; hash += (hash << 3)
  (inst add.w accum accum (shift accum :lsl 3))
  ;; hash ^= (hash >> 11)
  (inst eor.w accum accum (shift accum :lsr 11))
  ;; hash += (hash << 15)
  (inst add.w accum accum (shift accum :lsl 15))

  ;; Convert the 32-bit hash to a positive fixnum.
  ;; The .w instructions above zero-extended accum into 64 bits, so the
  ;; value is already non-negative.  A single left shift by fixnum-tag-bits
  ;; tags it as a fixnum without any sign-bit masking needed.
  (inst lsl result accum vm:fixnum-tag-bits))
