;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm64/arith.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Originally written by William Lott (SPARC).
;;; Ported to ARM64 (AArch64) from the SPARC backend.
;;;
;;; Key SPARC -> ARM64 instruction translations used here:
;;;
;;;   SPARC                         ARM64
;;;   -----                         -----
;;;   (inst andcc zero-tn r mask)   (inst ands zero-tn r mask)
;;;   (inst b :ne label)            (inst b :ne label)
;;;   (inst b :eq label)            (inst b :eq label)
;;;   (inst b :vc label)            (inst b :vc label)   ; overflow clear
;;;   (inst b :lt/:le/:gt/:ge lbl)  (inst b :lt/:le/:gt/:ge lbl)
;;;   (inst addcc dst a b)          (inst adds dst a b)
;;;   (inst subcc dst a b)          (inst subs dst a b)
;;;   (inst xorcc dst a b)          (inst eors dst a b)  ; sets flags
;;;   (inst sra dst src n)          (inst asr dst src n)
;;;   (inst srl dst src n)          (inst lsr dst src n)
;;;   (inst sll dst src n)          (inst lsl dst src n)
;;;   (inst move dst src)           (move dst src)       ; pseudo
;;;   (inst nop)                    -- omitted (no delay slots on ARM64)
;;;   (inst ld dst base off)        (loadw dst base ...)
;;;   (inst j reg off)              (lisp-jump reg)      ; via lip-tn
;;;   (inst li reg val)             (inst li reg val)    ; pseudo

(in-package "ARM64")


;;;; Addition and subtraction.

(define-assembly-routine (generic-+
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate +)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp temp2 non-descriptor-reg nl1-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; Check that both args are fixnums.  ANDS discards the result but
  ;; sets the Z flag; branch to the static function if either tag is
  ;; non-zero.
  (inst ands zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst ands zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)

  ;; Both fixnums: attempt the tagged add.  ADDS sets the V (overflow)
  ;; flag.  If no overflow the fixnum result is already in TEMP.
  (inst adds temp x y)
  (inst b :vc DONE)

  ;; Overflow: unbox both operands with ASR (arithmetic shift right),
  ;; add the unboxed values, then allocate a bignum to hold the result.
  (inst asr temp x fixnum-tag-bits)
  (inst asr temp2 y fixnum-tag-bits)
  (inst add temp2 temp temp2)
  (with-fixed-allocation (res temp bignum-type (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  ;; At least one arg is not a fixnum.  Tail-call the generic function.
  ;; Pass TEMP so loadw can materialise the large static-function offset
  ;; via LI + LDR rather than attempting a bare LDUR with imm9.
  (loadw code-tn null-tn (static-function-offset 'two-arg-+) 0 temp)
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (move cfp-tn csp-tn)
  (lisp-jump code-tn)

  DONE
  (move res temp))


(define-assembly-routine (generic--
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp temp2 non-descriptor-reg nl1-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst ands zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst ands zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)

  ;; Both fixnums: SUBS sets V on overflow.
  (inst subs temp x y)
  (inst b :vc DONE)

  ;; Overflow: unbox, subtract, box into bignum.
  (inst asr temp x fixnum-tag-bits)
  (inst asr temp2 y fixnum-tag-bits)
  (inst sub temp2 temp temp2)
  (with-fixed-allocation (res temp bignum-type (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (loadw code-tn null-tn (static-function-offset 'two-arg--) 0 temp)
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (move cfp-tn csp-tn)
  (lisp-jump code-tn)

  DONE
  (move res temp))


;;;; Multiplication

(define-assembly-routine (generic-*
			  (:cost 50)
			  (:return-style :full-call)
			  (:translate *)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lo   non-descriptor-reg nl1-offset)
			  (:temp hi   non-descriptor-reg nl2-offset)
			  (:temp lra  descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (inst ands zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst ands zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)

  ;; Unbox one operand so the product already carries the fixnum tag.
  ;; ASR sign-extends, matching SPARC's SRA.
  (inst asr temp x fixnum-tag-bits)

  ;; AArch64 has a true 64x64->128 multiply via SMULH (high 64 bits)
  ;; and MUL (low 64 bits), replacing the SPARC v8 SMUL/RDY pair.
  (inst mul  lo temp y)          ; low  64 bits of signed product
  (inst smulh hi temp y)         ; high 64 bits of signed product

  ;; Check whether the result fits in a fixnum.
  ;; It fits iff the high word is just the sign-extension of the low word,
  ;; i.e. (ASR lo 63) == hi.  Use EORS to test and set flags.
  (inst asr temp lo 63)
  (inst eors temp temp hi)           ; temp = 0 iff hi == sign-ext(lo)
  (inst b :eq LOW-FITS-IN-FIXNUM)

  ;; Result needs a bignum.  Shift the double-word hi:lo right by
  ;; fixnum-tag-bits to remove the fixnum tag contributed by y.
  ;; Equivalent to the SPARC SLL/SRL/OR/SRA sequence.
  (inst lsl temp hi (- 64 fixnum-tag-bits))  ; salvage top bits of lo
  (inst lsr lo lo fixnum-tag-bits)
  (inst orr lo lo temp)
  (inst asr hi hi fixnum-tag-bits)

  ;; Allocate a 2-word bignum (GC will reclaim the second word if only
  ;; one is needed, just as on SPARC).
  (with-fixed-allocation
      (res temp bignum-type (+ 2 bignum-digits-offset))
    (let ((one-word (gen-label)))
      ;; Re-check: does the result actually fit in one bignum digit?
      (inst asr temp lo 63)
      (inst eors temp temp hi)
      (inst b :eq one-word)
      ;; Need 2 digits: write the header for a 2-word bignum...
      (inst li temp (logior (ash 2 type-bits) bignum-type))
      (storew hi res (1+ bignum-digits-offset) other-pointer-type)
      (emit-label one-word)
      ;; ...or fall through here with the 1-word header.
      (storew temp res 0 other-pointer-type)
      (storew lo  res bignum-digits-offset other-pointer-type)))
  ;; Out of here.
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (loadw code-tn null-tn (static-function-offset 'two-arg-*) 0 temp)
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (move cfp-tn csp-tn)
  (lisp-jump code-tn)

  LOW-FITS-IN-FIXNUM
  (move res lo))


;;;; Comparison

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp)
       `(define-assembly-routine (,name
				  (:cost 10)
				  (:return-style :full-call)
				  (:policy :safe)
				  (:translate ,translate)
				  (:save-p t))
				 ((:arg x (descriptor-reg any-reg) a0-offset)
				  (:arg y (descriptor-reg any-reg) a1-offset)

				  (:res res descriptor-reg a0-offset)

				  (:temp temp non-descriptor-reg nl0-offset)
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  ;; If x is not a fixnum, go straight to the static function.
	  (inst ands zero-tn x fixnum-tag-mask)
	  (inst b :ne DO-STATIC-FN)
	  ;; If y IS a fixnum, skip the static-fn and do the comparison.
	  (inst ands zero-tn y fixnum-tag-mask)
	  (inst b :eq DO-COMPARE)
	  ;; y is not a fixnum: fall through to static function.
	  (inst cmp x y)                   ; (pre-load flags for DO-COMPARE)

	  DO-STATIC-FN
	  (loadw code-tn null-tn (static-function-offset ',static-fn) 0 temp)
	  (inst li nargs (fixnumize 2))
	  (move ocfp cfp-tn)
	  (move cfp-tn csp-tn)
	  (lisp-jump code-tn)

	  DO-COMPARE
	  ;; CMP has already been executed above (in the fall-through path
	  ;; from the fixnum check) or re-entered here from DO-STATIC-FN.
	  ;; Conditional branch on the requested relation; if false, load NIL.
	  (inst b ,cmp DONE)
	  (load-symbol res t)
	  (move res null-tn)
	  DONE)))

  (define-cond-assem-rtn generic-<  <  two-arg-<  :lt)
  (define-cond-assem-rtn generic-<= <= two-arg-<= :le)
  (define-cond-assem-rtn generic->  >  two-arg->  :gt)
  (define-cond-assem-rtn generic->= >= two-arg->= :ge))


(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; Pointer-equal => EQL immediately.
  (inst cmp x y)
  (inst b :eq RETURN-T)
  ;; x is a fixnum and they are unequal => NIL.
  (inst ands zero-tn x fixnum-tag-mask)
  (inst b :eq RETURN-NIL)
  ;; x is not a fixnum; if y is also not a fixnum, delegate.
  (inst ands zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)

  RETURN-NIL
  (move res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (loadw code-tn null-tn (static-function-offset 'eql) 0 temp)
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (move cfp-tn csp-tn)
  (lisp-jump code-tn)

  RETURN-T
  (load-symbol res t))


(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; Both must be fixnums for the fast path.
  (inst ands zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst ands zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst cmp x y)
  (inst b :eq RETURN-T)

  (move res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (loadw code-tn null-tn (static-function-offset 'two-arg-=) 0 temp)
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (move cfp-tn csp-tn)
  (lisp-jump code-tn)

  RETURN-T
  (load-symbol res t))


(define-assembly-routine (generic-/=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate /=)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; Quick pointer-equality check: if equal, definitely NIL.
  (inst cmp x y)
  (inst b :eq RETURN-NIL)
  ;; If either is non-fixnum, delegate to the generic function.
  (inst ands zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst ands zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)

  ;; Both fixnums and unequal: return T.
  (load-symbol res t)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  ;; Note: SPARC original calls 'two-arg-= here; preserved for fidelity.
  (loadw code-tn null-tn (static-function-offset 'two-arg-=) 0 temp)
  (inst li nargs (fixnumize 2))
  (move ocfp cfp-tn)
  (move cfp-tn csp-tn)
  (lisp-jump code-tn)

  RETURN-NIL
  (move res null-tn))
