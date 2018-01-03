;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/sparc/arith.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;;

(in-package "SPARC")



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
  (not-implemented "GENERIC-+")
  (inst andcc zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst andcc zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst nop)
  (inst addcc temp x y)
  (inst b :vc done)
  (inst nop)

  (inst sra temp x fixnum-tag-bits)
  (inst sra temp2 y fixnum-tag-bits)
  (inst add temp2 temp)
  (with-fixed-allocation (res temp bignum-type (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (inst ld code-tn null-tn (static-function-offset 'two-arg-+))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst move cfp-tn csp-tn)

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
  (not-implemented "GENERIC--")
  (inst andcc zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst andcc zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst nop)
  (inst subcc temp x y)
  (inst b :vc done)
  (inst nop)

  (inst sra temp x fixnum-tag-bits)
  (inst sra temp2 y fixnum-tag-bits)
  (inst sub temp2 temp temp2)
  (with-fixed-allocation (res temp bignum-type (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (inst ld code-tn null-tn (static-function-offset 'two-arg--))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst move cfp-tn csp-tn)

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
			  (:temp lo non-descriptor-reg nl1-offset)
			  (:temp hi non-descriptor-reg nl2-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (not-implemented "GENERIC-*")
  ;; If either arg is not a fixnum, call the static function.
  (inst andcc zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst andcc zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FUN)
  (inst nop)

  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum tag.
  (inst sran temp x fixnum-tag-bits)
  ;; Compute the produce temp * y and return the double-word product
  ;; in hi:lo.
  (cond ((backend-featurep :sparc-64)
	 ;; Sign extend y to a full 64-bits.  temp was already
	 ;; sign-extended by the sra instruction above.
	 (inst sra y 0)
	 (inst mulx hi temp y)
	 (inst move lo hi)
	 (inst srax hi 32))
	((or (backend-featurep :sparc-v8)
	     (backend-featurep :sparc-v9))
	 (inst smul lo temp y)
	 (inst rdy hi))
	(t
	 (let ((MULTIPLIER-POSITIVE (gen-label)))
	   (inst wry temp)
	   (inst andcc hi zero-tn)
	   (inst nop)
	   (inst nop)
	   (dotimes (i 32)
	     (inst mulscc hi y))
	   (inst mulscc hi zero-tn)
	   (inst cmp x)
	   (inst b :ge MULTIPLIER-POSITIVE)
	   (inst nop)
	   (inst sub hi y)
	   (emit-label MULTIPLIER-POSITIVE)
	   (inst rdy lo))))

  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word
  ;; is just 32 copies of the sign bit of the low word).
  (inst sra temp lo 31)
  (inst xorcc temp hi)
  (inst b :eq LOW-FITS-IN-FIXNUM)
  ;; Shift the double word hi:lo down two bits to get rid of the fixnum tag.
  (inst sll temp hi 30)
  (inst srl lo fixnum-tag-bits)
  (inst or lo temp)
  (inst sra hi fixnum-tag-bits)
  ;; Allocate a BIGNUM for the result. We always allocate 2 words for
  ;; the bignum result, even if we only need one.  The copying GC will
  ;; take care of the extra word if it isn't needed.
  (with-fixed-allocation
      (res temp bignum-type (+ 2 bignum-digits-offset))
    (let ((one-word (gen-label)))
      ;; We start out assuming that we need one word.  Is that correct?
      (inst sra temp lo 31)
      (inst xorcc temp hi)
      (inst b :eq one-word)
      (inst li temp (logior (ash 1 type-bits) bignum-type))
      ;; Need 2 words.  Set the header appropriately, and save the
      ;; high and low parts.
      (inst li temp (logior (ash 2 type-bits) bignum-type))
      (storew hi res (1+ bignum-digits-offset) other-pointer-type)
      (emit-label one-word)
      (storew temp res 0 other-pointer-type)
      (storew lo res bignum-digits-offset other-pointer-type)))
  ;; Out of here
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (inst ld code-tn null-tn (static-function-offset 'two-arg-*))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst move cfp-tn csp-tn)

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
				  
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (not-implemented ,name)
	  (inst andcc zero-tn x fixnum-tag-mask)
	  (inst b :ne DO-STATIC-FN)
	  (inst andcc zero-tn y fixnum-tag-mask)
	  (inst b :eq DO-COMPARE)
	  (inst cmp x y)
	  
	  DO-STATIC-FN
	  (inst ld code-tn null-tn (static-function-offset ',static-fn))
	  (inst li nargs (fixnumize 2))
	  (inst move ocfp cfp-tn)
	  (inst j code-tn
		(- (* function-code-offset word-bytes) function-pointer-type))
	  (inst move cfp-tn csp-tn)
	  
	  DO-COMPARE
	  (inst b ,cmp done)
	  (load-symbol res t)
	  (inst move res null-tn)
	  DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< :lt)
  (define-cond-assem-rtn generic-<= <= two-arg-<= :le)
  (define-cond-assem-rtn generic-> > two-arg-> :gt)
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

			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (not-implemented "GENERIC-EQL")
  (inst cmp x y)
  (inst b :eq RETURN-T)
  (inst andcc zero-tn x fixnum-tag-mask)
  (inst b :eq RETURN-NIL)
  (inst andcc zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst nop)

  RETURN-NIL
  (inst move res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (inst ld code-tn null-tn (static-function-offset 'eql))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst move cfp-tn csp-tn)

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

			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (not-implemented "GENERIC-=")
  (inst andcc zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst andcc zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst cmp x y)
  (inst b :eq RETURN-T)
  (inst nop)

  (inst move res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (inst ld code-tn null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst move cfp-tn csp-tn)

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

			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (not-implemented "GENERIC-/=")
  (inst cmp x y)
  (inst b :eq RETURN-NIL)
  (inst andcc zero-tn x fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst andcc zero-tn y fixnum-tag-mask)
  (inst b :ne DO-STATIC-FN)
  (inst nop)

  (load-symbol res t)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (inst ld code-tn null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst move cfp-tn csp-tn)

  RETURN-NIL
  (inst move res null-tn))
