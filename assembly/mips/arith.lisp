;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/arith.lisp,v 1.10 1992/03/08 18:35:13 wlott Exp $
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;;

(in-package "MIPS")


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
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FUN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)
  (inst add res x y)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-function-offset 'two-arg-+))
  (inst li nargs (fixnum 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn))


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
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FUN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)
  (inst sub res x y)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-function-offset 'two-arg--))
  (inst li nargs (fixnum 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn))


(define-assembly-routine (generic-*
			  (:cost 25)
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
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FUN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)

  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum tag.
  (inst sra temp x 2)
  (inst mult temp y)
  (inst mflo res)
  (inst mfhi hi)
  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word
  ;; is just 32 copies of the sign bit of the low word).
  (inst sra temp res 31)
  (inst xor temp hi)
  (inst beq temp DONE)
  ;; Shift the double word hi:res down two bits into hi:low to get rid of the
  ;; fixnum tag.
  (inst srl lo res 2)
  (inst sll temp hi 30)
  (inst or lo temp)
  (inst sra hi 2)
  ;; Allocate a BIGNUM for the result.
  (pseudo-atomic (temp)
    (let ((one-word (gen-label)))
      (inst addu res alloc-tn vm:other-pointer-type)
      ;; Assume we need one word.
      (inst addu alloc-tn (vm:pad-data-block (1+ vm:bignum-digits-offset)))
      ;; Is that correct?
      (inst sra temp lo 31)
      (inst xor temp hi)
      (inst beq temp one-word)
      (inst li temp (logior (ash 1 vm:type-bits) vm:bignum-type))
      ;; Nope, we need two, so allocate the addition space.
      (inst addu alloc-tn (- (vm:pad-data-block (+ 2 vm:bignum-digits-offset))
			     (vm:pad-data-block (1+ vm:bignum-digits-offset))))
      (inst li temp (logior (ash 2 vm:type-bits) vm:bignum-type))
      (storew hi res (1+ vm:bignum-digits-offset) vm:other-pointer-type)
      (emit-label one-word)
      (storew temp res 0 vm:other-pointer-type)
      (storew lo res vm:bignum-digits-offset vm:other-pointer-type)))
  ;; Out of here
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-function-offset 'two-arg-*))
  (inst li nargs (fixnum 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn)

  DONE)



;;;; Comparison routines.

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp not-p)
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
				  (:temp lip interior-reg lip-offset)
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (inst and temp x 3)
	  (inst bne temp DO-STATIC-FN)
	  (inst and temp y 3)
	  (inst beq temp DO-COMPARE)
	  ,cmp
	  
	  DO-STATIC-FN
	  (inst lw lip null-tn (static-function-offset ',static-fn))
	  (inst li nargs (fixnum 2))
	  (inst move ocfp cfp-tn)
	  (inst j lip)
	  (inst move cfp-tn csp-tn)
	  
	  DO-COMPARE
	  (inst ,(if not-p 'bne 'beq) temp done)
	  (inst move res null-tn)
	  (load-symbol res t)
	  DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< (inst slt temp x y) nil)
  (define-cond-assem-rtn generic-> > two-arg-> (inst slt temp y x) nil))


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
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst beq x y RETURN-T)
  (inst and temp x 3)
  (inst beq temp RETURN-NIL)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FN)
  (inst nop)

  RETURN-NIL
  (inst move res null-tn)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst lw lip null-tn (static-function-offset 'eql))
  (inst li nargs (fixnum 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
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
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst beq x y RETURN-T)
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FN)
  (inst nop)

  (inst move res null-tn)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst lw lip null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnum 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
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
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst beq x y RETURN-NIL)
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FN)
  (inst nop)

  (load-symbol res t)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst lw lip null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnum 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn)

  RETURN-NIL
  (inst move res null-tn))
