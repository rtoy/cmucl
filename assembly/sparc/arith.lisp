;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/arith.lisp,v 1.1 1990/11/22 11:49:50 wlott Exp $
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
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst andcc zero-tn x 3)
  (inst b :ne DO-STATIC-FUN)
  (inst andcc zero-tn y 3)
  (inst b :ne DO-STATIC-FUN)
  (inst nop)
  (inst addcc temp x y)
  (inst b :vc done)
  (inst nop)

  (inst sra temp x 2)
  (inst sra temp2 y 2)
  (inst add temp2 temp)
  (with-fixed-allocation (res temp vm:bignum-type 1)
    (storew temp2 res vm:bignum-digits-offset vm:other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg-+)
  (inst li nargs (fixnum 2))
  (loadw code-tn cname vm:symbol-raw-function-addr-slot vm:other-pointer-type)
  (inst move ocfp cfp-tn)
  (inst j code-tn)
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
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst andcc zero-tn x 3)
  (inst b :ne DO-STATIC-FUN)
  (inst andcc zero-tn y 3)
  (inst b :ne DO-STATIC-FUN)
  (inst nop)
  (inst subcc temp x y)
  (inst b :vc done)
  (inst nop)

  (inst sra temp x 2)
  (inst sra temp2 y 2)
  (inst add temp2 temp)
  (with-fixed-allocation (res temp vm:bignum-type 1)
    (storew temp2 res vm:bignum-digits-offset vm:other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg--)
  (inst li nargs (fixnum 2))
  (loadw code-tn cname vm:symbol-raw-function-addr-slot vm:other-pointer-type)
  (inst move ocfp cfp-tn)
  (inst j code-tn)
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
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (inst andcc zero-tn x 3)
  (inst b :ne DO-STATIC-FUN)
  (inst andcc zero-tn y 3)
  (inst b :ne DO-STATIC-FUN)
  (inst nop)

  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum tag.
  (inst sra temp x 2)
  (emit-multiply temp y hi res :signed)
  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word
  ;; is just 32 copies of the sign bit of the low word).
  (inst sra temp res 31)
  (inst xorcc temp hi)
  (inst b :eq DONE)
  ;; Shift the double word hi:res down two bits into hi:low to get rid of the
  ;; fixnum tag.
  (inst srl lo res 2)
  (inst sll temp hi 30)
  (inst or lo temp)
  (inst sra hi 2)
  ;; Allocate a BIGNUM for the result.
  (pseudo-atomic (temp)
    (let ((one-word (gen-label)))
      (inst add res alloc-tn other-pointer-type)
      ;; Assume we need one word.
      (inst add alloc-tn (pad-data-block (1+ bignum-digits-offset)))
      ;; Is that correct?
      (inst sra temp lo 31)
      (inst xorcc temp hi)
      (inst b :eq one-word)
      (inst li temp (logior (ash 1 type-bits) bignum-type))
      ;; Nope, we need two, so allocate the addition space.
      (inst add alloc-tn (- (pad-data-block (+ 2 bignum-digits-offset))
			    (pad-data-block (1+ bignum-digits-offset))))
      (inst li temp (logior (ash 2 type-bits) bignum-type))
      (storew hi res (1+ bignum-digits-offset) other-pointer-type)
      (emit-label one-word)
      (storew temp res 0 other-pointer-type)
      (storew lo res bignum-digits-offset other-pointer-type)))
  ;; Out of here
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg-*)
  (inst li nargs (fixnum 2))
  (loadw code-tn cname symbol-raw-function-addr-slot other-pointer-type)
  (inst move ocfp cfp-tn)
  (inst j code-tn)
  (inst move cfp-tn csp-tn)

  DONE)

(macrolet
    ((frob (name note cost type sc)
       `(define-assembly-routine (,name
				  (:note ,note)
				  (:cost ,cost)
				  (:translate *)
				  (:policy :fast-safe)
				  (:arg-types ,type ,type)
				  (:result-types ,type))
				 ((:arg x ,sc nl0-offset)
				  (:arg y ,sc nl1-offset)
				  (:res res ,sc nl0-offset)
				  (:temp temp ,sc nl2-offset))
	  ,@(when (eq type 'tagged-num)
	      `((inst sra x 2)))
	  (emit-multiply x y temp res :unsigned))))
  (frob unsigned-* "unsigned *" 40 unsigned-num unsigned-reg)
  (frob signed-* "unsigned *" 41 signed-num signed-reg)
  (frob fixnum-* "fixnum *" 30 tagged-num any-reg))



;;;; Division.

#+assembler
(defun 32/32-restoring-divide (dividend divisor rem quo iter)
  (let ((done-shifting (gen-label))
	(not-big-dividend (gen-label)))

    (inst addcc rem dividend 0)
    (inst b :ge not-big-dividend)
    (inst li iter (fixnum -1))

    ;; The dividend is huge.  If we shift the divisor until it's negative.
    (let ((loop (gen-label)))
      (inst cmp divisor)
      (inst b :lt done-shifting)
      (inst li quo -1)
      (emit-label loop)
      (inst addcc divisor divisor)
      (inst b :ge loop)
      (inst add iter (fixnum 1))
      (inst b done-shifting)
      (inst nop))

    ;; The dividend is not huge.  Keep shifting the divisor up until it's
    ;; larger than the dividend.
    (emit-label not-big-dividend)
    (let ((loop (gen-lable)))
      (inst cmp divisor rem)
      (inst b :ge done-shifting)
      (inst li quo -1)
      (emit-label loop)
      (inst sll divisor 1)
      (inst cmp quo rem)
      (inst b :nc loop)
      (inst add iter (fixnum 1)))

    (emit-label done-shifting)
    (let ((loop-1 (gen-label))
	  (loop-2 (gen-label))
	  (finished (gen-label)))
      (emit-label loop-1)
      (inst subcc iter (fixnum 1))
      (inst b :le finished)
      (emit-label loop-2)
      (inst sub temp rem divisor)
      (inst slr divisor 1)
      (inst b :cs loop)
      (inst addcc quo quo)
      (inst subcc iter (fixnum 1))
      (inst b :gt loop-2)
      (inst move rem temp)
      (emit-label finished))

    (inst not quo)))


(macrolet
    ((frob (name note cost prim-type sc)
       `(define-assembly-routine (,name
				  (:note ,note)
				  (:cost ,cost)
				  (:translate truncate)
				  (:policy :fast-safe)
				  (:arg-types ,prim-type ,prim-type)
				  (:result-types ,prim-type ,prim-type))
				 ((:arg dividend ,sc nl0-offset)
				  (:arg divisor ,sc nl1-offset)

				  (:res quo ,sc nl2-offset)
				  (:res rem ,sc nl0-offset)

				  (:temp iter any-reg nl3-offset)
				  (:temp temp non-descriptor-reg nl4-offset))
	  )))
  (frob fixnum-truncate "fixnum truncate" 50 tagged-num any-reg)
  (frob signed-truncate "fixnum truncate" 60 tagged-num any-reg)
  (frob unsigned-truncate "fixnum truncate" 55 tagged-num any-reg))
