;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/arith.lisp,v 1.2 1990/11/24 19:20:44 wlott Exp $
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
  (inst j code-tn
	(- (* vm:function-header-code-offset vm:word-bytes)
	   vm:function-pointer-type))
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
  (inst j code-tn
	(- (* vm:function-header-code-offset vm:word-bytes)
	   vm:function-pointer-type))
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
  (inst j code-tn
	(- (* vm:function-header-code-offset vm:word-bytes)
	   vm:function-pointer-type))
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
(defun emit-divide-loop (divisor rem quo tagged)
  (labels
      ((do-loop (depth)
	 (cond
	  ((zerop depth)
	   (inst unimp 0))
	  (t
	   (let ((label-1 (gen-label))
		 (label-2 (gen-label)))
	     (inst cmp divisor rem)
	     (inst b :geu label-1)
	     (inst nop)
	     (inst sll divisor 1)
	     (do-loop (1- depth))
	     (inst srl divisor 1)
	     (inst cmp divisor rem)
	     (emit-label label-1)
	     (inst b :gtu label-2)
	     (inst sll quo 1)
	     (inst add quo (if tagged (fixnum 1) 1))
	     (inst sub rem divisor)
	     (emit-label label-2))))))
    (do-loop (if tagged 30 32))))

(define-assembly-routine (positive-fixnum-truncate
			  (:note "unsigned fixnum truncate")
			  (:cost 45)
			  (:translate truncate)
			  (:policy :fast-safe)
			  (:arg-types positive-fixnum positive-fixnum)
			  (:result-types positive-fixnum positive-fixnum))
			 ((:arg dividend any-reg nl0-offset)
			  (:arg divisor any-reg nl1-offset)

			  (:res quo any-reg nl2-offset)
			  (:res rem any-reg nl0-offset))

  (move rem dividend)
  (inst li quo 0)
  (emit-divide-loop divisor rem quo t))


(define-assembly-routine (fixnum-truncate
			  (:note "fixnum truncate")
			  (:cost 50)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types tagged-num tagged-num)
			  (:result-types tagged-num tagged-num))
			 ((:arg dividend any-reg nl0-offset)
			  (:arg divisor any-reg nl1-offset)

			  (:res quo any-reg nl2-offset)
			  (:res rem any-reg nl0-offset)

			  (:temp quo-sign any-reg nl5-offset)
			  (:temp rem-sign any-reg nargs-offset))
  
  (inst xor quo-sign dividend divisor)
  (inst move rem-sign dividend)
  (let ((label (gen-label)))
    (inst cmp dividend)
    (inst ba :lt label)
    (inst neg dividend)
    (emit-label label))
  (let ((label (gen-label)))
    (inst cmp divisor)
    (inst ba :lt label)
    (inst neg divisor)
    (emit-label label))
  (emit-divide-loop divisor rem quo t)
  (let ((label (gen-label)))
    ;; If the quo-sign is negative, we need to negate quo.
    (inst cmp quo-sign)
    (inst ba :lt label)
    (inst neg quo)
    (emit-label label))
  (let ((label (gen-label)))
    ;; If the rem-sign is negative, we need to negate rem.
    (inst cmp rem-sign)
    (inst ba :lt label)
    (inst neg rem)
    (emit-label label)))


(define-assembly-routine (signed-truncate
			  (:note "(signed-byte 32) truncate")
			  (:cost 60)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types signed-num signed-num)
			  (:result-types signed-num signed-num))

			 ((:arg dividend signed-reg nl0-offset)
			  (:arg divisor signed-reg nl1-offset)

			  (:res quo signed-reg nl2-offset)
			  (:res rem signed-reg nl0-offset)

			  (:temp quo-sign signed-reg nl5-offset)
			  (:temp rem-sign signed-reg nargs-offset))
  
  (inst xor quo-sign dividend divisor)
  (inst move rem-sign dividend)
  (let ((label (gen-label)))
    (inst cmp dividend)
    (inst ba :lt label)
    (inst neg dividend)
    (emit-label label))
  (let ((label (gen-label)))
    (inst cmp divisor)
    (inst ba :lt label)
    (inst neg divisor)
    (emit-label label))
  (emit-divide-loop divisor rem quo nil)
  (let ((label (gen-label)))
    ;; If the quo-sign is negative, we need to negate quo.
    (inst cmp quo-sign)
    (inst ba :lt label)
    (inst neg quo)
    (emit-label label))
  (let ((label (gen-label)))
    ;; If the rem-sign is negative, we need to negate rem.
    (inst cmp rem-sign)
    (inst ba :lt label)
    (inst neg rem)
    (emit-label label)))


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
				  (:temp cname descriptor-reg cname-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (inst andcc zero-tn x 3)
	  (inst b :ne DO-STATIC-FN)
	  (inst andcc zero-tn y 3)
	  (inst b :eq DO-COMPARE)
	  (inst cmp x y)
	  
	  DO-STATIC-FN
	  (load-symbol cname ',static-fn)
	  (inst li nargs (fixnum 2))
	  (loadw code-tn cname vm:symbol-raw-function-addr-slot
		 vm:other-pointer-type)
	  (inst move ocfp cfp-tn)
	  (inst j code-tn
		(- (* vm:function-header-code-offset vm:word-bytes)
		   vm:function-pointer-type))
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
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst cmp x y)
  (inst b :eq RETURN-T)
  (inst andcc zero-tn x 3)
  (inst b :eq RETURN-NIL)
  (inst andcc zero-tn y 3)
  (inst b :ne DO-STATIC-FN)
  (inst nop)

  RETURN-NIL
  (inst move res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (load-symbol cname 'eql)
  (inst li nargs (fixnum 2))
  (loadw code-tn cname vm:symbol-raw-function-addr-slot vm:other-pointer-type)
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* vm:function-header-code-offset vm:word-bytes)
	   vm:function-pointer-type))
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
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst cmp x y)
  (inst b :eq RETURN-T)
  (inst andcc zero-tn x 3)
  (inst b :ne DO-STATIC-FN)
  (inst andcc zero-tn y 3)
  (inst b :ne DO-STATIC-FN)
  (inst nop)

  (inst move res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (load-symbol cname 'two-arg-=)
  (inst li nargs (fixnum 2))
  (loadw code-tn cname vm:symbol-raw-function-addr-slot vm:other-pointer-type)
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* vm:function-header-code-offset vm:word-bytes)
	   vm:function-pointer-type))
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
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst cmp x y)
  (inst b :eq RETURN-NIL)
  (inst andcc zero-tn x 3)
  (inst b :ne DO-STATIC-FN)
  (inst andcc zero-tn y 3)
  (inst b :ne DO-STATIC-FN)
  (inst nop)

  (load-symbol res t)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (load-symbol cname 'two-arg-=)
  (inst li nargs (fixnum 2))
  (loadw code-tn cname vm:symbol-raw-function-addr-slot vm:other-pointer-type)
  (inst move ocfp cfp-tn)
  (inst j code-tn
	(- (* vm:function-header-code-offset vm:word-bytes)
	   vm:function-pointer-type))
  (inst move cfp-tn csp-tn)

  RETURN-NIL
  (inst move res null-tn))
