;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm/arith.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;


(in-package "ARM")



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
  (not-implemented))


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
  (not-implemented))



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
  (not-implemented))

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
	  (not-implemented))))
  (frob unsigned-* "unsigned *" 40 unsigned-num unsigned-reg)
  (frob signed-* "unsigned *" 41 signed-num signed-reg)
  (frob fixnum-* "fixnum *" 30 tagged-num any-reg))



;;;; Division.

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
  (not-implemented))


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
  (not-implemented))


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
  
  (not-implemented))


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
	  (not-implemented))))

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
  (not-implemented))

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
  (not-implemented))

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
  (not-implemented))
