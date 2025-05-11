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

			  (:res res (descriptor-reg any-reg) a0-offset))
  (emit-not-implemented))


(define-assembly-routine (generic--
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset))
  (emit-not-implemented))



;;;; Multiplication


(define-assembly-routine (generic-*
			  (:cost 50)
			  (:return-style :full-call)
			  (:translate *)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset))
  (emit-not-implemented))

;; I (rtoy) am ripping this out right now because it doesn't
;; compile. The routine wants 2 non-descriptors and we currently only
;; have 1.  Plus, do we really need these assembly routines?  Is there
;; any reason why this would get called instead of the inline
;; multiplication vops that we already have?
#+nil
(macrolet
    ((frob (name note cost type sc)
       `(define-assembly-routine (,name
				  (:note ,note)
				  (:cost ,cost)
				  (:translate *)
				  (:policy :fast-safe)
				  (:arg-types ,type ,type)
				  (:result-types ,type))
				 ((:arg x ,sc a0-offset)
				  (:arg y ,sc a1-offset)
				  (:res res ,sc a0-offset))
	  (emit-not-implemented))))
  (frob unsigned-* "unsigned *" 40 unsigned-num descriptor-reg)
  (frob signed-* "unsigned *" 41 signed-num descriptor-reg)
  (frob fixnum-* "fixnum *" 30 tagged-num any-reg))



;;;; Division.

(define-assembly-routine (positive-fixnum-truncate
			  (:note "unsigned fixnum truncate")
			  (:cost 45)
			  (:translate truncate)
			  (:policy :fast-safe)
			  (:arg-types positive-fixnum positive-fixnum)
			  (:result-types positive-fixnum positive-fixnum))
			 ((:arg dividend any-reg a0-offset)
			  (:arg divisor any-reg a1-offset)

			  (:res quo any-reg a0-offset)
			  (:res rem any-reg a1-offset))
  (emit-not-implemented))


(define-assembly-routine (fixnum-truncate
			  (:note "fixnum truncate")
			  (:cost 50)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types tagged-num tagged-num)
			  (:result-types tagged-num tagged-num))
			 ((:arg dividend any-reg a0-offset)
			  (:arg divisor any-reg a1-offset)

			  (:res quo any-reg a0-offset)
			  (:res rem any-reg a1-offset))
  (emit-not-implemented))


#+nil
(define-assembly-routine (signed-truncate
			  (:note "(signed-byte 32) truncate")
			  (:cost 60)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types signed-num signed-num)
			  (:result-types signed-num signed-num))

			 ((:arg dividend signed-reg nl0-offset)
			  (:res rem signed-reg nl0-offset))
  
  (emit-not-implemented))


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
				  
				  (:res res descriptor-reg a0-offset))
	  (emit-not-implemented))))

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
			  
			  (:res res descriptor-reg a0-offset))
  (emit-not-implemented))

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset))
  (emit-not-implemented))

(define-assembly-routine (generic-/=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate /=)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset))
  (emit-not-implemented))
