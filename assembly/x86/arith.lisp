;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/x86/arith.lisp,v 1.8 1997/12/05 06:55:30 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;; Debugged by Paul Werkowski -- Spring/Summer 1995.
;;;
(in-package :x86)

(eval-when (compile eval)


;;;; Addition, Subtraction, and Multiplication

(defmacro define-generic-arith-routine ((fun cost) &body body)
  `(define-assembly-routine (,(symbolicate "GENERIC-" fun)
			     (:cost ,cost)
			     (:return-style :full-call)
			     (:translate ,fun)
			     (:policy :safe)
			     (:save-p t))
			    ((:arg x (descriptor-reg any-reg) edx-offset)
			     (:arg y (descriptor-reg any-reg)
			      ;; this seems wrong esi-offset
			      edi-offset)

			     (:res res (descriptor-reg any-reg) edx-offset)

			     (:temp eax dword-reg eax-offset)
			     (:temp ebx dword-reg ebx-offset)
			     (:temp ecx dword-reg ecx-offset))
    (declare (ignorable ebx))

    (inst test x 3)			; fixnum?
    (inst jmp :nz DO-STATIC-FUN)	; no - do generic
    (inst test y 3)			; fixnum?
    (inst jmp :z DO-BODY)		; yes - doit here

    DO-STATIC-FUN
    (inst pop eax)
    (inst push ebp-tn)
    (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
    (inst sub esp-tn (fixnum 2))
    (inst push eax)			; callers return addr
    (inst mov ecx (fixnum 2))		; arg count
    (inst jmp (make-ea :dword
	       	       :disp (+ nil-value (static-function-offset
					   ',(symbolicate "TWO-ARG-" fun)))))
    
    DO-BODY
    ,@body))

); eval-when


(define-generic-arith-routine (+ 10)
  (move res x)
  (inst add res y)
  (inst jmp :no OKAY)
  (inst rcr res 1)			; carry has correct sign
  (inst sar res 1)			; remove type bits

  (move ecx res)

  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
    (storew ecx res bignum-digits-offset other-pointer-type))
  
  OKAY)


(define-generic-arith-routine (- 10)
    ;;; I can't figure out the flags on subtract. Overflow never gets
    ;;; set and carry always does. (- 0 most-negative-fixnum) can't be
    ;;; easily detected so just let the upper level stuff do it.
  (inst jmp DO-STATIC-FUN)
  
  (move res x)
  (inst sub res y)
  (inst jmp :no OKAY)
  (inst rcr res 1)
  (inst sar res 1)			; remove type bits
  
  (move ecx res)
  
  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
    (storew ecx res bignum-digits-offset other-pointer-type))
  OKAY)

(define-generic-arith-routine (* 30)
  (move eax x)				; must use eax for 64-bit result
  (inst sar eax 2)			; remove *4 fixnum bias
  (inst imul y)				; result in edx:eax
  (inst jmp :no okay)			; still fixnum

  ;; zzz jrd changed edx to ebx in here, as edx isn't listed as a temp, above
  ;;     pfw says that loses big -- edx is target for arg x and result res
  ;;     note that 'edx' is not defined -- using x
  (inst shrd eax x 2)			; high bits from edx
  (inst sar x 2)			; now shift edx too

  (move ecx x)				; save high bits from cdq
  (inst cdq)				; edx:eax <- sign-extend of eax
  (inst cmp x ecx)
  (inst jmp :e SINGLE-WORD-BIGNUM)

  (with-fixed-allocation (res bignum-type (+ bignum-digits-offset 2))
    (storew eax res bignum-digits-offset other-pointer-type)
    (storew ecx res (1+ bignum-digits-offset) other-pointer-type))
  (inst jmp DONE)

  SINGLE-WORD-BIGNUM
  
  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
    (storew eax res bignum-digits-offset other-pointer-type))
  (inst jmp DONE)

  OKAY
  (move res eax)
  DONE)

(define-assembly-routine (generic-negate
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate %negate)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) edx-offset)
			  (:res res (descriptor-reg any-reg) edx-offset)

			  (:temp eax dword-reg eax-offset)
			  (:temp ecx dword-reg ecx-offset))
  (inst test x 3)
  (inst jmp :z FIXNUM)

  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnum 2))
  (inst push eax)
  (inst mov ecx (fixnum 1))		; arg count
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset '%negate))))
  
  FIXNUM
  (move res x)
  (inst neg res)			; (- most-negative-fixnum) is BIGNUM
  (inst jmp :no OKAY)
  (inst shr res 2)			; sign bit is data - remove type bits
  (move ecx res)

  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
    (storew ecx res bignum-digits-offset other-pointer-type))
  
  OKAY)



;;;; Comparison routines.

(eval-when (compile eval)

(defmacro define-cond-assem-rtn (name translate static-fn test)
  `(define-assembly-routine (,name
			     (:cost 10)
			     (:return-style :full-call)
			     (:policy :safe)
			     (:translate ,translate)
			     (:save-p t))
			    ((:arg x (descriptor-reg any-reg) edx-offset)
			     (:arg y (descriptor-reg any-reg) edi-offset)
			     
			     (:res res descriptor-reg edx-offset)

			     (:temp eax dword-reg eax-offset)
			     (:temp ecx dword-reg ecx-offset))
    (inst test x 3)
    (inst jmp :nz DO-STATIC-FN)
    (inst test y 3)
    (inst jmp :z DO-COMPARE)
    
    DO-STATIC-FN
    (inst pop eax)
    (inst push ebp-tn)
    (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
    (inst sub esp-tn (fixnum 2))
    (inst push eax)
    (inst mov ecx (fixnum 2))
    (inst jmp (make-ea :dword
	       	       :disp (+ nil-value
				(static-function-offset ',static-fn))))
    
    DO-COMPARE
    (inst cmp x y)
    (inst jmp ,test TRUE)
    (inst mov res nil-value)
    (inst pop eax)
    (inst add eax 2)
    (inst jmp eax)
    
    TRUE
    (load-symbol res t)))

); eval-when

(define-cond-assem-rtn generic-< < two-arg-< :l)
(define-cond-assem-rtn generic-> > two-arg-> :g)

(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) edx-offset)
			  (:arg y (descriptor-reg any-reg) edi-offset)
			  
			  (:res res descriptor-reg edx-offset)
			  
			  (:temp eax dword-reg eax-offset)
			  (:temp ecx dword-reg ecx-offset))
  (inst cmp x y)
  (inst jmp :e RETURN-T)
  (inst test x 3)
  (inst jmp :z RETURN-NIL)
  (inst test y 3)
  (inst jmp :nz DO-STATIC-FN)

  RETURN-NIL
  (inst mov res nil-value)
  (inst pop eax)
  (inst add eax 2)
  (inst jmp eax)

  DO-STATIC-FN
  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnum 2))
  (inst push eax)
  (inst mov ecx (fixnum 2))
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset 'eql))))
  
  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) edx-offset)
			  (:arg y (descriptor-reg any-reg) edi-offset)
			  
			  (:res res descriptor-reg edx-offset)
			  
			  (:temp eax dword-reg eax-offset)
			  (:temp ecx dword-reg ecx-offset)
			  )
  (inst test x 3)			; descriptor?
  (inst jmp :nz DO-STATIC-FN)		; yes do it here
  (inst test y 3)			; descriptor?
  (inst jmp :nz DO-STATIC-FN)
  (inst cmp x y)
  (inst jmp :e RETURN-T)		; ok

  (inst mov res nil-value)
  (inst pop eax)
  (inst add eax 2)
  (inst jmp eax)

  DO-STATIC-FN
  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnum 2))
  (inst push eax)
  (inst mov ecx (fixnum 2))
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset 'two-arg-=))))
  
  RETURN-T
  (load-symbol res t))

