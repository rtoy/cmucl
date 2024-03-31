;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/assembly/x86/arith.lisp $")
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

			     (:temp eax unsigned-reg eax-offset)
			     (:temp ebx unsigned-reg ebx-offset)
			     (:temp ecx unsigned-reg ecx-offset))
    (declare (ignorable ebx))

    (inst test x 3)			; fixnum?
    (inst jmp :nz DO-STATIC-FUN)	; no - do generic
    (inst test y 3)			; fixnum?
    (inst jmp :z DO-BODY)		; yes - doit here

    DO-STATIC-FUN
    (inst pop eax)
    (inst push ebp-tn)
    (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
    (inst sub esp-tn (fixnumize 2))
    (inst push eax)			; callers return addr
    (inst mov ecx (fixnumize 2))	; arg count
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

			  (:temp eax unsigned-reg eax-offset)
			  (:temp ecx unsigned-reg ecx-offset))
  (inst test x 3)
  (inst jmp :z FIXNUM)

  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 1))		; arg count
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

			     (:temp eax unsigned-reg eax-offset)
			     (:temp ecx unsigned-reg ecx-offset))
    (inst test x 3)
    (inst jmp :nz DO-STATIC-FN)
    (inst test y 3)
    (inst jmp :z DO-COMPARE)
    
    DO-STATIC-FN
    (inst pop eax)
    (inst push ebp-tn)
    (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
    (inst sub esp-tn (fixnumize 2))
    (inst push eax)
    (inst mov ecx (fixnumize 2))
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
			  
			  (:temp eax unsigned-reg eax-offset)
			  (:temp ecx unsigned-reg ecx-offset))
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
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
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
			  
			  (:temp eax unsigned-reg eax-offset)
			  (:temp ecx unsigned-reg ecx-offset)
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
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset 'two-arg-=))))
  
  RETURN-T
  (load-symbol res t))


;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.
;;;
;;; State:
;;;  0-1:   Constant matrix A. [0, #x9908b0df] (not used here)
;;;  2:     Index; init. to 1.
;;;  3-626: State.
;;;

;;; This assembly routine is called from the inline VOP and updates
;;; the state vector with new random numbers. The state vector is
;;; passed in the EAX register.
;;;
#+(and random-mt19937 assembler) ; we don't want a vop for this one.
(define-assembly-routine
    (random-mt19937-update)
    ((:temp state unsigned-reg eax-offset)
     (:temp k unsigned-reg ebx-offset)
     (:temp y unsigned-reg ecx-offset)
     (:temp tmp unsigned-reg edx-offset))

  ;; Save the temporary registers.
  (inst push k)
  (inst push y)
  (inst push tmp)

  ;; Generate a new set of results.
  (inst xor k k)
  LOOP1
  (inst mov y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov tmp (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 1 3 vm:vector-data-offset)
				     vm:word-bytes)
				  vm:other-pointer-type)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip1)
  (inst xor y #x9908b0df)
  SKIP1
  (inst xor y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ 397 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov (make-ea :dword :base state :index k :scale 4
		     :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type))
	y)
  (inst inc k)
  (inst cmp k (- 624 397))
  (inst jmp :b loop1)
  LOOP2
  (inst mov y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov tmp (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 1 3 vm:vector-data-offset)
				     vm:word-bytes)
				  vm:other-pointer-type)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip2)
  (inst xor y #x9908b0df)
  SKIP2
  (inst xor y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ (- 397 624) 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov (make-ea :dword :base state :index k :scale 4
		     :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type))
	y)
  (inst inc k)
  (inst cmp k (- 624 1))
  (inst jmp :b loop2)
  
  (inst mov y (make-ea :dword :base state
		       :disp (- (* (+ (- 624 1) 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov tmp (make-ea :dword :base state
			 :disp (- (* (+ 0 3 vm:vector-data-offset)
				     vm:word-bytes)
				  vm:other-pointer-type)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip3)
  (inst xor y #x9908b0df)
  SKIP3
  (inst xor y (make-ea :dword :base state
		       :disp (- (* (+ (- 397 1) 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov (make-ea :dword :base state
		     :disp (- (* (+ (- 624 1) 3 vm:vector-data-offset)
				 vm:word-bytes)
			      vm:other-pointer-type))
	y)

  ;; Restore the temporary registers and return.
  (inst pop tmp)
  (inst pop y)
  (inst pop k)
  (inst ret))

;;; Support for the xoroshiro128** generator.  See
;;; https://prng.di.unimi.it/xoroshiro128starstar.c for the official
;;; code.
;;;
;;; This is what we're implementing, where s[] is our state vector.
;;;
;;; static uint64_t s[2];
;;; static inline uint64_t rotl(const uint64_t x, int k) {
;;;   return (x << k) | (x >> (64 - k));
;;; }
;;;
;;; uint64_t next(void) {
;;;   const uint64_t s0 = s[0];
;;; 	 uint64_t s1 = s[1];
;;; 	 const uint64_t result = rotl(s0 * 5, 7) * 9;
;;; 
;;; 	 s1 ^= s0;
;;; 	 s[0] = rotl(s0, 24) ^ s1 ^ (s1 << 16); // a, b
;;; 	 s[1] = rotl(s1, 37); // c
;;; 
;;; 	 return result;
;;; }
;;;
;;; A VOP is also generated to call this assembly routine.  This
;;; routine computes a new 64-bit random number and also updates the
;;; state, which is (simple-array (double-float) (2)).
#+random-xoroshiro
(define-assembly-routine
  (xoroshiro-update
   (:translate kernel::random-xoroshiro-update)
   (:return-style :raw)
   (:cost 30)
   (:policy :fast-safe)
   (:arg-types simple-array-double-float)
   (:result-types unsigned-num unsigned-num))
  ((:arg state descriptor-reg eax-offset)
   (:res r1 unsigned-reg edx-offset)
   (:res r0 unsigned-reg ebx-offset)
   (:temp s0 double-reg xmm0-offset)
   (:temp s1 double-reg xmm1-offset)
   (:temp t0 double-reg xmm2-offset)
   (:temp t1 double-reg xmm3-offset))

  ;; s0 = state[0]
  (inst movsd s0 (make-ea :dword :base state
                                 :disp (- (+ (* vm:vector-data-offset
					        vm:word-bytes)
				             (* 8 0))
				          vm:other-pointer-type)))
  ;; t0 = s0 * 5 = s0 << 2 + s0
  (inst movapd t0 s0)                   ; t0 = s0
  (inst psllq t0 2)                     ; t0 = t0 << 2 = 4*t0
  (inst paddq t0 s0)                    ; t0 = t0 + s0 = 5*t0

  ;; t0 = rotl(t0, 7) = t0 << 7 | t0 >> (64-7)
  ;;    = rotl(s0*5, 7)
  (inst movapd t1 t0)          ; t1 = t0
  (inst psllq t1 7)            ; t1 = t0 << 7
  (inst psrlq t0 (- 64 7))     ; t0 = t0 >> 57
  (inst orpd t0 t1)            ; t0 = t0 << 7 | t0 >> 57 = rotl(t0, 7)

  ;; t0 = t0 * 9 = t0 << 3 + t0
  ;;    = rotl(s0*5, 7) * 9
  (inst movapd t1 t0)                   ; t1 = t0
  (inst psllq t1 3)                     ; t1 = t0 << 3
  (inst paddq t0 t1)                    ; t0 = t0 << 3 + t0 = 9*t0

  ;; Save the result as two 32-bit results.  r1 is the high 32 bits
  ;; and r0 is the low 32.
  (inst movd r0 t0)
  (inst psrlq t0 32)
  (inst movd r1 t0)

  ;; s1 = state[1]
  (inst movsd s1 (make-ea :dword :base state
			         :disp (- (+ (* vm:vector-data-offset
					        vm:word-bytes)
				             (* 8 1))
				          vm:other-pointer-type)))
  (inst xorpd s1 s0)                    ; s1 = s1 ^ s0

  ;; s0 can now be reused as a temp.
  ;; s0 = rotl(s0, 24)
  (inst movapd t0 s0)                   ; t0 = s0
  (inst psllq t0 24)                    ; t0 = s0 << 24
  (inst psrlq s0 (- 64 24))             ; s0 = s0 >> 40
  (inst orpd s0 t0)                     ; s0 = s0 | t0 = rotl(s0, 24)

  ;; s0 = s0 ^ s1 = rotl(s0, 24) ^ s1
  (inst xorpd s0 s1)

  ;; s0 = s0 ^ (s1 << 16)
  (inst movapd t0 s1)            ; t0 = s1
  (inst psllq t0 16)             ; t0 = s1 << 16
  (inst xorpd s0 t0)             ; s0 = rotl(s0, 24) ^ s1 ^ (s1 << 16)

  ;; Save s0 to state[0]
  (inst movsd (make-ea :dword :base state
			      :disp (- (+ (* vm:vector-data-offset
					     vm:word-bytes)
				          (* 8 0))
				       vm:other-pointer-type))
        s0)

  ;; s1 = rotl(s1, 37)
  (inst movapd t0 s1)                   ; t0 = s1
  (inst psllq t0 37)                    ; t0 = s1 << 37
  (inst psrlq s1 (- 64 37))             ; s1 = s1 >> 27
  (inst orpd s1 t0)                     ; s1 = t0 | s1 = rotl(s1, 37)

  ;; Save s1 to state[1]
  (inst movsd (make-ea :dword :base state
			      :disp (- (+ (* vm:vector-data-offset
					     vm:word-bytes)
				          (* 8 1))
				       vm:other-pointer-type))
        s1))
