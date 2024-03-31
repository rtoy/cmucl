;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/compiler/x86/arith.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition arithmetic VOPs for the x86.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,2000.
;;; 

(in-package :x86)
(intl:textdomain "cmucl-x86-vm")


;;;; Unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))


(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg) :target res))
  (:results (res :scs (any-reg)))
  (:note _N"inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg) :target res))
  (:results (res :scs (signed-reg)))
  (:note _N"inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (move res x)
    (inst neg res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (move res x)
    (inst neg res)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (move res x)
    (inst xor res (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (move res x)
    (inst not res)))



;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg)
			       (sc-is r control-stack)
			       (location= x r))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)
	       :load-if (not (and (sc-is x control-stack)
				  (sc-is y any-reg)
				  (sc-is r control-stack)
				  (location= x r)))))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r)))))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg)
			       (sc-is r signed-stack)
			       (location= x r))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg)
			       (sc-is r signed-stack)
			       (location= x r)))))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)
	       :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg unsigned-stack)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:results (r :scs (unsigned-reg)
	       :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)
	       :load-if (not (location= x r))))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 32) arithmetic"))


(eval-when (compile load eval)

(defmacro define-binop (translate untagged-penalty op)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
		  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
	 (move r x)
	 (inst ,op r y)))
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
		  fast-fixnum-binop-c)
       (:translate ,translate)
       (:generator 1
	 (move r x)
	 (inst ,op r (fixnumize y))))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (move r x)
	 (inst ,op r y)))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
		  fast-signed-binop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (move r x)
	 (inst ,op r y)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (move r x)
	 (inst ,op r y)))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
		  fast-unsigned-binop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (move r x)
	 (inst ,op r y)))))

); eval-when



(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logior 2 or)
(define-binop logxor 2 xor)


;;;; Special logand cases: (logand signed unsigned) => unsigned

(define-vop (fast-logand/signed-unsigned=>unsigned
	     fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand-c/signed-unsigned=>unsigned
	     fast-logand-c/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (unsigned-byte 32)))
  (:generator 1
    (cond ((and (= y #xffffffff)
		(sc-is x signed-reg))
	   ;; Just move x to the result if we're and'ing with all
	   ;; ones, which doesn't change the bits.
	   (unless (location= x r)
	     (move r x)))
	  (t
	   (move r x)
	   (inst and r y)))))
	   
(define-vop (fast-logand/unsigned-signed=>unsigned
	     fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (unsigned-reg)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y signed-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types unsigned-num signed-num))


;;;; Multiplication and division.

(define-vop (fast-*/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg) :target r)
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic")
  (:generator 4
    (move r x)
    (inst sar r 2)
    (inst imul r y)))

(define-vop (fast-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic")
  (:generator 3
    (inst imul r x y)))

(define-vop (fast-*/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg) :target r)
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 32) arithmetic")
  (:generator 5
    (move r x)
    (inst imul r y)))

(define-vop (fast-*-c/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 32) arithmetic")
  (:generator 4
    (inst imul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-safe-arith-op)
  (:translate *)
  (:args (x :scs (unsigned-reg) :target r)
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic")
  (:save-p :compute-only)
  (:generator 5
    (move r x)
    (inst imul r y)))

(define-vop (fast-truncate/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax)
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
		   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (any-reg))
	    (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note _N"inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 31
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (if (sc-is y any-reg)
	  (inst test y y)  ; Smaller instruction
	  (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst cdq)
    (inst idiv eax y)
    (if (location= quo eax)
	(inst shl eax 2)
	(inst lea quo (make-ea :dword :index eax :scale 4)))
    (move rem edx)))

#+nil
(define-vop (fast-truncate-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from :argument :to (:result 0)) eax)
  (:temporary (:sc any-reg :offset edx-offset :target rem
		   :from :eval :to (:result 1)) edx)
  (:temporary (:sc any-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (any-reg))
	    (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note _N"inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    (move eax x)
    (inst cdq)
    (inst mov y-arg (fixnumize y))
    (inst idiv eax y-arg)
    (if (location= quo eax)
	(inst shl eax 2)
	(inst lea quo (make-ea :dword :index eax :scale 4)))
    (move rem edx)))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg signed-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target quo
		   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
		   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (if (sc-is y unsigned-reg)
	  (inst test y y)  ; Smaller instruction
	  (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst xor edx edx)
    (inst div eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/unsigned=>unsigned fast-unsigned-binop-c)
  (:translate truncate)
  (:args (x :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (r :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:temporary (:sc unsigned-reg :offset edx-offset) edx)
  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
  (:generator 6
    (multiple-value-bind (recip shift overflowp)
        (c::find-unsigned-reciprocal y vm:word-bits)
      ;; q = floor(M*x/2^32)
      (inst mov eax recip)
      (inst mul eax x)			; edx:eax = x*recip
      (cond (overflowp
	     ;; The case where the sum overflows.  X86 has a rotate
	     ;; with carry instruction so use that to get the MSB of
	     ;; the sum and then a regular shift to get the correct
	     ;; number of shifts.
	     (inst add edx x)
	     (inst rcr edx 1)
	     (when (> shift 1)
	       (inst shr edx (1- shift))))
            (t
             ;; The easy case
             (unless (zerop shift)
               (inst shr edx shift))))
      ;; Compute the remainder
      (move rem x)			; Save x in case r is the same tn
      (move r edx)
      (move eax edx)
      (inst mov edx y)
      (inst mul eax edx)
      (inst sub rem eax))))

(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax)
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset edx-offset :target rem
		   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (signed-reg))
	    (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note _N"inline (signed-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (if (sc-is y signed-reg)
	  (inst test y y)  ; Smaller instruction
	  (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst cdq)
    (inst idiv eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/signed=>signed fast-signed-binop-c)
  (:translate truncate)
  (:args (x :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (r :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:temporary (:sc signed-reg :offset edx-offset) edx)
  (:temporary (:sc signed-reg :offset eax-offset) eax)
  (:generator 13
    (multiple-value-bind (recip shift)
        (c::find-signed-reciprocal y vm:word-bits)
      ;; Compute q = floor(M*n/2^32).  That is, the high half of the
      ;; product.
      (inst mov eax recip)
      (inst imul x)			; edx:eax = x * recip
      ;; Adjust if the M is negative.
      (when (minusp recip)
        (inst add edx x))
      ;; Shift quotient as needed.
      (unless (zerop shift)
	(inst sar edx shift))
      ;; Add one to quotient if X is negative.  This is done by right
      ;; shifting X to give either -1 or 0.  Then subtract this from
      ;; the quotient.  (NOTE: in the book, the sample code has this
      ;; wrong and ADDS instead of SUBTRACTS.)
      (move eax x)
      (inst sar eax 31)
      (inst sub edx eax)

      ;; Now compute the remainder.
      (move rem x)
      (move r edx)			; Save quotient for return
      (inst imul edx y)			; edx = q * y
      (inst sub rem edx))))


;;;; Shifting
(define-vop (fast-ash-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result
		 :load-if (not (and (sc-is number any-reg control-stack)
				    (sc-is result any-reg control-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types tagged-num (:constant integer))
  (:results (result :scs (any-reg)
		    :load-if (not (and (sc-is number control-stack)
				       (sc-is result control-stack)
				       (location= number result)))))
  (:result-types tagged-num)
  (:note _N"inline ASH")
  (:generator 2
    (move result number)
    (cond ((plusp amount)
	   ;; We don't have to worry about overflow because of the
	   ;; result type restriction.
	   (inst shl result amount))
	  (t
	   ;; If the amount is greater than 31, only shift by 31.  We
	   ;; have to do this because the shift instructions only look
	   ;; at the low five bits of the result.
	   (inst sar result (min 31 (- amount)))
	   ;; Fixnum correction.
	   (inst and result #xfffffffc)))))

(define-vop (fast-ash-left/fixnum=>fixnum)
  (:translate ash)
  (:args (number :scs (any-reg) :target result
		 :load-if (not (and (sc-is number control-stack)
				    (sc-is result control-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types tagged-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (any-reg) :from (:argument 0)
		    :load-if (not (and (sc-is number control-stack)
				       (sc-is result control-stack)
				       (location= number result)))))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:note _N"inline ASH")
  (:generator 3
    (move result number)
    (move ecx amount)
    ;; The result-type assures us that this shift will not overflow.
    (inst shl result :cl)))

(define-vop (fast-ash-c/unsigned=>unsigned)
	    (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result
		 :load-if (not (and (sc-is number unsigned-stack)
				    (sc-is result unsigned-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)
		    :load-if (not (and (sc-is number unsigned-stack)
				       (sc-is result unsigned-stack)
				       (location= number result)))))
  (:result-types unsigned-num)
  (:note _N"inline ASH")
  (:generator 3
    (move result number)
    (cond ((plusp amount)
	   ;; We don't have to worry about overflow because of the
	   ;; result type restriction.
	   (inst shl result amount))
	  ((< amount -31)
	   (inst mov result 0))
	  (t
	   (inst shr result (- amount))))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result
		 :load-if (not (and (sc-is number signed-stack)
				    (sc-is result signed-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)
		    :load-if (not (and (sc-is number signed-stack)
				       (sc-is result signed-stack)
				       (location= number result)))))
  (:result-types signed-num)
  (:note _N"inline ASH")
  (:generator 3
    (move result number)
    (cond ((plusp amount)
	   ;; We don't have to worry about overflow because of the
	   ;; result type restriction.
	   (inst shl result amount))
	  (t
	   ;; If the amount is greater than 31, only shift by 31.  We
	   ;; have to do this because the shift instructions only look
	   ;; at the low five bits of the result.
	   (inst sar result (min 31 (- amount)))))))

(define-vop (fast-ash-c/fixnum=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result
		 :load-if (not (and (sc-is number signed-stack)
				    (sc-is result signed-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types fixnum (:constant integer))
  (:results (result :scs (signed-reg)
		    :load-if (not (and (sc-is number signed-stack)
				       (sc-is result signed-stack)
				       (location= number result)))))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 1
    (let ((shift (- amount vm:fixnum-tag-bits)))
      (move result number)
      (cond ((plusp shift)
	     ;; We don't have to worry about overflow because of the
	     ;; result type restriction.
	     (inst shl result shift))
	    (t
	     ;; If the shift is greater than 31, only shift by 31.  We
	     ;; have to do this because the shift instructions only look
	     ;; at the low five bits of the result.
	     (inst sar result (min 31 (- shift))))))))

(define-vop (fast-ash-left/unsigned=>unsigned)
  (:translate ash)
  (:args (number :scs (unsigned-reg) :target result
		 :load-if (not (and (sc-is number unsigned-stack)
				    (sc-is result unsigned-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
		    :load-if (not (and (sc-is number unsigned-stack)
				       (sc-is result unsigned-stack)
				       (location= number result)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:note _N"inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    ;; The result-type assures us that this shift will not overflow.
    (inst shl result :cl)))

(define-vop (fast-ash-left/signed=>signed)
  (:translate ash)
  (:args (number :scs (signed-reg) :target result
		 :load-if (not (and (sc-is number signed-stack)
				    (sc-is result signed-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types signed-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (signed-reg) :from (:argument 0)
		    :load-if (not (and (sc-is number signed-stack)
				       (sc-is result signed-stack)
				       (location= number result)))))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:note _N"inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    ;; The result-type assures us that this shift will not overflow.
    (inst shl result :cl)))

(define-vop (fast-ash/unsigned=>unsigned)
	    (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note _N"inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns POSITIVE)
    (inst neg ecx)
    (inst cmp ecx 31)
    (inst jmp :be OKAY)
    (inst xor result result)
    (inst jmp DONE)
    OKAY
    (inst shr result :cl)
    (inst jmp DONE)
    
    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst shl result :cl)
    
    DONE))

(define-vop (fast-ash/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note _N"inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst cmp ecx 31)
    (inst jmp :be okay)
    (inst mov ecx 31)
    OKAY
    (inst sar result :cl)
    (inst jmp done)
      
    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst shl result :cl)
      
    DONE))


(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note _N"inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target res))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 30
    (move res arg)
    (inst cmp res 0)
    (inst jmp :ge POS)
    (inst not res)
    POS
    (inst bsr res res)
    (inst jmp :z DONE)
    (inst inc res)
    (inst shl res 2)
    DONE))

(define-vop (unsigned-byte-32-len)
  (:translate integer-length)
  (:note _N"inline (unsigned-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 30
    ;; The Intel docs say that BSR leaves the destination register
    ;; undefined if the source is 0.  However, gcc, LLVM, and MSVC
    ;; generate code that pretty much says BSR basically moves the
    ;; source to the destination if the source is 0.
    (inst bsr res arg)
    (inst jmp :z DONE)
    ;; The result of BSR is one too small for what we want, so
    ;; increment the result.
    (inst inc res)
    (inst shl res 2)
    DONE))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note _N"inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:generator 30
    (move result arg)

    (inst mov temp result)
    (inst shr temp 1)
    (inst and result #x55555555)
    (inst and temp #x55555555)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 2)
    (inst and result #x33333333)
    (inst and temp #x33333333)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 4)
    (inst and result #x0f0f0f0f)
    (inst and temp #x0f0f0f0f)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 8)
    (inst and result #x00ff00ff)
    (inst and temp #x00ff00ff)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 16)
    (inst and result #x0000ffff)
    (inst and temp #x0000ffff)
    (inst add result temp)))

(define-vop (sse3-unsigned-byte-32-count)
  (:translate logcount)
  (:note _N"inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:guard (backend-featurep :sse3))
  (:generator 2
    (inst popcnt result arg)))


;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note _N"inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:note _N"inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:info target not-p y))


(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar
	#'(lambda (suffix cost signed)
	    `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
					   tran suffix))
			  ,(intern
			    (format nil "~:@(FAST-CONDITIONAL~A~)"
				    suffix)))
	       (:translate ,tran)
	       (:generator ,cost
		 (inst cmp x
		       ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y))
		 (inst jmp (if not-p
			       ,(if signed not-cond not-unsigned)
			     ,(if signed cond unsigned))
		       target))))
	'(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
	'(4 3 6 5 6 5)
	'(t t t t nil nil))))

(define-conditional-vop < :l :b :ge :ae)

(define-conditional-vop > :g :a :le :be)

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql)
  (:generator 6
    (inst cmp x y)
    (inst jmp (if not-p :ne :e) target)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql)
  (:generator 5
    (cond ((and (sc-is x signed-reg) (zerop y))
	   (inst test x x))  ; Smaller instruction
	  (t
	   (inst cmp x y)))
    (inst jmp (if not-p :ne :e) target)))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql)
  (:generator 6
    (inst cmp x y)
    (inst jmp (if not-p :ne :e) target)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql)
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (zerop y))
	   (inst test x x))  ; Smaller instruction
	  (t
	   (inst cmp x y)))
    (inst jmp (if not-p :ne :e) target)))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note _N"inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst jmp (if not-p :ne :e) target)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (cond ((and (sc-is x any-reg descriptor-reg) (zerop y))
	   (inst test x x))  ; Smaller instruction
	  (t
	   (inst cmp x (fixnumize y))))
    (inst jmp (if not-p :ne :e) target)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg control-stack)))
  (:arg-types * (:constant (signed-byte 30)))
  (:variant-cost 6))


;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg) :target ecx)
	 (prev :scs (unsigned-reg) :target result)
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 0)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 1)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 4
    (move ecx shift)
    (move result prev)
    (inst shrd result next :cl)))

(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg) :target r
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (unsigned-reg)
	    :load-if (or (not (sc-is y unsigned-stack))
			 (and (sc-is x unsigned-stack)
			      (sc-is y unsigned-stack)
			      (location= x r)))))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)  :from (:argument 0)
	       :load-if (not (and (sc-is x unsigned-stack)
				  (sc-is r unsigned-stack)
				  (location= x r)))))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg) :target r
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is r unsigned-stack)
			       (location= x r)))))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)
	       :load-if (not (and (sc-is x unsigned-stack)
				  (sc-is r unsigned-stack)
				  (location= x r)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (move r x)
    (inst not r)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (move r x)
    (inst and r y)))

(def-source-transform 32bit-logical-nand (x y)
  `(32bit-logical-not (32bit-logical-and ,x ,y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (move r x)
    (inst or r y)))

(def-source-transform 32bit-logical-nor (x y)
  `(32bit-logical-not (32bit-logical-or ,x ,y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (move r x)
    (inst xor r y)))

(def-source-transform 32bit-logical-eqv (x y)
  `(32bit-logical-not (32bit-logical-xor ,x ,y)))

(def-source-transform 32bit-logical-orc1 (x y)
  `(32bit-logical-or (32bit-logical-not ,x) ,y))

(def-source-transform 32bit-logical-orc2 (x y)
  `(32bit-logical-or ,x (32bit-logical-not ,y)))

(def-source-transform 32bit-logical-andc1 (x y)
  `(32bit-logical-and (32bit-logical-not ,x) ,y))

(def-source-transform 32bit-logical-andc2 (x y)
  `(32bit-logical-and ,x (32bit-logical-not ,y)))

;;; Only the lower 5 bits of the shift amount are significant.
(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg) :target r)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num tagged-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (r :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note _N"SHIFT-TOWARDS-START")
  (:generator 1
    (move r num)
    (move ecx amount)
    (inst shr r :cl)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note _N"SHIFT-TOWARDS-END")
  (:generator 1
    (move r num)
    (move ecx amount)
    (inst shl r :cl)))



;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-type
  (unsigned-reg) unsigned-num bignum::%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-type
  (unsigned-reg) unsigned-num bignum::%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional)
  (:info target not-p)
  (:generator 3
    (inst or digit digit)
    (inst jmp (if not-p :s :ns) target)))


;;; For add and sub with carry the sc of carry argument is any-reg so
;;; the it may be passed as a fixnum or word and thus may be 0, 1, or
;;; 4. This is easy to deal with and may save a fixnum-word
;;; conversion.
;;;
(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target result)
	 (b :scs (unsigned-reg unsigned-stack) :to :eval)
	 (c :scs (any-reg) :target temp))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 2) :to :eval) temp)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
	    (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (move result a)
    (move temp c)
    (inst neg temp) ; Set the carry flag to 0 if c=0 else to 1
    (inst adc result b)
    (inst mov carry 0)
    (inst adc carry carry)))

;;; Note: the borrow is the oppostite of the x86 convention - 1 for no
;;; borrow and 0 for a borrow.
(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :to :eval :target result)
	 (b :scs (unsigned-reg unsigned-stack) :to :result)
	 (c :scs (any-reg control-stack)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from :eval)
	    (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 5
    (inst cmp c 1) ; Set the carry flag to 1 if c=0 else to 0
    (move result a)
    (inst sbb result b)
    (inst mov borrow 0)
    (inst adc borrow borrow)
    (inst xor borrow 1)))


(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack))
	 (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
		   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)
    (move lo eax)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack))
	 (prev :scs (unsigned-reg unsigned-stack))
	 (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
		   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (inst add eax prev)
    (inst adc edx 0)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)
    (move lo eax)))


(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
		   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (move hi edx)
    (move lo eax)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg unsigned-stack) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)
	       :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:generator 1
    (move r x)
    (inst not r)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg control-stack) :target digit))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)
		   :load-if (not (and (sc-is fixnum control-stack)
				      (sc-is digit unsigned-stack)
				      (location= fixnum digit)))))
  (:result-types unsigned-num)
  (:generator 1
    (move digit fixnum)
    (inst sar digit 2)))

(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target edx)
	 (div-low :scs (unsigned-reg) :target eax)
	 (divisor :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 1)
		   :to (:result 0) :target quo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 0)
		   :to (:result 1) :target rem) edx)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move edx div-high)
    (move eax div-low)
    (inst div eax divisor)
    (move quo eax)
    (move rem edx)))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)
		 :load-if (not (and (sc-is digit unsigned-stack)
				    (sc-is res control-stack signed-stack)
				    (location= digit res)))))
  (:result-types signed-num)
  (:generator 1
    (move res digit)
    (when (sc-is res any-reg control-stack)
      (inst shl res 2))))

(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result)
	 (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
		    :load-if (not (and (sc-is result unsigned-stack)
				       (location= digit result)))))
  (:result-types unsigned-num)
  (:generator 2
    (move result digit)
    (move ecx count)
    (inst sar result :cl)))

(define-vop (digit-ashr-c)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result))
  (:info count)
  (:arg-types unsigned-num (:constant (unsigned-byte #.(1- (integer-length vm:word-bits)))))
  (:results (result :scs (unsigned-reg) :from (:argument 0)
		    :load-if (not (and (sc-is result unsigned-stack)
				       (location= digit result)))))
  (:result-types unsigned-num)
  (:generator 1
    (move result digit)
    ;; If the count is greater than 31, it's the same as
    ;; shifting by 31, leaving just the sign bit.
    (inst sar result count)))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 2
    (move result digit)
    (move ecx count)
    (inst shr result :cl)))

(define-vop (digit-lshr-c digit-ashr-c)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
    (move result digit)
    (inst shr result count)))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 2
    (move result digit)
    (move ecx count)
    (inst shl result :cl)))

(define-vop (digit-ashl-c digit-ashr-c)
  (:translate bignum::%ashl)
  (:generator 1
    (move result digit)
    (inst shl result count)))




;;;; Static functions.

(define-static-function two-arg-/ (x y) :translate /)

(define-static-function two-arg-gcd (x y) :translate gcd)
(define-static-function two-arg-lcm (x y) :translate lcm)

(define-static-function two-arg-and (x y) :translate logand)
(define-static-function two-arg-ior (x y) :translate logior)
(define-static-function two-arg-xor (x y) :translate logxor)


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
(defknown random-mt19937 ((simple-array (unsigned-byte 32) (*)))
  (unsigned-byte 32) ())
;;;
(define-vop (random-mt19937)
  (:policy :fast-safe)
  (:translate random-mt19937)
  (:args (state :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to :result) k)
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from (:eval 0) :to :result) tmp)
  (:results (y :scs (unsigned-reg) :from (:eval 0)))
  (:result-types unsigned-num)
  (:generator 50
    (inst mov k (make-ea :dword :base state
			 :disp (- (* (+ 2 vm:vector-data-offset) vm:word-bytes)
				  vm:other-pointer-type)))
    (inst cmp k 624)
    (inst jmp :ne no-update)
    (inst mov tmp state)	; The state is passed in EAX.
    (inst call (make-fixup 'random-mt19937-update :assembly-routine))
    ;; Restore k, and set to 0.
    (inst xor k k)
    NO-UPDATE
    ;; y = ptgfsr[k++];
    (inst mov y (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
				  vm:other-pointer-type)))
    ;; y ^= (y >> 11);
    (inst shr y 11)
    (inst xor y (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
				  vm:other-pointer-type)))
    ;; y ^= (y << 7) & #x9d2c5680
    (inst mov tmp y)
    (inst inc k)
    (inst shl tmp 7)
    (inst mov (make-ea :dword :base state
		       :disp (- (* (+ 2 vm:vector-data-offset) vm:word-bytes)
				vm:other-pointer-type))
	  k)
    (inst and tmp #x9d2c5680)
    (inst xor y tmp)
    ;; y ^= (y << 15) & #xefc60000
    (inst mov tmp y)
    (inst shl tmp 15)
    (inst and tmp #xefc60000)
    (inst xor y tmp)
    ;; y ^= (y >> 18);
    (inst mov tmp y)
    (inst shr tmp 18)
    (inst xor y tmp)))


;;; Modular arithmetic
;;; logical operations
#+modular-arith
(progn
(c::define-modular-fun lognot-mod32 (x) lognot 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg unsigned-stack) :target r
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is r unsigned-stack)
			       (location= x r)))))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)
	       :load-if (not (and (sc-is x unsigned-stack)
				  (sc-is r unsigned-stack)
				  (location= x r)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (move r x)
    (inst not r)))

(define-vop (lognot-mod32/signed=>unsigned lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (signed-reg signed-stack)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is r unsigned-stack)))))
  (:arg-types signed-num))

;; Handle (ldb (byte 32 0) (- x)).  The (- x) gets converted to
;; (%negate x), so we build modular functions for %negate.

(c::define-modular-fun %negate-mod32 (x) kernel:%negate 32)

(define-vop (%negate-mod32/unsigned=>unsigned)
  (:translate %negate-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    (move res x)	      
    (inst neg res)))

(define-vop (%negate-mod32/signed=>unsigned)
  (:translate %negate-mod32)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    (move res x)
    (inst neg res)))

(macrolet
    ((frob (op)
       (let ((name (symbolicate "FAST-" op "/SIGNED=>UNSIGNED"))
	     (vop (symbolicate "FAST-" op "/SIGNED=>SIGNED"))
	     (trans (symbolicate op "-MOD32")))
	 `(progn
	    (defknown ,trans ((signed-byte 32) (signed-byte 32))
	      (unsigned-byte 32)
	      (movable foldable flushable))
	    (define-vop (,name ,vop)
	    (:translate ,trans)
	    (:results (r :scs (unsigned-reg)))
	    (:result-types unsigned-num))))))
  (frob +)
  (frob -)
  (frob logxor)
  (frob *))

(defmacro define-modular-backend (fun &optional constantp derived)
  (let ((mfun-name (symbolicate fun '-mod32))
	(modvop (symbolicate 'fast- fun '-mod32/unsigned=>unsigned))
	(modcvop (symbolicate 'fast- fun '-mod32-c/unsigned=>unsigned))
	(vop (symbolicate 'fast- (or derived fun) '/unsigned=>unsigned))
	(cvop (symbolicate 'fast- (or derived fun) '-c/unsigned=>unsigned))
	(smodvop (symbolicate 'fast- (or derived fun) '-mod32/signed=>unsigned))
	(svop (symbolicate 'fast- (or derived fun) '/signed=>unsigned)))
    `(progn
       (c::define-modular-fun ,mfun-name (x y) ,fun 32)
       (define-vop (,modvop ,vop)
	 (:translate ,mfun-name))
       ,@(when constantp
	       `((define-vop (,modcvop ,cvop)
		   (:translate ,mfun-name))))
       (define-vop (,smodvop ,svop)
	 (:translate ,mfun-name)))))

(define-modular-backend + t)
(define-modular-backend - t)
(define-modular-backend logxor t)
(define-modular-backend *)

(c::def-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(c::def-source-transform logandc1 (x y)
  `(logand (lognot ,x) ,y))
(c::def-source-transform logandc2 (x y)
  `(logand ,x (lognot ,y)))
(c::def-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(c::def-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))
(c::def-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))
(c::def-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

(defknown vm::ash-left-mod32 (integer (integer 0))
  (unsigned-byte 32)
  (foldable flushable movable))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod32))

)

(in-package :C)
#+modular-arith
(progn
(define-modular-fun-optimizer ash ((integer count) :width width)
  ;; The count needs to be (unsigned-byte 32) because the Sparc shift
  ;; instruction takes the count modulo 32.  (NOTE: Should we make
  ;; this work on Ultrasparcs?  We could then use the sllx instruction
  ;; which takes the count mod 64.  Then a left shift of 32 or more
  ;; will produce 0 in the lower 32 bits of the register, which is
  ;; what we want.)
  (when (and (<= width 32)
	     (csubtypep (continuation-type count)
			(specifier-type '(unsigned-byte 5))))
    (cut-to-width integer width)
    'vm::ash-left-mod32))
)

(in-package :x86)

(defknown ash-right-signed ((signed-byte #.vm:word-bits)
			    (and fixnum unsigned-byte))
  (signed-byte #.vm:word-bits)
  (movable foldable flushable))

(defknown ash-right-unsigned ((unsigned-byte #.vm:word-bits)
			      (and fixnum unsigned-byte))
  (unsigned-byte #.vm:word-bits)
  (movable foldable flushable))

(macrolet
    ((frob (trans name sc-type type shift-inst cost)
       `(define-vop (,name)
	  (:note _N"inline right ASH")
	  (:translate ,trans)
	  (:args (number :scs (,sc-type))
		 (amount :scs (signed-reg unsigned-reg immediate)))
	  (:arg-types ,type positive-fixnum)
	  (:results (result :scs (,sc-type)))
	  (:result-types ,type)
	  (:policy :fast-safe)
	  (:temporary (:sc unsigned-reg :offset ecx-offset) cl)
	  (:generator ,cost
	    (sc-case amount
	      ((signed-reg unsigned-reg)
	       (move cl amount)
	       (move result number)
	       (inst ,shift-inst result :cl))
	      (immediate
	       (let ((amt (tn-value amount)))
		 (move result number)
		 (inst ,shift-inst result amt))))))))
  (frob ash-right-signed fast-ash-right/signed=>signed
	signed-reg signed-num sar 4)
  (frob ash-right-unsigned fast-ash-right/unsigned=>unsigned
	unsigned-reg unsigned-num shr 4))

;; Constant right shift.
(macrolet
    ((frob (trans name sc-type type shift-inst cost max-shift)
       `(define-vop (,name)
	  (:note _N"inline right ASH")
	  (:translate ,trans)
	  (:args (number :target result :scs (,sc-type)))
	  (:info amount)
	  (:arg-types ,type
		      (:constant (integer 0 ,max-shift)))
	  (:results (result :scs (,sc-type)))
	  (:result-types ,type)
	  (:policy :fast-safe)
	  (:generator 4
	    (cond ((zerop amount)
		   (move result number))
		  (t
		   (move result number)
		   (inst ,shift-inst result amount)))))))
  (frob ash-right-signed fast-ash-right-c/signed=>signed
	signed-reg signed-num sar 1 31)
  (frob ash-right-unsigned fast-ash-right-c/unsigned=>unsigned
	unsigned-reg unsigned-num shr 1 31))

;; FIXME: The following stuff for right shifts should be moved to
;; vm-tran (or somewhere common), once we make it the same on sparc
;; and ppc.

;; Need these so constant folding works with the deftransform.

(defun ash-right-signed (num shift)
  (declare (type (signed-byte #.vm:word-bits) num)
	   (type (integer 0 #.(1- vm:word-bits)) shift))
  (ash num (- shift)))

(defun ash-right-unsigned (num shift)
  (declare (type (unsigned-byte #.vm:word-bits) num)
	   (type (integer 0 #.(1- vm:word-bits)) shift))
  (ash num (- shift)))

;; If we can prove that we have a right shift, just do the right shift
;; instead of calling the inline ASH which has to check for the
;; direction of the shift at run-time.
(in-package "C")

(deftransform ash ((num shift) (integer integer))
  (let ((num-type (continuation-type num))
	(shift-type (continuation-type shift)))
    ;; Can only handle right shifts
    (unless (csubtypep shift-type (specifier-type '(integer * 0)))
      (give-up))

    ;; If we can prove the shift is so large that all bits are shifted
    ;; out, return the appropriate constant.  If the shift is small
    ;; enough, call the VOP.  Otherwise, check for the shift size and
    ;; do the appropriate thing.  (Hmm, could we just leave the IF
    ;; s-expr and depend on other parts of the compiler to delete the
    ;; unreachable parts, if any?)
    (cond ((csubtypep num-type (specifier-type '(signed-byte #.vm:word-bits)))
	   ;; A right shift by 31 is the same as a right shift by
	   ;; larger amount.  We get just the sign.
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
	       `(vm::ash-right-signed num (- shift))
	       `(vm::ash-right-signed num (min (- shift) #.(1- vm:word-bits)))))
	  ((csubtypep num-type (specifier-type '(unsigned-byte #.vm:word-bits)))
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
	       `(vm::ash-right-unsigned num (- shift))
	       `(if (<= shift #.(- vm:word-bits))
		 0
		 (vm::ash-right-unsigned num (- shift)))))
	  (t
	   (give-up)))))

(in-package "VM")

#+random-xoroshiro
(progn
(defknown xoroshiro-next ((simple-array double-float (2)))
  (values (unsigned-byte 32) (unsigned-byte 32))
  (movable))

(define-vop (xoroshiro-next)
  (:policy :fast-safe)
  (:translate xoroshiro-next)
  (:args (state :scs (descriptor-reg) :to (:result 3)))
  (:arg-types simple-array-double-float)
  (:results (r1 :scs (unsigned-reg))
	    (r0 :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:temporary (:sc double-reg) s0)
  (:temporary (:sc double-reg) s1)
  (:temporary (:sc double-reg) t0)
  (:temporary (:sc double-reg) t1)
  (:generator 10
    ;; See https://prng.di.unimi.it/xoroshiro128starstar.c for the official code.
    ;;
    ;; This is what we're implementing, where s[] is our state vector.
    ;;
    ;; static uint64_t s[2];
    ;; static inline uint64_t rotl(const uint64_t x, int k) {
    ;;   return (x << k) | (x >> (64 - k));
    ;; }
    ;;
    ;; uint64_t next(void) {
    ;;   const uint64_t s0 = s[0];
    ;; 	 uint64_t s1 = s[1];
    ;; 	 const uint64_t result = rotl(s0 * 5, 7) * 9;
    ;; 
    ;; 	 s1 ^= s0;
    ;; 	 s[0] = rotl(s0, 24) ^ s1 ^ (s1 << 16); // a, b
    ;; 	 s[1] = rotl(s1, 37); // c
    ;; 
    ;; 	 return result;
    ;; }

    ;; s0 = state[0]
    (inst movsd s0 (make-ea :dword :base state
                            :disp (- (+ (* vm:vector-data-offset
					   vm:word-bytes)
				        (* 8 0))
				     vm:other-pointer-type)))
    ;; t0 = s0 * 5 = s0 << 2 + s0
    (inst movapd t0 s0)                 ; t0 = s0
    (inst psllq t0 2)                   ; t0 = t0 << 2 = 4*t0
    (inst paddq t0 s0)                  ; t0 = t0 + s0 = 5*t0

    ;; t0 = rotl(t0, 7) = t0 << 7 | t0 >> (64-7)
    ;;    = rotl(s0*5, 7)
    (inst movapd t1 t0)        ; t1 = t0
    (inst psllq t1 7)          ; t1 = t0 << 7
    (inst psrlq t0 (- 64 7))   ; t0 = t0 >> 57
    (inst orpd t0 t1)          ; t0 = t0 << 7 | t0 >> 57 = rotl(t0, 7)

    ;; t0 = t0 * 9 = t0 << 3 + t0
    ;;    = rotl(s0*5, 7) * 9
    (inst movapd t1 t0)                 ; t1 = t0
    (inst psllq t1 3)                   ; t1 = t0 << 3
    (inst paddq t0 t1)                  ; t0 = t0 << 3 + t0 = 9*t0

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
    (inst xorpd s1 s0)                  ; s1 = s1 ^ s0

    ;; s0 can now be reused as a temp.
    ;; s0 = rotl(s0, 24)
    (inst movapd t0 s0)                 ; t0 = s0
    (inst psllq t0 24)                  ; t0 = s0 << 24
    (inst psrlq s0 (- 64 24))           ; s0 = s0 >> 40
    (inst orpd s0 t0)                   ; s0 = s0 | t0 = rotl(s0, 24)

    ;; s0 = s0 ^ s1 = rotl(s0, 24) ^ s1
    (inst xorpd s0 s1)

    ;; s0 = s0 ^ (s1 << 16)
    (inst movapd t0 s1)          ; t0 = s1
    (inst psllq t0 16)           ; t0 = s1 << 16
    (inst xorpd s0 t0)           ; s0 = rotl(s0, 24) ^ s1 ^ (s1 << 16)

    ;; Save s0 to state[0]
    (inst movsd (make-ea :dword :base state
			 :disp (- (+ (* vm:vector-data-offset
					vm:word-bytes)
				     (* 8 0))
				  vm:other-pointer-type))
          s0)

    ;; s1 = rotl(s1, 37)
    (inst movapd t0 s1)                 ; t0 = s1
    (inst psllq t0 37)                  ; t0 = s1 << 37
    (inst psrlq s1 (- 64 37))           ; s1 = s1 >> 27
    (inst orpd s1 t0)                   ; s1 = t0 | s1 = rotl(s1, 37)

    ;; Save s1 to state[1]
    (inst movsd (make-ea :dword :base state
			 :disp (- (+ (* vm:vector-data-offset
					vm:word-bytes)
				     (* 8 1))
				  vm:other-pointer-type))
          s1)))
)

#+random-xoroshiro
(progn
(defknown kernel::random-xoroshiro-update ((simple-array double-float (2)))
  (values (unsigned-byte 32) (unsigned-byte 32))
  (movable))


(define-vop (random-xoroshiro-update)
  (:policy :fast-safe)
  (:translate kernel::random-xoroshiro-update)
  (:args (state :scs (descriptor-reg) :target state-arg))
  (:arg-types simple-array-double-float)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:temporary (:sc descriptor-reg :offset eax-offset) state-arg)
  (:temporary (:sc double-reg :offset xmm0-offset) s0)
  (:temporary (:sc double-reg :offset xmm1-offset) s1)
  (:temporary (:sc double-reg :offset xmm2-offset) t0)
  (:temporary (:sc double-reg :offset xmm3-offset) t1)
  (:temporary (:sc unsigned-reg :offset edx-offset :target hi) r1)
  (:temporary (:sc unsigned-reg :offset ebx-offset :target lo) r0)
  (:generator 50
    (move state-arg state)
    (move s0 s0)
    (move s1 s1)
    (move t0 t0)
    (move t1 t1)
    (inst call (make-fixup 'vm::xoroshiro-update :assembly-routine))
    (move hi r1)
    (move lo r0)))
)
