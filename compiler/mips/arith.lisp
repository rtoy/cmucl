;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/arith.lisp,v 1.26 1990/06/17 22:24:38 wlott Exp $
;;;
;;;    This file contains the VM definition arithmetic VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "C")



;;;; Unary operations.

(define-vop (fixnum-unop)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num)
  (:policy :fast-safe))

(define-vop (signed-unop)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num)
  (:policy :fast-safe))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst subu res zero-tn x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst subu res zero-tn x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0))
	      temp)
  (:translate lognot)
  (:generator 2
    (inst li temp (fixnum -1))
    (inst xor res x temp)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst nor res x zero-tn)))



;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop)
  (:args (x :target r :scs (any-reg))
	 (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-unsigned-binop)
  (:args (x :target r :scs (unsigned-reg))
	 (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-signed-binop)
  (:args (x :target r :scs (signed-reg))
	 (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(defmacro define-binop (translate cost op &optional unsigned)
  `(progn
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-"
					(string translate)
					"/FIXNUM=>FIXNUM"))
		  fast-fixnum-binop)
       (:args (x :target r
		 :scs (any-reg))
	      (y :target r
		 :scs (any-reg immediate zero
			       ,(if unsigned
				    'unsigned-immediate
				    'negative-immediate))))
       (:translate ,translate)
       (:generator ,cost
	 (inst ,op r x
	       (sc-case y
		 (any-reg y)
		 (zero zero-tn)
		 ((immediate
		   ,(if unsigned 'unsigned-immediate 'negative-immediate))
		  (fixnum (tn-value y)))))))
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-"
					(string translate)
					"/SIGNED=>SIGNED"))
		  fast-signed-binop)
       (:args (x :target r
		 :scs (signed-reg))
	      (y :target r
		 :scs (signed-reg immediate zero
				  ,(if unsigned
				       'unsigned-immediate
				       'negative-immediate))))
       (:translate ,translate)
       (:generator ,(1+ cost)
	 (inst ,op r x
	       (sc-case y
		 (signed-reg y)
		 (zero zero-tn)
		 ((immediate
		   ,(if unsigned 'unsigned-immediate 'negative-immediate))
		  (tn-value y))))))
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-"
					(string translate)
					"/UNSIGNED=>UNSIGNED"))
		  fast-unsigned-binop)
       (:args (x :target r
		 :scs (unsigned-reg))
	      (y :target r
		 :scs (unsigned-reg immediate zero
				    ,(if unsigned
					 'unsigned-immediate
					 'negative-immediate))))
       (:translate ,translate)
       (:generator ,(1+ cost)
	 (inst ,op r x
	       (sc-case y
		 (unsigned-reg y)
		 (zero zero-tn)
		 ((immediate
		   ,(if unsigned 'unsigned-immediate 'negative-immediate))
		  (tn-value y))))))))

(define-binop + 2 addu)
(define-binop - 2 subu)
(define-binop logior 1 or t)
(define-binop logand 1 and t)
(define-binop logxor 1 xor t)

;;; Special case fixnum + and - that don't check for overflow.  Useful when we
;;; know the output type is a fixnum.

(define-vop (fast-+/fixnum fast-+/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 1
    (inst add r x
	  (sc-case y
	    (any-reg y)
	    (zero zero-tn)
	    ((immediate negative-immediate)
	     (fixnum (tn-value y)))))))

(define-vop (fast--/fixnum fast--/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 1
    (inst sub r x
	  (sc-case y
	    (any-reg y)
	    (zero zero-tn)
	    ((immediate negative-immediate)
	     (fixnum (tn-value y)))))))


;;; Shifting

(define-vop (fast-ash)
  (:note "inline ASH")
  (:args (number :scs (signed-reg unsigned-reg))
	 (amount :scs (signed-reg immediate negative-immediate)))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (signed-reg unsigned-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:scs (non-descriptor-reg) :type random :to (:result 0))
	      ndesc)
  (:temporary (:scs (non-descriptor-reg) :type random :from (:argument 1))
	      foo)
  (:generator 3
    (sc-case amount
      (signed-reg
       (let ((positive (gen-label))
	     (done (gen-label)))
	 (inst bgez amount positive)
	 (inst subu ndesc zero-tn amount)
	 (inst and foo ndesc #x1f)
	 (inst beq foo ndesc done)
	 (inst sra result number ndesc)
	 (inst b done)
	 (inst sra result number 31)

	 (emit-label positive)
	 ;; The result-type assures us that this shift will not overflow.
	 (inst sll result number amount)

	 (emit-label done)))

      ((immediate negative-immediate)
       (let ((amount (tn-value amount)))
	 (if (minusp amount)
	     (sc-case result
	       (unsigned-reg
		(inst srl result number (- amount)))
	       (t
		(inst sra result number (- amount))))
	     (inst sll result number amount)))))))



(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
	  (test (gen-label)))
      (move shift arg)
      (inst bgez shift test)
      (move res zero-tn)
      (inst b test)
      (inst nor shift shift)

      (emit-label loop)
      (inst add res (fixnum 1))
      
      (emit-label test)
      (inst bne shift loop)
      (inst srl shift 1))))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target shift))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift temp)
  (:generator 30
    (let ((loop (gen-label))
	  (done (gen-label)))
      (move shift arg)
      (inst beq shift done)
      (move res zero-tn)
      (inst and temp shift 1)

      (emit-label loop)
      (inst sll temp 2)
      (inst add res temp)
      (inst srl shift 1)
      (inst bne shift loop)
      (inst and temp shift 1)

      (emit-label done))))
      

;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:translate *)
  (:generator 4
    (inst sra temp y 2)
    (inst mult x temp)
    (inst mflo r)))

#|
(define-vop (fast-*/fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:translate *)
  (:result-types *)
  (:generator 12
    (inst sra temp y 2)
    (inst mult x temp)
    (inst mfhi temp)
    (
    (inst mflo r)))
|#

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mult x y)
    (inst mflo r)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst multu x y)
    (inst mflo r)))





(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:args (x :target r :scs (signed-reg))
	 (y :target r :scs (signed-reg)))
  (:results (q :scs (signed-reg))
	    (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:generator 11
    (let ((zero (generate-error-code division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst div x y)
    (inst mflo q)
    (inst mfhi r)))

(define-vop (fast-rem/signed fast-signed-binop)
  (:translate rem)
  (:args (x :target r :scs (signed-reg))
	 (y :target r :scs (signed-reg)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 10
    (let ((zero (generate-error-code division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst div x y)
    (inst mfhi r)))




;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

#+nil
(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

#+nil
(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:arg-types tagged-num (:constant (signed-byte 16)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

#+nil
(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:arg-types tagged-num (:constant (unsigned-byte 15)))
  (:info target not-p y))


(defmacro define-conditional-vop (translate &rest generator)
  `(progn
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-IF-"
					(string translate)
					"/FIXNUM"))
		  fast-conditional/fixnum)
       (:translate ,translate)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
       (:generator 4
	 (let ((signed t))
	   ,@generator)))
     #+nil
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-IF-"
					(string translate)
					"-C/FIXNUM"))
		  fast-conditional-c/fixnum)
       (:translate ,translate)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
       (:generator 4
	 (let ((signed t)
	       (y (fixnum y)))
	   ,@generator)))
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-IF-"
					(string translate)
					"/SIGNED"))
		  fast-conditional/signed)
       (:translate ,translate)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
       (:generator 5
	 (let ((signed t))
	   ,@generator)))
     #+nil
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-IF-"
					(string translate)
					"-C/SIGNED"))
		  fast-conditional-c/signed)
       (:translate ,translate)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
       (:generator 5
	 (let ((signed t))
	   ,@generator)))
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-IF-"
					(string translate)
					"/UNSIGNED"))
		  fast-conditional/unsigned)
       (:translate ,translate)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
       (:generator 5
	 (let ((signed nil))
	   ,@generator)))
     #+nil
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-IF-"
					(string translate)
					"-C/UNSIGNED"))
		  fast-conditional-c/unsigned)
       (:translate ,translate)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
       (:generator 5
	 (let ((signed nil))
	   ,@generator)))))

(define-conditional-vop <
  (cond ((and signed (eql y 0))
	 (if not-p
	     (inst bgez x target)
	     (inst bltz x target)))
	(t
	 (if signed
	     (inst slt temp x y)
	     (inst sltu temp x y))
	 (if not-p
	     (inst beq temp zero-tn target)
	     (inst bne temp zero-tn target))))
  (inst nop))

(define-conditional-vop >
  (cond ((and signed (eql y 0))
	 (if not-p
	     (inst blez x target)
	     (inst bgtz x target)))
	(t
	 (if signed
	     (inst slt temp y x)
	     (inst sltu temp y x))
	 (if not-p
	     (inst beq temp zero-tn target)
	     (inst bne temp zero-tn target))))
  (inst nop))

(define-conditional-vop =
  (declare (ignore signed))
  (when (integerp y)
    (inst li temp y)
    (setf y temp))
  (if not-p
      (inst bne x y target)
      (inst beq x y target))
  (inst nop))





;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (next :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg)))
  (:policy :fast-safe)
  (:generator 4
    (let ((done (gen-label)))
      (inst beq shift done)
      (inst srl res next shift)
      (inst subu temp zero-tn shift)
      (inst sll temp prev temp)
      (inst or res res temp)
      (emit-label done)
      (move result res))))


(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:results (r :scs (unsigned-reg)))
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg)))
  (:generator 1
    (inst nor r x zero-tn)))

(define-vop (32bit-logical-nor 32bit-logical)
  (:translate 32bit-logical-nor)
  (:generator 1
    (inst nor r x y)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (inst and r x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (inst or r x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (inst xor r x y)))



;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-vop (bignum-ref word-index-ref)
  (:variant vm:bignum-digits-offset vm:other-pointer-type)
  (:translate bignum::%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word-index-set)
  (:variant vm:bignum-digits-offset vm:other-pointer-type)
  (:translate bignum::%bignum-set)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg immediate zero unsigned-immediate))
	 (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (let ((done (gen-label)))
      (inst bltz digit done)
      (move result null-tn)
      (load-symbol result 't)
      (emit-label done))))

(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
	    (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let ((carry-in (gen-label))
	  (done (gen-label)))
      (inst bne c carry-in)
      (inst addu res a b)

      (inst b done)
      (inst sltu carry res b)

      (emit-label carry-in)
      (inst addu res 1)
      (inst nor temp a zero-tn)
      (inst sltu carry b temp)
      (inst xor carry 1)

      (emit-label done)
      (move result res))))

(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
	    (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (temp :scs (non-descriptor-reg)))
  (:generator 4
    (let ((no-borrow-in (gen-label))
	  (done (gen-label)))

      (inst bne c no-borrow-in)
      (inst subu res a b)

      (inst subu res 1)
      (inst b done)
      (inst sltu borrow b a)

      (emit-label no-borrow-in)
      (inst sltu borrow a b)
      (inst xor borrow 1)

      (emit-label done)
      (move result res))))

(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 3
    (inst multu x y)
    (inst mflo lo)
    (inst mfhi hi)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst nor r x zero-tn)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra digit fixnum 2)))

(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target rem)
	 (b :scs (unsigned-reg))
	 (c :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg)) temp result)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 230
    (flet ((maybe-subtract (&optional (guess temp))
	     (inst subu temp guess 1)
	     (inst and temp c)
	     (inst subu a temp)))
      (inst sltu result a c)
      (maybe-subtract result)
      (dotimes (i 32)
	(inst sll a 1)
	(inst srl temp b 31)
	(inst or a temp)
	(inst sll b 1)
	(inst sltu temp a c)
	(inst sll result 1)
	(inst or result temp)
	(maybe-subtract)))
    (move rem a)
    (inst nor quo result zero-tn)))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst sll res digit 2))
      (signed-reg
       (move res digit)))))

(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra result digit count)))

(define-vop (digit-ashl)
  (:translate bignum::%ashl)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sll result digit count)))



;;;; Static functions.

(define-static-function two-arg-gcd (x y) :translate gcd)
(define-static-function two-arg-lcm (x y) :translate lcm)

(define-static-function two-arg-+ (x y) :translate +)
(define-static-function two-arg-- (x y) :translate -)
(define-static-function two-arg-* (x y) :translate *)
(define-static-function two-arg-/ (x y) :translate /)

(define-static-function two-arg-< (x y) :translate <)
(define-static-function two-arg-<= (x y) :translate <=)
(define-static-function two-arg-> (x y) :translate >)
(define-static-function two-arg->= (x y) :translate >=)
(define-static-function two-arg-= (x y) :translate =)
(define-static-function two-arg-/= (x y) :translate /=)

(define-static-function %negate (x) :translate %negate)

(define-static-function two-arg-and (x y) :translate logand)
(define-static-function two-arg-ior (x y) :translate logior)
(define-static-function two-arg-xor (x y) :translate logxor)

