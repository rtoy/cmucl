;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/arith.lisp,v 1.42 1991/02/20 14:43:08 ram Exp $
;;;
;;;    This file contains the VM definition arithmetic VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "MIPS")



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
  (:args (number :scs (signed-reg unsigned-reg) :to :save)
	 (amount :scs (signed-reg immediate negative-immediate)))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (signed-reg unsigned-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:temporary (:sc non-descriptor-reg :to :eval) temp)
  (:generator 3
    (sc-case amount
      (signed-reg
       (let ((positive (gen-label))
	     (done (gen-label)))
	 (inst bgez amount positive)
	 (inst subu ndesc zero-tn amount)
	 (inst slt temp ndesc 31)
	 (inst bne temp zero-tn done)
	 (sc-case number
	   (signed-reg (inst sra result number ndesc))
	   (unsigned-reg (inst srl result number ndesc)))
	 (inst b done)
	 (sc-case number
	   (signed-reg (inst sra result number 31))
	   (unsigned-reg (inst srl result number 31)))

	 (emit-label positive)
	 ;; The result-type assures us that this shift will not overflow.
	 (inst sll result number amount)

	 (emit-label done)))

      ((immediate negative-immediate)
       (let ((amount (tn-value amount)))
	 (if (minusp amount)
	     (sc-case number
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
  (:result-types positive-fixnum)
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
  (:result-types positive-fixnum)
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



(define-vop (fast-truncate/fixnum fast-fixnum-binop)
  (:translate truncate)
  (:results (q :scs (any-reg))
	    (r :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:temporary (:scs (non-descriptor-reg) :to :eval) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 11
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst div x y)
    (inst mflo temp)
    (inst sll q temp 2)
    (inst mfhi r)))

(define-vop (fast-truncate/unsigned fast-unsigned-binop)
  (:translate truncate)
  (:results (q :scs (unsigned-reg))
	    (r :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst divu x y)
    (inst mflo q)
    (inst mfhi r)))

(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:results (q :scs (signed-reg))
	    (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst div x y)
    (inst mflo q)
    (inst mfhi r)))






;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:policy :fast-safe))

(deftype integer-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 1))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(- bound bite 1))))
	(t
	 (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (integer-with-a-bite-out 14 #.(fixnum 1))))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (integer-with-a-bite-out 16 1)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (and (integer-with-a-bite-out 16 1)
					   unsigned-byte)))
  (:info target not-p y))


(defmacro define-conditional-vop (translate &rest generator)
  ;;
  ;; Squelch dead-code notes...
  `(;locally (declare (optimize (inhibit-warnings 3)))
    progn ; but not really, since that makes a big function, which doesn't load.
     ,@(mapcar #'(lambda (suffix cost signed)
		   (unless (and (member suffix '(/fixnum -c/fixnum))
				(eq translate 'eql))
		     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
						    translate suffix))
				   ,(intern
				     (format nil "~:@(FAST-CONDITIONAL~A~)"
					     suffix)))
			(:translate ,translate)
			(:generator ,cost
			  (let* ((signed ,signed)
				 (-c/fixnum ,(eq suffix '-c/fixnum))
				 (y (if -c/fixnum (fixnum y) y)))
			    ,@generator)))))
	       '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
	       '(3 2 5 4 5 4)
	       '(t t t t nil nil))))

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
	((integerp y)
	 (let ((y (+ y (if -c/fixnum (fixnum 1) 1))))
	   (if signed
	       (inst slt temp x y)
	       (inst sltu temp x y))
	   (if not-p
	       (inst bne temp zero-tn target)
	       (inst beq temp zero-tn target))))
	(t
	 (if signed
	     (inst slt temp y x)
	     (inst sltu temp y x))
	 (if not-p
	     (inst beq temp zero-tn target)
	     (inst bne temp zero-tn target))))
  (inst nop))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

(define-conditional-vop eql
  (declare (ignore signed))
  (when (integerp y)
    (inst li temp y)
    (setf y temp))
  (if not-p
      (inst bne x y target)
      (inst beq x y target))
  (inst nop))

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;
(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:ignore temp)
  (:generator 3
    (if not-p
	(inst bne x y target)
	(inst beq x y target))
    (inst nop)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (let ((y (cond ((eql y 0) zero-tn)
		   (t
		    (inst li temp (fixnum y))
		    temp))))
      (if not-p
	  (inst bne x y target)
	  (inst beq x y target))
      (inst nop))))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:arg-types * (:constant (signed-byte 14)))
  (:variant-cost 6))
  

;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
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
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
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
	 (index :scs (any-reg immediate zero negative-immediate))
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

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) temp)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 6
    (inst multu x y)
    (inst mflo temp)
    (inst addu lo temp carry-in)
    (inst sltu temp lo carry-in)
    (inst mfhi hi)
    (inst addu hi temp)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 9
    (inst multu x y)
    (inst addu lo prev carry-in)
    (inst sltu temp lo carry-in)
    (inst mfhi hi)
    (inst addu hi temp)
    (inst mflo temp)
    (inst addu lo temp)
    (inst sltu temp lo temp)
    (inst addu hi temp)))

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
  (:args (num-high :scs (unsigned-reg) :target rem)
	 (num-low :scs (unsigned-reg) :target rem-low)
	 (denom :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) rem-low)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:results (quo :scs (unsigned-reg) :from (:eval 0))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 325 ; number of inst assuming targeting works.
    (move rem num-high)
    (move rem-low num-low)
    (flet ((maybe-subtract (&optional (guess temp))
	     (inst subu temp guess 1)
	     (inst and temp denom)
	     (inst subu rem temp)))
      (inst sltu quo rem denom)
      (maybe-subtract quo)
      (dotimes (i 32)
	(inst sll rem 1)
	(inst srl temp rem-low 31)
	(inst or rem temp)
	(inst sll rem-low 1)
	(inst sltu temp rem denom)
	(inst sll quo 1)
	(inst or quo temp)
	(maybe-subtract)))
    (inst nor quo zero-tn)))

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

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
    (inst srl result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
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
