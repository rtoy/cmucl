;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/arith.lisp,v 1.12 1999/06/19 16:01:02 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition arithmetic VOPs for the SPARC.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "SPARC")



;;;; Unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))


(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (inst xor res x (fixnum -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst not res x)))



;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero))
	 (y :target r :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero))
	 (y :target r :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero))
	 (y :target r :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))


(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (signed-byte 11) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))


(eval-when (compile load eval)

(defmacro define-binop (translate untagged-penalty op)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
		  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
	 (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
		  fast-fixnum-binop-c)
       (:translate ,translate)
       (:generator 1
	 (inst ,op r x (fixnum y))))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
		  fast-signed-binop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (inst ,op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (inst ,op r x y)))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
		  fast-unsigned-binop-c)
       (:translate ,translate)
       (:generator ,untagged-penalty
	 (inst ,op r x y)))))

); eval-when

(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logandc2 2 andn)
(define-binop logior 2 or)
(define-binop logorc2 2 orn)
(define-binop logxor 2 xor)
(define-binop logeqv 2 xnor)

;;; Special case fixnum + and - that trap on overflow.  Useful when we
;;; don't know that the output type is a fixnum.

(define-vop (+/fixnum fast-+/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 4
    (inst taddcctv r x y)))

(define-vop (+-c/fixnum fast-+-c/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 3
    (inst taddcctv r x (fixnum y))))

(define-vop (-/fixnum fast--/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 4
    (inst tsubcctv r x y)))

(define-vop (--c/fixnum fast---c/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 3
    (inst tsubcctv r x (fixnum y))))

;;; Shifting

(define-vop (fast-ash)
  (:note "inline ASH")
  (:args (number :scs (signed-reg unsigned-reg) :to :save)
	 (amount :scs (signed-reg immediate)))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (signed-reg unsigned-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 3
    (sc-case amount
      (signed-reg
       (let ((positive (gen-label))
	     (done (gen-label)))
	 (inst cmp amount)
	 (inst b :ge positive)
	 (inst neg ndesc amount)
	 (inst cmp ndesc 31)
	 (inst b :le done)
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

      (immediate
       (let ((amount (tn-value amount)))
	 (if (minusp amount)
	     (let ((amount (min 31 (- amount))))
	       (sc-case number
		 (unsigned-reg
		  (inst srl result number amount))
		 (signed-reg
		  (inst sra result number amount))))
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
      (inst addcc shift zero-tn arg)
      (inst b :ge test)
      (move res zero-tn)
      (inst b test)
      (inst not shift)

      (emit-label loop)
      (inst add res (fixnum 1))
      
      (emit-label test)
      (inst cmp shift)
      (inst b :ne loop)
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
  (:guard (not (backend-featurep :sparc-v9)))
  (:generator 30
    (let ((loop (gen-label))
	  (done (gen-label)))
      (inst addcc shift zero-tn arg)
      (inst b :eq done)
      (move res zero-tn)

      (emit-label loop)
      (inst sub temp shift 1)
      (inst andcc shift temp)
      (inst b :ne loop)
      (inst add res (fixnum 1))

      (emit-label done))))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:guard (backend-featurep :sparc-v9))
  (:generator 2
    ;; Clear the upper 32-bits, just in case.  Is this really needed?
    (inst srl res res 0)
    (inst popc res arg)))


;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:guard (backend-featurep :sparc-v8))
  (:generator 4
    (inst sra temp y 2)
    (inst smul r x temp)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:guard (backend-featurep :sparc-v8))
  (:generator 3
    (inst smul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:guard (backend-featurep :sparc-v8))
  (:generator 3
    (inst umul r x y)))


;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(deftype integer-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 1))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(- bound bite 1))))
	(t
	 (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
	 (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 11)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg zero))
	 (y :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg zero)))
  (:arg-types signed-num (:constant (signed-byte 13)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg zero))
	 (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num (:constant (unsigned-byte 12)))
  (:info target not-p y))


(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed)
		   (unless (and (member suffix '(/fixnum -c/fixnum))
				(eq tran 'eql))
		     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
						    tran suffix))
				   ,(intern
				     (format nil "~:@(FAST-CONDITIONAL~A~)"
					     suffix)))
			(:translate ,tran)
			(:generator ,cost
			  (inst cmp x
				,(if (eq suffix '-c/fixnum) '(fixnum y) 'y))
			  (inst b (if not-p
				      ,(if signed not-cond not-unsigned)
				      ,(if signed cond unsigned))
				target)
			  (inst nop)))))
	       '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
	       '(4 3 6 5 6 5)
	       '(t t t t nil nil))))

(define-conditional-vop < :lt :ltu :ge :geu)

(define-conditional-vop > :gt :gtu :le :leu)

(define-conditional-vop eql :eq :eq :ne :ne)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg zero))
	 (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte 11)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmp x (fixnum y))
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:arg-types * (:constant (signed-byte 11)))
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
      (inst cmp shift)
      (inst b :eq done)
      (inst srl res next shift)
      (inst sub temp zero-tn shift)
      (inst sll temp prev temp)
      (inst or res temp)
      (emit-label done)
      (move result res))))


(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg zero))
	 (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num)
  (:generator 1
    (inst not r x)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (inst and r x y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (inst or r x y)))

(deftransform 32bit-logical-nor ((x y) (* *))
  '(32bit-logical-not (32bit-logical-or x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (inst xor r x y)))

(define-vop (32bit-logical-eqv 32bit-logical)
  (:translate 32bit-logical-eqv)
  (:generator 1
    (inst xnor r x y)))

(define-vop (32bit-logical-orc2 32bit-logical)
  (:translate 32bit-logical-orc2)
  (:generator 1
    (inst orn r x y)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-orc2 y x))

(define-vop (32bit-logical-andc2 32bit-logical)
  (:translate 32bit-logical-andc2)
  (:generator 1
    (inst andn r x y)))

(deftransform 32bit-logical-andc1 ((x y) (* *))
  '(32bit-logical-andc2 y x))


(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
	 (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "shift-towards-start")
  (:generator 1
    (inst sll r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "shift-towards-end")
  (:generator 1
    (inst srl r num amount)))




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
	 (index :scs (any-reg immediate zero))
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
      (inst cmp digit)
      (inst b :lt done)
      (move result null-tn)
      (load-symbol result t)
      (emit-label done))))

(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
	    (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst addcc zero-tn c -1)
    (inst addxcc result a b)
    (inst addx carry zero-tn zero-tn)))

(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
	    (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst subcc zero-tn c 1)
    (inst subxcc result a b)
    (inst addx borrow zero-tn zero-tn)
    (inst xor borrow 1)))

;;; EMIT-MULTIPLY -- This is used both for bignum stuff and in assembly
;;; routines.
;;; 
(defun emit-multiply (multiplier multiplicand result-high result-low)
  "Emit code to multiply MULTIPLIER with MULTIPLICAND, putting the result
  in RESULT-HIGH and RESULT-LOW.  KIND is either :signed or :unsigned.
  Note: the lifetimes of MULTIPLICAND and RESULT-HIGH overlap."
  (declare (type tn multiplier result-high result-low)
	   (type (or tn (signed-byte 13)) multiplicand))
  (cond #+nil
	((backend-featurep :sparc-v9)
	 ;; Take advantage of V9's 64-bit multiplier.  It seems that
	 ;; emit-multiply is only used to do an unsigned multiply, so
	 ;; the code only does an unsigned multiply.
	 ;;
	 ;; Clear out the high bits of the multiplier and
	 ;; multiplicand.  Multiply the two numbers and put the result
	 ;; in result-high.  Copy the low 32-bits to result-low.  Then
	 ;; shift result-high so the high 32-bits end up in the low
	 ;; 32-bits.
	 (inst srl multiplier multiplier 0)
	 (unless (numberp multiplicand)
	   (inst srl multiplicand multiplicand 0))
	 (inst mulx result-high multiplier multiplicand)
	 (inst move result-low result-high)
	 (inst srax result-high 32))
	((backend-featurep :sparc-v8)
	 ;; V8 has a multiply instruction.  Same restrictions as
	 ;; :sparc-v9.  This should also work for the V9, but umul and
	 ;; the Y register is deprecated on the V9.
	 (inst umul result-low multiplier multiplicand)
	 (inst rdy result-high))
	(t
	 (let ((label (gen-label)))
	   (inst wry multiplier)
	   (inst andcc result-high zero-tn)
	   ;; Note: we can't use the Y register until three insts
	   ;; after it's written.
	   (inst nop)
	   (inst nop)
	   (dotimes (i 32)
	     (inst mulscc result-high multiplicand))
	   (inst mulscc result-high zero-tn)
	   (inst cmp multiplicand)
	   (inst b :ge label)
	   (inst nop)
	   (inst add result-high multiplier)
	   (emit-label label)
	   (inst rdy result-low)))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
	 (y :scs (unsigned-reg) :to (:eval 1))
	 (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
	    (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)
    (inst addcc lo carry-in)
    (inst addx hi zero-tn)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
	 (y :scs (unsigned-reg) :to (:eval 1))
	 (prev :scs (unsigned-reg) :to (:eval 2))
	 (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
	    (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)
    (inst addcc lo carry-in)
    (inst addx hi zero-tn)
    (inst addcc lo prev)
    (inst addx hi zero-tn)))

(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:result 1))
	 (y :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst not r x)))

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
  (:args (div-high :scs (unsigned-reg) :target rem)
	 (div-low :scs (unsigned-reg) :target quo)
	 (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move rem div-high)
    (move quo div-low)
    (dotimes (i 33)
      (let ((label (gen-label)))
	(inst cmp rem divisor)
	(inst b :ltu label)
	(inst addxcc quo quo)
	(inst sub rem divisor)
	(emit-label label)
	(unless (= i 32)
	  (inst addx rem rem))))
    (inst not quo)))

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
