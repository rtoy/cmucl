;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/arith.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition arithmetic VOPs for ARM.
;;;

(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")


;;;; Unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))


(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note _N"inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note _N"inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (emit-not-implemented)
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (emit-not-implemented)
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (emit-not-implemented)
    (inst eor res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (emit-not-implemented)
    (inst mvn res x)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg))
	 (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg))
	 (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
	 (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (signed-byte #.(- 13 vm:fixnum-tag-bits)) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note _N"inline (signed-byte 32) arithmetic"))

(eval-when (compile load eval)

(defmacro define-binop (translate untagged-penalty op
			&optional arg-swap restore-fixnum-mask)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
		   fast-fixnum-binop)
       (:translate ,translate)
       ,@(when restore-fixnum-mask
	       `((:temporary (:sc non-descriptor-reg) temp)))
       (:generator 2
	 (emit-not-implemented)))
     ,@(unless arg-swap
	       `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
			       fast-fixnum-binop-c)
		   (:translate ,translate)
		   ,@(when restore-fixnum-mask
			   `((:temporary (:sc non-descriptor-reg) temp)))
		   (:generator 1
		     (emit-not-implemented)))))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (emit-not-implemented)
	 ,(if arg-swap
	      `(inst ,op r y x)
	      `(inst ,op r x y))))
     ,@(unless arg-swap
	       `((define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
			       fast-signed-binop-c)
		   (:translate ,translate)
		   (:generator ,untagged-penalty
		     (emit-not-implemented)))))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 (emit-not-implemented)))
     ,@(unless arg-swap
	       `((define-vop (,(symbolicate "FAST-" translate "-C/UNSIGNED=>UNSIGNED")
			       fast-unsigned-binop-c)
		   (:translate ,translate)
		   (:generator ,untagged-penalty
		     (emit-not-implemented)))))))

); eval-when

(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logandc1 2 bic t)
(define-binop logandc2 2 bic)
(define-binop logior 2 orr)
(define-binop logxor 2 eor)

;;; Special logand cases: (logand signed unsigned) => unsigned

(define-vop (fast-logand/signed-unsigned=>unsigned
	     fast-logand/unsigned=>unsigned)
    (:args (x :scs (signed-reg))
	   (y :target r :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand/unsigned-signed=>unsigned
	     fast-logand/unsigned=>unsigned)
    (:args (x :target r :scs (unsigned-reg))
	   (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num))

;; This vop is intended to handle the case (logand x #xffffffff) when
;; x is a (signed-byte 32).  We can just do a register move instead of
;; and'ing with #xffffffff.  Some other special cases are handled too
;; when the constant can fit inside the immediate operaand of the and
;; instruction.
(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-unsigned-binop-c)
  (:args (x :scs (signed-reg)))
  (:translate logand)
  (:arg-types signed-num
	      (:constant (or (and (unsigned-byte 12) (not (integer 0 0)))
			     (integer #xfffff000 #xffffffff))))
  (:generator 2				; Needs to be low to give this vop a chance.
    (emit-not-implemented)))

(define-vop (fast-abs/signed fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate abs)
  (:note _N"inline 32-bit abs")
  (:generator 1
    (emit-not-implemented)))

;;; Truncate

(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg))
	    (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note _N"inline (signed-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (emit-not-implemented)))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 8
    (emit-not-implemented)))

;;; Shifting

(define-vop (fast-ash/signed=>signed)
  (:note _N"inline (signed-byte 32) ASH")
  (:args (number :scs (signed-reg) :to :save)
	 (amount :scs (signed-reg immediate) :to :save))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 5
    (emit-not-implemented)))

(define-vop (fast-ash/unsigned=>unsigned)
  (:note _N"inline (unsigned-byte 32) ASH")
  (:args (number :scs (unsigned-reg) :to :save)
	 (amount :scs (signed-reg immediate) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 5
    (emit-not-implemented)))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:note _N"inline constant ASH")
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 4
    (emit-not-implemented)))

;; Some special cases where we know we want a left shift.  Just do the
;; shift, instead of checking for the sign of the shift.
(macrolet
    ((frob (name sc-type type result-type cost)
       `(define-vop (,name)
	 (:note _N"inline ASH")
	 (:translate ash)
	 (:args (number :scs (,sc-type))
	        (amount :scs (signed-reg unsigned-reg immediate)))
	 (:arg-types ,type positive-fixnum)
	 (:results (result :scs (,result-type)))
	 (:result-types ,type)
	 (:policy :fast-safe)
	 (:generator ,cost
	   (emit-not-implemented)))))
  (frob fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (frob fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (frob fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

;; Constant left shift
(macrolet
    ((frob (name sc-type type result-type cost)
       `(define-vop (,name)
	  (:note _N"inline ASH")
	  (:translate ash)
	  (:args (number :scs (,sc-type)))
	  (:info amount)
	  (:arg-types ,type
		      (:constant (integer 0 31)))
	  (:results (result :scs (,result-type)))
	  (:result-types ,type)
	  (:policy :fast-safe)
	  (:generator ,cost
	    ;; The result-type assures us that this shift will not
	    ;; overflow. And for fixnum's, the zero bits that get
	    ;; shifted in are just fine for the fixnum tag.
	    (emit-not-implemented)))))
  (frob fast-ash-left-c/signed=>signed signed-reg signed-num signed-reg 3)
  (frob fast-ash-left-c/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (frob fast-ash-left-c/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

    
(defknown ash-right-signed ((signed-byte #.vm:word-bits)
			    (and fixnum unsigned-byte))
  (signed-byte #.vm:word-bits)
  (movable foldable flushable))

(defknown ash-right-unsigned ((unsigned-byte #.vm:word-bits)
			      (and fixnum unsigned-byte))
  (unsigned-byte #.vm:word-bits)
  (movable foldable flushable))

;; Some special cases where we want a right shift.  Just do the shift.
;; (Needs appropriate deftransforms to call these, though.)

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
	 (:generator ,cost
	   (emit-not-implemented)))))
  (frob ash-right-signed fast-ash-right/signed=>signed
	signed-reg signed-num sra 3)
  (frob ash-right-unsigned fast-ash-right/unsigned=>unsigned
	unsigned-reg unsigned-num srl 3)
  )

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
	 (:generator ,cost
	   (emit-not-implemented)))))
  (frob ash-right-signed fast-ash-right-c/signed=>signed
	signed-reg signed-num sra 1 31)
  (frob ash-right-unsigned fast-ash-right-c/unsigned=>unsigned
	unsigned-reg unsigned-num srl 1 31)
  )

(define-vop (fast-ash-right/fixnum=>fixnum)
    (:note _N"inline right ASH")
  (:translate ash-right-signed)
  (:args (number :scs (any-reg))
	 (amount :scs (signed-reg unsigned-reg immediate)))
  (:arg-types tagged-num positive-fixnum)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 2
    ;; Shift the fixnum right by the desired amount.  Then zap out the
    ;; 2 LSBs to make it a fixnum again.  (Those bits are junk.)
    (emit-not-implemented)))
    



(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note _N"inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 30
    (emit-not-implemented)))

(define-vop (unsigned-byte-32-len)
  (:translate integer-length)
  (:note _N"inline (unsigned-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 30
    (emit-not-implemented)))


(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note _N"inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 35
    (emit-not-implemented)))


;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:translate *)
  (:generator 2
    ;; The cost here should be less than the cost for
    ;; */signed=>signed.  Why?  A fixnum product using signed=>signed
    ;; has to convert both args to signed-nums.  But using this, we
    ;; don't have to and that saves an instruction.
    (emit-not-implemented)))

;; Multiplication by a constant.
(define-vop (fast-*-c/unsigned=>unsigned fast-unsigned-binop-c)
  (:translate *)
  (:generator 2
    (emit-not-implemented)))

(define-vop (fast-*-c/signed=>signed fast-signed-binop-c)
  (:translate *)
  (:generator 2
    (emit-not-implemented)))

(define-vop (fast-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note _N"inline fixnum arithmetic")
  (:translate *)
  (:generator 1
    (emit-not-implemented)))


(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (emit-not-implemented)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (emit-not-implemented)))


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
	 (error (intl:gettext "Bad size specified for SIGNED-BYTE type specifier: ~S.") s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note _N"inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note _N"inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (signed-byte 13)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
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
			  (emit-not-implemented)))))
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
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note _N"inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (emit-not-implemented)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  ;; This is a signed-byte 11 because after fixnum shifting, we get a
  ;; 13-bit number, and that's the largest immediate allowed.
  (:arg-types tagged-num (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (emit-not-implemented)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  ;; This is a signed-byte 11 because after fixnum shifting, we get a
  ;; 13-bit number, and that's the largest immediate allowed.
  (:arg-types * (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:variant-cost 6))


;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 4
    (emit-not-implemented)))


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
    (emit-not-implemented)
    (inst mvn r x)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (emit-not-implemented)
    (inst and r x y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (emit-not-implemented)
    (inst orr r x y)))

(deftransform 32bit-logical-nor ((x y) (* *))
  '(32bit-logical-not (32bit-logical-or x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (emit-not-implemented)
    (inst eor r x y)))

(define-vop (32bit-logical-eqv 32bit-logical)
  (:translate 32bit-logical-eqv)
  (:generator 1
    (emit-not-implemented)))

(define-vop (32bit-logical-orc2 32bit-logical)
  (:translate 32bit-logical-orc2)
  (:generator 1
    (emit-not-implemented)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-orc2 y x))

(define-vop (32bit-logical-andc2 32bit-logical)
  (:translate 32bit-logical-andc2)
  (:generator 1
    (emit-not-implemented)))

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
  (:note _N"shift-towards-start")
  (:generator 1
    (emit-not-implemented)
    (inst lsl r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note _N"shift-towards-end")
  (:generator 1
    (emit-not-implemented)
    (inst lsr r num amount)))




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
	 (index :scs (any-reg immediate))
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
    (emit-not-implemented)))

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
    (emit-not-implemented)))

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
    (emit-not-implemented)))

;;; EMIT-MULTIPLY -- This is used both for bignum stuff and in assembly
;;; routines.
;;; 
(defun emit-multiply (multiplier multiplicand result-high result-low)
  "Emit code to multiply MULTIPLIER with MULTIPLICAND, putting the result
  in RESULT-HIGH and RESULT-LOW.  KIND is either :signed or :unsigned.
  Note: the lifetimes of MULTIPLICAND and RESULT-HIGH overlap."
  (declare (type tn multiplier result-high result-low)
	   (type (or tn (signed-byte 13)) multiplicand))
  ;; It seems that emit-multiply is only used to do an unsigned
  ;; multiply, so the code only does an unsigned multiply.
  (not-implemented emit-multiply))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
	 (y :scs (unsigned-reg) :to (:eval 1))
	 (carry-in :scs (unsigned-reg unsigned-stack) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
	    (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-not-implemented)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
	 (y :scs (unsigned-reg) :to (:eval 1))
	 (prev :scs (unsigned-reg unsigned-stack) :to (:eval 2))
	 (carry-in :scs (unsigned-reg unsigned-stack) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
	    (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-not-implemented)))

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
    (emit-not-implemented)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (emit-not-implemented)
    (inst mvn r x)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (emit-not-implemented)
    (inst asr digit fixnum fixnum-tag-bits)))

(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
	 (div-low :scs (unsigned-reg) :target quo)
	 (divisor :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg) :from (:eval 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (emit-not-implemented)))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (emit-not-implemented)))


(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (signed-reg unsigned-reg immediate)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (emit-not-implemented)))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
    (emit-not-implemented)))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 1
    (emit-not-implemented)))


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


;; Truncation by a constant can be done by multiplication with the
;; appropriate constant.
;;
;; See generic/vm-tran.lisp for the algorithm.

(define-vop (signed-truncate-by-mult)
  (:translate truncate)
  (:args (x :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note _N"inline (signed-byte 32) arithmetic")
  (:generator 6
    (emit-not-implemented)))

(define-vop (unsigned-truncate-by-mult)
  (:translate truncate)
  (:args (x :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note _N"inline (unsigned-byte 32) arithmetic")
  (:generator 6
    (emit-not-implemented)))

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
	       `(arm::ash-right-signed num (- shift))
	       `(arm::ash-right-signed num (min (- shift) #.(1- vm:word-bits)))))
	  ((csubtypep num-type (specifier-type '(unsigned-byte #.vm:word-bits)))
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
	       `(arm::ash-right-unsigned num (- shift))
	       `(if (<= shift #.(- vm:word-bits))
		 0
		 (arm::ash-right-unsigned num (- shift)))))
	  (t
	   (give-up)))))


;;; Arm implementation of modular arithmetic.
(in-package "ARM")
#+modular-arith
(progn
(c::define-modular-fun lognot-mod32 (x) lognot 32)

(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (inst mvn res x)))

(define-vop (lognot-mod32/signed=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (inst mvn res x)))

;; Handle (ldb (byte 32 0) (- x)).  The (- x) gets converted to
;; (%negate x), so we build modular functions for %negate.

(c::define-modular-fun %negate-mod32 (x) kernel:%negate 32)

(define-vop (%negate-mod32/unsigned=>unsigned fast-safe-arith-op)
  (:translate %negate-mod32)
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (emit-not-implemented)
    (inst neg res x)))

(define-vop (%negate-mod32/signed=>unsigned fast-safe-arith-op)
  (:translate %negate-mod32)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (emit-not-implemented)
    (inst neg res x)))

;; Define a bunch of vops for converting signed to unsigned.
(macrolet
    ((frob (op &optional trans)
       (let ((name (symbolicate "FAST-" op "/SIGNED=>UNSIGNED"))
	     (vop (symbolicate "FAST-" op "/SIGNED=>SIGNED"))
	     (trans (symbolicate (or trans op) "-MOD32")))
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
  (frob logandc1)
  (frob logandc2)
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
(define-modular-backend logandc1)
(define-modular-backend logandc2 t)
(define-modular-backend * t *)

(def-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))
(def-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))

(defknown vm::ash-left-mod32 (integer (integer 0))
  (unsigned-byte 32)
  (foldable flushable movable))

(defknown vm::ash-mod32 (integer integer)
  (unsigned-byte 32)
  (foldable flushable movable))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
	     digit-ashl)
  (:translate ash-left-mod32))

(define-vop (fast-ash-mod32/unsigned=>unsigned
	     fast-ash/unsigned=>unsigned)
    (:translate ash-mod32))

)

(in-package :c)

#+modular-arith
(define-modular-fun-optimizer ash ((integer count) :width width)
  (when (<= width 32)
    ;; We can do a modular shift.  If the shift is known to be a left
    ;; shift, we can use the left shift vop to get a left shift
    ;; instruction.  Otherwise, we can use the regular fast-ash vop to
    ;; get the shift.
    (let ((count-type (continuation-type count)))
      (cond ((csubtypep count-type (specifier-type '(unsigned-byte 5)))
	     (cut-to-width integer width)
	     'vm::ash-left-mod32)
	    ((csubtypep count-type (specifier-type '(integer -31 31)))
	     (cut-to-width integer width)
	     'vm::ash-mod32)
	    (t
	     ;; Return NIL to say we can't do anything special.
	     nil)))))

;;; If both arguments and the result are (unsigned-byte 32), try to come up
;;; with a ``better'' multiplication using multiplier recoding.  There are two
;;; different ways the multiplier can be recoded.  The more obvious is to shift
;;; X by the correct amount for each bit set in Y and to sum the results.  But
;;; if there is a string of bits that are all set, you can add X shifted by
;;; one more then the bit position of the first set bit and subtract X shifted
;;; by the bit position of the last set bit.  We can't use this second method
;;; when the high order bit is bit 31 because shifting by 32 doesn't work
;;; too well.
;;;


(defun *-transformer (y)
  (let ((y (continuation-value y)))
    (multiple-value-bind (result adds shifts)
	(strength-reduce-constant-multiply 'x y)
      ;; This is an approximate break-even point.  It's pretty
      ;; rough.
      (when (> (+ adds shifts) 9)
	(give-up))
      (or result 0))))

#+modular-arith
(deftransform * ((x y)
		 ((unsigned-byte 32) (constant-argument (unsigned-byte 32)))
		 (unsigned-byte 32))
  "recode as shifts and adds"
  (*-transformer y))

#+modular-arith
(deftransform vm::*-mod32 ((x y)
		 ((unsigned-byte 32) (constant-argument (unsigned-byte 32)))
		 (unsigned-byte 32))
  "recode as shifts and adds"
  (*-transformer y))
