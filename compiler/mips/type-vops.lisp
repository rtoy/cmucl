;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/type-vops.lisp,v 1.24 1990/12/18 20:49:49 wlott Exp $
;;; 
;;; This file contains the VM definition of type testing and checking VOPs
;;; for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for the MIPS R2000 by Christopher Hoover.
;;;
(in-package "MIPS")


;;;; Simple type checking and testing:
;;;
;;;    These types are represented by a single type code, so are easily
;;; open-coded as non-shifting type test.

(define-vop (check-simple-type)
  (:args
   (value :target result
	  :scs (any-reg descriptor-reg)))
  (:results
   (result :scs (any-reg descriptor-reg)))
  (:temporary (:type random :scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (simple-type-predicate)
  (:args
   (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:variant-vars type-code)
  (:temporary (:type random :scs (non-descriptor-reg)) temp)
  (:generator 4
    (test-simple-type value temp target not-p type-code)))

(macrolet ((frob (pred-name check-name ptype type-code error-code)
	     (let ((cost (if (< (eval type-code) vm:lowtag-limit) 4 9)))
	       `(progn
		  (define-vop (,pred-name simple-type-predicate)
		    (:variant ,type-code)
		    (:variant-cost ,cost)
		    (:translate ,pred-name))
		  (define-vop (,check-name check-simple-type)
		    (:generator ,cost
		      (let ((err-lab
			     (generate-error-code vop ,error-code value)))
			(test-simple-type value temp err-lab t ,type-code)
			(move result value))))
		  (primitive-type-vop ,check-name (:check) ,ptype)))))

  (frob functionp check-function function
    vm:function-pointer-type object-not-function-error)

  (frob listp check-list list
    vm:list-pointer-type object-not-list-error)

  (frob structurep check-structure structure
    vm:structure-pointer-type object-not-structure)

  (frob bignump check-bigunm bignum
    vm:bignum-type object-not-bignum-error)

  (frob ratiop check-ratio ratio
    vm:ratio-type object-not-ratio-error)

  (frob complexp check-complex complex
    vm:complex-type object-not-complex-error)

  (frob single-float-p check-single-float single-float
    vm:single-float-type object-not-single-float-error)

  (frob double-float-p check-double-float double-float
    vm:double-float-type object-not-double-float-error)

  (frob simple-string-p check-simple-string simple-string
    vm:simple-string-type object-not-simple-string-error)

  (frob simple-bit-vector-p check-simple-bit-vector simple-bit-vector
    vm:simple-bit-vector-type object-not-simple-bit-vector-error)

  (frob simple-vector-p check-simple-vector simple-vector
    vm:simple-vector-type object-not-simple-vector-error)

  (frob simple-array-unsigned-byte-2-p check-simple-array-unsigned-byte-2
    simple-array-unsigned-byte-2 vm:simple-array-unsigned-byte-2-type
    object-not-simple-array-unsigned-byte-2-error)

  (frob simple-array-unsigned-byte-4-p check-simple-array-unsigned-byte-4
    simple-array-unsigned-byte-4 vm:simple-array-unsigned-byte-4-type
    object-not-simple-array-unsigned-byte-4-error)

  (frob simple-array-unsigned-byte-8-p check-simple-array-unsigned-byte-8
    simple-array-unsigned-byte-8 vm:simple-array-unsigned-byte-8-type
    object-not-simple-array-unsigned-byte-8-error)

  (frob simple-array-unsigned-byte-16-p check-simple-array-unsigned-byte-16
    simple-array-unsigned-byte-16 vm:simple-array-unsigned-byte-16-type
    object-not-simple-array-unsigned-byte-16-error)

  (frob simple-array-unsigned-byte-32-p check-simple-array-unsigned-byte-32
    simple-array-unsigned-byte-32 vm:simple-array-unsigned-byte-32-type
    object-not-simple-array-unsigned-byte-32-error)

  (frob simple-array-single-float-p check-simple-array-single-float
    simple-array-single-float vm:simple-array-single-float-type
    object-not-simple-array-single-float-error)

  (frob simple-array-double-float-p check-simple-array-double-float
    simple-array-double-float vm:simple-array-double-float-type
    object-not-simple-array-double-float-error)

  (frob base-char-p check-base-character base-character
    vm:base-character-type object-not-base-character-error)

  (frob system-area-pointer-p check-system-area-pointer system-area-pointer
    vm:sap-type object-not-sap-error)

  (frob weak-pointer-p check-weak-pointer weak-pointer
    vm:weak-pointer-type object-not-weak-pointer-error))


;;; Slightly tenser versions for FIXNUM's
;;; 
(define-vop (check-fixnum check-simple-type)
  (:generator 3
    (let ((err-lab (generate-error-code vop object-not-fixnum-error value)))
      (inst and temp value #x3)
      (inst bne temp zero-tn err-lab)
      (move result value t))))

(define-vop (fixnump simple-type-predicate)
  (:ignore type-code)
  (:translate ext:fixnump)
  (:generator 3
    (inst and temp value #x3)
    (if not-p
	(inst bne temp zero-tn target)
	(inst beq temp zero-tn target))
    (inst nop)))


;;;; Hairy type tests:
;;;
;;;    These types are represented by a union of type codes.  
;;;

(define-vop (hairy-type-predicate)
  (:args
   (obj :scs (any-reg descriptor-reg)
	:target temp))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:type random :scs (non-descriptor-reg)) temp))

(define-vop (check-hairy-type)
  (:args
   (obj :scs (any-reg descriptor-reg)
	:target res))
  (:results
   (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random :scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (pred-name check-name error-code &rest types)
	     (let ((cost (* (+ (length types)
			       (count-if #'consp types))
			    4)))
	       `(progn
		  ,@(when pred-name
		      `((define-vop (,pred-name hairy-type-predicate)
			  (:translate ,pred-name)
			  (:generator ,cost
			    (test-hairy-type obj temp target not-p ,@types)))))
			
		  ,@(when check-name
		      `((define-vop (,check-name check-hairy-type)
			  (:generator ,cost
			    (let ((err-lab (generate-error-code vop
								,error-code
								obj)))
			      (test-hairy-type obj temp err-lab t ,@types))
			    (move res obj)))))))))

  (frob array-header-p nil nil
    vm:simple-array-type vm:complex-string-type vm:complex-bit-vector-type
    vm:complex-vector-type vm:complex-array-type)
  
  (frob nil check-function-or-symbol object-not-function-or-symbol-error
    vm:function-pointer-type vm:symbol-header-type)

  (frob stringp check-string object-not-string-error
    vm:simple-string-type vm:complex-string-type)

  (frob bit-vector-p check-bit-vector object-not-bit-vector-error
    vm:simple-bit-vector-type vm:complex-bit-vector-type)

  (frob vectorp check-vector object-not-vector-error
    vm:simple-string-type vm:simple-bit-vector-type vm:simple-vector-type
    vm:simple-array-unsigned-byte-2-type vm:simple-array-unsigned-byte-4-type
    vm:simple-array-unsigned-byte-8-type vm:simple-array-unsigned-byte-16-type
    vm:simple-array-unsigned-byte-32-type vm:simple-array-single-float-type
    vm:simple-array-double-float-type vm:complex-string-type
    vm:complex-bit-vector-type vm:complex-vector-type)

  (frob simple-array-p check-simple-array object-not-simple-array-error
    vm:simple-array-type vm:simple-string-type vm:simple-bit-vector-type
    vm:simple-vector-type vm:simple-array-unsigned-byte-2-type
    vm:simple-array-unsigned-byte-4-type vm:simple-array-unsigned-byte-8-type
    vm:simple-array-unsigned-byte-16-type vm:simple-array-unsigned-byte-32-type
    vm:simple-array-single-float-type vm:simple-array-double-float-type)

  (frob arrayp check-array object-not-array-error
    vm:simple-array-type vm:simple-string-type vm:simple-bit-vector-type
    vm:simple-vector-type vm:simple-array-unsigned-byte-2-type
    vm:simple-array-unsigned-byte-4-type vm:simple-array-unsigned-byte-8-type
    vm:simple-array-unsigned-byte-16-type vm:simple-array-unsigned-byte-32-type
    vm:simple-array-single-float-type vm:simple-array-double-float-type
    vm:complex-string-type vm:complex-bit-vector-type vm:complex-vector-type
    vm:complex-array-type)
    
  (frob numberp check-number object-not-number-error
    vm:even-fixnum-type vm:odd-fixnum-type vm:bignum-type vm:ratio-type
    vm:single-float-type vm:double-float-type vm:complex-type)

  (frob rationalp check-rational object-not-rational-error
    vm:even-fixnum-type vm:odd-fixnum-type vm:ratio-type vm:bignum-type)

  (frob floatp check-float object-not-float-error
    vm:single-float-type vm:double-float-type)

  (frob realp check-real object-not-real-error
    vm:even-fixnum-type vm:odd-fixnum-type vm:ratio-type vm:bignum-type
    vm:single-float-type vm:double-float-type)
  
  ;; ### May want to make this more tense.
  (frob integerp check-integer object-not-integer-error
    vm:even-fixnum-type vm:odd-fixnum-type vm:bignum-type))


;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p hairy-type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	(inst and temp obj #x3)
	(inst beq temp zero-tn yep)
	(test-simple-type obj temp nope t vm:bignum-type)
	(loadw temp obj 0 vm:other-pointer-type)
	(inst srl temp temp (1+ vm:type-bits))
	(if not-p
	    (inst bne temp zero-tn target)
	    (inst beq temp zero-tn target))
	(inst nop)
	(emit-label not-target)))))

(define-vop (check-signed-byte-32 check-hairy-type)
  (:generator 45
    (let ((nope (generate-error-code vop object-not-signed-byte-32-error obj))
	  (yep (gen-label)))
      (inst and temp obj #x3)
      (inst beq temp zero-tn yep)
      (test-simple-type obj temp nope t vm:bignum-type)
      (loadw temp obj 0 vm:other-pointer-type)
      (inst srl temp temp (1+ vm:type-bits))
      (inst bne temp zero-tn nope)
      (inst nop)
      (emit-label yep)
      (move res obj))))


;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(define-vop (unsigned-byte-32-p hairy-type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label))
	  (single-word (gen-label))
	  (fixnum (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	;; Is it a fixnum?
	(inst and temp obj #x3)
	(inst beq temp zero-tn fixnum)
	;; If not, is it a bignum?
	(test-simple-type obj temp nope t vm:bignum-type)
	;; Get the number of digits.
	(loadw temp obj 0 vm:other-pointer-type)
	(inst srl temp temp vm:type-bits)
	;; Is it one?
	(inst addu temp -1)
	(inst beq temp single-word)
	;; If it's other than two, we can't be an (unsigned-byte 32)
	(inst addu temp -1)
	(inst bne temp nope)
	;; Get the second digit.
	(loadw temp obj (1+ vm:bignum-digits-offset) vm:other-pointer-type)
	;; All zeros, its an (unsigned-byte 32).
	(inst beq temp yep)
	(inst nop)
	;; Otherwise, it isn't.
	(inst b nope)
	(inst nop)
	
	(emit-label single-word)
	;; Get the single digit.
	(loadw temp obj vm:bignum-digits-offset vm:other-pointer-type)
	;; positive implies (unsigned-byte 32).
	(inst bgez temp yep)
	(inst nop)
	;; Otherwise, nope.
	(inst b nope)
	(inst nop)

	(emit-label fixnum)
	;; positive fixnums are (unsigned-byte 32).
	(if not-p
	    (inst bltz obj target)
	    (inst bgez obj target))
	(inst nop)

	(emit-label not-target)))))	  

(define-vop (check-unsigned-byte-32 check-hairy-type)
  (:generator 45
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-32-error obj))
	  (yep (gen-label))
	  (fixnum (gen-label))
	  (single-word (gen-label)))
      ;; Is it a fixnum?
      (inst and temp obj #x3)
      (inst beq temp zero-tn fixnum)
      ;; If not, is it a bignum?
      (test-simple-type obj temp nope t vm:bignum-type)
      ;; Get the number of digits.
      (loadw temp obj 0 vm:other-pointer-type)
      (inst srl temp temp vm:type-bits)
      ;; Is it one?
      (inst addu temp -1)
      (inst beq temp single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst addu temp -1)
      (inst bne temp nope)
      ;; Get the second digit.
      (loadw temp obj (1+ vm:bignum-digits-offset) vm:other-pointer-type)
      ;; All zeros, its an (unsigned-byte 32).
      (inst beq temp yep)
      (inst nop)
      ;; Otherwise, it isn't.
      (inst b nope)
      (inst nop)
      
      (emit-label single-word)
      ;; Get the single digit.
      (loadw temp obj vm:bignum-digits-offset vm:other-pointer-type)
      ;; positive implies (unsigned-byte 32).
      (inst bgez temp yep)
      (inst nop)
      ;; Otherwise, nope.
      (inst b nope)
      (inst nop)
      
      (emit-label fixnum)
      ;; positive fixnums are (unsigned-byte 32).
      (inst bltz obj nope)
      (inst nop)
      
      (emit-label yep)
      (move res obj))))




;;;; List/symbol types:
;;; 
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (list-symbol-predicate)
  (:args
   (obj :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)))

(define-vop (check-list-symbol check-hairy-type)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp))


(macrolet ((frob (pred-name check-name error-code &rest body)
	     `(progn
		(define-vop (,pred-name list-symbol-predicate)
		  (:translate ,pred-name)
		  (:generator 8
		    ,@body))
		(define-vop (,check-name check-list-symbol)
		  (:generator 8
		    (let ((target (generate-error-code vop ,error-code obj))
			  (not-p t))
		      ,@body
		      (move res obj)))))))

  (frob symbolp check-symbol object-not-symbol-error
    (let* ((drop-thru (gen-label))
	   (is-symbol-label (if not-p drop-thru target)))
      (inst beq obj null-tn is-symbol-label)
      (inst nop)
      (test-simple-type obj temp target not-p vm:symbol-header-type)
      (emit-label drop-thru)))

  (frob consp check-cons object-not-cons-error
    (let* ((drop-thru (gen-label))
	   (is-not-cons-label (if not-p target drop-thru)))
      (inst beq obj null-tn is-not-cons-label)
      (inst nop)
      (test-simple-type obj temp target not-p vm:list-pointer-type)
      (emit-label drop-thru))))


;;;; Function Coercion

;;; If not a function, get the symbol value and test for that being a
;;; function.  Since we test for a function rather than the unbound
;;; marker, this works on NIL.
;;;
(define-vop (coerce-to-function)
  (:args (object :scs (descriptor-reg)
		:target result))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) nd-temp)
  (:temporary (:scs (descriptor-reg)) saved-object)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (let ((not-function-label (gen-label))
	  (not-coercable-label (gen-label))
	  (done-label (gen-label)))
      (test-simple-type object nd-temp not-function-label t
			vm:function-pointer-type)
      (move result object)
      (emit-label done-label)

      (assemble (*elsewhere*)
	(emit-label not-function-label)
	(test-simple-type object nd-temp not-coercable-label t
			  vm:symbol-header-type)
	(move saved-object object)
	(loadw result object vm:symbol-function-slot vm:other-pointer-type)
	(test-simple-type result nd-temp done-label nil
			  vm:function-pointer-type)
	(error-call vop undefined-symbol-error saved-object)
	
	(emit-label not-coercable-label)
	(error-call vop object-not-coercable-to-function-error object)))))

(define-vop (fast-safe-coerce-to-function)
  (:args (object :scs (descriptor-reg)
		:target result))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) nd-temp)
  (:temporary (:scs (descriptor-reg)) saved-object)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (let ((not-function-label (gen-label))
	  (done-label (gen-label)))
      (test-simple-type object nd-temp not-function-label t
			vm:function-pointer-type)
      (move result object)
      (emit-label done-label)

      (assemble (*elsewhere*)
	(emit-label not-function-label)
	(move saved-object object)
	(loadw result object vm:symbol-function-slot vm:other-pointer-type)
	(test-simple-type result nd-temp done-label nil
			  vm:function-pointer-type)
	(error-call vop undefined-symbol-error saved-object)))))
