;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/type-vops.lisp,v 1.4 1990/03/13 00:03:17 wlott Exp $
;;; 
;;; This file contains the VM definition of type testing and checking VOPs
;;; for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for the MIPS R2000 by Christopher Hoover.
;;;
(in-package "C")

;;; ### These belongs in compiler/fundb.lisp
;;; 
(defknown realp (t) boolean (movable foldable flushable))
(defknown system-area-pointer-p (t) boolean (movable foldable flushable))


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
  (:variant-vars type-code error-code)
  (:node-var node)
  (:temporary (:type random :scs (non-descriptor-reg)) temp)
  (:generator 4
    (let ((err-lab (generate-error-code node error-code value)))
      (test-simple-type value temp err-lab t type-code)
      (move result value))))

(define-vop (simple-type-predicate)
  (:args
   (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:variant-vars type-code)
  (:policy :fast-safe)
  (:temporary (:type random :scs (non-descriptor-reg)) temp)
  (:generator 4
    (test-simple-type value temp target not-p type-code)))

(macrolet ((frob (pred-name check-name ptype type-code error-code)
	     `(progn
		(define-vop (,pred-name simple-type-predicate)
		  (:variant ,type-code)
		  (:translate ,pred-name))

		,@(when check-name
		    `((define-vop (,check-name check-simple-type)
			(:variant ,type-code ,error-code))
		      (primitive-type-vop ,check-name (:check) ,ptype))))))

  ;; ### Want to tweek costs so that checks that do dereferences are
  ;; more expensive.
  ;; 
  ;; ### May want to add all of the (simple-array <mumble> (*))
  ;; primitive types.
  ;;
  ;; ### May need to add array-header-p and friends.  Whoever ports the
  ;; array code will probably have to frob stuff here.
  ;; 
  (frob functionp check-function function
    vm:function-pointer-type di:object-not-function-error)

  (frob listp check-list list
    vm:list-pointer-type di:object-not-list-error)

  (frob bignump check-bigunm bignum
    vm:bignum-type di:object-not-bignum-error)

  (frob ratiop check-ratio ratio
    vm:ratio-type di:object-not-ratio-error)

  (frob single-float-p check-single-float single-float
    vm:single-float-type di:object-not-single-float-error)

  (frob double-float-p check-double-float double-float
    vm:double-float-type di:object-not-double-float-error)

  (frob simple-string-p check-simple-string simple-string
    vm:simple-string-type di:object-not-simple-string-error)

  (frob simple-bit-vector-p check-simple-bit-vector simple-bit-vector
    vm:simple-bit-vector-type di:object-not-simple-bit-vector-error)

  (frob simple-vector-p check-simple-vector simple-vector
    vm:simple-vector-type di:object-not-simple-vector-error)

  ;; ### This should really be base-character-p ...
  (frob string-char-p check-base-character base-character
    vm:base-character-type di:object-not-base-character-error)

  (frob system-area-pointer-p check-system-area-pointer system-area-pointer
    vm:sap-type di:object-not-sap-error))


;;; Slightly tenser versions for FIXNUM's
;;; 
(define-vop (check-fixnum check-simple-type)
  (:ignore type-code error-code)
  (:generator 3
    (let ((err-lab (generate-error-code node di:object-not-fixnum-error
					value)))
      (inst andi temp value #x3)
      (inst bne temp zero-tn err-lab)
      (move result value t))))

(define-vop (fixnump simple-type-predicate)
  (:ignore type-code)
  (:generator 3
    (inst andi temp value #x3)
    (if not-p
	(inst bne temp zero-tn target)
	(inst beq temp zero-tn target))
    (nop)))

(primitive-type-vop check-fixnum (:check) fixnum)


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
  (:node-var node))

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
			    (let ((err-lab (generate-error-code
					    node ,error-code obj)))
			      (test-hairy-type obj temp err-lab t ,@types))
			    (move res obj)))))))))

  (frob nil check-function-or-symbol di:object-not-function-or-symbol-error
    vm:function-pointer-type vm:symbol-header-type)

  (frob vectorp check-vector di:object-not-vector-error
    (vm:simple-string-type vm:simple-array-double-float-type))

  (frob stringp check-string di:object-not-string-error
    vm:simple-string-type vm:complex-string-type)

  (frob bit-vector-p check-bit-vector di:object-not-bit-vector-error
    vm:simple-bit-vector-type vm:complex-bit-vector-type)

  (frob arrayp check-array di:object-not-array-error
    (vm:simple-array-type vm:complex-array-type))

  (frob numberp check-number di:object-not-number-error
    vm:even-fixnum-type vm:odd-fixnum-type
    (vm:bignum-type vm:complex-type))

  (frob rationalp check-rational di:object-not-rational-error
    vm:even-fixnum-type vm:odd-fixnum-type
    vm:ratio-type vm:bignum-type)

  (frob floatp check-float di:object-not-float-error
    vm:single-float-type vm:double-float-type)

  (frob realp check-real di:object-not-real-error
    vm:even-fixnum-type vm:odd-fixnum-type
    vm:ratio-type vm:bignum-type
    vm:single-float-type vm:double-float-type)
  
  ;; ### May want to make this more tense.
  (frob integerp check-integer di:object-not-integer-error
    vm:even-fixnum-type vm:odd-fixnum-type
    vm:bignum-type))


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
		    (let ((target (generate-error-code node ,error-code obj))
			  (not-p t))
		      ,@body
		      (move res obj)))))))

  (frob symbolp check-symbol di:object-not-symbol-error
    (let* ((drop-thru (gen-label))
	   (is-symbol-label (if not-p drop-thru target)))
      (inst beq obj null-tn is-symbol-label)
      (nop)
      (test-simple-type obj temp target not-p vm:symbol-header-type)
      (emit-label drop-thru)))

  (frob consp check-cons di:object-not-cons-error
    (let* ((drop-thru (gen-label))
	   (is-not-cons-label (if not-p target drop-thru)))
      (inst beq obj null-tn is-not-cons-label)
      (nop)
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
  (:node-var node)
  (:temporary (:type random  :scs (non-descriptor-reg)) nd-temp)
  (:temporary (:scs (descriptor-reg)) saved-object)
  (:generator 0
    (let ((not-function-label (gen-label))
	  (not-coercable-label (gen-label))
	  (done-label (gen-label)))
      (test-simple-type object nd-temp not-function-label t
			vm:function-pointer-type)
      (move result object)
      (emit-label done-label)

      (unassemble
	(assemble-elsewhere node
	  (emit-label not-function-label)
	  (test-simple-type object nd-temp not-coercable-label t
			    vm:symbol-header-type)
	  (move saved-object object)
	  (loadw result object vm:symbol-function-slot vm:other-pointer-type)
	  (test-simple-type result nd-temp done-label nil
			    vm:function-pointer-type)
	  (error-call di:undefined-symbol-error saved-object)

	  (emit-label not-coercable-label)
	  (error-call di:object-not-coercable-to-function-error object))))))
