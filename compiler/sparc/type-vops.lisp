;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/type-vops.lisp,v 1.1 1990/11/30 17:05:06 wlott Exp $
;;; 
;;; This file contains the VM definition of type testing and checking VOPs
;;; for the SPARC.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")


;;;; Simple type checking and testing:
;;;
;;;    These types are represented by a single type code, so are easily
;;; open-coded as a mask and compare.

(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:type random :scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:type random :scs (non-descriptor-reg)) temp))

(eval-when (compile eval)

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) vm:lowtag-limit) 7 2)))

(defmacro def-type-vops (pred-name check-name ptype error-code
				   &rest type-codes)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
	   `((define-vop (,pred-name type-predicate)
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value temp target not-p ,@type-codes)))))
       ,@(when check-name
	   `((define-vop (,check-name check-type)
	       (:generator ,cost
		 (let ((err-lab
			(generate-error-code vop ,error-code value)))
		   (test-type value temp err-lab t ,@type-codes)
		   (move result value))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

); eval-when (compile eval)


(def-type-vops fixnump check-fixnum nil object-not-fixnum-error
  vm:even-fixnum-type vm:odd-fixnum-type)

(def-type-vops functionp check-function function
  object-not-function-error vm:function-pointer-type)

(def-type-vops listp check-list list
  object-not-list-error vm:list-pointer-type)

#+nil ;; ### Only after we have real structures.
(def-type-vops structurep check-structure structure
  object-not-structure vm:structure-pointer-type)

(def-type-vops structurep check-structure structure
  object-not-structure-error vm:structure-header-type))

(def-type-vops bignump check-bigunm bignum
  object-not-bignum-error vm:bignum-type)

(def-type-vops ratiop check-ratio ratio
  object-not-ratio-error vm:ratio-type)

(def-type-vops complexp check-complex complex
  object-not-complex-error vm:complex-type)

(def-type-vops single-float-p check-single-float single-float
  object-not-single-float-error vm:single-float-type)

(def-type-vops double-float-p check-double-float double-float
  object-not-double-float-error vm:double-float-type)

(def-type-vops simple-string-p check-simple-string simple-string
  object-not-simple-string-error vm:simple-string-type)

(def-type-vops simple-bit-vector-p check-simple-bit-vector simple-bit-vector
  object-not-simple-bit-vector-error vm:simple-bit-vector-type)

(def-type-vops simple-vector-p check-simple-vector simple-vector
  object-not-simple-vector-error vm:simple-vector-type)

(def-type-vops simple-array-unsigned-byte-2-p
  check-simple-array-unsigned-byte-2
  simple-array-unsigned-byte-2
  object-not-simple-array-unsigned-byte-2-error
  vm:simple-array-unsigned-byte-2-type)

(def-type-vops simple-array-unsigned-byte-4-p
  check-simple-array-unsigned-byte-4
  simple-array-unsigned-byte-4
  object-not-simple-array-unsigned-byte-4-error
  vm:simple-array-unsigned-byte-4-type)

(def-type-vops simple-array-unsigned-byte-8-p
  check-simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8
  object-not-simple-array-unsigned-byte-8-error
  vm:simple-array-unsigned-byte-8-type)

(def-type-vops simple-array-unsigned-byte-16-p
  check-simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16
  object-not-simple-array-unsigned-byte-16-error
  vm:simple-array-unsigned-byte-16-type)

(def-type-vops simple-array-unsigned-byte-32-p
  check-simple-array-unsigned-byte-32
  simple-array-unsigned-byte-32
  object-not-simple-array-unsigned-byte-32-error
  vm:simple-array-unsigned-byte-32-type)

(def-type-vops simple-array-single-float-p check-simple-array-single-float
  simple-array-single-float object-not-simple-array-single-float-error
  vm:simple-array-single-float-type)

(def-type-vops simple-array-double-float-p check-simple-array-double-float
  simple-array-double-float object-not-simple-array-double-float-error
  vm:simple-array-double-float-type)

(def-type-vops base-char-p check-base-character base-character
  object-not-base-character-error vm:base-character-type)

(def-type-vops system-area-pointer-p check-system-area-pointer
  system-area-pointer object-not-sap-error vm:sap-type)

(def-type-vops weak-pointer-p check-weak-pointer weak-pointer
  object-not-weak-pointer-error vm:weak-pointer-type)

(def-type-vops array-header-p nil nil nil
  vm:simple-array-type vm:complex-string-type vm:complex-bit-vector-type
  vm:complex-vector-type vm:complex-array-type)

(def-type-vops nil check-function-or-symbol nil object-not-function-or-symbol-error
  vm:function-pointer-type vm:symbol-header-type)

(def-type-vops stringp check-string nil object-not-string-error
  vm:simple-string-type vm:complex-string-type)

(def-type-vops bit-vector-p check-bit-vector nil object-not-bit-vector-error
  vm:simple-bit-vector-type vm:complex-bit-vector-type)

(def-type-vops vectorp check-vector nil object-not-vector-error
  vm:simple-string-type vm:simple-bit-vector-type vm:simple-vector-type
  vm:simple-array-unsigned-byte-2-type vm:simple-array-unsigned-byte-4-type
  vm:simple-array-unsigned-byte-8-type vm:simple-array-unsigned-byte-16-type
  vm:simple-array-unsigned-byte-32-type vm:simple-array-single-float-type
  vm:simple-array-double-float-type vm:complex-string-type
  vm:complex-bit-vector-type vm:complex-vector-type)

(def-type-vops simple-array-p check-simple-array nil object-not-simple-array-error
  vm:simple-array-type vm:simple-string-type vm:simple-bit-vector-type
  vm:simple-vector-type vm:simple-array-unsigned-byte-2-type
  vm:simple-array-unsigned-byte-4-type vm:simple-array-unsigned-byte-8-type
  vm:simple-array-unsigned-byte-16-type vm:simple-array-unsigned-byte-32-type
  vm:simple-array-single-float-type vm:simple-array-double-float-type)

(def-type-vops arrayp check-array nil object-not-array-error
  vm:simple-array-type vm:simple-string-type vm:simple-bit-vector-type
  vm:simple-vector-type vm:simple-array-unsigned-byte-2-type
  vm:simple-array-unsigned-byte-4-type vm:simple-array-unsigned-byte-8-type
  vm:simple-array-unsigned-byte-16-type vm:simple-array-unsigned-byte-32-type
  vm:simple-array-single-float-type vm:simple-array-double-float-type
  vm:complex-string-type vm:complex-bit-vector-type vm:complex-vector-type
  vm:complex-array-type)

(def-type-vops numberp check-number nil object-not-number-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:bignum-type vm:ratio-type
  vm:single-float-type vm:double-float-type vm:complex-type)

(def-type-vops rationalp check-rational nil object-not-rational-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:ratio-type vm:bignum-type)

(def-type-vops integerp check-integer nil object-not-integer-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:bignum-type)

(def-type-vops floatp check-float nil object-not-float-error
  vm:single-float-type vm:double-float-type)

(def-type-vops realp check-real nil object-not-real-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:ratio-type vm:bignum-type
  vm:single-float-type vm:double-float-type)


;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	(inst andcc zero-tn value #x3)
	(inst b :eq yep)
	(test-type value temp nope t vm:other-pointer-type)
	(loadw temp value 0 vm:other-pointer-type)
	(inst cmp temp (+ (ash 1 vm:type-bits)
			  vm:bignum-type))
	(inst b (if not-p :ne :eq) target)
	(inst nop)
	(emit-label not-target)))))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop object-not-signed-byte-32-error value))
	  (yep (gen-label)))
      (inst andcc temp value #x3)
      (inst b :eq yep)
      (test-type value temp nope t vm:other-pointer-type)
      (loadw temp value 0 vm:other-pointer-type)
      (inst cmp temp (+ (ash 1 vm:type-bits) vm:bignum-type))
      (inst b :ne nope)
      (inst nop)
      (emit-label yep)
      (move result value))))


;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(define-vop (unsigned-byte-32-p type-predicate)
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
	(inst andcc temp value #x3)
	(inst b :eq fixnum)
	(inst cmp value)

	;; If not, is it an other pointer?
	(test-type value temp nope t vm:other-pointer-type)
	;; Get the header.
	(loadw temp value 0 vm:other-pointer-type)
	;; Is it one?
	(inst cmp temp (+ (ash 1 vm:type-bits) vm:bignum-type))
	(inst b :eq single-word)
	;; If it's other than two, we can't be an (unsigned-byte 32)
	(inst cmp temp (+ (ash 2 vm:type-bits) vm:bignum-type))
	(inst b :ne nope)
	;; Get the second digit.
	(loadw temp value (1+ vm:bignum-digits-offset) vm:other-pointer-type)
	;; All zeros, its an (unsigned-byte 32).
	(inst cmp temp)
	(inst b :eq yep)
	(inst nop)
	;; Otherwise, it isn't.
	(inst b nope)
	(inst nop)
	
	(emit-label single-word)
	;; Get the single digit.
	(loadw temp value vm:bignum-digits-offset vm:other-pointer-type)
	(inst cmp temp)

	;; positive implies (unsigned-byte 32).
	(emit-label fixnum)
	(inst b (if not-p :lt :ge) target)
	(inst nop)

	(emit-label not-target)))))	  

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-32-error value))
	  (yep (gen-label))
	  (fixnum (gen-label))
	  (single-word (gen-label)))
      ;; Is it a fixnum?
      (inst andcc temp value #x3)
      (inst b :eq fixnum)
      (inst cmp value)

      ;; If not, is it an other pointer?
      (test-type value temp nope t vm:other-pointer-type)
      ;; Get the number of digits.
      (loadw temp value 0 vm:other-pointer-type)
      ;; Is it one?
      (inst cmp temp (+ (ash 1 vm:type-bits) vm:bignum-type))
      (inst b :eq single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst cmp temp (+ (ash 2 vm:type-bits) vm:bignum-type))
      (inst b :ne nope)
      ;; Get the second digit.
      (loadw temp value (1+ vm:bignum-digits-offset) vm:other-pointer-type)
      ;; All zeros, its an (unsigned-byte 32).
      (inst cmp temp)
      (inst b :eq yep)
      ;; Otherwise, it isn't.
      (inst b :ne nope)
      (inst nop)
      
      (emit-label single-word)
      ;; Get the single digit.
      (loadw temp value vm:bignum-digits-offset vm:other-pointer-type)
      ;; positive implies (unsigned-byte 32).
      (inst cmp temp)
      
      (emit-label fixnum)
      (inst b :lt nope)
      (inst nop)
      
      (emit-label yep)
      (move result value))))




;;;; List/symbol types:
;;; 
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let* ((drop-thru (gen-label))
	   (is-symbol-label (if not-p drop-thru target)))
      (inst cmp value null-tn)
      (inst b :eq is-symbol-label)
      (test-type value temp target not-p vm:symbol-header-type)
      (emit-label drop-thru))))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((drop-thru (gen-label))
	  (error (generate-error-code vop object-not-symbol-error value)))
      (inst cmp value null-tn)
      (inst b :eq drop-thru)
      (test-type value temp error t vm:symbol-header-type)
      (emit-label drop-thru)
      (move result value))))
  
(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
	   (is-not-cons-label (if not-p target drop-thru)))
      (inst cmp value null-tn)
      (inst b :eq is-not-cons-label)
      (test-type value temp target not-p vm:list-pointer-type)
      (emit-label drop-thru))))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst cmp value null-tn)
      (inst b :eq error)
      (test-type value temp error t vm:list-pointer-type)
      (move result value))))


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
      (test-type object nd-temp not-function-label t
		 vm:function-pointer-type)
      (move result object)
      (emit-label done-label)

      (assemble (*elsewhere*)
	(emit-label not-function-label)
	(test-type object nd-temp not-coercable-label t
		   vm:symbol-header-type)
	(move saved-object object)
	(loadw result object vm:symbol-function-slot vm:other-pointer-type)
	(test-type result nd-temp done-label nil
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
  (:generator 10
    (let ((not-function-label (gen-label))
	  (done-label (gen-label)))
      (test-type object nd-temp not-function-label t vm:function-pointer-type)
      (move result object)
      (emit-label done-label)

      (assemble (*elsewhere*)
	(emit-label not-function-label)
	(move saved-object object)
	(loadw result object vm:symbol-function-slot vm:other-pointer-type)
	(test-type result nd-temp done-label nil vm:function-pointer-type)
	(error-call vop undefined-symbol-error saved-object)))))
