;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(define-vop (string-char-move)
  (:args (x :target y
	    :scs (string-char-reg)
	    :load nil))
  (:results (y :scs (string-char-reg)
	       :load nil))
  (:temporary (:scs (string-char-reg) :type string-char
	       :from :argument  :to :result)
	      temp)
  (:effects)
  (:affected)
  #+nil
  (:generator 0
    (sc-case x ((string-char-reg string-char-stack immediate-string-char
				 descriptor-reg any-reg stack)))
    (sc-case y ((string-char-reg string-char-stack
				 descriptor-reg any-reg stack)))

    (let* ((x-char (sc-is x string-char-reg string-char-stack
			  immediate-string-char))
	   (y-char (sc-is y string-char-reg string-char-stack))
	   (same-rep (if x-char y-char (not y-char)))
	   (src (if (sc-is x stack string-char-stack immediate-string-char)
		    temp x))
	   (dest (if (sc-is y stack string-char-stack) temp y)))

      (unless (and same-rep (location= x y))

	(unless (eq x src)
	  (sc-case x
	    ((string-char-stack stack)
	     (load-stack-tn src x))
	    (immediate-string-char
	     (loadi src (char-code (tn-value x))))))

	(if same-rep
	    (unless (location= src dest)
	      (inst lr dest src))
	    (if x-char
		(inst oiu dest src (ash system:%string-char-type
					clc::type-shift-16))
		(inst nilz dest src system:%character-code-mask)))

	(unless (eq y dest)
	  (store-stack-tn y dest)))))) 

(primitive-type-vop string-char-move (:coerce-to-t :coerce-from-t :move)
  string-char)

(define-vop (char-code)
  (:args (ch :scs (string-char-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:arg-types string-char)
  (:translate char-code)
  (:policy :fast-safe)
  #+nil
  (:generator 0
    (unless (location= ch res)
      (inst lr res ch))))

(define-vop (code-char)
  (:args (code :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (string-char-reg)))
  (:result-types string-char)
  (:translate code-char)
  (:policy :fast-safe)
  #+nil
  (:generator 0
    (unless (location= code res)
      (inst lr res code))))

;;; For comparison of string-chars, we require both operands to be in the
;;; untagged string-char-reg representation.  This will be a pessimization if
;;; both operands are tagged, but this won't happen often, and not in
;;; performance-critical cases.
;;;
(define-vop (string-char-compare pointer-compare)
  (:args (x :scs (string-char-reg))
	 (y :scs (string-char-reg)))
  (:arg-types string-char string-char))

(define-vop (fast-char=/string-char string-char-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</string-char string-char-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/string-char string-char-compare)
  (:translate char>)
  (:variant :gt))

;;; If we don't know that both operands are string-chars, then we just compare
;;; the whole boxed object.  This assume that the hairy character type code is
;;; greater than the string-char type, since a string-char must always be less
;;; than a hairy char.
;;;
(define-vop (char-compare pointer-compare)
  (:variant-cost 5))

(define-vop (fast-char= char-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char< char-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char> char-compare)
  (:translate char>)
  (:variant :gt))
