;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/char.lisp,v 1.4 1990/03/22 23:53:02 ch Exp $
;;; 
;;; This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan
;;; Converted for the MIPS R2000 by Christopher Hoover.
;;;
(in-package 'c)

(define-vop (base-character-move)
  (:args (x :target y
	    :scs (base-character-reg)
	    :load nil))
  (:results (y :scs (base-character-reg)
	       :load nil))
  (:temporary (:scs (base-character-reg) :type base-character
		    :from :argument  :to :result)
	      temp)
  (:effects)
  (:affected)
  (:generator 0
    (sc-case x ((base-character-reg base-character-stack
				    immediate-base-character descriptor-reg
				    any-reg control-stack)))
    (sc-case y ((base-character-reg base-character-stack
				 descriptor-reg any-reg control-stack)))

    (let* ((x-char (sc-is x base-character-reg base-character-stack
			  immediate-base-character))
	   (y-char (sc-is y base-character-reg base-character-stack))
	   (same-rep (if x-char y-char (not y-char)))
	   (src (if (sc-is x control-stack base-character-stack
			   immediate-base-character)
		    temp x))
	   (dest (if (sc-is y control-stack base-character-stack) temp y)))

      (unless (and same-rep (location= x y))

	(unless (eq x src)
	  (sc-case x
	    ((base-character-stack control-stack)
	     (load-stack-tn src x))
	    (immediate-base-character
	     (loadi src (char-code (tn-value x))))))

	(cond (same-rep
	       (move dest src))
	      (x-char
	       (inst sll dest src vm:type-bits)
	       (inst ori dest dest vm:base-character-type))
	      (t
	       (inst srl dest src vm:type-bits)))

	(unless (eq y dest)
	  (store-stack-tn y dest))))))

(primitive-type-vop base-character-move (:coerce-to-t :coerce-from-t :move)
  base-character)

(define-vop (char-code)
  (:args (ch :scs (base-character-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:arg-types base-character)
  (:translate char-code)
  (:policy :fast-safe)
  (:generator 0
    (inst sll res ch 2)))

(define-vop (code-char)
  (:args (code :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (base-character-reg)))
  (:result-types base-character)
  (:translate code-char)
  (:policy :fast-safe)
  (:generator 0
    (inst srl res code 2)))

;;; Comparison of base-characters -- works for boxed and unboxed
;;; characters since we don't have bits, etc.
;;;
(define-vop (base-character-compare pointer-compare)
  (:args (x :scs (base-character-reg))
	 (y :scs (base-character-reg)))
  (:arg-types base-character base-character))

(define-vop (fast-char=/base-character base-character-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</base-character base-character-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/base-character base-character-compare)
  (:translate char>)
  (:variant :gt))
