;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/char.lisp,v 1.6 1990/04/24 02:56:01 wlott Exp $
;;; 
;;; This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan
;;; Converted for the MIPS R2000 by Christopher Hoover.
;;;
(in-package "C")



;;;; Moves and coercions:

;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-character)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (base-character-reg)))
  (:generator 1
    (inst srl y x vm:type-bits)))
;;;
(define-move-vop move-to-base-character :move
  (any-reg descriptor-reg) (base-character-reg))


;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-character)
  (:args (x :scs (base-character-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst sll y x vm:type-bits)
    (inst or y y vm:base-character-type)))
;;;
(define-move-vop move-from-base-character :move
  (base-character-reg) (any-reg descriptor-reg))

;;; Move untagged base-character values.
;;;
(define-vop (base-character-move)
  (:args (x :target y
	    :scs (base-character-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-character-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop base-character-move :move
  (base-character-reg) (base-character-reg))


;;; Move untagged base-character arguments/return-values.
;;;
(define-vop (move-base-character-argument)
  (:args (x :target y
	    :scs (base-character-reg))
	 (fp :scs (descriptor-reg)
	     :load-if (not (sc-is y base-character-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (base-character-reg
       (move y x))
      (base-character-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-base-character-argument :move-argument
  (any-reg descriptor-reg base-character-reg) (base-character-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-character
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-character-reg) (any-reg descriptor-reg))



;;;; Other operations:

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


;;; Comparison of base-characters.
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

