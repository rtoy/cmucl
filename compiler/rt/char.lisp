;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/rt/char.lisp,v 1.1 1991/02/18 15:07:48 chiles Exp $
;;; 
;;; This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan and Bill Chiles.
;;;

(in-package "RT")



;;;; Moves and coercions:

;;; MOVE-TO-BASE-CHARACTER -- VOP.
;;;
;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-character)
  (:args (x :scs (any-reg descriptor-reg) :target y))
  (:arg-types base-character)
  (:results (y :scs (base-character-reg)))
  (:generator 1
    (move y x)
    (inst sr y vm:type-bits)))
;;;
(define-move-vop move-to-base-character :move
  (any-reg descriptor-reg) (base-character-reg))


;;; MOVE-FROM-BASE-CHARACTER -- VOP.
;;;
;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-character)
  (:args (x :scs (base-character-reg) :target temp))
  (:temporary (:scs (base-character-reg) :from (:argument 0)) temp)
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types base-character)
  (:generator 1
    (move temp x)
    (inst sl temp vm:type-bits)
    (inst oil y temp vm:base-character-type)))
;;;
(define-move-vop move-from-base-character :move
  (base-character-reg) (any-reg descriptor-reg))

;;; BASE-CHARACTER-MOVE -- VOP.
;;;
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


;;; MOVE-BASE-CHARACTER-ARGUMENT -- VOP.
;;;
;;; Move untagged base-character arguments/return-values.
;;;
(define-vop (move-base-character-argument)
  (:args (x :target y
	    :scs (base-character-reg))
	 (fp :scs (word-pointer-reg)
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
  (any-reg base-character-reg) (base-character-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-character
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-character-reg) (any-reg descriptor-reg))



;;;; Other operations:

;;; CHAR-CODE -- VOP.
;;;
;;; This assumes it is best to keep characters in their raw representation.
;;;
(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-character-reg) :target temp))
  (:arg-types base-character)
  (:temporary (:scs (base-character-reg)
		    :from (:argument 0) :to (:result 0)
		    :target res) temp)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (move temp ch)
    (inst sl temp 2)
    (move res temp)))

;;; CODE-CHAR -- VOP.
;;;
(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (base-character-reg)))
  (:result-types base-character)
  (:generator 1
    (move res code)
    (inst sr res 2)))



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
