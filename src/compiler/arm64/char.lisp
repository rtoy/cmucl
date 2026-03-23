;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/char.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the ARM64 VM definition of character operations.
;;;
;;; Written by Raymond Toy.
;;; Derived from the SPARC port by William Lott.
;;;
(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")



;;;; Moves and coercions:

;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-char)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (base-char-reg)))
  (:note _N"character untagging")
  (:generator 1
    (emit-not-implemented)
    (inst lsr y x vm:type-bits)))
;;;
(define-move-vop move-to-base-char :move
  (any-reg descriptor-reg) (base-char-reg))


;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-char)
  (:args (x :scs (base-char-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note _N"character tagging")
  (:generator 1
    (emit-not-implemented)
    (inst lsl y x vm:type-bits)
    (inst orr y y vm:base-char-type)))
;;;
(define-move-vop move-from-base-char :move
  (base-char-reg) (any-reg descriptor-reg))

;;; Move untagged base-char values.
;;;
(define-vop (base-char-move)
  (:args (x :target y
	    :scs (base-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-char-reg)
	       :load-if (not (location= x y))))
  (:note _N"character move")
  (:effects)
  (:affected)
  (:generator 0
    (emit-not-implemented)
    (move y x)))
;;;
(define-move-vop base-char-move :move
  (base-char-reg) (base-char-reg))


;;; Move untagged base-char arguments/return-values.
;;;
(define-vop (move-base-char-argument)
  (:args (x :target y
	    :scs (base-char-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y base-char-reg))))
  (:results (y))
  (:temporary (:sc non-descriptor-reg) temp)
  (:note _N"character arg move")
  (:generator 0
    (emit-not-implemented)
    (sc-case y
      (base-char-reg
       (move y x))
      (base-char-stack
       (storew x fp (tn-offset y) 0 temp)))))
;;;
(define-move-vop move-base-char-argument :move-argument
  (any-reg base-char-reg) (base-char-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-char
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-char-reg) (any-reg descriptor-reg))



;;;; Other operations:

(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-char-reg) :target res))
  (:arg-types base-char)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (emit-not-implemented)
    (inst lsl res ch fixnum-tag-bits)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 1
    (emit-not-implemented)
    (inst lsr res code fixnum-tag-bits)))


;;; Comparison of base-chars.
;;;
;;; ARM64 branch conditions for unsigned character ordering:
;;;   :lo  = unsigned less-than    (SPARC :ltu)
;;;   :hs  = unsigned >=           (SPARC :geu)
;;;   :hi  = unsigned greater-than (SPARC :gtu)
;;;   :ls  = unsigned <=           (SPARC :leu)
;;;   :eq  = equal
;;;   :ne  = not equal
;;;
;;; Characters are always non-negative integers so unsigned conditions
;;; are correct and consistent with the SPARC port.
;;;
(define-vop (base-char-compare)
  (:args (x :scs (base-char-reg))
	 (y :scs (base-char-reg)))
  (:arg-types base-char base-char)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note _N"inline comparison")
  (:variant-vars condition not-condition)
  (:generator 3
    (emit-not-implemented)
    (inst cmp x y)
    (inst b (if not-p not-condition condition) target)))

(define-vop (fast-char=/base-char base-char-compare)
  (:translate char=)
  (:variant :eq :ne))

(define-vop (fast-char</base-char base-char-compare)
  (:translate char<)
  (:variant :lo :hs))

(define-vop (fast-char>/base-char base-char-compare)
  (:translate char>)
  (:variant :hi :ls))

;;; Comparison against a compile-time constant character.
;;;
;;; ARM64 CMP accepts only a 12-bit unsigned immediate (0-4095), but
;;; char-code-limit is 65536.  MOVZ handles a full 16-bit unsigned
;;; immediate, covering the entire valid char-code range in one instruction.
;;;
(define-vop (base-char-compare-c)
  (:args (x :scs (base-char-reg)))
  (:arg-types base-char (:constant base-char))
  (:conditional)
  (:info target not-p y)
  (:policy :fast-safe)
  (:note _N"inline comparison")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars condition not-condition)
  (:generator 2
    (emit-not-implemented)
    (inst movz temp (char-code y))
    (inst cmp x temp)
    (inst b (if not-p not-condition condition) target)))

(define-vop (fast-char=-c/base-char base-char-compare-c)
  (:translate char=)
  (:variant :eq :ne))

(define-vop (fast-char<-c/base-char base-char-compare-c)
  (:translate char<)
  (:variant :lo :hs))

(define-vop (fast-char>-c/base-char base-char-compare-c)
  (:translate char>)
  (:variant :hi :ls))
