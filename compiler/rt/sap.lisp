;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/rt/sap.lisp,v 1.6 1991/07/10 17:51:14 ram Exp $
;;;
;;; This file contains the IBM RT VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;

(in-package "RT")



;;;; Moves and coercions:

;;; MOVE-TO-SAP -- VOP.
;;;
;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:generator 1
    (loadw y x vm:sap-pointer-slot vm:other-pointer-type)))
;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; MOVE-FROM-SAP -- VOP.
;;;
;;; Move an untagged SAP to a tagged representation.
;;;
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:sc any-reg) header)
  (:temporary (:sc word-pointer-reg) alloc)
  (:results (y :scs (descriptor-reg)))
  (:generator 1
    (with-fixed-allocation (y header alloc sap-type sap-size)
      (storew sap y sap-pointer-slot other-pointer-type))))
;;;
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))


;;; SAP-MOVE -- VOP.
;;;
;;; Move untagged sap values.
;;;
(define-vop (sap-move)
  (:args (x :target y
	    :scs (sap-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))


;;; MOVE-SAP-ARGUMENT -- VOP.
;;;
;;; Move untagged sap arguments/return-values.
;;;
(define-vop (move-sap-argument)
  (:args (x :target y
	    :scs (sap-reg))
	 (fp :scs (word-pointer-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (sap-reg
       (move y x))
      (sap-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-sap-argument :move-argument
  (descriptor-reg sap-reg) (sap-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged sap to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (sap-reg) (descriptor-reg))



;;;; SAP-INT and INT-SAP

(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe) 
 (:generator 1
    (move int sap)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))



;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg) :target res)
	 (offset :scs (signed-reg) :to :save))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 2
    ;; Can't use CAS since offset and ptr may be register 0, so we have to move.
    (move res ptr)
    (inst a res offset)))
;;;
(define-vop (pointer+-c pointer+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 16)))
  (:generator 1
    (inst a res ptr offset)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg) :target res)
	 (ptr2 :scs (sap-reg) :to :save))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (move res ptr1)
    (inst s res ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(eval-when (compile eval)

;;; DEFINE-SYSTEM-REF -- Internal Interface.
;;;
;;; Name is the name of a computed system-ref offset, from which we generate a
;;; <name>-c VOP for immediate constant offsets.  Shift is the multiples of two
;;; for which we must adjust the offset to make it an index in terms of bytes
;;; (the machines address units).  Translate and signed-translate are the Lisp
;;; function calls for which these VOP's are in-line expansions.
;;;
(defmacro define-system-ref (name shift 
				  translate result-sc result-type
				  &optional
				  signed-translate signed-result-sc
				  signed-result-type)
  (let ((access-form 
	 (ecase shift
	   (0 ;want a byte, indexed in bytes.
	    '((inst lc result base offset)
	      (when signed
		(inst sl result 24)
		(inst sar result 24))))
	   (1 ;Want 16 bits.  Incoming offset is in 16-bit quantities.
	      ;Offset here is in bytes.
	    '((if signed
		  (inst lha result base offset)
		  (inst lh result base offset))))
	   (2 ;Want 32 bits.  Incoming offset is in 32-bit quantities.
	      ;Offset here is in bytes.
	    '(signed ; suppress silly warnings.
	      (inst l result base offset)))))
	(name-c (symbolicate name "-C")))
    `(progn
       (define-vop (,name-c)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (base :scs (sap-reg)))
	 (:results (result :scs (,result-sc)))
	 (:result-types ,result-type)
	 (:arg-types system-area-pointer
		     (:constant (signed-byte ,(- 16 shift))))
	 (:info offset)
	 (:variant-vars signed)
	 (:variant nil)
	 (:generator 5
	   (let ((offset (ash offset ,shift)))
	     ,@access-form)))
       
       (define-vop (,name)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (object :scs (sap-reg) :to (:eval 0))
		(offset :scs (any-reg) :target base))
	 (:results (result :scs (,result-sc)))
	 (:arg-types system-area-pointer positive-fixnum)
	 (:result-types ,result-type)
	 (:temporary (:scs (sap-reg) :from (:argument 1)) base)
	 (:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		     bogus1 bogus2)
	 (:ignore bogus1 bogus2)
	 (:variant-vars signed)
	 (:variant nil)
	 (:generator 7
	   (move base offset)
	   ;;
	   ;; We shift right because the offset has fixnum lowtag.  Effectively
	   ;; the index has already been multiplied by 4.
	   ,@(let ((adj (- 2 shift)))
	       (unless (zerop adj)
		 `((inst sr base ,adj))))
	   (inst a base object)
	   (let ((offset 0))
	     ,@access-form)))
       
       ,@(when signed-translate
	   `((define-vop (,(symbolicate "SIGNED-" name-c) ,name-c)
	       (:translate ,signed-translate)
	       (:results (result :scs (,signed-result-sc)))
	       (:result-types ,signed-result-type)
	       (:variant t))
	     
	     (define-vop (,(symbolicate "SIGNED-" name) ,name)
	       (:translate ,signed-translate)
	       (:results (result :scs (,signed-result-sc)))
	       (:result-types ,signed-result-type)
	       (:variant t)))))))

) ;eval-when

(define-system-ref 8bit-system-ref 0
  sap-ref-8 unsigned-reg positive-fixnum
  signed-sap-ref-8 signed-reg tagged-num)

(define-system-ref 16bit-system-ref 1
  sap-ref-16 unsigned-reg positive-fixnum
  signed-sap-ref-16 signed-reg tagged-num)

(define-system-ref 32bit-system-ref 2
  sap-ref-32 unsigned-reg unsigned-num
  signed-sap-ref-32 signed-reg signed-num)

(define-system-ref sap-system-ref 2
  sap-ref-sap sap-reg system-area-pointer)


(eval-when (compile eval)

;;; DEFINE-SYSTEM-SET -- Internal.
;;;
;;; Name is the name of a computed system-ref offset, from which we generate a
;;; <name>-c VOP for immediate constant offsets.  Shift is the multiples of two
;;; for which we must adjust the offset to make it an index in terms of bytes
;;; (the machines address units).  Translate and signed-translate are the Lisp
;;; function calls for which these VOP's are in-line expansions.
;;;
(defmacro define-system-set (name shift translate data-scs data-type)
  (let ((set-form 
	 (ecase shift
	   (0 ;Want a byte.  Incoming offset is in bytes.  No shifting.
	    '(inst stc data base offset))
	   (1 ;Want 16 bits.  Incoming offset is in 16-bit quantities.
	    ;Offset here is in bytes.
	    '(inst sth data base offset))
	   (2 ;Want 32 bits.  Incoming offset is in 32-bit quantities.
	    ;Offset here is in bytes.
	    '(inst st data base offset))))
	(name-c (symbolicate name "-C")))
    `(progn
       (define-vop (,name-c)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (base :scs (sap-reg))
		(data :scs (,@data-scs) :target result :to (:result 0)))
	 (:arg-types system-area-pointer
		     (:constant (signed-byte ,(- 16 shift)))
		     ,data-type)
	 (:results (result :scs (,@data-scs)))
	 (:result-types ,data-type)
	 (:info offset)
	 (:generator 5
	   (let ((offset (ash offset ,shift)))
	     ,set-form)
	   (move result data)))
       
       (define-vop (,name)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (object :scs (sap-reg) :to (:eval 0))
		(offset :scs (any-reg) :target base)
		(data :scs (,@data-scs) :target result
		      :to (:eval 1)))
	 (:arg-types system-area-pointer positive-fixnum ,data-type)
	 (:temporary (:scs (sap-reg) :from (:argument 1) :to (:eval 2)) base)
	 ;; Add some bullshit temporaries because of human understanding about
	 ;; a peculiarity in compiler register allocation, so this will trick
	 ;; the compiler into giving us enough non-descriptor-regs.
	 ;; Bill did not write this!
	 (:temporary (:scs (non-descriptor-reg) :from (:eval 1) :to (:eval 2))
		     bogus1 bogus2)
	 (:ignore bogus1 bogus2)
	 (:results (result :scs (,@data-scs)))
	 (:result-types ,data-type)
	 (:generator 7
	   (move base offset)
	   ;;
	   ;; We shift right because the offset has fixnum lowtag.  Effectively
	   ;; the index has already been multiplied by 4.
	   ,@(let ((adj (- 2 shift)))
	       (unless (zerop adj)
		 `((inst sr base ,adj))))
	   (inst cas base base object)
	   (let ((offset 0))
	     ,set-form)
	   (move result data))))))

) ;EVAL-WHEN

(define-system-set 8bit-system-set 0 %set-sap-ref-8
  (signed-reg unsigned-reg) (:or signed-num unsigned-num))

(define-system-set 16bit-system-set 1 %set-sap-ref-16
  (signed-reg unsigned-reg) (:or signed-num unsigned-num))

(define-system-set 32bit-system-set 2 %set-sap-ref-32
  (signed-reg unsigned-reg) (:or signed-num unsigned-num))

(define-system-set sap-system-set 2 %set-sap-ref-sap
  (sap-reg) system-area-pointer)



;;;; Noise to convert normal lisp data objects into SAPs.

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst cal sap vector
	  (- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type))))



;;;; ### Noise to allow old forms to continue to work until they are gone.

(macrolet ((frob (prim func)
	     `(def-primitive-translator ,prim (&rest args)
		(warn "Someone used %primitive ~S -- should be ~S."
		      ',prim ',func)
		`(,',func ,@args))))
  (frob 32bit-system-ref sap-ref-32)
  (frob unsigned-32bit-system-ref sap-ref-32)
  (frob 16bit-system-ref sap-ref-16)
  (frob 8bit-system-ref sap-ref-8))

(macrolet ((frob (prim func)
	     `(def-primitive-translator ,prim (&rest args)
		(warn "Someone used %primitive ~S -- should be ~S."
		      ',prim (list 'setf ',func))
		`(setf ,',func ,@args))))
  (frob 32bit-system-set sap-ref-32)
  (frob signed-32bit-system-set sap-ref-32)
  (frob 16bit-system-set sap-ref-16)
  (frob 8bit-system-set sap-ref-8))
