;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/sap.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the ARM VM definition of SAP operations.
;;;
;;;
(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")


;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note _N"pointer to SAP coercion")
  (:generator 1
    (emit-not-implemented)
    (loadw y x sap-pointer-slot other-pointer-type)))

;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; Move an untagged SAP to a tagged representation.
;;;
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (res :scs (descriptor-reg)))
  (:note _N"SAP to pointer coercion") 
  (:generator 20
    (emit-not-implemented)
    (with-fixed-allocation (res ndescr sap-type sap-size)
      (storew sap res sap-pointer-slot other-pointer-type))))
;;;
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))


;;; Move untagged sap values.
;;;
(define-vop (sap-move)
  (:args (x :target y
	    :scs (sap-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
	       :load-if (not (location= x y))))
  (:note _N"SAP move")
  (:effects)
  (:affected)
  (:generator 0
    (emit-not-implemented)
    (move y x)))
;;;
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))


;;; Move untagged sap arguments/return-values.
;;;
(define-vop (move-sap-argument)
  (:args (x :target y
	    :scs (sap-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note _N"SAP argument move")
  (:generator 0
    (emit-not-implemented)
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
    (emit-not-implemented)
    (move int sap)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (move sap int)))



;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 2
    (emit-not-implemented)
    (inst add res ptr offset)))

(define-vop (pointer+-c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (unsigned-byte 8)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (emit-not-implemented)
    (inst add res ptr offset)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
	 (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (emit-not-implemented)
    (inst sub res ptr1 ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(eval-when (compile eval)

(defmacro def-system-ref-and-set
	  (ref-name set-name sc type size &optional signed)
  (let ((ref-name-c (symbolicate ref-name "-C"))
	(set-name-c (symbolicate set-name "-C"))
	(const-type '(arm-signed-offset 4095)))
    (macrolet ((load-inst (size)
		 `(ecase ,size
		    (:byte (if signed 'ldrsb 'ldrb))
		    (:short (if signed 'ldrsh 'ldrh))
		    (:long 'ldr)))
	       (store-inst (size)
		 `(ecase ,size
		    (:byte 'strb)
		    (:short 'strh)
		    (:long 'str))))
      `(progn
	 (define-vop (,ref-name)
	   (:translate ,ref-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg))
		  (offset :scs (signed-reg)))
	   (:arg-types system-area-pointer signed-num)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:generator 5
	     (emit-not-implemented)
	     (inst ,(load-inst size) result sap offset)))
	 (define-vop (,ref-name-c)
	   (:translate ,ref-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg)))
	   (:arg-types system-area-pointer (:constant ,const-type))
	   (:info offset)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:generator 4
	     (emit-not-implemented)
	     (inst ,(load-inst size) result sap offset)))
	 (define-vop (,set-name)
	   (:translate ,set-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg))
		  (offset :scs (signed-reg))
		  (value :scs (,sc) :target result))
	   (:arg-types system-area-pointer signed-num ,type)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:generator 5
	     (emit-not-implemented)
	     (inst ,(store-inst size) value sap offset)
	     (unless (location= result value)
	       (inst mov result value))))
	 (define-vop (,set-name-c)
	   (:translate ,set-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg))
		  (value :scs (,sc) :target result))
	   (:arg-types system-area-pointer (:constant ,const-type) ,type)
	   (:info offset)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:generator 4
	     (emit-not-implemented)
	     (inst ,(store-inst size) value sap offset)
	     (unless (location= result value)
	       (inst mov result value))))))))

(defmacro def-system-float-ref-and-set
	  (ref-name set-name sc type size)
  (let ((ref-name-c (symbolicate ref-name "-C"))
	(set-name-c (symbolicate set-name "-C"))
	(const-type '(arm-signed-offset 255)))
    (macrolet ((move-reg (size)
		 `(case ,size
		    (:single
		     '(inst vmov result value))
		    (:double
		     '(move-double-reg result value)))))
      `(progn
	 (define-vop (,ref-name)
	   (:translate ,ref-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg))
		  (offset :scs (signed-reg)))
	   (:arg-types system-area-pointer signed-num)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:temporary (:scs (non-descriptor-reg)) addr)
	   (:generator 5
	     (emit-not-implemented)
	     (inst add addr sap offset)
	     (inst vldr result addr 0)))
	 (define-vop (,ref-name-c)
	   (:translate ,ref-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg)))
	   (:arg-types system-area-pointer (:constant ,const-type))
	   (:info offset)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:generator 4
	     (emit-not-implemented)
	     (inst vldr result sap offset)))
	 (define-vop (,set-name)
	   (:translate ,set-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg))
		  (offset :scs (signed-reg))
		  (value :scs (,sc) :target result))
	   (:arg-types system-area-pointer signed-num ,type)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:temporary (:scs (non-descriptor-reg)) addr)
	   (:generator 5
	     (emit-not-implemented)
	     (inst add addr sap offset)
	     (inst vstr value addr 0)
	     (unless (location= result value)
	       (inst vmov result value))))
	 (define-vop (,set-name-c)
	   (:translate ,set-name)
	   (:policy :fast-safe)
	   (:args (sap :scs (sap-reg))
		  (value :scs (,sc) :target result))
	   (:arg-types system-area-pointer (:constant ,const-type) ,type)
	   (:info offset)
	   (:results (result :scs (,sc)))
	   (:result-types ,type)
	   (:generator 4
	     (emit-not-implemented)
	     (inst vstr value sap offset)
	     (unless (location= result value)
	       (inst vmov result value))))))))

); eval-when (compile eval)

(def-system-ref-and-set sap-ref-8 %set-sap-ref-8
  unsigned-reg positive-fixnum :byte nil)
(def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
  signed-reg tagged-num :byte t)
(def-system-ref-and-set sap-ref-16 %set-sap-ref-16
  unsigned-reg positive-fixnum :short nil)
(def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
  signed-reg tagged-num :short t)
(def-system-ref-and-set sap-ref-32 %set-sap-ref-32
  unsigned-reg unsigned-num :long nil)
(def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
  signed-reg signed-num :long t)
(def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
  sap-reg system-area-pointer :long)

(def-system-float-ref-and-set sap-ref-single %set-sap-ref-single
  single-reg single-float :single)
(def-system-float-ref-and-set sap-ref-double %set-sap-ref-double
  double-reg double-float :double)



;;; Noise to convert normal lisp data objects into SAPs.

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (emit-not-implemented)
    (inst add sap vector
	  (- (* vector-data-offset word-bytes) other-pointer-type))))



;;; Transforms for 64-bit SAP accessors.

(deftransform sap-ref-64 ((sap offset) (* *))
  '(logior (ash (sap-ref-32 sap offset) 32)
	   (sap-ref-32 sap (+ offset 4))))

(deftransform signed-sap-ref-64 ((sap offset) (* *))
  '(logior (ash (signed-sap-ref-32 sap offset) 32)
	   (sap-ref-32 sap (+ 4 offset))))

(deftransform %set-sap-ref-64 ((sap offset value) (* * *))
  '(progn
     (%set-sap-ref-32 sap offset (ash value -32))
     (%set-sap-ref-32 sap (+ offset 4) (logand value #xffffffff))))

(deftransform %set-signed-sap-ref-64 ((sap offset value) (* * *))
  '(progn
     (%set-signed-sap-ref-32 sap offset (ash value -32))
     (%set-sap-ref-32 sap (+ 4 offset) (logand value #xffffffff))))
