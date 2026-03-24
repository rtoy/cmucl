;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; Ported to ARM64 from the SPARC backend.
;;; **********************************************************************
;;;
;;;    This file contains the ARM64 definitions for array operations.
;;;
;;; Originally written by William Lott (SPARC).
;;; Signed-array support by Douglas Crosher 1997.
;;; Complex-float support by Douglas Crosher 1998.
;;; Ported to ARM64.
;;;
;;; ARM64 porting notes:
;;;
;;;   Instruction renames (SPARC -> ARM64):
;;;     SLLN  -> LSL   (logical shift left)
;;;     SRLN  -> LSR   (logical shift right)
;;;     SRA   -> ASR   (arithmetic shift right)
;;;     ANDN  -> BIC   (bit-clear; note operand order inversion)
;;;     LDN   -> LDR   (64-bit load)
;;;     STN   -> STR   (64-bit store)
;;;     LDDF  -> LDR   (double-float load, FP register)
;;;     STDF  -> STR   (double-float store, FP register)
;;;     LDF   -> LDR   (single-float load, FP register)
;;;     STF   -> STR   (single-float store, FP register)
;;;     FMOVS -> FMOV  (single-float register move)
;;;
;;;   Immediate-offset guard: (signed-byte 13) -> (signed-byte 9)
;;;     ARM64 unscaled load/store (LDUR/STUR) takes a 9-bit signed offset.
;;;     When the offset does not fit, materialise into a temp and use
;;;     the register-offset form (reg-offset base temp).
;;;
;;;   pseudo-atomic, allocation, and move helpers are from arm64-macros.lisp.
;;;
(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) gc-temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (emit-not-implemented)
    (pseudo-atomic ()
      ;; ndescr = (rank + (1+ array-dimensions-offset)) * word-bytes,
      ;; rounded down to 8-byte boundary (clear low 3 bits).
      (inst add ndescr rank (* (1+ array-dimensions-offset) vm:word-bytes))
      (inst and ndescr ndescr (lognot 7)) ; round down to 8-byte boundary (clear low 3 bits); AND takes bitmask imm, BIC does not (no immediate form)
      (allocation header ndescr other-pointer-type :temp-tn gc-temp)
      (inst add ndescr rank (fixnumize (1- vm:array-dimensions-offset)))
      (inst lsl ndescr ndescr vm:type-bits)  ; was (inst slln ...)
      (inst orr ndescr ndescr type)          ; was (inst or ...)
      ;; Remove the extraneous fixnum tag bits because TYPE and RANK
      ;; were fixnums.
      (inst lsr ndescr ndescr fixnum-tag-bits) ; was (inst srln ...)
      (storew ndescr header 0 vm:other-pointer-type))
    (move result header)))


;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t fixnum) fixnum
  (flushable))
(defknown lisp::%set-array-dimension (t fixnum fixnum) fixnum
  ())

(define-vop (%array-dimension word64-index-ref)
  (:translate lisp::%array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))

(define-vop (%set-array-dimension word64-index-set)
  (:translate lisp::%set-array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))


(defknown lisp::%array-rank (t) fixnum (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (emit-not-implemented)
    (loadw temp x 0 vm:other-pointer-type)
    (inst asr temp temp vm:type-bits)  ; was (inst sra temp vm:type-bits)
    (inst sub temp temp (1- vm:array-dimensions-offset))
    (inst lsl res temp fixnum-tag-bits))) ; was (inst slln res temp ...)


;;;; Bounds checking routine.

(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (emit-not-implemented)
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index)))
      ;; SPARC: (inst cmp index bound) / (inst b :geu error) / (inst nop)
      ;; ARM64: CMP sets flags; B.HS branches if unsigned >=
      (inst cmp index bound)
      (inst b :cs error)               ; :cs = carry set = unsigned >= (no :hs in ARM64)
      (move result index))))


;;;; Accessors/Setters

;;; Variants built on top of word64-index-ref, etc.  I.e. those vectors whose
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(defmacro def-data-vector-frobs (type variant element-type &rest scs)
  `(progn
     (define-vop (,(intern (concatenate 'simple-string
					"DATA-VECTOR-REF/"
					(string type)))
		  ,(intern (concatenate 'simple-string
					(string variant)
					"-REF")))
       (:note _N"inline array access")
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-ref)
       (:arg-types ,type positive-fixnum)
       (:results (value :scs ,scs))
       (:result-types ,element-type))
     (define-vop (,(intern (concatenate 'simple-string
					"DATA-VECTOR-SET/"
					(string type)))
		  ,(intern (concatenate 'simple-string
					(string variant)
					"-SET")))
       (:note _N"inline array store")
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-set)
       (:arg-types ,type positive-fixnum ,element-type)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg zero immediate))
	      (value :scs ,scs))
       (:results (result :scs ,scs))
       (:result-types ,element-type))))

#-unicode
(def-data-vector-frobs simple-string byte-index
  base-char base-char-reg)
#+unicode
(def-data-vector-frobs simple-string word16-index
  base-char base-char-reg)

(def-data-vector-frobs simple-vector word64-index
  * descriptor-reg any-reg)

(def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-16 word16-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-32 word32-index
  unsigned-num unsigned-reg)

(def-data-vector-frobs simple-array-signed-byte-30 word32-index
  tagged-num any-reg)
(define-vop (data-vector-ref/simple-array-signed-byte-32 signed-word32-index-ref)
  (:note _N"inline array access")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-32 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-32 word32-index-set)
  (:note _N"inline array store")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-32 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))

;;; Integer vectors whose elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;;
;;; The elements are stored in the word with the first element in the
;;; most-significant parts of the word.

(eval-when (compile eval)

(defmacro def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor vm:word-bits bits))
	 (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:note _N"inline array access")
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs (any-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
	 (:generator 20
	   (emit-not-implemented)
	   ;; temp = floor(index, elements-per-word) -> address of containing word.
	   (inst lsr temp index ,bit-shift)     ; was (inst srln temp index ,bit-shift)
	   (inst lsl temp temp fixnum-tag-bits) ; was (inst slln temp fixnum-tag-bits)
	   (inst add temp temp (- (* vm:vector-data-offset vm:word-bytes)
				    vm:other-pointer-type))
	   (inst ldr result (reg-offset object temp)) ; was (inst ldn result object temp)
	   ;; temp = mod(index, elements-per-word): which slot within the word.
	   ;; XOR with (elements-per-word - 1) gives the shift to the MSB-first element.
	   (inst and temp index ,(1- elements-per-word))
	   (inst eor temp temp ,(1- elements-per-word)) ; was (inst xor ...)
	   ,@(unless (= bits 1)
	       `((inst lsl temp temp ,(1- (integer-length bits))))) ; was slln
	   (inst lsr result result temp)        ; was (inst srln result temp)
	   (inst and result result ,(1- (ash 1 bits)))
	   (inst lsl value result vm:fixnum-tag-bits))) ; was (inst slln value ...)
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp)
	 (:generator 15
	   (emit-not-implemented)
	   (multiple-value-bind (word extra)
	       (floor index ,elements-per-word)
	     (setf extra (- ,(1- elements-per-word) extra))
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
	       (cond ((typep offset '(signed-byte 9))
		      (inst ldur result object offset)) ; was (inst ldn result object offset)
		     (t
		      (inst li temp offset)
		      (inst ldr result (reg-offset object temp))))) ; was (inst ldn result object temp)
	     (unless (zerop extra)
	       (inst lsr result result (* ,bits extra))) ; was (inst srln ...)
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note _N"inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg) :target shift)
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp old offset)
	 (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
	 (:generator 25
	   (emit-not-implemented)
	   (inst lsr offset index ,bit-shift)    ; was (inst srln ...)
	   (inst lsl offset offset fixnum-tag-bits) ; was (inst slln ...)
	   (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				       vm:other-pointer-type))
	   (inst ldr old (reg-offset object offset)) ; was (inst ldn old object offset)
	   (inst and shift index ,(1- elements-per-word))
	   (inst eor shift shift ,(1- elements-per-word)) ; was (inst xor ...)
	   ,@(unless (= bits 1)
	       `((inst lsl shift shift ,(1- (integer-length bits))))) ; was slln
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst li temp ,(1- (ash 1 bits)))
	     (inst lsl temp temp shift)      ; was (inst slln temp shift)
	     (inst mvn temp temp)            ; was (inst not temp) -- bitwise NOT
	     (inst and old old temp))
	   (unless (sc-is value zero)
	     (sc-case value
	       (immediate
		(inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
	       (unsigned-reg
		(inst and temp value ,(1- (ash 1 bits)))))
	     (inst lsl temp temp shift)      ; was (inst slln temp shift)
	     (inst orr old old temp))        ; was (inst or ...)
	   (inst str old (reg-offset object offset)) ; was (inst stn old object offset)
	   (sc-case value
	     (immediate
	      (inst li result (tn-value value)))
	     (t
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type
		     (:constant index)
		     positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) offset-reg temp old)
	 (:generator 20
	   (emit-not-implemented)
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
	       (cond ((typep offset '(signed-byte 9))
		      (inst ldur old object offset)) ; was (inst ldn old object offset)
		     (t
		      (inst li offset-reg offset)
		      (inst ldr old (reg-offset object offset-reg)))) ; was ldn
	       (unless (and (sc-is value immediate)
			    (= (tn-value value) ,(1- (ash 1 bits))))
		 (cond ((zerop extra)
			(inst lsl old old ,bits)  ; was (inst slln ...)
			(inst lsr old old ,bits)) ; was (inst srln ...)
		       (t
			(inst li temp
			      (lognot (ash ,(1- (ash 1 bits))
					   (* (logxor extra
						      ,(1- elements-per-word))
					      ,bits))))
			(inst and old old temp))))
	       (sc-case value
		 (zero)
		 (immediate
		  (let ((value (ash (logand (tn-value value)
					    ,(1- (ash 1 bits)))
				    (* (logxor extra
					       ,(1- elements-per-word))
				       ,bits))))
		    (cond ((typep value '(signed-byte 13))
			   (inst orr old old value)) ; was (inst or old value)
			  (t
			   (inst li temp value)
			   (inst orr old old temp))))) ; was (inst or old temp)
		 (unsigned-reg
		  (inst lsl temp value              ; was (inst slln temp value ...)
			(* (logxor extra ,(1- elements-per-word)) ,bits))
		  (inst orr old old temp)))         ; was (inst or ...)
	       (if (typep offset '(signed-byte 9))
		   (inst stur old object offset)            ; was (inst stn old object offset)
		   (inst str old (reg-offset object offset-reg)))) ; was ldn
	     (sc-case value
	       (immediate
		(inst li result (tn-value value)))
	       (t
		(move result value)))))))))

); eval-when (compile eval)

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)


;;; And the float variants.

(define-vop (data-vector-ref/simple-array-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types single-float)
  (:generator 5
    (emit-not-implemented)
    ;; Single floats are 4 bytes; index is a tagged fixnum (tag-bits = 2),
    ;; so the raw byte shift is (- 2 fixnum-tag-bits) = 0 -> no shift needed.
    ;; ARM64: ADD offset, index, #(base-offset)  then LDR (FP) value, [object, offset]
    (inst add offset index (- (* vm:vector-data-offset vm:word-bytes)
			       vm:other-pointer-type))
    (inst ldr value (reg-offset object offset)))) ; was (inst ldf value object offset)

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-array-single-float (:constant index))
  (:info index)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:result-types single-float)
  (:generator 3
    (emit-not-implemented)
    (let ((offset (+ (fixnumize index)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (if (typep offset '(signed-byte 9))
	  (inst ldur value object offset)   ; was (inst ldf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst ldr value (reg-offset object temp))))))) ; was (inst ldf ...)

(define-vop (data-vector-set/simple-array-single-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (emit-not-implemented)
    (inst add offset index
	  (- (* vm:vector-data-offset vm:word-bytes)
	     vm:other-pointer-type))
    (inst str value (reg-offset object offset)) ; was (inst stf value object offset)
    (unless (location= result value)
      (inst fmov result value))))               ; was (inst fmovs ...)

(define-vop (data-vector-set-c/simple-array-single-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float
	      (:constant index)
	      single-float)
  (:info index)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (emit-not-implemented)
    (let ((offset (+ (fixnumize index)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (if (typep offset '(signed-byte 9))
	  (inst stur value object offset)   ; was (inst stf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst str value (reg-offset object temp)))) ; was (inst stf ...)
      (unless (location= result value)
	(inst fmov result value)))))        ; was (inst fmovs ...)

(define-vop (data-vector-ref/simple-array-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 7
    (emit-not-implemented)
    ;; Double floats are 8 bytes; shift = (- vm:word-shift fixnum-tag-bits) = 1 -> LSL #1.
    (inst lsl offset index (- vm:word-shift fixnum-tag-bits)) ; was (inst slln ...)
    (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
			       vm:other-pointer-type))
    (inst ldr value (reg-offset object offset)))) ; was (inst lddf value object offset)

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-array-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 3
    (emit-not-implemented)
    (let ((offset (+ (* index double-float-bytes)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (if (typep offset '(signed-byte 9))
	  (inst ldur value object offset)   ; was (inst lddf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst ldr value (reg-offset object temp)))))))

(define-vop (data-vector-set/simple-array-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 20
    (emit-not-implemented)
    (inst lsl offset index (- vm:word-shift fixnum-tag-bits)) ; was (inst slln ...)
    (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
			       vm:other-pointer-type))
    (inst str value (reg-offset object offset))  ; was (inst stdf value object offset)
    (unless (location= result value)
      (move-double-reg result value))))

(define-vop (data-vector-set-c/simple-array-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float
	      (:constant index)
	      double-float)
  (:info index)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 10
    (emit-not-implemented)
    (let ((offset (+ (* index double-float-bytes)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (if (typep offset '(signed-byte 9))
	  (inst stur value object offset)   ; was (inst stdf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst str value (reg-offset object temp)))) ; was (inst stdf ...)
      (unless (location= result value)
	(move-double-reg result value)))))



;;;; Misc. Array VOPs.

#+nil
(define-vop (vector-word-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec clc::g-vector-header-words)
    (inst niuo res res clc::g-vector-words-mask-16)))

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))


;;;
(define-vop (data-vector-ref/simple-array-signed-byte-8 signed-byte-index-ref)
  (:note _N"inline array access")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-8 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-8 byte-index-set)
  (:note _N"inline array store")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-8 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


(define-vop (data-vector-ref/simple-array-signed-byte-16
	     signed-word16-index-ref)
  (:note _N"inline array access")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-16 word16-index-set)
  (:note _N"inline array store")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (emit-not-implemented)
    ;; Complex single float: 8 bytes total; shift = (- vm:word-shift fixnum-tag-bits) = 1.
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst lsl offset index (- vm:word-shift fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst ldr real-tn (reg-offset object offset))) ; was (inst ldf real-tn object offset)
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst add offset offset single-float-bytes)
      (inst ldr imag-tn (reg-offset object offset))))) ; was (inst ldf imag-tn object offset)

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-single-float
	      (:constant index))
  (:info index)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:result-types complex-single-float)
  (:generator 3
    (emit-not-implemented)
    (let ((offset (+ (* index (* 2 single-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (real-tn (complex-single-reg-real-tn value))
	  (imag-tn (complex-single-reg-imag-tn value)))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset single-float-bytes) '(signed-byte 9))
	     (inst ldur real-tn object offset) ; was (inst ldf ...)
	     (inst ldur imag-tn object (+ offset single-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst ldr real-tn (reg-offset object temp)) ; was (inst ldf real-tn object temp)
	     (inst add temp temp single-float-bytes)
	     (inst ldr imag-tn (reg-offset object temp))))))) ; was (inst ldf ...)

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (emit-not-implemented)
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (inst lsl offset index (- vm:word-shift fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst str value-real (reg-offset object offset)) ; was (inst stf ...)
      (unless (location= result-real value-real)
	(inst fmov result-real value-real)))               ; was (inst fmovs ...)
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst add offset offset single-float-bytes)
      (inst str value-imag (reg-offset object offset)) ; was (inst stf ...)
      (unless (location= result-imag value-imag)
	(inst fmov result-imag value-imag)))))             ; was (inst fmovs ...)

(define-vop (data-vector-set-c/simple-array-complex-single-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float
	      (:constant index)
	      complex-single-float)
  (:info index)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 3
    (emit-not-implemented)
    (let ((offset (+ (* index (* 2 single-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result))
	  (value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset single-float-bytes) '(signed-byte 9))
	     (inst stur value-real object offset) ; was (inst stf ...)
	     (inst stur value-imag object (+ offset single-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst str value-real (reg-offset object temp)) ; was (inst stf ...)
	     (inst add temp temp single-float-bytes)
	     (inst str value-imag (reg-offset object temp)))) ; was (inst stf ...)
      (unless (location= result-real value-real)
	(inst fmov result-real value-real))  ; was (inst fmovs ...)
      (unless (location= result-imag value-imag)
	(inst fmov result-imag value-imag))))) ; was (inst fmovs ...)

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (emit-not-implemented)
    ;; Complex double float: 16 bytes; shift = (- (+ vm:word-shift 1) fixnum-tag-bits) = 2.
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst lsl offset index (- (+ vm:word-shift 1) fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst ldr real-tn (reg-offset object offset))) ; was (inst lddf ...)
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst add offset offset double-float-bytes)
      (inst ldr imag-tn (reg-offset object offset))))) ; was (inst lddf ...)

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 5
    (emit-not-implemented)
    (let ((offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (real-tn (complex-double-reg-real-tn value))
	  (imag-tn (complex-double-reg-imag-tn value)))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset double-float-bytes) '(signed-byte 9))
	     (inst ldur real-tn object offset) ; was (inst lddf ...)
	     (inst ldur imag-tn object (+ offset double-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst ldr real-tn (reg-offset object temp)) ; was (inst lddf ...)
	     (inst add temp temp double-float-bytes)
	     (inst ldr imag-tn (reg-offset object temp))))))) ; was (inst lddf ...)

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (emit-not-implemented)
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (inst lsl offset index (- (+ vm:word-shift 1) fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst str value-real (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst add offset offset double-float-bytes)
      (inst str value-imag (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))

(define-vop (data-vector-set-c/simple-array-complex-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float
	      (:constant index)
	      complex-double-float)
  (:info index)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 15
    (emit-not-implemented)
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result))
	  (value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result))
	  (offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      ;; (No optimization for real-fits-9 but imag-does-not -- same as SPARC.)
      (cond ((typep (+ offset double-float-bytes) '(signed-byte 9))
	     (inst stur value-real object offset) ; was (inst stdf ...)
	     (inst stur value-imag object (+ offset double-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst str value-real (reg-offset object temp)) ; was (inst stdf ...)
	     (inst add temp temp double-float-bytes)
	     (inst str value-imag (reg-offset object temp)))) ; was (inst stdf ...)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real))
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))


;;;
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum single-float))
;;;
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum double-float))

;; Versions with constant offsets
(define-vop (raw-ref-single-c data-vector-ref-c/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant index)))
;;;
(define-vop (raw-set-single-c data-vector-set-c/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant index) single-float))
(define-vop (raw-ref-double-c data-vector-ref-c/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant index)))

(define-vop (raw-set-double-c data-vector-set-c/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant index) double-float))



(define-vop (raw-ref-complex-single
	     data-vector-ref/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-single
	     data-vector-set/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-single-float))
;;;
(define-vop (raw-ref-complex-double
	     data-vector-ref/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-double
	     data-vector-set/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-double-float))

(define-vop (raw-ref-complex-single-c
	     data-vector-ref-c/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant index)))
;;;
(define-vop (raw-set-complex-single-c
	     data-vector-set-c/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant index)
	      complex-single-float))
;;;
(define-vop (raw-ref-complex-double-c
	     data-vector-ref-c/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant index)))
;;;
(define-vop (raw-set-complex-double-c
	     data-vector-set-c/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant index)
	      complex-double-float))



;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.

(define-vop (raw-bits word64-index-ref)
  (:note _N"raw-bits VOP")
  (:translate %raw-bits)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))

(define-vop (set-raw-bits word64-index-set)
  (:note _N"setf raw-bits VOP")
  (:translate %set-raw-bits)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (unsigned-reg)))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))


#+double-double
(progn
(define-vop (data-vector-ref/simple-array-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-double-float positive-fixnum)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (emit-not-implemented)
    ;; Double-double float: 16 bytes; shift = (- (+ vm:word-shift 1) fixnum-tag-bits) = 2.
    (let ((hi-tn (double-double-reg-hi-tn value)))
      (inst lsl offset index (- (+ vm:word-shift 1) fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst ldr hi-tn (reg-offset object offset))) ; was (inst lddf ...)
    (let ((lo-tn (double-double-reg-lo-tn value)))
      (inst add offset offset double-float-bytes)
      (inst ldr lo-tn (reg-offset object offset))))) ; was (inst lddf ...)

(define-vop (data-vector-ref-c/simple-array-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 5
    (emit-not-implemented)
    (let ((offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (hi-tn (double-double-reg-hi-tn value))
	  (lo-tn (double-double-reg-lo-tn value)))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset double-float-bytes) '(signed-byte 9))
	     (inst ldur hi-tn object offset) ; was (inst lddf ...)
	     (inst ldur lo-tn object (+ offset double-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst ldr hi-tn (reg-offset object temp)) ; was (inst lddf ...)
	     (inst add temp temp double-float-bytes)
	     (inst ldr lo-tn (reg-offset object temp))))))) ; was (inst lddf ...)

(define-vop (data-vector-set/simple-array-double-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (double-double-reg) :target result))
  (:arg-types simple-array-double-double-float positive-fixnum
	      double-double-float)
  (:results (result :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (emit-not-implemented)
    (let ((value-hi (double-double-reg-hi-tn value))
	  (result-hi (double-double-reg-hi-tn result)))
      (inst lsl offset index (- (+ vm:word-shift 1) fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst str value-hi (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-hi value-hi)
	(move-double-reg result-hi value-hi)))
    (let ((value-lo (double-double-reg-lo-tn value))
	  (result-lo (double-double-reg-lo-tn result)))
      (inst add offset offset double-float-bytes)
      (inst str value-lo (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-lo value-lo)
	(move-double-reg result-lo value-lo)))))

(define-vop (data-vector-set-c/simple-array-double-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (double-double-reg) :target result))
  (:arg-types simple-array-double-double-float
	      (:constant index)
	      double-double-float)
  (:info index)
  (:results (result :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 15
    (emit-not-implemented)
    (let ((value-hi (double-double-reg-hi-tn value))
	  (result-hi (double-double-reg-hi-tn result))
	  (value-lo (double-double-reg-lo-tn value))
	  (result-lo (double-double-reg-lo-tn result))
	  (offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset double-float-bytes) '(signed-byte 9))
	     (inst stur value-hi object offset) ; was (inst stdf ...)
	     (inst stur value-lo object (+ offset double-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst str value-hi (reg-offset object temp)) ; was (inst stdf ...)
	     (inst add temp temp double-float-bytes)
	     (inst str value-lo (reg-offset object temp)))) ; was (inst stdf ...)
      (unless (location= result-hi value-hi)
	(move-double-reg result-hi value-hi))
      (unless (location= result-lo value-lo)
	(move-double-reg result-lo value-lo)))))

(define-vop (data-vector-ref/simple-array-complex-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-double-float positive-fixnum)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (emit-not-implemented)
    ;; Complex double-double: 32 bytes; shift = (- (+ vm:word-shift 2) fixnum-tag-bits) = 3.
    (let ((real-tn (complex-double-double-reg-real-hi-tn value)))
      (inst lsl offset index (- (+ vm:word-shift 2) fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst ldr real-tn (reg-offset object offset))) ; was (inst lddf ...)
    (let ((real-tn (complex-double-double-reg-real-lo-tn value)))
      (inst add offset offset double-float-bytes)
      (inst ldr real-tn (reg-offset object offset))) ; was (inst lddf ...)
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn value)))
      (inst add offset offset double-float-bytes)
      (inst ldr imag-tn (reg-offset object offset))) ; was (inst lddf ...)
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn value)))
      (inst add offset offset double-float-bytes)
      (inst ldr imag-tn (reg-offset object offset))))) ; was (inst lddf ...)

(define-vop (data-vector-ref-c/simple-array-complex-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 5
    (emit-not-implemented)
    (let ((offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (real-hi-tn (complex-double-double-reg-real-hi-tn value))
	  (imag-hi-tn (complex-double-double-reg-imag-hi-tn value))
	  (real-lo-tn (complex-double-double-reg-real-lo-tn value))
	  (imag-lo-tn (complex-double-double-reg-imag-lo-tn value)))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset (* 3 double-float-bytes)) '(signed-byte 9))
	     (inst ldur real-hi-tn object offset) ; was (inst lddf ...)
	     (inst ldur real-lo-tn object (+ offset double-float-bytes))
	     (inst ldur imag-hi-tn object (+ offset (* 2 double-float-bytes)))
	     (inst ldur imag-lo-tn object (+ offset (* 3 double-float-bytes))))
	    (t
	     (inst li temp offset)
	     (inst ldr real-hi-tn (reg-offset object temp)) ; was (inst lddf ...)
	     (inst add temp temp double-float-bytes)
	     (inst ldr real-lo-tn (reg-offset object temp))
	     (inst add temp temp double-float-bytes)
	     (inst ldr imag-hi-tn (reg-offset object temp))
	     (inst add temp temp double-float-bytes)
	     (inst ldr imag-lo-tn (reg-offset object temp))))))) ; was (inst lddf ...)

(define-vop (data-vector-set/simple-array-complex-double-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-double-double-reg) :target result))
  (:arg-types simple-array-complex-double-double-float positive-fixnum
	      complex-double-double-float)
  (:results (result :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (emit-not-implemented)
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result)))
      (inst lsl offset index (- (+ vm:word-shift 2) fixnum-tag-bits)) ; was (inst slln ...)
      (inst add offset offset (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))
      (inst str value-real (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real)))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result)))
      (inst add offset offset double-float-bytes)
      (inst str value-real (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real)))
    (let ((value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result)))
      (inst add offset offset double-float-bytes)
      (inst str value-imag (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))
    (let ((value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result)))
      (inst add offset offset double-float-bytes)
      (inst str value-imag (reg-offset object offset)) ; was (inst stdf ...)
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))

(define-vop (data-vector-set-c/simple-array-complex-double-double-float)
  (:note _N"inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (complex-double-double-reg) :target result))
  (:arg-types simple-array-complex-double-double-float
	      (:constant index)
	      complex-double-double-float)
  (:info index)
  (:results (result :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 15
    (emit-not-implemented)
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result))
	  (value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result))
	  (offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset double-float-bytes) '(signed-byte 9))
	     (inst stur value-real object offset) ; was (inst stdf ...)
	     (inst stur value-imag object (+ offset double-float-bytes)))
	    (t
	     (inst li temp offset)
	     (inst str value-real (reg-offset object temp)) ; was (inst stdf ...)
	     (inst add temp temp double-float-bytes)
	     (inst str value-imag (reg-offset object temp)))) ; was (inst stdf ...)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real))
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result))
	  (value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result))
	  (offset (+ (* index (* 2 double-float-bytes))
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      ;; ARM64: immediate-offset guard is (signed-byte 9), not 13.
      (cond ((typep (+ offset (* 3 double-float-bytes)) '(signed-byte 9))
	     (inst stur value-real object (+ offset (* 2 double-float-bytes)))
	     (inst stur value-imag object (+ offset (* 3 double-float-bytes))))
	    (t
	     (inst add temp temp double-float-bytes)
	     (inst str value-real (reg-offset object temp)) ; was (inst stdf ...)
	     (inst add temp temp double-float-bytes)
	     (inst str value-imag (reg-offset object temp)))) ; was (inst stdf ...)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real))
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))

)
