;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/array.lisp,v 1.22 1990/08/02 03:28:43 wlott Exp $
;;;
;;;    This file contains the MIPS definitions for array operations.
;;;
;;; Written by William Lott
;;;
(in-package "C")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:args (type :scs (any-reg descriptor-reg))
	 (rank :scs (any-reg descriptor-reg)))
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic (ndescr)
      (inst addu header alloc-tn vm:other-pointer-type)
      (inst addu alloc-tn
	    (+ (* vm:array-dimensions-offset vm:word-bytes)
	       vm:lowtag-mask))
      (inst addu alloc-tn rank)
      (inst li ndescr (lognot vm:lowtag-mask))
      (inst and alloc-tn ndescr)
      (inst addu ndescr rank (fixnum (1- vm:array-dimensions-offset)))
      (inst sll ndescr ndescr vm:type-bits)
      (inst or ndescr ndescr type)
      (inst srl ndescr ndescr 2)
      (storew ndescr header 0 vm:other-pointer-type))
    (move result header)))


;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t fixnum) fixnum
  (flushable))
(defknown ((setf lisp::%array-dimension))
	  (t fixnum fixnum) fixnum ())

(define-vop (%array-dimension word-index-ref)
  (:translate lisp::%array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))

(define-vop (%set-array-dimension word-index-set)
  (:translate (setf lisp::%array-dimension))
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))



(defknown lisp::%array-rank (t) fixnum (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 vm:other-pointer-type)
    (inst sra temp vm:type-bits)
    (inst subu temp (1- vm:array-dimensions-offset))
    (inst sll res temp 2)))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index)))
      (inst sltu temp index bound)
      (inst beq temp zero-tn error)
      (inst nop)
      (move result index))))



;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
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
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-set)
       (:arg-types ,type positive-fixnum ,element-type)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg zero immediate unsigned-immediate))
	      (value :scs ,scs))
       (:results (result :scs ,scs))
       (:result-types ,element-type))))

(def-data-vector-frobs simple-string byte-index
  base-character base-character-reg)
(def-data-vector-frobs simple-vector word-index
  * descriptor-reg any-reg)

(def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-16 halfword-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-32 word-index
  unsigned-num unsigned-reg)


;;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;; 

(defmacro def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor vm:word-bits bits))
	 (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs (any-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
	 (:generator 20
	   (inst srl temp index ,bit-shift)
	   (inst sll temp 2)
	   (inst add lip object temp)
	   (inst lw result lip
		 (- (* vm:vector-data-offset vm:word-bytes)
		    vm:other-pointer-type))
	   (inst and temp index ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst sll temp ,(1- (integer-length bits)))))
	   (inst srl result temp)
	   (inst and result ,(1- (ash 1 bits)))
	   (inst sll value result 2)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type
		     (:constant
		      (integer 0
			       ,(1- (* (1+ (- (floor (+ #x7fff
							vm:other-pointer-type)
						     vm:word-bytes)
					      vm:vector-data-offset))
				       elements-per-word)))))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (loadw result object (+ word vm:vector-data-offset) 
		    vm:other-pointer-type)
	     (unless (zerop extra)
	       (inst srl result (* extra ,bits)))
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg) :target shift)
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:scs (non-descriptor-reg)) temp old)
	 (:temporary (:scs (non-descriptor-reg) :from :eval) shift)
	 (:generator 25
	   (inst srl temp index ,bit-shift)
	   (inst sll temp 2)
	   (inst add lip object temp)
	   (inst lw old lip
		 (- (* vm:vector-data-offset vm:word-bytes)
		    vm:other-pointer-type))
	   (inst and shift index ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst sll shift ,(1- (integer-length bits)))))
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst li temp ,(1- (ash 1 bits)))
	     (inst sll temp shift)
	     (inst nor temp temp zero-tn)
	     (inst and old temp))
	   (unless (sc-is value zero)
	     (sc-case value
	       (immediate
		(inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
	       (unsigned-reg
		(inst and temp value ,(1- (ash 1 bits)))))
	     (inst sll temp shift)
	     (inst or old temp))
	   (inst sw old lip
		 (- (* vm:vector-data-offset vm:word-bytes)
		    vm:other-pointer-type))
	   (sc-case value
	     (immediate
	      (inst li result (tn-value value)))
	     (zero
	      (move result zero-tn))
	     (unsigned-reg
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type
		     (:constant
		      (integer 0
			       ,(1- (* (1+ (- (floor (+ #x7fff
							vm:other-pointer-type)
						     vm:word-bytes)
					      vm:vector-data-offset))
				       elements-per-word))))
		     positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp old)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (inst lw old object
		   (- (* (+ word vm:vector-data-offset) vm:word-bytes)
		      vm:other-pointer-type))
	     (unless (and (sc-is value immediate)
			  (= (tn-value value) ,(1- (ash 1 bits))))
	       (cond ((= extra ,(1- elements-per-word))
		      (inst sll old ,bits)
		      (inst srl old ,bits))
		     (t
		      (inst li temp
			    (lognot (ash ,(1- (ash 1 bits)) (* extra ,bits))))
		      (inst and old temp))))
	     (sc-case value
	       (zero)
	       (immediate
		(let ((value (ash (logand (tn-value value) ,(1- (ash 1 bits)))
				  (* extra ,bits))))
		  (cond ((< value #x10000)
			 (inst or old value))
			(t
			 (inst li temp value)
			 (inst or old temp)))))
	       (unsigned-reg
		(inst sll temp value (* extra ,bits))
		(inst or old temp)))
	     (inst sw old object
		   (- (* (+ word vm:vector-data-offset) vm:word-bytes)
		      vm:other-pointer-type))
	     (sc-case value
	       (immediate
		(inst li result (tn-value value)))
	       (zero
		(move result zero-tn))
	       (unsigned-reg
		(move result value)))))))))

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)


;;; And the float variants.
;;; 

(define-vop (data-vector-ref/simple-array-single-float)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst add lip object index)
    (inst lwc1 value lip
	  (- (* vm:vector-data-offset vm:word-bytes)
	     vm:other-pointer-type))
    (inst nop)))

(define-vop (data-vector-set/simple-array-single-float)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst add lip object index)
    (inst swc1 value lip
	  (- (* vm:vector-data-offset vm:word-bytes)
	     vm:other-pointer-type))
    (move result value)))

(define-vop (data-vector-ref/simple-array-double-float)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst add lip object index)
    (inst add lip index)
    (inst lwc1 value lip
	  (- (* vm:vector-data-offset vm:word-bytes)
	     vm:other-pointer-type))
    (inst lwc1-odd value lip
	  (+ (- (* vm:vector-data-offset vm:word-bytes)
		vm:other-pointer-type)
	     vm:word-bytes))
    (inst nop)))

(define-vop (data-vector-set/simple-array-double-float)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst add lip object index)
    (inst add lip index)
    (inst swc1 value lip
	  (- (* vm:vector-data-offset vm:word-bytes)
	     vm:other-pointer-type))
    (inst swc1-odd value lip
	  (+ (- (* vm:vector-data-offset vm:word-bytes)
		vm:other-pointer-type)
	     vm:word-bytes))
    (move result value)))



;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;; 

(define-vop (raw-bits word-index-ref)
  (:note "raw-bits VOP")
  (:translate %raw-bits)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))

(define-vop (set-raw-bits word-index-set)
  (:note "setf raw-bits VOP")
  (:translate (setf %raw-bits))
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate unsigned-immediate))
	 (value :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))




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

