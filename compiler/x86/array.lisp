;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/array.lisp,v 1.3 1997/03/25 16:59:26 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the x86 definitions for array operations.
;;;
;;; Written by William Lott
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997.
;;;
(in-package :x86)


;;;; Allocator for the array header.


(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg) bytes)
  (:temporary (:sc any-reg) header)
  (:temporary (:sc any-reg) alloc)
  (:results (result :scs (descriptor-reg)))
  (:generator 13
    (inst lea bytes
	  (make-ea :dword :base rank
		   :disp (+ (* (1+ array-dimensions-offset) word-bytes)
			    lowtag-mask)))
    (inst and bytes (lognot lowtag-mask))
    (inst lea header (make-ea :dword :base rank
			      :disp (fixnum (1- array-dimensions-offset))))
    (inst shl header type-bits)
    (inst or  header type)
    (inst shr header 2)
    #-cgc
    (with-allocation (alloc)
      (inst lea result (make-ea :dword :base alloc :disp other-pointer-type))
      (inst add alloc bytes))
    #+cgc
    (with-cgc-allocation (alloc bytes)
      (inst lea result (make-ea :dword :base alloc :disp other-pointer-type)))
    (storew header result 0 other-pointer-type)))


;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t index) index
  (flushable))
(defknown lisp::%set-array-dimension (t index index) index
  ())

(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-type
  (any-reg) positive-fixnum lisp::%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-type
  (any-reg) positive-fixnum lisp::%set-array-dimension)


(defknown lisp::%array-rank (t) index (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-type)
    (inst shr res type-bits)
    (inst sub res (1- array-dimensions-offset))))



;;;; Bounds checking routine.

(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array)
	 (bound :scs (any-reg))
	 (index :scs (any-reg immediate) :target result))
  (:arg-types * positive-fixnum tagged-num)
  (:results (result :scs (any-reg descriptor-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index))
	  (index (if (sc-is index immediate) (fixnum (tn-value index)) index)))
      (inst cmp bound index)
      ;; We use below-or-equal even though it's an unsigned test, because
      ;; negative indexes appear as real large unsigned numbers.  Therefore,
      ;; we get the < 0 and >= bound test all rolled into one.
      (inst jmp :be error)
      (unless (and (tn-p index) (location= result index))
        (inst mov result index)))))


;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(eval-when (compile eval)

(defmacro def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-type ,scs ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-type ,scs ,element-type
       data-vector-set)))

); eval-when (compile eval)

(def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)


(def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
  unsigned-reg)

;;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;; 

(eval-when (compile eval)

(defmacro def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor vm:word-bits bits))
	 (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:note "inline array access")
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (result :scs (unsigned-reg) :from (:argument 0)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc dword-reg :offset ecx-offset) ecx)
	 (:generator 20
	   (move ecx index)
	   (inst shr ecx ,bit-shift)
	   (inst mov result
		 (make-ea :dword :base object :index ecx :scale 4
			  :disp (- (* vector-data-offset word-bytes)
				   other-pointer-type)))
	   (move ecx index)
	   (inst and ecx ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst shl ecx ,(1- (integer-length bits)))))
	   (inst shr result :cl)
	   (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (loadw result object (+ word vector-data-offset) 
		    other-pointer-type)
	     (unless (zerop extra)
	       (inst shr result (* extra ,bits)))
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg) :target ptr)
		(index :scs (unsigned-reg) :target ecx)
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc dword-reg) word-index)
	 (:temporary (:sc dword-reg :from (:argument 0)) ptr old)
	 (:temporary (:sc dword-reg :offset ecx-offset :from (:argument 1))
		     ecx)
	 (:generator 25
	   (move word-index index)
	   (inst shr word-index ,bit-shift)
	   (inst lea ptr
		 (make-ea :dword :base object :index word-index :scale 4
			  :disp (- (* vector-data-offset word-bytes)
				   other-pointer-type)))
	   (loadw old ptr)
	   (move ecx index)
	   (inst and ecx ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst shl ecx ,(1- (integer-length bits)))))
	   (inst ror old :cl)
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst and old ,(lognot (1- (ash 1 bits)))))
	   (sc-case value
	     (immediate
	      (unless (zerop (tn-value value))
		(inst or old (logand (tn-value value) ,(1- (ash 1 bits))))))
	     (unsigned-reg
	      (inst or old value)))
	   (inst rol old :cl)
	   (storew old ptr)
	   (sc-case value
	     (immediate
	      (inst mov result (tn-value value)))
	     (unsigned-reg
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type (:constant index) positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc dword-reg :to (:result 0)) old)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (inst mov old
		   (make-ea :dword :base object
			    :disp (- (* (+ word vector-data-offset) word-bytes)
				     other-pointer-type)))
	     (sc-case value
	       (immediate
		(let* ((value (tn-value value))
		       (mask ,(1- (ash 1 bits)))
		       (shift (* extra ,bits)))
		  (unless (= value mask)
		    (inst and old (lognot (ash mask shift))))
		  (unless (zerop value)
		    (inst or old (ash value shift)))))
	       (unsigned-reg
		(let ((shift (* extra ,bits)))
		  (unless (zerop shift)
		    (inst ror old shift)
		    (inst and old (lognot ,(1- (ash 1 bits))))
		    (inst or old value)
		    (inst rol old shift)))))
	     (inst mov (make-ea :dword :base object
				:disp (- (* (+ word vector-data-offset) word-bytes)
					 other-pointer-type))
		   old)
	     (sc-case value
	       (immediate
		(inst mov result (tn-value value)))
	       (unsigned-reg
		(move result value)))))))))


); eval-when (compile eval)

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)

;;; And the float variants.
;;; 

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
   (with-empty-tn@fp-top(value)
     (inst fld (make-ea	:dword :base object :index index :scale 1
			:disp (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))))))

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 30)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (with-empty-tn@fp-top(value)
     (inst fld (make-ea	:dword :base object
			:disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				    (* 4 index))
				 vm:other-pointer-type))))))

(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base object :index index :scale 1
			      :disp (- (* vm:vector-data-offset vm:word-bytes)
				       vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base object :index index :scale 1
			      :disp (- (* vm:vector-data-offset vm:word-bytes)
				       vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

(define-vop (data-vector-set-c/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (single-reg) :target result))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 30))
	      single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base object
			      :disp (- (+ (* vm:vector-data-offset
					     vm:word-bytes)
					  (* 4 index))
				       vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base object
			      :disp (- (+ (* vm:vector-data-offset
					     vm:word-bytes)
					  (* 4 index))
				       vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (with-empty-tn@fp-top(value)
     (inst fldd (make-ea :dword :base object :index index :scale 2
			 :disp (- (* vm:vector-data-offset vm:word-bytes)
				  vm:other-pointer-type))))))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 30)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 6
   (with-empty-tn@fp-top(value)
     (inst fldd (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     (* 8 index))
				  vm:other-pointer-type))))))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 20
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fstd (make-ea :dword :base object :index index :scale 2
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base object :index index :scale 2
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))


(define-vop (data-vector-set-c/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (double-reg) :target result))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 30))
	      double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 19
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fstd (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset
					      vm:word-bytes)
					   (* 8 index))
					vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset
					      vm:word-bytes)
					   (* 8 index))
					vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))


;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-32 vector.
;;;
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
(define-vop (raw-ref-single-c data-vector-ref-c/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum single-float))
(define-vop (raw-set-single-c data-vector-set-c/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      single-float))
;;;
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
(define-vop (raw-ref-double-c data-vector-ref-c/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum double-float))
(define-vop (raw-set-double-c data-vector-set-c/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      double-float))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;; 

(define-full-reffer raw-bits * 0 other-pointer-type (unsigned-reg) unsigned-num
  %raw-bits)
(define-full-setter set-raw-bits * 0 other-pointer-type (unsigned-reg)
  unsigned-num %set-raw-bits)


;;;; Misc. Array VOPs.

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))

;;;;
;;;; dtc expanded and fixed the following:



;;; unsigned-byte-8

(define-vop (data-vector-ref/simple-array-unsigned-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum)
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (inst movzx value
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))


(define-vop (data-vector-ref-c/simple-array-unsigned-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-unsigned-byte-8 (:constant (signed-byte 30)))
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst movzx value
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset word-bytes) index)
			    other-pointer-type)))))


(define-vop (data-vector-set/simple-array-unsigned-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum positive-fixnum)
  (:temporary (:sc dword-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (move eax value)
    (inst mov
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset word-bytes) other-pointer-type))
	  al-tn)
    (move result eax)))


(define-vop (data-vector-set-c/simple-array-unsigned-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-unsigned-byte-8 (:constant (signed-byte 30))
	      positive-fixnum)
  (:temporary (:sc dword-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (move eax value)
    (inst mov
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset word-bytes) index)
			    other-pointer-type))
	  al-tn)
    (move result eax)))


;;; unsigned-byte-16

(define-vop (data-vector-ref/simple-array-unsigned-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-unsigned-byte-16 positive-fixnum)
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (inst movzx value
	  (make-ea :word :base object :index index :scale 2
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))


(define-vop (data-vector-ref-c/simple-array-unsigned-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-unsigned-byte-16 (:constant (signed-byte 30)))
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst movzx value
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes)
			       (* 2 index))
			    other-pointer-type)))))


(define-vop (data-vector-set/simple-array-unsigned-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:arg-types simple-array-unsigned-byte-16 positive-fixnum positive-fixnum)
  (:temporary (:sc dword-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :word :base object :index index :scale 2
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  ax-tn)
    (move result eax)))


(define-vop (data-vector-set-c/simple-array-unsigned-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-unsigned-byte-16 (:constant (signed-byte 30)) positive-fixnum)
  (:temporary (:sc dword-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (move eax value)
    (inst mov
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes)
			       (* 2 index))
			    other-pointer-type))
	  ax-tn)
    (move result eax)))


;;; simple-string

(define-vop (data-vector-ref/simple-string)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-string positive-fixnum)
  (:temporary (:sc dword-reg ; byte-reg
		   :offset eax-offset ; al-offset
		   :target value
		   :from (:eval 0) :to (:result 0))
	      eax)
  (:ignore eax)
  (:results (value :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 5
    (inst mov al-tn
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset word-bytes) other-pointer-type)))
    (move value al-tn)))

(define-vop (data-vector-ref-c/simple-string)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-string (:constant (signed-byte 30)))
  (:temporary (:sc dword-reg :offset eax-offset :target value
		   :from (:eval 0) :to (:result 0))
	      eax)
  (:ignore eax)
  (:results (value :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 4
    (inst mov al-tn
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset word-bytes) index)
			    other-pointer-type)))
    (move value al-tn)))


(define-vop (data-vector-set/simple-string)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (base-char-reg)))
  (:arg-types simple-string positive-fixnum base-char)
  (:results (result :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 5 
    (inst mov (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  value)
    (move result value)))


(define-vop (data-vector-set/simple-string-c)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (base-char-reg)))
  (:info index)
  (:arg-types simple-string (:constant (signed-byte 30)) base-char)
  (:results (result :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 4
   (inst mov (make-ea :byte :base object
		      :disp (- (+ (* vector-data-offset word-bytes)
				  index)
			       other-pointer-type))
	 value)
   (move result value)))


;;; signed-byte-16

#+nil
(define-vop (data-vector-ref/simple-array-signed-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsx value
	  (make-ea :word :base object :index index :scale 2
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))

#+nil
(define-vop (data-vector-ref-c/simple-array-signed-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-signed-byte-16 (:constant (signed-byte 30)))
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (inst movsx value
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes)
			       (* 2 index))
			    other-pointer-type)))))

#+nil
(define-vop (data-vector-set/simple-array-signed-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:temporary (:sc dword-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :word :base object :index index :scale 2
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  ax-tn)
    (move result eax)))

#+nil
(define-vop (data-vector-set-c/simple-array-signed-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-signed-byte-16 (:constant (signed-byte 30)) tagged-num)
  (:temporary (:sc dword-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (move eax value)
    (inst mov
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes)
			       (* 2 index))
			    other-pointer-type))
	  ax-tn)
    (move result eax)))
