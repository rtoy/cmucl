;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/array.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the ARM definitions for array operations.
;;;
(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (emit-not-implemented)))


;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t fixnum) fixnum
  (flushable))
(defknown lisp::%set-array-dimension (t fixnum fixnum) fixnum
  ())

(define-vop (%array-dimension word-index-ref)
  (:translate lisp::%array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))

(define-vop (%set-array-dimension word-index-set)
  (:translate lisp::%set-array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))



(defknown lisp::%array-rank (t) fixnum (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (emit-not-implemented)))



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
    (emit-not-implemented)))



;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whose
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
	      (index :scs (any-reg immediate))
	      (value :scs ,scs))
       (:results (result :scs ,scs))
       (:result-types ,element-type))))

#+unicode
(def-data-vector-frobs simple-string halfword-index
  base-char base-char-reg)

(def-data-vector-frobs simple-vector word-index
  * descriptor-reg any-reg)

(def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-16 halfword-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-32 word-index
  unsigned-num unsigned-reg)

(def-data-vector-frobs simple-array-signed-byte-30 word-index
  tagged-num any-reg)
(def-data-vector-frobs simple-array-signed-byte-32 word-index
  signed-num signed-reg)

;;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;;
;;; The elements are stored in the word with the first element in the
;;; most-significant parts of the word.
;;; 

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
	 (:generator 20
	   (emit-not-implemented)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (emit-not-implemented)))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note _N"inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg))
		(value :scs (unsigned-reg immediate)))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 25
	   (emit-not-implemented)))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type
		     (:constant index)
		     positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 20
	   (emit-not-implemented))))))

); eval-when (compile eval)

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)


;;; And the float variants.
;;; 

(define-vop (data-vector-ref/simple-array-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (emit-not-implemented)))

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-array-single-float (:constant index))
  (:info index)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 3
    (emit-not-implemented)))


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
  (:generator 5
    (emit-not-implemented)))

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
  (:generator 2
    (emit-not-implemented)))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
    (emit-not-implemented)))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-array-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 3
    (emit-not-implemented)))

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
  (:generator 20
    (emit-not-implemented)))

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
  (:generator 10
    (emit-not-implemented)))


;;;; Misc. Array VOPs.


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
	 (index :scs (any-reg immediate))
	 (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


(define-vop (data-vector-ref/simple-array-signed-byte-16
	     signed-halfword-index-ref)
  (:note _N"inline array access")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-16 halfword-index-set)
  (:note _N"inline array store")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg immediate))
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
  (:result-types complex-single-float)
  (:generator 5
    (emit-not-implemented)))

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-single-float
	      (:constant index))
  (:info index)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 3
    (emit-not-implemented)))

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
  (:generator 5
    (emit-not-implemented)))

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
  (:generator 3
    (emit-not-implemented)))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (emit-not-implemented)))

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 5
    (emit-not-implemented)))

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
  (:generator 20
    (emit-not-implemented)))

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
  (:generator 15
    (emit-not-implemented)))


;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-32 vector.
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

;;;
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
;;; 

(define-vop (raw-bits word-index-ref)
  (:note _N"raw-bits VOP")
  (:translate %raw-bits)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))

(define-vop (set-raw-bits word-index-set)
  (:note _N"setf raw-bits VOP")
  (:translate %set-raw-bits)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg immediate))
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
  (:generator 7
    (emit-not-implemented)))

(define-vop (data-vector-ref-c/simple-array-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:generator 5
    (emit-not-implemented)))

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
  (:generator 20
    (emit-not-implemented)))

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
  (:generator 15
    (emit-not-implemented)))

(define-vop (data-vector-ref/simple-array-complex-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-double-float positive-fixnum)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:generator 7
    (emit-not-implemented)))

(define-vop (data-vector-ref-c/simple-array-complex-double-double-float)
  (:note _N"inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:generator 5
    (emit-not-implemented)))

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
  (:generator 20
    (emit-not-implemented)))

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
  (:generator 15
    (emit-not-implemented)))

) ; double-double

