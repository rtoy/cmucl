;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/sse2-array.lisp,v 1.2.4.2 2008/12/19 01:31:33 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the x86 definitions for array operations.
;;;

(in-package :x86)

(macrolet
    ((frob (type move copy scale)
       (let ((ref-name (symbolicate "DATA-VECTOR-REF/SIMPLE-ARRAY-" type "-FLOAT"))
	     (c-ref-name (symbolicate "DATA-VECTOR-REF-C/SIMPLE-ARRAY-" type "-FLOAT"))
	     (set-name (symbolicate "DATA-VECTOR-SET/SIMPLE-ARRAY-" type "-FLOAT"))
	     (c-set-name (symbolicate "DATA-VECTOR-SET-C/SIMPLE-ARRAY-" type "-FLOAT"))
	     (result-sc (symbolicate type "-REG"))
	     (result-type (symbolicate type "-FLOAT"))
	     (array-sc (symbolicate "SIMPLE-ARRAY-" type "-FLOAT")))
	 `(progn
	    (define-vop (,ref-name)
	      (:note "inline array access")
	      (:translate data-vector-ref)
	      (:policy :fast-safe)
	      (:args (object :scs (descriptor-reg))
		     (index :scs (any-reg)))
	      (:arg-types ,array-sc positive-fixnum)
	      (:results (value :scs (,result-sc)))
	      (:result-types ,result-type)
	      (:guard (backend-featurep :sse2))
	      (:generator 5
		(inst ,move value
		      (make-ea :dword :base object :index index :scale ,scale
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))))
	    (define-vop (,c-ref-name)
	      (:note "inline array access")
	      (:translate data-vector-ref)
	      (:policy :fast-safe)
	      (:args (object :scs (descriptor-reg)))
	      (:info index)
	      (:arg-types ,array-sc (:constant (signed-byte 30)))
	      (:results (value :scs (,result-sc)))
	      (:result-types ,result-type)
	      (:guard (backend-featurep :sse2))
	      (:generator 4
		(inst ,move value
		      (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					   (* ,(* 4 scale) index))
					vm:other-pointer-type)))))
	    (define-vop (,set-name)
	      (:note "inline array store")
	      (:translate data-vector-set)
	      (:policy :fast-safe)
	      (:args (object :scs (descriptor-reg))
		     (index :scs (any-reg))
		     (value :scs (,result-sc) :target result))
	      (:arg-types ,array-sc positive-fixnum ,result-type)
	      (:results (result :scs (,result-sc)))
	      (:result-types ,result-type)
	      (:guard (backend-featurep :sse2))
	      (:generator 5
		(inst ,move (make-ea :dword :base object :index index :scale ,scale
				     :disp (- (* vm:vector-data-offset vm:word-bytes)
					      vm:other-pointer-type))
		      value)
		(unless (location= result value)
		  (inst ,copy result value))))

	    (define-vop (,c-set-name)
	      (:note "inline array store")
	      (:translate data-vector-set)
	      (:policy :fast-safe)
	      (:args (object :scs (descriptor-reg))
		     (value :scs (,result-sc) :target result))
	      (:info index)
	      (:arg-types ,array-sc (:constant (signed-byte 30))
			  ,result-type)
	      (:results (result :scs (,result-sc)))
	      (:result-types ,result-type)
	      (:guard (backend-featurep :sse2))
	      (:generator 4
		(inst ,move (make-ea :dword :base object
				     :disp (- (+ (* vm:vector-data-offset
						    vm:word-bytes)
						 (* ,(* 4 scale) index))
					      vm:other-pointer-type))
		      value)
		(unless (location= result value)
		  (inst ,copy result value))))))))
  (frob single movss movss 1)
  (frob double movsd movsd 2)
  (frob complex-single movlps movaps 2)
  (frob complex-double movupd movapd 4))


#+double-double
(progn
(define-vop (data-vector-ref/simple-array-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-double-float positive-fixnum)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 7
    (let ((hi-tn (double-double-reg-hi-tn value)))
      (inst movsd hi-tn
	    (make-ea :dword :base object :index index :scale 4
		     :disp (- (* vm:vector-data-offset vm:word-bytes)
			      vm:other-pointer-type))))
    (let ((lo-tn (double-double-reg-lo-tn value)))
      (inst movsd lo-tn (make-ea :dword :base object :index index :scale 4
				 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					     8)
					  vm:other-pointer-type))))))

(define-vop (data-vector-ref-c/simple-array-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 5
    (let ((hi-tn (double-double-reg-hi-tn value)))
      (inst movsd hi-tn
	    (make-ea :dword :base object
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 (* 16 index))
			      vm:other-pointer-type))))
    (let ((lo-tn (double-double-reg-lo-tn value)))
      (inst movsd lo-tn
	    (make-ea :dword :base object
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 (* 16 index)
				 8)
			      vm:other-pointer-type))))))

(define-vop (data-vector-set/simple-array-double-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (double-double-reg) :target result))
  (:arg-types simple-array-double-double-float positive-fixnum
	      double-double-float)
  (:results (result :scs (double-double-reg)))
  (:result-types double-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 20
    (let ((value-real (double-double-reg-hi-tn value))
	  (result-real (double-double-reg-hi-tn result)))
      (inst movsd (make-ea :dword :base object :index index :scale 4
			   :disp (- (* vm:vector-data-offset
				       vm:word-bytes)
				    vm:other-pointer-type))
	    value-real)
      (inst movsd result-real value-real))
    (let ((value-imag (double-double-reg-lo-tn value))
	  (result-imag (double-double-reg-lo-tn result)))
      (inst movsd (make-ea :dword :base object :index index :scale 4
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       8)
				    vm:other-pointer-type))
	    value-imag)
      (inst movsd result-imag value-imag))))

(define-vop (data-vector-set-c/simple-array-double-double-float)
  (:note "inline array store")
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
  (:guard (backend-featurep :sse2))
  (:generator 20
    (let ((value-real (double-double-reg-hi-tn value))
	  (result-real (double-double-reg-hi-tn result)))
      (inst movsd (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset
					  vm:word-bytes)
				       (* 16 index))
				    vm:other-pointer-type))
	    value-real)
      (inst movsd result-real value-real))
    (let ((value-imag (double-double-reg-lo-tn value))
	  (result-imag (double-double-reg-lo-tn result)))
      (inst movsd (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 16 index)
				       8)
				    vm:other-pointer-type))
	    value-imag)
      (inst movsd result-imag value-imag))))

(define-vop (data-vector-ref/simple-array-complex-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-double-float positive-fixnum)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 7
    (let ((real-tn (complex-double-double-reg-real-hi-tn value)))
      (inst movsd real-tn
	    (make-ea :dword :base object :index index :scale 8
		     :disp (- (* vm:vector-data-offset vm:word-bytes)
			      vm:other-pointer-type))))
    (let ((real-tn (complex-double-double-reg-real-lo-tn value)))
      (inst movsd real-tn
	    (make-ea :dword :base object :index index :scale 8
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 8)
			      vm:other-pointer-type))))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn value)))
      (inst movsd imag-tn
	    (make-ea :dword :base object :index index :scale 8
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 16)
			      vm:other-pointer-type))))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn value)))
      (inst movsd imag-tn
	    (make-ea :dword :base object :index index :scale 8
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 24)
			      vm:other-pointer-type))))))

(define-vop (data-vector-ref-c/simple-array-complex-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 5
    (let ((real-tn (complex-double-double-reg-real-hi-tn value)))
      (inst movsd real-tn
	    (make-ea :dword :base object
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 (* 32 index))
			      vm:other-pointer-type))))
    (let ((real-tn (complex-double-double-reg-real-lo-tn value)))
      (inst movsd real-tn
	    (make-ea :dword :base object
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 (* 32 index)
				 8)
			      vm:other-pointer-type))))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn value)))
      (inst movsd imag-tn
	    (make-ea :dword :base object
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 (* 32 index)
				 16)
			      vm:other-pointer-type))))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn value)))
      (inst movsd imag-tn
	    (make-ea :dword :base object
		     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				 (* 32 index)
				 24)
			      vm:other-pointer-type))))))

(define-vop (data-vector-set/simple-array-complex-double-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-double-double-reg) :target result))
  (:arg-types simple-array-complex-double-double-float positive-fixnum
	      complex-double-double-float)
  (:results (result :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 20
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result)))
      (inst movsd (make-ea :dword :base object :index index :scale 8
			   :disp (- (* vm:vector-data-offset
				       vm:word-bytes)
				    vm:other-pointer-type))
	    value-real)
      (inst movsd result-real value-real))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result)))
      (inst movsd (make-ea :dword :base object :index index :scale 8
			   :disp (- (+ (* vm:vector-data-offset
					  vm:word-bytes)
				       8)
				    vm:other-pointer-type))
	    value-real)
      (inst movsd result-real value-real))
    (let ((value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result)))
      (inst movsd (make-ea :dword :base object :index index :scale 8
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       16)
				    vm:other-pointer-type))
	    value-imag)
      (inst movsd result-imag value-imag))
    (let ((value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result)))
      (inst movsd (make-ea :dword :base object :index index :scale 8
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       24)
				    vm:other-pointer-type))
	    value-imag)
      (inst movsd result-imag value-imag))))

(define-vop (data-vector-set-c/simple-array-complex-double-double-float)
  (:note "inline array store")
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
  (:guard (backend-featurep :sse2))
  (:generator 20
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result)))
      (inst movsd (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset
					  vm:word-bytes)
				       (* 32 index))
				    vm:other-pointer-type))
	    value-real)
      (inst movsd result-real value-real))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result)))
      (inst movsd (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset
					  vm:word-bytes)
				       (* 32 index)
				       8)
				    vm:other-pointer-type))
	    value-real)
      (inst movsd result-real value-real))
    (let ((value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result)))
      (inst movsd (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 32 index)
				       16)
				    vm:other-pointer-type))
	    value-imag)
      (inst movsd result-imag value-imag))
    (let ((value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result)))
      (inst movsd (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 32 index)
				       24)
				    vm:other-pointer-type))
	    value-imag)
      (inst movsd result-imag value-imag))))

)
