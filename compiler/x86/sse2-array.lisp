;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/sse2-array.lisp,v 1.1.2.2.2.2 2008/10/11 01:35:24 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the x86 definitions for array operations.
;;;

(in-package :x86)

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:guard (backend-featurep :sse2))
  (:generator 5
    (inst movss value (make-ea :dword :base object :index index :scale 1
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 30)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:guard (backend-featurep :sse2))
  (:generator 4
    (inst movss value (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					   (* 4 index))
					vm:other-pointer-type)))))

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
  (:guard (backend-featurep :sse2))
  (:generator 5
    (inst movss (make-ea :dword :base object :index index :scale 1
			 :disp (- (* vm:vector-data-offset vm:word-bytes)
				  vm:other-pointer-type))
	  value)
    (inst movss result value)))

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
  (:guard (backend-featurep :sse2))
  (:generator 4
    (inst movss (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset
					vm:word-bytes)
				     (* 4 index))
				  vm:other-pointer-type))
	  value)
    (inst movss result value)))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:guard (backend-featurep :sse2))
  (:generator 7
    (inst movsd value (make-ea :dword :base object :index index :scale 2
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 30)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:guard (backend-featurep :sse2))
  (:generator 6
    (inst movsd value (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					   (* 8 index))
					vm:other-pointer-type)))))

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
  (:guard (backend-featurep :sse2))
  (:generator 20
    (inst movsd (make-ea :dword :base object :index index :scale 2
			 :disp (- (* vm:vector-data-offset vm:word-bytes)
				  vm:other-pointer-type))
	  value)
    (inst movsd result value)))

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
  (:guard (backend-featurep :sse2))
  (:generator 19
    (inst movsd (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset
					vm:word-bytes)
				     (* 8 index))
				  vm:other-pointer-type))
	  value)
    (inst movsd result value)))

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:guard (backend-featurep :sse2))
  (:generator 5
    #+(or)
    (progn
      (let ((real-tn (complex-single-reg-real-tn value)))
	(inst movss real-tn (make-ea :dword :base object :index index :scale 2
				     :disp (- (* vm:vector-data-offset vm:word-bytes)
					      vm:other-pointer-type))))
      (let ((imag-tn (complex-single-reg-imag-tn value)))
	(inst movss imag-tn (make-ea :dword :base object :index index :scale 2
				     :disp (- (* (1+ vm:vector-data-offset)
						 vm:word-bytes)
					      vm:other-pointer-type)))))
    (inst movlps value (make-ea :dword :base object :index index :scale 2
				:disp (- (* vm:vector-data-offset vm:word-bytes)
					 vm:other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:guard (backend-featurep :sse2))
  (:generator 4
    #+(or)
    (progn
      (let ((real-tn (complex-single-reg-real-tn value)))
	(inst movsd real-tn
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 8 index))
				vm:other-pointer-type))))
      (let ((imag-tn (complex-single-reg-imag-tn value)))
	(inst movsd imag-tn
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 8 index) 4)
				vm:other-pointer-type)))))
    (inst movlps value (make-ea :dword :base object
				:disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					    (* 8 index))
					 vm:other-pointer-type)))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:guard (backend-featurep :sse2))
  (:generator 5
    #+(or)
    (progn
      (let ((value-real (complex-single-reg-real-tn value))
	    (result-real (complex-single-reg-real-tn result)))
	(inst movss (make-ea :dword :base object :index index :scale 2
			     :disp (- (* vm:vector-data-offset
					 vm:word-bytes)
				      vm:other-pointer-type))
	      value-real)
	(inst movss result-real value-real))
      (let ((value-imag (complex-single-reg-imag-tn value))
	    (result-imag (complex-single-reg-imag-tn result)))
	(inst movss (make-ea :dword :base object :index index :scale 2
			     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					 4)
				      vm:other-pointer-type))
	      value-imag)
	(inst movss result-imag value-imag)))
    (inst movlps (make-ea :dword :base object :index index :scale 2
			  :disp (- (* vm:vector-data-offset
				      vm:word-bytes)
				   vm:other-pointer-type))
	  value)
    (inst movaps result value)))

(define-vop (data-vector-set-c/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-single-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 30))
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:guard (backend-featurep :sse2))
  (:generator 4
    #+(or)
    (progn
      (let ((value-real (complex-single-reg-real-tn value))
	    (result-real (complex-single-reg-real-tn result)))
	(inst movsd (make-ea :dword :base object
			     :disp (- (+ (* vm:vector-data-offset
					    vm:word-bytes)
					 (* 8 index))
				      vm:other-pointer-type))
	      value-real)
	(inst movsd result-real value-real))
      (let ((value-imag (complex-single-reg-imag-tn value))
	    (result-imag (complex-single-reg-imag-tn result)))
	(inst movsd (make-ea :dword :base object
			     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					 (* 8 index) 4)
				      vm:other-pointer-type))
	      value-imag)
	(inst movsd result-imag value-imag)))
    (inst movlps (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset
					 vm:word-bytes)
				      (* 8 index))
				   vm:other-pointer-type))
	  value)
    (inst movsd result value)))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 7
    #+(or)
    (progn
      (let ((real-tn (complex-double-reg-real-tn value)))
	(inst movsd real-tn (make-ea :dword :base object :index index :scale 4
				     :disp (- (* vm:vector-data-offset vm:word-bytes)
					      vm:other-pointer-type))))
      (let ((imag-tn (complex-double-reg-imag-tn value)))
	(inst movsd imag-tn
	      (make-ea :dword :base object :index index :scale 4
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   8)
				vm:other-pointer-type)))))
    (inst movupd value (make-ea :dword :base object :index index :scale 4
				   :disp (- (* vm:vector-data-offset vm:word-bytes)
					    vm:other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 6
    #+(or)
    (progn
      (let ((real-tn (complex-double-reg-real-tn value)))
	(inst movsd real-tn
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 16 index))
				vm:other-pointer-type))))
      (let ((imag-tn (complex-double-reg-imag-tn value)))
	(inst movsd imag-tn
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 16 index) 8)
				vm:other-pointer-type)))))
    (inst movupd value (make-ea :dword :base object
				:disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					    (* 16 index))
					 vm:other-pointer-type)))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 20
    #+(or)	      
    (progn
      (let ((value-real (complex-double-reg-real-tn value))
	    (result-real (complex-double-reg-real-tn result)))
	(inst movsd (make-ea :dword :base object :index index :scale 4
			     :disp (- (* vm:vector-data-offset
					 vm:word-bytes)
				      vm:other-pointer-type))
	      value-real)
	(inst movsd result-real value-real))
      (let ((value-imag (complex-double-reg-imag-tn value))
	    (result-imag (complex-double-reg-imag-tn result)))
	(inst movsd (make-ea :dword :base object :index index :scale 4
			     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					 8)
				      vm:other-pointer-type))
	      value-imag)
	(inst movsd result-imag value-imag)))
    (inst movupd (make-ea :dword :base object :index index :scale 4
			     :disp (- (* vm:vector-data-offset
					 vm:word-bytes)
				      vm:other-pointer-type))
	  value)
    (inst movapd result value)))

(define-vop (data-vector-set-c/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-double-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 30))
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:guard (backend-featurep :sse2))
  (:generator 19
    #+(or)	      
    (progn
      (let ((value-real (complex-double-reg-real-tn value))
	    (result-real (complex-double-reg-real-tn result)))
	(inst movsd (make-ea :dword :base object
			     :disp (- (+ (* vm:vector-data-offset
					    vm:word-bytes)
					 (* 16 index))
				      vm:other-pointer-type))
	      value-real)
	(inst movsd result-real value-real))
      (let ((value-imag (complex-double-reg-imag-tn value))
	    (result-imag (complex-double-reg-imag-tn result)))
	(inst movsd (make-ea :dword :base object
			     :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					 (* 16 index) 8)
				      vm:other-pointer-type))
	      value-imag)
	(inst movsd result-imag value-imag)))
    (inst movupd (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset
					 vm:word-bytes)
				      (* 16 index))
				   vm:other-pointer-type))
	  value)
    (inst movapd result value)))


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
