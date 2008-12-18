;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/x87-array.lisp,v 1.2.2.2 2008/12/18 21:50:19 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the x86 definitions for array operations.
;;;
;;; And the float variants.
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


#+long-float
(define-vop (data-vector-ref/simple-array-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-long-float positive-fixnum)
  (:temporary (:sc any-reg :from :eval :to :result) temp)
  (:results (value :scs (long-reg)))
  (:result-types long-float)
  (:generator 7
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (with-empty-tn@fp-top(value)
      (inst fldl (make-ea :dword :base object :index temp :scale 1
			  :disp (- (* vm:vector-data-offset vm:word-bytes)
				   vm:other-pointer-type))))))

#+long-float
(define-vop (data-vector-ref-c/simple-array-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-long-float (:constant (signed-byte 30)))
  (:results (value :scs (long-reg)))
  (:result-types long-float)
  (:generator 6
   (with-empty-tn@fp-top(value)
     (inst fldl (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     (* 12 index))
				  vm:other-pointer-type))))))

#+long-float
(define-vop (data-vector-set/simple-array-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (long-reg) :target result))
  (:arg-types simple-array-long-float positive-fixnum long-float)
  (:temporary (:sc any-reg :from (:argument 1) :to :result) temp)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:generator 20
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (store-long-float
	    (make-ea :dword :base object :index temp :scale 1
		     :disp (- (* vm:vector-data-offset vm:word-bytes)
			      vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (store-long-float
	    (make-ea :dword :base object :index temp :scale 1
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

#+long-float
(define-vop (data-vector-set-c/simple-array-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (long-reg) :target result))
  (:info index)
  (:arg-types simple-array-long-float (:constant (signed-byte 30)) long-float)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:generator 19
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (store-long-float (make-ea :dword :base object
				      :disp (- (+ (* vm:vector-data-offset
						     vm:word-bytes)
						  (* 12 index))
					       vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
	     ;; Value is in ST0 but not result.
	     (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (store-long-float (make-ea :dword :base object
				      :disp (- (+ (* vm:vector-data-offset
						     vm:word-bytes)
						  (* 12 index))
					       vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
		    (inst fstd result))
		  (inst fxch value)))))))

;;; Complex float variants.
(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fld (make-ea :dword :base object :index index :scale 2
			   :disp (- (* vm:vector-data-offset vm:word-bytes)
				    vm:other-pointer-type)))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fld (make-ea :dword :base object :index index :scale 2
			   :disp (- (* (1+ vm:vector-data-offset)
				       vm:word-bytes)
				    vm:other-pointer-type)))))))

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fld (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 8 index))
				    vm:other-pointer-type)))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fld (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 8 index) 4)
				    vm:other-pointer-type)))))))

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
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fst (make-ea :dword :base object :index index :scale 2
				:disp (- (* vm:vector-data-offset
					    vm:word-bytes)
					 vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fst result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fst (make-ea :dword :base object :index index :scale 2
				:disp (- (* vm:vector-data-offset
					    vm:word-bytes)
					 vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fst value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fst result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fst (make-ea :dword :base object :index index :scale 2
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     4)
				  vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fst result-imag))
      (inst fxch value-imag))))

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
  (:generator 4
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fst (make-ea :dword :base object
				:disp (- (+ (* vm:vector-data-offset
					       vm:word-bytes)
					    (* 8 index))
					 vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fst result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fst (make-ea :dword :base object
				:disp (- (+ (* vm:vector-data-offset
					       vm:word-bytes)
					    (* 8 index))
					 vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fst value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fst result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fst (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     (* 8 index) 4)
				  vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fst result-imag))
      (inst fxch value-imag))))


(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					8)
				     vm:other-pointer-type)))))))

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 6
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 16 index))
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 16 index) 8)
				     vm:other-pointer-type)))))))

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
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object :index index :scale 4
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      8)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

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
  (:generator 19
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 16 index))
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 16 index))
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      (* 16 index) 8)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))


#+long-float
(define-vop (data-vector-ref/simple-array-complex-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-long-float positive-fixnum)
  (:temporary (:sc any-reg :from :eval :to :result) temp)
  (:results (value :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 7
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (let ((real-tn (complex-long-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldl (make-ea :dword :base object :index temp :scale 2
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-long-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldl (make-ea :dword :base object :index temp :scale 2
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					12)
				     vm:other-pointer-type)))))))

#+long-float
(define-vop (data-vector-ref-c/simple-array-complex-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-long-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 6
    (let ((real-tn (complex-long-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldl (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 24 index))
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-long-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldl (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 24 index) 12)
				     vm:other-pointer-type)))))))

#+long-float
(define-vop (data-vector-set/simple-array-complex-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-long-reg) :target result))
  (:arg-types simple-array-complex-long-float positive-fixnum
	      complex-long-float)
  (:temporary (:sc any-reg :from (:argument 1) :to :result) temp)
  (:results (result :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 20
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (let ((value-real (complex-long-reg-real-tn value))
	  (result-real (complex-long-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (store-long-float
	      (make-ea :dword :base object :index temp :scale 2
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (store-long-float
	      (make-ea :dword :base object :index temp :scale 2
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-long-reg-imag-tn value))
	  (result-imag (complex-long-reg-imag-tn result)))
      (inst fxch value-imag)
      (store-long-float
       (make-ea :dword :base object :index temp :scale 2
		:disp (- (+ (* vm:vector-data-offset vm:word-bytes) 12)
			 vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

#+long-float
(define-vop (data-vector-set-c/simple-array-complex-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-long-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-long-float (:constant (signed-byte 30))
	      complex-long-float)
  (:results (result :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 19
    (let ((value-real (complex-long-reg-real-tn value))
	  (result-real (complex-long-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (store-long-float
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 24 index))
				vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (store-long-float
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 24 index))
				vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-long-reg-imag-tn value))
	  (result-imag (complex-long-reg-imag-tn result)))
      (inst fxch value-imag)
      (store-long-float
       (make-ea :dword :base object
		:disp (- (+ (* vm:vector-data-offset vm:word-bytes)
			    (* 24 index) 12)
			 vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))


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
  (:generator 7
    (let ((hi-tn (double-double-reg-hi-tn value)))
      (with-empty-tn@fp-top (hi-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))))
    (let ((lo-tn (double-double-reg-lo-tn value)))
      (with-empty-tn@fp-top (lo-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					8)
				     vm:other-pointer-type)))))))

(define-vop (data-vector-ref-c/simple-array-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:generator 5
    (let ((hi-tn (double-double-reg-hi-tn value)))
      (with-empty-tn@fp-top (hi-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 16 index))
				     vm:other-pointer-type)))))
    (let ((lo-tn (double-double-reg-lo-tn value)))
      (with-empty-tn@fp-top (lo-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 16 index)
					8)
				     vm:other-pointer-type)))))))

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
  (:generator 20
    (let ((value-real (double-double-reg-hi-tn value))
	  (result-real (double-double-reg-hi-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (double-double-reg-lo-tn value))
	  (result-imag (double-double-reg-lo-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object :index index :scale 4
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      8)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

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
  (:generator 20
    (let ((value-real (double-double-reg-hi-tn value))
	  (result-real (double-double-reg-hi-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 16 index))
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 16 index))
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (double-double-reg-lo-tn value))
	  (result-imag (double-double-reg-lo-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      (* 16 index)
				      8)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-ref/simple-array-complex-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-double-float positive-fixnum)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:generator 7
    (let ((real-tn (complex-double-double-reg-real-hi-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 8
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))))
    (let ((real-tn (complex-double-double-reg-real-lo-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 8
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					8)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 8
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					16)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 8
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					24)
				     vm:other-pointer-type)))))))

(define-vop (data-vector-ref-c/simple-array-complex-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:generator 5
    (let ((real-tn (complex-double-double-reg-real-hi-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 32 index))
				     vm:other-pointer-type)))))
    (let ((real-tn (complex-double-double-reg-real-lo-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 32 index)
					8)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 32 index)
					16)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 32 index)
					24)
				     vm:other-pointer-type)))))))

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
  (:generator 20
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object :index index :scale 8
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object :index index :scale 8
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object :index index :scale 8
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     8)
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object :index index :scale 8
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     8)
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object :index index :scale 8
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      16)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))
    (let ((value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object :index index :scale 8
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      24)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

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
  (:generator 20
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 32 index))
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 32 index))
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 32 index)
					     8)
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 32 index)
					     8)
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      (* 32 index)
				      16)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))
    (let ((value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      (* 32 index)
				      24)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

)