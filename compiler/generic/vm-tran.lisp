;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-tran.lisp,v 1.4 1990/03/21 23:36:16 wlott Exp $
;;;
;;;    This file contains impelemtentation-dependent transforms.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

;;; We need to define these predicates, since the TYPEP source transform picks
;;; whichever predicate was defined last when there are multiple predicates for
;;; equivalent types.
;;;
(def-source-transform single-float-p (x) `(short-float-p ,x))
(def-source-transform double-float-p (x) `(long-float-p ,x))

(def-source-transform structurep (x)
  (once-only ((n-x x))
    `(and (simple-vector-p ,n-x)
	  (eql (%primitive get-vector-subtype ,n-x)
	       system:%g-vector-structure-subtype))))

(def-source-transform compiled-function-p (x)
  `(functionp ,x))

(def-source-transform char-int (x)
  `(char-code ,x))

(def-source-transform abs (x)
  (once-only ((n-x x))
    `(if (< ,n-x 0) (- ,n-x) ,n-x)))



(macrolet ((frob (name primitive)
	     `(def-source-transform ,name (&rest foo)
		`(truly-the nil
			    (%primitive ,',primitive ,@foo)))))
  (frob %type-check-error type-check-error)
  (frob %odd-keyword-arguments-error odd-keyword-arguments-error)
  (frob %unknown-keyword-argument-error unknown-keyword-argument-error)
  (frob %argument-count-error argument-count-error))


(def-source-transform %more-arg-context (&rest foo)
  `(%primitive more-arg-context ,@foo))
;;;
(def-source-transform %verify-argument-count (&rest foo)
  `(%primitive verify-argument-count ,@foo))



;;; Let these pass for now.

(def-primitive-translator header-ref (obj slot)
  (warn "Someone used HEADER-REF.")
  `(%primitive fast-svref ,obj ,slot))

(def-primitive-translator header-set (obj slot value)
  (warn "Someone used HEADER-SET.")
  `(%primitive fast-svset ,obj ,slot ,value))

(def-primitive-translator header-length (obj)
  (warn "Someone used HEADER-LENGTH.")
  `(%primitive vector-length ,obj))



;;; Transforms for data-vector-ref for strange array types.


(deftransform data-vector-ref ((array index)
			       (simple-array t))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (when (or (atom dims) (= (length dims) 1))
	(give-up))
      (let ((el-type (array-type-element-type array-type))
	    (total-size (if (member '* dims)
			    '*
			    (reduce #'* dims))))
	`(data-vector-ref (truly-the (simple-array ,(type-specifier el-type)
						   (,total-size))
				     (%array-data-vector array))
			  index)))))

(deftransform data-vector-set ((array index new-value)
			       (simple-array t t))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (when (or (atom dims) (= (length dims) 1))
	(give-up))
      (let ((el-type (array-type-element-type array-type))
	    (total-size (if (member '* dims)
			    '*
			    (reduce #'* dims))))
	`(data-vector-ref (truly-the (simple-array ,(type-specifier el-type)
						   (,total-size))
				     (%array-data-vector array))
			  index
			  new-value)))))


;;; Transforms for getting at arrays of unsigned-byte n when n < 8.

#+nil
(macrolet
    ((frob (type bits)
       `(progn
	  (deftransform data-vector-ref ((vector index)
					 (,type *))
	    `(multiple-value-bind (word bit)
				  (floor index ,(truncate 16 ,bits))
	       (ldb ,(ecase vm:target-byte-order
		       (:little-endian '(byte ,bits bit))
		       (:big-endian '(byte 1 (- 16 ,bits bit))))
		    (%raw-bits vector (+ (* word 16)
					 (* vm:vector-data-offset
					    vm:word-bits))))))
	  (deftransform data-vector-set ((vector index new-value)
					 (,type * *))
	    `(multiple-value-bind (word bit)
				  (floor index ,(truncate 16 ,bits))
	       (setf (ldb ,(ecase vm:target-byte-order
			     (:little-endian '(byte ,bits bit))
			     (:big-endian '(byte 1 (- 16 ,bits bit))))
			  (%raw-bits vector (+ (* word 16)
					       (* vm:vector-data-offset
						  vm:word-bits))))
		     new-value))))))
  (frob simple-bit-vector 1)
  (frob (simple-array (unsigned-byte 2) (*)) 2)
  (frob (simple-array (unsigned-byte 4) (*)) 4))
