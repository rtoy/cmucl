;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/interr.lisp,v 1.6 1990/06/09 00:55:37 wlott Exp $
;;;
;;; Functions and macros to define and deal with internal errors (i.e.
;;; problems that can be signaled from assembler code).
;;;
;;; Written by William Lott.
;;;

(in-package "KERNEL")

(export '(error-number-or-lose))

(export '(unknown-error object-not-function-error object-not-list-error
	  object-not-bignum-error object-not-ratio-error
	  object-not-single-float-error object-not-double-float-error
	  object-not-simple-string-error object-not-simple-bit-vector-error
	  object-not-simple-vector-error object-not-fixnum-error
	  object-not-function-or-symbol-error object-not-vector-error
	  object-not-string-error object-not-bit-vector-error
	  object-not-array-error object-not-number-error
	  object-not-rational-error object-not-float-error
	  object-not-real-error object-not-integer-error
	  object-not-cons-error object-not-symbol-error
	  undefined-symbol-error object-not-coercable-to-function-error
	  invalid-argument-count-error bogus-argument-to-values-list-error
	  unbound-symbol-error object-not-base-character-error
	  object-not-sap-error invalid-unwind-error unseen-throw-tag-error
	  division-by-zero-error object-not-type-error
	  odd-keyword-arguments-error unknown-keyword-argument-error
	  not-<=-error not-=-error invalid-array-index-error
	  wrong-number-of-indices-error object-not-simple-array-error
	  object-not-signed-byte-32-error object-not-unsigned-byte-32-error
	  object-not-simple-array-unsigned-byte-2-error
	  object-not-simple-array-unsigned-byte-4-error
	  object-not-simple-array-unsigned-byte-8-error
	  object-not-simple-array-unsigned-byte-16-error
	  object-not-simple-array-unsigned-byte-32-error
	  object-not-simple-array-single-float-error
	  object-not-simple-array-double-float-error
	  object-not-complex-error object-not-weak-pointer-error))



;;;; Internal Errors

(defvar *internal-errors* (make-array 10 :initial-element nil))

(defstruct (error-info
	    (:print-function %print-error-info))
  name
  description
  function)

(defun %print-error-info (info stream depth)
  (declare (ignore depth))
  (format stream "#<error-info for ~S>" (error-info-name info)))

(defun error-number-or-lose (name)
  (or (position-if #'(lambda (info)
		       (and info (eq name (error-info-name info))))
		   *internal-errors*)
      (error "Unknown internal error: ~S" name)))


(eval-when (compile eval)

(defvar *meta-errors*)
(setf *meta-errors* (make-array 10 :initial-element nil))

(defun meta-error-number (name)
  (or (when (boundp '*internal-errors*)
	(position-if #'(lambda (info)
			 (and info (eq name (error-info-name info))))
		     *internal-errors*))
      (position name *meta-errors*)
      (do ((number 0 (1+ number)))
	  ((and (or (not (boundp '*internal-errors*))
		    (>= number (length *internal-errors*))
		    (null (svref *internal-errors* number)))
		(or (>= number (length *meta-errors*))
		    (null (svref *meta-errors* number))))
	   (when (>= number (length *meta-errors*))
	     (setf *meta-errors*
		   (replace (make-array (+ number 10) :initial-element nil)
			    *meta-errors*)))
	   (setf (svref *meta-errors* number) name)
	   number))))


(defmacro deferr (name description args &rest body)
  `(%deferr ',name
	    ,(meta-error-number name)
	    ,description
	    #'(lambda ,args
		,@body)))

)

(defun %deferr (name number description function)
  (when (>= number (length *internal-errors*))
    (setf *internal-errors*
	  (replace (make-array (+ number 10) :initial-element nil)
		   *internal-errors*)))
  (setf (svref *internal-errors* number)
	(make-error-info :name name
			 :description description
			 :function function))
  name)

) ; Eval-When (Compile Load Eval)


(deferr unknown-error
  "Unknown.  System lossage."
  (&rest args)
  (error "Unknown error:~{ ~S~})" args))

(deferr object-not-function-error
  "Object is not of type FUNCTION."
  (&rest args)
  (error "object-not-function:~{ ~S~}" args))

(deferr object-not-list-error
  "Object is not of type LIST."
  (&rest args)
  (error "object-not-list:~{ ~S~}" args))

(deferr object-not-bignum-error
  "Object is not of type BIGNUM."
  (&rest args)
  (error "object-not-bignum:~{ ~S~}" args))

(deferr object-not-ratio-error
  "Object is not of type RATIO."
  (&rest args)
  (error "object-not-ratio:~{ ~S~}" args))

(deferr object-not-single-float-error
  "Object is not of type SINGLE-FLOAT."
  (&rest args)
  (error "object-not-single-float:~{ ~S~}" args))

(deferr object-not-double-float-error
  "Object is not of type DOUBLE-FLOAT."
  (&rest args)
  (error "object-not-double-float:~{ ~S~}" args))

(deferr object-not-simple-string-error
  "Object is not of type SIMPLE-STRING."
  (&rest args)
  (error "object-not-simple-string:~{ ~S~}" args))

(deferr object-not-simple-bit-vector-error
  "Object is not of type SIMPLE-BIT-VECTOR."
  (&rest args)
  (error "object-not-simple-bit-vector:~{ ~S~}" args))

(deferr object-not-simple-vector-error
  "Object is not of type SIMPLE-VECTOR."
  (&rest args)
  (error "object-not-simple-vector:~{ ~S~}" args))

(deferr object-not-fixnum-error
  "Object is not of type FIXNUM."
  (&rest args)
  (error "object-not-fixnum:~{ ~S~}" args))

(deferr object-not-function-or-symbol-error
  "Object is not of type FUNCTION or of type SYMBOL."
  (&rest args)
  (error "object-not-function-or-symbol:~{ ~S~}" args))

(deferr object-not-vector-error
  "Object is not of type VECTOR."
  (&rest args)
  (error "object-not-vector:~{ ~S~}" args))

(deferr object-not-string-error
  "Object is not of type STRING."
  (&rest args)
  (error "object-not-string:~{ ~S~}" args))

(deferr object-not-bit-vector-error
  "Object is not of type BIT-VECTOR."
  (&rest args)
  (error "object-not-bit-vector:~{ ~S~}" args))

(deferr object-not-array-error
  "Object is not of type ARRAY."
  (&rest args)
  (error "object-not-array:~{ ~S~}" args))

(deferr object-not-number-error
  "Object is not of type NUMBER."
  (&rest args)
  (error "object-not-number:~{ ~S~}" args))

(deferr object-not-rational-error
  "Object is not of type RATIONAL."
  (&rest args)
  (error "object-not-rational:~{ ~S~}" args))

(deferr object-not-float-error
  "Object is not of type FLOAT."
  (&rest args)
  (error "object-not-float:~{ ~S~}" args))

(deferr object-not-real-error
  "Object is not of type REAL."
  (&rest args)
  (error "object-not-real:~{ ~S~}" args))

(deferr object-not-integer-error
  "Object is not of type INTEGER."
  (&rest args)
  (error "object-not-integer:~{ ~S~}" args))

(deferr object-not-cons-error
  "Object is not of type CONS."
  (&rest args)
  (error "object-not-cons:~{ ~S~}" args))

(deferr object-not-symbol-error
  "Object is not of type SYMBOL."
  (&rest args)
  (error "object-not-symbol:~{ ~S~}" args))

(deferr undefined-symbol-error
  "Undefined symbol."
  (&rest args)
  (error "undefined-symbol:~{ ~S~}" args))

(deferr object-not-coercable-to-function-error
  "Object is not coercable to type FUNCTION."
  (&rest args)
  (error "object-not-coercable-to-function:~{ ~S~}" args))

(deferr invalid-argument-count-error
  "Invalid argument count."
  (&rest args)
  (error "invalid-argument-count:~{ ~S~}" args))

(deferr bogus-argument-to-values-list-error
  "Bogus argument to VALUES-LIST."
  (&rest args)
  (error "bogus-argument-to-values-list:~{ ~S~}" args))

(deferr unbound-symbol-error
  "Unbound symbol."
  (&rest args)
  (error "unbound-symbol:~{ ~S~}" args))

(deferr object-not-base-character-error
  "Object is not of type BASE-CHARACTER."
  (&rest args)
  (error "object-not-base-character:~{ ~S~}" args))

(deferr object-not-sap-error
  "Object is not a System Area Pointer (SAP)."
  (&rest args)
  (error "object-not-sap:~{ ~S~}" args))

(deferr invalid-unwind-error
  "Attempt to RETURN-FROM a block that no longer exists."
  (&rest args)
  (error "invalid-unwind:~{ ~S~}" args))

(deferr unseen-throw-tag-error
  "Attempt to THROW to a non-existent tag."
  (&rest args)
  (error "unseen-throw-tag:~{ ~S~}" args))

(deferr division-by-zero-error
  "Attempt to divide by zero."
  (&rest args)
  (error "division-by-zero:~{ ~S~}" args))

(deferr object-not-type-error
  "Object is of the wrong type."
  (&rest args)
  (error "object-not-type:~{ ~S~}" args))

(deferr odd-keyword-arguments-error
  "Odd number of keyword arguments."
  (&rest args)
  (error "odd-keyword-arguments:~{ ~S~}" args))

(deferr unknown-keyword-argument-error
  "Unknown keyword."
  (&rest args)
  (error "unknown-keyword-argument:~{ ~S~}" args))

(deferr not-<=-error
  "Not less than or equal."
  (&rest args)
  (error "not-<=:~{ ~S~}" args))

(deferr not-=-error
  "Not equal."
  (&rest args)
  (error "not-=:~{ ~S~}" args))

(deferr invalid-array-index-error
  "Invalid array index."
  (&rest args)
  (error "invalid-array-index:~{ ~S~}" args))

(deferr wrong-number-of-indices-error
  "Wrong number of indices."
  (&rest args)
  (error "wrong-number-of-indices:~{ ~S~}" args))

(deferr object-not-simple-array-error
  "Object is not of type SIMPLE-ARRAY."
  (&rest args)
  (error "object-not-simple-array:~{ ~S~}" args))

(deferr object-not-signed-byte-32-error
  "Object is not of type (SIGNED-BYTE 32)."
  (&rest args)
  (error "object-not-signed-byte-32:~{ ~S~}" args))

(deferr object-not-unsigned-byte-32-error
  "Object is not of type (UNSIGNED-BYTE 32)."
  (&rest args)
  (error "object-not-unsigned-byte-32:~{ ~S~}" args))

(deferr object-not-simple-array-unsigned-byte-2-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 2) (*))."
  (&rest args)
  (error "object-not-simple-array-unsigned-byte-2:~{ ~S~}" args))

(deferr object-not-simple-array-unsigned-byte-4-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 4) (*))."
  (&rest args)
  (error "object-not-simple-array-unsigned-byte-4:~{ ~S~}" args))

(deferr object-not-simple-array-unsigned-byte-8-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))."
  (&rest args)
  (error "object-not-simple-array-unsigned-byte-8:~{ ~S~}" args))

(deferr object-not-simple-array-unsigned-byte-16-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*))."
  (&rest args)
  (error "object-not-simple-array-unsigned-byte-16:~{ ~S~}" args))

(deferr object-not-simple-array-unsigned-byte-32-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))."
  (&rest args)
  (error "object-not-simple-array-unsigned-byte-32:~{ ~S~}" args))

(deferr object-not-simple-array-single-float-error
  "Object is not of type (SIMPLE-ARRAY SINGLE-FLOAT (*))."
  (&rest args)
  (error "object-not-simple-array-single-float:~{ ~S~}" args))

(deferr object-not-simple-array-double-float-error
  "Object is not of type (SIMPLE-ARRAY DOUBLE-FLOAT (*))."
  (&rest args)
  (error "object-not-simple-array-double-float:~{ ~S~}" args))

(deferr object-not-complex-error
  "Object is not of type COMPLEX."
  (&rest args)
  (error "object-not-complex:~{ ~S~}" args))

(deferr object-not-weak-pointer-error
  "Object is not a WEAK-POINTER."
  (&rest args)
  (error "object-not-weak-pointer:~{ ~S~}" args))



#+new-compiler
(defvar *finding-name* nil)

#+new-compiler
(defun find-interrupted-name ()
  (if *finding-name*
      "<error finding name>"
      (handler-case
	  (let ((*finding-name* t))
	    (do ((frame (di:top-frame) (di:frame-down frame)))
		((or (null frame)
		     (di::frame-escaped frame))
		 (if frame
		     (di:debug-function-name
		      (di:frame-debug-function frame))
		     "<error finding name>"))))
	(error () "<error finding name>")
	(di:debug-condition () "<error finding name>"))))


#+new-compiler
(defun internal-error (signal code scp)
  (declare (ignore signal code))
  (alien-bind ((sc (make-alien 'mach:sigcontext
			       #.(c-sizeof 'mach:sigcontext)
			       scp)
		   mach:sigcontext
		   t)
	       (regs (mach:sigcontext-regs (alien-value sc)) mach:int-array t))
    (let* ((pc (sap+ (alien-access
		      (mach:sigcontext-pc
		       (alien-value sc)))
		     (if (logbitp 31
				  (alien-access
				   (mach:sigcontext-cause
				    (alien-value sc))))
			 4
			 0)))
	   #+nil (bad-inst (sap-ref-32 pc 0))
	   (number (sap-ref-8 pc 4))
	   (info (svref *internal-errors* number))
	   (args nil))
      (do ((ptr (sap+ pc 5) (sap+ ptr 1)))
	  ((zerop (sap-ref-8 ptr 0)))
	(without-gcing
	 (push (di::make-lisp-obj
		(alien-access (mach:int-array-ref (alien-value regs)
						  (sap-ref-8 ptr 0))))
	       args)))
      (error 'simple-error
	     :format-string "~A [~D]:~{ ~S~}"
	     :format-arguments (list (if info
					 (error-info-description info)
					 "Unknown error")
				     number
				     (nreverse args))
	     :function-name (find-interrupted-name)))))
