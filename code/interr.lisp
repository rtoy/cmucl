;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/interr.lisp,v 1.15 1990/12/11 15:18:29 wlott Exp $
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
	  object-not-complex-error object-not-weak-pointer-error
	  object-not-structure-error))



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
  (let* ((rest-pos (position '&rest args))
	 (required (if rest-pos (subseq args 0 rest-pos) args))
	 (fp (gensym))
	 (sigcontext (gensym))
	 (sc-offsets (gensym))
	 (temp (gensym)))
    `(%deferr ',name
	      ,(meta-error-number name)
	      ,description
	      #+new-compiler
	      #'(lambda (name ,fp ,sigcontext ,sc-offsets)
		  (declare (ignorable name ,fp ,sigcontext ,sc-offsets))
		  (macrolet ((set-value (var value)
			       (let ((pos (position var ',required)))
				 (unless pos
				   (error "~S isn't one of the required args."
					  var))
				 `(let ((,',temp ,value))
				    (di::sub-set-debug-var-slot
				     ,',fp (nth ,pos ,',sc-offsets)
				     ,',temp ,',sigcontext)
				    (setf ,var ,',temp)))))
		    (let (,@(let ((offset -1))
			      (mapcar #'(lambda (var)
					  `(,var (di::sub-access-debug-var-slot
						  ,fp
						  (nth ,(incf offset)
						       ,sc-offsets)
						  ,sigcontext)))
				      required))
			  ,@(when rest-pos
			      `((,(nth (1+ rest-pos) args)
				 (mapcar #'(lambda (sc-offset)
					     (di::sub-access-debug-var-slot
					      ,fp
					      sc-offset
					      ,sigcontext))
					 (nthcdr ,rest-pos ,sc-offsets))))))
		      ,@body))))))


) ; Eval-When (Compile Eval)

(defun %deferr (name number description #+new-compiler function)
  (when (>= number (length *internal-errors*))
    (setf *internal-errors*
	  (replace (make-array (+ number 10) :initial-element nil)
		   *internal-errors*)))
  (setf (svref *internal-errors* number)
	(make-error-info :name name
			 :description description
			 #+new-compiler :function #+new-compiler function))
  name)




(deferr unknown-error
  "Unknown.  System lossage."
  (&rest args)
  (error "Unknown error:~{ ~S~})" args))

(deferr object-not-function-error
  "Object is not of type FUNCTION."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'function))

(deferr object-not-list-error
  "Object is not of type LIST."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'list))

(deferr object-not-bignum-error
  "Object is not of type BIGNUM."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'bignum))

(deferr object-not-ratio-error
  "Object is not of type RATIO."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'ratio))

(deferr object-not-single-float-error
  "Object is not of type SINGLE-FLOAT."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'single-float))

(deferr object-not-double-float-error
  "Object is not of type DOUBLE-FLOAT."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'double-float))

(deferr object-not-simple-string-error
  "Object is not of type SIMPLE-STRING."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-string))

(deferr object-not-simple-bit-vector-error
  "Object is not of type SIMPLE-BIT-VECTOR."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-bit-vector))

(deferr object-not-simple-vector-error
  "Object is not of type SIMPLE-VECTOR."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-vector))

(deferr object-not-fixnum-error
  "Object is not of type FIXNUM."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'fixnum))

(deferr object-not-function-or-symbol-error
  "Object is not of type FUNCTION or SYMBOL."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(or function symbol)))

(deferr object-not-vector-error
  "Object is not of type VECTOR."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'vector))

(deferr object-not-string-error
  "Object is not of type STRING."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'string))

(deferr object-not-bit-vector-error
  "Object is not of type BIT-VECTOR."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'bit-vector))

(deferr object-not-array-error
  "Object is not of type ARRAY."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'array))

(deferr object-not-number-error
  "Object is not of type NUMBER."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'number))

(deferr object-not-rational-error
  "Object is not of type RATIONAL."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'rational))

(deferr object-not-float-error
  "Object is not of type FLOAT."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'float))

(deferr object-not-real-error
  "Object is not of type REAL."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'real))

(deferr object-not-integer-error
  "Object is not of type INTEGER."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'integer))

(deferr object-not-cons-error
  "Object is not of type CONS."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'cons))

(deferr object-not-symbol-error
  "Object is not of type SYMBOL."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'symbol))

(deferr undefined-symbol-error
  "Undefined symbol."
  (symbol)
  (error 'undefined-function
	 :function-name name
	 :name symbol))

(deferr object-not-coercable-to-function-error
  "Object is not coercable to type FUNCTION."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'coercable-to-function))

(deferr invalid-argument-count-error
  "Invalid argument count."
  (nargs)
  (error 'simple-error
	 :function-name name
	 :format-string "Invalid number of arguments: ~S"
	 :format-arguments (list nargs)))

(deferr bogus-argument-to-values-list-error
  "Bogus argument to VALUES-LIST."
  (list)
  (error 'simple-error
	 :function-name name
	 :format-string "Attempt to use VALUES-LIST on a dotted-list:~%  ~S"
	 :format-arguments (list list)))

(deferr unbound-symbol-error
  "Unbound symbol."
  (symbol)
  (error 'unbound-variable :function-name name :name symbol))

(deferr object-not-base-character-error
  "Object is not of type BASE-CHARACTER."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'base-character))

(deferr object-not-sap-error
  "Object is not a System Area Pointer (SAP)."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'system-area-pointer))

(deferr invalid-unwind-error
  "Attempt to RETURN-FROM a block that no longer exists."
  ()
  (error 'control-error
	 :function-name name
	 :format-string
	 "Attempt to RETURN-FROM a block or GO to a tag that no longer exists"))

(deferr unseen-throw-tag-error
  "Attempt to THROW to a non-existent tag."
  (tag)
  (error 'control-error
	 :function-name name
	 :format-string "Attempt to THROW to a tag that does not exist: ~S"
	 :format-arguments (list tag)))

(deferr division-by-zero-error
  "Attempt to divide by zero."
  (this that)
  (error 'division-by-zero
	 :function-name name
	 :operation 'division
	 :operands (list this that)))

(deferr object-not-type-error
  "Object is of the wrong type."
  (object type)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type type))

(deferr odd-keyword-arguments-error
  "Odd number of keyword arguments."
  ()
  (error 'simple-error
	 :function-name name
	 :format-string "Odd number of keyword arguments."))

(deferr unknown-keyword-argument-error
  "Unknown keyword."
  (key)
  (error 'simple-error
	 :function-name name
	 :format-string "Unknown keyword: ~S"
	 :format-arguments (list key)))

(deferr not-<=-error
  "Not less than or equal."
  (this that)
  (error 'simple-error
	 :function-name name
	 :format-string "Assertion that ~S <= ~S failed."
	 :format-arguments (list this that)))

(deferr not-=-error
  "Not equal."
  (this that)
  (error 'simple-error
	 :function-name name
	 :format-string "Assertion that ~S = ~S failed."
	 :format-arguments (list this that)))

(deferr invalid-array-index-error
  "Invalid array index."
  (array bound index)
  (error 'simple-error
	 :function-name name
	 :format-string
	 "Invalid array index, ~D for ~S.  Should have been less than ~D"
	 :format-arguments (list index array bound)))


;;; ### Is this used?
(deferr wrong-number-of-indices-error
  "Wrong number of indices."
  (&rest args)
  (error "wrong-number-of-indices:~{ ~S~}" args))

(deferr object-not-simple-array-error
  "Object is not of type SIMPLE-ARRAY."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-array))

(deferr object-not-signed-byte-32-error
  "Object is not of type (SIGNED-BYTE 32)."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(signed-byte 32)))

(deferr object-not-unsigned-byte-32-error
  "Object is not of type (UNSIGNED-BYTE 32)."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(unsigned-byte 32)))

(deferr object-not-simple-array-unsigned-byte-2-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 2) (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 2) (*))))

(deferr object-not-simple-array-unsigned-byte-4-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 4) (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 4) (*))))

(deferr object-not-simple-array-unsigned-byte-8-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 8) (*))))

(deferr object-not-simple-array-unsigned-byte-16-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 16) (*))))

(deferr object-not-simple-array-unsigned-byte-32-error
  "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 32) (*))))

(deferr object-not-simple-array-single-float-error
  "Object is not of type (SIMPLE-ARRAY SINGLE-FLOAT (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array single-float (*))))

(deferr object-not-simple-array-double-float-error
  "Object is not of type (SIMPLE-ARRAY DOUBLE-FLOAT (*))."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array double-float (*))))

(deferr object-not-complex-error
  "Object is not of type COMPLEX."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'complex))

(deferr object-not-weak-pointer-error
  "Object is not a WEAK-POINTER."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'weak-pointer))

(deferr object-not-structure-error
	"Object is not a STRUCTURE."
  (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'structure))



;;;; internal-error signal handler.

(defvar *finding-name* nil)

(defun find-interrupted-name ()
  (if *finding-name*
      "<error finding name>"
      (handler-case
	  (let ((*finding-name* t))
	    (do ((frame (di:top-frame) (di:frame-down frame)))
		((or (null frame)
		     (and (di::compiled-frame-p frame)
			  (di::compiled-frame-escaped frame)))
		 (if (di::compiled-frame-p frame)
		     (di:debug-function-name
		      (di:frame-debug-function frame))
		     "<error finding name>"))))
	(error () "<error finding name>")
	(di:debug-condition () "<error finding name>"))))


(defun internal-error (scp continuable)
  (declare (ignore continuable))
  (alien-bind ((sc (make-alien 'mach:sigcontext
			       #.(c-sizeof 'mach:sigcontext)
			       scp)
		   mach:sigcontext
		   t)
	       (regs (mach:sigcontext-regs (alien-value sc)) mach:int-array t))
    (multiple-value-bind
	(error-number arguments)
	(vm:internal-error-arguments (alien-value sc))
      (let ((fp (int-sap (di::escape-register (alien-value sc)
					      vm::cfp-offset)))
	    (name (find-interrupted-name))
	    (info (and (< -1 error-number (length *internal-errors*))
		       (svref *internal-errors* error-number))))
	(cond ((null info)
	       (error 'simple-error
		      :function-name name
		      :format-string
		      "Unknown internal error, ~D?  args=~S"
		      :format-arguments
		      (list error-number
			    (mapcar #'(lambda (sc-offset)
					(di::sub-access-debug-var-slot
					 fp
					 sc-offset
					 (alien-value sc)))
				    arguments))))
	      ((null (error-info-function info))
	       (error 'simple-error
		      :function-name name
		      :format-string
		      "Internal error ~D: ~A.  args=~S"
		      :format-arguments
		      (list error-number
			    (error-info-description info)
			    (mapcar #'(lambda (sc-offset)
					(di::sub-access-debug-var-slot
					 fp
					 sc-offset
					 (alien-value sc)))
				    arguments))))
	      (t
	       (funcall (error-info-function info) name fp sc arguments)))))))
