;;; -*- Package: VM; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/objdef.lisp,v 1.16 1992/02/26 00:56:53 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/objdef.lisp,v 1.16 1992/02/26 00:56:53 wlott Exp $
;;;
;;; This file contains the machine independent aspects of the object
;;; representation.
;;;
;;; Written by William Lott.
;;;
(in-package "VM")

(export '(lowtag-bits lowtag-mask lowtag-limit type-bits type-mask
	  target-most-positive-fixnum target-most-negative-fixnum
	  even-fixnum-type function-pointer-type other-immediate-0-type
	  list-pointer-type odd-fixnum-type structure-pointer-type
	  other-immediate-1-type other-pointer-type bignum-type ratio-type
	  single-float-type double-float-type complex-type
	  simple-array-type simple-string-type simple-bit-vector-type
	  simple-vector-type simple-array-unsigned-byte-2-type
	  simple-array-unsigned-byte-4-type
	  simple-array-unsigned-byte-8-type
	  simple-array-unsigned-byte-16-type
	  simple-array-unsigned-byte-32-type simple-array-single-float-type
	  simple-array-double-float-type complex-string-type
	  complex-bit-vector-type complex-vector-type complex-array-type
	  code-header-type function-header-type closure-header-type
	  closure-function-header-type return-pc-header-type
	  value-cell-header-type symbol-header-type base-char-type
	  sap-type unbound-marker-type weak-pointer-type
	  structure-header-type funcallable-instance-header-type
	  vector-normal-subtype
	  vector-valid-hashing-subtype vector-must-rehash-subtype
	  primitive-object primitive-object-p
	  primitive-object-name primitive-object-header
	  primitive-object-lowtag primitive-object-options
	  primitive-object-slots primitive-object-size
	  primitive-object-variable-length slot-name slot-docs slot-rest-p
	  slot-offset slot-length slot-options *primitive-objects*
	  define-for-each-primitive-object))

(in-package "KERNEL")
(export '(%set-funcallable-instance-function %make-funcallable-instance))

(in-package "VM")


;;;; Type based constants:

(eval-when (compile eval load)

(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  "Mask to extract the low tag bits from a pointer.")
  
(defconstant lowtag-limit (ash 1 lowtag-bits)
  "Exclusive upper bound on the value of the low tag bits from a
  pointer.")

(defconstant type-bits 8
  "Number of bits used in the header word of a data block for typeing.")

(defconstant type-mask (1- (ash 1 type-bits))
  "Mask to extract the type from a header word.")

); eval-when


(defparameter target-most-positive-fixnum (1- (ash 1 29))
  "most-positive-fixnum in the target architecture.")

(defparameter target-most-negative-fixnum (ash -1 29)
  "most-negative-fixnum in the target architecture.")


;;; The main types.  These types are represented by the low three bits of the
;;; pointer or immeditate object.
;;; 
(defenum (:suffix -type)
  even-fixnum
  function-pointer
  other-immediate-0
  list-pointer
  odd-fixnum
  structure-pointer
  other-immediate-1
  other-pointer)

;;; The heap types.  Each of these types is in the header of objects in
;;; the heap.
;;; 
(defenum (:suffix -type
	  :start (+ (ash 1 lowtag-bits) other-immediate-0-type)
	  :step (ash 1 (1- lowtag-bits)))
  bignum
  ratio
  single-float
  double-float
  complex
  
  simple-array
  simple-string
  simple-bit-vector
  simple-vector
  simple-array-unsigned-byte-2
  simple-array-unsigned-byte-4
  simple-array-unsigned-byte-8
  simple-array-unsigned-byte-16
  simple-array-unsigned-byte-32
  simple-array-single-float
  simple-array-double-float
  complex-string
  complex-bit-vector
  complex-vector
  complex-array
  
  code-header
  function-header
  closure-header
  funcallable-instance-header
  unused-function-header-1
  unused-function-header-2
  unused-function-header-3
  closure-function-header
  return-pc-header
  value-cell-header
  symbol-header
  base-char
  sap
  unbound-marker
  weak-pointer
  structure-header)


;;; The different vector subtypes.
;;; 
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)




;;;; Primitive data objects definition noise.

(eval-when (compile load eval)

(defstruct (prim-object-slot
	    (:conc-name slot-)
	    (:constructor %make-slot
			  (name docs rest-p length options))
	    (:make-load-form-fun :just-dump-it-normally))
  (name nil :type symbol)
  (docs nil :type (or null simple-string))
  (rest-p nil :type (member t nil))
  (offset 0 :type fixnum)
  (length 1 :type fixnum)
  (options nil :type list))

(defun make-slot (name &rest options
		       &key docs rest-p (length (if rest-p 0 1))
		       &allow-other-keys)
  (let ((options (copy-list options)))
    (remf options :docs)
    (remf options :rest-p)
    (remf options :length)
    (%make-slot name docs rest-p length options)))

(defstruct (primitive-object
	    (:make-load-form-fun :just-dump-it-normally))
  (name nil :type symbol)
  (header nil :type symbol)
  (lowtag nil :type symbol)
  (options nil :type list)
  (slots nil :type list)
  (size 0 :type fixnum)
  (variable-length nil :type (member t nil)))


(defvar *primitive-objects* nil)

); eval-when (compile load eval)


(defmacro define-primitive-object ((name &rest options
					 &key header lowtag
					 &allow-other-keys)
				   &rest slots)
  (setf options (copy-list options))
  (remf options :header)
  (remf options :lowtag)
  (let ((prim-obj
	 (make-primitive-object :name name
				:header header
				:lowtag lowtag
				:options options
				:slots (mapcar #'(lambda (slot)
						   (if (atom slot)
						       (make-slot slot)
						       (apply #'make-slot
							      slot)))
					       slots))))
    (collect ((forms) (exports))
      (let ((offset (if (primitive-object-header prim-obj) 1 0))
	    (variable-length nil))
	(dolist (slot (primitive-object-slots prim-obj))
	  (when variable-length
	    (error "~S is anything after a :rest-p t slot." slot))
	  (let* ((rest-p (slot-rest-p slot))
		 (offset-sym
		  (intern (concatenate 'simple-string
				       (string name)
				       "-"
				       (string (slot-name slot))
				       (if rest-p "-OFFSET" "-SLOT")))))
	    (forms `(defconstant ,offset-sym ,offset
		      ,@(when (slot-docs slot) (list (slot-docs slot)))))
	    (setf (slot-offset slot) offset)
	    (exports offset-sym)
	    (incf offset (slot-length slot))
	    (when rest-p (setf variable-length t))))
	(setf (primitive-object-variable-length prim-obj) variable-length)
	(unless variable-length
	  (let ((size (intern (concatenate 'simple-string
					   (string name)
					   "-SIZE"))))
	    (forms `(defconstant ,size ,offset
		      ,(format nil
			       "Number of slots used by each ~S~
			       ~@[~* including the header~]."
			       name header)))
	    (exports size)))
	(setf (primitive-object-size prim-obj) offset))
      `(eval-when (compile load eval)
	 (setf *primitive-objects*
	       (cons ',prim-obj
		     (delete ',name *primitive-objects*
			     :key #'primitive-object-name)))
	 (export ',(exports))
	 ,@(forms)))))

(defmacro define-for-each-primitive-object ((var) &body body)
  (let ((name (gensym)))
    `(macrolet ((,name (,var) ,@body))
       ,@(mapcar #'(lambda (x)
		     `(,name ,x))
		 *primitive-objects*))))


;;;; The primitive objects themselves.


(define-primitive-object (cons :lowtag list-pointer-type
			       :alloc-trans cons)
  (car :ref-vop car :ref-trans car
       :setf-vop c::set-car :set-trans c::%rplaca
       :init :arg)
  (cdr :ref-vop cdr :ref-trans cdr
       :setf-vop set-cdr :set-trans c::%rplacd
       :init :arg))

(define-primitive-object (structure :lowtag structure-pointer-type
				    :header structure-header-type
				    :alloc-trans make-structure)
  (slots :rest-p t))

(define-primitive-object (bignum :lowtag other-pointer-type
				 :header bignum-type
				 :alloc-trans bignum::%allocate-bignum)
  (digits :rest-p t :c-type "long"))

(define-primitive-object (ratio :lowtag other-pointer-type
				:header ratio-type
				:alloc-vop c::make-ratio
				:alloc-trans %make-ratio)
  (numerator :ref-vop numerator :init :arg)
  (denominator :ref-vop denominator :init :arg))

(define-primitive-object (single-float :lowtag other-pointer-type
				       :header single-float-type)
  (value :c-type "float"))

(define-primitive-object (double-float :lowtag other-pointer-type
				       :header double-float-type)
  (filler)
  (value :c-type "double" :length 2))

(define-primitive-object (complex :lowtag other-pointer-type
				  :header complex-type
				  :alloc-vop c::make-complex
				  :alloc-trans %make-complex)
  (real :ref-vop realpart :init :arg)
  (imag :ref-vop imagpart :init :arg))

(define-primitive-object (array :lowtag other-pointer-type
				:header t)
  (fill-pointer :type index
		:ref-trans %array-fill-pointer
		:ref-known (flushable foldable)
		:set-trans (setf %array-fill-pointer)
		:set-known (unsafe))
  (fill-pointer-p :type (member t nil)
		  :ref-trans %array-fill-pointer-p
		  :ref-known (flushable foldable)
		  :set-trans (setf %array-fill-pointer-p)
		  :set-known (unsafe))
  (elements :type index
	    :ref-trans %array-available-elements
	    :ref-known (flushable foldable)
	    :set-trans (setf %array-available-elements)
	    :set-known (unsafe))
  (data :type array
	:ref-trans %array-data-vector
	:ref-known (flushable foldable)
	:set-trans (setf %array-data-vector)
	:set-known (unsafe))
  (displacement :type (or index null)
		:ref-trans %array-displacement
		:ref-known (flushable foldable)
		:set-trans (setf %array-displacement)
		:set-known (unsafe))
  (displaced-p :type (member t nil)
	       :ref-trans %array-displaced-p
	       :ref-known (flushable foldable)
	       :set-trans (setf %array-displaced-p)
	       :set-known (unsafe))
  (dimensions :rest-p t))

(define-primitive-object (vector :lowtag other-pointer-type :header t)
  (length :ref-trans c::vector-length
	  :type index
	  :ref-known (flushable foldable))
  (data :rest-p t :c-type "unsigned long"))

(define-primitive-object (code :lowtag other-pointer-type :header t)
  (code-size :ref-vop c::code-code-size)
  (entry-points :ref-vop c::code-entry-points
		:set-vop c::set-code-entry-points)
  (debug-info :type t
	      :ref-trans di::code-debug-info
	      :ref-known (flushable)
	      :set-vop c::set-code-debug-info)
  (trace-table-offset)
  (constants :rest-p t))

(define-primitive-object (function-header :lowtag function-pointer-type
					  :header function-header-type)
  (self :ref-vop c::function-self :set-vop c::set-function-self)
  (next :ref-vop c::function-next :set-vop c::set-function-next)
  (name :ref-vop c::function-name
	:ref-known (flushable)
	:ref-trans %function-header-name
	:set-vop c::set-function-name)
  (arglist :ref-vop c::function-arglist
	   :ref-known (flushable)
	   :ref-trans lisp::%function-header-arglist
	   :set-vop c::set-function-arglist)
  (type :ref-vop c::function-type
	:ref-known (flushable)
	:ref-trans lisp::%function-header-type
	:set-vop c::set-function-type)
  (code :rest-p t :c-type "unsigned char"))

(define-primitive-object (return-pc :lowtag other-pointer-type :header t)
  (return-point :c-type "unsigned char" :rest-p t))

(define-primitive-object (closure :lowtag function-pointer-type
				  :header closure-header-type
				  :alloc-vop c::make-closure)
  (function :init :arg
	    :ref-vop c::closure-function
	    :ref-known (flushable)
	    :ref-trans %closure-function)
  (info :rest-p t :set-vop c::closure-init :ref-vop c::closure-ref))

(define-primitive-object (funcallable-instance
			  :lowtag function-pointer-type
			  :header funcallable-instance-header-type
			  :alloc-vop make-funcallable-instance
			  :alloc-trans %make-funcallable-instance)
  (function :init :arg
	    :set-vop set-funcallable-instance-function
	    :set-trans %set-funcallable-instance-function
	    :set-known (unsafe))
  (info :rest-p t))

(define-primitive-object (value-cell :lowtag other-pointer-type
				     :header value-cell-header-type
				     :alloc-vop make-value-cell
				     :alloc-trans make-value-cell)
  (value :set-vop value-cell-set
	 :set-trans value-cell-set
	 :set-known (unsafe)
	 :ref-vop value-cell-ref
	 :ref-trans value-cell-ref
	 :ref-known (flushable)
	 :init :arg))

(define-primitive-object (symbol :lowtag other-pointer-type
				 :header symbol-header-type)
  (value :set-trans %set-symbol-value
	 :setf-vop set)
  (function)
  (raw-function-addr :c-type "char *")
  (setf-function)
  (plist :ref-trans symbol-plist
	 :setf-vop %set-symbol-plist
	 :set-trans %set-symbol-plist)
  (name :ref-trans symbol-name)
  (package :ref-trans symbol-package
	   :setf-vop %set-symbol-package
	   :set-trans %set-symbol-package))

(define-primitive-object (sap :lowtag other-pointer-type
			      :header sap-type)
  (pointer :c-type "char *"))


(define-primitive-object (weak-pointer :lowtag other-pointer-type
				       :header weak-pointer-type
				       :alloc-trans c::%make-weak-pointer)
  (value :ref-trans c::%weak-pointer-value
	 :ref-known (flushable)
	 :set-trans (setf c::%weak-pointer-value)
	 :set-known (unsafe)
	 :init :arg)
  (broken :ref-trans c::%weak-pointer-broken
	  :ref-known (flushable)
	  :set-trans (setf c::%weak-pointer-broken)
	  :set-known (unsafe)
	  :init :arg)
  (next :c-type "struct weak_pointer *"))
  

;;; Other non-heap data blocks.

(define-primitive-object (binding)
  value
  symbol)

(define-primitive-object (unwind-block)
  (current-uwp :c-type "struct unwind_block *")
  (current-cont :c-type "lispobj *")
  current-code
  entry-pc)

(define-primitive-object (catch-block)
  (current-uwp :c-type "struct unwind_block *")
  (current-cont :c-type "lispobj *")
  current-code
  entry-pc
  tag
  (previous-catch :c-type "struct catch_block *")
  size)

