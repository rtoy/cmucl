;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/objdef.lisp,v 1.21 1992/12/13 14:47:40 wlott Exp $")
;;;
;;; **********************************************************************
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
	  byte-code-function-type byte-code-closure-type
	  dylan-function-header-type
	  value-cell-header-type symbol-header-type base-char-type
	  sap-type unbound-marker-type weak-pointer-type
	  structure-header-type funcallable-instance-header-type
	  fdefn-type vector-normal-subtype
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

(export '(%numerator %denominator %realpart %imagpart
	  %code-code-size %code-entry-points %code-debug-info
	  %function-self %function-next %function-name %function-arglist
	  %function-type))

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
  byte-code-function
  byte-code-closure
  dylan-function-header
  closure-function-header
  #-gengc return-pc-header #+gengc forwarding-pointer
  value-cell-header
  symbol-header
  base-char
  sap
  unbound-marker
  weak-pointer
  structure-header
  fdefn)


;;; The different vector subtypes.
;;; 
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)


;;;; The primitive objects themselves.

(define-primitive-object (cons :lowtag list-pointer-type
			       :alloc-trans cons)
  (car :ref-trans car :set-trans c::%rplaca :init :arg)
  (cdr :ref-trans cdr :set-trans c::%rplacd :init :arg))

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
				:alloc-trans %make-ratio)
  (numerator :ref-known (flushable movable)
	     :ref-trans %numerator
	     :init :arg)
  (denominator :ref-known (flushable movable)
	       :ref-trans %denominator
	       :init :arg))

(define-primitive-object (single-float :lowtag other-pointer-type
				       :header single-float-type)
  (value :c-type "float"))

(define-primitive-object (double-float :lowtag other-pointer-type
				       :header double-float-type)
  (filler)
  (value :c-type "double" :length 2))

(define-primitive-object (complex :lowtag other-pointer-type
				  :header complex-type
				  :alloc-trans %make-complex)
  (real :ref-known (flushable movable) :ref-trans %realpart :init :arg)
  (imag :ref-known (flushable movable) :ref-trans %imagpart :init :arg))

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
  (code-size :ref-known (flushable movable)
	     :ref-trans %code-code-size)
  (entry-points :type (or function null)
		:ref-known (flushable)
		:ref-trans %code-entry-points
		:set-known (unsafe)
		:set-trans (setf %code-entry-points))
  (debug-info :type t
	      :ref-known (flushable)
	      :ref-trans %code-debug-info
	      :set-known (unsafe)
	      :set-trans (setf %code-debug-info))
  (trace-table-offset)
  (constants :rest-p t))

(define-primitive-object (fdefn :lowtag other-pointer-type
				:header fdefn-type)
  (name :ref-trans fdefn-name)
  (function :ref-trans fdefn-function)
  (raw-addr :c-type "char *"))

(define-primitive-object (function :lowtag function-pointer-type
				   :header function-header-type)
  #-gengc (self :ref-trans %function-self :set-trans (setf %function-self))
  #+gengc (entry-point :c-type "char *")
  (next :type (or function null)
	:ref-known (flushable)
	:ref-trans %function-next
	:set-known (unsafe)
	:set-trans (setf %function-next))
  (name :ref-known (flushable)
	:ref-trans %function-name
	:set-known (unsafe)
	:set-trans (setf %function-name))
  (arglist :ref-known (flushable)
	   :ref-trans %function-arglist
	   :set-known (unsafe)
	   :set-trans (setf %function-arglist))
  (type :ref-known (flushable)
	:ref-trans %function-type
	:set-known (unsafe)
	:set-trans (setf %function-type))
  (code :rest-p t :c-type "unsigned char"))

#-gengc
(define-primitive-object (return-pc :lowtag other-pointer-type :header t)
  (return-point :c-type "unsigned char" :rest-p t))

(define-primitive-object (closure :lowtag function-pointer-type
				  :header closure-header-type)
  #-gengc (function :init :arg :ref-trans %closure-function)
  #+gengc (entry-point :c-type "char *")
  (info :rest-p t))

(define-primitive-object (funcallable-instance
			  :lowtag function-pointer-type
			  :header funcallable-instance-header-type
			  :alloc-trans %make-funcallable-instance)
  #-gengc (function :init :arg
		    :set-trans %set-funcallable-instance-function)
  #+gengc (entry-point :c-type "char *")
  (info :rest-p t))

(define-primitive-object (value-cell :lowtag other-pointer-type
				     :header value-cell-header-type
				     :alloc-trans make-value-cell)
  (value :set-trans value-cell-set
	 :set-known (unsafe)
	 :ref-trans value-cell-ref
	 :ref-known (flushable)
	 :init :arg))

(define-primitive-object (symbol :lowtag other-pointer-type
				 :header symbol-header-type
				 :alloc-trans make-symbol)
  (value :set-trans %set-symbol-value
	 :init :unbound)
  unused
  (plist :ref-trans symbol-plist
	 :set-trans %set-symbol-plist
	 :init :null)
  (name :ref-trans symbol-name :init :arg)
  (package :ref-trans symbol-package
	   :set-trans %set-symbol-package
	   :init :null))

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

#+gengc
(define-primitive-object (sigcontext-chain)
  (scp :c-type "struct sigcontext *")
  (next :c-type "struct sigcontext_chain *"))

#+gengc
(define-primitive-object (mutator)
  (thread :c-type "struct thread *")
  ;; Signal control magic.
  (foreign-fn-call-active :c-type "boolean")
  (interrupts-enabled :c-type "boolean")
  (interrupt-pending :c-type "boolean")
  (pending-signal :c-type "int")
  (pending-code :c-type "int")
  (pending-mask :c-type "unsigned long")
  (sigcontext-chain :c-type "struct sigcontext_chain *")
  ;; Stacks.
  (control-stack-base :c-type "lispobj *")
  (control-stack-pointer :c-type "lispobj *")
  (control-stack-end :c-type "lispobj *")
  (control-frame-pointer :c-type "lispobj *")
  (current-unwind-protect :c-type "struct unwind_block *")
  (current-catch-block :c-type "struct catch_block *")
  (binding-stack-base :c-type "struct binding *")
  (binding-stack-pointer :c-type "struct binding *")
  (binding-stack-end :c-type "struct binding *")
  (number-stack-base :c-type "char *")
  (number-stack-pointer :c-type "char *")
  (number-stack-end :c-type "char *")
  (eval-stack)
  (eval-stack-top)
  ;; Allocation stuff.
  (nursery-start :c-type "lispobj *")
  (nursery-fill-pointer :c-type "lispobj *")
  (nursery-end :c-type "lispobj *")
  (storebuf-start :c-type "lispobj **")
  (storebuf-fill-pointer :c-type "lispobj **")
  (storebuf-end :c-type "lispobj **")
  (words-consed :c-type "unsigned long"))
