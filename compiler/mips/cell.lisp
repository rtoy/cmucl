;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/cell.lisp,v 1.11 1990/02/20 19:39:39 wlott Exp $
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "VM")

(export '(cons-structure cons-size cons-car-slot cons-cdr-slot

	  bignum-structure bignum-size bignum-digits-offset

	  ratio-structure ratio-size ratio-numerator-slot
	  ratio-denominator-slot

	  single-float-structure single-float-size single-float-value-slot

	  double-float-structure double-float-size double-float-value-slot

	  complex-structure complex-size complex-real-slot complex-imag-slot

	  array-structure array-fill-pointer-slot array-elements-slot
	  array-data-slot array-displacement-slot array-displaced-p-slot
	  array-dimensions-offset

	  vector-structure vector-length-slot vector-data-offset

	  code-structure code-code-size-slot code-entry-points-slot
	  code-debug-info-slot code-constants-offset

	  function-header-structure function-header-self-slot
	  function-header-next-slot function-header-name-slot
	  function-header-arglist-slot function-header-type-slot
	  function-header-code-offset

	  return-pc-structure return-pc-return-point-offset

	  closure-structure closure-function-slot closure-info-offset

	  value-cell-structure value-cell-size value-cell-value-slot

	  symbol-structure symbol-size symbol-value-slot
	  symbol-function-slot symbol-plist-slot symbol-name-slot
	  symbol-package-slot))


(in-package "C")


;;;; Data object definition macros.

(eval-when (compile eval load)
  (defun parse-slot (slot)
    (multiple-value-bind
	(name props)
	(if (atom slot)
	    (values slot nil)
	    (values (car slot) (cdr slot)))
      (values name
	      (getf props :rest)
	      (getf props :boxed t)
	      (getf props :ref-vop)
	      (getf props :ref-trans)
	      (getf props :set-vop)
	      (getf props :setf-vop)
	      (getf props :set-trans)
	      (getf props :docs)
	      (getf props :init :zero)))))

(defmacro defslots ((name &key header (lowtag 0) alloc-vop alloc-trans)
		    &rest slots)
  (let ((compile-time nil)
	(load-time nil)
	(index (if header 1 0))
	(slot-names (if header '(header)))
	(did-rest nil)
	(init-forms nil)
	(init-args nil)
	(need-unbound-marker nil))
    (dolist (slot slots)
      (when did-rest
	(error "Rest slot ~S in defslots of ~S is not the last one."
	       did-rest name))
      (multiple-value-bind
	  (slot-name rest boxed ref-vop ref-trans
		     set-vop setf-vop set-trans docs init)
	  (parse-slot slot)
	(let ((const (intern (concatenate 'simple-string
					  (string name)
					  "-"
					  (string slot-name)
					  (if rest "-OFFSET" "-SLOT")))))
	  (push `(defconstant ,const
		   ,index
		   ,@(if docs (list docs)))
		compile-time)
	  (when (or set-vop setf-vop)
	    (push `(define-vop ,(cond (rest `(,set-vop slot-set))
				      (set-vop `(,set-vop cell-set))
				      (t `(,setf-vop cell-setf)))
		     (:variant ,const ,lowtag)
		     ,@(when set-trans
			 `((:translate ,set-trans))))
		  load-time))
	  (when ref-vop
	    (push `(define-vop (,ref-vop ,(if rest 'slot-ref 'cell-ref))
		     (:variant ,const ,lowtag)
		     ,@(when ref-trans
			 `((:translate ,ref-trans))))
		  load-time))
	  (case init
	    (:zero)
	    (:null
	     (push `(storew null-tn result ,const ,lowtag) init-forms))
	    (:unbound
	     (setf need-unbound-marker t)
	     (push `(storew temp result ,const ,lowtag) init-forms))
	    (:arg
	     (push slot-name init-args)
	     (push `(storew ,slot-name result ,const ,lowtag) init-forms))))
	(push (if (or (not boxed) rest)
		  `(,slot-name
		    ,@(if (not boxed) '(:boxed nil))
		    ,@(if rest '(:rest t)))
		  slot-name)
	      slot-names)
	(if rest
	    (setf did-rest slot-name)
	    (incf index))))
    (let ((size (intern (concatenate 'simple-string
				     (string name)
				     (if did-rest "-BASE-SIZE" "-SIZE")))))
      (push `(defconstant ,size ,index
	       ,(format nil
			"Number of slots used by each ~S~
			~@[ including the header~]~@[ excluding any data~]."
			name header did-rest))
	    compile-time)
      (when alloc-vop
	(push `(define-vop (,alloc-vop)
		 (:args ,@(when did-rest
			    `((extra-words :scs (any-reg descriptor-reg))))
			,@(mapcar #'(lambda (name)
				      `(,name :scs (any-reg descriptor-reg)))
				  (nreverse init-args)))
		 (:temporary (:scs (non-descriptor-reg) :type random)
			     ndescr
			     ,@(when (or need-unbound-marker header did-rest)
				 '(temp)))
		 (:temporary (:scs (descriptor-reg) :to (:result 0)
				   :target real-result) result)
		 (:results (real-result :scs (descriptor-reg)))
		 (:policy :fast-safe)
		 ,@(when alloc-trans
		     `((:translate ,alloc-trans)))
		 (:generator 37
		   (pseudo-atomic (ndescr)
		     (inst addiu result alloc-tn ,lowtag)
		     ,@(cond ((and header did-rest)
			      `((inst addiu temp extra-words ,size)
				(inst addu alloc-tn alloc-tn temp)
				(inst sll temp temp
				      (- vm:type-bits vm:word-bits))
				(inst ori temp temp ,header)
				(storew temp result 0 ,lowtag)
				(inst addiu alloc-tn alloc-tn vm:lowtag-mask)
				(loadi temp (lognot vm:lowtag-mask))
				(inst and alloc-tn alloc-tn temp)))
			     (did-rest
			      (error ":REST T with no header in ~S?" name))
			     (header
			      `((inst addiu alloc-tn alloc-tn
				      (vm:pad-data-block ,size))
				(loadi temp (logior (ash ,size vm:type-bits)
						    ,header))
				(storew temp result 0 ,lowtag)))
			     (t
			      `((inst addiu alloc-tn alloc-tn
				      (vm:pad-data-block ,size)))))
		     ,@(when need-unbound-marker
			 `((loadi temp vm:unbound-marker-type)))
		     ,@(nreverse init-forms)
		     (move real-results result))))
	      load-time)))
    `(progn
       (eval-when (compile load eval)
	 ,@(nreverse compile-time))
       ,@(nreverse load-time)
       (defconstant ,(intern (concatenate 'simple-string
					  (string name)
					  "-STRUCTURE"))
	 ',(reverse slot-names)))))

(defslots (cons :lowtag list-pointer-type
		:alloc-vop cons-vop :alloc-trans cons)
  (car :ref-vop car :ref-trans car
       :setf-vop set-car :set-trans %rplaca
       :init :arg)
  (cdr :ref-vop cdr :ref-trans cdr
       :setf-vop set-cdr :set-trans %rplacd
       :init :arg))


(defslots (bignum :lowtag other-pointer-type :header bignum-type)
  (digits :rest t :boxed nil))

(defslots (ratio :lowtag other-pointer-type :header ratio-type)
  numerator
  denominator)

(defslots (single-float :lowtag other-pointer-type :header single-float-type)
  value)

(defslots (double-float :lowtag other-pointer-type :header double-float-type)
  value
  more-value)

(defslots (complex :lowtag other-pointer-type :header complex-type)
  real
  imag)

(defslots (array :lowtag other-pointer-type :header t)
  fill-pointer
  elements
  data
  displacement
  displaced-p
  (dimensions :rest t))

(defslots (vector :lowtag other-pointer-type :header t)
  length
  (data :rest t :boxed nil))

(defslots (code :lowtag other-pointer-type :header t)
  code-size
  entry-points
  debug-info
  (constants :rest t))

(defslots (function-header :lowtag function-pointer-type
			   :header function-header-type)
  self
  next
  name
  arglist
  type
  (code :rest t :boxed nil))

(defslots (return-pc :lowtag other-pointer-type :header t)
  (return-point :boxed nil :rest t))

(defslots (closure :lowtag function-pointer-type :header closure-header-type
		   :alloc-vop make-closure)
  (function :init :arg)
  (info :rest t :set-vop closure-init :ref-vop closure-ref))

(defslots (value-cell :lowtag other-pointer-type :header value-cell-type
		      :alloc-vop make-value-cell)
  (value :set-vop value-cell-set :ref-vop value-cell-ref :init :arg))

(defslots (symbol :lowtag other-pointer-type :header symbol-header-type
		  :alloc-vop make-symbol-vop :alloc-trans %make-symbol)
  (value :setf-vop set :set-trans set :init :unbound)
  (function :setf-vop set-symbol-function :set-trans %sp-set-definition
	    :init :unbound)
  (plist :ref-vop symbol-plist :ref-trans symbol-plist
	 :setf-vop set-symbol-plist :set-trans %sp-set-plist
	 :init :null)
  (name :ref-vop symbol-name :ref-trans symbol-name :init :arg)
  (package :ref-vop symbol-package :ref-trans symbol-package
	   :setf-vop set-package :init :null))





;;;; Symbol hacking VOPs:

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:node-var node)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  #+nil
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot)
    (let ((err-lab (generate-error-code node clc::error-symbol-unbound
					obj-temp)))
      (test-special-value value temp '%trap-object err-lab nil))))

;;; With Symbol-Function, we check that the result is a function, so NIL is
;;; always un-fbound.
;;;
(define-vop (symbol-function checked-cell-ref)
  (:translate symbol-function)
  #+nil
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp symbol-function-slot)
    (let ((err-lab (generate-error-code node clc::error-symbol-undefined
					obj-temp)))
      (test-simple-type value temp err-lab t system:%function-type))))


;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  #+nil
  (:generator 9
    (loadw value object (/ clc::symbol-value 4))
    (test-special-value value temp '%trap-object target (not not-p))))


;;; SYMBOL isn't a primitive type, so we can't use it for the arg restriction
;;; on the symbol case of fboundp.  Instead, we transform to a funny function.

(defknown fboundp/symbol (t) boolean (flushable))
;;;
(deftransform fboundp ((x) (symbol))
  '(fboundp/symbol x))
;;;
(define-vop (fboundp/symbol boundp-frob)
  (:translate fboundp/symbol)
  #+nil
  (:generator 10
    (loadw value object (/ clc::symbol-definition 4))
    (test-simple-type value temp target not-p system:%function-type)))

#+nil
(def-source-transform makunbound (x)
  `(set ,x (%primitive make-immediate-type 0 system:%trap-type)))


(define-vop (fast-symbol-value cell-ref)
  (:variant vm:symbol-value-slot vm:other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (fast-symbol-function cell-ref)
  (:variant vm:symbol-function-slot vm:other-pointer-type)
  (:policy :fast)
  (:translate symbol-function))


#+nil
(define-miscop bind (val symbol) :results ())

#+nil
(define-miscop unbind (num) :results ())



;;;; Structure hackery:

;;; ### This is only necessary until we get real structures up and running.

(define-vop (structure-ref slot-ref)
  (:variant vm:vector-data-offset vm:other-pointer-type))

(define-vop (structure-set slot-set)
  (:variant vm:vector-data-offset vm:other-pointer-type))
