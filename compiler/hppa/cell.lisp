;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/hppa/cell.lisp,v 1.1 1992/07/13 03:48:20 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the HPPA
;;;
;;; Written by William Lott.
;;; 

(in-package "HPPA")


;;;; Data object definition macros.

(define-for-each-primitive-object (obj)
  (collect ((forms))
    (let ((lowtag (primitive-object-lowtag obj)))
      (dolist (slot (primitive-object-slots obj))
	(let* ((name (slot-name slot))
	       (offset (slot-offset slot))
	       (rest-p (slot-rest-p slot))
	       (slot-opts (slot-options slot))
	       (ref-trans (getf slot-opts :ref-trans))
	       (ref-vop (getf slot-opts :ref-vop ref-trans))
	       (set-trans (getf slot-opts :set-trans))
	       (setf-function-p (and (listp set-trans)
				     (= (length set-trans) 2)
				     (eq (car set-trans) 'setf)))
	       (setf-vop (getf slot-opts :setf-vop
			       (when setf-function-p
				 (intern (concatenate
					  'simple-string
					  "SET-"
					  (string (cadr set-trans)))))))
	       (set-vop (getf slot-opts :set-vop
			      (if setf-vop nil set-trans))))
	  (when ref-vop
	    (forms `(define-vop (,ref-vop ,(if rest-p 'slot-ref 'cell-ref))
				(:variant ,offset ,lowtag)
		      ,@(when ref-trans
			  `((:translate ,ref-trans))))))
	  (when (or set-vop setf-vop)
	    (forms `(define-vop ,(cond ((and rest-p setf-vop)
					(error "Can't automatically generate ~
					a setf VOP for :rest-p ~
					slots: ~S in ~S"
					       name
					       (primitive-object-name obj)))
				       (rest-p `(,set-vop slot-set))
				       ((and set-vop setf-function-p)
					(error "Setf functions (list ~S) must ~
					use :setf-vops."
					       set-trans))
				       (set-vop `(,set-vop cell-set))
				       (setf-function-p
					`(,setf-vop cell-setf-function))
				       (t
					`(,setf-vop cell-setf)))
		      (:variant ,offset ,lowtag)
		      ,@(when set-trans
			  `((:translate ,set-trans)))))))))
    (when (forms)
      `(progn
	 ,@(forms)))))



;;;; Symbol hacking VOPs:

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move object obj-temp)
    (loadw value obj-temp symbol-value-slot other-pointer-type)
    (let ((err-lab (generate-error-code vop unbound-symbol-error obj-temp)))
      (inst li unbound-marker-type temp)
      (inst bc := nil value temp err-lab))))

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
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-type)
    (inst li unbound-marker-type temp)
    (inst bc :<> not-p value temp target)))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))



;;;; Fdefinition (fdefn) objects.

(define-vop (safe-fdefn-function)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp)
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp fdefn-function-slot other-pointer-type)
    (let ((err-lab (generate-error-code vop undefined-symbol-error obj-temp)))
      (inst bc := nil value null-tn err-lab))))

(define-vop (set-fdefn-function)
  (:policy :fast-safe)
  (:translate (setf fdefn-function))
  (:args (function :scs (descriptor-reg) :target result)
	 (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (load-type type function (- function-pointer-type))
    (inst addi (- function-header-type) type type)
    (inst comb := type zero-tn normal-fn)
    (inst addi (- (ash function-header-code-offset word-shift)
		  function-pointer-type)
	  function lip)
    (inst li (make-fixup "closure_tramp" :foreign) lip)
    NORMAL-FN
    (storew function fdefn fdefn-function-slot other-pointer-type)
    (storew lip fdefn fdefn-raw-addr-slot other-pointer-type)
    (move function result)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-function-slot other-pointer-type)
    (inst li (make-fixup "undefined_tramp" :foreign) temp)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-type)
    (move fdefn result)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-type)
    (inst addi (* binding-size word-bytes) bsp-tn bsp-tn)
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-type)))

(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-type)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (inst addi (- (* binding-size word-bytes)) bsp-tn bsp-tn)))

(define-vop (unbind-to-here)
  (:args (where :scs (descriptor-reg any-reg)))
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (inst comb := where bsp-tn done :nullify t)
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))

    LOOP
    (inst comb := symbol zero-tn skip)
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-type)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

    SKIP
    (inst addi (* -2 word-bytes) bsp-tn bsp-tn)
    (inst comb :<> where bsp-tn loop :nullify t)
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))

    DONE))



;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset function-pointer-type
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-reffer set-funcallable-instance-info *
  funcallable-instance-info-offset function-pointer-type
  (descriptor-reg any-reg) * %set-funcallable-instance-info)



;;;; Structure hackery:

(define-vop (structure-length)
  (:policy :fast-safe)
  (:translate structure-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 structure-pointer-type)
    (inst srl res type-bits res)))

(define-vop (structure-ref slot-ref)
  (:variant structure-slots-offset structure-pointer-type)
  (:policy :fast-safe)
  (:translate structure-ref)
  (:arg-types structure (:constant index)))

(define-vop (structure-set slot-set)
  (:policy :fast-safe)
  (:translate structure-set)
  (:variant structure-slots-offset structure-pointer-type)
  (:arg-types structure (:constant index) *))

(define-full-reffer structure-index-ref * structure-slots-offset
  structure-pointer-type (descriptor-reg any-reg) * structure-ref)

(define-full-setter structure-index-set * structure-slots-offset
  structure-pointer-type (descriptor-reg any-reg) * structure-set)



;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-type
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-type
  (descriptor-reg any-reg) * code-header-set)
