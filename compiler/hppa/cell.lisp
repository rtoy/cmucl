;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/hppa/cell.lisp,v 1.3 1993/01/15 01:33:53 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the HPPA
;;;
;;; Written by William Lott.
;;; 

(in-package "HPPA")


;;;; Data object ref/set stuff.

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (loadw result object offset lowtag)))

(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (storew value object offset lowtag)))



;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
;;;
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-type))

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

(define-vop (fdefn-function cell-ref)
  (:variant fdefn-function-slot other-pointer-type))

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
    (inst addi (- (ash function-code-offset word-shift) function-pointer-type)
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

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset function-pointer-type
  (descriptor-reg any-reg) * %set-funcallable-instance-info)


(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset function-pointer-type))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset function-pointer-type))



;;;; Value Cell hackery.

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-type))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-type))



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
