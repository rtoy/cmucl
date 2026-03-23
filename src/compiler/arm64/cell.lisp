;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/cell.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for ARM64.
;;;
;;; Originally written for SPARC by Rob MacLachlan, converted by William Lott.
;;; Ported to ARM64.
;;;

(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; Data object ref/set stuff.

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (emit-not-implemented)
    (loadw result object offset lowtag)))

(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (emit-not-implemented)
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
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (emit-not-implemented)
    (move obj-temp object)
    (loadw value obj-temp vm:symbol-value-slot vm:other-pointer-type)
    (let ((err-lab (generate-error-code vop unbound-symbol-error obj-temp)))
      ;; inst cmp: alias for SUBS with Rd=XZR.
      ;; Defined in checkpoint as (define-instruction-macro cmp (rn src) `(inst subs null-tn ,rn ,src))
      (inst cmp value vm:unbound-marker-type)
      ;; inst b :eq label: conditional branch B.cond.
      ;; Defined in checkpoint as (define-instruction b (segment cond-or-target &optional target))
      (inst b :eq err-lab))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (emit-not-implemented)
    (loadw value object vm:symbol-value-slot vm:other-pointer-type)
    ;; inst cmp: SUBS XZR, value, unbound-marker-type
    (inst cmp value vm:unbound-marker-type)
    ;; inst b :eq/:ne target: B.cond conditional branch
    (inst b (if not-p :eq :ne) target)))

(define-vop (fast-symbol-value cell-ref)
  (:variant vm:symbol-value-slot vm:other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg null)))
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 2
    (emit-not-implemented)
    ;; The symbol-hash slot of NIL holds NIL because it is also the cdr
    ;; slot, so we strip the two low tag bits to ensure it is a fixnum.
    (loadw res symbol symbol-hash-slot other-pointer-type)
    ;; inst bic has NO immediate form (invertp=t excludes integer src types in checkpoint).
    ;; Use inst and with the bitwise complement instead:
    ;;   AND res, res, (lognot fixnum-tag-mask)  -- keeps all bits except the tag bits.
    (inst and res res (lognot vm:fixnum-tag-mask))))

(define-vop (%set-symbol-hash cell-set)
  (:translate %set-symbol-hash)
  (:variant symbol-hash-slot other-pointer-type))


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
    (emit-not-implemented)
    (move obj-temp object)
    (loadw value obj-temp fdefn-function-slot other-pointer-type)
    (let ((err-lab (generate-error-code vop undefined-symbol-error obj-temp)))
      ;; inst cmp: SUBS XZR, value, null-tn
      (inst cmp value null-tn)
      ;; inst b :eq err-lab: B.EQ conditional branch
      (inst b :eq err-lab))))

(define-vop (set-fdefn-function)
  (:policy :fast-safe)
  (:translate (setf fdefn-function))
  (:args (function :scs (descriptor-reg) :target result)
	 (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (emit-not-implemented)
    (let ((normal-fn (gen-label)))
      ;; load-type: defined in arm64-macros as (inst ldurb target source offset).
      ;; Reads the low type byte of the object header word.
      (load-type type function (- function-pointer-type))
      ;; inst cmp: SUBS XZR, type, function-header-type
      (inst cmp type function-header-type)
      ;; inst mov: macro expanding to (inst orr rd null-tn src).
      ;; Move function -> temp unconditionally before the branch (no delay slot on ARM64).
      (inst mov temp function)
      ;; inst b :eq normal-fn: skip closure-tramp load if already a plain function
      (inst b :eq normal-fn)
      ;; inst li: materialise an assembly-routine address into a register.
      ;; Used throughout arm64-macros for loading fixup/immediate values.
      (inst li temp (make-fixup 'closure-tramp :assembly-routine))
      (emit-label normal-fn)
      (storew function fdefn fdefn-function-slot other-pointer-type)
      (storew temp fdefn fdefn-raw-addr-slot other-pointer-type)
      ;; inst mov: ORR result, XZR, function
      (inst mov result function))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (emit-not-implemented)
    (storew null-tn fdefn fdefn-function-slot other-pointer-type)
    ;; inst li: load assembly-routine address (arm64-macros pattern)
    (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-type)
    ;; inst mov: ORR result, XZR, fdefn
    (inst mov result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (emit-not-implemented)
    (loadw temp symbol vm:symbol-value-slot vm:other-pointer-type)
    ;; inst add: ADD bsp-tn, bsp-tn, #imm.
    ;; Defined in checkpoint as (def add 1 0 0 nil t).
    (inst add bsp-tn bsp-tn (* 2 vm:word-bytes))
    (storew temp bsp-tn (- vm:binding-value-slot vm:binding-size))
    (storew symbol bsp-tn (- vm:binding-symbol-slot vm:binding-size))
    (storew val symbol vm:symbol-value-slot vm:other-pointer-type)))


(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (emit-not-implemented)
    (loadw symbol bsp-tn (- vm:binding-symbol-slot vm:binding-size))
    (loadw value bsp-tn (- vm:binding-value-slot vm:binding-size))
    (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
    ;; zero-tn: ARM64 XZR alias used throughout the backend
    (storew zero-tn bsp-tn (- vm:binding-symbol-slot vm:binding-size))
    ;; inst sub: SUB bsp-tn, bsp-tn, #imm.
    ;; Defined in checkpoint as (def sub 1 1 0 neg t).
    (inst sub bsp-tn bsp-tn (* 2 vm:word-bytes))))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (emit-not-implemented)
    (let ((loop (gen-label))
	  (skip (gen-label))
	  (done (gen-label)))
      (move where arg)
      ;; inst cmp: SUBS XZR, where, bsp-tn
      (inst cmp where bsp-tn)
      ;; inst b :eq done: skip loop body entirely if already at target
      (inst b :eq done)

      (emit-label loop)
      (loadw symbol bsp-tn (- vm:binding-symbol-slot vm:binding-size))
      ;; inst cbz symbol skip: Compare and Branch if Zero.
      ;; Defined in checkpoint as format-compare-branch with op=0 (CBZ).
      ;; Replaces the SPARC (inst cmp symbol) + (inst b :eq skip) pair.
      (inst cbz symbol skip)
      (loadw value bsp-tn (- vm:binding-value-slot vm:binding-size))
      (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
      (storew zero-tn bsp-tn (- vm:binding-symbol-slot vm:binding-size))

      (emit-label skip)
      ;; inst sub: SUB bsp-tn, bsp-tn, #imm
      (inst sub bsp-tn bsp-tn (* 2 vm:word-bytes))
      ;; inst cmp + inst b :ne loop: loop until where == bsp-tn
      (inst cmp where bsp-tn)
      (inst b :ne loop)

      (emit-label done))))



;;;; Closure indexing.

(define-vop (closure-index-ref word64-index-ref)
  (:variant vm:closure-info-offset vm:function-pointer-type)
  (:translate %closure-index-ref))

(define-vop (funcallable-instance-info word64-index-ref)
  (:variant funcallable-instance-info-offset vm:function-pointer-type)
  (:translate %funcallable-instance-info))

(define-vop (set-funcallable-instance-info word64-index-set)
  (:variant funcallable-instance-info-offset function-pointer-type)
  (:translate %set-funcallable-instance-info))

(define-vop (funcallable-instance-lexenv cell-ref)
  (:variant funcallable-instance-lexenv-slot function-pointer-type))


(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset function-pointer-type))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset function-pointer-type))


;;;; Value Cell hackery.

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-type))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-type))



;;;; Instance hackery:

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (emit-not-implemented)
    (loadw temp struct 0 instance-pointer-type)
    ;; inst lsr: Logical Shift Right.
    ;; Defined in checkpoint as (def lsr 1 63) with (segment rd rn shift).
    ;; LSR rd, temp, vm:type-bits  -- shifts header word right to extract length field.
    (inst lsr res temp vm:type-bits)))

(define-vop (instance-ref slot-ref)
  (:variant instance-slots-offset instance-pointer-type)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:arg-types instance (:constant index)))

(define-vop (instance-set slot-set)
  (:policy :fast-safe)
  ;; This translation is disabled because %instance-set needs a return value
  ;; and this VOP doesn't return anything.  See SPARC notes for context.
  ;;(:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance (:constant index) *))

(define-vop (instance-index-ref word64-index-ref)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance positive-fixnum))

(define-vop (instance-index-set word64-index-set)
  (:policy :fast-safe)
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance positive-fixnum *))



;;;; Code object frobbing.

(define-vop (code-header-ref word64-index-ref)
  (:translate code-header-ref)
  (:policy :fast-safe)
  (:variant 0 other-pointer-type))

(define-vop (code-header-set word64-index-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:variant 0 other-pointer-type))
