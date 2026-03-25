;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/alloc.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the ARM64 port.
;;;
;;; Written by William Lott (SPARC original).
;;; Ported to ARM64.
;;;
;;; Porting notes:
;;;
;;;   SPARC inst SLLN  -> ARM64 inst LSL  (logical shift left, immediate)
;;;   SPARC inst SRLN  -> ARM64 inst LSR  (logical shift right, immediate)
;;;   SPARC inst OR    -> ARM64 inst ORR
;;;   SPARC inst AND   -> ARM64 inst AND
;;;   SPARC inst ADD   -> ARM64 inst ADD
;;;   SPARC inst LI    -> ARM64 inst LI   (materialise integer into register)
;;;   SPARC inst ST/LD -> ARM64 STOREW/LOADW macros (unchanged interface)
;;;
;;;   The control stack on AArch64 grows downward, but the allocation
;;;   macro and with-fixed-allocation already account for this.
;;;
;;;   SPARC uses TRAP for the allocation trap; ARM64 uses UDF (already
;;;   embedded in the ALLOCATION macro in arm64-macros.lisp).

(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; Dynamic-Extent.

;;;
;;; Take an arg where to move the stack pointer instead of returning
;;; it via :results, because the former generates a single move.
;;;
(define-vop (%dynamic-extent-start)
  (:args (saved-stack-pointer :scs (any-reg control-stack)))
  (:results)
  (:policy :safe)
  (:generator 0
    (emit-not-implemented)
    (sc-case saved-stack-pointer
      (control-stack
       (let ((offset (tn-offset saved-stack-pointer)))
	 (storew csp-tn cfp-tn offset)))
      (any-reg
       (move saved-stack-pointer csp-tn)))))

(define-vop (%dynamic-extent-end)
  (:args (saved-stack-pointer :scs (any-reg control-stack)))
  (:results)
  (:policy :safe)
  (:generator 0
    (emit-not-implemented)
    (sc-case saved-stack-pointer
      (control-stack
       (let ((offset (tn-offset saved-stack-pointer)))
	 (loadw csp-tn cfp-tn offset)))
      (any-reg
       (move csp-tn saved-stack-pointer)))))


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:scs (non-descriptor-reg)) alloc-temp)
  (:info num dynamic-extent)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (emit-not-implemented)
    (cond ((zerop num)
	   (move result null-tn))
	  ((and star (= num 1))
	   (move result (tn-ref-tn things)))
	  (t
	   (macrolet
	       ((maybe-load (tn)
		  (once-only ((tn tn))
		    `(sc-case ,tn
		       ((any-reg descriptor-reg zero null)
			,tn)
		       (control-stack
			(load-stack-tn temp ,tn alloc-temp)
			temp)))))
	     (let* ((cons-cells (if star (1- num) num))
		    (alloc (* (pad-data-block cons-size) cons-cells)))
	       (pseudo-atomic ()
		 (allocation res alloc list-pointer-type
			     :stack-p dynamic-extent
			     :temp-tn alloc-temp)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (storew (maybe-load (tn-ref-tn things)) ptr
			   cons-car-slot list-pointer-type)
		   (setf things (tn-ref-across things))
		   (inst add ptr ptr (pad-data-block cons-size))
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-type))
		 (storew (maybe-load (tn-ref-tn things)) ptr
			 cons-car-slot list-pointer-type)
		 (storew (if star
			     (maybe-load (tn-ref-tn (tn-ref-across things)))
			     null-tn)
			 ptr cons-cdr-slot list-pointer-type))
	       (move result res)))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))


;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
	 (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) size)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:generator 100
    (emit-not-implemented)
    ;; boxed = (boxed-arg + (1+ code-trace-table-offset-slot)) rounded
    ;; down to a lispobj-aligned boundary.
    (inst add boxed boxed-arg (fixnumize (1+ code-trace-table-offset-slot)))
    (inst and boxed boxed (lognot lowtag-mask)) ; clear low tag bits
    ;; unboxed = ceil(unboxed-arg / word-bytes) * word-bytes, as a byte count.
    ;; unboxed-arg is a tagged fixnum; shift off the fixnum tag to get bytes.
    ;; Then round up to the next lispobj-aligned boundary (add lowtag-mask,
    ;; then clear the low bits).
    (inst lsr unboxed unboxed-arg vm:fixnum-tag-bits) ; untag -> raw byte count
    (inst add unboxed unboxed lowtag-mask)             ; round up
    (inst and unboxed unboxed (lognot lowtag-mask)) ; align down
    (pseudo-atomic ()
      ;; Figure out how much space we really need and allocate it.
      (inst add size boxed unboxed)
      (allocation result size other-pointer-type :temp-tn ndescr)
      ;; Build the code-header word:
      ;;   header = (boxed << (type-bits - word-shift)) | code-header-type
      ;; SPARC used SLLN (logical-shift-left); ARM64 uses LSL.
      ;; boxed is an aligned byte count (tag bits cleared by bic above),
      ;; so shift by (- type-bits word-shift) to place the word count in
      ;; the header type field.
      (inst lsl ndescr boxed (- type-bits word-shift))
      ;; ORR with a register to combine the shifted boxed count with
      ;; code-header-type.  LI materialises the type byte into size
      ;; (free after the allocation call above) to avoid relying on
      ;; code-header-type being a valid ARM64 logical immediate.
      (inst li size code-header-type)
      (inst orr ndescr ndescr size)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (storew null-tn result code-debug-info-slot other-pointer-type))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (emit-not-implemented)
    (with-fixed-allocation (result temp fdefn-type fdefn-size)
      ;; For the linkage-table stuff, we need to look up the address
      ;; of undefined_tramp from the linkage table instead of using
      ;; the address directly.
      (inst li temp (make-fixup 'undefined-tramp
				:assembly-routine))
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))


(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length dynamic-extent)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (emit-not-implemented)
    (let ((size (+ length closure-info-offset)))
      (with-fixed-allocation (result temp closure-header-type size
				     :lowtag function-pointer-type
				     :stack-p dynamic-extent)
	(storew function result closure-function-slot function-pointer-type)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (emit-not-implemented)
    (with-fixed-allocation
	(result temp value-cell-header-type value-cell-size)
      (storew value result value-cell-value-slot other-pointer-type))))


;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (emit-not-implemented)
    (inst li result unbound-marker-type)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag dynamic-extent)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (emit-not-implemented)
    (with-fixed-allocation (result temp type words :lowtag lowtag :stack-p dynamic-extent)
      )))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:scs (any-reg)) temp)
  (:generator 6
    (emit-not-implemented)
    ;; bytes = (extra + (1+ words)) * word-bytes  [extra is a tagged fixnum]
    ;; (* (1+ words) word-bytes) is always a multiple of word-bytes (8), which
    ;; is a multiple of fixnum-scale (4), so adding it preserves the fixnum tag.
    (inst add bytes extra (* (1+ words) word-bytes))
    ;; Build the header word.
    ;; SPARC: SLLN header bytes (- type-bits vm:fixnum-tag-bits)
    ;;   because bytes is still a tagged fixnum at this point.
    ;; ARM64: LSL is the equivalent.
    (inst lsl header bytes (- type-bits vm:fixnum-tag-bits))
    (inst add header header (+ (ash -2 type-bits) type))
    ;; Round bytes down to a lispobj-aligned boundary.
    (inst and bytes bytes (lognot lowtag-mask))
    (pseudo-atomic ()
      (allocation result bytes lowtag :temp-tn temp)
      (storew header result 0 lowtag))))
