;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/c-call.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;; Ported to ARM64 from the SPARC backend.
;;;
;;; ARM64 (AArch64) C ABI summary:
;;;   - Integer/pointer arguments: X0-X7  (up to 8 integer/pointer args in regs)
;;;   - FP arguments:              D0-D7  (up to 8 FP args in separate FP regs)
;;;   - Integer/pointer results:   X0 (single), X0+X1 (128-bit / multi-value)
;;;   - FP results:                D0 (single) or D0+D1
;;;   - 64-bit integers are native; no need to split across two 32-bit regs.
;;;   - The stack must be 16-byte aligned before a BLR/BL call instruction.
;;;   - The C ABI "home area" (register save area) is not required on AArch64;
;;;     the caller only needs to allocate stack space for arguments that spill
;;;     beyond the 8 integer or 8 FP argument registers.
;;;
;;; Lisp register mapping (from arm64/vm.lisp):
;;;   nl0-nl7  ->  X0-X7    (C argument / non-descriptor scratch regs)
;;;   cfunc    ->  X9       (C function address)
;;;   nsp      ->  X31/SP   (native / C stack pointer)
;;;   lip      ->  X29      (interior pointer, used as scratch for calls here)
;;;

(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

;;;; Helpers

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))


;;;; Argument passing state

;;; AArch64 AAPCS64: up to 8 integer/pointer args in X0-X7, then stack.
;;; Unlike SPARC there is no "home area" minimum; the minimum stack frame
;;; allocation we impose is 0 (we let make-call-out-tns grow it naturally).
;;; We keep stack-frame-size in 64-bit word units and convert to bytes when
;;; computing the frame size passed to alloc-number-stack-space.

(defstruct arg-state
  (register-args 0)           ; integer/pointer regs consumed (0..7 -> X0..X7)
  (float-register-args 0)     ; FP regs consumed (0..7 -> D0..D7)
  (stack-frame-size 0))       ; number of stack words needed for overflow args

;;; INT-ARG -- allocate a wired TN for one integer/pointer argument.
;;;
;;; The first 8 integer arguments go into nl0..nl7 (X0..X7).
;;; Subsequent arguments spill to the number stack (nsp-relative).
;;; AArch64 passes all integer/pointer values as 64-bit quantities even if
;;; the underlying C type is narrower; sign/zero extension is the callee's
;;; responsibility.

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-register-args state)))
    (cond ((< reg-args 8)
           ;; Passes in nl<reg-args>  (X0..X7, offsets nl0-offset..nl7-offset).
	   (setf (arg-state-register-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   ;; Spill to the number stack.  Each stack word is 8 bytes (64-bit).
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc frame-size))))))

;;; FLOAT-ARG -- allocate a wired TN for one floating-point argument.
;;;
;;; The first 8 FP arguments go into D0..D7 / S0..S7.
;;; Both single-reg and double-reg are numbered 0..31 (no stride) in vm.lisp,
;;; so the offset is simply the argument index.
;;; Subsequent FP arguments spill to the number stack.

(defun float-arg (state prim-type reg-sc stack-sc)
  (let ((fp-args (arg-state-float-register-args state)))
    (cond ((< fp-args 8)
	   (setf (arg-state-float-register-args state) (1+ fp-args))
	   (my-make-wired-tn prim-type reg-sc fp-args))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc frame-size))))))


;;;; Alien type methods -- argument TNs

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-64 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-64 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'single-float 'single-reg 'single-stack))

(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'double-float 'double-reg 'double-stack))



;;;; Alien type methods -- result TNs

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  ;; Integer results: X0 then X1 (nl0 then nl1).
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-64 'signed-reg)
	    (values 'unsigned-byte-64 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))

(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  ;; AArch64: FP result in D0.  double-reg offset 0 = V0 (D0).
  (my-make-wired-tn 'double-float 'double-reg 0))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  ;; AArch64: FP result in S0.  single-reg offset 0 = V0 (S0).
  (my-make-wired-tn 'single-float 'single-reg 0))


(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error (intl:gettext "Too many result values from c-call.")))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    values)))


;;;; make-call-out-tns

(def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-function-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
	      ;; Stack frame size in bytes, 16-byte aligned per AAPCS64.
	      (logandc2 (+ (* (arg-state-stack-frame-size arg-state) word-bytes)
                          15)
                        15)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-function-type-result-type type)
	       (make-result-state))))))


;;;; %alien-funcall transform
;;;
;;; On ARM64 all integers (including 64-bit) are passed natively in a single
;;; 64-bit register.  The only special cases we need to handle are:
;;;   * single-float  -> pass in S0..S7 FP registers
;;;   * double-float  -> pass in D0..D7 FP registers
;;;   * 64-bit result -> returned in X0, no reassembly needed
;;;
;;; Because ARM64 has native 64-bit integers and dedicated FP argument
;;; registers, the %alien-funcall transform is simpler than SPARC's.
;;; We only need to intercept single-float args (which must be converted to
;;; integer form when no FP register is available, though AAPCS64 does use
;;; S0..S7 so in practice they stay as floats; this transform is still needed
;;; for the compiler's type machinery).

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    ;; On ARM64:
    ;;   - 64-bit integers are native; no splitting.
    ;;   - doubles go in D0..D7; no integer-register passing needed.
    ;;   - single-float: pass in S0..S7 (FP regs), but the alien type system
    ;;     expects integer bits on some paths; handle via single-float-bits
    ;;     only when there are no FP arg registers available (conservatively
    ;;     we handle them here so the VOP wiring is correct).
    (if (or (some #'alien-single-float-type-p arg-types)
	    (some #'alien-double-float-type-p arg-types))
	(collect ((new-args) (lambda-vars) (new-arg-types))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (cond
                ;; Single float: the AAPCS64 passes these in S regs.
                ;; Keep as single-float so the wired FP TN gets used.
		((alien-single-float-type-p type)
		 (new-args arg)
		 (new-arg-types type))
                ;; Double float: passed in D regs natively.
		((alien-double-float-type-p type)
		 (new-args arg)
		 (new-arg-types type))
		(t
		 (new-args arg)
		 (new-arg-types type)))))
	  `(lambda (function type ,@(lambda-vars))
	     (declare (ignore type))
	     (%alien-funcall function
			     ',(make-alien-function-type
				:arg-types (new-arg-types)
				:result-type result-type)
			     ,@(new-args))))
	(c::give-up))))


;;;; VOPs

;;; foreign-symbol-address
;;;
;;; Returns the address of a foreign (C) symbol as a SAP.
;;; The #-linkage-table version materialises the address directly via LI
;;; (a pseudo-instruction that loads a 64-bit immediate).  The #+linkage-table
;;; version uses the same mechanism but emits a :foreign fixup that the linker
;;; resolves through the linkage table.

#-linkage-table
(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (emit-not-implemented)
    (inst li res (make-fixup (extern-alien-name foreign-symbol) :foreign))))

(define-vop (foreign-symbol-code-address)
  (:translate #+linkage-table foreign-symbol-code-address
	      #-linkage-table foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (emit-not-implemented)
    (inst li res (make-fixup (extern-alien-name foreign-symbol) :foreign))))

(define-vop (foreign-symbol-data-address)
  (:translate foreign-symbol-data-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:temporary (:scs (non-descriptor-reg)) addr)
  (:generator 2
    (emit-not-implemented)
    (inst li addr (make-fixup (extern-alien-name foreign-symbol) :foreign-data))
    (loadw res addr)))


;;; call-out
;;;
;;; Call a C function.  FUNCTION is the SAP holding the C entry point
;;; (already resolved, e.g. via foreign-symbol-code-address).  The
;;; argument/result TNs have been wired to the correct ABI registers by
;;; make-call-out-tns above; this VOP simply moves FUNCTION into the
;;; cfunc register and jumps through call_into_c.
;;;
;;; On AArch64 call_into_c is responsible for:
;;;   1. Saving all Lisp callee-saved registers that overlap the C caller-saved
;;;      set (X19-X28 are callee-saved in C but used for Lisp state).
;;;   2. Saving and restoring the Lisp thread state (bsp, csp, etc.).
;;;   3. Performing the actual BLR to cfunc (X9).
;;;   4. Restoring Lisp state and returning via BR LR.
;;;
;;; The CFUNC-TN (X9) is not one of the C argument registers (X0-X7),
;;; so loading the function address into it does not disturb any
;;; already-wired argument TNs.

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:scs (any-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (emit-not-implemented)
    (let ((cur-nfp (current-nfp-tn vop)))
      ;; Save the number-stack frame pointer if there is one.
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      ;; Move the function SAP into the cfunc register (X9).
      (move cfunc function)
      ;; Load the address of call_into_c into a temporary and call it.
      ;; On AArch64 we use LI to materialise the fixup address then BLR.
      (inst li temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst blr temp)
      ;; Restore the number-stack frame pointer.
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


;;; alloc-number-stack-space
;;;
;;; Allocate AMOUNT bytes on the C (number) stack by decrementing NSP.
;;; The stack must remain 16-byte aligned after the adjustment (AAPCS64).
;;; RESULT receives the new (post-decrement) NSP value, adjusted by
;;; number-stack-displacement if it differs from NSP-TN itself.

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (emit-not-implemented)
    (unless (zerop amount)
      ;; Round up to 16-byte boundary per AAPCS64.
      (let ((delta (logandc2 (+ amount 15) 15)))
	(cond ((typep delta '(unsigned-byte 12))
	       ;; Fits in the SUB immediate field.
	       (inst sub nsp-tn nsp-tn delta))
	      (t
	       (inst li temp delta)
	       (inst sub nsp-tn nsp-tn temp)))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result TN was allocated by
      ;; make-call-out-tns above, which takes number-stack-displacement
      ;; into account itself.
      (inst add result nsp-tn number-stack-displacement))))


;;; dealloc-number-stack-space
;;;
;;; Deallocate AMOUNT bytes from the C stack (inverse of the above).

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (emit-not-implemented)
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 15) 15)))
	(cond ((typep delta '(unsigned-byte 12))
	       (inst add nsp-tn nsp-tn delta))
	      (t
	       (inst li temp delta)
	       (inst add nsp-tn nsp-tn temp)))))))
