;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/compiler/x86/sse2-c-call.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;

(in-package :x86)
(use-package :alien)
(use-package :alien-internals)
(intl:textdomain "cmucl-sse2")

;; Note: other parts of the compiler depend on vops having exactly
;; these names.  Don't change them, unless you also change the other
;; parts of the compiler.

(define-vop (call-out)
  (:args (function :scs (sap-reg))
	 (args :more t))
  (:results (results :more t))
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from :eval :to :result) eax)
  (:temporary (:sc unsigned-reg :offset ecx-offset
		   :from :eval :to :result) ecx)
  (:temporary (:sc unsigned-reg :offset edx-offset
		   :from :eval :to :result) edx)
  (:temporary (:sc single-stack) temp-single)
  (:temporary (:sc double-stack) temp-double)
  #+core-math
  (:temporary (:sc unsigned-stack) save-fpu-cw)
  #+core-math
  (:temporary (:sc unsigned-stack) fpu-cw)
  #+core-math
  (:temporary (:sc unsigned-reg :offset esi-offset) temp-cw)
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args ecx edx)
  (:guard (backend-featurep :sse2))
  (:generator 0
    #+(and nil core-math)
    (progn
      ;; Save the x87 FPU control word.  Then modify it to set the
      ;; precision bits to 3 for 64-bit mantissas for 80-bit
      ;; arithmetic.  If we don't some of some special functions
      ;; return incorrect values because the x87 precision was set to
      ;; single.
      (inst fnstcw save-fpu-cw)
      (move temp-cw save-fpu-cw)
      (inst and temp-cw (dpb 0 (byte 2 8) #xffff)) ; Zap the precision control bits
      (inst or temp-cw (dpb 3 (byte 3 8) 0)) ; Set precision control bits
      (move fpu-cw temp-cw)
      (inst fldcw fpu-cw)		; New CW
      )

    (cond ((policy node (> space speed))
	   (move eax function)
	   (inst call (make-fixup (extern-alien-name "call_into_c") :foreign)))
	  (t
	   (inst call function)
	   ;; To give the debugger a clue. XX not really internal-error?
	   (note-this-location vop :internal-error)))
    ;; FIXME: check that a float result is returned when expected. If
    ;; we don't, we'll either get a NaN when doing the fstp or we'll
    ;; leave an entry on the FPU and we'll eventually overflow the FPU
    ;; stack.
    (when (and results
	       (location= (tn-ref-tn results) xmm0-tn))
      ;; If there's a float result, it would have been returned
      ;; in ST(0) according to the ABI. We want it in xmm0.
      (sc-case (tn-ref-tn results)
	(single-reg
	 (inst fstp (ea-for-sf-stack temp-single))
	 (inst movss xmm0-tn (ea-for-sf-stack temp-single)))
	(double-reg
	 (inst fstpd (ea-for-df-stack temp-double))
	 (inst movsd xmm0-tn (ea-for-df-stack temp-double)))))
    ;; Restore the x87 FPU control settings
    #+(and nil core-math)
    (inst fldcw save-fpu-cw)))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (assert (location= result esp-tn))

    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub esp-tn delta)))
    ;; Align the stack to a 16-byte boundary.  This is required an
    ;; Darwin and should be harmless everywhere else.
    (inst and esp-tn #xfffffff0)
    (move result esp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add esp-tn delta)))))

