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
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args ecx edx)
  (:guard (backend-featurep :sse2))
  (:generator 0 
    (cond ((policy node (> space speed))
	   (move eax function)
	   (inst call (make-fixup (extern-alien-name "call_into_c") :foreign))
	   (when (and results (location= (tn-ref-tn results) xmm0-tn))
	     ;; If there is a float result from the foreign call,
	     ;; call_into_c has arranged for the result to be in XMM0,
	     ;; as a double. If we wanted a single float, do the
	     ;; conversion here.
	     (sc-case (tn-ref-tn results)
	       (single-reg
		(inst cvtsd2ss xmm0-tn xmm0-tn))
	       (double-reg
		;; Nothing needed for double because call_into_c saved
		;; the result as a double.
		))))
	  (t
	   (inst call function)
	   ;; To give the debugger a clue. XX not really internal-error?
	   (note-this-location vop :internal-error)

	   (when (and results
		      (location= (tn-ref-tn results) xmm0-tn))
	     ;; If there's a float result, it would have been returned
	     ;; in fr0 according to the ABI. We want it in xmm0.
	     (sc-case (tn-ref-tn results)
	       (single-reg
		(inst fstp (ea-for-sf-stack temp-single))
		(inst movss xmm0-tn (ea-for-sf-stack temp-single)))
	       (double-reg
		(inst fstpd (ea-for-df-stack temp-double))
		(inst movsd xmm0-tn (ea-for-df-stack temp-double)))))))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (assert (location= result esp-tn))

    #+(or linux)
    (progn
      ;; Is this needed with sse2?
      (inst sub esp-tn 4)
      (inst fnstcw (make-ea :word :base esp-tn))
      (inst and (make-ea :word :base esp-tn) #xcff)
      (inst or (make-ea :word :base esp-tn) #x300)
      (inst fldcw (make-ea :word :base esp-tn))
      (inst add esp-tn 4))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub esp-tn delta)))
    #+darwin (inst and esp-tn #xfffffff0)
    (move result esp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add esp-tn delta)))
    #+(or linux)
    (progn
      ;; Is this needed with sse2?
      (inst sub esp-tn 4)
      (inst fnstcw (make-ea :word :base esp-tn))
      (inst and (make-ea :word :base esp-tn) #xcff)
      (inst or (make-ea :word :base esp-tn) #x200)
      (inst fldcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst add esp-tn 4))))

