;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/c-call.lisp,v 1.2 1990/11/03 18:13:36 wlott Exp $
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "MIPS")

(def-vm-support-routine make-call-out-nsp-tn ()
  (make-wired-tn (primitive-type-or-lose 'positive-fixnum)
		 (sc-number-or-lose 'any-reg)
		 nsp-offset))

(def-vm-support-routine make-call-out-argument-tns (arg-types)
  (let ((stack-frame-size 0)
	(did-int-arg nil)
	(float-args 0))
    (collect ((tns))
      (dolist (type arg-types)
	(let ((name (if (consp type) (car type) type)))
	  (ecase name
	    ((unsigned-byte port)
	     (if (< stack-frame-size 4)
		 (tns (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
				     (sc-number-or-lose 'unsigned-reg)
				     (+ stack-frame-size 4)))
		 (tns (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
				     (sc-number-or-lose 'unsigned-stack)
				     stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (signed-byte
	     (if (< stack-frame-size 4)
		 (tns (make-wired-tn (primitive-type-or-lose 'signed-byte-32)
				     (sc-number-or-lose 'signed-reg)
				     (+ stack-frame-size 4)))
		 (tns (make-wired-tn (primitive-type-or-lose 'signed-byte-32)
				     (sc-number-or-lose 'signed-stack)
				     stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (system-area-pointer
	     (if (< stack-frame-size 4)
		 (tns (make-wired-tn (primitive-type-or-lose
				      'system-area-pointer)
				     (sc-number-or-lose 'sap-reg)
				     (+ stack-frame-size 4)))
		 (tns (make-wired-tn (primitive-type-or-lose
				      'system-area-pointer)
				     (sc-number-or-lose 'sap-stack)
				     stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (double-float
	     ;; Round to a dual-word.
	     (setf stack-frame-size (logandc2 (1+ stack-frame-size) 1))
	     (cond ((>= stack-frame-size 4)
		    (tns (make-wired-tn (primitive-type-or-lose 'double-float)
					(sc-number-or-lose 'double-stack)
					stack-frame-size)))
		   ((and (not did-int-arg) (< float-args 2))
		    (tns (make-wired-tn (primitive-type-or-lose 'double-float)
					(sc-number-or-lose 'double-reg)
					(+ (* float-args 2) 12))))
		   (t
		    (error "Can't put floats in int regs yet.")))
	     (incf stack-frame-size 2)
	     (incf float-args))
	    (single-float
	     (cond ((>= stack-frame-size 4)
		    (tns (make-wired-tn (primitive-type-or-lose 'single-float)
					(sc-number-or-lose 'single-stack)
					stack-frame-size)))
		   ((and (not did-int-arg) (< float-args 2))
		    (tns (make-wired-tn (primitive-type-or-lose 'single-float)
				   (sc-number-or-lose 'single-reg)
				   (+ (* float-args 2) 12))))
		   (t
		    (error "Can't put floats in int regs yet.")))
	     (incf stack-frame-size)
	     (incf float-args)))))
      (values (tns)
	      (logandc2 (1+ stack-frame-size) 1)))))

(def-vm-support-routine make-call-out-result-tn (type)
  (let ((name (if (consp type) (car type) type)))
    (ecase name
      ((unsigned-byte port)
       (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
		      (sc-number-or-lose 'unsigned-reg)
		      2))
      (signed-byte
       (make-wired-tn (primitive-type-or-lose 'signed-byte-32)
		      (sc-number-or-lose 'signed-reg)
		      2))
      (system-area-pointer
       (make-wired-tn (primitive-type-or-lose 'system-area-pointer)
		      (sc-number-or-lose 'sap-reg)
		      2))
      (double-float
       (make-wired-tn (primitive-type-or-lose 'double-float)
		      (sc-number-or-lose 'double-reg)
		      0))
      (single-float
       (make-wired-tn (primitive-type-or-lose 'single-float)
		      (sc-number-or-lose 'single-reg)
		      0)))))

(define-vop (foreign-symbol-address)
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:generator 2
    (inst li res (make-fixup foreign-symbol :foreign))))

(define-vop (call-out)
  (:args (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:info function)
  (:temporary (:sc any-reg :offset 2 :to (:result 0)) v0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:sc any-reg :offset code-offset) code)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((lra-label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst compute-lra-from-code lra code lra-label temp)
      (inst li v0 (make-fixup function :foreign))
      (inst li temp (make-fixup "call_into_c" :foreign))
      (inst j temp)
      (inst nop)

      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (inst addu nsp-tn nsp-tn (- (logandc2 (+ amount 7) 7)))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (inst addu nsp-tn nsp-tn (logandc2 (+ amount 7) 7))))
