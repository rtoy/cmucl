;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/c-call.lisp,v 1.6 1991/04/21 16:57:16 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/c-call.lisp,v 1.6 1991/04/21 16:57:16 wlott Exp $
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "MIPS")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(def-vm-support-routine make-call-out-nsp-tn ()
  (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset))

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
		 (tns (my-make-wired-tn 'unsigned-byte-32
					'unsigned-reg
					(+ stack-frame-size 4)))
		 (tns (my-make-wired-tn 'unsigned-byte-32
					'unsigned-stack
					stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (signed-byte
	     (if (< stack-frame-size 4)
		 (tns (my-make-wired-tn 'signed-byte-32
					'signed-reg
					(+ stack-frame-size 4)))
		 (tns (my-make-wired-tn 'signed-byte-32
					'signed-stack
					stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (system-area-pointer
	     (if (< stack-frame-size 4)
		 (tns (my-make-wired-tn 'system-area-pointer
					'sap-reg
					(+ stack-frame-size 4)))
		 (tns (my-make-wired-tn 'system-area-pointer
					'sap-stack
					stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (double-float
	     ;; Round to a dual-word.
	     (setf stack-frame-size (logandc2 (1+ stack-frame-size) 1))
	     (cond ((>= stack-frame-size 4)
		    (tns (my-make-wired-tn 'double-float
					   'double-stack
					   stack-frame-size)))
		   ((and (not did-int-arg) (< float-args 2))
		    (tns (my-make-wired-tn 'double-float
					   'double-reg
					   (+ (* float-args 2) 12))))
		   (t
		    (error "Can't put floats in int regs yet.")))
	     (incf stack-frame-size 2)
	     (incf float-args))
	    (single-float
	     (cond ((>= stack-frame-size 4)
		    (tns (my-make-wired-tn 'single-float
					   'single-stack
					   stack-frame-size)))
		   ((and (not did-int-arg) (< float-args 2))
		    (tns (my-make-wired-tn 'single-float
					   'single-reg
					   (+ (* float-args 2) 12))))
		   (t
		    (error "Can't put floats in int regs yet.")))
	     (incf stack-frame-size)
	     (incf float-args)))))
      (values (tns)
	      (* stack-frame-size word-bytes)))))

(def-vm-support-routine make-call-out-result-tn (type)
  (let ((name (if (consp type) (car type) type)))
    (ecase name
      ((unsigned-byte port)
       (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg 2))
      (signed-byte
       (my-make-wired-tn 'signed-byte-32 'signed-reg 2))
      (system-area-pointer
       (my-make-wired-tn 'system-area-pointer 'sap-reg 2))
      (double-float
       (my-make-wired-tn 'double-float 'double-reg 0))
      (single-float
       (my-make-wired-tn 'single-float 'single-reg 0)))))


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
    (unless (zerop amount)
      (inst addu nsp-tn nsp-tn (- (logandc2 (+ amount 7) 7))))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (inst addu nsp-tn nsp-tn (logandc2 (+ amount 7) 7)))))
