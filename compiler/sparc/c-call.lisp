;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/c-call.lisp,v 1.4 1992/03/02 01:59:28 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (register-args 0)
  (stack-frame-size 0))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-register-args state)))
    (cond ((< reg-args 6)
	   (setf (arg-state-register-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc (+ frame-size 16 1 6)))))))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (alien::sap :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

(def-alien-type-method (integer :result-tn) (type)
  (if (alien-integer-type-signed type)
      (my-make-wired-tn 'signed-byte-32 'signed-reg nl0-offset)
      (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg nl0-offset)))
  
(def-alien-type-method (alien::sap :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'system-area-pointer 'sap-reg nl0-offset))

(def-alien-type-method (double-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'double-float 'double-reg 0))

(def-alien-type-method (values :result-tn) (type)
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type))
	  (alien-values-type-values type)))


(def-vm-support-routine make-call-out-argument-tns (type)
  (declare (type alien-function-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-function-type-result-type type))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    (if (some #'alien::alien-double-float-type-p arg-types)
	(collect ((new-args) (lambda-vars) (new-arg-types))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (cond ((alien::alien-double-float-type-p type)
		     (new-args `(double-float-low-bits ,arg))
		     (new-args `(double-float-high-bits ,arg))
		     (new-arg-types (parse-alien-type '(unsigned 32)))
		     (new-arg-types (parse-alien-type '(signed 32))))
		    (t
		     (new-args arg)
		     (new-arg-types type)))))
	  `(lambda (function type ,@(lambda-vars))
	     (declare (ignore type))
	     (%alien-funcall function
			     ',(alien::make-alien-function-type
				:arg-types (new-arg-types)
				:result-type result-type)
			     ,@(new-args))))
	(c::give-up))))


(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li res (make-fixup (concatenate 'simple-string "_" foreign-symbol)
			     :foreign))))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:scs (any-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (move cfunc function)
      (inst li temp (make-fixup "_call_into_c" :foreign))
      (inst jal lip temp)
      (inst nop)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (unless (zerop amount)
      (inst add nsp-tn nsp-tn (- (logandc2 (+ amount 7) 7))))
    (unless (location= nsp-tn result)
      (inst add result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (inst add nsp-tn nsp-tn (logandc2 (+ amount 7) 7)))))
