;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/c-call.lisp,v 1.2 1991/04/21 16:58:26 wlott Exp $
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(def-vm-support-routine make-call-out-nsp-tn ()
  (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset))

(def-vm-support-routine make-call-out-argument-tns (arg-types)
  (let ((register-args 0)
	(stack-frame-size 0))
    (collect ((tns))
      (dolist (type arg-types)
	(let ((name (if (consp type) (car type) type)))
	  (flet ((int-arg (prim-type sc)
		   (cond ((< register-args 6)
			  (tns (my-make-wired-tn prim-type sc
						 (+ register-args nl0-offset)))
			  (incf register-args))
			 (t
			  (tns (my-make-wired-tn prim-type sc
						 (+ stack-frame-size 16 1 6)))
			  (incf stack-frame-size)))))
	    (ecase name
	      ((unsigned-byte port)
	       (int-arg 'unsigned-byte-32 'unsigned-reg))
	      (signed-byte
	       (int-arg 'signed-byte-32 'signed-reg))
	      (system-area-pointer
	       (int-arg 'system-area-pointer 'sap-reg))
	      (double-float
	       (error "A double-float made it through the deftransform?"))
	      (single-float
	       (error "Can't deal with floats yet."))))))
      (values (tns)
	      (* stack-frame-size word-bytes)))))

(def-vm-support-routine make-call-out-result-tn (type)
  (let ((name (if (consp type) (car type) type)))
    (ecase name
      ((unsigned-byte port)
       (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg nl0-offset))
      (signed-byte
       (my-make-wired-tn 'signed-byte-32 'signed-reg nl0-offset))
      (system-area-pointer
       (my-make-wired-tn 'system-area-pointer 'sap-reg nl0-offset))
      (double-float
       (my-make-wired-tn 'double-float 'double-reg 0))
      (single-float
       (error "Can't return single-floats yet.")))))


(deftransform ext::call-foreign-function
	      ((name return-type arg-types &rest args)
	       (string t list &rest t))
  (assert (c::constant-continuation-p name))
  (assert (c::constant-continuation-p return-type))
  (assert (c::constant-continuation-p arg-types))
  (let ((name (c::continuation-value name))
	(return-type (c::continuation-value return-type))
	(arg-types (c::continuation-value arg-types)))
    (assert (= (length arg-types) (length args)))
    (if (member 'double-float arg-types)
	(let ((new-args nil)
	      (lambda-vars nil)
	      (new-types nil))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (push arg lambda-vars)
	      (case type
		(double-float
		 (push `(double-float-high-bits ,arg) new-args)
		 (push '(signed-byte 32) new-types)
		 (push `(double-float-low-bits ,arg) new-args)
		 (push '(unsigned-byte 32) new-types))
		(t
		 (push arg new-args)
		 (push type new-types)))))
	  `(lambda (name return-type arg-types ,@(nreverse lambda-vars))
	     (declare (ignore name return-type arg-types))
	     (ext::call-foreign-function ,name ',return-type
					 ',(nreverse new-types)
					 ,@(nreverse new-args))))
	(c::give-up))))


(define-vop (foreign-symbol-address)
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:generator 2
    (inst li res (make-fixup (concatenate 'simple-string "_" foreign-symbol)
			     :foreign))))

(define-vop (call-out)
  (:args (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:info function)
  (:temporary (:sc any-reg :offset cfunc-offset :to (:result 0)) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:scs (any-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst li cfunc
	    (make-fixup (concatenate 'simple-string "_" function) :foreign))
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
