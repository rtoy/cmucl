;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/rt/c-call.lisp,v 1.2 1991/04/20 17:03:50 wlott Exp $
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;; Converted by Bill Chiles.
;;;

(in-package "RT")



(defun c-call-wired-tn (primitive-type sc-name offset)
  (make-wired-tn (primitive-type-or-lose primitive-type)
		 (sc-number-or-lose sc-name)
		 offset))

(def-vm-support-routine make-call-out-nsp-tn ()
  (c-call-wired-tn 'positive-fixnum 'word-pointer-reg nsp-offset))


(def-vm-support-routine make-call-out-argument-tns (arg-types)
  (let ((nargs 0))
    (collect ((tns))
      (dolist (type arg-types)
	(multiple-value-bind
	    (ptype reg-sc stack-sc)
	    (ecase (if (consp type) (car type) type)
	      ((unsigned-byte port)
	       (values 'unsigned-byte-32 'unsigned-reg 'unsigned-stack))
	      (signed-byte
	       (values 'signed-byte-32 'signed-reg 'signed-stack))
	      (system-area-pointer
	       (values 'system-area-pointer 'sap-reg 'sap-stack)))
	  ;; C expects 4 register args and the 5th arg at the top of the stack.
	  ;; We can't put args in registers, because we need those registers
	  ;; for something else.  So we put them just beyond the end of the
	  ;; stack and the trampoline code will move them into place.
	  (tns (c-call-wired-tn ptype stack-sc nargs))
	  (incf nargs)))
      (values (tns)
	      (logandc2 (1+ nargs) 1)))))

(def-vm-support-routine make-call-out-result-tn (type)
  (let ((offset nl0-offset))
    (labels
	((frob (type)
	   (let ((name (if (consp type) (car type) type)))
	     (prog1
		 (ecase name
		   ((unsigned-byte port)
		    (c-call-wired-tn 'unsigned-byte-32 'unsigned-reg offset))
		   (signed-byte
		    (c-call-wired-tn 'signed-byte-32 'signed-reg offset))
		   (system-area-pointer
		    (c-call-wired-tn 'system-area-pointer 'sap-reg offset))
		   (values
		    (when (consp type)
		      (mapcar #'frob (cdr type)))))
	       (incf offset)))))
      (frob type))))

(deftransform ext::call-foreign-function
	      ((name return-type arg-types &rest args)
	       (string * list &rest t))
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

(deftransform ext::call-foreign-function
	      ((name return-type arg-types &rest args)
	       (string t * &rest t))
  (assert (c::constant-continuation-p name))
  (assert (c::constant-continuation-p return-type))
  (assert (c::constant-continuation-p arg-types))
  (let ((name (c::continuation-value name))
	(return-type (c::continuation-value return-type))
	(arg-types (c::continuation-value arg-types))
	(arg-names (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) args)))
    (case return-type
      (double-float
       `(lambda (name return-type arg-types ,@arg-names)
	  (declare (ignore name return-type arg-types))
	  (multiple-value-bind
	      (hi lo)
	      (ext::call-foreign-function ,name
					  '(values signed-byte unsigned-byte)
					  ',arg-types
					  ,@arg-names)
	    (make-double-float hi lo))))
      (single-float
       `(lambda (name return-type arg-types ,@arg-names)
	  (make-single-float
	   (ext::call-foreign-function ,name 'signed-byte ',arg-types
				       ,@arg-names))))
      (t
       (c::give-up)))))

(define-vop (foreign-symbol-address)
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:generator 2
    (inst cai res (make-fixup (concatenate 'simple-string "_" foreign-symbol)
			      :foreign))))

(define-vop (call-out)
  (:args (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:info function)
  (:temporary (:sc any-reg :offset nl0-offset :to (:result 0)) nl0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:sc any-reg :offset code-offset) code)
  (:temporary (:scs (sap-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((lra-label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (inst compute-lra-from-code lra code lra-label)
      (inst cai nl0
	    (make-fixup (concatenate 'simple-string "_" function) :foreign))
      (inst cai temp (make-fixup "call_into_c" :foreign))
      (inst b temp)

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
      (inst cal nsp-tn nsp-tn (- (logandc2 (+ amount 7) 7))))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (inst cal nsp-tn  nsp-tn (logandc2 (+ amount 7) 7)))))
