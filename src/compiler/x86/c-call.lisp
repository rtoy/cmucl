;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/compiler/x86/c-call.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Debugging and Enhancements by Douglas Crosher 1996,1997,1998,1999.
;;;

(in-package :x86)
(use-package :alien)
(use-package :alien-internals)
(intl:textdomain "cmucl-x86-vm")

;; The move-argument vop is going to store args on the stack for
;; call-out. These tn's will be used for that. move-arg is normally
;; used for things going down the stack but C wants to have args
;; indexed in the positive direction.

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (stack-frame-size 0))

(def-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind
	(ptype stack-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-stack)
	    (values 'unsigned-byte-32 'unsigned-stack))
      (my-make-wired-tn ptype stack-sc stack-frame-size))))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'system-area-pointer
		      'sap-stack
		      stack-frame-size)))

#+long-float
(def-alien-type-method (long-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 3))
    (my-make-wired-tn 'long-float 'long-stack stack-frame-size)))

(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (my-make-wired-tn 'double-float 'double-stack stack-frame-size)))

(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'single-float 'single-stack stack-frame-size)))

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 eax-offset)
    (1 edx-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind
	(ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-reg)
	    (values 'unsigned-byte-32 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))

(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

#+long-float
(def-alien-type-method (long-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'long-float 'long-reg (* num-results 2))))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    ;; The XMM registers start at 8.
    (my-make-wired-tn 'double-float 'double-reg (+ num-results 8))))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    ;; The XMM registers start at 8.
    (my-make-wired-tn 'single-float 'single-reg (+ num-results 8))))

(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error (intl:gettext "Too many result values from c-call.")))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    (alien-values-type-values type))))

(def-vm-support-routine make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg esp-offset)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method :result-tn
					(alien-function-type-result-type type)
					(make-result-state))))))

(defun %alien-funcall-aux (function type &rest args)
  (declare (ignorable function type args))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    (unless (or (some #'(lambda (type)
			  (and (alien-integer-type-p type)
			       (> (alien::alien-integer-type-bits type) 32)))
		      arg-types)
		(and (alien-integer-type-p result-type)
		     (/= (alien::alien-integer-type-bits result-type) 32)))
      (c::give-up))
    (collect ((new-args) (lambda-vars) (new-arg-types))
      (dolist (type arg-types)
	(let ((arg (gensym)))
	  (lambda-vars arg)
	  (cond ((and (alien-integer-type-p type)
		      (> (alien::alien-integer-type-bits type) 32))
		 (new-args `(logand ,arg #xffffffff))
		 (new-args `(ash ,arg -32))
		 (new-arg-types (parse-alien-type '(unsigned 32)))
		 (if (alien-integer-type-signed type)
		     (new-arg-types (parse-alien-type '(signed 32)))
		     (new-arg-types (parse-alien-type '(unsigned 32)))))
		(t
		 (new-args arg)
		 (new-arg-types type)))))
      (cond ((and (alien-integer-type-p result-type)
		  (< (alien::alien-integer-type-bits result-type) 32))
	     (let ((new-result-type
		     (parse-alien-type
		      (if (alien-integer-type-signed result-type)
			  '(signed 32)
			  '(unsigned 32)))))
	       `(lambda (function type ,@(lambda-vars))
		  (declare (ignore type))
		  (%alien-funcall function
				  ',(make-alien-function-type
				     :arg-types (new-arg-types)
				     :result-type new-result-type)
				  ,@(new-args)))))
	    ((and (alien-integer-type-p result-type)
		  (> (alien::alien-integer-type-bits result-type) 32))
	     (let ((new-result-type
		     (let ((alien::*values-type-okay* t))
		       (parse-alien-type
			(if (alien-integer-type-signed result-type)
			    '(values (unsigned 32) (signed 32))
			    '(values (unsigned 32) (unsigned 32)))))))
	       `(lambda (function type ,@(lambda-vars))
		  (declare (ignore type))
		  (multiple-value-bind (low high)
		      (%alien-funcall function
				      ',(make-alien-function-type
					 :arg-types (new-arg-types)
					 :result-type new-result-type)
				      ,@(new-args))
		    (logior low (ash high 32))))))
	    (t
	     `(lambda (function type ,@(lambda-vars))
		(declare (ignore type))
		(%alien-funcall function
				',(make-alien-function-type
				   :arg-types (new-arg-types)
				   :result-type result-type)
				,@(new-args))))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (apply #'%alien-funcall-aux function type args))

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
   (inst lea res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign))))

(define-vop (foreign-symbol-data-address)
  (:translate foreign-symbol-data-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
   (inst mov res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign-data))))

(define-vop (alloc-alien-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (assert (not (location= result esp-tn)))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub (make-ea :dword
			   :disp (+ nil-value
				    (static-symbol-offset '*alien-stack*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      delta)))
    (load-symbol-value result *alien-stack*)))

(define-vop (dealloc-alien-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add (make-ea :dword
			   :disp (+ nil-value
				    (static-symbol-offset '*alien-stack*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      delta)))))
