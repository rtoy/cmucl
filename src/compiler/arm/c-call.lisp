;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/c-call.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;;
(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

(defun c-call-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  ;; TODO: What is the stack frame size for ARM?
  (stack-frame-size 0))

(defun int-arg (state prim-type reg-sc stack-sc)
  ;; C expectes 4 register args and the 5th arg at the top of the
  ;; stack.  We don't have enough non-descriptors to do this, so all
  ;; integer args are placed on the stack, and we depend on
  ;; call_into_c to do the right thing.
  (let ((frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ frame-size))
    (c-call-wired-tn prim-type stack-sc frame-size)))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nfp-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-reg)
	    (values 'unsigned-byte-32 'unsigned-reg))
      (c-call-wired-tn ptype reg-sc (result-reg-offset num-results)))))
  
(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (c-call-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (c-call-wired-tn 'double-float 'double-reg 0))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (c-call-wired-tn 'single-float 'single-reg 0))

(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error (intl:gettext "Too many result values from c-call.")))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    values)))

(def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-function-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-normal-tn *fixnum-primitive-type*)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-function-type-result-type type)
	       (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    ;; We need to do something special for the following argument
    ;; types: single-float, double-float, and 64-bit integers.  For
    ;; results, we need something special for 64-bit integer results.
    (if (or (some #'alien-single-float-type-p arg-types)
	    (some #'alien-double-float-type-p arg-types)
	    (some #'(lambda (type)
		      (and (alien-integer-type-p type)
			   (> (alien::alien-integer-type-bits type) 32)))
		  arg-types)
	    (and (alien-integer-type-p result-type)
		 (> (alien::alien-integer-type-bits result-type) 32)))
	(collect ((new-args) (lambda-vars) (new-arg-types) (mv-vars) (mv-form))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (cond ((and (alien-integer-type-p type)
			  (> (alien::alien-integer-type-bits type) 32))
		     ;; 64-bit long long types are stored in
		     ;; consecutive locations, most significant word
		     ;; first (big-endian).
		     (new-args `(ash ,arg -32))
		     (new-args `(logand ,arg #xffffffff))
		     (if (alien-integer-type-signed type)
			 (new-arg-types (parse-alien-type '(signed 32)))
			 (new-arg-types (parse-alien-type '(unsigned 32))))
		     (new-arg-types (parse-alien-type '(unsigned 32))))
		    ((alien-single-float-type-p type)
		     (new-args `(single-float-bits ,arg))
		     (new-arg-types (parse-alien-type '(signed 32))))
		    ((alien-double-float-type-p type)
		     ;; Use double-float-bits instead of
		     ;; double-float-hi/lo-bits to get the bits
		     ;; out.  This gives a some improvement
		     ;; because there's only store of the FP value.
		     ;;
		     ;; Sparc calling conventions say floats must be
		     ;; passed in the integer registers.
		     (let ((mvarg1 (gensym))
			   (mvarg2 (gensym)))
		       (mv-vars `(,mvarg1 ,mvarg2))
		       (mv-form `(double-float-bits ,arg))
		       (new-args mvarg1)
		       (new-args mvarg2)
		       (new-arg-types (parse-alien-type '(signed 32)))
		       (new-arg-types (parse-alien-type '(unsigned 32)))))
		    (t
		     (new-args arg)
		     (new-arg-types type)))))
	  (flet ((mv (vars forms body)
		   ;; Create the set of nested mv-binds
		   (let ((res body))
		     (do ((v (reverse vars) (cdr v))
			  (f (reverse forms) (cdr f)))
			 ((null v))
		       (setf res `(multiple-value-bind ,(car v)
				   ,(car f)
				   ,res)))
		     res)))
	    (cond ((and (alien-integer-type-p result-type)
			(> (alien::alien-integer-type-bits result-type) 32))
		   (let* ((new-result-type
			   (let ((alien::*values-type-okay* t))
			     (parse-alien-type
			      (if (alien-integer-type-signed result-type)
				  '(values (signed 32) (unsigned 32))
				  '(values (unsigned 32) (unsigned 32))))))
			  (body (mv (mv-vars)
				    (mv-form)
				    `(multiple-value-bind (high low)
				      (%alien-funcall function
				       ',(make-alien-function-type
					  :arg-types (new-arg-types)
					  :result-type new-result-type)
				       ,@(new-args))
				      (logior low (ash high 32))))))
		     `(lambda (function type ,@(lambda-vars))
		       (declare (ignore type))
		       ,body)))
		  (t
		   (let ((body (mv (mv-vars)
				   (mv-form)
				   `(%alien-funcall function
				     ',(make-alien-function-type
					:arg-types (new-arg-types)
					:result-type result-type)
				     ,@(new-args)))))
		     `(lambda (function type ,@(lambda-vars))
		       (declare (ignore type))
		       ,body))))))
	(c::give-up))))


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
    (inst li res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign))))

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
    (emit-not-implemented)))

(define-vop (foreign-symbol-data-address)
  (:translate foreign-symbol-data-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (emit-not-implemented)))

(define-vop (call-out)
  (:args (function :scs (sap-reg))
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:vop-var vop)
  (:generator 0
    (emit-not-implemented)))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (emit-not-implemented)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (emit-not-implemented)))
