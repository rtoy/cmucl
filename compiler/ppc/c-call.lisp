;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/ppc/c-call.lisp,v 1.4 2004/07/25 18:15:52 pmai Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "PPC")

;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.  Non-descriptor stack frames must be multiples of 16
;;; bytes under the PPC SVr4 ABI (though the EABI may be less
;;; restrictive).  On linux, two words are reserved for the stack
;;; backlink and saved LR (see SB!VM::NUMBER-STACK-DISPLACEMENT).

(defconstant +stack-alignment-bytes+
  ;; Duh.  PPC Linux (and VxWorks) adhere to the EABI.
  #-darwin 7
  ;; But Darwin doesn't
  #+darwin 15)

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (gpr-args 0)
  (fpr-args 0)
  ;; SVR4 [a]abi wants two words on stack (callee saved lr, backpointer).
  #-darwin
  (stack-frame-size 2)
  ;; PowerOpen ABI wants 8 words on the stack corresponding to GPR3-10
  ;; in addition to the 6 words of link area (see number-stack-displacement)
  #+darwin
  (stack-frame-size (+ 8 6)))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-gpr-args state)))
    (cond ((< reg-args 8)
	   (setf (arg-state-gpr-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc frame-size))))))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

;;; If a single-float arg has to go on the stack, it's promoted to
;;; double.  That way, C programs can get subtle rounding errors
;;; when unrelated arguments are introduced.

#-darwin
(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

#+darwin
(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state))
	 (gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
	   ;; Corresponding GPR is kept empty for functions with fixed args
	   (incf (arg-state-gpr-args state))
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  ((< fprs 13)
	   ;; According to PowerOpen ABI, we need to pass those both in the
	   ;; FPRs _and_ the stack.  However empiric testing on OS X/gcc
	   ;; shows they are only passed in FPRs, AFAICT.
	   ;;
	   ;; "I" in "AFAICT" probably refers to PRM.  -- CSR, still
	   ;; reverse-engineering comments in 2003 :-)
	   ;;
	   ;; Yes, it does -- me :)
	   (incf (arg-state-fpr-args state))
	   (incf (arg-state-stack-frame-size state))
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  (t
	   ;; Pass on stack only
	   (let ((stack-offset (arg-state-stack-frame-size state)))
	     (incf (arg-state-stack-frame-size state))
	     (my-make-wired-tn 'single-float 'single-stack stack-offset))))))

#-darwin
(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))
	   
#+darwin
(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((fprs (arg-state-fpr-args state))
	(gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
	   ;; Corresponding GPRs are also kept empty
	   (incf (arg-state-gpr-args state) 2)
	   (when (> (arg-state-gpr-args state) 8)
	     ;; Spill one word to stack
	     (decf (arg-state-gpr-args state))
	     (incf (arg-state-stack-frame-size state)))
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  ((< fprs 13)
	   ;; According to PowerOpen ABI, we need to pass those both in the
	   ;; FPRs _and_ the stack.  However empiric testing on OS X/gcc
	   ;; shows they are only passed in FPRs, AFAICT.
	   (incf (arg-state-stack-frame-size state) 2)
	   (incf (arg-state-fpr-args state))
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  (t
	   ;; Pass on stack only
	   (let ((stack-offset (arg-state-stack-frame-size state)))
	     (incf (arg-state-stack-frame-size state) 2)
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

;;; Result state handling

(defstruct result-state
  (num-results 0))

(defun generate-result-reg-offset (state)
  (let ((slot (result-state-num-results state)))
    (incf (result-state-num-results state))
    (ecase slot
      (0 nl0-offset)
      (1 nl1-offset))))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((offset (generate-result-reg-offset state)))
    (if (alien-integer-type-signed type)
        (my-make-wired-tn 'signed-byte-32 'signed-reg offset)
        (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg offset))))


(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (my-make-wired-tn 'system-area-pointer 'sap-reg
                    (generate-result-reg-offset state)))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 1))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg 1))

(def-alien-type-method (values :result-tn) (type state)
  (when (> (length (alien-values-type-values type)) 2)
    (error "Too many result values from c-call."))
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type state))
	  (alien-values-type-values type)))


(def-vm-support-routine make-call-out-tns (type)
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
	       (alien-function-type-result-type type)
	       (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    (if (or (some #'(lambda (type)
		      (and (alien-integer-type-p type)
			   (> (alien::alien-integer-type-bits type) 32)))
		  arg-types)
	    (and (alien-integer-type-p result-type)
		 (> (alien::alien-integer-type-bits result-type) 32)))
	(collect ((new-args) (lambda-vars) (new-arg-types))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (cond ((and (alien-integer-type-p type)
			  (> (alien::alien-integer-type-bits type) 32))
		     (new-args `(ash ,arg -32))
		     (new-args `(logand ,arg #xffffffff))
		     (if (alien-integer-type-signed type)
			 (new-arg-types (parse-alien-type '(signed 32)))
			 (new-arg-types (parse-alien-type '(unsigned 32))))
		     (new-arg-types (parse-alien-type '(unsigned 32))))
		    (t
		     (new-args arg)
		     (new-arg-types type)))))
	  (cond ((and (alien-integer-type-p result-type)
		      (> (alien::alien-integer-type-bits result-type) 32))
		 (let ((new-result-type
			(let ((alien::*values-type-okay* t))
			  (parse-alien-type
			   (if (alien-integer-type-signed result-type)
			       '(values (signed 32) (unsigned 32))
			       '(values (unsigned 32) (unsigned 32)))))))
		   `(lambda (function type ,@(lambda-vars))
		      (declare (ignore type))
		      (multiple-value-bind (high low)
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
				    ,@(new-args))))))
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
    (inst lr res  (make-fixup (extern-alien-name foreign-symbol) :foreign))))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst lr temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst mtctr temp)
      (move cfunc function)
      (inst bctrl)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (- (logandc2 (+ amount number-stack-displacement
                                   +stack-alignment-bytes+)
				+stack-alignment-bytes+))))
	(cond ((>= delta (ash -1 16))
	       (inst stwu nsp-tn nsp-tn delta))
	      (t
	       (inst lr temp delta)
	       (inst stwux  nsp-tn nsp-tn temp)))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result tn was allocated by
      ;; make-call-out-tns above, which takes the number-stack-displacement
      ;; into account itself.
      (inst addi result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount number-stack-displacement
                                +stack-alignment-bytes+)
			     +stack-alignment-bytes+)))
	(cond ((< delta (ash 1 16))
	       (inst addi nsp-tn nsp-tn delta))
	      (t
	       (inst lwz nsp-tn nsp-tn 0)))))))


;; bogus stubs for now

(export '(make-callback-trampoline callback-accessor-form))

(defun callback-accessor-form (type sp offset)
  (error "Callbacks not implemented on Alpha"))

(defun make-callback-trampoline (index return-type)
  "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
  (error "Callbacks not implemented on Alpha"))
