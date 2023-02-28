;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: src/compiler/x86/float-sse2.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the x86.
;;;

(in-package :x86)
(intl:textdomain "cmucl-sse2")

;;; Popping the FP stack.
;;;
;;; The default is to use a store and pop, fstp fr0.
;;; For the AMD Athlon, using ffreep fr0 is faster.
;;;
(defun fp-pop ()
  (if (backend-featurep :athlon)
      (inst ffreep fr0-tn)
      (inst fstp fr0-tn)))


(macrolet ((ea-for-xf-desc (tn slot)
	     `(make-ea
	       :dword :base ,tn
	       :disp (- (* ,slot vm:word-bytes) vm:other-pointer-type))))
  (defun ea-for-sf-desc (tn)
    (ea-for-xf-desc tn vm:single-float-value-slot))
  (defun ea-for-df-desc (tn)
    (ea-for-xf-desc tn vm:double-float-value-slot))
  #+long-float
  (defun ea-for-lf-desc (tn)
    (ea-for-xf-desc tn vm:long-float-value-slot))
  ;; Complex floats
  (defun ea-for-csf-real-desc (tn)
    (ea-for-xf-desc tn vm:complex-single-float-real-slot))
  (defun ea-for-csf-imag-desc (tn)
    (ea-for-xf-desc tn vm:complex-single-float-imag-slot))
  (defun ea-for-cdf-real-desc (tn)
    (ea-for-xf-desc tn vm:complex-double-float-real-slot))
  (defun ea-for-cdf-imag-desc (tn)
    (ea-for-xf-desc tn vm:complex-double-float-imag-slot))
  #+long-float
  (defun ea-for-clf-real-desc (tn)
    (ea-for-xf-desc tn vm:complex-long-float-real-slot))
  #+long-float
  (defun ea-for-clf-imag-desc (tn)
    (ea-for-xf-desc tn vm:complex-long-float-imag-slot))
  #+double-double
  (defun ea-for-cddf-real-hi-desc (tn)
    (ea-for-xf-desc tn vm:complex-double-double-float-real-hi-slot))
  #+double-double
  (defun ea-for-cddf-real-lo-desc (tn)
    (ea-for-xf-desc tn vm:complex-double-double-float-real-lo-slot))
  #+double-double
  (defun ea-for-cddf-imag-hi-desc (tn)
    (ea-for-xf-desc tn vm:complex-double-double-float-imag-hi-slot))
  #+double-double
  (defun ea-for-cddf-imag-lo-desc (tn)
    (ea-for-xf-desc tn vm:complex-double-double-float-imag-lo-slot))
  )

(macrolet ((ea-for-xf-stack (tn kind)
	     `(make-ea
	       :dword :base ebp-tn
	       :disp (- (* (+ (tn-offset ,tn)
			      (ecase ,kind (:single 1) (:double 2) (:long 3)))
			 vm:word-bytes)))))
  (defun ea-for-sf-stack (tn)
    (ea-for-xf-stack tn :single))
  (defun ea-for-df-stack (tn)
    (ea-for-xf-stack tn :double))
  #+long-float
  (defun ea-for-lf-stack (tn)
    (ea-for-xf-stack tn :long)))

;;; Complex float stack EAs
(macrolet ((ea-for-cxf-stack (tn kind slot &optional base)
	     `(make-ea
	       :dword :base ,base
	       :disp (- (* (+ (tn-offset ,tn)
			      (* (ecase ,kind
				   (:single 1)
				   (:double 2)
				   (:long 3))
				 (ecase ,slot
				   ;; We want the real part to be at
				   ;; the lower address!
				   (:real 2)
				   (:imag 1)
				   (:real-hi 1)
				   (:real-lo 2)
				   (:imag-hi 3)
				   (:imag-lo 4))))
			 vm:word-bytes)))))
  (defun ea-for-csf-real-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :single :real base))
  (defun ea-for-csf-imag-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :single :imag base))
  (defun ea-for-cdf-real-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :real base))
  (defun ea-for-cdf-imag-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :imag base))
  ;;
  #+long-float
  (defun ea-for-clf-real-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :long :real base))
  #+long-float
  (defun ea-for-clf-imag-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :long :imag base))

  #+double-double
  (defun ea-for-cddf-real-hi-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :real-hi base))
  #+double-double
  (defun ea-for-cddf-real-lo-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :real-lo base))
  #+double-double
  (defun ea-for-cddf-imag-hi-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :imag-hi base))
  #+double-double
  (defun ea-for-cddf-imag-lo-stack (tn &optional (base ebp-tn))
    (ea-for-cxf-stack tn :double :imag-lo base))
  )

;;; The x86 can't store a long-float to memory without popping the
;;; stack and marking a register as empty, so it is necessary to
;;; restore the register from memory.
(defun store-long-float (ea)
   (inst fstpl ea)
   (inst fldl ea))


;;;; Move functions:

;;; x is source, y is destination
(define-move-function (load-single 2) (vop x y)
  ((single-stack) (single-reg))
  (inst movss y (ea-for-sf-stack x)))

(define-move-function (store-single 2) (vop x y)
  ((single-reg) (single-stack))
  (inst movss (ea-for-sf-stack y) x))

(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (inst movsd y (ea-for-df-stack x)))

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (inst movsd (ea-for-df-stack y) x))

#+long-float
(define-move-function (load-long 2) (vop x y)
  ((long-stack) (long-reg))
  (with-empty-tn@fp-top(y)
     (inst fldl (ea-for-lf-stack x))))

#+long-float
(define-move-function (store-long 2) (vop x y)
  ((long-reg) (long-stack))
  (cond ((zerop (tn-offset x))
	 (store-long-float (ea-for-lf-stack y)))
	(t
	 (inst fxch x)
	 (store-long-float (ea-for-lf-stack y))
	 ;; This may not be necessary as ST0 is likely invalid now.
	 (inst fxch x))))

(define-move-function (load-fp-constant 2) (vop x y)
  ((fp-constant) (single-reg double-reg))
  (let ((value (c::constant-value (c::tn-leaf x))))
    (cond ((and (zerop value)
		(= (float-sign value) 1))
	   (sc-case y
	     (single-reg (inst xorps y y))
	     (double-reg (inst xorpd y y))))
	  (t
	   (warn (intl:gettext "Ignoring bogus i387 Constant ~a") value)))))


;;;; Complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
		  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (1+ (tn-offset x))))

#+long-float
(defun complex-long-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'long-reg *backend*)
		  :offset (tn-offset x)))
#+long-float
(defun complex-long-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'long-reg *backend*)
		  :offset (1+ (tn-offset x))))

#+double-double
(progn
(defun complex-double-double-reg-real-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-double-double-reg-real-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 1 (tn-offset x))))
(defun complex-double-double-reg-imag-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 2 (tn-offset x))))
(defun complex-double-double-reg-imag-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 3 (tn-offset x))))
)
;;; x is source, y is destination
(define-move-function (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (inst movlps y (ea-for-csf-real-stack x)))

(define-move-function (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (inst movlps (ea-for-csf-real-stack y) x))

(define-move-function (load-complex-double 2) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (inst movupd y (ea-for-cdf-real-stack x)))

(define-move-function (store-complex-double 2) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (inst movupd (ea-for-cdf-real-stack y) x))

#+long-float
(define-move-function (load-complex-long 2) (vop x y)
  ((complex-long-stack) (complex-long-reg))
  (let ((real-tn (complex-long-reg-real-tn y)))
    (with-empty-tn@fp-top(real-tn)
      (inst fldl (ea-for-clf-real-stack x))))
  (let ((imag-tn (complex-long-reg-imag-tn y)))
    (with-empty-tn@fp-top(imag-tn)
      (inst fldl (ea-for-clf-imag-stack x)))))

#+long-float
(define-move-function (store-complex-long 2) (vop x y)
  ((complex-long-reg) (complex-long-stack))
  (let ((real-tn (complex-long-reg-real-tn x)))
    (cond ((zerop (tn-offset real-tn))
	   (store-long-float (ea-for-clf-real-stack y)))
	  (t
	   (inst fxch real-tn)
	   (store-long-float (ea-for-clf-real-stack y))
	   (inst fxch real-tn))))
  (let ((imag-tn (complex-long-reg-imag-tn x)))
    (inst fxch imag-tn)
    (store-long-float (ea-for-clf-imag-stack y))
    (inst fxch imag-tn)))

#+double-double
(progn
(define-move-function (load-complex-double-double 4) (vop x y)
  ((complex-double-double-stack) (complex-double-double-reg))
  (let ((real-tn (complex-double-double-reg-real-hi-tn y)))
    (inst movsd real-tn (ea-for-cddf-real-hi-stack x)))
  (let ((real-tn (complex-double-double-reg-real-lo-tn y)))
    (inst movsd real-tn (ea-for-cddf-real-lo-stack x)))
  (let ((imag-tn (complex-double-double-reg-imag-hi-tn y)))
    (inst movsd imag-tn (ea-for-cddf-imag-hi-stack x)))
  (let ((imag-tn (complex-double-double-reg-imag-lo-tn y)))
    (inst movsd imag-tn (ea-for-cddf-imag-lo-stack x))))

(define-move-function (store-complex-double-double 4) (vop x y)
  ((complex-double-double-reg) (complex-double-double-stack))
  ;; FIXME: These may not be right!!!!
  (let ((real-tn (complex-double-double-reg-real-hi-tn x)))
    (inst movsd (ea-for-cddf-real-hi-stack y) real-tn))
  (let ((real-tn (complex-double-double-reg-real-lo-tn x)))
    (inst movsd (ea-for-cddf-real-lo-stack y) real-tn))
  (let ((imag-tn (complex-double-double-reg-imag-hi-tn x)))
    (inst movsd (ea-for-cddf-imag-hi-stack y) imag-tn))
  (let ((imag-tn (complex-double-double-reg-imag-lo-tn x)))
    (inst movsd (ea-for-cddf-imag-lo-stack y) imag-tn)))

)

;;;; Move VOPs:

;;;
;;; Float register to register moves.
;;;
#+nil
(define-vop (float-move)
  (:args (x))
  (:results (y))
  (:note _N"float move")
  (:generator 0
     (unless (location= x y)
       (inst movq y x))))

(define-vop (float-move/single)
  (:args (x))
  (:results (y))
  (:note _N"float move")
  (:temporary (:sc single-stack) temp)
  (:generator 0
    (unless (location= x y)
      (let ((x-offset (tn-offset x))
	    (y-offset (tn-offset y)))
	(cond ((and (zerop x-offset)
		    (>= y-offset 8))
	       ;; Move fr0 to xmm
	       (inst fst (ea-for-sf-stack temp))
	       (inst movss y (ea-for-sf-stack temp)))
	      ((and (>= x-offset 8)
		    (>= y-offset 8))
	       (inst movq y x))
	      (t
	       (error "Don't know how to move ~S to ~S" x y)))))))

(define-vop (float-move/double)
  (:args (x))
  (:results (y))
  (:note _N"float move")
  (:temporary (:sc double-stack) temp)
  (:generator 0
    (unless (location= x y)
      (let ((x-offset (tn-offset x))
	    (y-offset (tn-offset y)))
	(cond ((and (zerop x-offset)
		    (>= y-offset 8))
	       ;; Move fr0 to xmm
	       (inst fstd (ea-for-df-stack temp))
	       (inst movsd y (ea-for-df-stack temp)))
	      ((and (>= x-offset 8)
		    (>= y-offset 8))
	       (inst movq y x))
	      (t
	       (error "Don't know how to move ~S to ~S" x y)))))))

(define-vop (single-move float-move/single)
  (:args (x :scs (single-reg) :target y :load-if (not (location= x y))))
  (:results (y :scs (single-reg) :load-if (not (location= x y)))))

(define-move-vop single-move :move (single-reg) (single-reg))

(define-vop (double-move float-move/double)
  (:args (x :scs (double-reg) :target y :load-if (not (location= x y))))
  (:results (y :scs (double-reg) :load-if (not (location= x y)))))
(define-move-vop double-move :move (double-reg) (double-reg))

#+long-float
(define-vop (long-move float-move)
  (:args (x :scs (long-reg) :target y :load-if (not (location= x y))))
  (:results (y :scs (long-reg) :load-if (not (location= x y)))))
#+long-float
(define-move-vop long-move :move (long-reg) (long-reg))

;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:generator 0
    (unless (location= x y)
      (inst movaps y x))))

(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:generator 0
    (unless (location= x y)
      (inst movapd y x))))

(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

    
#+long-float
(define-vop (complex-long-move complex-float-move)
  (:args (x :scs (complex-long-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-long-reg) :load-if (not (location= x y)))))
#+long-float
(define-move-vop complex-long-move :move
  (complex-long-reg) (complex-long-reg))


;;;
;;; Move from float to a descriptor reg. allocating a new float
;;; object in the process.
;;;
(define-vop (move-from-single)
  (:args (x :scs (single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:single-float-type vm:single-float-size node)
       (inst movss (ea-for-sf-desc y) x))))
(define-move-vop move-from-single :move
  (single-reg) (descriptor-reg))

(define-vop (move-from-double)
  (:args (x :scs (double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:double-float-type vm:double-float-size node)
       (inst movsd (ea-for-df-desc y) x))))
(define-move-vop move-from-double :move
  (double-reg) (descriptor-reg))

#+long-float
(define-vop (move-from-long)
  (:args (x :scs (long-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:long-float-type vm:long-float-size node)
       (with-tn@fp-top(x)
	 (store-long-float (ea-for-lf-desc y))))))
#+long-float
(define-move-vop move-from-long :move
  (long-reg) (descriptor-reg))

(define-vop (move-from-fp-constant)
  (:args (x :scs (fp-constant)))
  (:results (y :scs (descriptor-reg)))
  (:generator 2
     (ecase (c::constant-value (c::tn-leaf x))
       (0f0 (load-symbol-value y *fp-constant-0f0*))
       #+nil
       (1f0 (load-symbol-value y *fp-constant-1s0*))
       (0d0 (load-symbol-value y *fp-constant-0d0*))
       #+nil
       (1d0 (load-symbol-value y *fp-constant-1d0*)))))
(define-move-vop move-from-fp-constant :move
  (fp-constant) (descriptor-reg))

;;;
;;; Move from a descriptor to a float register
;;;
(define-vop (move-to-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (single-reg)))
  (:note _N"pointer to float coercion")
  (:generator 2
    (inst movss y (ea-for-sf-desc x))))
(define-move-vop move-to-single :move (descriptor-reg) (single-reg))

(define-vop (move-to-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-reg)))
  (:note _N"pointer to float coercion")
  (:generator 2
    (inst movsd y (ea-for-df-desc x))))
(define-move-vop move-to-double :move (descriptor-reg) (double-reg))

#+long-float
(define-vop (move-to-long)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (long-reg)))
  (:note _N"pointer to float coercion")
  (:generator 2
     (with-empty-tn@fp-top(y)
       (inst fldl (ea-for-lf-desc x)))))
#+long-float
(define-move-vop move-to-long :move (descriptor-reg) (long-reg))


;;;
;;; Move from complex float to a descriptor reg. allocating a new
;;; complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"complex float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:complex-single-float-type
			       vm:complex-single-float-size node)
       (inst movlps (ea-for-csf-real-desc y) x))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"complex float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:complex-double-float-type
			       vm:complex-double-float-size node)
       (inst movupd (ea-for-cdf-real-desc y) x))))

(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

#+long-float
(define-vop (move-from-complex-long)
  (:args (x :scs (complex-long-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"complex float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:complex-long-float-type
			       vm:complex-long-float-size node)
       (let ((real-tn (complex-long-reg-real-tn x)))
	 (with-tn@fp-top(real-tn)
	   (store-long-float (ea-for-clf-real-desc y))))
       (let ((imag-tn (complex-long-reg-imag-tn x)))
	 (with-tn@fp-top(imag-tn)
	   (store-long-float (ea-for-clf-imag-desc y)))))))
#+long-float
(define-move-vop move-from-complex-long :move
  (complex-long-reg) (descriptor-reg))

#+double-double
(define-vop (move-from-complex-double-double)
  (:args (x :scs (complex-double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"complex double-double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm::complex-double-double-float-type
			       vm::complex-double-double-float-size node)
       (let ((real-tn (complex-double-double-reg-real-hi-tn x)))
	 (inst movsd (ea-for-cddf-real-hi-desc y) real-tn))
       (let ((real-tn (complex-double-double-reg-real-lo-tn x)))
	 (inst movsd (ea-for-cddf-real-lo-desc y) real-tn))
       (let ((imag-tn (complex-double-double-reg-imag-hi-tn x)))
	 (inst movsd (ea-for-cddf-imag-hi-desc y) imag-tn))
       (let ((imag-tn (complex-double-double-reg-imag-lo-tn x)))
	 (inst movsd (ea-for-cddf-imag-lo-desc y) imag-tn)))))
;;;
#+double-double
(define-move-vop move-from-complex-double-double :move
  (complex-double-double-reg) (descriptor-reg))
;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note _N"pointer to complex float coercion")
  (:generator 2
    (inst movlps y (ea-for-csf-real-desc x))))

(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note _N"pointer to complex float coercion")
  (:generator 2
    (inst movupd y (ea-for-cdf-real-desc x))))

(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))


;;;
;;; The move argument vops.
;;;
;;; Note these are also used to stuff fp numbers onto the c-call stack
;;; so the order is different than the lisp-stack.

;;; The general move-argument vop
(macrolet ((frob (name sc stack-sc format)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (fp :scs (any-reg)
			     :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note _N"float argument move")
		  (:generator ,(case format (:single 2) (:double 3) (:long 4))
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 (inst movq y x)))
		      (,stack-sc
		       (if (= (tn-offset fp) esp-offset)
			   (let* ((offset (* (tn-offset y) word-bytes))
				  (ea (make-ea :dword :base fp :disp offset)))
			     ,@(ecase format
				      (:single '((inst movss ea x)))
				      (:double '((inst movsd ea x)))))
			   (let ((ea (make-ea
				      :dword :base fp
				      :disp (- (* (+ (tn-offset y)
						     ,(case format
							    (:single 1)
							    (:double 2)
							    (:long 3)))
						  vm:word-bytes)))))
			     ,@(ecase format 
				      (:single '((inst movss ea x)))
				      (:double '((inst movsd ea x))))))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack :single)
  (frob move-double-float-argument double-reg double-stack :double))

;;;; Complex float move-argument vop
(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note _N"complex float argument move")
  (:generator 3
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
	 (inst movaps y x)))
      (complex-single-stack
       (inst movlps (ea-for-csf-real-stack y fp) x)))))

(define-move-vop move-complex-single-float-argument :move-argument
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note _N"complex float argument move")
  (:generator 3
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
	 (inst movapd y x)))
      (complex-double-stack
       (inst movupd (ea-for-cdf-real-stack y fp) x)))))

(define-move-vop move-complex-double-float-argument :move-argument
  (complex-double-reg descriptor-reg) (complex-double-reg))

#+double-double
(define-vop (move-complex-double-double-float-argument)
  (:args (x :scs (complex-double-double-reg) :target y)
	 (fp :scs (any-reg) :load-if (not (sc-is y complex-double-double-reg))))
  (:results (y))
  (:note _N"complex double-double-float argument move")
  (:generator 2
    (sc-case y
      (complex-double-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-double-reg-real-hi-tn x))
	       (y-real (complex-double-double-reg-real-hi-tn y)))
	   (inst movsd y-real x-real))
	 (let ((x-real (complex-double-double-reg-real-lo-tn x))
	       (y-real (complex-double-double-reg-real-lo-tn y)))
	   (inst movsd y-real x-real))
	 (let ((x-imag (complex-double-double-reg-imag-hi-tn x))
	       (y-imag (complex-double-double-reg-imag-hi-tn y)))
	   (inst movsd y-imag x-imag))
	 (let ((x-imag (complex-double-double-reg-imag-lo-tn x))
	       (y-imag (complex-double-double-reg-imag-lo-tn y)))
	   (inst movsd y-imag x-imag))))
      (complex-double-double-stack
       (let ((real-tn (complex-double-double-reg-real-hi-tn x)))
	 (inst movsd (ea-for-cddf-real-hi-stack y fp) real-tn))
       (let ((real-tn (complex-double-double-reg-real-lo-tn x)))
	 (inst movsd (ea-for-cddf-real-lo-stack y fp) real-tn))
       (let ((imag-tn (complex-double-double-reg-imag-hi-tn x)))
	 (inst movsd (ea-for-cddf-imag-hi-stack y fp) imag-tn))
       (let ((imag-tn (complex-double-double-reg-imag-lo-tn x)))
	 (inst movsd (ea-for-cddf-imag-lo-stack y fp) imag-tn))))
    ))

#+double-double
(define-move-vop move-complex-double-double-float-argument :move-argument
  (complex-double-double-reg descriptor-reg) (complex-double-double-reg))

(define-move-vop move-argument :move-argument
  (single-reg double-reg #+long-float long-reg
   #+double-double double-double-reg
   complex-single-reg complex-double-reg #+long-float complex-long-reg
   #+double-double complex-double-double-reg)
  (descriptor-reg))


;;;; Arithmetic VOPs:


;;; dtc: The floating point arithmetic vops.
;;; 
;;; Note: Although these can accept x and y on the stack or pointed to
;;; from a descriptor register, they will work with register loading
;;; without these.  Same deal with the result - it need only be a
;;; register.  When load-tns are needed they will probably be in ST0
;;; and the code below should be able to correctly handle all cases.
;;;
;;; However it seems to produce better code if all arg. and result
;;; options are used; on the P86 there is no extra cost in using a
;;; memory operand to the FP instructions - not so on the PPro.
;;;
;;; It may also be useful to handle constant args?
;;;
;;; 22-Jul-97: descriptor args lose in some simple cases when
;;; a function result computed in a loop. Then Python insists
;;; on consing the intermediate values! For example
#|
(defun test(a n)
  (declare (type (simple-array double-float (*)) a)
	   (fixnum n))
  (let ((sum 0d0))
    (declare (type double-float sum))
  (dotimes (i n)
    (incf sum (* (aref a i)(aref a i))))
    sum))
|#
;;; So, disabling descriptor args until this can be fixed elsewhere.
;;;

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc) :target r)
                       (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float))

(macrolet ((generate (movinst opinst commutative arg-type)
	     (multiple-value-bind (rtype stack-sc ea ea-stack)
		 (if (eq arg-type 'single)
		     (values 'single-reg 'single-stack 'ea-for-sf-desc 'ea-for-sf-stack)
		     (values 'double-reg 'double-stack 'ea-for-df-desc 'ea-for-df-stack))
	       `(progn
		  (cond
		    ((location= x r)
		     ;; x and r are the same.  We can just operate on x,
		     ;; and we're done.
		     (sc-case y
		       (,rtype
			(inst ,opinst x y))
		       (descriptor-reg
			(inst ,opinst x (,ea y)))
		       (,stack-sc
			(inst ,opinst x (,ea-stack y)))))
		    ((and ,commutative (location= y r))
		     ;; y = r and the operation is commutative, so just
		     ;; do the operation with r and x.
		     (inst ,opinst y x))
		    ((not (location= r y))
		     ;; x, y, and r are three different regs.  So just
		     ;; move r to x and do the operation on r.
		     (inst ,movinst r x)
		     (sc-case y
		       (,rtype
			(inst ,opinst r y))
		       (descriptor-reg
			(inst ,opinst r (,ea y)))
		       (,stack-sc
			(inst, opinst r (,ea-stack y)))))
		    (t
		     ;; The hard case where the operation is not
		     ;; commutative, but y might be r.  Don't want to
		     ;; destroy y in this case, so use a temp so we
		     ;; don't accidentally overwrite y.
		     (inst ,movinst tmp x)
		     (sc-case y
		       (,rtype
			(inst ,opinst tmp y))
		       (descriptor-reg
			(inst ,opinst tmp (,ea y)))
		       (,stack-sc
			(inst, opinst tmp (,ea-stack y))))
		     (inst ,movinst r tmp))))))
           (frob (op sinst sname scost dinst dname dcost commutative)
             `(progn
                (define-vop (,sname single-float-op)
		  (:args (x :scs (single-reg) :target r)
			 (y :scs (single-reg descriptor-reg)
			    :load-if (not (sc-is y single-stack))))
		  (:translate ,op)
                  (:temporary (:sc single-reg) tmp)
                  (:generator ,scost
                    (generate movss ,sinst ,commutative single)))
                (define-vop (,dname double-float-op)
		  (:args (x :scs (double-reg) :target r)
			 (y :scs (double-reg descriptor-reg)
			    :load-if (not (sc-is y double-stack))))
                  (:translate ,op)
                  (:temporary (:sc double-reg) tmp)
                  (:generator ,dcost
                    (generate movsd ,dinst ,commutative double))))))
  (frob + addss +/single-float 2 addsd +/double-float 2 t)
  (frob - subss -/single-float 2 subsd -/double-float 2 nil)
  (frob * mulss */single-float 4 mulsd */double-float 5 t)
  (frob / divss //single-float 12 divsd //double-float 19 nil))

(define-vop (fsqrt)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
     (note-this-location vop :internal-error)
     (inst sqrtsd y x)))

(macrolet ((frob ((name translate mov sc type) &body body)
             `(define-vop (,name)
	        (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:temporary (:sc ,sc) tmp)
                (:note _N"inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
		  (note-this-location vop :internal-error)
		  (inst pcmpeqd tmp tmp)		; all 1's
		  ;; we should be able to do this better.  what we
		  ;; really would like to do is use the target as the
		  ;; temp whenever it's not also the source
		  (unless (location= x y)
		    (inst ,mov y x))
		  ,@body))))
  (frob (%negate/double-float %negate movsd double-reg double-float)
	(inst psllq tmp 63)		; tmp = #x8000000000000000
	(inst xorpd y tmp))
  (frob (%negate/single-float %negate movss single-reg single-float)
	(inst pslld tmp 31)		; tmp = #x80000000
	(inst xorps y tmp))
  (frob (abs/double-float abs  movsd double-reg double-float)
	(inst psrlq tmp 1)		; tmp = #x7fffffffffffffff
	(inst andpd y tmp))
  (frob (abs/single-float abs movss single-reg single-float)
	(inst psrld tmp 1)		; tmp = #x7fffffff
	(inst andps y tmp)))


;;;; Comparison:

#+long-float
(deftransform eql ((x y) (long-float long-float))
  `(and (= (long-float-low-bits x) (long-float-low-bits y))
	(= (long-float-high-bits x) (long-float-high-bits y))
	(= (long-float-exp-bits x) (long-float-exp-bits y))))

#+double-double
(deftransform eql ((x y) (double-double-float double-double-float))
  '(and (eql (double-double-hi x) (double-double-hi y))
	(eql (double-double-lo x) (double-double-lo y))))


;;;; comparison

(define-vop (float-compare)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:note _N"inline float comparison"))

;;; comiss and comisd can cope with one or other arg in memory: we
;;; could (should, indeed) extend these to cope with descriptor args
;;; and stack args

(define-vop (single-float-compare float-compare)
  (:args (x :scs (single-reg)) (y :scs (single-reg descriptor-reg)))
  (:conditional)
  (:arg-types single-float single-float))
(define-vop (double-float-compare float-compare)
  (:args (x :scs (double-reg)) (y :scs (double-reg descriptor-reg)))
  (:conditional)
  (:arg-types double-float double-float))

(macrolet
    ((frob (size inst)
       (let ((ea (ecase size
		   (single
		    'ea-for-sf-desc)
		   (double
		    'ea-for-df-desc)))
	     (name (symbolicate "=/" size "-FLOAT"))
	     (sc-type (symbolicate size "-REG"))
	     (inherit (symbolicate size "-FLOAT-COMPARE")))
	 `(define-vop (,name ,inherit)
	    (:translate =)
	    (:info target not-p)
	    (:vop-var vop)
	    (:generator 3
	      (note-this-location vop :internal-error)
	      (sc-case y
		(,sc-type
		 (inst ,inst x y))
		(descriptor-reg
		 (inst ,inst x (,ea y))))
	      ;; if PF&CF, there was a NaN involved => not equal
	      ;; otherwise, ZF => equal
	      (cond (not-p
		     (inst jmp :p target)
		     (inst jmp :ne target))
		    (t
		     (let ((not-lab (gen-label)))
		       (inst jmp :p not-lab)
		       (inst jmp :e target)
		       (emit-label not-lab)))))))))
  (frob single ucomiss)
  (frob double ucomisd))

#+nil
(define-vop (=/single-float single-float-compare)
    (:translate =)
  (:info target not-p)
  (:vop-var vop)
  (:generator 3
    (note-this-location vop :internal-error)
    (sc-case y
      (single-reg
       (inst ucomiss x y))
      (descriptor-reg
       (inst ucomiss x (ea-for-sf-desc y))))
    ;; if PF&CF, there was a NaN involved => not equal
    ;; otherwise, ZF => equal
    (cond (not-p
           (inst jmp :p target)
           (inst jmp :ne target))
          (t
           (let ((not-lab (gen-label)))
             (inst jmp :p not-lab)
             (inst jmp :e target)
             (emit-label not-lab))))))

#+nil
(define-vop (=/double-float double-float-compare)
    (:translate =)
  (:info target not-p)
  (:vop-var vop)
  (:generator 3
    (note-this-location vop :internal-error)
    (sc-case y
      (double-reg
       (inst ucomisd x y))
      (descriptor-reg
       (inst ucomisd x (ea-for-df-desc y))))
    (cond (not-p
           (inst jmp :p target)
           (inst jmp :ne target))
          (t
           (let ((not-lab (gen-label)))
             (inst jmp :p not-lab)
             (inst jmp :e target)
             (emit-label not-lab))))))

(macrolet
    ((frob (op size inst yep nope)
       (let ((ea (ecase size
		   (single
		    'ea-for-sf-desc)
		   (double
		    'ea-for-df-desc)))
	     (name (symbolicate op "/" size "-FLOAT"))
	     (sc-type (symbolicate size "-REG"))
	     (inherit (symbolicate size "-FLOAT-COMPARE")))
	 `(define-vop (,name ,inherit)
	    (:translate ,op)
	    (:info target not-p)
	    (:generator 3
	      (sc-case y
		(,sc-type
		 (inst ,inst x y))
		(descriptor-reg
		 (inst ,inst x (,ea y))))
	      (cond (not-p
		     (inst jmp :p target)
		     (inst jmp ,nope target))
		    (t
		     (let ((not-lab (gen-label)))
		       (inst jmp :p not-lab)
		       (inst jmp ,yep target)
		       (emit-label not-lab)))))))))
  (frob < single ucomiss :b :nb)
  (frob < double ucomisd :b :nb)
  (frob > single ucomiss :a :na)
  (frob > double ucomisd :a :na))

#+nil
(defmacro frob-float-compare (op size inst yep nope)
  (let ((ea (ecase size
	      (single
	       'ea-for-sf-desc)
	      (double
	       'ea-for-df-desc)))
	(name (symbolicate op "/" size "-FLOAT"))
	(sc-type (symbolicate size "-REG"))
	(inherit (symbolicate size "-FLOAT-COMPARE")))
    `(define-vop (,name ,inherit)
       (:translate ,op)
       (:info target not-p)
       (:generator 3
	 (sc-case y
	   (,sc-type
	    (inst ,inst x y))
	   (descriptor-reg
	    (inst ,inst x (,ea y))))
	 (cond (not-p
		(inst jmp :p target)
		(inst jmp ,nope target))
	       (t
		(let ((not-lab (gen-label)))
		  (inst jmp :p not-lab)
		  (inst jmp ,yep target)
		  (emit-label not-lab))))))))

#+nil
(frob-float-compare < single ucomiss :b :nb)
#+nil
(frob-float-compare < double ucomisd :b :nb)
#+nil
(define-vop (</double-float double-float-compare)
  (:translate <)
  (:info target not-p)
  (:generator 3
    (sc-case y
      (double-reg
       (inst comisd x y))
      (descriptor-reg
       (inst comisd x (ea-for-df-desc y))))
    (cond (not-p
           (inst jmp :p target)
           (inst jmp :nc target))
          (t
           (let ((not-lab (gen-label)))
             (inst jmp :p not-lab)
             (inst jmp :c target)
             (emit-label not-lab))))))

#+nil
(define-vop (</single-float single-float-compare)
  (:translate <)
  (:info target not-p)
  (:generator 3
    (sc-case y
      (single-reg
       (inst comiss x y))
      (descriptor-reg
       (inst comiss x (ea-for-sf-desc y))))
    (cond (not-p
           (inst jmp :p target)
           (inst jmp :nc target))
          (t
           (let ((not-lab (gen-label)))
             (inst jmp :p not-lab)
             (inst jmp :c target)
             (emit-label not-lab))))))

#+nil
(define-vop (>/double-float double-float-compare)
  (:translate >)
  (:info target not-p)
  (:generator 3
    (sc-case y
      (double-reg
       (inst comisd x y))
      (descriptor-reg
       (inst comisd x (ea-for-df-desc y))))
    (cond (not-p
           (inst jmp :p target)
           (inst jmp :na target))
          (t
           (let ((not-lab (gen-label)))
             (inst jmp :p not-lab)
             (inst jmp :a target)
             (emit-label not-lab))))))

#+nil
(define-vop (>/single-float single-float-compare)
  (:translate >)
  (:info target not-p)
  (:generator 3
    (sc-case y
      (single-reg
       (inst comiss x y))
      (descriptor-reg
       (inst comiss x (ea-for-sf-desc y))))
    (cond (not-p
           (inst jmp :p target)
           (inst jmp :na target))
          (t
           (let ((not-lab (gen-label)))
             (inst jmp :p not-lab)
             (inst jmp :a target)
             (emit-label not-lab))))))



;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-stack signed-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note _N"inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (sc-case x
                    (signed-reg
                     (note-this-location vop :internal-error)
                     (inst ,inst y x))
                    (signed-stack
                     (note-this-location vop :internal-error)
                     (inst ,inst y x)))))))
  (frob %single-float/signed %single-float cvtsi2ss single-reg single-float)
  (frob %double-float/signed %double-float cvtsi2sd double-reg double-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
	     (let ((ea (if (eq from-sc 'single-reg)
			   'ea-for-sf-desc
			   'ea-for-df-desc)))
	       `(define-vop (,name)
		  (:args (x :scs (,from-sc descriptor-reg) :target y))
		  (:results (y :scs (,to-sc)))
		  (:arg-types ,from-type)
		  (:result-types ,to-type)
		  (:policy :fast-safe)
		  (:note _N"inline float coercion")
		  (:translate ,translate)
		  (:vop-var vop)
		  (:save-p :compute-only)
		  (:generator 2
		    (note-this-location vop :internal-error)
		    (sc-case x
		      (,from-sc
		       (inst ,inst y x))
		      (descriptor-reg
		       (inst ,inst y (,ea x)))))))))
  (frob %single-float/double-float %single-float cvtsd2ss double-reg
	double-float single-reg single-float)

  (frob %double-float/single-float %double-float cvtss2sd
	single-reg single-float double-reg double-float))

(macrolet ((frob (trans inst from-sc from-type round-p)
             (declare (ignore round-p))
	     (let ((ea (if (eq from-sc 'single-reg)
			   'ea-for-sf-desc
			   'ea-for-df-desc)))
	       `(define-vop (,(symbolicate trans "/" from-type))
		  (:args (x :scs (,from-sc descriptor-reg)))
		  (:temporary (:sc any-reg) temp-reg)
		  (:results (y :scs (signed-reg)))
		  (:arg-types ,from-type)
		  (:result-types signed-num)
		  (:translate ,trans)
		  (:policy :fast-safe)
		  (:note _N"inline float truncate")
		  (:vop-var vop)
		  (:save-p :compute-only)
		  (:generator 5
		    (sc-case y
		      (signed-stack
		       (sc-case x
			 (,from-sc
			  (inst ,inst temp-reg x))
			 (descriptor-reg
			  (inst ,inst temp-reg (,ea x))))
		       (move y temp-reg))
		      (signed-reg
		       (sc-case x
			 (,from-sc
			  (inst ,inst y x))
			 (descriptor-reg
			  (inst ,inst y (,ea x)))))))))))
  (frob %unary-truncate cvttss2si single-reg single-float nil)
  (frob %unary-truncate cvttsd2si double-reg double-float nil)

  (frob %unary-round cvtss2si single-reg single-float t)
  (frob %unary-round cvtsd2si double-reg double-float t))

(define-vop (fast-unary-ftruncate/single-float)
  (:args (x :scs (single-reg descriptor-reg)))
  (:arg-types single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:policy :fast-safe)
  (:translate c::fast-unary-ftruncate)
  (:temporary (:sc signed-reg) temp)
  (:note _N"inline ftruncate")
  (:generator 2
    (sc-case x
      (single-reg
       (inst cvttss2si temp x))
      (descriptor-reg
       (inst cvttss2si temp (ea-for-sf-desc x))))
    (inst cvtsi2ss r temp)))

(define-vop (fast-unary-ftruncate/double-float)
  (:args (x :scs (double-reg descriptor-reg) :target r))
  (:arg-types double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:policy :fast-safe)
  (:translate c::fast-unary-ftruncate)
  (:temporary (:sc signed-reg) temp)
  (:note _N"inline ftruncate")
  (:generator 2
    (sc-case x
      (double-reg
       (inst cvttsd2si temp x))
      (descriptor-reg
       (inst cvttsd2si temp (ea-for-df-desc x))))
    (inst cvtsi2sd r temp)))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (or (and (sc-is bits signed-stack)
                                      (sc-is res single-reg))
                                 (and (sc-is bits signed-stack)
                                      (sc-is res single-stack)
                                      (location= bits res))))))
  (:results (res :scs (single-reg single-stack)))
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case res
       (single-stack
        (sc-case bits
          (signed-reg
           (inst mov res bits))
          (signed-stack
           (assert (location= bits res)))))
       (single-reg
        (sc-case bits
          (signed-reg
           (inst movd res bits))
          (signed-stack
           (inst movd res bits)))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:temporary (:sc double-reg) temp)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (inst movd temp hi-bits)
    (inst psllq temp 32)
    (inst movd res lo-bits)
    (inst orpd res temp)))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case float
         (single-reg
	  (inst movd bits float))
         (single-stack
          (move bits float))
         (descriptor-reg
	  (loadw
	   bits float vm:single-float-value-slot vm:other-pointer-type))))
      (signed-stack
       (sc-case float
         (single-reg
          (inst movss bits float)))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:sc double-reg) temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (double-reg
	(inst movq temp float)
	(inst psrlq temp 32)
	(inst movd hi-bits temp))
       (double-stack
        (loadw hi-bits ebp-tn (- (1+ (tn-offset float)))))
       (descriptor-reg
        (loadw hi-bits float (1+ double-float-value-slot)
               vm:other-pointer-type)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (double-reg
	(inst movd lo-bits float))
       (double-stack
        (loadw lo-bits ebp-tn (- (+ 2 (tn-offset float)))))
       (descriptor-reg
        (loadw lo-bits float vm:double-float-value-slot
	       vm:other-pointer-type)))))

(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))
		:to (:result 1)))
  (:results (hi-bits :scs (signed-reg))
	    (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types signed-num unsigned-num)
  (:temporary (:sc double-reg) temp)
  (:translate kernel::double-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
        (inst movq temp float)
	(inst movd lo-bits temp)
	(inst psrlq temp 32)
	(inst movd hi-bits temp))
      (double-stack
       (loadw hi-bits ebp-tn (- (+ 1 (tn-offset float))))
       (loadw lo-bits ebp-tn (- (+ 2 (tn-offset float)))))
      (descriptor-reg
       (loadw hi-bits float (1+ double-float-value-slot)
	   vm:other-pointer-type)
       (loadw lo-bits float vm:double-float-value-slot
	       vm:other-pointer-type)))))


;;;; Float mode hackery:

(deftype float-modes () '(unsigned-byte 24))

;; For the record, here is the format of the MXCSR register.
;;
;; Bit
;; 31-16      Reserved
;; 15         Flush to zero
;; 14-13      Rounding control
;; 12         precision mask (inexact)
;; 11         underflow mask
;; 10         overflow mask
;;  9         divide-by-zero mask
;;  8         denormal operation mask
;;  7         invalid operation mask
;;  6         denormals-are-zeros
;;  5         precision flag (inexact)
;;  4         underflow flag
;;  3         overflow flag
;;  2         divide-by-zero flag
;;  1         denormal operation flag
;;  0         invalid operation flag
;;
;; See below for rounding control
(defknown sse2-floating-point-modes () float-modes (flushable))
(defknown ((setf sse2-floating-point-modes)) (float-modes) float-modes)

;; Returns exactly the mxcsr register, except the masks are flipped
;; because we want exception enable flags, not masks.
(define-vop (sse2-floating-point-modes)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sse2-floating-point-modes)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack) temp)
  (:generator 3
    (inst stmxcsr temp)
    (inst mov result temp)
    (inst xor result (ash #x3f 7))))

;; Set mxcsr exactly to whatever is given, except we invert the
;; exception enable flags to make them match the exception mask flags.
(define-vop (set-sse2-floating-point-modes)
  (:args (new :scs (unsigned-reg) :to :result :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate (setf sse2-floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack) cw-stack)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 8
    ;; The high 16 bits are reserved and will cause a segfault if set,
    ;; so clear out those bits.
    (inst mov temp new)
    (inst and temp #xffff)
    (inst xor temp (ash #x3f 7))	; Convert enables to masks
    (inst mov cw-stack temp)
    (inst ldmxcsr cw-stack)
    (move res new)))

;; For the record here is the format of the x87 control and status
;; words:
;;
;; Status word:
;;
;; Bit
;; 15         FPU Busy
;; 14         Condition code C3
;; 13-11      top of stack
;; 10         Condition code C2
;;  9         Condition code C1
;;  8         Condition code C0
;;  7         Error summary status
;;  6         Stack fault
;;  5         precision flag (inexact)
;;  4         underflow flag
;;  3         overflow flag
;;  2         divide-by-zero flag
;;  1         denormal operation flag
;;  0         invalid operation flag
;;
;; Control word
;;
;; Bit
;; 15-13      Reserved
;; 12         Infinity control
;; 11-10      Rounding control
;;  9-8       Precision control
;;  7-6       Reserved
;;  5         precision mask (inexact)
;;  4         underflow mask
;;  3         overflow mask
;;  2         divide-by-zero mask
;;  1         denormal operation mask
;;  0         invalid operation mask
;;
;; Round control:
;;
;; 00   nearest
;; 01   negative infinity
;; 10   positive infinity
;; 11   zero (truncate)
;;
;; Precision control
;;
;; 00   single precision (24 bits)
;; 01   reserved
;; 10   double precision (53 bits)
;; 11   double extended precision (64 bits)

(defknown x87-floating-point-modes () float-modes (flushable))
(defknown ((setf x87-floating-point-modes)) (float-modes)
  float-modes)

;; Extract the control and status words from the FPU.  The low 16 bits
;; contain the control word, and the high 16 bits contain the status.
(define-vop (x87-floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate x87-floating-point-modes)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack) cw-stack)
  (:temporary (:sc unsigned-reg :offset eax-offset) sw-reg)
  (:generator 8
   (inst fnstsw)
   (inst fnstcw cw-stack)
   (inst and sw-reg #xff)		; mask exception flags
   (inst shl sw-reg 16)
   (inst byte #x66)			; operand size prefix
   (inst or sw-reg cw-stack)
   (inst xor sw-reg #x3f)		; invert exception mask
   (move res sw-reg)))

;; Set the control and status words from the FPU.  The low 16 bits
;; contain the control word, and the high 16 bits contain the status.
(define-vop (x87-set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :to :result :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf x87-floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack) cw-stack)
  (:temporary (:sc byte-reg :offset al-offset) sw-reg)
  (:temporary (:sc unsigned-reg :offset ecx-offset) old)
  (:generator 6
   (inst mov cw-stack new)
   (inst xor cw-stack #x3f)  ; invert exception mask
   (inst fnstsw)
   (inst fldcw cw-stack)  ; always update the control word
   (inst mov old new)
   (inst shr old 16)
   (inst cmp cl-tn sw-reg)  ; compare exception flags
   (inst jmp :z DONE)  ; skip updating the status word
   (inst sub esp-tn 28)
   (inst fstenv (make-ea :dword :base esp-tn))
   (inst mov (make-ea :byte :base esp-tn :disp 4) cl-tn)
   (inst fldenv (make-ea :dword :base esp-tn))
   (inst add esp-tn 28)
   DONE
   (move res new)))


(defun sse2-floating-point-modes ()
  (sse2-floating-point-modes))
(defun (setf sse2-floating-point-modes) (new)
  (setf (sse2-floating-point-modes) new))

(defun x87-floating-point-modes ()
  (x87-floating-point-modes))
(defun (setf x87-floating-point-modes) (new)
  (setf (x87-floating-point-modes) new))


;;;; Complex float VOPs
(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :to :save)
	 (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:temporary (:sc complex-single-reg) temp)
  (:note _N"inline complex single-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-single-reg
       ;; x = a + b*i = b|a
       (inst xorps temp temp)		; temp = 0|0|0|0
       (inst movss temp real)		; temp = 0|0|0|a
       (inst unpcklps temp imag)	; temp = 0|0|b|a
       (inst movaps r temp))
      (complex-single-stack
       (inst movss (ea-for-csf-real-stack r) real)
       (inst movss (ea-for-csf-imag-stack r) imag)))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :to :save)
	 (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:temporary (:sc complex-double-reg) temp)
  (:note _N"inline complex double-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-double-reg
       ;; x = a + b*i = b|a
       (inst movsd temp real)		; temp = ?|a
       (inst unpcklpd temp imag)	; temp = b|a
       (inst movapd r temp))
      (complex-double-stack
       (inst movsd (ea-for-cdf-real-stack r) real)
       (inst movsd (ea-for-cdf-imag-stack r) imag)))))

(define-vop (realpart/complex-single-float)
  (:translate realpart)
  (:args (x :scs (complex-single-reg complex-single-stack descriptor-reg)))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc single-reg) temp)
  (:policy :fast-safe)
  (:note _N"complex float realpart")
  (:generator 3
    (sc-case x
      (complex-single-reg
       (cond ((location= r x)
	      (inst xorps temp temp)	; temp = 0|0|0|0
	      (inst movss temp x)	; temp = 0|0|0|x
	      (inst movss r temp))	; r = temp
	     (t
	      (inst xorps r r)		; temp = 0|0|0|0
	      (inst movss r x))))	; r = 0|0|0|x
      (complex-single-stack
       (inst movss r (ea-for-csf-real-stack x)))
      (descriptor-reg
       (inst movss r (ea-for-csf-real-desc x))))))

(define-vop (realpart/complex-double-float)
  (:translate realpart)
  (:args (x :scs (complex-double-reg complex-double-stack descriptor-reg)))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc double-reg) temp)
  (:policy :fast-safe)
  (:note "complex float realpart")
  (:generator 3
    (sc-case x
      (complex-double-reg
       (cond ((location= r x)
	      (inst xorpd temp temp)	; temp = 0|0
	      (inst movsd temp x)	; temp = 0|x
	      (inst movsd r temp))	; r = temp
	     (t
	      (inst xorpd r r)		; r = 0|0
	      (inst movsd r x))))	; r = 0|x
      (complex-double-stack
       (inst movsd r (ea-for-cdf-real-stack x)))
      (descriptor-reg
       (inst movsd r (ea-for-cdf-real-desc x))))))

(define-vop (imagpart/complex-single-float)
  (:translate imagpart)
  (:args (x :scs (complex-single-reg complex-single-stack descriptor-reg)))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc complex-single-reg) temp)
  (:policy :fast-safe)
  (:note _N"complex float imagpart")
  (:generator 3
    (sc-case x
      (complex-single-reg
       ;; x = a+b*i = b|a
       ;; Get the imag part to the low part of temp.  We don't care about
       ;; the other parts of r.
       (inst movaps temp x)		; temp = u|u|b|a
       (inst shufps temp x #b01)	; temp = a|a|a|b
       (inst xorps r r)			; r = 0|0|0|0
       (inst movss r temp)		; r = 0|0|0|b
       )
      (complex-single-stack
       (inst movss r (ea-for-csf-imag-stack x)))
      (descriptor-reg
       (inst movss r (ea-for-csf-imag-desc x))))))

(define-vop (imagpart/complex-double-float)
  (:translate imagpart)
  (:args (x :scs (complex-double-reg complex-double-stack descriptor-reg)))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc complex-double-reg) temp)
  (:policy :fast-safe)
  (:note _N"complex float imagpart")
  (:generator 3
    (sc-case x
      (complex-double-reg
       (cond ((location= r x)
	      (inst xorpd temp temp)	; temp = 0|0
	      (inst movhlps temp x)	; temp = 0|b
	      (inst movsd r temp))	; r = temp
	     (t
	      (inst xorpd r r)		; r = 0|0
	      (inst movhlps r x))))	; r = 0|b
      (complex-double-stack
       (inst movsd r (ea-for-cdf-imag-stack x)))
      (descriptor-reg
       (inst movsd r (ea-for-cdf-imag-desc x))))))

;;; A hack dummy VOP to bias the representation selection of its
;;; argument towards a FP register which can help avoid consing at
;;; inappropriate locations.

(defknown double-float-reg-bias (double-float) (values))
;;;
(define-vop (double-float-reg-bias)
  (:translate double-float-reg-bias)
  (:args (x :scs (double-reg double-stack) :load-if nil))
  (:arg-types double-float)
  (:policy :fast-safe)
  (:note _N"inline dummy FP register bias")
  (:ignore x)
  (:generator 0))

(defknown single-float-reg-bias (single-float) (values))
;;;
(define-vop (single-float-reg-bias)
  (:translate single-float-reg-bias)
  (:args (x :scs (single-reg single-stack) :load-if nil))
  (:arg-types single-float)
  (:policy :fast-safe)
  (:note _N"inline dummy FP register bias")
  (:ignore x)
  (:generator 0))

;;; Support for double-double floats

#+double-double
(progn

(defun double-double-reg-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))

(defun double-double-reg-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (1+ (tn-offset x))))

(define-move-function (load-double-double 4) (vop x y)
  ((double-double-stack) (double-double-reg))
  (let ((real-tn (double-double-reg-hi-tn y)))
    (inst movsd real-tn (ea-for-cdf-real-stack x)))
  (let ((imag-tn (double-double-reg-lo-tn y)))
    (inst movsd imag-tn (ea-for-cdf-imag-stack x))))

(define-move-function (store-double-double 4) (vop x y)
  ((double-double-reg) (double-double-stack))
  (let ((real-tn (double-double-reg-hi-tn x)))
    (inst movsd (ea-for-cdf-real-stack y) real-tn))
  (let ((imag-tn (double-double-reg-lo-tn x)))
    (inst movsd (ea-for-cdf-imag-stack y) imag-tn)))

;;; Double-double float register to register moves

(define-vop (double-double-move)
  (:args (x :scs (double-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (double-double-reg) :load-if (not (location= x y))))
  (:note _N"double-double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the double-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-hi (double-double-reg-hi-tn x))
	     (y-hi (double-double-reg-hi-tn y)))
	 (inst movsd y-hi x-hi)
       (let ((x-lo (double-double-reg-lo-tn x))
	     (y-lo (double-double-reg-lo-tn y)))
	 (inst movsd y-lo x-lo))))))
;;;
(define-move-vop double-double-move :move
  (double-double-reg) (double-double-reg))

;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.

(define-vop (move-from-double-double)
  (:args (x :scs (double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note _N"double double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y vm:double-double-float-type
			       vm:double-double-float-size node)
       (let ((real-tn (double-double-reg-hi-tn x)))
	 (inst movsd (ea-for-cdf-real-desc y) real-tn))
       (let ((imag-tn (double-double-reg-lo-tn x)))
	 (inst movsd (ea-for-cdf-imag-desc y) imag-tn)))))
;;;
(define-move-vop move-from-double-double :move
  (double-double-reg) (descriptor-reg))

;;; Move from a descriptor to a double-double float register

(define-vop (move-to-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-double-reg)))
  (:note _N"pointer to double-double-float coercion")
  (:generator 2
    (let ((real-tn (double-double-reg-hi-tn y)))
      (inst movsd real-tn (ea-for-cdf-real-desc x)))
    (let ((imag-tn (double-double-reg-lo-tn y)))
      (inst movsd imag-tn (ea-for-cdf-imag-desc x)))))

(define-move-vop move-to-double-double :move
  (descriptor-reg) (double-double-reg))

;;; double-double float move-argument vop

(define-vop (move-double-double-float-argument)
  (:args (x :scs (double-double-reg) :target y)
	 (fp :scs (any-reg) :load-if (not (sc-is y double-double-reg))))
  (:results (y))
  (:note _N"double double-float argument move")
  (:generator 2
    (sc-case y
      (double-double-reg
       (unless (location= x y)
	 (let ((x-real (double-double-reg-hi-tn x))
	       (y-real (double-double-reg-hi-tn y)))
	   (inst movsd y-real x-real))
	 (let ((x-imag (double-double-reg-lo-tn x))
	       (y-imag (double-double-reg-lo-tn y)))
	   (inst movsd y-imag x-imag))))
      (double-double-stack
       (let ((hi-tn (double-double-reg-hi-tn x)))
	 (inst movsd (ea-for-cdf-real-stack y fp) hi-tn))
       (let ((lo-tn (double-double-reg-lo-tn x)))
	 (inst movsd (ea-for-cdf-imag-stack y fp) lo-tn))))))

(define-move-vop move-double-double-float-argument :move-argument
  (double-double-reg descriptor-reg) (double-double-reg))


(define-vop (move-to-complex-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-double-reg)))
  (:note _N"pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-double-reg-real-hi-tn y)))
      (inst movsd real-tn (ea-for-cddf-real-hi-desc x)))
    (let ((real-tn (complex-double-double-reg-real-lo-tn y)))
      (inst movsd real-tn (ea-for-cddf-real-lo-desc x)))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn y)))
      (inst movsd imag-tn (ea-for-cddf-imag-hi-desc x)))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn y)))
      (inst movsd imag-tn (ea-for-cddf-imag-lo-desc x)))))

(define-move-vop move-to-complex-double-double :move
  (descriptor-reg) (complex-double-double-reg))


(define-vop (make/double-double-float)
  (:args (hi :scs (double-reg) :target r
	     :load-if (not (location= hi r)))
	 (lo :scs (double-reg) :to :save))
  (:results (r :scs (double-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r double-double-stack))))
  (:arg-types double-float double-float)
  (:result-types double-double-float)
  (:translate kernel::%make-double-double-float)
  (:note _N"inline double-double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (double-double-reg
       (let ((r-real (double-double-reg-hi-tn r)))
	 (unless (location= hi r-real)
	   (inst movsd r-real hi)))
       (let ((r-imag (double-double-reg-lo-tn r)))
	 (unless (location= lo r-imag)
	   (inst movsd r-imag lo))))
      (double-double-stack
       (unless (location= hi r)
	 (inst movsd (ea-for-cdf-real-stack r) hi))
       (inst movsd (ea-for-cdf-imag-stack r) lo)))))

(define-vop (double-double-value)
  (:args (x :target r))
  (:results (r))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 3
    (cond ((sc-is x double-double-reg)
	   (let ((value-tn
		  (make-random-tn :kind :normal
				  :sc (sc-or-lose 'double-reg *backend*)
				  :offset (+ offset (tn-offset x)))))
	     (unless (location= value-tn r)
	       (inst movsd r value-tn))))
	  ((sc-is r double-reg)
	   (let ((ea (sc-case x
		       (double-double-stack
			(ecase offset
			  (0 (ea-for-cdf-real-stack x))
			  (1 (ea-for-cdf-imag-stack x))))
		       (descriptor-reg
			(ecase offset
			  (0 (ea-for-cdf-real-desc x))
			  (1 (ea-for-cdf-imag-desc x)))))))
	     (inst movsd r ea)))
	  (t (error "double-double-value VOP failure")))))


(define-vop (hi/double-double-value double-double-value)
  (:translate kernel::double-double-hi)
  (:args (x :scs (double-double-reg double-double-stack descriptor-reg)
	    :target r))
  (:arg-types double-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:note _N"double-double high part")
  (:variant 0))

(define-vop (lo/double-double-value double-double-value)
  (:translate kernel::double-double-lo)
  (:args (x :scs (double-double-reg double-double-stack descriptor-reg)
	    :target r))
  (:arg-types double-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:note _N"double-double low part")
  (:variant 1))

(define-vop (make-complex-double-double-float)
  (:translate complex)
  (:args (real :scs (double-double-reg) :target r
	       :load-if (not (location= real r))
	       )
	 (imag :scs (double-double-reg) :to :save))
  (:arg-types double-double-float double-double-float)
  (:results (r :scs (complex-double-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-double-stack))))
  (:result-types complex-double-double-float)
  (:note _N"inline complex double-double-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-double-double-reg
       (let ((r-real (complex-double-double-reg-real-hi-tn r))
	     (a-real (double-double-reg-hi-tn real)))
	 (unless (location= a-real r-real)
	   (inst movsd r-real a-real)))
       (let ((r-real (complex-double-double-reg-real-lo-tn r))
	     (a-real (double-double-reg-lo-tn real)))
	 (unless (location= a-real r-real)
	   (inst movsd r-real a-real)))
       (let ((r-imag (complex-double-double-reg-imag-hi-tn r))
	     (a-imag (double-double-reg-hi-tn imag)))
	 (unless (location= a-imag r-imag)
	   (inst movsd r-imag a-imag)))
       (let ((r-imag (complex-double-double-reg-imag-lo-tn r))
	     (a-imag (double-double-reg-lo-tn imag)))
	 (unless (location= a-imag r-imag)
	   (inst movsd r-imag a-imag))))
      (complex-double-double-stack
       (unless (location= real r)
	 (inst movsd (ea-for-cddf-real-hi-stack r) real))
       (let ((real-lo (double-double-reg-lo-tn real)))
	 (inst movsd (ea-for-cddf-real-lo-stack r) real-lo))
       (let ((imag-val (double-double-reg-hi-tn imag)))
	 (inst movsd (ea-for-cddf-imag-hi-stack r) imag-val))
       (let ((imag-val (double-double-reg-lo-tn imag)))
	 (inst movsd (ea-for-cddf-imag-lo-stack r) imag-val))))))

(define-vop (complex-double-double-float-value)
  (:args (x :scs (complex-double-double-reg descriptor-reg) :target r
	    :load-if (not (sc-is x complex-double-double-stack))))
  (:arg-types complex-double-double-float)
  (:results (r :scs (double-double-reg)))
  (:result-types double-double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:generator 3
    (sc-case x
      (complex-double-double-reg
       (let ((value-tn (ecase slot
			 (:real (complex-double-double-reg-real-hi-tn x))
			 (:imag (complex-double-double-reg-imag-hi-tn x))))
	     (r-hi (double-double-reg-hi-tn r)))
	 (unless (location= value-tn r-hi)
	   (inst movsd r-hi value-tn)))
       (let ((value-tn (ecase slot
			 (:real (complex-double-double-reg-real-lo-tn x))
			 (:imag (complex-double-double-reg-imag-lo-tn x))))
	     (r-lo (double-double-reg-lo-tn r)))
	 (unless (location= value-tn r-lo)
	   (inst movsd r-lo value-tn))))
      (complex-double-double-stack
       (let ((r-hi (double-double-reg-hi-tn r)))
	 (inst movsd r-hi (ecase slot
			    (:real (ea-for-cddf-real-hi-stack x))
			    (:imag (ea-for-cddf-imag-hi-stack x)))))
       (let ((r-lo (double-double-reg-lo-tn r)))
	 (inst movsd r-lo (ecase slot
			    (:real (ea-for-cddf-real-lo-stack x))
			    (:imag (ea-for-cddf-imag-lo-stack x))))))
      (descriptor-reg
       (let ((r-hi (double-double-reg-hi-tn r)))
	 (inst movsd r-hi (ecase slot
			    (:real (ea-for-cddf-real-hi-desc x))
			    (:imag (ea-for-cddf-imag-hi-desc x)))))
       (let ((r-lo (double-double-reg-lo-tn r)))
	 (inst movsd r-lo (ecase slot
			    (:real (ea-for-cddf-real-lo-desc x))
			    (:imag (ea-for-cddf-imag-lo-desc x)))))))))

(define-vop (realpart/complex-double-double-float complex-double-double-float-value)
  (:translate realpart)
  (:note _N"complex float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-double-float complex-double-double-float-value)
  (:translate imagpart)
  (:note _N"complex float imagpart")
  (:variant :imag))

); progn


;;; Vops for complex arithmetic.  These are usually much faster than
;;; the compiler-generated code using deftransforms.

;; Negate a complex
(macrolet
    ((negate-complex (type shift xor amount)
       (let ((name (symbolicate "%NEGATE/COMPLEX-" type "-FLOAT"))
	     (sc-type (symbolicate "COMPLEX-" type "-FLOAT"))
	     (sc (symbolicate "COMPLEX-" type "-REG")))
	 `(define-vop (,name)
	    (:translate %negate)
	    (:args (x :scs (,sc) :target r))
	    (:arg-types ,sc-type)
	    (:results (r :scs (,sc)))
	    (:result-types ,sc-type)
	    (:policy :fast-safe)
	    (:temporary (:scs (,sc)) t0)
	    (:generator 1
	      (inst pcmpeqd t0 t0)	; all ones
	      (inst ,shift t0 ,amount)	; #x8000...0000
	      (unless (location= x r)
		(inst movaps r x))
	      (inst ,xor r t0))))))
  (negate-complex single pslld xorps 31)
  (negate-complex double psllq xorpd 63))

;; Convert various number types to complex double-floats
(macrolet
    ((convert-complex (trans op to from)
       (let ((name (symbolicate to "/" from))
	     (from-sc (symbolicate from "-REG"))
	     (from-type (symbolicate from "-FLOAT"))
	     (to-sc (symbolicate to "-REG"))
	     (to-type (symbolicate to "-FLOAT")))
	 `(define-vop (,name)
	   (:translate ,trans)
	   (:args (x :scs (,from-sc) :target r))
	   (:arg-types ,from-type)
	   (:results (r :scs (,to-sc)))
	   (:result-types ,to-type)
	   (:policy :fast-safe)
	   (:generator 1
	     ;; NOTE: We don't have 128-bit aligned objects, so we
	     ;; can't use the stack or descriptors here.
	     (inst ,op r x))))))
  (convert-complex %complex-double-float cvtps2pd complex-double complex-single)
  (convert-complex %complex-single-float cvtpd2ps complex-single complex-double))

(macrolet
    ((convert-complex (trans op base-ea to from movinst)
       (let ((name (symbolicate to "/" from))
	     (from-sc (symbolicate from "-REG"))
	     (from-sc-stack (symbolicate from "-STACK"))
	     (from-type (symbolicate from "-FLOAT"))
	     (to-sc (symbolicate to "-REG"))
	     (to-type (symbolicate to "-FLOAT")))
	 `(define-vop (,name)
	   (:translate ,trans)
	   (:args (x :scs (,from-sc ,from-sc-stack descriptor-reg)
		   :target r))
	   (:arg-types ,from-type)
	   (:results (r :scs (,to-sc)))
	   (:result-types ,to-type)
	   (:temporary (:sc ,to-sc) temp)
	   (:policy :fast-safe)
	   (:generator 1
	     (sc-case x
	       (,from-sc
		;; Need to make sure the imaginary part is zero
		(cond ((location= x r)
		       (inst xorps temp temp)
		       (inst ,op temp x)
		       (inst ,movinst r temp))
		      (t
		       (inst xorps r r)
		       (inst ,op r x))))
	       (,from-sc-stack
		(inst xorps r r)
		(inst ,op r (,(symbolicate "EA-FOR-" base-ea "-STACK") x)))
	       (descriptor-reg
		(inst xorps r r)
		(inst ,op r (,(symbolicate "EA-FOR-" base-ea "-DESC") x)))))))))
  (convert-complex %complex-double-float cvtss2sd sf complex-double single movapd)
  (convert-complex %complex-single-float cvtsd2ss df complex-single double movaps))

;; Add and subtract for two complex arguments
(macrolet
    ((generate (movinst opinst commutative)
       `(cond
	 ((location= x r)
	  (inst ,opinst x y))
	 ((and ,commutative (location= y r))
	  (inst ,opinst y x))
	 ((not (location= r y))
	  (inst ,movinst r x)
	  (inst ,opinst r y))
	 (t
	  (inst ,movinst tmp x)
	  (inst ,opinst tmp y)
	  (inst ,movinst r tmp))))
     (complex-add/sub (op inst float-type cost &optional commutative)
       (let* ((vop-name (symbolicate (symbol-name op) "/COMPLEX-" float-type "-FLOAT"))
	      (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
	      (complex-reg (symbolicate "COMPLEX-" float-type "-REG")))
	 ;; Note: It would probably improve things if we could use
	 ;; memory operands, but we can't because the instructions
	 ;; assumed 128-bit alignment, which we can't guarantee.
	 `(define-vop (,vop-name)
	   (:args (x :scs (,complex-reg) :target r)
	          (y :scs (,complex-reg)))
	   (:results (r :scs (,complex-reg)))
	   (:arg-types ,c-type ,c-type)
	   (:result-types ,c-type)
	   (:policy :fast-safe)
	   (:note _N"inline complex float arithmetic")
	   (:translate ,op)
	   (:temporary (:sc ,complex-reg) tmp)
	   (:generator ,cost
	     (generate movaps ,inst ,commutative))))))
  (complex-add/sub + addps single 1 t)
  (complex-add/sub + addpd double 1 t)
  (complex-add/sub - subps single 1)
  (complex-add/sub - subpd double 1))

;; Add and subtract a complex and a float
(macrolet
    ((generate (movinst opinst)
       `(cond
	 ((location= x r)
	  (inst ,opinst x rtmp))
	 ((not (location= r rtmp))
	  (inst ,movinst r x)
	  (inst ,opinst r rtmp))
	 (t
	  (inst ,movinst tmp x)
	  (inst ,opinst tmp rtmp)
	  (inst ,movinst r tmp))))
     (complex-op-float (size op fop base-ea cost)
       (let ((vop-name (symbolicate "COMPLEX-" size "-FLOAT-"
				    op
				    "-" size "-FLOAT"))
	     (complex-reg (symbolicate "COMPLEX-" size "-REG"))
	     (real-reg (symbolicate size "-REG"))
	     (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
	     (r-type (symbolicate size "-FLOAT"))
	     (r-stack (symbolicate size "-STACK"))
	     (ea-stack (symbolicate "EA-FOR-" base-ea "-STACK"))
	     (ea-desc (symbolicate "EA-FOR-" base-ea "-DESC"))
	     (loadinst (ecase size
			 (single 'movss)
			 (double 'movsd)))
	     (movinst (ecase size
			(single 'movaps)
			(double 'movapd))))
	 `(define-vop (,vop-name)
	    (:args (x :scs (,complex-reg))
	           (y :scs (,real-reg ,r-stack descriptor-reg)))
	    (:results (r :scs (,complex-reg)))
	    (:arg-types ,c-type ,r-type)
	    (:result-types ,c-type)
	    (:policy :fast-safe)
	    (:note _N"inline complex float/float arithmetic")
	    (:translate ,op)
	    (:temporary (:sc ,complex-reg) tmp)
	    (:temporary (:sc ,real-reg) rtmp)
	    (:generator ,cost
	      ;; Clear out high and low parts of temp, which will
	      ;; eventually hold y.
	      (inst xorpd rtmp rtmp)
	      (sc-case y
		(,real-reg
		 (inst ,loadinst rtmp y)
		 (generate ,movinst ,fop))
		(,r-stack
		 (let ((ea (,ea-stack y)))
		   (inst ,loadinst rtmp ea)
		   (generate ,movinst ,fop)))
		(descriptor-reg
		 (let ((ea (,ea-desc y)))
		   (inst ,loadinst rtmp ea)
		   (generate ,movinst ,fop)))))))))
  (complex-op-float single + addps sf 1)
  (complex-op-float single - subps sf 1)
  (complex-op-float double + addpd df 1)
  (complex-op-float double - subpd df 1))

;; Add a float and a complex
(macrolet
    ((generate (movinst opinst)
       `(cond
	 ((location= x r)
	  (inst ,opinst x rtmp))
	 ((not (location= r y))
	  (inst ,movinst r x)
	  (inst ,opinst r rtmp))
	 (t
	  (inst ,movinst tmp x)
	  (inst ,opinst tmp rtmp)
	  (inst ,movinst r tmp))))
     (complex-op-float (size op fop base-ea cost)
       (let ((vop-name (symbolicate size "-FLOAT-"
				    op
				    "-" "COMPLEX-" size "-FLOAT"))
	     (complex-reg (symbolicate "COMPLEX-" size "-REG"))
	     (real-reg (symbolicate size "-REG"))
	     (c-type (symbolicate "COMPLEX-" size "-FLOAT"))
	     (r-type (symbolicate size "-FLOAT"))
	     (r-stack (symbolicate size "-STACK"))
	     (ea-stack (symbolicate "EA-FOR-" base-ea "-STACK"))
	     (ea-desc (symbolicate "EA-FOR-" base-ea "-DESC"))
	     (loadinst (ecase size
			 (single 'movss)
			 (double 'movsd)))
	     (movinst (ecase size
			(single 'movaps)
			(double 'movapd))))
	 `(define-vop (,vop-name)
	    (:args (y :scs (,real-reg ,r-stack descriptor-reg))
	           (x :scs (,complex-reg)))
	    (:results (r :scs (,complex-reg)))
	    (:arg-types ,r-type ,c-type)
	    (:result-types ,c-type)
	    (:policy :fast-safe)
	    (:note _N"inline complex float/float arithmetic")
	    (:translate ,op)
	    (:temporary (:sc ,complex-reg) tmp)
	    (:temporary (:sc ,real-reg) rtmp)
	    (:generator ,cost
	      (inst xorpd rtmp rtmp)
	      (sc-case y
		(,real-reg
		 (inst ,loadinst rtmp y)
		 (generate ,movinst ,fop))
		(,r-stack
		 (let ((ea (,ea-stack y)))
		   (inst ,loadinst rtmp ea)
		   (generate ,movinst ,fop)))
		(descriptor-reg
		 (let ((ea (,ea-desc y)))
		   (inst ,loadinst rtmp ea)
		   (generate ,movinst ,fop)))))))))
  (complex-op-float single + addps sf 1)
  (complex-op-float double + addpd df 1))

;; Multiply a complex by a float or a float by a complex.
(macrolet
    ((complex-*-float (float-type fmul copy cost)
       (let* ((vop-name (symbolicate "COMPLEX-"
				     float-type
				     "-FLOAT-*-"
				     float-type
				     "-FLOAT"))
	      (vop-name-r (symbolicate float-type
				       "-FLOAT-*-COMPLEX-"
				       float-type
				       "-FLOAT"))
	      (complex-sc-type (symbolicate "COMPLEX-" float-type "-REG"))
	      (real-sc-type (symbolicate float-type "-REG"))
	      (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
	      (r-type (symbolicate float-type "-FLOAT")))
	 `(progn
	   ;; Complex * float
	   (define-vop (,vop-name)
	     (:args (x :scs (,complex-sc-type))
	            (y :scs (,real-sc-type)))
	     (:results (r :scs (,complex-sc-type)))
	     (:arg-types ,c-type ,r-type)
	     (:result-types ,c-type)
	     (:policy :fast-safe)
	     (:note _N"inline complex float arithmetic")
	     (:translate *)
	     (:temporary (:scs (,complex-sc-type)) t0)
	     (:generator ,cost
	       (inst movaps t0 y)	; t0 = y
	       (inst ,copy t0 t0)	; t0 = y|y
	       (unless (location= x r)
		 (inst movaps r x))	; r = xi|xr
	       (inst ,fmul r t0)))
	   (define-vop (,vop-name-r)
	     (:args (x :scs (,real-sc-type))
	            (y :scs (,complex-sc-type)))
	     (:results (r :scs (,complex-sc-type)))
	     (:arg-types ,r-type ,c-type)
	     (:result-types ,c-type)
	     (:policy :fast-safe)
	     (:note _N"inline complex float arithmetic")
	     (:translate *)
	     (:temporary (:scs (,complex-sc-type)) t0)
	     (:generator ,cost
	       (inst movaps t0 x)	; t0 = 0|x or 0|0|0|x
	       (inst ,copy t0 t0)	; t0 = x|x or 0|0|x|x
	       (unless (location= y r)
		 (inst movaps r y))	; r = yi|yr or 0|0|yi|yr
	       (inst ,fmul r t0)))))))
  (complex-*-float single mulps unpcklps 4)
  (complex-*-float double mulpd unpcklpd 4))

;; Divide a complex by a real
(define-vop (complex-double-float-/-double-float)
  (:args (x :scs (complex-double-reg)) (y :scs (double-reg)))
  (:results (r :scs (complex-double-reg)))
  (:arg-types complex-double-float double-float)
  (:result-types complex-double-float)
  (:policy :fast-safe)
  (:note _N"inline complex float arithmetic")
  (:translate /)
  (:temporary (:sc complex-double-reg) t0)
  (:generator 4
    (inst movaps t0 y)			; t0 = u|y
    (inst unpcklpd t0 t0)		; t0 = y|y
    (unless (location= x r)
      (inst movaps r x))		; r = xi|xr
    (inst divpd r t0)))

(define-vop (complex-single-float-/-single-float)
  (:args (x :scs (complex-single-reg)) (y :scs (single-reg)))
  (:results (r :scs (complex-single-reg)))
  (:arg-types complex-single-float single-float)
  (:result-types complex-single-float)
  (:policy :fast-safe)
  (:note _N"inline complex float arithmetic")
  (:translate /)
  (:temporary (:sc complex-single-reg) t0 t1)
  (:generator 5
    ;; The upper parts of x may contain junk and dividing that by y
    ;; may cause spurious signals.  Thus, copy the complex number to
    ;; the high part.
    (inst movaps t0 y)			; t0 = u|u|u|y
    (inst shufps t0 t0 0)		; t0 = y|y|y|y
    (inst movaps t1 x)			; t1 = u|u|xi|xr
    (inst movlhps t1 t1)		; t1 = xi|xr|xi|xr
    (inst divps t1 t0)
    (inst xorps t0 t0)			; t0 = 0|0|0|0
    (inst movaps r t1)
    (inst movlhps r t0)))

(define-vop (sse3-*/complex-double-float)
  (:translate *)
  (:args (x :scs (complex-double-reg))
	 (y :scs (complex-double-reg)))
  (:arg-types complex-double-float complex-double-float)
  (:results (r :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:policy :fast-safe)
  (:temporary (:scs (complex-double-reg)) t1 t2)
  (:guard (backend-featurep :sse3))
  (:generator 8
    ;; Basic algorithm from the paper "The Microarchitecture of the
    ;; Intel Pentium 4 Processor on 90nm Technololgy"
    ;;
    ;; This requires SSSE3 instructions (addsubpd, movddup).
    ;;
    ;; x = a + b*i.  In sse2 reg we have: b|a
    ;; y = c + d*i.  In sse2 reg we have: d|c
    (inst movddup t1 x)			; t1 = a|a
    (inst mulpd t1 y)			; t1 = a*d|a*c
    (inst movapd t2 x)			; t2 = b|a
    (inst unpckhpd t2 t2)		; t2 = b|b
    (inst mulpd t2 y)			; t2 = b*d|b*c
    (inst shufpd t2 t2 1)		; t2 = b*c|b*d
    (inst addsubpd t1 t2)		; t2 = a*d+b*c|a*c-b*d
    (inst movapd r t1)))

(define-vop (*/complex-double-float)
  (:translate *)
  (:args (x :scs (complex-double-reg))
	 (y :scs (complex-double-reg)))
  (:arg-types complex-double-float complex-double-float)
  (:results (r :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:policy :fast-safe)
  (:temporary (:scs (complex-double-reg)) t0 t1 t2)
  (:temporary (:scs (unsigned-reg)) tmp)
  (:generator 13
    ;; Basic algorithm from the paper "The Microarchitecture of the
    ;; Intel Pentium 4 Processor on 90nm Technololgy"

    ;; x = a+b*i = b|a
    ;; y = c+d*i = d|c
    ;; r = a*c-b*d + i*(a*d+b*c)
    (inst movapd t1 y)			; t1 = d|c
    (inst movapd t2 y)			; t2 = d|c
    (inst unpcklpd t1 t1)		; t1 = c|c
    (inst unpckhpd t2 t2)		; t2 = d|d
    (inst mulpd t1 x)			; t1 = b*c|a*c
    (inst mulpd t2 x)			; t2 = b*d|a*d
    (inst shufpd t2 t2 1)		; t2 = a*d|b*d
    (inst mov tmp #x80000000)
    (inst movd t0 tmp)			; t0 = 0|0|0|#x80000000
    (inst psllq t0 32)			; t0 = 0|#x80000000,00000000
    (inst xorpd t2 t0)			; t2 = a*d|-b*d
    (inst addpd t2 t1)			; t2 = a*d+b*c | a*c-b*d
    (inst movapd r t2)))


(define-vop (*/complex-single-float)
  (:translate *)
  (:args (x :scs (complex-single-reg))
	 (y :scs (complex-single-reg)))
  (:arg-types complex-single-float complex-single-float)
  (:results (r :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:policy :fast-safe)
  (:temporary (:scs (complex-single-reg)) t0 t1 t2)
  (:temporary (:scs (unsigned-reg)) tmp)
  (:generator 14
    ;; Basic algorithm from the paper "The Microarchitecture of the
    ;; Intel Pentium 4 Processor on 90nm Technololgy"

    ;; x = a+b*i = b|a
    ;; y = c+d*i = d|c
    ;; r = a*c-b*d + i*(a*d+b*c)
    (inst movaps t1 y)			; t1 = u|u|d|c
    (inst movaps t2 y)			; t2 = u|u|d|c
    (inst shufps t1 t1 #b00000000)	; t1 = c|c|c|c
    (inst shufps t2 t2 #b01010101)	; t2 = d|d|d|d
    (inst mulps t1 x)			; t1 = b*c|a*c
    (inst mulps t2 x)			; t2 = b*d|a*d
    (inst shufps t2 t2 1)		; t2 = a*d|b*d
    (inst mov tmp #x80000000)
    (inst movd t0 tmp)			; t0 = 0|0|0|#x80000000
    (inst xorps t2 t0)			; t2 = a*d|-b*d
    (inst addps t2 t1)			; t2 = a*d+b*c | a*c-b*d
    (inst xorps t1 t1)			; t1 = 0|0|0|0
    (inst movaps r t2)
    (inst movlhps r t1)))

;; Conjugate
(define-vop (conjugate/complex-double-float)
  (:translate conjugate)
  (:args (z :scs (complex-double-reg)))
  (:arg-types complex-double-float)
  (:results (r :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:policy :fast-safe)
  (:temporary (:scs (complex-double-reg)) ztmp)
  (:temporary (:scs (unsigned-reg)) tmp)
  (:generator 2
    (inst mov tmp #x80000000)
    (inst movd ztmp tmp)
    (inst psllq ztmp 32)		; ztmp = 0|#x80000000,00000000
    (inst shufpd ztmp ztmp 1)		; ztmp = #x80000000,00000000|0
    (inst xorpd ztmp z)			; ztmp = -xi|xi
    (inst movapd r ztmp)))

(define-vop (conjugate/complex-single-float)
  (:translate conjugate)
  (:args (z :scs (complex-single-reg)))
  (:arg-types complex-single-float)
  (:results (r :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:policy :fast-safe)
  (:temporary (:scs (complex-single-reg)) ztmp)
  (:temporary (:scs (unsigned-reg)) tmp)
  (:generator 2
    (inst mov tmp #x80000000)
    (inst movd ztmp tmp)
    (inst psllq ztmp 32)		; ztmp = #x80000000|0
    (inst xorps ztmp z)			; ztmp = -xi|xr
    (inst movaps r ztmp)))
