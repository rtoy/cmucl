;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/float.lisp,v 1.25 1999/11/18 14:25:39 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;; Sparc conversion by William Lott.
;;; Complex-float and long-float support by Douglas Crosher 1998.
;;;
(in-package "SPARC")


;;;; Move functions:

(define-move-function (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst ldf y (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes)))

(define-move-function (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst stf x (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes)))


(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (inst lddf y nfp offset)))

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (inst stdf x nfp offset)))

;;; The offset may be an integer or a TN in which case it will be
;;; temporarily modified but is restored if restore-offset is true.
;;;
(defun load-long-reg (reg base offset &optional (restore-offset t))
  (if (backend-featurep :sparc-v9)
      (inst ldxf reg base offset)
      (let ((reg0 (make-random-tn :kind :normal
				  :sc (sc-or-lose 'double-reg *backend*)
				  :offset (tn-offset reg)))
	    (reg2 (make-random-tn :kind :normal
				  :sc (sc-or-lose 'double-reg *backend*)
				  :offset (+ 2 (tn-offset reg)))))
	(cond ((integerp offset)
	       (inst lddf reg0 base offset)
	       (inst lddf reg2 base (+ offset (* 2 vm:word-bytes))))
	      (t
	       (inst lddf reg0 base offset)
	       (inst add offset (* 2 vm:word-bytes))
	       (inst lddf reg2 base offset)
	       (when restore-offset
		 (inst sub offset (* 2 vm:word-bytes))))))))

#+long-float
(define-move-function (load-long 2) (vop x y)
  ((long-stack) (long-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (load-long-reg y nfp offset)))

;;; The offset may be an integer or a TN in which case it will be
;;; temporarily modified but is restored if restore-offset is true.
;;;
(defun store-long-reg (reg base offset &optional (restore-offset t))
  (if (backend-featurep :sparc-v9)
      (inst stxf reg base offset)
      (let ((reg0 (make-random-tn :kind :normal
				  :sc (sc-or-lose 'double-reg *backend*)
				  :offset (tn-offset reg)))
	    (reg2 (make-random-tn :kind :normal
				  :sc (sc-or-lose 'double-reg *backend*)
				  :offset (+ 2 (tn-offset reg)))))
	(cond ((integerp offset)
	       (inst stdf reg0 base offset)
	       (inst stdf reg2 base (+ offset (* 2 vm:word-bytes))))
	      (t
	       (inst stdf reg0 base offset)
	       (inst add offset (* 2 vm:word-bytes))
	       (inst stdf reg2 base offset)
	       (when restore-offset
		 (inst sub offset (* 2 vm:word-bytes))))))))

#+long-float
(define-move-function (store-long 2) (vop x y)
  ((long-reg) (long-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (store-long-reg x nfp offset)))


;;;; Move VOPs:

;;; Exploit the V9 double-float move instruction. This is conditional
;;; on the :sparc-v9 feature.
(defun move-double-reg (dst src)
  (cond ((backend-featurep :sparc-v9)
	 (inst fmovd dst src))
	(t
	 (dotimes (i 2)
	   (let ((dst (make-random-tn :kind :normal
				      :sc (sc-or-lose 'single-reg *backend*)
				      :offset (+ i (tn-offset dst))))
		 (src (make-random-tn :kind :normal
				      :sc (sc-or-lose 'single-reg *backend*)
				      :offset (+ i (tn-offset src)))))
	     (inst fmovs dst src))))))

;;; Exploit the V9 long-float move instruction. This is conditional
;;; on the :sparc-v9 feature.
(defun move-long-reg (dst src)
  (cond ((backend-featurep :sparc-v9)
	 (inst fmovx dst src))
	(t
	 (dotimes (i 4)
	   (let ((dst (make-random-tn :kind :normal
				      :sc (sc-or-lose 'single-reg *backend*)
				      :offset (+ i (tn-offset dst))))
		 (src (make-random-tn :kind :normal
				      :sc (sc-or-lose 'single-reg *backend*)
				      :offset (+ i (tn-offset src)))))
	     (inst fmovs dst src))))))

(macrolet ((frob (vop sc format)
	     `(progn
		(define-vop (,vop)
		  (:args (x :scs (,sc)
			    :target y
			    :load-if (not (location= x y))))
		  (:results (y :scs (,sc)
			       :load-if (not (location= x y))))
		  (:note "float move")
		  (:generator 0
		    (unless (location= y x)
		      ,@(ecase format
			  (:single `((inst fmovs y x)))
			  (:double `((move-double-reg y x)))
			  (:long `((move-long-reg y x)))))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg :single)
  (frob double-move double-reg :double)
  #+long-float
  (frob long-move long-reg :long))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars format size type data)
  (:generator 13
    (with-fixed-allocation (y ndescr type size))
    (ecase format
      (:single
       (inst stf x y (- (* data vm:word-bytes) vm:other-pointer-type)))
      (:double
       (inst stdf x y (- (* data vm:word-bytes) vm:other-pointer-type)))
      (:long
       (store-long-reg x y (- (* data vm:word-bytes)
			      vm:other-pointer-type))))))

(macrolet ((frob (name sc &rest args)
	     `(progn
		(define-vop (,name move-from-float)
		  (:args (x :scs (,sc) :to :save))
		  (:results (y :scs (descriptor-reg)))
		  (:variant ,@args))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg :single
    vm:single-float-size vm:single-float-type vm:single-float-value-slot)
  (frob move-from-double double-reg :double
    vm:double-float-size vm:double-float-type vm:double-float-value-slot)
  #+long-float
  (frob move-from-long long-reg	:long
     vm:long-float-size vm:long-float-type vm:long-float-value-slot))

(macrolet ((frob (name sc format value)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:note "pointer to float coercion")
		  (:generator 2
		    (inst ,(ecase format
			     (:single 'ldf)
			     (:double 'lddf))
			  y x
			  (- (* ,value vm:word-bytes) vm:other-pointer-type))))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg :single vm:single-float-value-slot)
  (frob move-to-double double-reg :double vm:double-float-value-slot))

#+long-float
(define-vop (move-to-long)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (long-reg)))
  (:note "pointer to float coercion")
  (:generator 2
    (load-long-reg y x (- (* vm:long-float-value-slot vm:word-bytes)
			  vm:other-pointer-type))))
#+long-float
(define-move-vop move-to-long :move (descriptor-reg) (long-reg))

(macrolet ((frob (name sc stack-sc format)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (nfp :scs (any-reg)
			      :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note "float argument move")
		  (:generator ,(ecase format (:single 1) (:double 2))
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 ,@(ecase format
			     (:single '((inst fmovs y x)))
			     (:double '((move-double-reg y x))))))
		      (,stack-sc
		       (let ((offset (* (tn-offset y) vm:word-bytes)))
			 (inst ,(ecase format
				  (:single 'stf)
				  (:double 'stdf))
			       x nfp offset))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack :single)
  (frob move-double-float-argument double-reg double-stack :double))

#+long-float
(define-vop (move-long-float-argument)
  (:args (x :scs (long-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y long-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 3
    (sc-case y
      (long-reg
       (unless (location= x y)
	 (move-long-reg y x)))
      (long-stack
       (let ((offset (* (tn-offset y) vm:word-bytes)))
	 (store-long-reg x nfp offset))))))
;;;
#+long-float
(define-move-vop move-long-float-argument :move-argument
  (long-reg descriptor-reg) (long-reg))


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
		  :offset (+ (tn-offset x) 2)))

#+long-float
(defun complex-long-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'long-reg *backend*)
		  :offset (tn-offset x)))
#+long-float
(defun complex-long-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'long-reg *backend*)
		  :offset (+ (tn-offset x) 4)))


(define-move-function (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst ldf real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst ldf imag-tn nfp (+ offset vm:word-bytes)))))

(define-move-function (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst stf real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst stf imag-tn nfp (+ offset vm:word-bytes)))))


(define-move-function (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lddf real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lddf imag-tn nfp (+ offset (* 2 vm:word-bytes))))))

(define-move-function (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stdf real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stdf imag-tn nfp (+ offset (* 2 vm:word-bytes))))))


#+long-float
(define-move-function (load-complex-long 5) (vop x y)
  ((complex-long-stack) (complex-long-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-long-reg-real-tn y)))
      (load-long-reg real-tn nfp offset))
    (let ((imag-tn (complex-long-reg-imag-tn y)))
      (load-long-reg imag-tn nfp (+ offset (* 4 vm:word-bytes))))))

#+long-float
(define-move-function (store-complex-long 5) (vop x y)
  ((complex-long-reg) (complex-long-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-long-reg-real-tn x)))
      (store-long-reg real-tn nfp offset))
    (let ((imag-tn (complex-long-reg-imag-tn x)))
      (store-long-reg imag-tn nfp (+ offset (* 4 vm:word-bytes))))))

;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-single-reg-real-tn x))
	     (y-real (complex-single-reg-real-tn y)))
	 (inst fmovs y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
	     (y-imag (complex-single-reg-imag-tn y)))
	 (inst fmovs y-imag x-imag)))))
;;;
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-reg-real-tn x))
	     (y-real (complex-double-reg-real-tn y)))
	 (move-double-reg y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
	     (y-imag (complex-double-reg-imag-tn y)))
	 (move-double-reg y-imag x-imag)))))
;;;
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

#+long-float
(define-vop (complex-long-move)
  (:args (x :scs (complex-long-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-long-reg) :load-if (not (location= x y))))
  (:note "complex long float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-long-reg-real-tn x))
	     (y-real (complex-long-reg-real-tn y)))
	 (move-long-reg y-real x-real))
       (let ((x-imag (complex-long-reg-imag-tn x))
	     (y-imag (complex-long-reg-imag-tn y)))
	 (move-long-reg y-imag x-imag)))))
;;;
#+long-float
(define-move-vop complex-long-move :move
  (complex-long-reg) (complex-long-reg))

;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr vm:complex-single-float-type
			       vm:complex-single-float-size))
     (let ((real-tn (complex-single-reg-real-tn x)))
       (inst stf real-tn y (- (* vm:complex-single-float-real-slot
				 vm:word-bytes)
			      vm:other-pointer-type)))
     (let ((imag-tn (complex-single-reg-imag-tn x)))
       (inst stf imag-tn y (- (* vm:complex-single-float-imag-slot
				 vm:word-bytes)
			      vm:other-pointer-type)))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr vm:complex-double-float-type
			       vm:complex-double-float-size))
     (let ((real-tn (complex-double-reg-real-tn x)))
       (inst stdf real-tn y (- (* vm:complex-double-float-real-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((imag-tn (complex-double-reg-imag-tn x)))
       (inst stdf imag-tn y (- (* vm:complex-double-float-imag-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))))
;;;
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

#+long-float
(define-vop (move-from-complex-long)
  (:args (x :scs (complex-long-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex long float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr vm:complex-long-float-type
			       vm:complex-long-float-size))
     (let ((real-tn (complex-long-reg-real-tn x)))
       (store-long-reg real-tn y (- (* vm:complex-long-float-real-slot
				       vm:word-bytes)
				    vm:other-pointer-type)))
     (let ((imag-tn (complex-long-reg-imag-tn x)))
       (store-long-reg imag-tn y (- (* vm:complex-long-float-imag-slot
				       vm:word-bytes)
				    vm:other-pointer-type)))))
;;;
#+long-float
(define-move-vop move-from-complex-long :move
  (complex-long-reg) (descriptor-reg))

;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst ldf real-tn x (- (* complex-single-float-real-slot word-bytes)
			     other-pointer-type)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst ldf imag-tn x (- (* complex-single-float-imag-slot word-bytes)
			     other-pointer-type)))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lddf real-tn x (- (* complex-double-float-real-slot word-bytes)
			      other-pointer-type)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lddf imag-tn x (- (* complex-double-float-imag-slot word-bytes)
			      other-pointer-type)))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

#+long-float
(define-vop (move-to-complex-long)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-long-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-long-reg-real-tn y)))
      (load-long-reg real-tn x (- (* complex-long-float-real-slot word-bytes)
				  other-pointer-type)))
    (let ((imag-tn (complex-long-reg-imag-tn y)))
      (load-long-reg imag-tn x (- (* complex-long-float-imag-slot word-bytes)
				  other-pointer-type)))))
#+long-float
(define-move-vop move-to-complex-long :move
  (descriptor-reg) (complex-long-reg))

;;;
;;; Complex float move-argument vop
;;;
(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "complex single-float argument move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
	 (let ((x-real (complex-single-reg-real-tn x))
	       (y-real (complex-single-reg-real-tn y)))
	   (inst fmovs y-real x-real))
	 (let ((x-imag (complex-single-reg-imag-tn x))
	       (y-imag (complex-single-reg-imag-tn y)))
	   (inst fmovs y-imag x-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-single-reg-real-tn x)))
	   (inst stf real-tn nfp offset))
	 (let ((imag-tn (complex-single-reg-imag-tn x)))
	   (inst stf imag-tn nfp (+ offset word-bytes))))))))
(define-move-vop move-complex-single-float-argument :move-argument
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double-float argument move")
  (:generator 2
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-reg-real-tn x))
	       (y-real (complex-double-reg-real-tn y)))
	   (move-double-reg y-real x-real))
	 (let ((x-imag (complex-double-reg-imag-tn x))
	       (y-imag (complex-double-reg-imag-tn y)))
	   (move-double-reg y-imag x-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-double-reg-real-tn x)))
	   (inst stdf real-tn nfp offset))
	 (let ((imag-tn (complex-double-reg-imag-tn x)))
	   (inst stdf imag-tn nfp (+ offset (* 2 word-bytes)))))))))
(define-move-vop move-complex-double-float-argument :move-argument
  (complex-double-reg descriptor-reg) (complex-double-reg))

#+long-float
(define-vop (move-complex-long-float-argument)
  (:args (x :scs (complex-long-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-long-reg))))
  (:results (y))
  (:note "complex long-float argument move")
  (:generator 2
    (sc-case y
      (complex-long-reg
       (unless (location= x y)
	 (let ((x-real (complex-long-reg-real-tn x))
	       (y-real (complex-long-reg-real-tn y)))
	   (move-long-reg y-real x-real))
	 (let ((x-imag (complex-long-reg-imag-tn x))
	       (y-imag (complex-long-reg-imag-tn y)))
	   (move-long-reg y-imag x-imag))))
      (complex-long-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-long-reg-real-tn x)))
	   (store-long-reg real-tn nfp offset))
	 (let ((imag-tn (complex-long-reg-imag-tn x)))
	   (store-long-reg imag-tn nfp (+ offset (* 4 word-bytes)))))))))
#+long-float
(define-move-vop move-complex-long-float-argument :move-argument
  (complex-long-reg descriptor-reg) (complex-long-reg))


(define-move-vop move-argument :move-argument
  (single-reg double-reg #+long-float long-reg
   complex-single-reg complex-double-reg #+long-float complex-long-reg)
  (descriptor-reg))


;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-op)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:results (r :scs (,sc)))
		(:arg-types ,ptype ,ptype)
		(:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float)
  #+long-float
  (frob long-float-op long-reg long-float))

(macrolet ((frob (op sinst sname scost dinst dname dcost)
	     `(progn
		(define-vop (,sname single-float-op)
		  (:translate ,op)
		  (:generator ,scost
		    (inst ,sinst r x y)))
		(define-vop (,dname double-float-op)
		  (:translate ,op)
		  (:generator ,dcost
		    (inst ,dinst r x y))))))
  (frob + fadds +/single-float 2 faddd +/double-float 2)
  (frob - fsubs -/single-float 2 fsubd -/double-float 2)
  (frob * fmuls */single-float 4 fmuld */double-float 5)
  (frob / fdivs //single-float 12 fdivd //double-float 19))

#+long-float
(macrolet ((frob (op linst lname lcost)
	     `(define-vop (,lname long-float-op)
		  (:translate ,op)
		  (:generator ,lcost
		    (inst ,linst r x y)))))
  (frob + faddx +/long-float 2)
  (frob - fsubx -/long-float 2)
  (frob * fmulx */long-float 6)
  (frob / fdivx //long-float 20))


(macrolet ((frob (name inst translate sc type)
	     `(define-vop (,name)
		(:args (x :scs (,sc)))
		(:results (y :scs (,sc)))
		(:translate ,translate)
		(:policy :fast-safe)
		(:arg-types ,type)
		(:result-types ,type)
		(:note "inline float arithmetic")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1
		  (note-this-location vop :internal-error)
		  (inst ,inst y x)))))
  (frob abs/single-float fabss abs single-reg single-float)
  (frob %negate/single-float fnegs %negate single-reg single-float))

(define-vop (abs/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate abs)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (cond ((backend-featurep :sparc-v9)
	   (inst fabsd y x))
	  (t
	   (inst fabss y x)
	   (let ((y-odd (make-random-tn :kind :normal
					:sc (sc-or-lose 'single-reg *backend*)
					:offset (+ 1 (tn-offset y))))
		 (x-odd (make-random-tn :kind :normal
					:sc (sc-or-lose 'single-reg *backend*)
					:offset (+ 1 (tn-offset x)))))
	     (inst fmovs y-odd x-odd))))))

(define-vop (%negate/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %negate)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (cond ((backend-featurep :sparc-v9)
	   (inst fnegd y x))
	  (t
	   (inst fnegs y x)
	   (let ((y-odd (make-random-tn :kind :normal
					:sc (sc-or-lose 'single-reg *backend*)
					:offset (+ 1 (tn-offset y))))
		 (x-odd (make-random-tn :kind :normal
					:sc (sc-or-lose 'single-reg *backend*)
					:offset (+ 1 (tn-offset x)))))
	     (inst fmovs y-odd x-odd))))))

#+long-float
(define-vop (abs/long-float)
  (:args (x :scs (long-reg)))
  (:results (y :scs (long-reg)))
  (:translate abs)
  (:policy :fast-safe)
  (:arg-types long-float)
  (:result-types long-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (cond ((backend-featurep :sparc-v9)
	   (inst fabsx y x))
	  (t
	   (inst fabss y x)
	   (dotimes (i 3)
	     (let ((y-odd (make-random-tn
			   :kind :normal
			   :sc (sc-or-lose 'single-reg *backend*)
			   :offset (+ i 1 (tn-offset y))))
		   (x-odd (make-random-tn
			   :kind :normal
			   :sc (sc-or-lose 'single-reg *backend*)
			   :offset (+ i 1 (tn-offset x)))))
	       (inst fmovs y-odd x-odd)))))))

#+long-float
(define-vop (%negate/long-float)
  (:args (x :scs (long-reg)))
  (:results (y :scs (long-reg)))
  (:translate %negate)
  (:policy :fast-safe)
  (:arg-types long-float)
  (:result-types long-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (cond ((backend-featurep :sparc-v9)
	   (inst fnegx y x))
	  (t
	   (inst fnegs y x)
	   (dotimes (i 3)
	     (let ((y-odd (make-random-tn
			   :kind :normal
			   :sc (sc-or-lose 'single-reg *backend*)
			   :offset (+ i 1 (tn-offset y))))
		   (x-odd (make-random-tn
			   :kind :normal
			   :sc (sc-or-lose 'single-reg *backend*)
			   :offset (+ i 1 (tn-offset x)))))
	       (inst fmovs y-odd x-odd)))))))


;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (ecase format
      (:single (inst fcmps x y))
      (:double (inst fcmpd x y))
      (:long (inst fcmpx x y)))
    ;; The SPARC V9 doesn't need an instruction between a
    ;; floating-point compare and a floating-point branch.
    (unless (backend-featurep :sparc-v9)
      (inst nop))
    (inst fb (if not-p nope yep) target)
    (inst nop)))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-compare)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float)
  #+long-float
  (frob long-float-compare long-reg long-float))

(macrolet ((frob (translate yep nope sname dname #+long-float lname)
	     `(progn
		(define-vop (,sname single-float-compare)
		  (:translate ,translate)
		  (:variant :single ,yep ,nope))
		(define-vop (,dname double-float-compare)
		  (:translate ,translate)
		  (:variant :double ,yep ,nope))
	        #+long-float
		(define-vop (,lname long-float-compare)
		  (:translate ,translate)
		  (:variant :long ,yep ,nope)))))
  (frob < :l :ge </single-float </double-float #+long-float </long-float)
  (frob > :g :le >/single-float >/double-float #+long-float >/long-float)
  (frob = :eq :ne eql/single-float eql/double-float #+long-float eql/long-float))

#+long-float
(deftransform eql ((x y) (long-float long-float))
  '(and (= (long-float-low-bits x) (long-float-low-bits y))
	(= (long-float-mid-bits x) (long-float-mid-bits y))
	(= (long-float-high-bits x) (long-float-high-bits y))
	(= (long-float-exp-bits x) (long-float-exp-bits y))))


;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg) :target stack-temp
			  :load-if (not (sc-is x signed-stack))))
		(:temporary (:scs (single-stack) :from :argument) stack-temp)
		(:temporary (:scs (single-reg) :to :result :target y) temp)
		(:results (y :scs (,to-sc)))
		(:arg-types signed-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (let ((stack-tn
			 (sc-case x
			   (signed-reg
			    (inst st x
				  (current-nfp-tn vop)
				  (* (tn-offset temp) vm:word-bytes))
			    stack-temp)
			   (signed-stack
			    x))))
		    (inst ldf temp
			  (current-nfp-tn vop)
			  (* (tn-offset stack-tn) vm:word-bytes))
		    (note-this-location vop :internal-error)
		    (inst ,inst y temp))))))
  (frob %single-float/signed %single-float fitos single-reg single-float)
  (frob %double-float/signed %double-float fitod double-reg double-float)
  #+long-float
  (frob %long-float/signed %long-float fitox long-reg long-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (,from-sc)))
		(:results (y :scs (,to-sc)))
		(:arg-types ,from-type)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 2
		  (note-this-location vop :internal-error)
		  (inst ,inst y x)))))
  (frob %single-float/double-float %single-float fdtos
    double-reg double-float single-reg single-float)
  #+long-float
  (frob %single-float/long-float %single-float fxtos
    long-reg long-float single-reg single-float)
  (frob %double-float/single-float %double-float fstod
    single-reg single-float double-reg double-float)
  #+long-float
  (frob %double-float/long-float %double-float fxtod
    long-reg long-float double-reg double-float)
  #+long-float
  (frob %long-float/single-float %long-float fstox
    single-reg single-float long-reg long-float)
  #+long-float
  (frob %long-float/double-float %long-float fdtox
    double-reg double-float long-reg long-float))

(macrolet ((frob (trans from-sc from-type inst)
	     `(define-vop (,(symbolicate trans "/" from-type))
		(:args (x :scs (,from-sc) :target temp))
		(:temporary (:from (:argument 0) :sc single-reg) temp)
		(:temporary (:scs (signed-stack)) stack-temp)
		(:results (y :scs (signed-reg)
			     :load-if (not (sc-is y signed-stack))))
		(:arg-types ,from-type)
		(:result-types signed-num)
		(:translate ,trans)
		(:policy :fast-safe)
		(:note "inline float truncate")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (note-this-location vop :internal-error)
		  (inst ,inst temp x)
		  (sc-case y
		    (signed-stack
		     (inst stf temp (current-nfp-tn vop)
			   (* (tn-offset y) vm:word-bytes)))
		    (signed-reg
		     (inst stf temp (current-nfp-tn vop)
			   (* (tn-offset stack-temp) vm:word-bytes))
		     (inst ld y (current-nfp-tn vop)
			   (* (tn-offset stack-temp) vm:word-bytes))))))))
  (frob %unary-truncate single-reg single-float fstoi)
  (frob %unary-truncate double-reg double-float fdtoi)
  #+long-float
  (frob %unary-truncate long-reg long-float fxtoi)
  #-sun4
  (frob %unary-round single-reg single-float fstoir)
  #-sun4
  (frob %unary-round double-reg double-float fdtoir))

#+sun4
(deftransform %unary-round ((x) (float) (signed-byte 32))
  '(let* ((trunc (truly-the (signed-byte 32) (%unary-truncate x)))
	  (extra (- x trunc))
	  (absx (abs extra))
	  (one-half (float 1/2 x)))
     (if (if (oddp trunc)
	     (>= absx one-half)
	     (> absx one-half))
	 (truly-the (signed-byte 32) (%unary-truncate (+ x extra)))
	 trunc)))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
	       :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
		 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (signed-stack)) stack-temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case res
	 (single-reg
	  (inst st bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst ldf res (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (single-stack
	  (inst st bits (current-nfp-tn vop)
		(* (tn-offset res) vm:word-bytes)))))
      (signed-stack
       (sc-case res
	 (single-reg
	  (inst ldf res (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes)))
	 (single-stack
	  (unless (location= bits res)
	    (inst ld temp (current-nfp-tn vop)
		  (* (tn-offset bits) vm:word-bytes))
	    (inst st temp (current-nfp-tn vop)
		  (* (tn-offset res) vm:word-bytes)))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
		 :load-if (not (sc-is res double-stack))))
  (:temporary (:scs (double-stack)) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
		      (double-stack res)
		      (double-reg temp))))
      (inst st hi-bits (current-nfp-tn vop)
	    (* (tn-offset stack-tn) vm:word-bytes))
      (inst st lo-bits (current-nfp-tn vop)
	    (* (1+ (tn-offset stack-tn)) vm:word-bytes)))
    (when (sc-is res double-reg)
      (inst lddf res (current-nfp-tn vop)
	    (* (tn-offset temp) vm:word-bytes)))))

#+long-float
(define-vop (make-long-float)
    (:args (hi-bits :scs (signed-reg))
	   (lo1-bits :scs (unsigned-reg))
	   (lo2-bits :scs (unsigned-reg))
	   (lo3-bits :scs (unsigned-reg)))
  (:results (res :scs (long-reg)
		 :load-if (not (sc-is res long-stack))))
  (:temporary (:scs (long-stack)) temp)
  (:arg-types signed-num unsigned-num unsigned-num unsigned-num)
  (:result-types long-float)
  (:translate make-long-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
		      (long-stack res)
		      (long-reg temp))))
      (inst st hi-bits (current-nfp-tn vop)
	    (* (tn-offset stack-tn) vm:word-bytes))
      (inst st lo1-bits (current-nfp-tn vop)
	    (* (1+ (tn-offset stack-tn)) vm:word-bytes))
      (inst st lo2-bits (current-nfp-tn vop)
	    (* (+ 2 (tn-offset stack-tn)) vm:word-bytes))
      (inst st lo3-bits (current-nfp-tn vop)
	    (* (+ 3 (tn-offset stack-tn)) vm:word-bytes)))
    (when (sc-is res long-reg)
      (load-long-reg res (current-nfp-tn vop)
		     (* (tn-offset temp) vm:word-bytes)))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
		:load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
		  :load-if (or (sc-is float descriptor-reg single-stack)
			       (not (sc-is bits signed-stack)))))
  (:temporary (:scs (signed-stack)) stack-temp)
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
	  (inst stf float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst ld bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (single-stack
	  (inst ld bits (current-nfp-tn vop)
		(* (tn-offset float) vm:word-bytes)))
	 (descriptor-reg
	  (loadw bits float vm:single-float-value-slot
		 vm:other-pointer-type))))
      (signed-stack
       (sc-case float
	 (single-reg
	  (inst stf float (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stdf float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (inst ld hi-bits (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes)))
      (double-stack
       (inst ld hi-bits (current-nfp-tn vop)
	     (* (tn-offset float) vm:word-bytes)))
      (descriptor-reg
       (loadw hi-bits float vm:double-float-value-slot
	      vm:other-pointer-type)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stdf float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (inst ld lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset stack-temp)) vm:word-bytes)))
      (double-stack
       (inst ld lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset float)) vm:word-bytes)))
      (descriptor-reg
       (loadw lo-bits float (1+ vm:double-float-value-slot)
	      vm:other-pointer-type)))))

#+long-float
(define-vop (long-float-exp-bits)
  (:args (float :scs (long-reg descriptor-reg)
		:load-if (not (sc-is float long-stack))))
  (:results (exp-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types signed-num)
  (:translate long-float-exp-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
				    :sc (sc-or-lose 'double-reg *backend*)
				    :offset (tn-offset float))))
	 (inst stdf float (current-nfp-tn vop)
	       (* (tn-offset stack-temp) vm:word-bytes)))
       (inst ld exp-bits (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes)))
      (long-stack
       (inst ld exp-bits (current-nfp-tn vop)
	     (* (tn-offset float) vm:word-bytes)))
      (descriptor-reg
       (loadw exp-bits float vm:long-float-value-slot
	      vm:other-pointer-type)))))

#+long-float
(define-vop (long-float-high-bits)
  (:args (float :scs (long-reg descriptor-reg)
		:load-if (not (sc-is float long-stack))))
  (:results (high-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
				    :sc (sc-or-lose 'double-reg *backend*)
				    :offset (tn-offset float))))
	 (inst stdf float (current-nfp-tn vop)
	       (* (tn-offset stack-temp) vm:word-bytes)))
       (inst ld high-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset stack-temp)) vm:word-bytes)))
      (long-stack
       (inst ld high-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset float)) vm:word-bytes)))
      (descriptor-reg
       (loadw high-bits float (1+ vm:long-float-value-slot)
	      vm:other-pointer-type)))))

#+long-float
(define-vop (long-float-mid-bits)
  (:args (float :scs (long-reg descriptor-reg)
		:load-if (not (sc-is float long-stack))))
  (:results (mid-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-mid-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
				    :sc (sc-or-lose 'double-reg *backend*)
				    :offset (+ 2 (tn-offset float)))))
	 (inst stdf float (current-nfp-tn vop)
	       (* (tn-offset stack-temp) vm:word-bytes)))
       (inst ld mid-bits (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes)))
      (long-stack
       (inst ld mid-bits (current-nfp-tn vop)
	     (* (+ 2 (tn-offset float)) vm:word-bytes)))
      (descriptor-reg
       (loadw mid-bits float (+ 2 vm:long-float-value-slot)
	      vm:other-pointer-type)))))

#+long-float
(define-vop (long-float-low-bits)
  (:args (float :scs (long-reg descriptor-reg)
		:load-if (not (sc-is float long-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types long-float)
  (:result-types unsigned-num)
  (:translate long-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (long-reg
       (let ((float (make-random-tn :kind :normal
				    :sc (sc-or-lose 'double-reg *backend*)
				    :offset (+ 2 (tn-offset float)))))
	 (inst stdf float (current-nfp-tn vop)
	       (* (tn-offset stack-temp) vm:word-bytes)))
       (inst ld lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset stack-temp)) vm:word-bytes)))
      (long-stack
       (inst ld lo-bits (current-nfp-tn vop)
	     (* (+ 3 (tn-offset float)) vm:word-bytes)))
      (descriptor-reg
       (loadw lo-bits float (+ 3 vm:long-float-value-slot)
	      vm:other-pointer-type)))))


;;;; Float mode hackery:

(deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:sc unsigned-stack) temp)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (inst stfsr nfp (* word-bytes (tn-offset temp)))
      (loadw res nfp (tn-offset temp))
      (inst nop))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack) temp)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (storew new nfp (tn-offset temp))
      (inst ldfsr nfp (* word-bytes (tn-offset temp)))
      (move res new))))


;;;; Special functions.

#-long-float
(define-vop (fsqrt)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:guard (backend-featurep :sparc-v7))
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (inst fsqrtd y x)))

#+long-float
(define-vop (fsqrt)
  (:args (x :scs (long-reg)))
  (:results (y :scs (long-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types long-float)
  (:result-types long-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (inst fsqrtx y x)))


;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst fmovs r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst fmovs r-imag imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (unless (location= real r)
	   (inst stf real nfp offset))
	 (inst stf imag nfp (+ offset vm:word-bytes)))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note "inline complex double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
	 (unless (location= real r-real)
	   (move-double-reg r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (move-double-reg r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (unless (location= real r)
	   (inst stdf real nfp offset))
	 (inst stdf imag nfp (+ offset (* 2 vm:word-bytes))))))))

#+long-float
(define-vop (make-complex-long-float)
  (:translate complex)
  (:args (real :scs (long-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (long-reg) :to :save))
  (:arg-types long-float long-float)
  (:results (r :scs (complex-long-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-long-stack))))
  (:result-types complex-long-float)
  (:note "inline complex long-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-long-reg
       (let ((r-real (complex-long-reg-real-tn r)))
	 (unless (location= real r-real)
	   (move-long-reg r-real real)))
       (let ((r-imag (complex-long-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (move-long-reg r-imag imag))))
      (complex-long-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (unless (location= real r)
	   (store-long-reg real nfp offset))
	 (store-long-reg imag nfp (+ offset (* 4 vm:word-bytes))))))))

(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg) :target r
	    :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-single-reg
       (let ((value-tn (ecase slot
			 (:real (complex-single-reg-real-tn x))
			 (:imag (complex-single-reg-imag-tn x)))))
	 (unless (location= value-tn r)
	   (inst fmovs r value-tn))))
      (complex-single-stack
       (inst ldf r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 1))
					      (tn-offset x))
					   vm:word-bytes))))))

(define-vop (realpart/complex-single-float complex-single-float-value)
  (:translate realpart)
  (:note "complex single float realpart")
  (:variant :real))

(define-vop (imagpart/complex-single-float complex-single-float-value)
  (:translate imagpart)
  (:note "complex single float imagpart")
  (:variant :imag))

(define-vop (complex-double-float-value)
  (:args (x :scs (complex-double-reg) :target r
	    :load-if (not (sc-is x complex-double-stack))))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-double-reg
       (let ((value-tn (ecase slot
			 (:real (complex-double-reg-real-tn x))
			 (:imag (complex-double-reg-imag-tn x)))))
	 (unless (location= value-tn r)
	   (move-double-reg r value-tn))))
      (complex-double-stack
       (inst lddf r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 2))
					       (tn-offset x))
					    vm:word-bytes))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))

#+long-float
(define-vop (complex-long-float-value)
  (:args (x :scs (complex-long-reg) :target r
	    :load-if (not (sc-is x complex-long-stack))))
  (:arg-types complex-long-float)
  (:results (r :scs (long-reg)))
  (:result-types long-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case x
      (complex-long-reg
       (let ((value-tn (ecase slot
			 (:real (complex-long-reg-real-tn x))
			 (:imag (complex-long-reg-imag-tn x)))))
	 (unless (location= value-tn r)
	   (move-long-reg r value-tn))))
      (complex-long-stack
       (load-long-reg r (current-nfp-tn vop)
		      (* (+ (ecase slot (:real 0) (:imag 4)) (tn-offset x))
			 vm:word-bytes))))))

#+long-float
(define-vop (realpart/complex-long-float complex-long-float-value)
  (:translate realpart)
  (:note "complex long float realpart")
  (:variant :real))

#+long-float
(define-vop (imagpart/complex-long-float complex-long-float-value)
  (:translate imagpart)
  (:note "complex long float imagpart")
  (:variant :imag))
