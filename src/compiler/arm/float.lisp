;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/float.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the ARM.
;;;
;;;
(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")


;;;; Move functions:

(define-move-function (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (not-implemented "DEFINE-MOVE LOAD-SINGLE")
  (load-symbol-value lip-tn *number-frame-pointer*)
  (inst vldr y lip-tn (* (tn-offset x) vm:word-bytes)))

(define-move-function (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (not-implemented "DEFINE-MOVE STORE-SINGLE")
  (load-symbol-value lip-tn *number-frame-pointer*)
  (inst vstr x lip-tn (* (tn-offset y) vm:word-bytes)))


(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (not-implemented "DEFINE-MOVE LOAD-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (inst vldr y lip-tn offset)))

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (not-implemented "DEFINE-MOVE STORE-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (inst vstr x lip-tn offset)))


;;;; Move VOPs:

(defun move-double-reg (dst src)
  (unless (location= dst src)
    (inst vmov dst src)))

(macrolet ((frob (vop sc format)
	     `(progn
		(define-vop (,vop)
		  (:args (x :scs (,sc)
			    :target y
			    :load-if (not (location= x y))))
		  (:results (y :scs (,sc)
			       :load-if (not (location= x y))))
		  (:note _N"float move")
		  (:generator 0
		    (emit-not-implemented)
		    (unless (location= y x)
		      ,@(ecase format
			  (:single `((inst vmov y x)))
			  (:double `((inst vmov y x)))))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg :single)
  (frob double-move double-reg :double))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note _N"float to pointer coercion")
  (:variant-vars format size type data)
  (:generator 13
    (emit-not-implemented)))

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
    vm:double-float-size vm:double-float-type vm:double-float-value-slot))

(macrolet ((frob (name sc format value)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:note _N"pointer to float coercion")
		  (:generator 2
		    (emit-not-implemented)))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg :single vm:single-float-value-slot)
  (frob move-to-double double-reg :double vm:double-float-value-slot))

(macrolet ((frob (name sc stack-sc format)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (nfp :scs (any-reg)
			      :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note _N"float argument move")
		  (:generator ,(ecase format (:single 1) (:double 2))
		    (emit-not-implemented)
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 (inst vmov y x)))
		      (,stack-sc
		       (let ((offset (* (tn-offset y) vm:word-bytes)))
			 (inst vstr x nfp offset))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack :single)
  (frob move-double-float-argument double-reg double-stack :double))


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

#+double-double
(progn
(defun complex-double-double-reg-real-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-double-double-reg-real-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 2 (tn-offset x))))
(defun complex-double-double-reg-imag-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 4 (tn-offset x))))
(defun complex-double-double-reg-imag-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 6 (tn-offset x))))
)

(define-move-function (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (not-implemented "DEFINE-MOVE LOAD-COMPLEX-SINGLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst vldr real-tn lip-tn offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst vldr imag-tn lip-tn  (+ offset vm:word-bytes)))))

(define-move-function (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (not-implemented "DEFINE-MOVE STORE-COMPLEX-SINGLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst vstr real-tn lip-tn offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst vstr imag-tn lip-tn (+ offset vm:word-bytes)))))


(define-move-function (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (not-implemented "DEFINE-MOVE LOAD-COMPLEX-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst vldr real-tn lip-tn offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst vldr imag-tn lip-tn (+ offset (* 2 vm:word-bytes))))))

(define-move-function (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (not-implemented "DEFINE-MOVE STORE-COMPLEX-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst vstr real-tn lip-tn offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst vstr imag-tn lip-tn (+ offset (* 2 vm:word-bytes))))))

#+double-double
(progn
(define-move-function (load-complex-double-double 4) (vop x y)
  ((complex-double-double-stack) (complex-double-double-reg))
  (not-implemented "DEFINE-MOVE LOAD-COMPLEX-DOUBLE-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((value-tn (complex-double-double-reg-real-hi-tn y)))
    (load-symbol-value lip-tn *number-frame-pointer*)
      (inst vldr value-tn lip-tn offset))
    (let ((value-tn (complex-double-double-reg-real-lo-tn y)))
      (inst vldr value-tn lip-tn (+ offset (* 2 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-hi-tn y)))
      (inst vldr value-tn lip-tn (+ offset (* 4 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-lo-tn y)))
      (inst vldr value-tn lip-tn (+ offset (* 6 vm:word-bytes))))))

(define-move-function (store-complex-double-double 4) (vop x y)
  ((complex-double-double-reg) (complex-double-double-stack))
  (not-implemented "DEFINE-MOVE STORE-COMPLEX-DOUBLE-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((value-tn (complex-double-double-reg-real-hi-tn x)))
      (inst vstr value-tn lip-tn offset))
    (let ((value-tn (complex-double-double-reg-real-lo-tn x)))
      (inst vstr value-tn lip-tn (+ offset (* 2 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-hi-tn x)))
      (inst vstr value-tn lip-tn (+ offset (* 4 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-lo-tn x)))
      (inst vstr value-tn lip-tn (+ offset (* 6 vm:word-bytes))))))

)

;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note _N"complex single float move")
  (:generator 0
     (emit-not-implemented)
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-single-reg-real-tn x))
	     (y-real (complex-single-reg-real-tn y)))
	 (inst vmov y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
	     (y-imag (complex-single-reg-imag-tn y)))
	 (inst vmov y-imag x-imag)))))
;;;
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note _N"complex double float move")
  (:generator 0
     (emit-not-implemented)
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

#+double-double
(define-vop (complex-double-double-move)
  (:args (x :scs (complex-double-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-double-reg) :load-if (not (location= x y))))
  (:note _N"complex double-double float move")
  (:generator 0
     (emit-not-implemented)
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-double-reg-real-hi-tn x))
	     (y-real (complex-double-double-reg-real-hi-tn y)))
	 (move-double-reg y-real x-real))
       (let ((x-real (complex-double-double-reg-real-lo-tn x))
	     (y-real (complex-double-double-reg-real-lo-tn y)))
	 (move-double-reg y-real x-real))
       (let ((x-real (complex-double-double-reg-imag-hi-tn x))
	     (y-real (complex-double-double-reg-imag-hi-tn y)))
	 (move-double-reg y-real x-real))
       (let ((x-imag (complex-double-double-reg-imag-lo-tn x))
	     (y-imag (complex-double-double-reg-imag-lo-tn y)))
	 (move-double-reg y-imag x-imag)))))
;;;
#+double-double
(define-move-vop complex-double-double-move :move
  (complex-double-double-reg) (complex-double-double-reg))

;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"complex single float to pointer coercion")
  (:generator 13
    (emit-not-implemented)))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"complex double float to pointer coercion")
  (:generator 13
    (emit-not-implemented)))
;;;
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

#+double-double
(define-vop (move-from-complex-double-double)
  (:args (x :scs (complex-double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"complex double-double float to pointer coercion")
  (:generator 13
    (emit-not-implemented)))
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
    (emit-not-implemented)))

(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note _N"pointer to complex float coercion")
  (:generator 2
    (emit-not-implemented)))

(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

#+double-double
(define-vop (move-to-complex-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-double-reg)))
  (:note _N"pointer to complex double-double float coercion")
  (:generator 2
    (emit-not-implemented)))

#+double-double
(define-move-vop move-to-complex-double-double :move
  (descriptor-reg) (complex-double-double-reg))

;;;
;;; Complex float move-argument vop
;;;
(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note _N"complex single-float argument move")
  (:generator 1
    (emit-not-implemented)
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
	 (let ((x-real (complex-single-reg-real-tn x))
	       (y-real (complex-single-reg-real-tn y)))
	   (inst vmov y-real x-real))
	 (let ((x-imag (complex-single-reg-imag-tn x))
	       (y-imag (complex-single-reg-imag-tn y)))
	   (inst vmov y-imag x-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-single-reg-real-tn x)))
	   (inst vstr real-tn nfp offset))
	 (let ((imag-tn (complex-single-reg-imag-tn x)))
	   (inst vstr imag-tn nfp (+ offset word-bytes))))))))
(define-move-vop move-complex-single-float-argument :move-argument
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note _N"complex double-float argument move")
  (:generator 2
    (emit-not-implemented)
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
	   (inst vstr real-tn nfp offset))
	 (let ((imag-tn (complex-double-reg-imag-tn x)))
	   (inst vstr imag-tn nfp (+ offset (* 2 word-bytes)))))))))
(define-move-vop move-complex-double-float-argument :move-argument
  (complex-double-reg descriptor-reg) (complex-double-reg))

#+double-double
(define-vop (move-complex-double-double-float-argument)
  (:args (x :scs (complex-double-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-double-reg))))
  (:results (y))
  (:note _N"complex double-double float argument move")
  (:generator 2
    (emit-not-implemented)
    (sc-case y
      (complex-double-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-double-reg-real-hi-tn x))
	       (y-real (complex-double-double-reg-real-hi-tn y)))
	   (move-double-reg y-real x-real))
	 (let ((x-real (complex-double-double-reg-real-lo-tn x))
	       (y-real (complex-double-double-reg-real-lo-tn y)))
	   (move-double-reg y-real x-real))
	 (let ((x-imag (complex-double-double-reg-imag-hi-tn x))
	       (y-imag (complex-double-double-reg-imag-hi-tn y)))
	   (move-double-reg y-imag x-imag))
	 (let ((x-imag (complex-double-double-reg-imag-lo-tn x))
	       (y-imag (complex-double-double-reg-imag-lo-tn y)))
	   (move-double-reg y-imag x-imag))))
      (complex-double-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-double-double-reg-real-hi-tn x)))
	   (inst vstr real-tn nfp offset))
	 (let ((real-tn (complex-double-double-reg-real-lo-tn x)))
	   (inst vstr real-tn nfp (+ offset (* 2 word-bytes))))
	 (let ((imag-tn (complex-double-double-reg-imag-hi-tn x)))
	   (inst vstr imag-tn nfp (+ offset (* 4 word-bytes))))
	 (let ((imag-tn (complex-double-double-reg-imag-lo-tn x)))
	   (inst vstr imag-tn nfp (+ offset (* 6 word-bytes)))))))))

#+double-double
(define-move-vop move-complex-double-double-float-argument :move-argument
  (complex-double-double-reg descriptor-reg) (complex-double-double-reg))

(define-move-vop move-argument :move-argument
  (single-reg double-reg #+double-double double-double-reg
   complex-single-reg complex-double-reg
   #+double-double complex-double-double-reg)
  (descriptor-reg))


;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note _N"inline float arithmetic")
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
  (frob double-float-op double-reg double-float))

(macrolet ((frob (op sinst sname scost dinst dname dcost)
	     `(progn
		(define-vop (,sname single-float-op)
		  (:translate ,op)
		  (:generator ,scost
		    (emit-not-implemented)
		    (inst ,sinst r x y)))
		(define-vop (,dname double-float-op)
		  (:translate ,op)
		  (:generator ,dcost
		    (emit-not-implemented)
		    (inst ,dinst r x y))))))
  (frob + vadd +/single-float 2 vadd +/double-float 2)
  (frob - vsub -/single-float 2 vsub -/double-float 2)
  (frob * vmul */single-float 4 vmul */double-float 5)
  (frob / vdiv //single-float 12 vdiv //double-float 19))


(macrolet ((frob (name inst translate sc type)
	     `(define-vop (,name)
		(:args (x :scs (,sc)))
		(:results (y :scs (,sc)))
		(:translate ,translate)
		(:policy :fast-safe)
		(:arg-types ,type)
		(:result-types ,type)
		(:note _N"inline float arithmetic")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1
		  (note-this-location vop :internal-error)
		  (emit-not-implemented)
		  (inst ,inst y x)))))
  (frob abs/single-float vabs abs single-reg single-float)
  (frob %negate/single-float vneg %negate single-reg single-float))

(defun negate-double-reg (dst src)
  (inst vneg dst src))

(defun abs-double-reg (dst src)
  (inst vabs dst src))

(define-vop (abs/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate abs)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (emit-not-implemented)
    (abs-double-reg y x)))

(define-vop (%negate/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %negate)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-this-location vop :internal-error)
    (emit-not-implemented)
    (negate-double-reg y x)))

;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note _N"inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (emit-not-implemented)))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-compare)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate yep nope sname dname)
	     `(progn
		(define-vop (,sname single-float-compare)
		  (:translate ,translate)
		  (:variant :single ,yep ,nope))
		(define-vop (,dname double-float-compare)
		  (:translate ,translate)
		  (:variant :double ,yep ,nope)))))
  (frob < :l :ge </single-float </double-float)
  (frob > :g :le >/single-float >/double-float)
  (frob = :eq :ne eql/single-float eql/double-float))

#+double-double
(deftransform eql ((x y) (double-double-float double-double-float))
  '(and (eql (double-double-hi x) (double-double-hi y))
	(eql (double-double-lo x) (double-double-lo y))))


;;;; Conversion:

;; Tell the compiler about %%single-float and %%double-float.  Add
;; functions for byte-compiled code.
(macrolet
    ((frob (name type)
       `(progn
	  (defknown ,name ((signed-byte 32))
	    ,type)
	  (defun ,name (n)
	    (declare (type (signed-byte 32) n))
	    (,name n)))))
  (frob %%single-float single-float)
  (frob %%double-float double-float))

(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg) :target stack-temp
			  :load-if (not (sc-is x signed-stack))))
		(:temporary (:scs (single-stack) :from :argument) stack-temp)
		(:results (y :scs (,to-sc)))
		(:arg-types signed-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note _N"inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (emit-not-implemented)))))
  (frob %single-float/signed %%single-float fitos single-reg single-float)
  (frob %double-float/signed %%double-float fitod double-reg double-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (,from-sc)))
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
		  (emit-not-implemented)
		  (inst ,inst y x)))))
  (frob %single-float/double-float %single-float vcvt.f32.f64
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float vcvt.f64.f32
    single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
	     `(define-vop (,(symbolicate trans "/" from-type))
		(:args (x :scs (,from-sc)))
		(:results (y :scs (signed-reg)
			     :load-if (not (sc-is y signed-stack))))
		(:arg-types ,from-type)
		(:result-types signed-num)
		(:translate ,trans)
		(:policy :fast-safe)
		(:note _N"inline float truncate")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (note-this-location vop :internal-error)
		  (emit-not-implemented)))))
  (frob %unary-truncate single-reg single-float fstoi)
  (frob %unary-truncate double-reg double-float fdtoi))

#+nil
(define-vop (fast-unary-ftruncate/single-float)
  (:args (x :scs (single-reg)))
  (:arg-types single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:policy :fast-safe)
  (:translate c::fast-unary-ftruncate)
  (:note _N"inline ftruncate")
  (:generator 2
    (inst vcvt.s32.f32 r x)
    (inst vcvt.f32.s32 r r)))

#+nil
(define-vop (fast-unary-ftruncate/double-float)
  (:args (x :scs (double-reg) :target r))
  (:arg-types double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:policy :fast-safe)
  (:translate c::fast-unary-ftruncate)
  (:note _N"inline ftruncate")
  (:generator 2
    (inst vcvt.s32.f64 r x)
    (inst vcvt.f64.s32 r r)))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
	       :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
		 :load-if (not (sc-is res single-stack))))
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (emit-not-implemented)))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
		 :load-if (not (sc-is res double-stack))))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (emit-not-implemented)))


(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
		:load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
		  :load-if (or (sc-is float descriptor-reg single-stack)
			       (not (sc-is bits signed-stack)))))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (emit-not-implemented)))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)))

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
    (emit-not-implemented)))

#+nil
(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg))
	    (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num unsigned-num)
  (:translate kernel::double-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)))


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

  (:generator 3
    (emit-not-implemented)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)))



;;;; Special functions.

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
    (emit-not-implemented)
    (inst vsqrt y x)))


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
  (:note _N"inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note _N"inline complex double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)))

(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg descriptor-reg) :target r
	    :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)))

(define-vop (realpart/complex-single-float complex-single-float-value)
  (:translate realpart)
  (:note _N"complex single float realpart")
  (:variant :real))

(define-vop (imagpart/complex-single-float complex-single-float-value)
  (:translate imagpart)
  (:note _N"complex single float imagpart")
  (:variant :imag))

(define-vop (complex-double-float-value)
  (:args (x :scs (complex-double-reg descriptor-reg) :target r
	    :load-if (not (sc-is x complex-double-stack))))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note _N"complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note _N"complex double float imagpart")
  (:variant :imag))


;;; Support for double-double floats

#+double-double
(progn
(defun double-double-reg-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))

(defun double-double-reg-lo-tn (x)
  ;; The low tn is 2 more than the offset because double regs are
  ;; even.
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 2 (tn-offset x))))

(define-move-function (load-double-double 4) (vop x y)
  ((double-double-stack) (double-double-reg))
  (not-implemented "DEFINE-MOVE LOAD-DOUBLE-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((hi-tn (double-double-reg-hi-tn y)))
      (inst vldr hi-tn lip-tn offset))
    (let ((lo-tn (double-double-reg-lo-tn y)))
      (inst vldr lo-tn lip-tn (+ offset (* 2 vm:word-bytes))))))

(define-move-function (store-double-double 4) (vop x y)
  ((double-double-reg) (double-double-stack))
  (not-implemented "DEFINE-MOVE STORE-DOUBLE-DOUBLE")
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (load-symbol-value lip-tn *number-frame-pointer*)
    (let ((hi-tn (double-double-reg-hi-tn x)))
      (inst vstr hi-tn lip-tn offset))
    (let ((lo-tn (double-double-reg-lo-tn x)))
      (inst vstr lo-tn lip-tn (+ offset (* 2 vm:word-bytes))))))

;;; Double-double float register to register moves

(define-vop (double-double-move)
  (:args (x :scs (double-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (double-double-reg) :load-if (not (location= x y))))
  (:note _N"double-double float move")
  (:generator 0
     (emit-not-implemented)
     (unless (location= x y)
       ;; Note the double-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-hi (double-double-reg-hi-tn x))
	     (y-hi (double-double-reg-hi-tn y)))
	 (move-double-reg y-hi x-hi))
       (let ((x-lo (double-double-reg-lo-tn x))
	     (y-lo (double-double-reg-lo-tn y)))
	 (move-double-reg y-lo x-lo)))))
;;;
(define-move-vop double-double-move :move
  (double-double-reg) (double-double-reg))

;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.

(define-vop (move-from-double-double)
  (:args (x :scs (double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"double-double float to pointer coercion")
  (:generator 13
    (emit-not-implemented)))
;;;
(define-move-vop move-from-double-double :move
  (double-double-reg) (descriptor-reg))

;;; Move from a descriptor to a double-double float register

(define-vop (move-to-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-double-reg)))
  (:note _N"pointer to double-double float coercion")
  (:generator 2
    (emit-not-implemented)))

(define-move-vop move-to-double-double :move
  (descriptor-reg) (double-double-reg))

;;; double-double float move-argument vop

(define-vop (move-double-double-float-argument)
  (:args (x :scs (double-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y double-double-reg))))
  (:results (y))
  (:note _N"double-double float argument move")
  (:generator 2
    (emit-not-implemented)
    (sc-case y
      (double-double-reg
       (unless (location= x y)
	 (let ((x-hi (double-double-reg-hi-tn x))
	       (y-hi (double-double-reg-hi-tn y)))
	   (move-double-reg y-hi x-hi))
	 (let ((x-lo (double-double-reg-lo-tn x))
	       (y-lo (double-double-reg-lo-tn y)))
	   (move-double-reg y-lo x-lo))))
      (double-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((hi-tn (double-double-reg-hi-tn x)))
	   (inst vstr hi-tn nfp offset))
	 (let ((lo-tn (double-double-reg-lo-tn x)))
	   (inst vstr lo-tn nfp (+ offset (* 2 word-bytes)))))))))

(define-move-vop move-double-double-float-argument :move-argument
  (double-double-reg descriptor-reg) (double-double-reg))


(define-vop (make/double-double-float)
  (:args (hi :scs (double-reg) :target res
	     :load-if (not (location= hi res)))
	 (lo :scs (double-reg) :to :save))
  (:results (res :scs (double-double-reg) :from (:argument 0)
		 :load-if (not (sc-is res double-double-stack))))
  (:arg-types double-float double-float)
  (:result-types double-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:translate kernel::%make-double-double-float)
  (:note _N"inline double-double float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)
    (sc-case res
      (double-double-reg
       (let ((res-hi (double-double-reg-hi-tn res)))
	 (unless (location= res-hi hi)
	   (move-double-reg res-hi hi)))
       (let ((res-lo (double-double-reg-lo-tn res)))
	 (unless (location= res-lo lo)
	   (move-double-reg res-lo lo))))
      (double-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset res) vm:word-bytes)))
	 (load-symbol-value lip *number-frame-pointer*)
	 (unless (location= hi res)
	   (inst vstr hi lip offset))
	 (inst vstr lo lip (+ offset (* 2 vm:word-bytes))))))))

(define-vop (double-double-float-value)
  (:args (x :scs (double-double-reg descriptor-reg) :target r
	    :load-if (not (sc-is x double-double-stack))))
  (:arg-types double-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case x
      (double-double-reg
       (let ((value-tn (ecase slot
			 (:hi (double-double-reg-hi-tn x))
			 (:lo (double-double-reg-lo-tn x)))))
	 (unless (location= value-tn r)
	   (move-double-reg r value-tn))))
      (double-double-stack
       (load-symbol-value lip *number-frame-pointer*)
       (inst vldr r
	     lip (* (+ (ecase slot (:hi 0) (:lo 2))
			  (tn-offset x))
		       vm:word-bytes)))
      (descriptor-reg
       (emit-not-implemented)))))

(define-vop (hi/double-double-value double-double-float-value)
  (:translate kernel::double-double-hi)
  (:note _N"double-double high part")
  (:variant :hi))

(define-vop (lo/double-double-value double-double-float-value)
  (:translate kernel::double-double-lo)
  (:note _N"double-double low part")
  (:variant :lo))


(define-vop (make-complex-double-double-float)
  (:translate complex)
  (:args (real :scs (double-double-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (double-double-reg) :to :save))
  (:arg-types double-double-float double-double-float)
  (:results (r :scs (complex-double-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-double-stack))))
  (:result-types complex-double-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:note _N"inline complex double-double float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)
    (sc-case r
      (complex-double-double-reg
       (let ((r-real (complex-double-double-reg-real-hi-tn r))
	     (real-hi (double-double-reg-hi-tn real)))
	 (move-double-reg r-real real-hi))
       (let ((r-real (complex-double-double-reg-real-lo-tn r))
	     (real-lo (double-double-reg-lo-tn real)))
	 (move-double-reg r-real real-lo))
       (let ((r-imag (complex-double-double-reg-imag-hi-tn r))
	     (imag-hi (double-double-reg-hi-tn imag)))
	 (move-double-reg r-imag imag-hi))
       (let ((r-imag (complex-double-double-reg-imag-lo-tn r))
	     (imag-lo (double-double-reg-lo-tn imag)))
	 (move-double-reg r-imag imag-lo)))
      (complex-double-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (load-symbol-value lip *number-frame-pointer*)
	 (let ((r-real (double-double-reg-hi-tn real)))
	   (inst vstr r-real lip offset))
	 (let ((r-real (double-double-reg-lo-tn real)))
	   (inst vstr r-real lip  (+ offset (* 2 vm:word-bytes))))
	 (let ((r-imag (double-double-reg-hi-tn imag)))
	   (inst vstr r-imag lip (+ offset (* 4 vm:word-bytes))))
	 (let ((r-imag (double-double-reg-lo-tn imag)))
	   (inst vstr r-imag lip (+ offset (* 6 vm:word-bytes)))))))))

(define-vop (complex-double-double-float-value)
  (:args (x :scs (complex-double-double-reg descriptor-reg)
	    :load-if (not (or (sc-is x complex-double-double-stack)))))
  (:arg-types complex-double-double-float)
  (:results (r :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case x
      (complex-double-double-reg
       (let ((value-tn (ecase slot
			 (:real (complex-double-double-reg-real-hi-tn x))
			 (:imag (complex-double-double-reg-imag-hi-tn x))))
	     (r-hi (double-double-reg-hi-tn r)))
	 (unless (location= value-tn r-hi)
	   (move-double-reg r-hi value-tn)))
       (let ((value-tn (ecase slot
			 (:real (complex-double-double-reg-real-lo-tn x))
			 (:imag (complex-double-double-reg-imag-lo-tn x))))
	     (r-lo (double-double-reg-lo-tn r)))
	 (unless (location= value-tn r-lo)
	   (move-double-reg r-lo value-tn))))
      (complex-double-double-stack
       (let ((r-hi (double-double-reg-hi-tn r)))
	 (load-symbol-value lip *number-frame-pointer*)
	 (inst vldr r-hi
	       lip  (* (+ (ecase slot (:real 0) (:imag 4))
			     (tn-offset x))
			  vm:word-bytes)))
       (let ((r-lo (double-double-reg-lo-tn r)))
	 (inst vldr r-lo
	       lip (* (+ (ecase slot (:real 2) (:imag 6))
			    (tn-offset x))
			 vm:word-bytes))))
      (descriptor-reg
       (emit-not-implemented)))))

(define-vop (realpart/complex-double-double-float complex-double-double-float-value)
  (:translate realpart)
  (:note _N"complex double-double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-double-float complex-double-double-float-value)
  (:translate imagpart)
  (:note _N"complex double-double float imagpart")
  (:variant :imag))

); progn

