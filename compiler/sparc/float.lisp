;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/float.lisp,v 1.22 1998/03/11 17:11:35 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;; Sparc conversion by William Lott.
;;; Complex-float support by Douglas Crosher 1998.
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



;;;; Move VOPs:

;;; Exploit the V9 double-float move instruction. This is conditional
;;; on the :sparc-v9 feature.
(defun move-double-reg (dst src)
  (cond ((backend-featurep :sparc-v9)
	 ;; May need to re-map of the register numbers?
	 (inst fmovd dst src))
	(t
	 (inst fmovs dst src)
	 (inst fmovs-odd dst src))))

(macrolet ((frob (vop sc double-p)
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
		      ,@(if double-p
			  `((move-double-reg y x))
			  `((inst fmovs y x))))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg nil)
  (frob double-move double-reg t))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars double-p size type data)
  (:generator 13
    (with-fixed-allocation (y ndescr type size))
    (if double-p
	(inst stdf x y (- (* data vm:word-bytes) vm:other-pointer-type))
	(inst stf x y (- (* data vm:word-bytes) vm:other-pointer-type)))))

(macrolet ((frob (name sc &rest args)
	     `(progn
		(define-vop (,name move-from-float)
		  (:args (x :scs (,sc) :to :save))
		  (:results (y :scs (descriptor-reg)))
		  (:variant ,@args))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    nil vm:single-float-size vm:single-float-type vm:single-float-value-slot)
  (frob move-from-double double-reg
    t vm:double-float-size vm:double-float-type vm:double-float-value-slot))

(macrolet ((frob (name sc double-p value)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:note "pointer to float coercion")
		  (:generator 2
		    (inst ,(if double-p 'lddf 'ldf) y x
			  (- (* ,value vm:word-bytes) vm:other-pointer-type))))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg nil vm:single-float-value-slot)
  (frob move-to-double double-reg t vm:double-float-value-slot))


(macrolet ((frob (name sc stack-sc double-p)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (nfp :scs (any-reg)
			      :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note "float argument move")
		  (:generator ,(if double-p 2 1)
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 ,@(if double-p
			     '((move-double-reg y x))
			     '((inst fmovs y x)))))
		      (,stack-sc
		       (let ((offset (* (tn-offset y) vm:word-bytes)))
			 (inst ,(if double-p 'stdf 'stf) x nfp offset))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack nil)
  (frob move-double-float-argument double-reg double-stack t))


;;;; Complex float move functions
#+complex-float
(progn

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
		  :offset (+ (tn-offset x) 2)))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ (tn-offset x) 2)))


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

) ; progn complex-float


(define-move-vop move-argument :move-argument
  (single-reg double-reg
   #+complex-float complex-single-reg #+complex-float complex-double-reg)
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
  (frob double-float-op double-reg double-float))

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

(macrolet ((frob (name inst translate double-p sc type)
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
		  (inst ,inst y x)
		  ,@(when double-p
		      '((inst fmovs-odd y x)))))))
  (frob abs/single-float fabss abs nil single-reg single-float)
  (frob abs/double-float fabss abs t double-reg double-float)
  (frob %negate/single-float fnegs %negate nil single-reg single-float)
  (frob %negate/double-float fnegs %negate t double-reg double-float))


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
      (:double (inst fcmpd x y)))
    (inst nop)
    (inst fb (if not-p nope yep) target)
    (inst nop)))

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


;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg) :target temp
			  :load-if (not (sc-is x signed-stack))))
		(:temporary (:scs (single-stack)) temp)
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
			    temp)
			   (signed-stack
			    x))))
		    (inst ldf y
			  (current-nfp-tn vop)
			  (* (tn-offset stack-tn) vm:word-bytes))
		    (note-this-location vop :internal-error)
		    (inst ,inst y y))))))
  (frob %single-float/signed %single-float fitos single-reg single-float)
  (frob %double-float/signed %double-float fitod double-reg double-float))

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
  (frob %double-float/single-float %double-float fstod
    single-reg single-float double-reg double-float))

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
	  (loadw bits float vm:single-float-value-slot vm:other-pointer-type))))
      (signed-stack
       (sc-case float
	 (single-reg
	  (inst stf float (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)
		     :load-if (or (sc-is float descriptor-reg double-stack)
				  (not (sc-is hi-bits signed-stack)))))
  (:temporary (:scs (signed-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case hi-bits
      (signed-reg
       (sc-case float
	 (double-reg
	  (inst stf float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst ld hi-bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (double-stack
	  (inst ld hi-bits (current-nfp-tn vop)
		(* (tn-offset float) vm:word-bytes)))
	 (descriptor-reg
	  (loadw hi-bits float vm:double-float-value-slot
		 vm:other-pointer-type))))
      (signed-stack
       (sc-case float
	 (double-reg
	  (inst stf float (current-nfp-tn vop)
		(* (tn-offset hi-bits) vm:word-bytes))))))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)
		     :load-if (or (sc-is float descriptor-reg double-stack)
				  (not (sc-is lo-bits unsigned-stack)))))
  (:temporary (:scs (unsigned-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case lo-bits
      (unsigned-reg
       (sc-case float
	 (double-reg
	  (inst stf-odd float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst ld lo-bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (double-stack
	  (inst ld lo-bits (current-nfp-tn vop)
		(* (1+ (tn-offset float)) vm:word-bytes)))
	 (descriptor-reg
	  (loadw lo-bits float (1+ vm:double-float-value-slot)
		 vm:other-pointer-type))))
      (unsigned-stack
       (sc-case float
	 (double-reg
	  (inst stf-odd float (current-nfp-tn vop)
		(* (tn-offset lo-bits) vm:word-bytes))))))))


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


;;;; Complex float VOPs

#+complex-float
(progn

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r)
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
	 (inst stf real nfp offset)
	 (inst stf imag nfp (+ offset vm:word-bytes)))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r)
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
	 (inst stdf real nfp offset)
	 (inst stdf imag nfp (+ offset (* 2 vm:word-bytes))))))))

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

) ; progn complex-float
