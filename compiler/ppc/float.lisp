;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/ppc/float.lisp,v 1.1 2001/02/11 14:22:04 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the PPC.
;;;
;;; Written by Rob MacLachlan
;;; Sparc conversion by William Lott.
;;;
(in-package "PPC")


;;;; Move functions:

(define-move-function (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst lfs y (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes)))

(define-move-function (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst stfs x (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes)))


(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (inst lfd y nfp offset)))

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (inst stfd x nfp offset)))



;;;; Move VOPs:

(macrolet ((frob (vop sc)
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
                      (inst fmr y x))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:variant-vars double-p size type data)
  (:generator 13
    (with-fixed-allocation (y pa-flag ndescr type size))
    (if double-p
	(inst stfd x y (- (* data vm:word-bytes) vm:other-pointer-type))
	(inst stfs x y (- (* data vm:word-bytes) vm:other-pointer-type)))))

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
		    (inst ,(if double-p 'lfd 'lfs) y x
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
			 (inst fmr y x)))
		      (,stack-sc
		       (let ((offset (* (tn-offset y) vm:word-bytes)))
			 (inst ,(if double-p 'stfd 'stfs) x nfp offset))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack nil)
  (frob move-double-float-argument double-reg double-stack t))


(define-move-vop move-argument :move-argument
  (single-reg double-reg) (descriptor-reg))


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
  (frob + fadds +/single-float 2 fadd +/double-float 2)
  (frob - fsubs -/single-float 2 fsub -/double-float 2)
  (frob * fmuls */single-float 4 fmul */double-float 5)
  (frob / fdivs //single-float 12 fdiv //double-float 19))

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
  (frob abs/single-float fabs abs single-reg single-float)
  (frob abs/double-float fabs abs double-reg double-float)
  (frob %negate/single-float fneg %negate single-reg single-float)
  (frob %negate/double-float fneg %negate double-reg double-float))


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
      ((:single :double)
       (inst fcmpo :cr1 x y)))
    (inst b?  :cr1 (if not-p nope yep) target)))

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
  (frob < :lt :ge </single-float </double-float)
  (frob > :gt :le >/single-float >/double-float)
  (frob = :eq :ne eql/single-float eql/double-float))


;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg)))
		(:temporary (:scs (double-stack)) temp)
                (:temporary (:scs (double-reg)) fmagic)
                (:temporary (:scs (signed-reg)) rtemp)
		(:results (y :scs (,to-sc)))
		(:arg-types signed-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (let* ((stack-offset (* (tn-offset temp) vm:word-bytes))
                         (nfp-tn (current-nfp-tn vop))
                         (temp-offset-high (* stack-offset vm:word-bytes))
                         (temp-offset-low (* (1+ stack-offset) vm:word-bytes)))
                    (inst lis rtemp #x4330)   ; High word of magic constant
                    (inst stw rtemp nfp-tn temp-offset-high)
                    (inst lis rtemp #x8000)
                    (inst stw rtemp nfp-tn temp-offset-low)
                    (inst lfd fmagic nfp-tn temp-offset-high)
                    (inst xor rtemp rtemp x)          ; invert sign bit of x : rtemp had #x80000000
                    (inst stw rtemp nfp-tn temp-offset-low)
                    (inst lfd y nfp-tn temp-offset-high) 		    
		    (note-this-location vop :internal-error)
		    (inst ,inst y y fmagic))))))
  (frob %single-float/signed %single-float fsubs single-reg single-float)
  (frob %double-float/signed %double-float fsub double-reg double-float))

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
  (frob %single-float/double-float %single-float frsp
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float fmr
    single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
	     `(define-vop (,(symbolicate trans "/" from-type))
		(:args (x :scs (,from-sc) :target temp))
		(:temporary (:from (:argument 0) :sc single-reg) temp)
		(:temporary (:scs (double-stack)) stack-temp)
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
		     (inst stfd temp (current-nfp-tn vop)
			   (* (tn-offset y) vm:word-bytes)))
		    (signed-reg
		     (inst stfd temp (current-nfp-tn vop)
			   (* (tn-offset stack-temp) vm:word-bytes))
		     (inst lwz y (current-nfp-tn vop)
			   (+ 4 (* (tn-offset stack-temp) vm:word-bytes)))))))))
  (frob %unary-truncate single-reg single-float fctiwz)
  (frob %unary-truncate double-reg double-float fctiwz)
  (frob %unary-round single-reg single-float fctiw)
  (frob %unary-round double-reg double-float fctiw))



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
	  (inst stw bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst lfs res (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (single-stack
	  (inst stw bits (current-nfp-tn vop)
		(* (tn-offset res) vm:word-bytes)))))
      (signed-stack
       (sc-case res
	 (single-reg
	  (inst lfs res (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes)))
	 (single-stack
	  (unless (location= bits res)
	    (inst lwz temp (current-nfp-tn vop)
		  (* (tn-offset bits) vm:word-bytes))
	    (inst stw temp (current-nfp-tn vop)
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
      (inst stw hi-bits (current-nfp-tn vop)
	    (* (tn-offset stack-tn) vm:word-bytes))
      (inst stw lo-bits (current-nfp-tn vop)
	    (* (1+ (tn-offset stack-tn)) vm:word-bytes)))
    (when (sc-is res double-reg)
      (inst lfd res (current-nfp-tn vop)
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
	  (inst stfs float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst lwz bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (single-stack
	  (inst lwz bits (current-nfp-tn vop)
		(* (tn-offset float) vm:word-bytes)))
	 (descriptor-reg
	  (loadw bits float vm:single-float-value-slot vm:other-pointer-type))))
      (signed-stack
       (sc-case float
	 (single-reg
	  (inst stfs float (current-nfp-tn vop)
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
	  (inst stfd float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst lwz hi-bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (double-stack
	  (inst lwz hi-bits (current-nfp-tn vop)
		(* (tn-offset float) vm:word-bytes)))
	 (descriptor-reg
	  (loadw hi-bits float vm:double-float-value-slot
		 vm:other-pointer-type))))
      (signed-stack
       (sc-case float
	 (double-reg
	  (inst stfd float (current-nfp-tn vop)
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
	  (inst stfd float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst lwz lo-bits (current-nfp-tn vop)
		(* (1+ (tn-offset stack-temp)) vm:word-bytes)))
	 (double-stack
	  (inst lwz lo-bits (current-nfp-tn vop)
		(* (1+ (tn-offset float)) vm:word-bytes)))
	 (descriptor-reg
	  (loadw lo-bits float (1+ vm:double-float-value-slot)
		 vm:other-pointer-type))))
      (unsigned-stack
       (sc-case float
	 (double-reg
	  (inst stfd float (current-nfp-tn vop)
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
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc single-reg) fp-temp)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (inst mffs fp-temp)
      (inst stfd fp-temp nfp (* word-bytes (tn-offset temp)))
      (loadw res nfp (1+ (tn-offset temp))))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc single-reg) fp-temp)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (storew new nfp (1+ (tn-offset temp)))
      (inst lfd fp-temp nfp (* word-bytes (tn-offset temp)))
      (inst mtfsf 255 fp-temp)
      (move res new))))
