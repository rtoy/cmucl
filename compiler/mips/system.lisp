;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/system.lisp,v 1.6 1990/03/12 23:40:55 wlott Exp $
;;;
;;;    MIPS VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Mips conversion by William Lott and Christopher Hoover.
;;;
(in-package "C")

#+nil
(define-vop (vector-word-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec clc::g-vector-header-words)
    (inst niuo res res clc::g-vector-words-mask-16)))


(define-vop (pointer-compare)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 3
    (three-way-comparison x y condition :unsigned not-p target temp)))

(macrolet ((frob (name cond)
	     `(progn
		(def-primitive-translator ,name (x y) `(,',name ,x ,y))
		(defknown ,name (t t) boolean (movable foldable flushable))
		(define-vop (,name pointer-compare)
		  (:translate ,name)
		  (:variant ,cond)))))
  (frob pointer< :lt)
  (frob pointer> :gt))

(define-vop (check-op)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:variant-vars condition not-p error)
  (:node-var node)
  (:policy :fast-safe)
  (:generator 3
    (let ((target (generate-error-code node error x y)))
      (three-way-comparison x y condition :signed not-p target temp))))

(define-vop (check<= check-op)
  (:variant :gt t di:not-<=-error)
  (:translate check<=))

(define-vop (check= check-op)
  (:variant :eq nil di:not-=-error)
  (:translate check=))



(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst sll res ptr 2)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate unsigned-immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      ((immediate unsigned-immediate)
       (inst sll temp val vm:type-bits)
       (inst ori res temp (tn-value type)))
      (t
       (inst sra temp type 2)
       (inst sll res val (- vm:type-bits 2))
       (inst or res res temp)))))




(define-vop (current-sp)
  (:results (val :scs (any-reg descriptor-reg)))
  (:generator 1
    (move val csp-tn)))



(define-vop (halt)
  (:generator 1
    (inst break vm:halt-trap)))


;;; This guy makes sure that there aren't any random garbage pointers lying
;;; around in registers by clearing all of the boxed registers.  Our allocating
;;; all of the boxed registers as temporaries will prevent any TNs from being
;;; packed in those registers at the time this VOP is invoked.
;;;
#+nil
(define-vop (clear-registers)
  (:temporary (:sc any-reg :offset 1) a0)
  (:temporary (:sc any-reg :offset 3) a1)
  (:temporary (:sc any-reg :offset 5) a2)
  (:temporary (:sc any-reg :offset 4) t0)
  (:temporary (:sc any-reg :offset 7) l0)
  (:temporary (:sc any-reg :offset 8) l1)
  (:temporary (:sc any-reg :offset 9) l2)
  (:temporary (:sc any-reg :offset 10) l3)
  (:temporary (:sc any-reg :offset 11) l4)
  (:generator 10
    (inst lis a0 0)
    (inst lis a1 0)
    (inst lis a2 0)
    (inst lis t0 0)
    (inst lis l0 0)
    (inst lis l1 0)
    (inst lis l2 0)
    (inst lis l3 0)
    (inst lis l4 0)))
