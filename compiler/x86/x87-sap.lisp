;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/x87-sap.lisp,v 1.3 2010/03/19 15:19:01 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the x86 VM definition of SAP operations.
;;;

(in-package :x86)
(intl:textdomain "cmucl-x87")

;;; Sap-Ref-Double
(define-vop (sap-ref-double)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
     (with-empty-tn@fp-top(result)
        (inst fldd (make-ea :dword :base sap :index offset)))))

(define-vop (sap-ref-double-c)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
     (with-empty-tn@fp-top(result)
        (inst fldd (make-ea :dword :base sap :disp offset)))))

(define-vop (%set-sap-ref-double)
  (:translate %set-sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs (double-reg)))
  (:arg-types system-area-pointer signed-num double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fstd (make-ea :dword :base sap :index offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base sap :index offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))

(define-vop (%set-sap-ref-double-c)
  (:translate %set-sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (value :scs (double-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)) double-float)
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fstd (make-ea :dword :base sap :disp offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base sap :disp offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))


;;; Sap-Ref-Single
(define-vop (sap-ref-single)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
     (with-empty-tn@fp-top(result)
        (inst fld (make-ea :dword :base sap :index offset)))))

(define-vop (sap-ref-single-c)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
     (with-empty-tn@fp-top(result)
        (inst fld (make-ea :dword :base sap :disp offset)))))

(define-vop (%set-sap-ref-single)
  (:translate %set-sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs (single-reg)))
  (:arg-types system-area-pointer signed-num single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base sap :index offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base sap :index offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

(define-vop (%set-sap-ref-single-c)
  (:translate %set-sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (value :scs (single-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)) single-float)
  (:info offset)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base sap :disp offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base sap :disp offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))


;;; Sap-Ref-Long
(define-vop (sap-ref-long)
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (#+long-float long-reg #-long-float double-reg)))
  (:result-types #+long-float long-float #-long-float double-float)
  (:generator 5
     (with-empty-tn@fp-top(result)
        (inst fldl (make-ea :dword :base sap :index offset)))))

(define-vop (sap-ref-long-c)
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (#+long-float long-reg #-long-float double-reg)))
  (:result-types #+long-float long-float #-long-float double-float)
  (:generator 4
     (with-empty-tn@fp-top(result)
        (inst fldl (make-ea :dword :base sap :disp offset)))))

(define-vop (%set-sap-ref-long)
  (:translate %set-sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs (#+long-float long-reg #-long-float double-reg)))
  (:arg-types system-area-pointer signed-num #+long-float long-float 
					     #-long-float double-float)
  (:results (result :scs (#+long-float long-reg #-long-float double-reg)))
  (:result-types #+long-float long-float #-long-float double-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (store-long-float (make-ea :dword :base sap :index offset))
	   (unless (zerop (tn-offset result))
	     ;; Value is in ST0 but not result.
	     (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (store-long-float (make-ea :dword :base sap :index offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
		    (inst fstd result))
		  (inst fxch value)))))))

