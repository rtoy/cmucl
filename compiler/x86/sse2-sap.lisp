;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/sse2-sap.lisp,v 1.1.2.1 2008/09/26 18:56:41 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the x86 VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,1998,1999.
;;;
(in-package :x86)

(define-vop (sap-ref-double)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (inst movsd result (make-ea :dword :base sap :index offset))))

(define-vop (sap-ref-double-c)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (inst movsd result (make-ea :dword :base sap :disp offset))))

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
       (inst movsd (make-ea :dword :base sap :index offset) value)
       (inst movsd result value)))

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
       (inst movsd (make-ea :dword :base sap :disp offset) value)
       (inst movsd result value)))

(define-vop (sap-ref-single)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (inst movss result (make-ea :dword :base sap :index offset))))

(define-vop (sap-ref-single-c)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (inst movss result (make-ea :dword :base sap :disp offset))))

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
    (inst movss (make-ea :dword :base sap :index offset) value)
    (inst movss result value)))

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
    (inst movss (make-ea :dword :base sap :disp offset) value)
    (inst movss result value)))

(define-vop (sap-ref-long)
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs ( double-reg)))
  (:result-types double-float)
  (:generator 5
    (inst movsd result (make-ea :dword :base sap :index offset))))

(define-vop (sap-ref-long-c)
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs ( double-reg)))
  (:result-types double-float)
  (:generator 4
    (inst movsd result (make-ea :dword :base sap :disp offset))))

(define-vop (%set-sap-ref-long)
  (:translate %set-sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs ( double-reg)))
  (:arg-types system-area-pointer signed-num double-float)
  (:results (result :scs ( double-reg)))
  (:result-types double-float)
  (:generator 5
    (inst movsd (make-ea :dword :base sap :index offset) value)
    (inst movsd result value)))
