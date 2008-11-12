1;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/sse2-sap.lisp,v 1.2 2008/11/12 15:04:23 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the x86 VM definition of SAP operations.
;;;

(in-package :x86)

(macrolet
    ((frob (name type inst)
       (let ((sc-type (symbolicate type "-REG"))
	     (res-type (symbolicate type "-FLOAT")))
	 `(progn
	    (define-vop (,(symbolicate "SAP-REF-" name))
	      (:translate ,(symbolicate "SAP-REF-" name))
	      (:policy :fast-safe)
	      (:args (sap :scs (sap-reg))
		     (offset :scs (signed-reg)))
	      (:arg-types system-area-pointer signed-num)
	      (:results (result :scs (,sc-type)))
	      (:result-types ,res-type)
	      (:generator 5
		(inst ,inst result (make-ea :dword :base sap :index offset))))
	    (define-vop (,(symbolicate "SAP-REF-" type "-C"))
		(:translate ,(symbolicate "SAP-REF-" type))
	      (:policy :fast-safe)
	      (:args (sap :scs (sap-reg)))
	      (:arg-types system-area-pointer (:constant (signed-byte 32)))
	      (:info offset)
	      (:results (result :scs (,sc-type)))
	      (:result-types ,res-type)
	      (:generator 4
		(inst ,inst result (make-ea :dword :base sap :disp offset))))
	    (define-vop (,(symbolicate "%SET-SAP-REF-" type))
	      (:translate ,(symbolicate "%SET-SAP-REF-" type))
	      (:policy :fast-safe)
	      (:args (sap :scs (sap-reg) :to (:eval 0))
		     (offset :scs (signed-reg) :to (:eval 0))
		     (value :scs (,sc-type)))
	      (:arg-types system-area-pointer signed-num ,res-type)
	      (:results (result :scs (,sc-type)))
	      (:result-types ,res-type)
	      (:generator 5
		(inst ,inst (make-ea :dword :base sap :index offset) value)
		(unless (location= result value)
		  (inst ,inst result value))))
	    (define-vop (,(symbolicate "%SET-SAP-REF-" type "-C"))
	      (:translate ,(symbolicate "%SET-SAP-REF-" type))
	      (:policy :fast-safe)
	      (:args (sap :scs (sap-reg) :to (:eval 0))
		     (value :scs (,sc-type)))
	      (:arg-types system-area-pointer (:constant (signed-byte 32))
			  ,res-type)
	      (:info offset)
	      (:results (result :scs (,sc-type)))
	      (:result-types ,res-type)
	      (:generator 4
		(inst ,inst (make-ea :dword :base sap :disp offset) value)
		(unless (location= result value)
		  (inst ,inst result value))))))))
  (frob double double movsd)
  (frob single single movss)
  ;; Not really right since these aren't long floats
  (frob long   double movsd))
