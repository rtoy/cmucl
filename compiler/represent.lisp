;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the implementation independent code for the
;;; representation selection phase in the compiler.  Representation selection
;;; decides whether to use non-descriptor representations for objects and emits
;;; the appropriate representation-specific move and coerce vops.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;; REPRESENTATION-SELECT-1-BLOCK  --  Internal
;;;
(defun representation-select-1-block (block)
  (declare (type ir2-block block))
  )

;;; SELECT-TN-REPRESENTATION  --  Internal
;;;
;;;    Select the best representation for a normal TN.
;;;
(defun select-tn-representation (tn)
