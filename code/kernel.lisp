;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/kernel.lisp,v 1.3 1990/11/05 18:27:45 wlott Exp $
;;;    
(in-package "KERNEL")


(defun get-header-data (x)
  "Return the 24 bits of data in the header of object X, which must be an
  other-pointer object."
  (get-header-data x))

(defun set-header-data (x val)
  "Sets the 24 bits of data in the header of object X (which must be an
  other-pointer object) to VAL."
  (set-header-data x val))

(defun get-closure-length (x)
  "Returns the length of the closure X.  This is one more than the number
  of variables closed over."
  (get-closure-length x))

(defun get-lowtag (x)
  "Returns the three-bit lowtag for the object X."
  (get-lowtag x))

(defun get-type (x)
  "Returns the 8-bit header type for the object X."
  (get-type x))

(defun vector-sap (x)
  "Return a System-Area-Pointer pointing to the data for the vector X, which
  must be simple."
  (declare (type (simple-unboxed-array (*)) x))
  (vector-sap x))


(defun c::binding-stack-pointer-sap ()
  "Return a System-Area-Pointer pointing to the end of the binding stack."
  (c::binding-stack-pointer-sap))

(defun c::dynamic-space-free-pointer ()
  "Returns a System-Area-Pointer pointing to the next free work of the current
  dynamic space."
  (c::dynamic-space-free-pointer))

(defun c::control-stack-pointer-sap ()
  "Return a System-Area-Pointer pointing to the end of the control stack."
  (c::control-stack-pointer-sap))

(defun %function-header-arglist (func)
  "Extracts the arglist from the function header FUNC."
  (%function-header-arglist func))

(defun %function-header-name (func)
  "Extracts the name from the function header FUNC."
  (%function-header-name func))

(defun %function-header-type (func)
  "Extracts the type from the function header FUNC."
  (%function-header-type func))

(defun %closure-function (closure)
  "Extracts the function from CLOSURE."
  (%closure-function closure))

(defun c::vector-length (vector)
  "Return the length of VECTOR.  There is no reason to use this, 'cause
  (length (the vector foo)) is the same."
  (c::vector-length vector))

(defun %sxhash-simple-string (string)
  "Return the SXHASH for the simple-string STRING."
  (%sxhash-simple-string string))

(defun %sxhash-simple-substrubg (string length)
  "Return the SXHASH for the first LENGTH characters of the simple-string
  STRING."
  (%sxhash-simple-substring string length))

(defun %closure-index-ref (closure index)
  "Extract the INDEXth slot from CLOSURE."
  (%closure-index-ref closure index))
