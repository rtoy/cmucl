;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/kernel.lisp,v 1.1 1990/07/13 16:36:18 wlott Exp $
;;;    
(in-package "KERNEL")


(defconstant native-byte-order '#.target-byte-order
  "Either :BIG-ENDIAN (IBM-PC/RT) or :LITTLE-ENDIAN (PMAX).")

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
