;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Bit-vector hacking utilities, potentially implementation-dependent for
;;; speed.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(declaim (inline clear-bit-vector set-bit-vector bit-vector-replace
		 bit-vector-copy))

;;; Clear-Bit-Vector  --  Interface
;;;
;;;    Clear a bit-vector to zeros.
;;;
(defun clear-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (bit-xor vec vec t))


;;; Set-Bit-Vector  --  Interface
;;;
;;;    Fill a bit vector with ones.
;;;
(defun set-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (bit-orc2 vec vec t))


;;; Bit-Vector-Replace  --  Interface
;;;
;;;    Replace the bits in To with the bits in From.
;;;
(defun bit-vector-replace (to from)
  (declare (type simple-bit-vector to from))
  (bit-ior from from to))


;;; Bit-Vector-Copy  --  Interface
;;;
;;;    Copy a bit-vector.
;;;
(defun bit-vector-copy (vec)
  (declare (type simple-bit-vector vec))
  (bit-ior vec vec (make-array (length vec) :element-type 'bit)))
