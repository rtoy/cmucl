;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-typetran.lisp,v 1.2 1990/03/27 21:19:56 wlott Exp $
;;;
;;; This file contains the implimentation specific type transformation magic.
;;; Basically, the various non-standard predicates that can be used in typep
;;; transformations.
;;; 
;;; Written by William Lott.
;;;

(in-package "C")


;;;; Internal predicates:
;;;
;;;    These type predicates are used to implement simple cases of typep.  They
;;; shouldn't be used explicitly.

(define-type-predicate base-char-p base-character)
(define-type-predicate bignump bignum)
(define-type-predicate double-float-p double-float)
(define-type-predicate fixnump fixnum)
(define-type-predicate long-float-p long-float)
(define-type-predicate ratiop ratio)
(define-type-predicate short-float-p short-float)
(define-type-predicate single-float-p single-float)
(define-type-predicate simple-array-p simple-array)
(define-type-predicate system-area-pointer-p system-area-pointer)

;;; Unlike the un-%'ed versions, these are true type predicates, accepting any
;;; type object.
;;;
(define-type-predicate %string-char-p string-char)
(define-type-predicate %standard-char-p standard-char)

