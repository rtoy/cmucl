;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-fndb.lisp,v 1.1 1990/03/27 21:01:52 wlott Exp $
;;;
;;; This file defines the machine specific function signatures.
;;;
;;; Written by William Lott.
;;;
(in-package "C")


;;;; Internal type predicates:
;;;
;;;    Simple typep uses that don't have any standard predicate are translated
;;; into non-standard unary predicates.

(defknown (fixnump bignump ratiop short-float-p single-float-p double-float-p
	   long-float-p base-char-p %string-char-p %standard-char-p structurep
	   array-header-p simple-array-p system-area-pointer-p realp)
  (t) boolean (movable foldable flushable))


;;;; Miscellaneous "sub-primitives":

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))
