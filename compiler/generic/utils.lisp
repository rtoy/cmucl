;;; -*- Package: VM; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/utils.lisp,v 1.1 1990/11/03 03:17:32 wlott Exp $
;;;
;;; Utility functions needed by the back end to generate code.
;;;
;;; Written by William Lott.
;;; 

(in-package "VM")

(export '(fixnum static-symbol-p static-symbol-offset offset-static-symbol))



;;;; Handy routine for making fixnums:

(defun fixnum (num)
  "Make a fixnum out of NUM.  (i.e. shift by two bits if it will fit.)"
  (if (<= #x-20000000 num #x1fffffff)
      (ash num 2)
      (error "~D is too big for a fixnum." num)))



;;;; Routines for dealing with static symbols.

(defun static-symbol-p (symbol)
  (member symbol static-symbols))

(defun static-symbol-offset (symbol)
  "Returns the byte offset of the static symbol Symbol."
  (let ((posn (position symbol static-symbols)))
    (unless posn (error "~S is not a static symbol." symbol))
    (+ (* posn (pad-data-block symbol-size))
       (pad-data-block (1- symbol-size))
       other-pointer-type
       (- list-pointer-type))))

(defun offset-static-symbol (offset)
  "Given a byte offset, Offset, returns the appropriate static symbol."
  (multiple-value-bind
      (n rem)
      (truncate (+ offset list-pointer-type (- other-pointer-type)
		   (- (pad-data-block (1- symbol-size))))
		(pad-data-block symbol-size))
    (unless (and (zerop rem) (<= 0 n (1- (length static-symbols))))
      (error "Byte offset, ~D, is not correct." offset))
    (elt static-symbols n)))

