;;; -*- Package: VM; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/utils.lisp,v 1.9 2004/05/24 23:18:37 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; Utility functions needed by the back end to generate code.
;;;
;;; Written by William Lott.
;;; 

(in-package "VM")

(export '(fixnumize static-symbol-p static-symbol-offset offset-static-symbol
	  static-function-offset))



;;;; Handy routine for making fixnums:

(defun fixnumize (num)
  "Make a fixnum out of NUM.  (i.e. shift by two bits if it will fit.)"
  (if (<= most-negative-fixnum num most-positive-fixnum)
      (ash num (1- vm:lowtag-bits))
      (error "~D is too big for a fixnum." num)))



;;;; Routines for dealing with static symbols.

(defun static-symbol-p (symbol)
  (or (null symbol)
      (and (member symbol static-symbols) t)))

(defun static-symbol-offset (symbol)
  "Returns the byte offset of the static symbol Symbol."
  (if symbol
      (let ((posn (position symbol static-symbols)))
	(unless posn (error "~S is not a static symbol." symbol))
	(+ (* posn (pad-data-block symbol-size))
	   (pad-data-block #+amd64 symbol-size
			   #-amd64 (1- symbol-size))
	   other-pointer-type
	   (- list-pointer-type)))
      0))

(defun offset-static-symbol (offset)
  "Given a byte offset, Offset, returns the appropriate static symbol."
  (if (zerop offset)
      nil
      (multiple-value-bind
	  (n rem)
	  (truncate (+ offset list-pointer-type (- other-pointer-type)
		       (- (pad-data-block #+amd64 symbol-size
					  #-amd64 (1- symbol-size))))
		    (pad-data-block symbol-size))
	(unless (and (zerop rem) (<= 0 n (1- (length static-symbols))))
	  (error "Byte offset, ~D, is not correct." offset))
	(elt static-symbols n))))

(defun static-function-offset (name)
  "Return the (byte) offset from NIL to the start of the fdefn object
   for the static function NAME."
  (let ((static-syms (length static-symbols))
	(static-function-index (position name static-functions)))
    (unless static-function-index
      (error "~S isn't a static function." name))
    (+ (* static-syms (pad-data-block symbol-size))
       (pad-data-block #+amd64 symbol-size
		       #-amd64 (1- symbol-size))
       (- list-pointer-type)
       (* static-function-index (pad-data-block fdefn-size))
       (* fdefn-raw-addr-slot word-bytes))))

(defun offset-static-function (offset)
  "Given a byte offset, Offset, returns the appropriate static function
   symbol."
  (let* ((static-syms (length static-symbols))
	 (offsets (+ (* static-syms (pad-data-block symbol-size))
		     (pad-data-block #+amd64 symbol-size
				     #-amd64 (1- symbol-size))
		     (- list-pointer-type)
		     (* fdefn-raw-addr-slot word-bytes))))
    (multiple-value-bind (index rmdr)
	(floor (- offset offsets) (pad-data-block fdefn-size))
      (unless (and (zerop rmdr)
		   (>= index 0)
		   (< index (length static-symbols)))
	(error "Byte offset, ~D, is not correct." offset))
      (elt static-functions index))))
