;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-macs.lisp,v 1.7 1992/05/30 17:39:27 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-macs.lisp,v 1.7 1992/05/30 17:39:27 wlott Exp $
;;;
;;;    This file contains some macros and constants that are object-format
;;; specific or are used for defining the object format.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 
(in-package "VM")

(export '(*assembly-unit-length*))



;;;; Other random stuff.

;;; PAD-DATA-BLOCK -- Internal Interface.
;;;
;;; This returns a form that returns a dual-word aligned number of bytes when
;;; given a number of words.
;;;
(defmacro pad-data-block (words)
  `(logandc2 (+ (ash ,words word-shift) lowtag-mask) lowtag-mask))

;;; DEFENUM -- Internal Interface.
;;;
(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
		   &rest identifiers)
  (let ((results nil)
	(index 0)
	(start (eval start))
	(step (eval step)))
    (dolist (id identifiers)
      (when id
	(multiple-value-bind
	    (root docs)
	    (if (consp id)
		(values (car id) (cdr id))
		(values id nil))
	  (push `(defconstant ,(intern (concatenate 'simple-string
						    (string prefix)
						    (string root)
						    (string suffix)))
		   ,(+ start (* step index))
		   ,@docs)
		results)))
      (incf index))
    `(eval-when (compile load eval)
       ,@(nreverse results))))



;;;; Some general constant definitions:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

(in-package "C")

(export '(fasl-file-implementations
	  pmax-fasl-file-implementation
	  sparc-fasl-file-implementation
	  rt-fasl-file-implementation
	  rt-afpa-fasl-file-implementation
	  x86-fasl-file-implementation
	  hppa-fasl-file-implementation))

;;; Constants for the different implementations.  These are all defined in
;;; one place to make sure they are all unique.

(defparameter fasl-file-implementations
  '(nil "Pmax" "Sparc" "RT" "RT/AFPA" "x86" "HPPA"))
(defconstant pmax-fasl-file-implementation 1)
(defconstant sparc-fasl-file-implementation 2)
(defconstant rt-fasl-file-implementation 3)
(defconstant rt-afpa-fasl-file-implementation 4)
(defconstant x86-fasl-file-implementation 5)
(defconstant hppa-fasl-file-implementation 6)

;;; The maximum number of SCs in any implementation.
(defconstant sc-number-limit 32)
