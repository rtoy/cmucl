;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-macs.lisp,v 1.2 1991/02/20 15:17:30 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/generic/vm-macs.lisp,v 1.2 1991/02/20 15:17:30 ram Exp $
;;;
;;;    This file contains some macros and constants that are object-format
;;; specific or are used for defining the object format.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 
(in-package "VM")

(defmacro pad-data-block (words)
  `(logandc2 (+ (ash ,words vm:word-shift) lowtag-mask) lowtag-mask))

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
	  rt-fasl-file-implementation))

;;; Constants for the different implementations.  These are all defined in
;;; one place to make sure they are all unique.

(defconstant fasl-file-implementations '(nil "Pmax" "Sparc" "RT"))
(defconstant pmax-fasl-file-implementation 1)
(defconstant sparc-fasl-file-implementation 2)
(defconstant rt-fasl-file-implementation 3)

;;; The maximum number of SCs in any implementation.
(defconstant sc-number-limit 32)
