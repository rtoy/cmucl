;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/rt-machdef.lisp,v 1.3.1.1 1994/10/19 23:23:26 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package "MACH")

(export '(s-context-onstack s-context-mask s-context-sp s-context-fp
	  s-context-ap s-context-iar s-context-icscs s-context-saveiar
	  s-context-regs s-context *s-context indirect-*s-context
	  s-context-pc))

(def-c-record s-context
  (onstack unsigned-long)
  (mask unsigned-long)
  (floatsave system-area-pointer)
  (sp system-area-pointer)
  (fp system-area-pointer)
  (ap system-area-pointer)
  (iar system-area-pointer)
  (icscs unsigned-long)
  (saveiar system-area-pointer)
  (regs int-array))

(defoperator (s-context-pc system-area-pointer) ((x s-context))
  `(s-context-iar (alien-value ,x)))
