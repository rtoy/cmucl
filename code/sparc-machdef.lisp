;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sparc-machdef.lisp,v 1.2.1.1 1994/10/19 23:24:40 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package "MACH")

(export '(s-context-onstack s-context-mask s-context-sp s-context-pc
	  s-context-npc s-context-psr s-context-g1 s-context-o0
	  s-context-regs s-context-fpregs s-context-y s-context-fsr
	  s-context *s-context indirect-*s-context))

(def-c-record s-context
  (onstack unsigned-long)
  (mask unsigned-long)
  (sp system-area-pointer)
  (pc system-area-pointer)
  (npc system-area-pointer)
  (psr unsigned-long)
  (g1 unsigned-long)
  (o0 unsigned-long)
  (regs int-array)
  (fpregs int-array)
  (y unsigned-long)
  (fsr unsigned-long))
