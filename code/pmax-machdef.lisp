;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pmax-machdef.lisp,v 1.2.1.1 1994/10/19 23:22:57 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package "MACH")


(export '(s-context-onstack s-context-mask s-context-pc s-context-regs
	  s-context-mdlo s-context-mdhi s-context-ownedfp s-context-fpregs
	  s-context-fpc_csr s-context-fpc_eir s-context-cause
	  s-context-badvaddr s-context-badpaddr s-context *s-context
	  indirect-*s-context))


(def-c-record s-context
  (onstack unsigned-long)
  (mask unsigned-long)
  (pc system-area-pointer)
  (regs int-array)
  (mdlo unsigned-long)
  (mdhi unsigned-long)
  (ownedfp unsigned-long)
  (fpregs int-array)
  (fpc_csr unsigned-long)
  (fpc_eir unsigned-long)
  (cause unsigned-long)
  (badvaddr system-area-pointer)
  (badpaddr system-area-pointer))
