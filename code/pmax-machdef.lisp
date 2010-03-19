;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pmax-machdef.lisp,v 1.4 2010/03/19 15:18:59 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package "MACH")
(intl:textdomain "cmucl")


(export '(sigcontext-onstack sigcontext-mask sigcontext-pc sigcontext-regs
	  sigcontext-mdlo sigcontext-mdhi sigcontext-ownedfp sigcontext-fpregs
	  sigcontext-fpc_csr sigcontext-fpc_eir sigcontext-cause
	  sigcontext-badvaddr sigcontext-badpaddr sigcontext *sigcontext
	  indirect-*sigcontext))


(def-c-record sigcontext
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
