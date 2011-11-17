;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/rt-machdef.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package "MACH")
(intl:textdomain "cmucl")

(export '(sigcontext-onstack sigcontext-mask sigcontext-sp sigcontext-fp
	  sigcontext-ap sigcontext-iar sigcontext-icscs sigcontext-saveiar
	  sigcontext-regs sigcontext *sigcontext indirect-*sigcontext
	  sigcontext-pc))

(def-c-record sigcontext
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

(defoperator (sigcontext-pc system-area-pointer) ((x sigcontext))
  `(sigcontext-iar (alien-value ,x)))
