;;; -*- Log: code.log; Package: Mach -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Record definitions needed for the interface to Mach.
;;;
(in-package "MACH")

(export '(sigcontext-onstack sigcontext-mask sigcontext-sp sigcontext-pc
	  sigcontext-npc sigcontext-psr sigcontext-g1 sigcontext-o0
	  sigcontext-regs sigcontext-fpregs sigcontext-y sigcontext-fsr
	  sigcontext *sigcontext indirect-*sigcontext))

(def-c-record sigcontext
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
