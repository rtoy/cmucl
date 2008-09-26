;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/sse2-c-call.lisp,v 1.1.2.1 2008/09/26 18:56:41 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Debugging and Enhancements by Douglas Crosher 1996,1997,1998,1999.
;;;

(in-package :x86)
(use-package :alien)
(use-package :alien-internals)

(define-vop (call-out)
  (:args (function :scs (sap-reg))
	 (args :more t))
  (:results (results :more t))
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from :eval :to :result) eax)
  (:temporary (:sc unsigned-reg :offset ecx-offset
		   :from :eval :to :result) ecx)
  (:temporary (:sc unsigned-reg :offset edx-offset
		   :from :eval :to :result) edx)
  (:temporary (:sc double-stack) temp)
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args ecx edx)
  (:guard (backend-featurep :sse2))
  (:generator 0 
    (cond ((or (policy node (> space speed)))
	   (move eax function)
	   (inst call (make-fixup (extern-alien-name "call_into_c") :foreign)))
	  (t
	   ;; Setup the NPX for C; all the FP registers need to be
	   ;; empty; pop them all.
	   (dotimes (i 8)
	     (fp-pop))

	   (inst call function)
	   ;; To give the debugger a clue. XX not really internal-error?
	   (note-this-location vop :internal-error)

	   ;; Restore the NPX for lisp; insure no regs are empty.
	   (dotimes (i 7)
	     (inst fldz))

	   #-sse2
	   (if (and results
		    (location= (tn-ref-tn results) fr0-tn))
	       ;; The return result is in fr0.
	       (inst fxch fr7-tn) ; move the result back to fr0
	       (inst fldz)) ; insure no regs are empty
	   #+sse2
	   (cond ((and results
		       (location= (tn-ref-tn results) fr0-tn))
		  ;; Result is in fr0
		  (inst fxch fr7-tn) ; move the result back to fr0
		  (inst fstd (ea-for-df-stack temp))
		  (inst movsd fr0-tn (ea-for-df-stack temp)))
		 (t
		  (inst fldz)))
		    
	   ))))
