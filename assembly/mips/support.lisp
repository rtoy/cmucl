;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/support.lisp,v 1.4 1990/11/03 17:14:59 wlott Exp $
;;;
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;
(in-package "MIPS")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values
      `((inst jal (make-fixup ',name :assembly-routine))
	(inst nop))
      nil))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
	   (nfp-save (make-symbol "NFP-SAVE"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn ,nfp-save cur-nfp))
	    (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
	    (inst j (make-fixup ',name :assembly-routine))
	    (inst nop)
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :unknown-return)
	    (move csp-tn old-fp-tn)
	    (inst nop)
	    (inst compute-code-from-lra code-tn code-tn
		  lra-label ,temp)
	    (when cur-nfp
	      (load-stack-tn cur-nfp ,nfp-save))))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  (:temporary (:sc descriptor-reg :offset lra-offset
			   :from (:eval 0) :to (:eval 1))
		      ,lra)
	  (:temporary (:scs (control-stack) :offset nfp-save-offset)
		      ,nfp-save)))))
    (:none
     (values
      `((inst j (make-fixup ',name :assembly-routine))
	(inst nop))
      nil))))


(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst j lip-tn)
       (inst nop)))
    (:full-call
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose
					 'descriptor-reg)
				    :offset lra-offset)
		    lip-tn :offset 2)))
    (:none)))
