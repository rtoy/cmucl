;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/sparc/support.lisp,v 1.3 1992/03/06 11:03:58 wlott Exp $
;;;
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;
(in-package "SPARC")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (let ((temp (make-symbol "TEMP"))
	   (lip (make-symbol "LIP")))
       (values 
	`((inst jali ,lip ,temp (make-fixup ',name :assembly-routine))
	  (inst nop))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  (:temporary (:scs (interior-reg) :from (:eval 0) :to (:eval 1))
		      ,lip)))))
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
	    (inst ji ,temp (make-fixup ',name :assembly-routine))
	    (inst nop)
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :unknown-return)
	    (move csp-tn ocfp-tn)
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
     (let ((temp (make-symbol "TEMP")))
       (values 
	`((inst ji ,temp (make-fixup ',name :assembly-routine))
	  (inst nop))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)))))))

(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst j
	     (make-random-tn :kind :normal
			     :sc (sc-or-lose 'interior-reg *backend*)
			     :offset lip-offset)
	     8)
       (inst nop)))
    (:full-call
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose 'descriptor-reg *backend*)
				    :offset lra-offset)
		    :offset 2)))
    (:none)))
