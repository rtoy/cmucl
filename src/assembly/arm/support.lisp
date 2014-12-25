;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/arm/support.lisp $")
;;;
;;; **********************************************************************
;;;
(in-package "ARM")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     #+(or)
     (let ((temp (make-symbol "TEMP"))
	   (lip (make-symbol "LIP")))
       (values 
	`((inst jali ,lip ,temp (make-fixup ',name :assembly-routine))
	  (inst nop))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  (:temporary (:scs (interior-reg) :from (:eval 0) :to (:eval 1))
		      ,lip))))
     `((emit-not-implemented)))
    (:full-call
     #+(or)
     (let ((temp (make-symbol "TEMP"))
	   (nfp-save (make-symbol "NFP-SAVE"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn ,nfp-save cur-nfp))
	    (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
	    (note-next-instruction ,vop :call-site)
	    (inst ji ,temp (make-fixup ',name :assembly-routine))
	    (inst nop)
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :single-value-return)
	    (without-scheduling ()
	      (move csp-tn ocfp-tn)
	      (inst nop))
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
		      ,nfp-save)
	  (:save-p :compute-only))))
     `((emit-not-implemented)))
    (:none
     #+(or)
     (let ((temp (make-symbol "TEMP")))
       (values 
	`((inst ji ,temp (make-fixup ',name :assembly-routine))
	  (inst nop))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp))))
     `((emit-not-implemented)))))

(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     #+(or)
     `((inst j
	     (make-random-tn :kind :normal
			     :sc (sc-or-lose 'interior-reg *backend*)
			     :offset lip-offset)
	     8)
       (inst nop))
     `((emit-not-implemented)))
    (:full-call
     #+(or)
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose 'descriptor-reg *backend*)
				    :offset lra-offset)
		    :offset 2))
     `((emit-not-implemented)))
    (:none)))
