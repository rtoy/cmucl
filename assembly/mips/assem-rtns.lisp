;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/assem-rtns.lisp,v 1.6 1990/04/26 22:25:27 wlott Exp $
;;;
;;;
(in-package "C")



;;;; The undefined-function.

;;; Just signal an undefined-symbol error.  Note: this must look like a
;;; function, because it magically gets called in place of a function when
;;; there is no real function to call.

(define-assembly-routine (undefined-function (:arg cname :sc any-reg
						   :offset cname-offset)
					     (:temp lexenv :sc any-reg
						    :offset lexenv-offset)
					     (:temp function :sc any-reg
						    :offset code-offset)
					     (:temp nargs :sc any-reg
						    :offset nargs-offset)
					     (:temp lip :sc interior-reg)
					     (:temp temp
						    :sc non-descriptor-reg))
  ;; Allocate function header.
  (align vm:lowtag-bits)
  (inst word vm:function-header-type)
  (dotimes (i (1- vm:function-header-code-offset))
    (inst word 0))
  ;; Cause the error.
  (cerror-call continue di:undefined-symbol-error cname)

  continue

  (let ((not-sym (generate-cerror-code di:object-not-symbol-error cname)))
    (test-simple-type cname temp not-sym t vm:symbol-header-type))

  (loadw lexenv cname vm:symbol-function-slot vm:other-pointer-type)
  (loadw function lexenv vm:closure-function-slot vm:function-pointer-type)
  (lisp-jump function lip))


;;;; Non-local exit noise.


(define-assembly-routine (unwind (:arg block)
				 (:arg start :sc any-reg :offset args-offset)
				 (:arg count :sc any-reg :offset nargs-offset)
				 (:temp lip :sc interior-reg :type interior)
				 (:temp lra :sc descriptor-reg)
				 (:temp cur-uwp :sc any-reg :type fixnum)
				 (:temp next-uwp :sc any-reg :type fixnum)
				 (:temp target-uwp :sc any-reg :type fixnum))
  (declare (ignore start count))

  (let ((error (generate-error-code di:invalid-unwind-error)))
    (inst beq block zero-tn error))
  
  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst bne cur-uwp target-uwp do-uwp)
  (inst nop)
      
  (move cur-uwp block)

  do-exit
      
  (loadw fp-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
  (lisp-return lra lip)

  do-uwp

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (inst b do-exit)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block*))



(define-assembly-routine (throw (:arg target)
				(:arg start :sc any-reg :offset args-offset)
				(:arg count :sc any-reg :offset nargs-offset)
				(:temp catch :sc any-reg :type fixnum)
				(:temp tag :sc descriptor-reg)
				(:temp ndescr :sc non-descriptor-reg))

  (load-symbol-value catch lisp::*current-catch-block*)
    
  loop
    
  (let ((error (generate-error-code di:unseen-throw-tag-error target)))
    (inst beq catch zero-tn error)
    (inst nop))
    
  (loadw tag catch vm:catch-block-tag-slot)
  (inst beq tag target exit)
  (inst nop)
  (inst b loop)
  (loadw catch catch vm:catch-block-previous-catch-slot)
    
  exit

  (move target catch)
  (inst li ndescr (make-fixup 'unwind :assembly-routine))
  (inst j ndescr)
  (inst nop))




;;;; Byte-blt

(define-assembly-routine (byte-blt (:arg src :sc sap-reg)
				   (:arg src-offset :sc any-reg)
				   (:arg dst :sc sap-reg)
				   (:arg dst-offset :sc any-reg)
				   (:arg dst-end :sc any-reg)
				   (:temp temp :sc non-descriptor-reg))

  ;; If there is nothing to do, don't do it.
  (inst beq dst-offset dst-end done)

  ;; Fix up the src and dst offsets.

  (inst sra temp src-offset 2)
  (inst addu src temp)
  (inst sra temp dst-offset 2)
  (inst addu dst temp)

  ;; The loop

  loop

  (inst lbu temp src)
  (inst addu src 1)
  (inst sb temp dst)
  (inst addu dst-offset (fixnum 1))
  (inst bne dst-offset dst-end loop)
  (inst addu dst 1)
  
  done

  )
