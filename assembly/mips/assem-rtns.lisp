;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/mips/assem-rtns.lisp,v 1.1 1990/03/05 21:13:52 wlott Exp $
;;;
;;;
(in-package "C")



;;;; The undefined-function.

;;; Just signal an undefined-symbol error.  Note: this must look like a
;;; function, because it magically gets called in place of a function when
;;; there is no real function to call.

(define-assembly-routine (undefined-function (:arg name :sc any-reg
						   :offset cname-offset))
  ;; Allocate function header.
  (align vm:lowtag-bits)
  (inst word vm:function-header-type)
  (dotimes (i (1- vm:function-header-code-offset))
    (nop))
  ;; Cause the error.
  (error-call di:undefined-symbol-error name))



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

  (let ((error (generate-error-code 'unwind-error di:invalid-unwind-error)))
    (inst beq block zero-tn error))
  
  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst bne cur-uwp target-uwp do-uwp)
  (nop)
      
  (move cur-uwp block)

  do-exit
      
  (loadw cont-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
  (lisp-return lra lip)

  do-uwp

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (b do-exit)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block*))



(define-assembly-routine (throw (:arg target)
				(:arg start :sc any-reg :offset args-offset)
				(:arg count :sc any-reg :offset nargs-offset)
				(:temp catch :sc any-reg :type fixnum)
				(:temp tag :sc descriptor-reg)
				(:temp ndescr :sc non-descriptor-reg))

  (load-symbol-value catch lisp::*current-catch-block*)
    
  loop
    
  (let ((error (generate-error-code 'throw-error
				    di:unseen-throw-tag-error
				    target)))
    (inst beq catch zero-tn error)
    (nop))
    
  (loadw tag catch vm:catch-block-tag-slot)
  (inst beq tag target exit)
  (nop)
  (b loop)
  (loadw catch catch vm:catch-block-previous-catch-slot)
    
  exit

  (move target catch)
  (inst load-assembly-address ndescr 'unwind)
  (inst jr ndescr)
  (nop))

