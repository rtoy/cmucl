;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/static-fn.lisp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: src/compiler/arm64/static-fn.lisp $
;;;
;;; This file contains the VOPs and macro magic necessary to call static
;;; functions on ARM64.
;;;
;;; Written by William Lott.
;;; Ported to ARM64 from the SPARC implementation.
;;;
(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")



(define-vop (static-function-template)
  (:save-p t)
  (:policy :safe)
  (:variant-vars symbol)
  (:vop-var vop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:sc descriptor-reg :offset lr-offset) lra)
  (:temporary (:scs (descriptor-reg)) func)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) old-fp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save))


(eval-when (compile load eval)


(defun static-function-template-name (num-args num-results)
  (intern (format nil "~:@(~R-arg-~R-result-static-function~)"
		  num-args num-results)))


(defun moves (dst src)
  (collect ((moves))
    (do ((dst dst (cdr dst))
	 (src src (cdr src)))
	((or (null dst) (null src)))
      (moves `(move ,(car dst) ,(car src))))
    (moves)))

(defun static-function-template-vop (num-args num-results)
  (assert (and (<= num-args register-arg-count)
	       (<= num-results register-arg-count))
	  (num-args num-results)
	  (intl:gettext "Either too many args (~D) or too many results (~D).  Max = ~D")
	  num-args num-results register-arg-count)
  (let ((num-temps (max num-args num-results)))
    (collect ((temp-names) (temps) (arg-names) (args) (result-names) (results))
      (dotimes (i num-results)
	(let ((result-name (intern (format nil "RESULT-~D" i))))
	  (result-names result-name)
	  (results `(,result-name :scs (any-reg descriptor-reg)))))
      (dotimes (i num-temps)
	(let ((temp-name (intern (format nil "TEMP-~D" i))))
	  (temp-names temp-name)
	  (temps `(:temporary (:sc descriptor-reg
			       :offset ,(nth i register-arg-offsets)
			       ,@(when (< i num-args)
				   `(:from (:argument ,i)))
			       ,@(when (< i num-results)
				   `(:to (:result ,i)
				     :target ,(nth i (result-names)))))
			      ,temp-name))))
      (dotimes (i num-args)
	(let ((arg-name (intern (format nil "ARG-~D" i))))
	  (arg-names arg-name)
	  (args `(,arg-name
		  :scs (any-reg descriptor-reg)
		  :target ,(nth i (temp-names))))))
      `(define-vop (,(static-function-template-name num-args num-results)
		    static-function-template)
	 (:args ,@(args))
	 ,@(temps)
	 (:results ,@(results))
	 (:generator ,(+ 50 num-args num-results)
	   (emit-not-implemented)
	   (let ((lra-label (gen-label))
		 (cur-nfp (current-nfp-tn vop)))
	     ,@(moves (temp-names) (arg-names))
	     ;; Load the function object from the symbol's static-function slot.
	     ;; (mem ...) uses the unsigned imm12 encoding: offset must be a
	     ;; non-negative multiple of 8 (the 64-bit access size).
	     ;; static-function-offset returns a byte offset into the nil vector;
	     ;; static function slots are word-aligned, so this is always satisfied.
	     ;; Max encodable byte offset is 4095*8 = 32760, well above any
	     ;; realistic static-function table size.
	     (inst ldr func (mem null-tn (static-function-offset symbol)))
	     ;; Set nargs to the fixnumized argument count.
	     (inst mov nargs (fixnumize ,num-args))
	     ;; Save the non-descriptor frame pointer if present.
	     (when cur-nfp
	       (store-stack-tn nfp-save cur-nfp))
	     ;; old-fp <- current frame pointer (cfp-tn).
	     (inst mov old-fp cfp-tn)
	     ;; Allocate a new frame: cfp <- csp (stack grows down on ARM64,
	     ;; but the Lisp frame convention is the same as SPARC here).
	     (inst mov cfp-tn csp-tn)
	     ;; Compute the LRA (return address) as a tagged pointer into
	     ;; the current code object, placing it in the LRA register.
	     ;; ARM64 uses ADR/ADRP + tag arithmetic instead of SPARC's
	     ;; compute-lra-from-code pseudo-op, but the VOP-level interface
	     ;; is identical.
	     (inst compute-lra-from-code lra code-tn lra-label temp)
	     (note-this-location vop :call-site)
	     ;; Jump to the function entry point.
	     ;; On ARM64 the raw entry point is at:
	     ;;   func + (function-code-offset * word-size) - function-pointer-tag
	     ;; We use BR (branch-to-register, no link) because the return
	     ;; address is already stashed in lra above.
	     (inst ldr temp (mem func (- (ash function-code-offset word-shift)
				       function-pointer-type)))
	     (inst br temp)
	     ;; Move the called function into the code register for GC
	     ;; (ARM64 equivalent of SPARC's "move code-tn func" in delay slot).
	     (inst mov code-tn func)
	     (emit-return-pc lra-label)
	     ,(collect ((bindings) (links))
		(do ((temp (temp-names) (cdr temp))
		     (name 'values (gensym))
		     (prev nil name)
		     (i 0 (1+ i)))
		    ((= i num-results))
		  (bindings `(,name
			      (make-tn-ref ,(car temp) nil)))
		  (when prev
		    (links `(setf (tn-ref-across ,prev) ,name))))
		`(let ,(bindings)
		   ,@(links)
		   (default-unknown-values vop
		       ,(if (zerop num-results) nil 'values)
		       ,num-results move-temp temp lra-label)))
	     (when cur-nfp
	       (load-stack-tn cur-nfp nfp-save))
	     ,@(moves (result-names) (temp-names))))))))


) ; eval-when (compile load eval)


(macrolet ((frob (num-args num-res)
	     (static-function-template-vop (eval num-args) (eval num-res))))
  (frob 0 1)
  (frob 1 1)
  (frob 2 1)
  (frob 3 1)
  (frob 4 1))


(defmacro define-static-function (name args &key (results '(x)) translate
				       policy cost arg-types result-types)
  `(define-vop (,name
		,(static-function-template-name (length args)
						(length results)))
     (:variant ',name)
     (:note ,(format nil "static-function ~@(~S~)" name))
     ,@(when translate
	 `((:translate ,translate)))
     ,@(when policy
	 `((:policy ,policy)))
     ,@(when cost
	 `((:generator-cost ,cost)))
     ,@(when arg-types
	 `((:arg-types ,@arg-types)))
     ,@(when result-types
	 `((:result-types ,@result-types)))))
