;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/static-fn.lisp,v 1.2 1990/03/29 16:29:12 wlott Exp $
;;;
;;; This file contains the VOPs and macro magic necessary to call static
;;; functions.
;;;
;;; Written by William Lott.
;;;
(in-package "C")



(define-vop (static-function-template)
  (:save-p t)
  (:policy :fast-safe)
  (:variant-vars symbol)
  (:node-var node)
  (:temporary (:scs (descriptor-reg any-reg)) temp)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:sc descriptor-reg :offset lra-offset) lra)
  (:temporary (:sc descriptor-reg :offset cname-offset) cname)
  (:temporary (:sc descriptor-reg :offset lexenv-offset) lexenv)
  (:temporary (:scs (descriptor-reg)) function)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:temporary (:sc any-reg :offset nargs-offset) nargs))


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
	  "Either too many args (~D) or too many results (~D).  Max = ~D"
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
	   (let ((lra-label (gen-label)))
	     ,@(moves (temp-names) (arg-names))
	     (loadi nargs (fixnum ,num-args))
	     (load-symbol cname symbol)
	     (loadw lexenv cname vm:symbol-function-slot vm:other-pointer-type)
	     (inst compute-lra-from-code lra code-tn lra-label)
	     (loadw function lexenv vm:closure-function-slot
		    vm:function-pointer-type)
	     (lisp-jump function lip)
	     (emit-return-pc lra-label)
	     (unassemble
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
		    (default-unknown-values node
					    ,(if (zerop num-results)
						 nil
						 'values)
					    ,num-results
					    move-temp temp lra-label))))
	     ,@(moves (result-names) (temp-names))))))))


) ; eval-when (compile load eval)


(expand
 (collect ((templates '(progn)))
   (dotimes (i register-arg-count)
     (templates (static-function-template-vop i 1)))
   (templates)))


(defmacro define-static-function (name args &key (results '(x)) translate
				       policy cost arg-types result-types)
  `(define-vop (,name
		,(static-function-template-name (length args)
						(length results)))
     (:variant ',name)
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
