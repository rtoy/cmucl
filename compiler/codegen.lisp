;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    The implementation-independent parts of the code generator.  We use
;;; functions and information provided by the VM definition to convert IR2 into
;;; assembly code.  After emitting code, we finish the assembly and then do the
;;; post-assembly phase.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;;; Utilities used during code generation.

;;; Current-Frame-Size  --  Interface
;;;
;;;    The size of the currently compiled component's stack frame in bytes.
;;;
(defun current-frame-size ()
  (* 4 (finite-sb-current-size
	(sc-sb (svref *sc-numbers* (sc-number-or-lose 'stack))))))


;;; Generate-Code  --  Interface
;;;
(defun generate-code (component)
  (let ((prev-env nil))
    (do-ir2-blocks (block component)
      (let* ((1block (ir2-block-block block))
	     (lambda (block-lambda 1block)))
	(when (and (eq (block-info 1block) block) lambda)
	  (emit-label (block-label 1block))
	  (let ((env (lambda-environment lambda)))
	    (unless (eq env prev-env)
	      (let ((lab (gen-label)))
		(setf (ir2-environment-elsewhere-start (environment-info env))
		      lab)
		(emit-label-elsewhere lab))
	      (setq prev-env env)))))

      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	  ((null vop))
	(let ((gen (vop-info-generator-function (vop-info vop))))
	  (if gen 
	      (funcall gen vop)
	      (format t "Missing generator for ~S.~%"
		      (template-name (vop-info vop))))))))

  (finish-assembly))
