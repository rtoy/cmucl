;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/codegen.lisp,v 1.15 1991/08/25 18:14:00 ram Exp $")
;;;
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

(export '(component-header-length sb-allocated-size current-nfp-tn
	  callee-nfp-tn callee-return-pc-tn *code-segment* *elsewhere*
	  trace-table-entry pack-trace-table))

;;;; Utilities used during code generation.

;;; Component-Header-Length   --  Interface
;;; 
(defun component-header-length (&optional (component *compile-component*))
  "Returns the number of bytes used by the code object header."
  (let* ((2comp (component-info component))
	 (constants (ir2-component-constants 2comp))
	 (num-consts (length constants)))
    (ash (logandc2 (1+ num-consts) 1) vm:word-shift)))

;;; SB-Allocated-Size  --  Interface
;;;
(defun sb-allocated-size (name)
  "The size of the Name'd SB in the currently compiled component.  Useful
  mainly for finding the size for allocating stack frames."
  (finite-sb-current-size (sb-or-lose name *backend*)))


;;; Current-NFP-TN  --  Interface
;;;
(defun current-nfp-tn (vop)
  "Return the TN that is used to hold the number stack frame-pointer in VOP's
  function.  Returns NIL if no number stack frame was allocated."
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (let ((block (ir2-block-block (vop-block vop))))
    (when (ir2-environment-number-stack-p
	   (environment-info
	    (block-environment block)))
      (ir2-component-nfp (component-info (block-component block)))))))

;;; CALLEE-NFP-TN  --  Interface
;;;
(defun callee-nfp-tn (2env)
  "Return the TN that is used to hold the number stack frame-pointer in the
  function designated by 2env.  Returns NIL if no number stack frame was
  allocated."
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (when (ir2-environment-number-stack-p 2env)
      (ir2-component-nfp (component-info *compile-component*)))))


;;; CALLEE-RETURN-PC-TN  --  Interface
;;;
(defun callee-return-pc-tn (2env)
  "Return the TN used for passing the return PC in a local call to the function
  designated by 2env."
  (ir2-environment-return-pc-pass 2env))


;;;; Generate-code and support routines.

(defvar *trace-table-info*)
(defvar *code-segment* nil)
(defvar *elsewhere* nil)

;;; Init-Assembler  --  Interface
;;; 
(defun init-assembler ()
  (setf *code-segment* (make-segment))
  (setf *elsewhere* (make-segment))
  (undefined-value))

(defvar *assembly-optimize* t
  "Set to NIL to inhibit assembly-level optimization.  For compiler debugging,
  rather than policy control.")

(defvar *assembly-check* nil
  "Set to T to enable lifetime consistency checking of the assembly code.")

;;; Generate-Code  --  Interface
;;;
(defun generate-code (component)
  (let ((prev-env nil)
	(*trace-table-info* nil))
    (do-ir2-blocks (block component)
      (let ((1block (ir2-block-block block)))
	(when (and (eq (block-info 1block) block)
		   (block-start 1block))
	  (assemble (*code-segment* nil)
	    (emit-label (block-label 1block)))
	  (let ((env (block-environment 1block)))
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
		      (template-name (vop-info vop)))))))
    
    (assemble (*code-segment* nil)
      (insert-segment *elsewhere*))
    (expand-pseudo-instructions *code-segment*)
    (when *assembly-check*
      (segment-check-registers *code-segment* *elsewhere*))
    
    (when (and (policy (lambda-bind
			(block-home-lambda
			 (block-next (component-head component))))
		       (or (>= speed cspeed) (>= space cspeed)))
	       *assembly-optimize*)
      (optimize-segment *code-segment*))
    (let ((length (finalize-segment *code-segment*)))
      (values length (nreverse *trace-table-info*)))))

(defun emit-label-elsewhere (label)
  (assemble (*elsewhere* nil)
    (emit-label label)))

(defun label-elsewhere-p (label)
  (<= (label-position *elsewhere*) (label-position label)))

(defun trace-table-entry (state)
  (let ((label (gen-label)))
    (emit-label label)
    (push (cons label state) *trace-table-info*))
  (undefined-value))

;;; COMPUTE-TRACE-TABLE -- interface.
;;;
;;; Convert the list of (label . state) entries into an ivector.
;;; 
(eval-when (compile load eval)
  (defconstant bits-per-state 3)
  (defconstant bits-per-entry 16)
  (defconstant bits-per-offset (- bits-per-entry bits-per-state))
  (defconstant max-offset (ash 1 bits-per-offset)))
;;;
(defun pack-trace-table (table)
  (declare (list table))
  (let ((last-posn 0)
	(last-state 0)
	(result (make-array (length table)
			    :element-type '(unsigned-byte #.bits-per-entry)))
	(index 0))
    (dolist (entry table)
      (let* ((posn (label-position (car entry)))
	     (state (cdr entry)))
	(flet ((push-entry (offset state)
		 (when (>= index (length result))
		   (setf result
			 (replace (make-array
				   (truncate (* (length result) 5) 4)
				   :element-type
				   '(unsigned-byte #.bits-per-entry))
				  result)))
		 (setf (aref result index)
		       (logior (ash offset bits-per-state)
			       state))
		 (incf index)))
	  (do ((offset (- posn last-posn) (- offset max-offset)))
	      ((< offset max-offset)
	       (push-entry offset state))
	    (push-entry 0 last-state)))
	(setf last-posn posn)
	(setf last-state state)))
    (if (eql (length result) index)
	result
	(subseq result 0 index))))
