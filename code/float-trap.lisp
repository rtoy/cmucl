;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/float-trap.lisp,v 1.35.10.2 2010/02/09 18:41:59 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains stuff for controlling floating point traps.  It is
;;; IEEE float specific, but should work for pretty much any FPU where the
;;; state fits in one word and exceptions are represented by bits being set.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "VM")
(intl:textdomain "cmucl")

(export '(current-float-trap floating-point-modes sigfpe-handler))
(in-package "EXTENSIONS")
(export '(set-floating-point-modes get-floating-point-modes
	  with-float-traps-masked))
(in-package "VM")

(eval-when (compile load eval)

(defconstant float-trap-alist
  (list (cons :underflow float-underflow-trap-bit)
	(cons :overflow float-overflow-trap-bit)
	(cons :inexact float-inexact-trap-bit)
	(cons :invalid float-invalid-trap-bit)
	(cons :divide-by-zero float-divide-by-zero-trap-bit)
	#+x86 (cons :denormalized-operand float-denormal-trap-bit)))

;;; FLOAT-TRAP-MASK  --  Internal
;;;
;;;    Return a mask with all the specified float trap bits set.
;;;
(defun float-trap-mask (names)
  (reduce #'logior
	  (mapcar #'(lambda (x)
		      (or (cdr (assoc x float-trap-alist))
			  (error _"Unknown float trap kind: ~S." x)))
		  names)))

(defconstant rounding-mode-alist
  (list (cons :nearest float-round-to-nearest)
	(cons :zero float-round-to-zero)
	(cons :positive-infinity float-round-to-positive)
	(cons :negative-infinity float-round-to-negative)))
  
); Eval-When (Compile Load Eval)


;;; Interpreter stubs.
;;;
#+(not x86)
(progn
(defun floating-point-modes () (floating-point-modes))
(defun (setf floating-point-modes) (new) (setf (floating-point-modes) new))
)

#+(and x86 (not sse2))
(progn
  (defun floating-point-modes ()
    (let ((x87-modes (vm::x87-floating-point-modes)))
      ;; Massage the bits from x87-floating-point-modes into the order
      ;; that the rest of the system wants them to be.  (Must match
      ;; format in the SSE2 mxcsr register.)
      (logior (ash (logand #x3f x87-modes) 7) ; control
	      (logand #x3f (ash x87-modes -16)))))
  (defun (setf floating-point-modes) (new)
    (let* ((rc (ldb float-rounding-mode new))
	   (x87-modes
	    (logior (ash (logand #x3f new) 16)
		    (ash rc 10)
		    (logand #x3f (ash new -7))
		    ;; Set precision control to be 53-bit, always.
		    ;; (The compiler takes care of handling
		    ;; single-float precision, and we don't support
		    ;; long-floats.)
		    (ash 2 8))))
    (setf (x87-floating-point-modes) x87-modes)))
  )

#+sse2
(progn
  (defun floating-point-modes ()
    ;; Combine the modes from the FPU and SSE2 units.  Since the sse
    ;; mode contains all of the common information we want, we massage
    ;; the x87-modes to match, and then OR the x87 and sse2 modes
    ;; together.  Note: We ignore the rounding control bits from the
    ;; FPU and only use the SSE2 rounding control bits.
    (let* ((x87-modes (vm::x87-floating-point-modes))
	   (sse-modes (vm::sse2-floating-point-modes))
	   (final-mode (logior sse-modes
			       (ash (logand #x3f x87-modes) 7) ; control
			       (logand #x3f (ash x87-modes -16)))))

      final-mode))
  (defun (setf floating-point-modes) (new-mode)
    (declare (type (unsigned-byte 24) new-mode))
    ;; Set the floating point modes for both X87 and SSE2.  This
    ;; include the rounding control bits.
    (let* ((rc (ldb float-rounding-mode new-mode))
	   (x87-modes
	    (logior (ash (logand #x3f new-mode) 16)
		    (ash rc 10)
		    (logand #x3f (ash new-mode -7))
		    ;; Set precision control to be 64-bit, always.  We
		    ;; don't use the x87 registers with sse2, so this
		    ;; is ok and would be the correct setting if we
		    ;; ever support long-floats.
		    (ash 3 8))))
      (setf (vm::sse2-floating-point-modes) new-mode)
      (setf (vm::x87-floating-point-modes) x87-modes))
    new-mode)
)

;;; SET-FLOATING-POINT-MODES  --  Public
;;;
(defun set-floating-point-modes (&key (traps nil traps-p)
				      (rounding-mode nil round-p)
				      (current-exceptions nil current-x-p)
				      (accrued-exceptions nil accrued-x-p)
				      (fast-mode nil fast-mode-p))
  _N"This function sets options controlling the floating-point hardware.  If a
  keyword is not supplied, then the current value is preserved.  Possible
  keywords:

   :TRAPS
       A list of the exception conditions that should cause traps.  Possible
       exceptions are :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID,
       :DIVIDE-BY-ZERO, and on the X86 :DENORMALIZED-OPERAND. Initially
       all traps except :INEXACT are enabled.

   :ROUNDING-MODE
       The rounding mode to use when the result is not exact.  Possible values
       are :NEAREST, :POSITIVE-INFINITY, :NEGATIVE-INFINITY and :ZERO.
       Initially, the rounding mode is :NEAREST.

   :CURRENT-EXCEPTIONS
   :ACCRUED-EXCEPTIONS
       These arguments allow setting of the exception flags.  The main use is
       setting the accrued exceptions to NIL to clear them.

   :FAST-MODE
       Set the hardware's \"fast mode\" flag, if any.  When set, IEEE
       conformance or debuggability may be impaired.  Some machines may not
       have this feature, in which case the value is always NIL.

   GET-FLOATING-POINT-MODES may be used to find the floating point modes
   currently in effect."
  (let ((modes (floating-point-modes)))
    (when traps-p
      (setf (ldb float-traps-byte modes) (float-trap-mask traps)))
    (when round-p
      (setf (ldb float-rounding-mode modes)
	    (or (cdr (assoc rounding-mode rounding-mode-alist))
		(error _"Unknown rounding mode: ~S." rounding-mode))))
    (when current-x-p
      (setf (ldb float-exceptions-byte modes)
	    (float-trap-mask current-exceptions))
      #+(and darwin ppc)
      (when (member :invalid current-exceptions)
 	;; Clear out the bits for the detected invalid operation
 	(setf (ldb vm:float-invalid-op-1-byte modes) 0)))

    (when accrued-x-p
      (setf (ldb float-sticky-bits modes)
	    (float-trap-mask accrued-exceptions))
      #+(and darwin ppc)
      (when (member :invalid current-exceptions)
 	;; Clear out the bits for the detected invalid operation
 	(setf (ldb vm:float-invalid-op-1-byte modes) 0)))
    (when fast-mode-p
      (if fast-mode
	  (setq modes (logior float-fast-bit modes))
	  (setq modes (logand (lognot float-fast-bit) modes))))
    (setf (floating-point-modes) modes))
    
  (values))


;;; GET-FLOATING-POINT-MODES  --  Public
;;;
(defun get-floating-point-modes ()
  _N"This function returns a list representing the state of the floating point
  modes.  The list is in the same format as the keyword arguments to
  SET-FLOATING-POINT-MODES, i.e. 
      (apply #'set-floating-point-modes (get-floating-point-modes))

  sets the floating point modes to their current values (and thus is a no-op)."
  (flet ((exc-keys (bits)
	   (macrolet ((frob ()
			`(collect ((res))
			   ,@(mapcar #'(lambda (x)
					 `(when (logtest bits ,(cdr x))
					    (res ',(car x))))
				     float-trap-alist)
			   (res))))
	     (frob))))
    (let ((modes (floating-point-modes))) 
      `(:traps ,(exc-keys (ldb float-traps-byte modes))
	:rounding-mode ,(car (rassoc (ldb float-rounding-mode modes)
				     rounding-mode-alist))
	:current-exceptions ,(exc-keys (ldb float-exceptions-byte modes))
	:accrued-exceptions ,(exc-keys (ldb float-sticky-bits modes))
	:fast-mode ,(logtest float-fast-bit modes)))))

  
;;; CURRENT-FLOAT-TRAP  --  Interface
;;;
(defmacro current-float-trap (&rest traps)
  _N"Current-Float-Trap Trap-Name*
  Return true if any of the named traps are currently trapped, false
  otherwise."
  `(not (zerop (logand ,(dpb (float-trap-mask traps) float-traps-byte 0)
		       (floating-point-modes)))))


;;; SIGFPE-HANDLER  --  Interface
;;;
;;;    Signal the appropriate condition when we get a floating-point error.
;;;
(defun sigfpe-handler (signal code scp)
  (declare (ignore signal code)
	   (type system-area-pointer scp))
  (let* ((modes (sigcontext-floating-point-modes
		 (alien:sap-alien scp (* unix:sigcontext))))
	 (traps (logand (ldb float-exceptions-byte modes)
			(ldb float-traps-byte modes))))
    #+(and darwin ppc)
    (let ((new-modes modes))
      ;; Clear out all exceptions and save them to the context.
      ;;
      ;; XXX: Should we just clear out the bits for the traps that are
      ;; enabled?  If we did that then the accrued exceptions would be
      ;; correct.
      (setf (ldb float-sticky-bits new-modes) 0)
      ;; Clear out the various sticky invalid operation bits too.
      ;;
      ;; XXX: Should we only do that if the invalid trap is enabled?
      (setf (ldb float-invalid-op-1-byte new-modes) 0)
      (setf (ldb float-invalid-op-2-byte new-modes) 0)
      (setf (floating-point-modes) new-modes)
      (setf (sigcontext-floating-point-modes
	     (alien:sap-alien scp (* unix:sigcontext)))
	    new-modes))

    #+sse2
    (let* ((new-modes modes)
	   (new-exceptions (logandc2 (ldb float-exceptions-byte new-modes)
				     traps)))
      ;; Clear out the status for any enabled traps.  With SSE2, if
      ;; the current exception is enabled, the next FP instruction
      ;; will cause the exception to be signaled again.  Hence, we
      ;; need to clear out the exceptions that we are handling here.
      (setf (ldb float-exceptions-byte new-modes) new-exceptions)
      ;; XXX: This seems not right.  Shouldn't we be setting the modes
      ;; in the sigcontext instead?  This however seems to do what we
      ;; want.
      (setf (vm:floating-point-modes) new-modes))
    
    (multiple-value-bind (fop operands)
	(let ((sym (find-symbol "GET-FP-OPERANDS" "VM")))
	  (if (fboundp sym)
	      (funcall sym (alien:sap-alien scp (* unix:sigcontext)) modes)
	      (values nil nil)))
      (cond ((not (zerop (logand float-divide-by-zero-trap-bit traps)))
	     (error 'division-by-zero
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-invalid-trap-bit traps)))
	     (error 'floating-point-invalid-operation
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-overflow-trap-bit traps)))
	     (error 'floating-point-overflow
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-underflow-trap-bit traps)))
	     (error 'floating-point-underflow
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-inexact-trap-bit traps)))
	     (error 'floating-point-inexact
		    :operation fop
		    :operands operands))
	    (t
	     (error _"SIGFPE with no exceptions currently enabled?"))))))

;;; WITH-FLOAT-TRAPS-MASKED  --  Public
;;;
(defmacro with-float-traps-masked (traps &body body)
  _N"Execute BODY with the floating point exceptions listed in TRAPS
  masked (disabled).  TRAPS should be a list of possible exceptions
  which includes :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID and
  :DIVIDE-BY-ZERO and on the X86 :DENORMALIZED-OPERAND. The respective
  accrued exceptions are cleared at the start of the body to support
  their testing within, and restored on exit."
  (let ((traps (dpb (float-trap-mask traps) float-traps-byte 0))
	(exceptions (dpb (float-trap-mask traps) float-sticky-bits 0))
	(trap-mask (dpb (lognot (float-trap-mask traps))
			float-traps-byte #xffffffff))
	(exception-mask (dpb (lognot (vm::float-trap-mask traps))
			     float-sticky-bits #xffffffff))
	;; On ppc if we are masking the invalid trap, we need to make
	;; sure we wipe out the various individual sticky bits
	;; representing the invalid operation.  Otherwise, if we
	;; enable the invalid trap later, these sticky bits will cause
	;; an exception.
	#+ppc
	(invalid-mask (if (member :invalid traps)
			  (dpb 0 vm:float-invalid-op-1-byte #xffffffff)
			  #xffffffff))
	(orig-modes (gensym)))
    `(let ((,orig-modes (floating-point-modes)))
      (unwind-protect
	   (progn
	     (setf (floating-point-modes)
		   (logand ,orig-modes ,(logand trap-mask exception-mask)))
	     ,@body)
	;; Restore the original traps and exceptions.
	(setf (floating-point-modes)
	      (logior (logand ,orig-modes ,(logior traps exceptions))
		      (logand (floating-point-modes)
			      ,(logand trap-mask exception-mask)
			      #+ppc
			      ,invalid-mask
		       #+mips ,(dpb 0 float-exceptions-byte #xffffffff))))))))
