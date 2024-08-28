;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/float-trap.lisp $")
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

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(current-float-trap floating-point-modes sigfpe-handler))
)
(in-package "EXTENSIONS")
(export '(set-floating-point-modes
	  get-floating-point-modes
	  decode-floating-point-modes
	  encode-floating-point-modes
	  with-float-traps-masked
	  with-float-traps-enabled
          with-float-rounding-mode))
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
			  (error (intl:gettext "Unknown float trap kind: ~S.") x)))
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

#+(and sse2 (not darwin))
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
    (declare (type (unsigned-byte 32) new-mode))
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
      (setf (vm::sse2-floating-point-modes) (ldb (byte 24 0) new-mode))
      (setf (vm::x87-floating-point-modes) (ldb (byte 24 0) x87-modes)))
    new-mode)
  )

#+(and sse2 darwin)
(progn
  (defun floating-point-modes ()
    ;; Get just the SSE2 mode bits.
    (vm::sse2-floating-point-modes))

  (defun (setf floating-point-modes) (new-mode)
    (declare (type (unsigned-byte 24) new-mode))
    ;; Set the floating point modes for SSE2.
    (setf (vm::sse2-floating-point-modes) new-mode)
    new-mode)
  )

;;; %SET-FLOATING-POINT-MODES -- Public
;;;
(defun encode-floating-point-modes (&key (floating-point-modes (floating-point-modes))
				       (traps nil traps-p)
				       (rounding-mode nil round-p)
				       (current-exceptions nil current-x-p)
				       (accrued-exceptions nil accrued-x-p)
				       (fast-mode nil fast-mode-p))
  "Encode the floating-point modes according to the give options and the
  specified mode, Floating-Point-Modes.  The resulting new mode is
  returned.  If a keyword is not supplied, then the current value is
  preserved.  Possible keywords:

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
  (let ((modes floating-point-modes))
    (when traps-p
      (let ((trap-mask-bits (float-trap-mask traps)))
	(setf (ldb float-traps-byte modes) trap-mask-bits)
	#+(and x86 sse2)
	(progn
	  ;; Clear out any current or accrued exceptions that match
	  ;; the traps that we are enabling.  If we don't then
	  ;; enabling the traps causes the exceptions to be signaled
	  ;; immediately.  This is a bit annoying.  If the user really
	  ;; wants to resignal the exceptions, he can do that himself.
	  (setf (ldb float-sticky-bits modes)
		(logandc2 (ldb float-sticky-bits modes) trap-mask-bits))
	  (setf (ldb float-exceptions-byte modes)
		(logandc2 (ldb float-exceptions-byte modes) trap-mask-bits)))))
    (when round-p
      (setf (ldb float-rounding-mode modes)
	    (or (cdr (assoc rounding-mode rounding-mode-alist))
		(error (intl:gettext "Unknown rounding mode: ~S.") rounding-mode))))
    (when current-x-p
      (setf (ldb float-exceptions-byte modes)
	    (float-trap-mask current-exceptions))
      #+(and darwin ppc)
      (when (member :invalid current-exceptions)
 	;; Clear out the bits for the detected invalid operation
 	(setf (ldb vm:float-invalid-op-1-byte modes) 0)
	(setf (ldb vm:float-invalid-op-2-byte modes) 0)))

    (when accrued-x-p
      (setf (ldb float-sticky-bits modes)
	    (float-trap-mask accrued-exceptions))
      #+(and darwin ppc)
      (when (member :invalid current-exceptions)
 	;; Clear out the bits for the detected invalid operation
 	(setf (ldb vm:float-invalid-op-1-byte modes) 0)
	(setf (ldb vm:float-invalid-op-2-byte modes) 0)))

    #+(and darwin ppc)
    (when (or accrued-x-p current-x-p)
      (setf (ldb vm:float-exceptions-summary-byte modes) 0))

    (when fast-mode-p
      (if fast-mode
	  (setq modes (logior float-fast-bit modes))
	  (setq modes (logand (lognot float-fast-bit) modes))))

    modes))

;;; SET-FLOATING-POINT-MODES  --  Public
;;;
(defun set-floating-point-modes (&rest args
				 &key traps
				      rounding-mode
				      current-exceptions
				      accrued-exceptions
				      fast-mode)
  "This function sets options controlling the floating-point hardware.  If a
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
  (declare (ignorable traps rounding-mode current-exceptions accrued-exceptions fast-mode))

  (setf (floating-point-modes)
	(apply #'encode-floating-point-modes args))
  (values))


;;; %GET-FLOATING-POINT-MODES  --  Public
;;;
(defun decode-floating-point-modes (modes)
  "This function returns a list representing the state of the floating point
  modes given in Modes.  The list is in the same format as the keyword arguments to
  SET-FLOATING-POINT-MODES."
  (flet ((exc-keys (bits)
	   (macrolet ((frob ()
			`(collect ((res))
			   ,@(mapcar #'(lambda (x)
					 `(when (logtest bits ,(cdr x))
					    (res ',(car x))))
				     float-trap-alist)
			   (res))))
	     (frob))))
    `(:traps ,(exc-keys (ldb float-traps-byte modes))
      :rounding-mode ,(car (rassoc (ldb float-rounding-mode modes)
				   rounding-mode-alist))
      :current-exceptions ,(exc-keys (ldb float-exceptions-byte modes))
      :accrued-exceptions ,(exc-keys (ldb float-sticky-bits modes))
      :fast-mode ,(logtest float-fast-bit modes))))

;;; GET-FLOATING-POINT-MODES  --  Public
;;;
(defun get-floating-point-modes ()
  "This function returns a list representing the state of the floating point
  modes.  The list is in the same format as the keyword arguments to
  SET-FLOATING-POINT-MODES, i.e. 
      (apply #'set-floating-point-modes (get-floating-point-modes))

  sets the floating point modes to their current values (and thus is a no-op)."
  (decode-floating-point-modes (floating-point-modes)))

  
;;; CURRENT-FLOAT-TRAP  --  Interface
;;;
(defmacro current-float-trap (&rest traps)
  "Current-Float-Trap Trap-Name*
  Return true if any of the named traps are currently trapped, false
  otherwise."
  `(not (zerop (logand ,(dpb (float-trap-mask traps) float-traps-byte 0)
		       (floating-point-modes)))))


;;; SIGFPE-HANDLER  --  Interface
;;;
;;;    Signal the appropriate condition when we get a floating-point error.
;;;
#-(and solaris x86)
(defun sigfpe-handler (signal code scp)
  (declare (ignore signal)
	   (type system-area-pointer scp))
  (let* ((modes (sigcontext-floating-point-modes
		 (alien:sap-alien scp (* unix:sigcontext))))
	 (traps (logand (ldb float-exceptions-byte modes)
			(ldb float-traps-byte modes))))

    
    (multiple-value-bind (fop operands)
	(let ((sym (find-symbol "GET-FP-OPERANDS" "VM")))
	  (if (fboundp sym)
	      (funcall sym (alien:sap-alien scp (* unix:sigcontext)) modes)
	      (values nil nil)))
      ;; Don't let throws get away without resetting the
      ;; floating-point modes back to the original values which we get
      ;; from the sigcontext.  Because we can throw, we never return
      ;; from the signal handler so the sigcontext is never restored.
      ;; This means we need to restore the fpu state ourselves.
      (unwind-protect
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
		 #+x86
		 ((not (zerop (logand float-denormal-trap-bit traps)))
		  (error 'floating-point-denormal-operand
			 :operation fop
			 :operands operands))
		 (t
		  ;; It looks like the sigcontext on Solaris/x86 doesn't
		  ;; actually save the status word of the FPU.  The
		  ;; operands also seem to be missing.  Signal a general
		  ;; arithmetic error.
		  #+(and x86 solaris)
		  (error _"SIGFPE with no exceptions currently enabled? (si-code = ~D)"
			 code)
		  ;; For all other x86 ports, we should only get here if
		  ;; the SIGFPE was caused by an integer overflow on
		  ;; division.  For sparc and ppc, I (rtoy) don't think
		  ;; there's any other way to get here since integer
		  ;; overflows aren't signaled.
		  ;;
		  ;; In that case, FOP should be /, so we can generate a
		  ;; nice arithmetic-error.  It's possible to use CODE,
		  ;; which is supposed to indicate what caused the
		  ;; exception, but each OS is different, so we don't; FOP
		  ;; can tell us.
		  #-(and x86 solaris)
		  (if fop
		      (error 'arithmetic-error
			     :operation fop
			     :operands operands)
		      (error _"SIGFPE with no exceptions currently enabled? (si-code = ~D)"
			     code))))
	;; Cleanup
	(let* ((new-modes modes)
	       (new-exceptions (logandc2 (ldb float-exceptions-byte new-modes)
					 traps)))
	  #+(and darwin ppc)
	  (progn
	    ;; (format t "sigfpe: modes   = #B~32,'0b~%" modes)
	    ;; (format t "sigfpe: new-exc = #B~32,'0b~%" new-exceptions)
	    (setf (ldb float-exceptions-byte new-modes) new-exceptions)
	    ;; Clear out all exceptions.
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
	    ;; Clear the FP exception summary bit too.
	    (setf (ldb float-exceptions-summary-byte new-modes) 0)
	    ;; (format t "sigfpe: new modes   = #B~32,'0b~%" new-modes)
	    (setf (floating-point-modes) new-modes))

	  #+sse2
	  (progn
	    ;; Clear out the status for any enabled traps.  With SSE2, if
	    ;; the current exception is enabled, the next FP instruction
	    ;; will cause the exception to be signaled again.  Hence, we
	    ;; need to clear out the exceptions that we are handling here.
	    (setf (ldb float-exceptions-byte new-modes) new-exceptions)
	    #+nil
	    (progn
	      (format *debug-io* "sigcontext modes: #x~4x (~A)~%"
		      modes (decode-floating-point-modes modes))
	      (format *debug-io* "current modes:    #x~4x (~A)~%"
		      (vm:floating-point-modes) (get-floating-point-modes))
	      (format *debug-io* "new  modes: #x~x (~A)~%"
		      new-modes (decode-floating-point-modes new-modes)))
	    (setf (vm:floating-point-modes) new-modes))

	  #-(or sse2 (and darwin ppc))
	  (progn
	    ;; Apparently nothing needed for sparc it seems The FPU
	    ;; state in the signal handler is unchanged and it seems we
	    ;; don't need to reset it any way when we throw out.
	    ))))))

#+(and solaris x86)
(progn
  ;; See /usr/include/sys/machsig.h
  ;;
  ;; There are 2 constants for integer division by zero and integer
  ;; overflow that we aren't using.
  (defconstant +fpe-fltdiv+ 3
    "Signal code for FP divide by zero")
  (defconstant +fpe-fltovf+ 4
    "Signal code for FP overflow")
  (defconstant +fpe-fltund+ 5
    "Signal code for FP underflow")
  (defconstant +fpe-fltres+ 6
    "Signal code for FP inexact result")
  (defconstant +fpe-fltinv+ 7
    "Signal code for FP invalid operation")
  ;; Not sure what this means.
  (defconstant +fpe-fltsub+ 8
    "Signal code for subscript out of range")
  (defconstant +fpe-fltden+ 9
    "Signal code for FP denormalize"))

#+(and solaris x86)
(defun sigfpe-handler (signal code scp)
  (declare (ignore signal)
	   (type system-area-pointer scp))
  (let* ((modes (sigcontext-floating-point-modes
		 (alien:sap-alien scp (* unix:sigcontext))))
	 (traps (logand (ldb float-exceptions-byte modes)
			(ldb float-traps-byte modes))))

    
    (multiple-value-bind (fop operands)
	(let ((sym (find-symbol "GET-FP-OPERANDS" "VM")))
	  (if (fboundp sym)
	      (funcall sym (alien:sap-alien scp (* unix:sigcontext)) modes)
	      (values nil nil)))
      ;; Don't let throws get away without resetting the
      ;; floating-point modes back to the original values which we get
      ;; from the sigcontext.  Because we can throw, we never return
      ;; from the signal handler so the sigcontext is never restored.
      ;; This means we need to restore the fpu state ourselves.
      (unwind-protect
	   (cond 
	     ((= code +fpe-fltdiv+)
	      (error 'division-by-zero
		     :operation fop
		     :operands operands))
	     ((= code +fpe-fltovf+)
	      (error 'floating-point-overflow
		     :operation fop
		     :operands operands))
	     ((= code +fpe-fltund+)
	      (error 'floating-point-underflow
		     :operation fop
		     :operands operands))
	     ((= code +fpe-fltres+)
	      (error 'floating-point-inexact
		     :operation fop
		     :operands operands))
	     ((= code +fpe-fltinv+)
	      (error 'floating-point-invalid-operation
		     :operation fop
		     :operands operands))
	     ((= code +fpe-fltden+)
	      (error 'floating-point-denormal-operand
		     :operation fop
		     :operands operands))
	     (t
	      (error _"SIGFPE code ~D not handled" code)))
	;; Cleanup
	(let* ((new-modes modes)
	       (new-exceptions (logandc2 (ldb float-exceptions-byte new-modes)
					 traps)))
	  #+sse2
	  (progn
	    ;; Clear out the status for any enabled traps.  With SSE2, if
	    ;; the current exception is enabled, the next FP instruction
	    ;; will cause the exception to be signaled again.  Hence, we
	    ;; need to clear out the exceptions that we are handling here.
	    (setf (ldb float-exceptions-byte new-modes) new-exceptions)
	    ;;#+nil
	    (progn
	      (format *debug-io* "sigcontext modes: #x~4x (~A)~%"
		      modes (decode-floating-point-modes modes))
	      (format *debug-io* "current modes:    #x~4x (~A)~%"
		      (vm:floating-point-modes) (get-floating-point-modes))
	      (format *debug-io* "new  modes: #x~x (~A)~%"
		      new-modes (decode-floating-point-modes new-modes)))
	    (setf (vm:floating-point-modes) new-modes)))))))

(macrolet
    ((with-float-traps (name merge-traps docstring)
       ;; Define macros to enable or disable floating-point
       ;; exceptions.  Masked exceptions and enabled exceptions only
       ;; differ whether we AND in the bits or OR them, respectively.
       ;; MERGE-TRAPS is the logical operation to merge the traps with
       ;; the current floating-point mode.  Thus, use  and MERGE-EXCEPTIONS is the
       ;; logical operation to merge the exceptions (sticky bits) with
       ;; the current mode.
       (let ((macro-name (symbolicate "WITH-FLOAT-TRAPS-" name)))
	 `(progn
	    (defmacro ,macro-name (traps &body body)
	      ,docstring
	      (let ((trap-mask (dpb (lognot (float-trap-mask traps))
				    float-traps-byte #xffffffff))
		    (exception-mask (dpb (lognot (vm::float-trap-mask traps))
					 float-sticky-bits #xffffffff))
		    ;; On ppc if we are masking the invalid trap, we need to make
		    ;; sure we wipe out the various individual sticky bits
		    ;; representing the invalid operation.  Otherwise, if we
		    ;; enable the invalid trap later, these sticky bits will cause
		    ;; an exception.
		    ;;
		    ;; FIXME: Consider removing these for ppc.  Since
		    ;; we now restore the original modes exactly, I
		    ;; don't think these are needed anymore.
		    #+ppc
		    (invalid-mask (if (member :invalid traps)
				      (dpb 0
					   (byte 1 31)
					   (dpb 0 vm::float-invalid-op-2-byte
						(dpb 0 vm:float-invalid-op-1-byte #xffffffff)))
				      #xffffffff))
		    (orig-modes (gensym)))
		`(let ((,orig-modes (floating-point-modes)))
		   (unwind-protect
			(progn
			  (setf (floating-point-modes)
				(ldb (byte 32 0)
				     (logand (,',merge-traps ,orig-modes ,trap-mask)
					     ,exception-mask)))
			  ,@body)
		     ;; Restore the modes exactly as they were.
		     (setf (floating-point-modes) ,orig-modes)))))))))

  ;; WITH-FLOAT-TRAPS-MASKED  --  Public
  (with-float-traps masked logand
    _N"Execute BODY with the floating point exceptions listed in TRAPS
  masked (disabled).  TRAPS should be a list of possible exceptions
  which includes :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID and
  :DIVIDE-BY-ZERO and on the X86 :DENORMALIZED-OPERAND. The respective
  accrued exceptions are cleared at the start of the body to support
  their testing within, and restored on exit.")

  ;; WITH-FLOAT-TRAPS-ENABLED --  Public
  (with-float-traps enabled logorc2
    _N"Execute BODY with the floating point exceptions listed in TRAPS
  enabled.  TRAPS should be a list of possible exceptions which
  includes :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID and
  :DIVIDE-BY-ZERO and on the X86 :DENORMALIZED-OPERAND. The respective
  accrued exceptions are cleared at the start of the body to support
  their testing within, and restored on exit."))

(defmacro with-float-rounding-mode ((rounding-mode) &body body)
  _N"Execute BODY with the floating-point rounding mode set to
  ROUNDING-MODE.  ROUNDING-MODE must be a one:

   :NEAREST
       the default mode of round to nearest even.
   :ZERO
       round numbers down towards zero.  Positive numbers round down
       and negative numbers round up.
   :POSITIVE-INFINITY
       round numbers up towards positive infinity.
   :NEGATIVE-INFINITY
       round numbers down towards negative infinity.

  These are the same as the possible values for the rounding mode in
  SET-FLOATING-POINT-MODES.

  Only the rounding mode is restored on exit; other floating-point
  modes are not modified."
  (let ((old-mode (gensym "OLD-MODE-"))
        (new-mode (gensym "NEW-MODE-")))
  `(let ((,old-mode (ldb float-rounding-mode (floating-point-modes)))
         (,new-mode (cdr (assoc ,rounding-mode rounding-mode-alist))))
     (unwind-protect
          (progn
            (setf (floating-point-modes)
                  (dpb ,new-mode float-rounding-mode (floating-point-modes)))
            ,@body)
       ;; Restore just the rounding mode to the original value.
       (setf (floating-point-modes)
             (dpb ,old-mode float-rounding-mode (floating-point-modes)))))))
