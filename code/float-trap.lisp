;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/float-trap.lisp,v 1.2 1990/10/14 19:06:38 ram Exp $
;;;
;;;    This file contains stuff for controlling floating point traps.  It is
;;; fairly specific to the MIPS R2010.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "VM")
(export '(current-float-trap floating-point-modes sigfpe-handler))
(in-package "EXTENSIONS")
(export '(set-floating-point-modes))
(in-package "VM")

(eval-when (compile load eval)

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-underflow-trap-bit (ash 1 1))
(defconstant float-overflow-trap-bit (ash 1 2))
(defconstant float-divide-by-zero-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant float-rounding-mode (byte 2 0))
(defconstant float-sticky-bits (byte 5 2))
(defconstant float-traps-byte (byte 5 7))
(defconstant float-exceptions-byte (byte 5 12))
(defconstant float-condition-bit (ash 1 23))

(defconstant float-trap-alist
  (list (cons :underflow float-underflow-trap-bit)
	(cons :overflow float-overflow-trap-bit)
	(cons :inexact float-inexact-trap-bit)
	(cons :invalid float-invalid-trap-bit)
	(cons :divide-by-zero float-divide-by-zero-trap-bit)))

  
;;; FLOAT-TRAP-MASK  --  Internal
;;;
;;;    Return a mask with all the specified float trap bits set.
;;;
(defun float-trap-mask (names)
  (reduce #'logior
	  (mapcar #'(lambda (x)
		      (or (cdr (assoc x float-trap-alist))
			  (error "Unknown float trap kind: ~S." x)))
		  names)))
  
); Eval-When (Compile Load Eval)


;;; Interpreter stubs.
;;;
(defun floating-point-modes () (floating-point-modes))
(defun (setf floating-point-modes) () (floating-point-modes))


;;; SET-FLOATING-POINT-MODES  --  Public
;;;
(defun set-floating-point-modes (&key traps)
  "Set options for the floating point hardware:
   :Traps
       A list of the exception conditions that should cause traps.  Possible
       exceptions are :underflow, :overflow, :inexact, :invalid and
       :divide-by-zero."
  (setf (ldb float-traps-byte (floating-point-modes))
	(float-trap-mask traps))
  (values))


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
(defun sigfpe-handler (signal code scp)
  (declare (ignore signal code))
  (system:alien-bind ((sc (system:make-alien 'mach:sigcontext
					     #.(ext:c-sizeof 'mach:sigcontext)
					     scp)
			  mach:sigcontext
			  t))
    (let ((traps (ldb float-exceptions-byte
		      (system:alien-access
		       (mach:sigcontext-fpc_csr
			(system:alien-value sc))))))
      (cond ((not (zerop (logand float-divide-by-zero-trap-bit traps)))
	     (error 'division-by-zero))
	    ((not (zerop (logand float-invalid-trap-bit traps)))
	     (error 'ext:floating-point-invalid))
	    ((not (zerop (logand float-overflow-trap-bit traps)))
	     (error 'floating-point-overflow))
	    ((not (zerop (logand float-underflow-trap-bit traps)))
	     (error 'floating-point-underflow))
	    ((not (zerop (logand float-inexact-trap-bit traps)))
	     (error 'ext:floating-point-inexact))
	    (t
	     (error "SIGFPE with no current exceptions?"))))))
