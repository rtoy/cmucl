;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/float-trap.lisp,v 1.10 1997/08/30 18:21:32 dtc Exp $")
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

(defconstant rounding-mode-alist
  (list (cons :nearest float-round-to-nearest)
	(cons :zero float-round-to-zero)
	(cons :positive-infinity float-round-to-positive)
	(cons :negative-infinity float-round-to-negative)))
  
); Eval-When (Compile Load Eval)


;;; Interpreter stubs.
;;;
(defun floating-point-modes () (floating-point-modes))
(defun (setf floating-point-modes) (new) (setf (floating-point-modes) new))


;;; SET-FLOATING-POINT-MODES  --  Public
;;;
(defun set-floating-point-modes (&key (traps nil traps-p)
				      (rounding-mode nil round-p)
				      (current-exceptions nil current-x-p)
				      (accrued-exceptions nil accrued-x-p)
				      (fast-mode nil fast-mode-p))
  "This function sets options controlling the floating-point hardware.  If a
  keyword is not supplied, then the current value is preserved.  Possible
  keywords:

   :TRAPS
       A list of the exception conditions that should cause traps.  Possible
       exceptions are :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID and
       :DIVIDE-BY-ZERO.  Initially all traps except :INEXACT are enabled.

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
		(error "Unknown rounding mode: ~S." rounding-mode))))
    (when current-x-p
      (setf (ldb float-exceptions-byte modes)
	    (float-trap-mask current-exceptions)))
    (when accrued-x-p
      (setf (ldb float-sticky-bits modes)
	    (float-trap-mask accrued-exceptions)))
    (when fast-mode-p
      (if fast-mode
	  (setq modes (logior float-fast-bit modes))
	  (setq modes (logand (lognot float-fast-bit) modes))))
    (setf (floating-point-modes) modes))
    
  (values))


;;; GET-FLOATING-POINT-MODES  --  Public
;;;
(defun get-floating-point-modes ()
  "This function returns a list representing the state of the floating point
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
  "Current-Float-Trap Trap-Name*
  Return true if any of the named traps are currently trapped, false
  otherwise."
  `(not (zerop (logand ,(dpb (float-trap-mask traps) float-traps-byte 0)
		       (floating-point-modes)))))


;;; SIGFPE-HANDLER  --  Interface
;;;
;;;    Signal the appropriate condition when we get a floating-point error.
;;;

#+FreeBSD
(define-condition floating-point-exception (arithmetic-error)
  ((flags :initarg :traps
	  :reader floating-point-exception-traps))
  (:report (lambda (condition stream)
	     (format stream "Arithmetic error ~S signalled.~%"
		     (type-of condition))
	     (let ((traps (floating-point-exception-traps condition)))
	       (if traps
		   (format stream
			   "Trapping conditions are: ~%~{ ~s~^~}~%"
			   traps)
		   (write-line
		    "No traps are enabled? How can this be?"
		    stream))))))

(defun sigfpe-handler (signal code scp)
  (declare (ignore signal code #+FreeBSD scp)
	   (type system-area-pointer scp))
  (let* ((modes (sigcontext-floating-point-modes
		 (alien:sap-alien scp (* unix:sigcontext))))
	 (traps (logand (ldb float-exceptions-byte modes)
			(ldb float-traps-byte modes))))
    (cond ((not (zerop (logand float-divide-by-zero-trap-bit traps)))
	   (error 'division-by-zero))
	  ((not (zerop (logand float-invalid-trap-bit traps)))
	   (error 'floating-point-invalid-operation))
	  ((not (zerop (logand float-overflow-trap-bit traps)))
	   (error 'floating-point-overflow))
	  ((not (zerop (logand float-underflow-trap-bit traps)))
	   (error 'floating-point-underflow))
	  ((not (zerop (logand float-inexact-trap-bit traps)))
	   (error 'ext:floating-point-inexact))
	  #+FreeBSD
	  ((zerop (ldb float-exceptions-byte modes))
	   ;; I can't tell what caused the exception!!
	   (error 'floating-point-exception
		  :traps (getf (get-floating-point-modes) :traps)))
	  (t
	   (error "SIGFPE with no exceptions currently enabled?")))))

;;; WITH-FLOAT-TRAPS-MASKED  --  Public
;;;
(defmacro with-float-traps-masked (traps &body body)
  (let ((trap-mask (dpb (lognot (float-trap-mask traps))
			float-traps-byte #xffffffff))
	(exception-mask (dpb (lognot (vm::float-trap-mask traps))
			     float-exceptions-byte #xffffffff)))
    `(let ((orig-modes (floating-point-modes)))
      (unwind-protect
	   (progn
	     (setf (floating-point-modes) (logand orig-modes ,trap-mask))
	     ,@body)
	(setf (floating-point-modes)
	      (logand (logior (floating-point-modes)
			      (mask-field float-traps-byte orig-modes))
		      ,exception-mask))))))
