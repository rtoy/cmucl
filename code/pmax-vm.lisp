;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pmax-vm.lisp,v 1.3 1990/11/26 15:16:43 wlott Exp $
;;;
;;; This file contains the PMAX specific runtime stuff.
;;;
(in-package "MIPS")
(use-package "SYSTEM")

(export '(fixup-code-object internal-error-arguments))


;;;; Add machine specific features to *features*

(pushnew :decstation-3100 *features*)
(pushnew :pmax *features*)



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset fixup kind)
  (multiple-value-bind (word-offset rem) (truncate offset word-bytes)
    (unless (zerop rem)
      (error "Unaligned instruction?  offset=#x~X." offset))
    (system:without-gcing
     (let ((sap (truly-the system-area-pointer
			   (%primitive c::code-instructions code))))
       (ecase kind
	 (:jump
	  (assert (zerop (ash fixup -26)))
	  (setf (ldb (byte 26 0)
		     (system:sap-ref-32 sap word-offset))
		(ash fixup -2)))
	 (:lui
	  (setf (sap-ref-16 sap (* word-offset 2))
		(+ (ash fixup -16)
		   (if (logbitp 15 fixup) 1 0))))
	 (:addi
	  (setf (sap-ref-16 sap (* word-offset 2))
		(ldb (byte 16 0) fixup))))))))




;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (alien-bind ((sc (make-alien 'mach:sigcontext
			       #.(c-sizeof 'mach:sigcontext)
			       scp)
		   mach:sigcontext
		   t)
	       (regs (mach:sigcontext-regs (alien-value sc)) mach:int-array t))
    (let* ((original-pc (alien-access (mach:sigcontext-pc (alien-value sc))))
	   (pc (sap+ original-pc
		     (+ (if (logbitp 31
				     (alien-access
				      (mach:sigcontext-cause
				       (alien-value sc))))
			    4
			    0)
			(if (= (sap-ref-8 original-pc 4) 255)
			    1
			    0))))
	   (length (sap-ref-8 pc 4))
	   (vector (make-array length :element-type '(unsigned-byte 8))))
      (copy-from-system-area pc (* vm:byte-bits 5)
			     vector (* vm:word-bits
				       vm:vector-data-offset)
			     (* length vm:byte-bits))
      (let* ((index 0)
	     (error-number (c::read-var-integer vector index)))
	(collect ((sc-offsets))
	  (loop
	    (when (>= index length)
	      (return))
	    (sc-offsets (c::read-var-integer vector index)))
	  (values error-number (sc-offsets)))))))

