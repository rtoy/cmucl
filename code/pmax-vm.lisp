;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pmax-vm.lisp,v 1.7 1991/02/08 13:34:42 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pmax-vm.lisp,v 1.7 1991/02/08 13:34:42 ram Exp $
;;;
;;; This file contains the PMAX specific runtime stuff.
;;;
(in-package "MIPS")
(use-package "SYSTEM")

(export '(fixup-code-object internal-error-arguments))


;;;; Add machine specific features to *features*

(pushnew :decstation-3100 *features*)
(pushnew :pmax *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "DECstation")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "DECstation")



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
(defun internal-error-arguments (sc)
  (alien-bind ((sc sc mach:sigcontext t))
    (let ((pc (alien-access (mach:sigcontext-pc (alien-value sc)))))
      (declare (type system-area-pointer pc))
      (when (logbitp 31
		     (alien-access (mach:sigcontext-cause (alien-value sc))))
	(setf pc (sap+ pc 4)))
      (when (= (sap-ref-8 pc 4) 255)
	(setf pc (sap+ pc 1)))
      (let* ((length (sap-ref-8 pc 4))
	     (vector (make-array length :element-type '(unsigned-byte 8))))
	(declare (type (unsigned-byte 8) length)
		 (type (simple-array (unsigned-byte 8) (*)) vector))
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
	    (values error-number (sc-offsets))))))))


;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun sigcontext-floating-point-modes (scp)
  (alien-bind ((sc (make-alien 'mach:sigcontext
					     #.(ext:c-sizeof 'mach:sigcontext)
					     scp)
			  mach:sigcontext
			  t))
    (alien-access (mach:sigcontext-fpc_csr (alien-value sc)))))
