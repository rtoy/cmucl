;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/rt-vm.lisp,v 1.5 1992/02/21 22:00:06 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the RT specific runtime stuff.
;;;
(in-package "RT")
(use-package "SYSTEM")

(export '(fixup-code-object internal-error-arguments))


;;;; Add machine specific features to *features*

(pushnew :ibm-pc-rt *features*)
(pushnew :ibmrt *features*)
(pushnew :rt *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "IBM PC/RT")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "IBM PC/RT")



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset) (type (unsigned-byte 32) fixup))
  (system:without-gcing
   (let ((sap (sap+ (kernel:code-instructions code) offset)))
     (ecase kind
       (:cal
	(setf (sap-ref-16 sap 2)
	      (ldb (byte 16 0) fixup)))
       (:cau
	(let ((high (ldb (byte 16 16) fixup)))
	  (setf (sap-ref-16 sap 2)
		(if (logbitp 15 fixup) (1+ high) high))))
       (:ba
	(unless (zerop (ash fixup -24))
	  (warn "#x~8,'0X out of range for branch-absolute." fixup))
	(setf (sap-ref-8 sap 1)
	      (ldb (byte 8 16) fixup))
	(setf (sap-ref-16 sap 2)
	      (ldb (byte 16 0) fixup)))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (sc)
  (alien-bind ((sc sc mach:sigcontext t))
    (let ((pc (alien-access (mach:sigcontext-iar (alien-value sc)))))
      (declare (type system-area-pointer pc))
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
  0
  #+nil
  (alien-bind ((sc (make-alien 'mach:sigcontext
					     #.(ext:c-sizeof 'mach:sigcontext)
					     scp)
			  mach:sigcontext
			  t))
    (alien-access (mach:sigcontext-fsr (alien-value sc)))))
