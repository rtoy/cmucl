;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/rt-vm.lisp,v 1.6.1.1 1994/10/19 23:23:40 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the RT specific runtime stuff.
;;;
(in-package "RT")
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  s-context-register s-context-float-register
	  s-context-floating-point-modes extern-alien-name))


;;;; The s-context structure.

(def-alien-type s-context
  (struct nil
    (sc-onstack unsigned-long)
    (sc-mask unsigned-long)
    (sc-floatsave system-area-pointer)
    (sc-sp system-area-pointer)
    (sc-fp system-area-pointer)
    (sc-ap system-area-pointer)
    (sc-pc system-area-pointer) ; IBM calls it the iar.
    (sc-icscs unsigned-long)
    (sc-saveiar system-area-pointer)
    (sc-regs (array unsigned-long 16))))



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
;;; Given the s-context, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (with-alien ((scp (* s-context) scp))
    (let ((pc (slot scp 'sc-pc)))
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



;;;; s-context accessing stuff.

;;; s-context-REGISTER -- Internal.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun s-context-register (scp index)
  (declare (type (alien (* s-context)) scp))
  (with-alien ((scp (* s-context) scp))
    (deref (slot scp 'sc-regs) index)))

(defun %set-s-context-register (scp index new)
  (declare (type (alien (* s-context)) scp))
  (with-alien ((scp (* s-context) scp))
    (setf (deref (slot scp 'sc-regs) index) new)
    new))

(defsetf s-context-register %set-s-context-register)


;;; s-context-FLOAT-REGISTER  --  Internal
;;;
;;; Like s-context-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun s-context-float-register (scp index format)
  (declare (type (alien (* s-context)) scp)
	   (ignore scp index))
  ;; ### Some day we should figure out how to do this right.
  (ecase format
    (single-float 0.0s0)
    (double-float 0.0d0)))
;;;
(defun %set-s-context-float-register (scp index format new-value)
  (declare (type (alien (* s-context)) scp)
	   (ignore scp index format))
  ;; ### Some day we should figure out how to do this right.
  new-value)
;;;
(defsetf s-context-float-register %set-s-context-float-register)


;;; s-context-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a s-context pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun s-context-floating-point-modes (scp)
  (declare (ignore scp))
  ;; ### Some day we should figure out how to do this right.
  0)




;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the RT,
;;; we prepend an underscore.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  (concatenate 'string "_" name))

