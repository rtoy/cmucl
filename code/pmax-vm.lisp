;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pmax-vm.lisp,v 1.1 1990/10/23 02:03:06 wlott Exp $
;;;
;;; This file contains the PMAX specific runtime stuff.
;;;
(in-package "VM")

(export '(fixup-code-object))

(defun fixup-code-object (code offset fixup kind)
  ;; Currently, the only kind of fixup we can have is a lui followed by an
  ;; addi.
  (multiple-value-bind
      (word-offset rem)
      (truncate offset vm:word-bytes)
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
