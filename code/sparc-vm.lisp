;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sparc-vm.lisp,v 1.7 1991/02/03 11:20:08 wlott Exp $
;;;
;;; This file contains the SPARC specific runtime stuff.
;;;
(in-package "SPARC")
(use-package "SYSTEM")

(export '(fixup-code-object internal-error-arguments))


;;;; Add machine specific features to *features*

(pushnew :SPARCstation *features*)
(pushnew :sparc *features*)
(pushnew :sun4 *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "SPARCstation")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "SPARCstation")



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
	 (:call
	  (error "Can't deal with CALL fixups, yet."))
	 (:sethi
	  (setf (ldb (byte 22 0) (sap-ref-32 sap word-offset))
		(ldb (byte 22 10) fixup)))
	 (:add
	  (setf (ldb (byte 10 0) (sap-ref-32 sap word-offset))
		(ldb (byte 10 0) fixup))))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (sc)
  (alien-bind ((sc sc mach:sigcontext t))
    (let* ((pc (alien-access (mach:sigcontext-pc (alien-value sc))))
	   (bad-inst (sap-ref-32 pc 0))
	   (op (ldb (byte 2 30) bad-inst))
	   (op2 (ldb (byte 3 22) bad-inst))
	   (op3 (ldb (byte 6 19) bad-inst)))
      (cond ((and (= op #b00) (= op2 #b000))
	     (args-for-unimp-inst sc))
	    ((and (= op #b10) (= (ldb (byte 4 2) op3) #b1000))
	     (args-for-tagged-add-inst sc bad-inst))
	    ((and (= op #b10) (= op3 #b111010))
	     (args-for-tcc-inst bad-inst))
	    (t
	     (values (error-number-or-lose 'unknown-error)
		     nil))))))

(defun args-for-unimp-inst (sc)
  (alien-bind ((sc sc mach:sigcontext t))
    (let* ((pc (alien-access (mach:sigcontext-pc (alien-value sc))))
	   (length (sap-ref-8 pc 4))
	   (vector (make-array length :element-type '(unsigned-byte 8))))
      (declare (type system-area-pointer pc)
	       (type (unsigned-byte 8) length)
	       (type (simple-array (unsigned-byte 8) (*)) vector))
      (copy-from-system-area pc (* sparc:byte-bits 5)
			     vector (* sparc:word-bits
				       sparc:vector-data-offset)
			     (* length sparc:byte-bits))
      (let* ((index 0)
	     (error-number (c::read-var-integer vector index)))
	(collect ((sc-offsets))
		 (loop
		   (when (>= index length)
		     (return))
		   (sc-offsets (c::read-var-integer vector index)))
		 (values error-number (sc-offsets)))))))

(defun args-for-tagged-add-inst (sc bad-inst)
  (alien-bind ((sc sc mach:sigcontext t)
	       (regs (mach:sigcontext-regs (alien-value sc)) mach:int-array t))
    (let* ((rs1 (ldb (byte 5 14) bad-inst))
	   (op1 (di::make-lisp-obj
		 (alien-access
		  (mach:int-array-ref (alien-value regs)
				      rs1)))))
      (if (fixnump op1)
	  (if (zerop (ldb (byte 1 13) bad-inst))
	      (let* ((rs2 (ldb (byte 5 0) bad-inst))
		     (op2 (di::make-lisp-obj
			   (alien-access
			    (mach:int-array-ref (alien-value regs)
						rs2)))))
		(if (fixnump op2)
		    (values (error-number-or-lose 'unknown-error)
			    nil)
		    (values (error-number-or-lose 'object-not-fixnum-error)
			    (list (c::make-sc-offset
				   sparc:descriptor-reg-sc-number
				   rs2)))))
	      (values (error-number-or-lose 'unknown-error)
		      nil))
	  (values (error-number-or-lose 'object-not-fixnum-error)
		  (list (c::make-sc-offset sparc:descriptor-reg-sc-number
					   rs1)))))))

(defun args-for-tcc-inst (bad-inst)
  (let* ((trap-number (ldb (byte 8 0) bad-inst))
	 (reg (ldb (byte 5 8) bad-inst)))
    (values (case trap-number
	      (#.sparc:object-not-list-trap
	       (error-number-or-lose 'object-not-list-error))
	      (#.sparc:object-not-structure-trap
	       (error-number-or-lose 'object-not-structure-error))
	      (t
	       (error-number-or-lose 'unknown-error)))
	    (list (c::make-sc-offset sparc:descriptor-reg-sc-number reg)))))


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
    (alien-access (mach:sigcontext-fsr (alien-value sc)))))
