;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sparc-vm.lisp,v 1.11 1992/03/02 00:05:30 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sparc-vm.lisp,v 1.11 1992/03/02 00:05:30 wlott Exp $
;;;
;;; This file contains the SPARC specific runtime stuff.
;;;
(in-package "SPARC")
(use-package "SYSTEM")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-register sigcontext-float-register
	  sigcontext-floating-point-modes extern-alien-name))


;;;; The sigcontext structure.

(def-alien-type sigcontext
  (struct nil
    (sc-onstack unsigned-long)
    (sc-mask unsigned-long)
    (sc-sp system-area-pointer)
    (sc-pc system-area-pointer)
    (sc-npc system-area-pointer)
    (sc-psr unsigned-long)
    (sc-g1 unsigned-long)
    (sc-o0 unsigned-long)
    (sc-regs (array unsigned-long 32))
    (sc-fpregs (array unsigned-long 32))
    (sc-y unsigned-long)
    (sc-fsr unsigned-long)))



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
  (declare (type index offset))
  (unless (zerop (rem offset vm:word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive c::code-instructions code))))
     (ecase kind
       (:call
	(error "Can't deal with CALL fixups, yet."))
       (:sethi
	(setf (ldb (byte 22 0) (sap-ref-32 sap offset))
	      (ldb (byte 22 10) fixup)))
       (:add
	(setf (ldb (byte 10 0) (sap-ref-32 sap offset))
	      (ldb (byte 10 0) fixup)))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let* ((pc (with-alien ((scp (* sigcontext) scp))
	       (slot scp 'sc-pc)))
	 (bad-inst (sap-ref-32 pc 0))
	 (op (ldb (byte 2 30) bad-inst))
	 (op2 (ldb (byte 3 22) bad-inst))
	 (op3 (ldb (byte 6 19) bad-inst)))
    (declare (type system-area-pointer pc))
    (cond ((and (= op #b00) (= op2 #b000))
	   (args-for-unimp-inst scp))
	  ((and (= op #b10) (= (ldb (byte 4 2) op3) #b1000))
	   (args-for-tagged-add-inst scp bad-inst))
	  ((and (= op #b10) (= op3 #b111010))
	   (args-for-tcc-inst bad-inst))
	  (t
	   (values (error-number-or-lose 'unknown-error)
		   nil)))))

(defun args-for-unimp-inst (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let* ((pc (with-alien ((scp (* sigcontext) scp))
	       (slot scp 'sc-pc)))
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
	       (values error-number (sc-offsets))))))

(defun args-for-tagged-add-inst (scp bad-inst)
  (declare (type (alien (* sigcontext)) scp))
  (let* ((rs1 (ldb (byte 5 14) bad-inst))
	 (op1 (di::make-lisp-obj (sigcontext-register scp rs1))))
    (if (fixnump op1)
	(if (zerop (ldb (byte 1 13) bad-inst))
	    (let* ((rs2 (ldb (byte 5 0) bad-inst))
		   (op2 (di::make-lisp-obj (sigcontext-register scp rs2))))
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
					 rs1))))))

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


;;;; Sigcontext access functions.

;;; SIGCONTEXT-REGISTER -- Internal.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (deref (slot scp 'sc-regs) index)))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (setf (deref (slot scp 'sc-regs) index) new)
    new))

(defsetf sigcontext-register %set-sigcontext-register)


;;; SIGCONTEXT-FLOAT-REGISTER  --  Internal
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'sc-fpregs))))
      (ecase format
	(single-float (system:sap-ref-single sap (* index vm:word-bytes)))
	(double-float (system:sap-ref-double sap (* index vm:word-bytes)))))))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'sc-fpregs))))
      (ecase format
	(single-float
	 (setf (sap-ref-single sap (* index vm:word-bytes)) new-value))
	(double-float
	 (setf (sap-ref-double sap (* index vm:word-bytes)) new-value))))))
;;;
(defsetf sigcontext-float-register %set-sigcontext-float-register)


;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (slot scp 'sc-fsr)))



;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the SPARC,
;;; we prepend an underscore.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  (concatenate 'string "_" name))
