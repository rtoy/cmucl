;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sparc-svr4-vm.lisp,v 1.4 1997/02/19 01:41:40 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the SPARC specific runtime stuff.
;;;
(in-package "SPARC")
(use-package "SYSTEM")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution))


;;;; The sigcontext structure.

(def-alien-type sigset
  (struct sigset
    (sigbits (array unsigned-long 4))))

(def-alien-type sigaltstack
  (struct sigaltstack
    (ss-sp system-area-pointer)
    (ss-size unsigned-long)
    (ss-flags unsigned-long)))

; In the include files this structure consists of 3 structures:
; struct ucontext, that includes struct mcontext which includes; struct fpu.
; Because the fpu structure contains doubles, it must be aligned on
; 8 byte bounderies, consequently so must struct mcontext and struct
; ucontext.
(def-alien-type sigcontext
  (struct nil
    (uc-flags unsigned-long)
    (uc-link system-area-pointer)
    (uc-mask sigset)
    (uc-stack sigaltstack)
    (uc-pad unsigned-long)  ;; align on 8.
    ; struct mcontext 
    (uc-psr unsigned-long)
    (uc-pc system-area-pointer)
    (uc-npc system-area-pointer)
    (uc-regs
     (union un-regs
      (regs (array unsigned-long 16))	; %y, %g1 - %g7, %o0 - %o7
      (regs2
       (struct mcontext-regs
        (uc-y unsigned-long)
        (uc-fill1 (array unsigned-long 13)) ; %g1 - %g7 , %o0 - %o5
        (uc-sp (* unsigned-long)) ; pointer to %l0 - %l7, %i0 - %i7
        (uc-o7 unsigned-long)))))
    (uc-windows system-area-pointer)	 ; usually nil
    ;; struct fpu, already aligned at 8 bytes in mcontext
    (fpregs (array unsigned-long 32))
    (fpq    system-area-pointer)
    (fsr    unsigned-long)
    (fpu-qcnt unsigned-char)
    (fpu-q-entrysize unsigned-char)
    (fpu-en unsigned-char)
    (fpu-sparc-pad unsigned-long)	; align length to 8.
    ;; end of struct fpu
    (filler (array long 22))		; 21 from definition + 1 padding
    ; end of mcontext
    (filler2 (array long 24))))		; 23 from def + 1 padding


;;;; Add machine specific features to *features*

(pushnew :SPARCstation *features*)
(pushnew :sparc *features*)
(pushnew :sun4 *features*)
(pushnew :svr4 *features*)
;; There's SVR4 by Sun called Solaris 2, there's the SVR4 reference port by
;; ICL that runs on ICL's own DRS 6000s.
#+sunos
(pushnew :solaris *features*)



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
  ;(write "SCP = ") (write scp)
  ;(write "slot = ") (write (slot scp 'uc-pc))
  (let* ((pc (with-alien ((scp (* sigcontext) scp))
	       (slot scp 'uc-pc)))
	 (bad-inst (sap-ref-32 pc 0))
	 (op (ldb (byte 2 30) bad-inst))
	 (op2 (ldb (byte 3 22) bad-inst))
	 (op3 (ldb (byte 6 19) bad-inst)))
    (declare (type system-area-pointer pc))
    ;(write "PC = ") (write pc)
    (cond ((and (= op #b00) (= op2 #b000))
	   (args-for-unimp-inst scp))
	  ((and (= op #b10) (= (ldb (byte 4 2) op3) #b1000))
	   (args-for-tagged-add-inst scp bad-inst))
	  ((and (= op #b10) (= op3 #b111010))
	   (args-for-tcc-inst bad-inst))
	  (t
	   (values #.(error-number-or-lose 'unknown-error) nil)))))

(defun args-for-unimp-inst (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let* ((pc (with-alien ((scp (* sigcontext) scp))
	       (slot scp 'uc-pc)))
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
		  (values #.(error-number-or-lose 'unknown-error) nil)
		  (values #.(error-number-or-lose 'object-not-fixnum-error)
			  (list (c::make-sc-offset
				 sparc:descriptor-reg-sc-number
				 rs2)))))
	    (values #.(error-number-or-lose 'unknown-error) nil))
	(values #.(error-number-or-lose 'object-not-fixnum-error)
		(list (c::make-sc-offset sparc:descriptor-reg-sc-number
					 rs1))))))

(defun args-for-tcc-inst (bad-inst)
  (let* ((trap-number (ldb (byte 8 0) bad-inst))
	 (reg (ldb (byte 5 8) bad-inst)))
    (values (case trap-number
	      (#.sparc:object-not-list-trap
	       #.(error-number-or-lose 'object-not-list-error))
	      (#.sparc:object-not-instance-trap
	       #.(error-number-or-lose 'object-not-instance-error))
	      (t
	       #.(error-number-or-lose 'unknown-error)))
	    (list (c::make-sc-offset sparc:descriptor-reg-sc-number reg)))))


;;;; sigcontext access functions.

;;; sigcontext-PROGRAM-COUNTER -- Interface.
;;;
(defun sigcontext-program-counter (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (slot scp 'uc-pc)
    ))

;;; sigcontext-REGISTER -- Interface.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (if (zerop index)
	0
	(if (< index 16)
	    (deref (slot (slot scp 'uc-regs) 'regs) index)
	    (deref (slot (slot (slot scp 'uc-regs) 'regs2) 'uc-sp) (- index 16))
	    ))))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (if (zerop index)
	0
	(if (< index 16)
	    (setf (deref (slot (slot scp 'uc-regs) 'regs) index) new)
	    (setf (deref (slot (slot (slot scp 'uc-regs) 'regs2) 'uc-sp)
			 (- index 16)) new))
	))
  new)

(defsetf sigcontext-register %set-sigcontext-register)


;;; sigcontext-FLOAT-REGISTER  --  Interface
;;;
;;; Like sigcontext-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'fpregs))))
      (ecase format
	(single-float (system:sap-ref-single sap (* index vm:word-bytes)))
	(double-float (system:sap-ref-double sap (* index vm:word-bytes)))))))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'fpregs))))
      (ecase format
	(single-float
	 (setf (sap-ref-single sap (* index vm:word-bytes)) new-value))
	(double-float
	 (setf (sap-ref-double sap (* index vm:word-bytes)) new-value))))))
;;;
(defsetf sigcontext-float-register %set-sigcontext-float-register)


;;; sigcontext-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (slot scp 'fsr)))



;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).
;;; On SVR4 SPARC, we just return the name.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  name)



;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component executable.
;;; On the sparc, we don't need to do anything, because the i and d caches
;;; are unified.
;;;
;;; XXX: newer machines, such as the SuperSPARC and MicroSPARC based ones,
;;; have a split I&D cache and do require cahce flusing.
;;; 
(defun sanctify-for-execution (component)
  (without-gcing
    (alien-funcall (extern-alien "os_flush_icache"
				 (function void
					   system-area-pointer
					   unsigned-long))
		   (code-instructions component)
		   (* (code-header-ref component code-code-size-slot)
		      word-bytes)))
  nil)
