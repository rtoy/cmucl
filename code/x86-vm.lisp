;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: X86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
;(ext:file-comment
;  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/x86-vm.lisp,v 1.2 1997/04/13 21:07:29 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the X86 specific runtime stuff.
;;;

(in-package :vm)
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "UNIX")
(use-package :kernel)

(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution
	  alternate-get-global-address))


;;;; The sigcontext structure.
;;;; Add machine specific features to *features*

(pushnew :x86 *features*)



#+linux
(def-alien-type nil 
  (struct fpreg
          (significand (array unsigned-short 4))
          (exponent unsigned-short)))
#+linux
(def-alien-type  nil
   (struct fpstate
        (cw  unsigned-long)
        (sw  unsigned-long)
        (tag  unsigned-long)
        (ipoff  unsigned-long)
        (cssel  unsigned-long)
        (dataoff  unsigned-long)
        (datasel unsigned-long)
        (fpreg (array (struct fpreg) 8))
        (status unsigned-long)))

;;; for FreeBSD
#+freebsd
(def-alien-type sigcontext
    (struct nil
	(sc-onstack unsigned-int)
	(sc-mask    unsigned-int)
	(sc-sp      unsigned-int)
	(sc-fp	    unsigned-int)
	(sc-isp	    unsigned-int)
	(sc-pc	    unsigned-int)
	(sc-efl     unsigned-int)		; sc_ps
	(sc-es	    unsigned-int)
	(sc-ds	    unsigned-int)
	(sc-cs	    unsigned-int)
	(sc-ss	    unsigned-int)
	(sc-edi	    unsigned-int)
	(sc-esi	    unsigned-int)
	(sc-ebx	    unsigned-int)
	(sc-edx	    unsigned-int)
	(sc-ecx	    unsigned-int)
	(sc-eax	    unsigned-int)))

;; For Linux...
#+linux
(def-alien-type sigcontext
    (struct nil
	(gs  unsigned-short)
	(__gsh   unsigned-short)
	(fs      unsigned-short)
	(__fsh   unsigned-short)
	(sc-es   unsigned-short)
	(__esh   unsigned-short)
	(sc-ds   unsigned-short)
	(__dsh   unsigned-short)
	(sc-edi  unsigned-long)
	(sc-esi  unsigned-long)
	(ebp     unsigned-long)
	(sc-sp   unsigned-long)
	(sc-ebx  unsigned-long)
	(sc-edx  unsigned-long)
	(sc-ecx  unsigned-long)
	(sc-eax  unsigned-long)
	(trapno  unsigned-long)
	(err     unsigned-long)
	(sc-pc   unsigned-long)
	(sc-cs   unsigned-short)
	(__csh   unsigned-short)
	(sc-efl  unsigned-long)
	(esp_at_signal   unsigned-long)
	(sc-ss   unsigned-short)
	(__ssh   unsigned-short)
;       (fpstate   unsigned-long) ;; fpstate struct pointer
	(fpstate (* (struct fpstate)))
	(sc-mask unsigned-long)
	(cr2     unsigned-long)))



;;;; MACHINE-TYPE and MACHINE-VERSION

#-cross-compiler
(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "X86")


#-cross-compiler
(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "X86")



;;; FIXUP-CODE-OBJECT -- Interface
;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.

(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer (c::code-instructions code))))
     (unless (member kind '(:absolute :relative))
       (error "Unknown code-object-fixup kind ~s." kind))
     (ecase kind
       (:absolute
	;; word at sap + offset contains a value to be replaced by
	;; adding that value to fixup.
	(setf (sap-ref-32 sap offset)
	      (+ fixup (sap-ref-32 sap offset))))
       (:relative
	;; fixup is actual address wanted. replace word with value
	;; to add to that loc to get there.
	;; (format t "x86-fixup ~a ~x ~x ~a~&" code offset fixup kind)
	(let* ((loc-sap (+ (sap-int sap) offset))
	       (rel-val (- fixup loc-sap 4)))
	  (declare (type (unsigned-byte 32) loc-sap)
		   (type (signed-byte 32) rel-val))
	  ;;(format t "sap ~x ~x ~x~&" (sap-int sap) loc-sap rel-val)
	  (setf (sap-ref-32 sap offset)  rel-val)) ))))
  nil)



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((pc (int-sap (slot scp 'sc-pc))))
      (declare (type system-area-pointer pc))
      ;; using INT3 the pc is .. INT3 <here> code length bytes...
      (let* ((length (sap-ref-8 pc 1))
	     (vector (make-array length :element-type '(unsigned-byte 8))))
	(declare (type (unsigned-byte 8) length)
		 (type (simple-array (unsigned-byte 8) (*)) vector))
	(copy-from-system-area pc (* vm:byte-bits 2)
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


;;;; Sigcontext access functions.

;;; SIGCONTEXT-PROGRAM-COUNTER -- Interface.
;;;
(defun sigcontext-program-counter (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (int-sap (slot scp 'sc-pc))))

;;; SIGCONTEXT-REGISTER -- Interface.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;

(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (case index				; ugly -- I know.
      (#.eax-offset (slot scp 'sc-eax))
      (#.ecx-offset (slot scp 'sc-ecx))
      (#.edx-offset (slot scp 'sc-edx))
      (#.ebx-offset (slot scp 'sc-ebx))
      (#.esp-offset (slot scp 'sc-sp))
#-linux      (#.ebp-offset (slot scp 'sc-fp))
#+linux      (#.ebp-offset (slot scp 'ebp))
      (#.esi-offset (slot scp 'sc-esi))
      (#.edi-offset (slot scp 'sc-edi)))))


(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (case index
      (#.eax-offset (setf (slot scp 'sc-eax) new))
      (#.ecx-offset (setf (slot scp 'sc-ecx) new))
      (#.edx-offset (setf (slot scp 'sc-edx) new))
      (#.ebx-offset (setf (slot scp 'sc-ebx) new))
      (#.esp-offset (setf (slot scp 'sc-sp)  new))
#-linux      (#.ebp-offset (setf (slot scp 'sc-fp)  new))
#+linux      (#.ebp-offset (setf (slot scp 'ebp)  new))
      (#.esi-offset (setf (slot scp 'sc-esi) new))
      (#.edi-offset (setf (slot scp 'sc-edi) new))))
  new)

(defsetf sigcontext-register %set-sigcontext-register)


;;; SIGCONTEXT-FLOAT-REGISTER  --  Interface
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;; XXX
#-linux
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    ;; fp regs not in sigcontext -- need new vop or c support
    (let ((sap #+nil (alien-sap (slot scp 'sc-fpregs))))
      (declare (ignore sap))
      index
      (ecase format
	(single-float 0s0
	 #+nil (system:sap-ref-single sap (* index vm:word-bytes)))
	(double-float 0d0
	 #+nil(system:sap-ref-double sap (* index vm:word-bytes)))))))

#+linux
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    ;; fp regs in sigcontext !!!
    (let ((reg-sap (alien-sap (deref (slot (deref (slot scp 'fpstate) 0)
					    'fpreg)
				     index))))
      (ecase format
        (single-float
          (system:sap-ref-single reg-sap 0))
        (double-float 
          (system:sap-ref-double reg-sap 0))))))

;;;
#-linux
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  scp index format new-value
  #+nil
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'fpregs))))
      (ecase format
	(single-float
	 (setf (sap-ref-single sap (* index vm:word-bytes)) new-value))
	(double-float
	 (setf (sap-ref-double sap (* index vm:word-bytes)) new-value))))))
#+linux
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((reg-sap (alien-sap (deref (slot (deref (slot scp 'fpstate) 0)
					    'fpreg)
				     index))))
      (ecase format
        (single-float
         (setf (system:sap-ref-single reg-sap 0) new-value))
        (double-float
         (setf (system:sap-ref-double reg-sap 0)new-value))))))

;;;

(defsetf sigcontext-float-register %set-sigcontext-float-register)

;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;

#+FreeBSD
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp)
	   (ignore scp))
  ;; This is broken until some future release of FreeBSD!!!
  (floating-point-modes))
  
#+linux
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let ((cw (slot (deref (slot scp 'fpstate) 0) 'cw))
	(sw (slot (deref (slot scp 'fpstate) 0) 'sw)))
    ;;(format t "cw = ~4x~%sw = ~4x~%" cw sw)
    ;; NOT TESTED -- clear sticky bits to clear interrupt condition
    (setf (slot (deref (slot scp 'fpstate) 0) 'sw) (logandc2 sw #x3f))
    ;;(format t "new sw = ~x~%" (slot (deref (slot scp 'fpstate) 0) 'sw))
    ;; simulate floating-point-modes VOP
    (logior (ash (logand sw #xffff) 16) (logxor (logand cw #xffff) #x3f))))


;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).
;;;
;;; On the x86 under FreeBSD, we prepend an underscore. If this is not
;;; done under Linux then this is the place to make the change.
;;;
(defun extern-alien-name (name)
  (declare (type simple-string name))
  (lisp:concatenate 'string #+linux "" #-linux "_" name))

;;; This used to live in foreign.lisp but it gets loaded too late
;;; to be useful. This gets used by the loader to map lisp foreign
;;; symbol names to the OS's version of it. This was added for the
;;; Linux port -- maybe it makes the above extern-alien-name
;;; obsolete?
(defun system:alternate-get-global-address(symbol)
  (declare (type simple-string symbol))
  (let ((namex symbol)
        (table lisp::*foreign-symbols*)) ; defined in load.lisp
    (cond ((gethash namex table nil))
#+linux   ((gethash (concatenate 'string "PVE_stub_" namex) table nil))
#+linux   ((gethash (concatenate 'string "" namex) table nil)) ; Linux
#+freebsd ((gethash (concatenate 'string "_" namex) table nil)); FreeBSD
          ((gethash (concatenate 'string "__" namex) table nil))
          ((gethash (concatenate 'string "__libc_" namex) table nil))
          (t (progn (format t "Error: can't be in alt-get-gl-addr ~a" namex)
	;; returning 0 is VERY dangerous!
		0)))))



;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component executable.
;;; On the sparc, we don't need to do anything, because the i and d caches
;;; are unified.
;;; 
(defun sanctify-for-execution (component)
  (declare (ignore component))
  nil)
 
;;; FLOAT-WAIT
;;;
;;; This is used in error.lisp to insure floating-point  exceptions
;;; are properly trapped. The compiler translates this to a VOP.
;;; Note: if you are compiling this from an old version you may need
;;; to disable this until the float-wait VOP is entrenched.
(defun float-wait()
  (float-wait))

;;; FLOAT CONSTANTS
;;;
;;; These are used by the FP move-from-{single|double} VOPs
;;; rather than the i387 load constant instructions to avoid
;;; consing in some cases.

(defvar *fp-constant-0s0* 0s0)
(defvar *fp-constant-0d0* 0d0)
(defvar *fp-constant-1s0* 1s0)
(defvar *fp-constant-1d0* 1d0)
