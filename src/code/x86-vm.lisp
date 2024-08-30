;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: X86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: src/code/x86-vm.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the X86 specific runtime stuff.
;;;
;;; Code movement fixups by Douglas T. Crosher, 1997.
;;; Thread support by Douglas T. Crosher, 1999.
;;;

(in-package "X86")
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "KERNEL")

(intl:textdomain "cmucl-x86-vm")

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution))
)

#+complex-fp-vops
(sys:register-lisp-feature :complex-fp-vops)

#+(or x87 (not :sse2))
(sys:register-lisp-feature :x87)
#+sse2
(progn
  (setf *features* (delete :x87 *features*))
  (sys:register-lisp-feature :sse2))


;;;; The sigcontext structure.

(def-alien-type unix:sigcontext system-area-pointer)

;;;; Add machine specific features to *features*

(pushnew :x86 *features*)


;;;; MACHINE-TYPE and MACHINE-VERSION

#-cross-compiler
(defun machine-type ()
  _N"Returns a string describing the type of the local machine."
  ;; Use cpuid to get the processor type.
  (with-output-to-string (s)
    (multiple-value-bind (max-input ebx ecx edx)
	(x86::cpuid 0)
      (declare (ignore max-input))
      (flet ((int-to-string (int)
	       (dotimes (k 4)
		 (let ((code (ldb (byte 8 (* 8 k)) int)))
		   ;; Don't print out null chars.  We're
		   ;; assuming this only happens at the end
		   ;; of the brand string.
		   (unless (zerop code)
		     (write-char (code-char code) s))))))
	(int-to-string ebx)
	(int-to-string edx)
	(int-to-string ecx)))))


#-cross-compiler
(defun machine-version ()
  _N"Returns a string describing the version of the local machine."
  ;; UWe use the processor brand string method to get more detailed
  ;; information about the processor.  If it's not available, just
  ;; give up, even though we could use the brand index (CPUID with
  ;; EAX=1) to get an identifier.
  (let ((max-cpuid (x86::cpuid #x80000000)))
    (cond ((or (not (logbitp 31 max-cpuid))
	       (< max-cpuid #x80000004))
	   ;; Processor brand string not supported, just give up.
	   "X86")
	  (t
	   (with-output-to-string (s)
	     (labels ((int-to-string (int)
			(dotimes (k 4)
			  (let ((code (ldb (byte 8 (* 8 k)) int)))
			    ;; Don't print out null chars.  We're
			    ;; assuming this only happens at the end
			    ;; of the brand string.
			    (unless (zerop code)
			      (write-char (code-char code) s)))))
		      (cpuid-to-string (input)
			(multiple-value-bind (eax ebx ecx edx)
			    (x86::cpuid input)
			  (int-to-string eax)
			  (int-to-string ebx)
			  (int-to-string ecx)
			  (int-to-string edx))))
	       (cpuid-to-string #x80000002)
	       (cpuid-to-string #x80000003)
	       (cpuid-to-string #x80000004)))))))


;;; Fixup-Code-Object -- Interface
;;;
;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;;
;;; Add a fixup offset to the vector of fixup offsets for the given
;;; code object.
;;;
;;; Counter to measure the storage overhead.
(defvar *num-fixups* 0)
;;; XXX
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (flet ((add-fixup (code offset)
	   ;; Although this could check for and ignore fixups for code
	   ;; objects in the read-only and static spaces, this should
	   ;; only be the case when *enable-dynamic-space-code* is
	   ;; True.
	   (when lisp::*enable-dynamic-space-code*
	     (incf *num-fixups*)
	     (let ((fixups (code-header-ref code code-constants-offset)))
	       (cond ((typep fixups '(simple-array (unsigned-byte 32) (*)))
		      (let ((new-fixups
			     (adjust-array fixups (1+ (length fixups))
					   :element-type '(unsigned-byte 32))))
			(setf (aref new-fixups (length fixups)) offset)
			(setf (code-header-ref code code-constants-offset)
			      new-fixups)))
		     (t
		      (unless (or (eq (get-type fixups) vm:unbound-marker-type)
				  (zerop fixups))
			(format t "** Init. code FU = ~s~%" fixups))
		      (setf (code-header-ref code code-constants-offset)
			    (make-array 1 :element-type '(unsigned-byte 32)
					:initial-element offset))))))))
    (system:without-gcing
     (let* ((sap (truly-the system-area-pointer
			    (kernel:code-instructions code)))
	    (obj-start-addr (logand (kernel:get-lisp-obj-address code)
				    #xfffffff8))
	    #+nil (const-start-addr (+ obj-start-addr (* 5 4)))
	    (code-start-addr (sys:sap-int (kernel:code-instructions code)))
	    (ncode-words (kernel:code-header-ref code 1))
	    (code-end-addr (+ code-start-addr (* ncode-words 4))))
       (unless (member kind '(:absolute :relative))
	 (error (intl:gettext "Unknown code-object-fixup kind ~s.") kind))
       (ecase kind
	 (:absolute
	  ;; Word at sap + offset contains a value to be replaced by
	  ;; adding that value to fixup.
	  (setf (sap-ref-32 sap offset) (+ fixup (sap-ref-32 sap offset)))
	  ;; Record absolute fixups that point within the code object.
	  (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
	    (add-fixup code offset)))
	 (:relative
	  ;; Fixup is the actual address wanted.
	  ;;
	  ;; Record relative fixups that point outside the code
	  ;; object.
	  (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
	    (add-fixup code offset))
	  ;; Replace word with value to add to that loc to get there.
	  (let* ((loc-sap (+ (sap-int sap) offset))
		 (rel-val (- fixup loc-sap 4)))
	    (declare (type (unsigned-byte 32) loc-sap)
		     (type (signed-byte 32) rel-val))
	    (setf (signed-sap-ref-32 sap offset) rel-val))))))
    nil))

;;; Do-Load-Time-Code-Fixups
;;;
;;; Add a code fixup to a code object generated by new-genesis. The
;;; fixup has already been applied, it's just a matter of placing the
;;; fixup in the code's fixup vector if necessary.
;;;
#+gencgc
(defun do-load-time-code-fixup (code offset fixup kind)
  (flet ((add-load-time-code-fixup (code offset)
	   (let ((fixups (code-header-ref code vm:code-constants-offset)))
	     (cond ((typep fixups '(simple-array (unsigned-byte 32) (*)))
		    (let ((new-fixups
			   (adjust-array fixups (1+ (length fixups))
					 :element-type '(unsigned-byte 32))))
		      (setf (aref new-fixups (length fixups)) offset)
		      (setf (code-header-ref code vm:code-constants-offset)
			    new-fixups)))
		   (t
		    (unless (or (eq (get-type fixups) vm:unbound-marker-type)
				(zerop fixups))
		      (%primitive print "** Init. code FU"))
		    (setf (code-header-ref code vm:code-constants-offset)
			  (make-array 1 :element-type '(unsigned-byte 32)
				      :initial-element offset)))))))
    (let* ((sap (truly-the system-area-pointer
			   (kernel:code-instructions code)))
	   (obj-start-addr
	    (logand (kernel:get-lisp-obj-address code) #xfffffff8))
	   (code-start-addr (sys:sap-int (kernel:code-instructions code)))
	   (ncode-words (kernel:code-header-ref code 1))
	 (code-end-addr (+ code-start-addr (* ncode-words 4))))
      (ecase kind
	(:absolute
	 ;; Record absolute fixups that point within the
	 ;; code object.
	 (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
	   (add-load-time-code-fixup code offset)))
	(:relative
	 ;; Record relative fixups that point outside the
	 ;; code object.
	 (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
	   (add-load-time-code-fixup code offset)))))))


;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* unix:sigcontext)) scp))
  (with-alien ((scp (* unix:sigcontext) scp))
    (let ((pc (sigcontext-program-counter scp)))
      (declare (type system-area-pointer pc))
      ;; The pc should point to the start of the UD1 instruction.  So
      ;; we have something like:
      ;;
      ;;   offset  contents
      ;;   0       UD1 (contains the trap code)
      ;;   3       length
      ;;   4...    bytes
      (let* ((length (sap-ref-8 pc 3))
	     (vector (make-array length :element-type '(unsigned-byte 8))))
	(declare (type (unsigned-byte 8) length)
		 (type (simple-array (unsigned-byte 8) (*)) vector))
	;; Grab bytes after the length byte where the number of bytes is
	;; given by value of the length byte.
	(copy-from-system-area pc (* vm:byte-bits 4)
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
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_pc"
			  (function system-area-pointer
				    (* unix:sigcontext)))))
    (sap-ref-sap (alien-funcall fn scp) 0)))

;;; SIGCONTEXT-REGISTER -- Interface.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun sigcontext-register (scp index)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_reg"
			  (function system-area-pointer
				    (* unix:sigcontext)
				    (integer 32)))))
    (sap-ref-32 (alien-funcall fn scp index) 0)))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_reg"
			  (function system-area-pointer
				    (* unix:sigcontext)
				    (integer 32)))))
    (setf (sap-ref-32 (alien-funcall fn scp index) 0) new)))

(defsetf sigcontext-register %set-sigcontext-register)


;;; SIGCONTEXT-FLOAT-REGISTER  --  Interface
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float
;;; register.  Format is the type of float to return.  For SSE2, also
;;; support complex numbers.  The format in this case is
;;; complex-single-float and complex-double-float.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_fpu_reg"
			  (function system-area-pointer
				    (* unix:sigcontext)
				    (integer 32)))))
    #+x87
    (coerce (sap-ref-long (alien-funcall fn scp index) 0) format)
    #+sse2
    (if (< index 8)
	(coerce (sap-ref-long (alien-funcall fn scp index) 0) format)
	(ecase format
	  (single-float
	   (sap-ref-single (alien-funcall fn scp index) 0))
	  (double-float
	   (sap-ref-double (alien-funcall fn scp index) 0))
	  (complex-single-float
	   ;; Need to extract the parts out out of the XMM register
	   (let ((addr (alien-funcall fn scp index)))
	     (complex (sap-ref-single addr 0)
		      (sap-ref-single addr 4))))
	  (complex-double-float
	   (let ((addr (alien-funcall fn scp index)))
	     (complex (sap-ref-double addr 0)
		      (sap-ref-double addr 8))))))))

;;;
(defun %set-sigcontext-float-register (scp index format new)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_fpu_reg"
			  (function system-area-pointer
				    (* unix:sigcontext)
				    (integer 32)))))
    (let* ((sap (alien-funcall fn scp index)))
      (if (< index 8)
	  (let ((result (setf (sap-ref-long sap 0) (coerce new 'long-float))))
	    (coerce result format))
	  (ecase format
	    (single-float
	     (setf (sap-ref-single sap 0) new))
	    (double-float
	     (setf (sap-ref-double sap 0) new))
	    (complex-single-float
	     (setf (sap-ref-single sap 0) (realpart new))
	     (setf (sap-ref-single sap 4) (imagpart new)))
	    (complex-double-float
	     (setf (sap-ref-double sap 0) (realpart new))
	     (setf (sap-ref-double sap 8) (imagpart new))))))))

;;;
(defsetf sigcontext-float-register %set-sigcontext-float-register)

;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_fpu_modes"
			  (function (integer 32)
				    (* unix:sigcontext)))))
    (alien-funcall fn scp)))

(defun %set-sigcontext-floating-point-modes (scp new-mode)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_set_sigcontext_fpu_modes"
			  (function (integer 32)
				    (* unix:sigcontext)
				    c-call:unsigned-int))))
    (alien-funcall fn scp new-mode)
    new-mode))

(defsetf sigcontext-floating-point-modes %set-sigcontext-floating-point-modes)

#+solaris
(progn
(defun sigcontext-floating-point-modes-x87 (scp)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_x87_modes"
			  (function (integer 32)
				    (* unix:sigcontext)))))
    (alien-funcall fn scp)))

(defun %set-sigcontext-floating-point-modes-x87 (scp new-modes)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_set_sigcontext_x87_modes"
			  (function c-call:void
				    (* unix:sigcontext)
				    c-call:unsigned-int))))
    (alien-funcall fn scp new-modes)
    new-modes))
  
(defun sigcontext-floating-point-modes-sse2 (scp)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_sigcontext_sse2_modes"
			  (function (integer 32)
				    (* unix:sigcontext)))))
    (alien-funcall fn scp)))

(defun %set-sigcontext-floating-point-modes-sse2 (scp new-modes)
  (declare (type (alien (* unix:sigcontext)) scp))
  (let ((fn (extern-alien "os_set_sigcontext_sse2_modes"
			  (function c-call:void
				    (* unix:sigcontext)
				    c-call:unsigned-int))))
    (alien-funcall fn scp new-modes)
    new-modes))
)


;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).
;;;
(defun extern-alien-name (name)
  (declare (type simple-string name))
  #-elf
  (concatenate 'string "_"  name)
  #+elf
  name)

#+(and (or linux (and freebsd elf)) (not linkage-table))
(defun lisp::foreign-symbol-address-aux (name flavor)
  (declare (ignore flavor))
  (multiple-value-bind (value found)
      (gethash name lisp::*foreign-symbols* 0)
    (if found
	value
	(multiple-value-bind (value found)
	    (gethash
	     (concatenate 'string "PVE_stub_" name)
	     lisp::*foreign-symbols* 0)
	  (if found
	      value
	      (let ((value (system:alternate-get-global-address name)))
		(when (zerop value)
		  (error (intl:gettext "Unknown foreign symbol: ~S") name))
		value))))))



;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component
;;; executable - nothing on the x86.
;;; 
(defun sanctify-for-execution (component)
  (declare (ignore component))
  nil)
 
;;; FLOAT-WAIT
;;;
;;; This is used in error.lisp to insure floating-point  exceptions
;;; are properly trapped. The compiler translates this to a VOP.
;;;
(defun float-wait()
  (float-wait))

;;; FLOAT CONSTANTS
;;;
;;; These are used by the FP move-from-{single|double} VOPs rather
;;; than the i387 load constant instructions to avoid consing in some
;;; cases. Note these are initialise by genesis as they are needed
;;; early.
;;;
(defvar *fp-constant-0f0*)
(defvar *fp-constant-0d0*)

;;; The current alien stack pointer; saved/restored for non-local
;;; exits.
(defvar *alien-stack*)

;;; Support for the MT19937 random number generator. The update
;;; function is implemented as an assembly routine. This definition is
;;; transformed to a call to this routine allowing its use in byte
;;; compiled code.
;;;
#+random-mt19937
(defun random-mt19937 (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state))
  (random-mt19937 state))


;;;; Useful definitions for writing thread safe code.

(in-package "KERNEL")

(export '(atomic-push-symbol-value atomic-pop-symbol-value
	  atomic-pusha atomic-pushd atomic-push-vector))

(defun %instance-set-conditional (object slot test-value new-value)
  (declare (type instance object)
	   (type index slot))
  "Atomically compare object's slot value to test-value and if EQ store
   new-value in the slot. The original value of the slot is returned."
  (%instance-set-conditional object slot test-value new-value))

(defun set-symbol-value-conditional (symbol test-value new-value)
  (declare (type symbol symbol))
  "Atomically compare symbol's value to test-value and if EQ store
  new-value in symbol's value slot and return the original value."
  (set-symbol-value-conditional symbol test-value new-value))

(defun rplaca-conditional (cons test-value new-value)
  (declare (type cons cons))
  "Atomically compare the car of CONS to test-value and if EQ store
  new-value its car and return the original value."
  (rplaca-conditional cons test-value new-value))

(defun rplacd-conditional (cons test-value new-value)
  (declare (type cons cons))
  "Atomically compare the cdr of CONS to test-value and if EQ store
  new-value its cdr and return the original value."
  (rplacd-conditional cons test-value new-value))

(defun data-vector-set-conditional (vector index test-value new-value)
  (declare (type simple-vector vector))
  "Atomically compare an element of vector to test-value and if EQ store
  new-value the element and return the original value."
  (data-vector-set-conditional vector index test-value new-value))

(defmacro atomic-push-symbol-value (val symbol)
  "Thread safe push of val onto the list in the symbol global value."
  (ext:once-only ((n-val val))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list ,symbol))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (set-symbol-value-conditional
		       ',symbol ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

(defmacro atomic-pop-symbol-value (symbol)
  "Thread safe pop from the list in the symbol global value."
  (let ((new-list (gensym))
	(old-list (gensym)))
    `(loop
      (let* ((,old-list ,symbol)
	     (,new-list (cdr ,old-list)))
	(when (eq (set-symbol-value-conditional
		   ',symbol ,old-list ,new-list)
		  ,old-list)
	  (return (car ,old-list)))))))

(defmacro atomic-pusha (val cons)
  "Thread safe push of val onto the list in the car of cons."
  (once-only ((n-val val)
	      (n-cons cons))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list (car ,n-cons)))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (rplaca-conditional ,n-cons ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

(defmacro atomic-pushd (val cons)
  "Thread safe push of val onto the list in the cdr of cons."
  (once-only ((n-val val)
	      (n-cons cons))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list (cdr ,n-cons)))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (rplacd-conditional ,n-cons ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

(defmacro atomic-push-vector (val vect index)
  "Thread safe push of val onto the list in the vector element."
  (once-only ((n-val val)
	      (n-vect vect)
	      (n-index index))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list (svref ,n-vect ,n-index)))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (data-vector-set-conditional
		       ,n-vect ,n-index ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

#+linkage-table
(progn
(defun lisp::foreign-symbol-address-aux (name flavor)
  (let ((entry-num (lisp::register-foreign-linkage name flavor)))
    (+ #.vm:target-foreign-linkage-space-start
       (* entry-num vm:target-foreign-linkage-entry-size))))

(defun lisp::find-foreign-symbol (addr)
  (declare (type (unsigned-byte 32) addr))
  (when (>= addr vm:target-foreign-linkage-space-start)
    (let ((entry (/ (- addr vm:target-foreign-linkage-space-start)
		    vm:target-foreign-linkage-entry-size)))
      (when (< entry (lisp::foreign-linkage-symbols))
	(lisp::foreign-linkage-entry entry)))))
)

(in-package "X86")

(defun get-fp-operation (scp)
  (declare (type (alien (* unix:sigcontext)) scp))
  ;; Get the instruction that caused the SIGFPE from the context.  The
  ;; SIGFPE can be caused by either a floating-point operation or an
  ;; integer division (overflow).  We return the operation associated
  ;; with the instruction, and the the operands of the instruction, if
  ;; possible.

  ;; For SSE2, the PC should be at the offending SSE2 instruction
  (let ((pc (sigcontext-program-counter scp)))
    #+(or)
    (progn
      (format *debug-io* "~&PC = ~S~%" pc)
      (format *debug-io* " ~2,'0X~%" (sys:sap-ref-8 pc 0))
      (format *debug-io* " ~2,'0X~%" (sys:sap-ref-8 pc 1))
      (format *debug-io* " ~2,'0X~%" (sys:sap-ref-8 pc 2))
      (format *debug-io* " ~2,'0X~%" (sys:sap-ref-8 pc 3))
      (format *debug-io* " ~2,'0X~%" (sys:sap-ref-8 pc 4))
      (finish-output *debug-io*))

    (labels
	((fop (x)
	   ;; Look at the byte and see what kind of operation is
	   ;; encoded.
	   (cdr (assoc x '((#x58 . +) (#x59 . *) (#x5c . -) (#x5e . /)))))
	 (decode-mod-r/m (byte)
	   ;; Return the mod bits, the r/m bits, and the value, in
	   ;; that order.  See, for example, Table 2-1 in the Intel 64
	   ;; and IA-32 Architectures Software Developer's Manual,
	   ;; Volume 2A.
	   (values (ldb (byte 2 6) byte)
		   (ldb (byte 3 0) byte)
		   (ldb (byte 3 3) byte)))
	 (decode-operands (offset format)
	   (multiple-value-bind (mod r/m v)
	       (decode-mod-r/m (sys:sap-ref-8 pc offset))
	     #+(or)
	     (format *debug-io* "~&mod = #b~2,'0b~%r/m = #b~3,'0b~%v   = #b~3,'0b~%" mod r/m v)
	     ;; I'm lazy right now and don't want to try to fetch the
	     ;; operand from memory if the source is in memory.  Just
	     ;; return NIL for that.
	     (values (sigcontext-float-register scp (+ 8 v) format)
		     (when (= mod #b11)
		       (sigcontext-float-register scp (+ 8 r/m) format))))))
      ;; Look at the instruction and see if it's one of the arithmetic
      ;; SSE2 instructions or an integer division instruction.  If so,
      ;; figure out the operation and try to get the operands.
      ;; Currently, if an operand is in memory, we don't try to fetch
      ;; it.
      ;;
      ;; Also, for the packed operations that hold complex numbers,
      ;; it's not exactly clear what to do.  The main issue is that
      ;; when multiplying or dividing complex numbers, there is no
      ;; single instruction.  The operation is decomposed into several
      ;; operations and the contents of the packed register may not
      ;; have any simple relationship to the Lisp complex number.  For
      ;; now, instead of returning the complex number, we return a
      ;; list of the components.  Perhaps this is better than nothing,
      ;; but might be confusing.
      (cond ((and (= (sys:sap-ref-8 pc 0) #xf2)
		  (= (sys:sap-ref-8 pc 1) #x0f)
		  (fop (sys:sap-ref-8 pc 2)))
	     ;; ADDSD:  F2 0F 58
	     ;; MULSD:  F2 0F 59
	     ;; SUBSD:  F2 0F 5C
	     ;; DIVSD:  F2 0F 5E
	     ;; SQRTSD: F2 0F 51
	     (multiple-value-bind (dst src)
		 (decode-operands 3 'double-float)
	       (values (fop (sys:sap-ref-8 pc 2)) dst src)))
	    ((and (= (sys:sap-ref-8 pc 0) #xf3)
		  (= (sys:sap-ref-8 pc 1) #x0f)
		  (fop (sys:sap-ref-8 pc 2)))
	     ;; ADDSS:  F3 0F 58
	     ;; MULSS:  F3 0F 59
	     ;; SUBSS:  F3 0F 5C
	     ;; DIVSS:  F3 0F 5E
	     ;; SQRTSS: F3 0F 51
	     (multiple-value-bind (dst src)
		 (decode-operands 3 'single-float)
	       (values (fop (sys:sap-ref-8 pc 2)) dst src)))
	    ((and (= (sys:sap-ref-8 pc 0) #x66)
		  (= (sys:sap-ref-8 pc 1) #x0f)
		  (fop (sys:sap-ref-8 pc 2)))
	     ;; ADDPD:  66 0F 58
	     ;; MULPD:  66 0F 59
	     ;; SUBPD:  66 0F 5C
	     ;; DIVPD:  66 0F 5E
	     (multiple-value-bind (dst src)
		 (decode-operands 3 'complex-double-float)
	       (values (fop (sys:sap-ref-8 pc 2))
		       (list (realpart dst)
			     (imagpart dst))
		       (when src
			 (list (realpart src)
			       (imagpart src))))))
	    ((and (= (sys:sap-ref-8 pc 0) #x0f)
		  (fop (sys:sap-ref-8 pc 1)))
	     ;; ADDPS:  0F 58
	     ;; MULPS:  0F 59
	     ;; SUBPS:  0F 5C
	     ;; DIVPS:  0F 5E
	     (multiple-value-bind (dst src)
		 (decode-operands 2 'complex-single-float)
	       (values (fop (sys:sap-ref-8 pc 1))
		       (list (realpart dst)
			     (imagpart dst))
		       (when src
			 (list (realpart src)
			       (imagpart src))))))
	    ((or (= (sys:sap-ref-8 pc 0) #xf7))
	     ;; DIV or IDIV.  We don't support 8-bit division
	     (multiple-value-bind (mod r/m v)
		 (decode-mod-r/m (sys:sap-ref-8 pc 1))
	       #+(or)
	       (format t "DIV: #X~X: mod, r/m v = ~X ~X ~X~%"
		       (sys:sap-ref-8 pc 0)
		       mod r/m v)
	       ;; r/m tells us the divisor reg
	       (flet ((maybe-adjust-sign (x 64bit-p)
			;; Maybe convert unsigned integer X to a
			;; signed integer.  64BIT-P is set if X is
			;; supposed to be a 64-bit integer.
			(if (= v 7)
			    (- x (if 64bit-p
				     #x10000000000000000
				     #x100000000))
			    x)))
		 ;; For the div instructions, the dividend is always
		 ;; in EDX:EAX
		 (let ((dividend (maybe-adjust-sign
				  (+ (ash (sigcontext-register scp 4) 32)
				     (sigcontext-register scp 0))
				  t))
		       (divisor (maybe-adjust-sign
				 (sigcontext-register scp (ash r/m 1))
				 nil)))
		   (values '/
			   dividend
			   divisor)))))
	    (t
	     (values nil nil nil))))))

(defun get-fp-operands (scp modes)
  (declare (type (alien (* unix:sigcontext)) scp)
	   (ignore modes))
  ;; From the offending FP instruction, get the operation and
  ;; operands, if we can.
  ;;
  ;; FIXME: How do we distinguish between an exception caused by SSE2
  ;; and one caused by x87?
  (multiple-value-bind (fop dst src)
      (get-fp-operation scp)
    (values fop (list dst src))))
