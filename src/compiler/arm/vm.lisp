;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp and has been
;;; placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/vm.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition for ARM, based partially on
;;; the RT and Sparc VM definitions.
;;;
;;;
(in-package "ARM")


;;;; Define the registers

(eval-when (compile eval)

(defmacro defreg (name offset)
  (let ((offset-sym (symbolicate name "-OFFSET")))
    `(eval-when (compile eval load)
       (defconstant ,offset-sym ,offset)
       (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

(defmacro defregset (name &rest regs)
  `(eval-when (compile eval load)
     (defconstant ,name
       (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs)))))

); eval-when (compile eval)


(eval-when (compile load eval)

(defvar *register-names* (make-array 16 :initial-element nil))

); eval-when (compile load eval)


(defreg a0 0)
(defreg a1 1)
(defreg a2 2)
(defreg nargs 3)
(defreg nl0 4)
(defreg cfunc 5)
(defreg code 6)
(defreg lexenv 7)			; aka cname/fdefn
(defreg lra 8)
(defreg null 9)				; C thread register
(defreg csp 10)				; Lisp control stack pointer
(defreg cfp 11)				; Lisp control stack frame pointer
(defreg ocfp 12)			; Lisp old cfp
(defreg nsp 13)				; ARM SP register
(defreg lip 14)				; ARM LR register
;; Shouldn't normally be touched, but might be needed for PC-relative
;; addressing.
(defreg pc 15)				; ARM PC

(defregset non-descriptor-regs
  nl0 cfunc nargs)

(defregset descriptor-regs
  a0 a1 a2 ocfp lra)

(defregset register-arg-offsets
  a0 a1 a2)



;;;; SB and SC definition:

;; How many double-float registers are available?
(defconstant double-float-registers
  #+vfpv3-d16 16 #-vfpv3-d16 32)

(define-storage-base registers :finite :size 16)
(define-storage-base float-registers :finite :size #.(* 2 double-float-registers))
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;; 
(defmacro define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
	      (let* ((class (car classes))
		     (sc-name (car class))
		     (constant-name (intern (concatenate 'simple-string
							 (string sc-name)
							 "-SC-NUMBER"))))
		(list* `(define-storage-class ,sc-name ,index
			  ,@(cdr class))
		       `(defconstant ,constant-name ,index)
		       `(export ',constant-name)
		       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(define-storage-classes

  ;; Non-immediate contstants in the constant pool
  (constant constant)

  ;; NULL is in a register.
  (null immediate-constant)

  ;; Anything else that can be an immediate.
  (immediate immediate-constant)


  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (base-char-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack
		:element-size 2 :alignment 2) ; double floats.
  #+double-double
  (double-double-stack non-descriptor-stack :element-size 4 :alignment 2)
  ;; complex-single-floats
  (complex-single-stack non-descriptor-stack :element-size 2)
  ;; complex-double-floats.
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)

  #+double-double
  (complex-double-double-stack non-descriptor-stack :element-size 8 :alignment 4)
  
  ;; **** Things that can go in the integer registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (base-char-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (base-char-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations #.non-descriptor-regs)

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   :reserve-locations (28 29 30 31)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 0 below (* 2 double-float-registers)
			by 2 collect i)
   :element-size 2 :alignment 2
   :reserve-locations #.(list (- double-float-registers 4) (- double-float-registers 2))
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  ;; Non-descriptor double-double floats
  #+double-double
  (double-double-reg float-registers
   :locations #.(loop for i from 0 below (* 2 double-float-registers)
		   by 4 collect i)
   :element-size 4 :alignment 4
   :constant-scs ()
   :save-p t
   :alternate-scs (double-double-stack))
  
  (complex-single-reg float-registers
   :locations #.(loop for i from 0 below 32 by 2 collect i)
   :element-size 2 :alignment 2
   :reserve-locations (28 30)
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations #.(loop for i from 0 below (* 2 double-float-registers)
		      by 4 collect i)
   :element-size 4 :alignment 4
   ;;:reserve-locations (list (- double-float-registers 2))
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  #+double-double
  (complex-double-double-reg float-registers
   :locations #.(loop for i from 0 below (* 2 double-float-registers)
		      by 8 collect i)
   :element-size 8 :alignment 8
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-double-stack))


  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size)
  
  )



;;;; Make some random tns for important registers.

(eval-when (compile eval)

(defmacro defregtn (name sc)
  (let ((offset-sym (symbolicate name "-OFFSET"))
	(tn-sym (symbolicate name "-TN")))
    `(defparameter ,tn-sym
       (make-random-tn :kind :normal
		       :sc (sc-or-lose ',sc)
		       :offset ,offset-sym))))

); eval-when (compile eval)

(defregtn null descriptor-reg)
(defregtn code descriptor-reg)

(defregtn nargs any-reg)
(defregtn csp any-reg)
(defregtn cfp any-reg)
(defregtn ocfp any-reg)
(defregtn nsp any-reg)

(defregtn lexenv descriptor-reg)


;;; Immediate-Constant-SC  --  Interface
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    (null
     (sc-number-or-lose 'null *backend*))
    ((or fixnum system-area-pointer character)
     (sc-number-or-lose 'immediate *backend*))
    (symbol
     (if (static-symbol-p value)
	 (sc-number-or-lose 'immediate *backend*)
	 nil))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (compile load eval)

;;; Offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 3)

;;; Names to use for the argument registers.
;;; 
(defconstant register-arg-names '(a0 a1 a2))

); Eval-When (Compile Load Eval)


;;; A list of TN's describing the register arguments.
;;;
(defparameter register-arg-tns
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  register-arg-offsets))

;;; SINGLE-VALUE-RETURN-BYTE-OFFSET
;;;
;;; This is used by the debugger.
;;;
(export 'single-value-return-byte-offset)
(defconstant single-value-return-byte-offset 8)


;;; LOCATION-PRINT-NAME  --  Interface
;;;
;;;    This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(def-vm-support-routine location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
	(offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
		     (format nil "R~D" offset)))
      ;; How do we distingish single floats from doubles?
      (float-registers (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))

