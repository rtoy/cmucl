;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/vm.lisp,v 1.38 1990/11/13 23:00:46 wlott Exp $
;;;
;;; This file contains the VM definition for the MIPS R2000 and the new
;;; object format.
;;;
;;; Written by Christopher Hoover and William Lott.
;;;
(in-package "MIPS")


;;;; Registers

(eval-when (compile eval)

(defmacro defreg (name offset)
  (let ((offset-sym (symbolicate name "-OFFSET")))
    `(progn
       (eval-when (compile eval load)
	 (defconstant ,offset-sym ,offset))
       (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

(defmacro defregset (name &rest regs)
  `(eval-when (compile eval load)
     (defconstant ,name
       (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs)))))

)

(defvar *register-names* (make-array 32 :initial-element nil))

(defreg zero 0)
(defreg nl3 1)
(defreg nl4 2)
(defreg flags 3)
(defreg nl0 4)
(defreg nl1 5)
(defreg nl2 6)
(defreg nargs 7)
(defreg a0 8)
(defreg a1 9)
(defreg a2 10)
(defreg a3 11)
(defreg a4 12)
(defreg a5 13)
(defreg cname 14)
(defreg lexenv 15)
(defreg nfp 16)
(defreg old-fp 17)
(defreg lra 18)
(defreg l0 19)
(defreg null 20)
(defreg bsp 21)
(defreg fp 22)
(defreg csp 23)
(defreg l1 24)
(defreg alloc 25)
(defreg l2 28)
(defreg nsp 29)
(defreg code 30)
(defreg lip 31)

(defregset non-descriptor-regs
  nl0 nl1 nl2 nl3 nl4 nargs)

(defregset descriptor-regs
  a0 a1 a2 a3 a4 a5 cname lexenv nfp old-fp lra l0 l1 l2)

(defregset register-arg-offsets
  a0 a1 a2 a3 a4 a5)


;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 32)
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


  ;; Immediate numeric constants.
  ;; 
  ;;   zero = (integer 0 0)
  ;; 
  ;;   negative-immediate = (integer #x-1FFF #-x0001)
  ;;        The funny lower bound guarantees that the negation of an immediate
  ;;        is still an immediate.
  ;; 
  ;;   immediate = (integer 0 #x1FFE)
  ;;	   The funny upper bound guarantees that (1+ immediate) will fit in
  ;;        16 bits.
  ;; 
  ;;   unsigned-immediate = (integer #x1FFF #x3FFE)
  ;;	   The funny upper bound guarantees that (1+ immediate) will fit in
  ;;        16 bits.
  ;;
  (zero immediate-constant)
  (negative-immediate immediate-constant)
  (immediate immediate-constant)
  (unsigned-immediate immediate-constant)

  ;; Immediate SCs for things other than numbers. 
  (null immediate-constant)
  (immediate-base-character immediate-constant)
  (immediate-sap immediate-constant)

  ;; Anything else that can be computed faster than loaded that doesn't fit in
  ;; any of the above immediate SCs.
  (random-immediate immediate-constant)



  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (base-character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack :element-size 2) ; double floats.



  ;; **** Things that can go in the integer registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :constant-scs (negative-immediate zero immediate unsigned-immediate
			   immediate-base-character random-immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :constant-scs (constant null random-immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (base-character-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate-base-character)
   :save-p t
   :alternate-scs (base-character-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate-sap)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (negative-immediate zero immediate unsigned-immediate
				     random-immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate unsigned-immediate random-immediate)
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
   :locations (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
   ;; Note: we don't bother with the element size, 'cause load-tn's with
   ;; an element-size other than one don't work, and nothing can be allocated
   ;; in the odd fp regs anyway.
   ;; :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))




  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size))




;;;; Random TNs for interesting registers

(eval-when (compile eval)

(defmacro defregtn (name sc)
  (let ((offset-sym (symbolicate name "-OFFSET"))
	(tn-sym (symbolicate name "-TN")))
    `(defparameter ,tn-sym
       (make-random-tn :kind :normal
		       :sc (sc-or-lose ',sc)
		       :offset ,offset-sym))))

)

(defregtn zero any-reg)
(defregtn lip interior-reg)
(defregtn code descriptor-reg)
(defregtn flags non-descriptor-reg)
(defregtn alloc any-reg)
(defregtn null descriptor-reg)

(defregtn nargs any-reg)
(defregtn cname descriptor-reg)
(defregtn lexenv descriptor-reg)

(defregtn bsp any-reg)
(defregtn csp any-reg)
(defregtn fp any-reg)
(defregtn old-fp any-reg)
(defregtn nsp any-reg)
(defregtn nfp any-reg)



;;;; Side-Effect Classes

(export '(vop-attributes))

(def-boolean-attribute vop
  any)



;;;
;;; Immediate-Constant-SC  --  Interface
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero *backend*))
    (null
     (sc-number-or-lose 'null *backend*))
    ((integer #x-1FFF #x-0001)
     (sc-number-or-lose 'negative-immediate *backend*))
    ((integer 0 #x1FFE)
     (sc-number-or-lose 'immediate *backend*))
    ((integer #x1FFF #x3FFE)
     (sc-number-or-lose 'unsigned-immediate *backend*))
    (symbol
     (if (vm:static-symbol-p value)
	 (sc-number-or-lose 'random-immediate *backend*)
	 nil))
    (#-new-compiler (signed-byte 30)
     #+new-compiler fixnum
     (sc-number-or-lose 'random-immediate *backend*))
    #+new-compiler
    (system-area-pointer
     (sc-number-or-lose 'immediate-sap *backend*))
    (character
     #-new-compiler
     (if (string-char-p value)
	 (sc-number-or-lose 'immediate-base-character *backend*)
	 nil)
     #+new-compiler
     (sc-number-or-lose 'immediate-base-character *backend*))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (compile load eval)

;;; Offsets of special stack frame locations
(defconstant old-fp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)


;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 6)

;;; The offsets within the register-arg SC that we pass values in, first
;;; value first.
;;;

;;; Names to use for the argument registers.
;;; 
(defconstant register-arg-names '(a0 a1 a2 a3 a4 a5))

); Eval-When (Compile Load Eval)


;;; A list of TN's describing the register arguments.
;;;
(defparameter register-arg-tns
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  register-arg-offsets))



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
      (float-registers (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))
