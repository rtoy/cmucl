;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/vm.lisp,v 1.22 1990/04/23 16:45:25 wlott Exp $
;;;
;;; This file contains the VM definition for the MIPS R2000 and the new
;;; object format.
;;;
;;; Written by Christopher Hoover and William Lott.
;;;
(in-package "C")


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
  `(progn
     ,@(let ((index -1))
	 (mapcar #'(lambda (class)
		     `(define-storage-class ,(car class) ,(incf index)
			,@(cdr class)))
		 classes))))

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
   :locations (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 28 31)
   :constant-scs (constant negative-immediate zero immediate unsigned-immediate
			   immediate-base-character random-immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations (8 9 10 11 12 13 14 15 16 17 18 19 28 31)
   :constant-scs (constant negative-immediate zero immediate unsigned-immediate
			   immediate-base-character random-immediate null)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (base-character-reg registers
   :locations (2 3 4 5 6 7)
   :constant-scs (immediate-base-character)
   :save-p t
   :alternate-scs (base-character-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
   :locations (2 3 4 5 6 7)
   :constant-scs (immediate-sap)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations (2 3 4 5 6 7)
   :constant-scs (negative-immediate zero immediate unsigned-immediate
				     random-immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations (2 3 4 5 6 7)
   :constant-scs (zero immediate unsigned-immediate random-immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations (2 3 4 5 6 7))

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (1))


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
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))




  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size))




;;;; Primitive Type Definitions

;;; *Anything*
;;; 
(def-primitive-type t (descriptor-reg))
(defvar *any-primitive-type* (primitive-type-or-lose 't))

;;; Primitive integer types.
;;;
(def-primitive-type fixnum (any-reg))
#| ### Rob needs to think about this more.
(def-primitive-type positive-fixnum (any-reg signed-reg unsigned-reg))
(def-primitive-type negative-fixnum (any-reg signed-reg))
(def-primitive-type negative-signed-byte-32 (signed-reg))
(def-primitive-type unsigned-byte-31 (signed-reg unsigned-reg))
(def-primitive-type unsigned-byte-32 (unsigned-reg))
|#

;;; Other primitive immediate types.
(def-primitive-type base-character (base-character-reg any-reg))

;;; Primitive pointer types.
;;; 
(def-primitive-type function (descriptor-reg))
(def-primitive-type list (descriptor-reg))
(def-primitive-type structure (descriptor-reg))

;;; Primitive other-pointer number types.
;;; 
(def-primitive-type bignum (descriptor-reg))
(def-primitive-type ratio (descriptor-reg))
(def-primitive-type complex (descriptor-reg))
(def-primitive-type single-float (single-reg descriptor-reg))
(def-primitive-type double-float (double-reg descriptor-reg))

;;; Primitive other-pointer array types.
;;; 
(def-primitive-type simple-string (descriptor-reg) :type simple-base-string)
(def-primitive-type simple-bit-vector (descriptor-reg))
(def-primitive-type simple-vector (descriptor-reg))
(def-primitive-type simple-array-unsigned-byte-2 (descriptor-reg)
  :type (simple-array (unsigned-byte 2) (*)))
(def-primitive-type simple-array-unsigned-byte-4 (descriptor-reg)
  :type (simple-array (unsigned-byte 4) (*)))
(def-primitive-type simple-array-unsigned-byte-8 (descriptor-reg)
  :type (simple-array (unsigned-byte 8) (*)))
(def-primitive-type simple-array-unsigned-byte-16 (descriptor-reg)
  :type (simple-array (unsigned-byte 16) (*)))
(def-primitive-type simple-array-unsigned-byte-32 (descriptor-reg)
  :type (simple-array (unsigned-byte 32) (*)))
(def-primitive-type simple-array-single-float (descriptor-reg)
  :type (simple-array single-float (*)))
(def-primitive-type simple-array-double-float (descriptor-reg)
  :type (simple-array double-float (*)))

;;; Note: The complex array types are not inclueded, 'cause it is pointless to
;;; restrict VOPs to them.

;;; Other primitive other-pointer types.
;;; 
(def-primitive-type system-area-pointer (sap-reg descriptor-reg))

;;; Random primitive types that don't exist at the LISP level.
;;; 
(def-primitive-type random (non-descriptor-reg) :type nil)
(def-primitive-type interior (interior-reg) :type nil)
(def-primitive-type catch-block (catch-block) :type nil)




;;;; Primitive-type-of and friends.

;;; Primitive-Type-Of  --  Interface
;;;
;;;    Return the most restrictive primitive type that contains Object.
;;;
(defun primitive-type-of (object)
  (let ((type (ctype-of object)))
    (cond ((not (member-type-p type)) (primitive-type type))
	  ((equal (member-type-members type) '(nil))
	   (primitive-type-or-lose 'list))
	  (t
	   *any-primitive-type*))))

;;; 
(defvar *simple-array-primitive-types*
  '((base-character . simple-string)
    (string-char . simple-string)
    (bit . simple-bit-vector)
    ((unsigned-byte 2) . simple-array-unsigned-byte-2)
    ((unsigned-byte 4) . simple-array-unsigned-byte-4)
    ((unsigned-byte 8) . simple-array-unsigned-byte-8)
    ((unsigned-byte 16) . simple-array-unsigned-byte-16)
    ((unsigned-byte 32) . simple-array-unsigned-byte-32)
    (single-float . simple-array-single-float)
    (double-float . simple-array-double-float)
    (t . simple-vector))
  "An a-list for mapping simple array element types to their
  corresponding primitive types.")

;;;
;;; Return the primitive type corresponding to a type descriptor
;;; structure. The second value is true when the primitive type is
;;; exactly equivalent to the argument Lisp type.
;;;
;;; In a bootstrapping situation, we should be careful to use the
;;; correct values for the system parameters.
;;;
(defun primitive-type (type)
  (declare (type ctype type))
  (macrolet ((any () '(values *any-primitive-type* nil))
	     (exactly (type) `(values (primitive-type-or-lose ',type) t))
	     (part-of (type) `(values (primitive-type-or-lose ',type) nil)))
    (etypecase type
      (numeric-type
       (let ((lo (numeric-type-low type))
	     (hi (numeric-type-high type)))
	 (case (numeric-type-complexp type)
	   (:real
	    (case (numeric-type-class type)
	      (integer
	       (cond ((and hi lo
			   (>= lo most-negative-fixnum)
			   (<= hi most-positive-fixnum))
		      (values (primitive-type-or-lose 'fixnum)
			      (and (= lo most-negative-fixnum)
				   (= hi most-positive-fixnum))))
		     ((or (and hi (< hi most-negative-fixnum))
			  (and lo (> lo most-positive-fixnum)))
		      (part-of bignum))
		     (t
		      (any))))
	      (float
	       (let ((exact (and (null lo) (null hi))))
		 (case (numeric-type-format type)
		   ((short-float single-float)
		    (values (primitive-type-or-lose 'single-float) exact))
		   ((double-float long-float)
		    (values (primitive-type-or-lose 'double-float) exact))
		   (t
		    (any)))))
	      (t
	       (any))))
	   (:complex
	    (part-of complex))
	   (t
	    (any)))))
      (array-type
       (if (array-type-complexp type)
	   (any)
	   (let* ((dims (array-type-dimensions type))
		  (etype (array-type-specialized-element-type type))
		  (type-spec (type-specifier etype))
		  (ptype (cdr (assoc type-spec *simple-array-primitive-types*
				     :test #'equal))))
	     (if (and (consp dims) (null (rest dims)) ptype)
		 (values (primitive-type-or-lose ptype) (eq (first dims) '*))
		 (any)))))
      (union-type
       (if (type= type (specifier-type 'list))
	   (exactly list)
	   (let ((types (union-type-types type)))
	     (multiple-value-bind (res exact)
				  (primitive-type (first types))
	       (dolist (type (rest types) (values res exact))
		 (multiple-value-bind (ptype ptype-exact)
				      (primitive-type type)
		   (unless ptype-exact (setq exact nil))
		   (unless (eq ptype res)
		     (return (any)))))))))
      (member-type
       (let* ((members (member-type-members type))
	      (res (primitive-type-of (first members))))
	 (dolist (mem (rest members) (values res nil))
	   (unless (eq (primitive-type-of mem) res)
	     (return (values *any-primitive-type* nil))))))
      (named-type
       (case (named-type-name type)
	 ((t bignum ratio complex function system-area-pointer)
	  (values (primitive-type-or-lose (named-type-name type)) t))
	 ((character base-character string-char)
	  (exactly base-character))
	 (standard-char
	  (part-of base-character))
	 (cons
	  (part-of list))
	 (t
	  (any))))
      (ctype
       (any)))))


;;;; Magical Registers

(eval-when (compile eval load)
  (defconstant zero-offset 0)
  (defconstant lip-offset 1)
  (defconstant null-offset 20)
  (defconstant bsp-offset 21)
  (defconstant fp-offset 22)
  (defconstant csp-offset 23)
  (defconstant flags-offset 24)
  (defconstant alloc-offset 25)
  (defconstant nsp-offset 29)
  (defconstant code-offset 30))

;;; 
;;; Wired Zero
(defparameter zero-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset zero-offset))

;;; 
;;; Lisp-interior-pointer register.
(defparameter lip-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset lip-offset))

;;;
;;; ``Wired'' NIL
(defparameter null-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset null-offset))

;;; 
;;; Binding stack pointer
(defparameter bsp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset bsp-offset))

;;;
;;; Frame Pointer
(defparameter fp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset fp-offset))

;;; 
;;; Control stack pointer
(defparameter csp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset csp-offset))

;;;
;;; FLAGS magic register
(defparameter flags-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset flags-offset))

;;; 
;;; Allocation pointer
(defparameter alloc-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset alloc-offset))
;;; 
;;; Number stack pointer
(defparameter nsp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset nsp-offset))

;;; 
;;; Code Pointer
(defparameter code-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset code-offset))

;;;
;;; Global Pointer (for C call-out)
(defparameter gp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset 29))



;;;; Side-Effect Classes

(def-boolean-attribute vop
  any)


;;;; Constants

;;;
;;; Immediate-Constant-SC  --  Interface
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(defun immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero))
    (null
     (sc-number-or-lose 'null))
    ((integer #x-1FFF #x-0001)
     (sc-number-or-lose 'negative-immediate))
    ((integer 0 #x1FFE)
     (sc-number-or-lose 'immediate))
    ((integer #x1FFF #x3FFE)
     (sc-number-or-lose 'unsigned-immediate))
    (symbol
     (if (vm:static-symbol-p value)
	 (sc-number-or-lose 'random-immediate)
	 nil))
    ((signed-byte 29) #+nil (or (signed-byte 32) (unsigned-byte 32))
     (sc-number-or-lose 'random-immediate))
    #+new-compiler
    (system-area-pointer
     (sc-number-or-lose 'immediate-sap))
    (character
     (if (string-char-p value)
	 (sc-number-or-lose 'immediate-base-character)
	 nil))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (sc-number-or-lose 'descriptor-reg))
(defconstant control-stack-arg-scn (sc-number-or-lose 'control-stack))

(eval-when (compile load eval)

;;; Offset of special registers used during calls
;;;
(defconstant nargs-offset 7)
(defconstant cname-offset 14)
(defconstant lexenv-offset 15)
(defconstant args-offset 16)
(defconstant old-fp-offset 17)
(defconstant lra-offset 18)

;;; A few additional registers distinct from all the linkage regs.  These are
;;; needed by copy-more-args.
;;;
(defconstant nl0-offset 2)
(defconstant nl1-offset 3)
(defconstant nl2-offset 4)
(defconstant nl3-offset 5)
(defconstant l0-offset 19)

;;; Offsets of special stack frame locations
(defconstant old-fp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

); Eval-When (Compile Load Eval)  


(defparameter nargs-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset nargs-offset))

(defparameter args-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset args-offset))

(defparameter old-fp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset old-fp-offset))

(defparameter lra-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset lra-offset))


(eval-when (compile load eval)

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 6)

;;; The offsets within the register-arg SC that we pass values in, first
;;; value first.
;;;
(defconstant register-arg-offsets '(8 9 10 11 12 13))

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
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
	(offset (tn-offset tn)))
    (ecase sb
      (registers (mips:register-name (tn-offset tn)))
      (float-registers (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))
