;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/vm.lisp,v 1.21 1990/04/13 17:15:19 wlott Exp $
;;;
;;; This file contains the VM definition for the MIPS R2000 and the new
;;; object format.
;;;
;;; Written by Christopher Hoover and William Lott.
;;;
(in-package "C")


;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base number-stack :unbounded :size 8)
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
  ;; Objects that can be stored in any register (immediate objects)
  (any-reg registers
   :locations (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 28 31))

  ;; Objects that must be seen by GC (pointer objects)
  (descriptor-reg registers
   :locations (8 9 10 11 12 13 14 15 16 17 18 19 28 31))

  ;; Objects that must not be seen by GC (unboxed objects)
  (non-descriptor-reg registers
   :locations (2 3 4 5 6 7))

  ;; Pointers to the interior of objects.
  (interior-reg registers
   :locations (1))

  ;; Unboxed base-characters
  (base-character-reg registers
   :locations (2 3 4 5 6 7))

  ;; Unboxed SAP's (arbitrary pointers into address space)
  (sap-reg registers
   :locations (2 3 4 5 6 7))

  ;; Stack for descriptor objects (scanned by GC)
  (control-stack control-stack)

  ;; Stack for non-descriptor objects (not scanned by GC)
  (number-stack number-stack)

  ;; Unboxed base-character stack
  (base-character-stack number-stack)

  ;; Unboxed SAP stack
  (sap-stack number-stack)

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

  ;; Immediate null/nil.
  (null immediate-constant)

  ;; Immediate unboxed base-characters.
  (immediate-base-character immediate-constant)

  ;; Immediate unboxed SAP's.
  (immediate-sap immediate-constant)

  ;; Objects that are easier to create using immediate loads than to fetch
  ;; from the constant pool, but which aren't directly usable as immediate
  ;; operands.  These are only recognized by move VOPs.
  (random-immediate immediate-constant)

  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size))



;;;; Interfaces for stack sizes.

(defun current-frame-size ()
  (values (* vm:word-bytes
	     (finite-sb-current-size (sb-or-lose 'control-stack)))
	  (* vm:word-bytes
	     (finite-sb-current-size (sb-or-lose 'number-stack)))))



;;;; Move costs.

;;;
;;; Move costs for operand loading and storing
;;;
#+nil ;; These move costs don't work.
(define-move-costs
  ((immediate zero null random-immediate)
   ;; load immediate or reg->reg move.
   (1 any-reg descriptor-reg))

  ((immediate-base-character)
   ;; Load immediate.
   (1 base-character-reg any-reg descriptor-reg))

  ((immediate-sap)
   ;; lui/ori pair.
   (2 sap-reg))

  ((any-reg)
   ;; No conversion necessary for these.
   (1 any-reg descriptor-reg)
   ;; Must shift and addiu the type code.
   (2 base-character-reg)
   ;; No type conversion, but we have to write it on the stack.
   (5 control-stack))

  ((descriptor-reg)
   ;; No conversion necessary for these.
   (1 any-reg descriptor-reg)
   ;; Must shift and addiu the type code.
   (2 base-character-reg)
   ;; Must indirect the src ptr.
   (5 sap-reg)
   ;; No type conversion, but we have to write it on the stack.
   (5 control-stack))

  ((base-character-reg)
   ;; Just move.
   (1 base-character-reg)
   ;; Must add type info.
   (2 any-reg descriptor-reg)
   ;; Must store it.
   (5 base-character-stack))

  ((sap-reg)
   ;; Just move
   (1 sap-reg)
   ;; Must allocate space for it.
   (10 descriptor-reg)
   ;; Must store it.
   (5 sap-stack))

  ((control-stack constant)
   ;; Must indirect the stack/code pointer.
   (5 any-reg descriptor-reg))

  ((base-character-stack)
   ;; Just indirect.
   (5 base-character-reg))

  ((sap-stack)
   ;; Just indirect.
   (5 sap-reg)))

(define-move-costs
  ((any-reg descriptor-reg non-descriptor-reg)
   (1 any-reg descriptor-reg non-descriptor-reg)
   (2 base-character-reg sap-reg)
   (5 control-stack number-stack))

  ((control-stack number-stack constant)
   (5 any-reg descriptor-reg non-descriptor-reg)
   (6 base-character-reg sap-reg))

  ((immediate zero null random-immediate)
   (1 any-reg descriptor-reg non-descriptor-reg))

  ((immediate-base-character)
   (1 base-character-reg)
   (2 any-reg descriptor-reg non-descriptor-reg))

  ((immediate-sap)
   (1 sap-reg)
   (2 any-reg descriptor-reg non-descriptor-reg))

  ((base-character-reg)
   (1 base-character-reg)
   (2 any-reg descriptor-reg non-descriptor-reg)
   (5 base-character-stack)
   (6 control-stack number-stack))

  ((sap-reg)
   (1 sap-reg)
   (2 any-reg descriptor-reg non-descriptor-reg)
   (5 sap-stack)
   (6 control-stack number-stack))

  ((base-character-stack)
   (5 base-character-reg))

  ((sap-stack)
   (5 sap-reg)))


;;;
;;; SCs which must saved on a function call.
(define-save-scs
  (control-stack any-reg descriptor-reg)
  (base-character-stack base-character-reg)
  (sap-stack sap-reg))


;;;; Primitive Type Definitions

(def-primitive-type t (descriptor-reg control-stack))
(defvar *any-primitive-type* (primitive-type-or-lose 't))

;;; 
(def-primitive-type fixnum (any-reg control-stack))

(def-primitive-type base-character
  (base-character-reg any-reg base-character-stack control-stack))

;;; 
(def-primitive-type function (descriptor-reg control-stack))
(def-primitive-type list (descriptor-reg control-stack))

;;;
(def-primitive-type bignum (descriptor-reg control-stack))
(def-primitive-type ratio (descriptor-reg control-stack))
(def-primitive-type complex (descriptor-reg control-stack))
(def-primitive-type single-float (descriptor-reg control-stack))
(def-primitive-type double-float (descriptor-reg control-stack))

;;;
(def-primitive-type simple-string (descriptor-reg control-stack)
  :type simple-base-string)
(def-primitive-type simple-bit-vector (descriptor-reg control-stack))
(def-primitive-type simple-vector (descriptor-reg control-stack))
(def-primitive-type simple-array-unsigned-byte-2 (descriptor-reg control-stack)
  :type (simple-array (unsigned-byte 2) (*)))
(def-primitive-type simple-array-unsigned-byte-4 (descriptor-reg control-stack)
  :type (simple-array (unsigned-byte 4) (*)))
(def-primitive-type simple-array-unsigned-byte-8 (descriptor-reg control-stack)
  :type (simple-array (unsigned-byte 8) (*)))
(def-primitive-type simple-array-unsigned-byte-16 (descriptor-reg control-stack)
  :type (simple-array (unsigned-byte 16) (*)))
(def-primitive-type simple-array-unsigned-byte-32 (descriptor-reg control-stack)
  :type (simple-array (unsigned-byte 32) (*)))
(def-primitive-type simple-array-single-float (descriptor-reg control-stack)
  :type (simple-array single-float (*)))
(def-primitive-type simple-array-double-float (descriptor-reg control-stack)
  :type (simple-array double-float (*)))

(def-primitive-type system-area-pointer (sap-reg sap-stack))

(def-primitive-type random (non-descriptor-reg) :type nil)
(def-primitive-type interior (interior-reg) :type nil)
(def-primitive-type catch-block (catch-block) :type nil)


;;;
#|  These are not needed 'cause its pointless to restrict VOPs to them.
(def-primitive complex-string (descriptor-reg control-stack))
(def-primitive complex-bit-vector (descriptor-reg control-stack))
(def-primitive complex-vector (descriptor-reg control-stack))
|#


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
  (etypecase type
    (numeric-type
     (if (not (eq (numeric-type-complexp type) :real))
	 (values *any-primitive-type* nil)
	 (case (numeric-type-class type)
	   (integer
	    (let ((lo (numeric-type-low type))
		  (hi (numeric-type-high type)))
	      (if (and hi lo
		       (>= lo most-negative-fixnum)
		       (<= hi most-positive-fixnum))
		  (values (primitive-type-or-lose 'fixnum)
			  (and (= lo most-negative-fixnum)
			       (= hi most-positive-fixnum)))
		  (values *any-primitive-type* nil))))
	   (t
	    (values *any-primitive-type* nil)))))
    (array-type
       (if (array-type-complexp type)
	   (values *any-primitive-type* nil)
	   (let* ((dims (array-type-dimensions type))
		  (etype (array-type-specialized-element-type type))
		  (type-spec (type-specifier etype))
		  (ptype (cdr (assoc type-spec *simple-array-primitive-types*
				     :test #'equal))))
	     (if (and (consp dims) (null (rest dims)) ptype)
		 (values (primitive-type-or-lose ptype) (eq (first dims) '*))
		 (values *any-primitive-type* nil)))))
    (union-type
     (if (type= type (specifier-type 'list))
	 (values (primitive-type-or-lose 'list) t)
	 (let ((types (union-type-types type)))
	   (multiple-value-bind (res exact)
				(primitive-type (first types))
	     (dolist (type (rest types))
	       (multiple-value-bind (ptype ptype-exact)
				    (primitive-type type)
		 (unless ptype-exact (setq exact nil))
		 (setq res (primitive-type-union res ptype))))
	     (values res exact)))))
    (member-type
     (values (reduce #'primitive-type-union
		     (mapcar #'primitive-type-of 
			     (member-type-members type)))
	     nil))
    (named-type
     (case (named-type-name type)
       ((t bignum ratio complex function system-area-pointer)
	(values (primitive-type-or-lose (named-type-name type)) t))
       ((character base-character string-char)
	(values (primitive-type-or-lose 'base-character) t))
       (standard-char
	(values (primitive-type-or-lose 'base-character) nil))
       (cons
	(values (primitive-type-or-lose 'list) nil))
       (t
	(values *any-primitive-type* nil))))
    (ctype
     (values *any-primitive-type* nil))))


;;;; Magical Registers

(eval-when (compile eval load)
  (defconstant zero-offset 0)
  (defconstant lip-offset 1)
  (defconstant null-offset 20)
  (defconstant bsp-offset 21)
  (defconstant cont-offset 22)
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
(defparameter cont-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset cont-offset))

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
    (fixnum
     (sc-number-or-lose 'random-immediate))
    ;; ### what here?
    ;(sap
    ; (sc-number-or-lose 'immediate-sap))
    (t
     ;;
     ;; ### hack around bug in (typep x 'string-char)
     (if (and (characterp value) (string-char-p value))
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
(defconstant oldcont-offset 17)
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
(defconstant oldcont-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant lexenv-save-offset 2)

); Eval-When (Compile Load Eval)  


(defparameter nargs-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset nargs-offset))

(defparameter args-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset args-offset))

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
