;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/vm.lisp,v 1.35 1990/10/23 02:23:46 wlott Exp $
;;;
;;; This file contains the VM definition for the MIPS R2000 and the new
;;; object format.
;;;
;;; Written by Christopher Hoover and William Lott.
;;;
(in-package "C")


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




;;;; Primitive Type Definitions

;;; *Anything*
;;; 
(def-primitive-type t (descriptor-reg))
(defvar *any-primitive-type* (primitive-type-or-lose 't))

;;; Primitive integer types that fit in registers.
;;;
(def-primitive-type positive-fixnum (any-reg signed-reg unsigned-reg)
  :type (unsigned-byte 29))
(def-primitive-type unsigned-byte-31 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 31))
(def-primitive-type unsigned-byte-32 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 32))
(def-primitive-type fixnum (any-reg signed-reg)
  :type (signed-byte 30))
(def-primitive-type signed-byte-32 (signed-reg descriptor-reg)
  :type (signed-byte 32))

(defvar *fixnum-primitive-type* (primitive-type-or-lose 'fixnum))

(def-primitive-type-alias tagged-num (:or positive-fixnum fixnum))
(def-primitive-type-alias unsigned-num (:or unsigned-byte-32
					    unsigned-byte-31
					    positive-fixnum))
(def-primitive-type-alias signed-num (:or signed-byte-32
					  fixnum
					  unsigned-byte-31
					  positive-fixnum))

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
(def-primitive-type weak-pointer (descriptor-reg))

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
(defun-cached (primitive-type
	       :hash-function (lambda (x)
				(logand (cache-hash-eq x) #x1FF))
	       :hash-bits 9
	       :values 2
	       :default (values nil :empty))
	      ((type eq))
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
	       (cond ((and hi lo)
		      (dolist (spec
			       '((positive-fixnum 0 #.(1- (ash 1 29)))
				 (unsigned-byte-31 0 #.(1- (ash 1 31)))
				 (unsigned-byte-32 0 #.(1- (ash 1 32)))
				 (fixnum #.(ash -1 29) #.(1- (ash 1 29)))
				 (signed-byte-32 #.(ash -1 31)
						 #.(1- (ash 1 31))))
			       (if (or (< hi (ash -1 29))
				       (> lo (1- (ash 1 29))))
				   (part-of bignum)
				   (any)))
			(let ((type (car spec))
			      (min (cadr spec))
			      (max (caddr spec)))
			  (when (<= min lo hi max)
			    (return (values (primitive-type-or-lose type)
					    (and (= lo min) (= hi max))))))))
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
	 ((t bignum ratio complex function structure
	     system-area-pointer weak-pointer)
	  (values (primitive-type-or-lose (named-type-name type)) t))
	 ((character base-character string-char)
	  (exactly base-character))
	 (standard-char
	  (part-of base-character))
	 (cons
	  (part-of list))
	 (t
	  (any))))
      (function-type
       (exactly function))
      (structure-type
       (part-of structure))
      (ctype
       (any)))))


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
    (#-new-compiler (signed-byte 30)
     #+new-compiler fixnum
     (sc-number-or-lose 'random-immediate))
    #+new-compiler
    (system-area-pointer
     (sc-number-or-lose 'immediate-sap))
    (character
     #-new-compiler
     (if (string-char-p value)
	 (sc-number-or-lose 'immediate-base-character)
	 nil)
     #+new-compiler
     (sc-number-or-lose 'immediate-base-character))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (sc-number-or-lose 'control-stack))

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
(defun location-print-name (tn)
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
