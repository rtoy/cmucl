;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pred.lisp,v 1.28 1992/12/05 22:10:02 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Predicate functions for CMU Common Lisp.
;;;
;;; Written by William Lott.
;;;

(in-package "EXTENSIONS")
(export '(structurep fixnump bignump bitp ratiop weak-pointer-p))

(in-package "SYSTEM")
(export '(system-area-pointer system-area-pointer-p))

(in-package "LISP" :use "KERNEL")

(export '(typep null symbolp atom consp listp numberp integerp rationalp
	  floatp complexp characterp stringp bit-vector-p vectorp
	  simple-vector-p simple-string-p simple-bit-vector-p arrayp
	  functionp compiled-function-p commonp eq eql equal equalp not
	  type-of upgraded-array-element-type realp
	  ;; Names of types...
	  array atom bignum bit bit-vector character common
	  compiled-function complex cons double-float
	  fixnum float function integer keyword list long-float nil
	  null number ratio rational real sequence short-float signed-byte
	  simple-array simple-bit-vector simple-string simple-vector
	  single-float standard-char string string-char symbol t
	  unsigned-byte vector structure satisfies))



;;;; Primitive predicates.  These must be supported by the compiler.

(eval-when (compile eval)
  (defparameter primitive-predicates
    '(array-header-p
      arrayp
      atom
      base-char-p
      bignump
      bit-vector-p
      characterp
      code-component-p
      consp
      compiled-function-p
      complexp
      double-float-p
      fdefn-p
      fixnump
      floatp
      functionp
      integerp
      listp
      long-float-p
      lra-p
      not
      null
      numberp
      rationalp
      ratiop
      realp
      scavenger-hook-p
      short-float-p
      simple-array-p
      simple-bit-vector-p
      simple-string-p
      simple-vector-p
      single-float-p
      stringp
      structurep
      symbolp
      system-area-pointer-p
      weak-pointer-p
      vectorp
      unsigned-byte-32-p
      signed-byte-32-p
      simple-array-unsigned-byte-2-p
      simple-array-unsigned-byte-4-p
      simple-array-unsigned-byte-8-p
      simple-array-unsigned-byte-16-p
      simple-array-unsigned-byte-32-p
      simple-array-single-float-p
      simple-array-double-float-p
      dylan::dylan-function-p
      )))

(macrolet
    ((frob ()
       `(progn
	  ,@(mapcar #'(lambda (pred)
			`(defun ,pred (object)
			   ,(format nil
				    "Return T if OBJECT is a~:[~;n~] ~(~A~) ~
				     and NIL otherwise."
				    (find (schar (string pred) 0) "AEIOUaeiou")
				    (string pred))
			   (,pred object)))
		    primitive-predicates))))
  (frob))


;;;; TYPE-OF -- public.
;;;
;;; Return the specifier for the type of object.  This is not simply
;;; (type-specifier (ctype-of object)) because ctype-of has different goals
;;; than type-of.
;;; 
(defun type-of (object)
  "Return the type of OBJECT."
  (typecase object
    ;; First the ones that we can tell by testing the lowtag
    (fixnum 'fixnum)
    (function (type-specifier (ctype-of object)))
    (null 'null)
    (list 'cons)

    ;; Any other immediates.
    (character
     (typecase object
       (standard-char 'standard-char)
       (base-char 'base-char)
       (t 'character)))

    ;; And now for the complicated ones.
    (number
     (etypecase object
       (fixnum 'fixnum)
       (bignum 'bignum)
       (float
	(etypecase object
	  (double-float 'double-float)
	  (single-float 'single-float)
	  (short-float 'short-float)
	  (long-float 'long-float)))
       (ratio 'ratio)
       (complex 'complex)))
    (symbol
     (if (eq (symbol-package object)
	     (symbol-package :foo))
	 'keyword
	 'symbol))
    (structure
     (let ((name (structure-ref object 0)))
       (case name
	 (alien-internals:alien-value
	  `(alien:alien
	    ,(alien-internals:unparse-alien-type
	      (alien-internals:alien-value-type object))))
	 (t name))))
    (array (type-specifier (ctype-of object)))
    (system-area-pointer 'system-area-pointer)
    (weak-pointer 'weak-pointer)
    (code-component 'code-component)
    (lra 'lra)
    (fdefn 'fdefn)
    (scavenger-hook 'scavenger-hook)
    (t
     (warn "Can't figure out the type of ~S" object)
     t)))

;;;; UPGRADED-ARRAY-ELEMENT-TYPE  --  public
;;;
(defun upgraded-array-element-type (spec)
  "Return the element type that will actually be used to implement an array
   with the specifier :ELEMENT-TYPE Spec."
  (type-specifier
   (array-type-specialized-element-type
    (specifier-type `(array ,spec)))))

;;;; SUBTYPEP -- public.
;;;
;;; Just parse the type specifiers and call csubtype.
;;; 
(defun subtypep (type1 type2)
  "Return two values indicating the relationship between type1 and type2:
  T and T: type1 definatly is a subtype of type2.
  NIL and T: type1 definatly is not a subtype of type2.
  NIL and NIL: who knows?"
  (csubtypep (specifier-type type1) (specifier-type type2)))


;;;; TYPEP -- public.
;;;
;;; Just call %typep
;;; 
(defun typep (object type)
  "Return T iff OBJECT is of type TYPE."
  (declare (type (or list symbol) type))
  (%typep object type))

(eval-when (compile eval)
  (defmacro only-if-bound (name object)
    `(and (fboundp ',name)
	  (let ((object ,object))
	    (declare (optimize (inhibit-warnings 3)))
	    (,name object)))))
  
;;; %TYPEP -- internal.
;;;
;;; The actual typep engine.  The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
;;; 
(defun %typep (object specifier)
  (%%typep object
	   (if (ctype-p specifier)
	       specifier
	       (specifier-type specifier))))
;;;
(defun %%typep (object type)
  (declare (type ctype type))
  (etypecase type
    (named-type
     (case (named-type-name type)
       ((* t)
	t)
       ((nil)
	nil)
       (character (characterp object))
       (base-char (base-char-p object))
       (standard-char (and (characterp object) (standard-char-p object)))
       (extended-char
	(and (characterp object) (not (base-char-p object))))
       (function (functionp object))
       (cons (consp object))
       (symbol (symbolp object))
       (keyword
	(and (symbolp object)
	     (eq (symbol-package object)
		 (symbol-package :foo))))
       (system-area-pointer (system-area-pointer-p object))
       (weak-pointer (weak-pointer-p object))
       (code-component (code-component-p object))
       (lra (lra-p object))
       (fdefn (fdefn-p object))
       (scavenger-hook (scavenger-hook-p object))
       (structure (structurep object))
       (dylan::dylan-function (dylan::dylan-function-p object))
       (dylan::generic-function
	(only-if-bound dylan::generic-function-p object))
       (dylan::exit-function
	(only-if-bound dylan::exit-function-p object))
       (dylan::next-method-func
	(only-if-bound dylan::next-method-func-p object))
       (dylan::method
	(only-if-bound dylan::method-p object))
       (dylan::defined-method
	(only-if-bound dylan::defined-method-p object))
       (dylan::builtin-method
	(only-if-bound dylan::builtin-method-p object))
       (dylan::slot-accessor-method
	(only-if-bound dylan::slot-accessor-method-p object))
       (dylan::slot-setter-method
	(only-if-bound dylan::slot-setter-method-p object))
       (dylan::slot-getter-method
	(only-if-bound dylan::slot-getter-method-p object))
       (t nil)))
    (numeric-type
     (and (numberp object)
	  (let ((num (if (complexp object) (realpart object) object)))
	    (ecase (numeric-type-class type)
	      (integer (integerp num))
	      (rational (rationalp num))
	      (float
	       (ecase (numeric-type-format type)
		 (short-float (typep object 'short-float))
		 (single-float (typep object 'single-float))
		 (double-float (typep object 'double-float))
		 (long-float (typep object 'long-float))
		 ((nil) (floatp num))))
	      ((nil) t)))
	  (flet ((bound-test (val)
			     (let ((low (numeric-type-low type))
				   (high (numeric-type-high type)))
			       (and (cond ((null low) t)
					  ((listp low) (> val (car low)))
					  (t (>= val low)))
				    (cond ((null high) t)
					  ((listp high) (< val (car high)))
					  (t (<= val high)))))))
	    (ecase (numeric-type-complexp type)
	      ((nil) t)
	      (:complex
	       (and (complexp object)
		    (bound-test (realpart object))
		    (bound-test (imagpart object))))
	      (:real
	       (and (not (complexp object))
		    (bound-test object)))))))
    (array-type
     (and (arrayp object)
	  (ecase (array-type-complexp type)
	    ((t) (not (typep object 'simple-array)))
	    ((nil) (typep object 'simple-array))
	    (* t))
	  (or (eq (array-type-dimensions type) '*)
	      (do ((want (array-type-dimensions type) (cdr want))
		   (got (array-dimensions object) (cdr got)))
		  ((and (null want) (null got)) t)
		(unless (and want got
			     (or (eq (car want) '*)
				 (= (car want) (car got))))
		  (return nil))))
	  (or (eq (array-type-element-type type) *wild-type*)
	      (type= (array-type-specialized-element-type type)
		     (specifier-type (array-element-type object))))))
    (member-type
     (if (member object (member-type-members type)) t))
    (structure-type
     (structure-typep object (structure-type-name type)))
    (union-type
     (dolist (type (union-type-types type))
       (when (%%typep object type)
	 (return t))))
    (unknown-type
     ;; Type may be unknown to the compiler (and SPECIFIER-TYPE), yet be
     ;; a defined structure in the core.
     (let ((orig-spec (unknown-type-specifier type)))
       (if (and (symbolp orig-spec)
		(info type defined-structure-info orig-spec))
	   (structure-typep object orig-spec)
	   (error "Unknown type specifier: ~S" orig-spec))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (or (atom hairy-spec)
	      (dolist (spec (cdr hairy-spec) t)
		(unless (%%typep object (specifier-type spec))
		  (return nil)))))
	 (not
	  (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
	    (error "Invalid type specifier: ~S" hairy-spec))
	  (not (%%typep object (specifier-type (cadr hairy-spec)))))
	 (satisfies
	  (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
	    (error "Invalid type specifier: ~S" hairy-spec))
	  (let ((fn (cadr hairy-spec)))
	    (if (funcall (typecase fn
			   (function fn)
			   (symbol (symbol-function fn))
			   (t
			    (coerce fn 'function)))
			 object)
		t
		nil))))))
    (alien-type-type
     (alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (function-type
     (error "Function types are not a legal argument to TYPEP:~%  ~S"
	    (type-specifier type)))))



;;; Structure-Typep  --  Internal
;;;
;;; This is called by %typep when it tries to match against a structure type,
;;; and typep of types that are known to be structure types at compile time
;;; are converted to this.
;;;
(defun structure-typep (object type)
  (declare (optimize speed))
  (let ((info (info type defined-structure-info type)))
    (if info
	(and (structurep object)
	     (let ((obj-name (structure-ref object 0)))
	       (or (eq obj-name type)
		   (if (member obj-name (c::dd-included-by info)
			       :test #'eq)
		       t nil))))
	(error "~S is an unknown structure type specifier." type))))


;;;; Equality predicates.

;;; EQ -- public.
;;;
;;; Real simple, 'cause the compiler takes care of it.
;;; 

(defun eq (obj1 obj2)
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq obj1 obj2))


;;; EQUAL -- public.
;;;
(defun equal (x y)
  "Returns T if X and Y are EQL or if they are structured components
  whose elements are EQUAL.  Strings and bit-vectors are EQUAL if they
  are the same length and have indentical components.  Other arrays must be
  EQ to be EQUAL."
  (cond ((eql x y) t)
	((consp x)
	 (and (consp y)
	      (equal (car x) (car y))
	      (equal (cdr x) (cdr y))))
	((stringp x)
	 (and (stringp y) (string= x y)))
	((pathnamep x)
	 (and (pathnamep y) (pathname= x y)))
	((bit-vector-p x)
	 (and (bit-vector-p y)
	      (= (the fixnum (length x))
		 (the fixnum (length y)))
	      (do ((i 0 (1+ i))
		   (length (length x)))
		  ((= i length) t)
		(declare (fixnum i))
		(or (= (the fixnum (bit x i))
		       (the fixnum (bit y i)))
		    (return nil)))))
	(t nil)))

;;; EQUALP -- public.
;;; 
(defun equalp (x y)
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion.  Characters may differ in alphabetic case.  Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction."
  (cond ((eq x y) t)
	((characterp x) (char-equal x y))
	((numberp x) (and (numberp y) (= x y)))
	((consp x)
	 (and (consp y)
	      (equalp (car x) (car y))
	      (equalp (cdr x) (cdr y))))
	((pathnamep x)
	 (and (pathnamep y) (pathname= x y)))
	((structurep x)
	 (let ((length (structure-length x)))
	   (and (structurep y)
		(= length (structure-length y))
		(dotimes (i length t)
		  (let ((x-el (structure-ref x i))
			(y-el (structure-ref y i)))
		    (unless (or (eq x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((vectorp x)
	 (let ((length (length x)))
	   (and (vectorp y)
		(= length (length y))
		(dotimes (i length t)
		  (let ((x-el (aref x i))
			(y-el (aref y i)))
		    (unless (or (eq x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((arrayp x)
	 (and (arrayp y)
	      (= (array-rank x) (array-rank y))
	      (dotimes (axis (array-rank x) t)
		(unless (= (array-dimension x axis)
			   (array-dimension y axis))
		  (return nil)))
	      (dotimes (index (array-total-size x) t)
		(let ((x-el (row-major-aref x index))
		      (y-el (row-major-aref y index)))
		  (unless (or (eq x-el y-el)
			      (equalp x-el y-el))
		    (return nil))))))
	(t nil)))
