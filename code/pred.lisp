;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pred.lisp,v 1.42 1997/11/15 04:38:50 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Predicate functions for CMU Common Lisp.
;;;
;;; Written by William Lott.
;;;

(in-package "KERNEL")
(export '(%instancep instance fixnump bignump bitp ratiop weak-pointer-p
		     %typep class-cell-typep))

(in-package "SYSTEM")
(export '(system-area-pointer system-area-pointer-p))

(in-package "LISP")

(export '(typep null symbolp atom consp listp numberp integerp rationalp
	  floatp complexp characterp stringp bit-vector-p vectorp
	  simple-vector-p simple-string-p simple-bit-vector-p arrayp
	  functionp compiled-function-p eq eql equal equalp not
	  type-of upgraded-array-element-type realp
	  ;; Names of types...
	  array atom bignum bit bit-vector character common
	  compiled-function complex cons double-float
	  fixnum float function integer keyword list long-float nil
	  null number ratio rational real sequence short-float signed-byte
	  simple-array simple-bit-vector simple-string simple-vector
	  single-float standard-char string string-char symbol t
	  unsigned-byte vector satisfies))



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
      #+complex-float complex-rational-p
      #+complex-float complex-float-p
      #+complex-float complex-single-float-p
      #+complex-float complex-double-float-p
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
      %instancep
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
      #+signed-array simple-array-signed-byte-8-p
      #+signed-array simple-array-signed-byte-16-p
      #+signed-array simple-array-signed-byte-30-p
      #+signed-array simple-array-signed-byte-32-p
      simple-array-single-float-p
      simple-array-double-float-p
      #+complex-float simple-array-complex-single-float-p
      #+complex-float simple-array-complex-double-float-p
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
;;; than type-of.  In particular, speed is more important than precision, and
;;; it is not permitted to return member types.
;;; 
(defun type-of (object)
  "Return the type of OBJECT."
  (if (typep object '(or function array))
      (type-specifier (ctype-of object))
      (let* ((class (layout-class (layout-of object)))
	     (name (class-name class)))
	(if (%instancep object)
	    (case name
	      (alien-internals:alien-value
	       `(alien:alien
		 ,(alien-internals:unparse-alien-type
		   (alien-internals:alien-value-type object))))
	      (t
	       (class-proper-name class)))
	    name))))


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
  T and T: type1 definitely is a subtype of type2.
  NIL and T: type1 definitely is not a subtype of type2.
  NIL and NIL: who knows?"
  (csubtypep (specifier-type type1) (specifier-type type2)))


;;;; TYPEP:

(declaim (start-block typep %typep class-cell-typep))

;;; TYPEP -- public.
;;;
;;; Just call %typep
;;; 
(defun typep (object type)
  "Return T iff OBJECT is of type TYPE."
  (%typep object type))

  
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
     (ecase (named-type-name type)
       ((* t) t)
       ((nil) nil)))
    (numeric-type
     (and (numberp object)
	  (let ((num (if (complexp object) (realpart object) object)))
	    (ecase (numeric-type-class type)
	      (integer (integerp num))
	      (rational (rationalp num))
	      (float
	       (ecase (numeric-type-format type)
		 (short-float (typep num 'short-float))
		 (single-float (typep num 'single-float))
		 (double-float (typep num 'double-float))
		 (long-float (typep num 'long-float))
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
    (class
     (class-typep (layout-of object) type object))
    (union-type
     (dolist (type (union-type-types type))
       (when (%%typep object type)
	 (return t))))
    (unknown-type
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
	   (error "Unknown type specifier: ~S"
		  (unknown-type-specifier reparse))
	   (%%typep object reparse))))
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


;;; CLASS-CELL-TYPEP  --  Interface
;;;
;;;    Do type test from a class cell, allowing forward reference and
;;; redefinition.
;;;
;;; 2-Feb-97 add third arg optional for back compatibility and boot
(defun class-cell-typep (obj-layout cell &optional object)
  (let ((class (class-cell-class cell)))
    (unless class
      (error "Class has not yet been defined: ~S" (class-cell-name cell)))
    (class-typep obj-layout class object)))


;;; CLASS-TYPEP  --  Internal
;;;
;;;    Test whether Obj-Layout is from an instance of Class.
;;;
(defun class-typep (obj-layout class object)
  (declare (optimize speed))
  (when (layout-invalid obj-layout)
    (if (and (typep (class-of object) 'standard-class) object)
	(setq obj-layout (pcl::check-wrapper-validity object))
	(error "TYPEP on obsolete object (was class ~S)."
	       (class-proper-name (layout-class obj-layout)))))
  (let ((layout (class-layout class))
	(obj-inherits (layout-inherits obj-layout)))
    (when (layout-invalid layout)
      (error "Class is currently invalid: ~S" class))
    (or (eq obj-layout layout)
	(dotimes (i (length obj-inherits) nil)
	  (when (eq (svref obj-inherits i) layout)
	    (return t))))))

(declaim (end-block))


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
	((%instancep x)
	 (let* ((layout-x (%instance-layout x))
		(len (layout-length layout-x)))
	   (and (%instancep y)
		(eq layout-x (%instance-layout y))
		(structure-class-p (layout-class layout-x))
		(do ((i 1 (1+ i)))
		    ((= i len) t)
		  (declare (fixnum i))
		  (let ((x-el (%instance-ref x i))
			(y-el (%instance-ref y i)))
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
