;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Predicate functions for Spice Lisp.
;;; The type predicates are implementation-specific.  A different version
;;;   of this file will be required for implementations with different
;;;   data representations.
;;;
;;; Written and currently maintained by Scott Fahlman.
;;; Based on an earlier version by Joe Ginder.
;;;
(in-package 'lisp)
(export '(typep null symbolp atom consp listp numberp integerp rationalp
	  floatp complexp characterp stringp bit-vector-p vectorp
	  simple-vector-p simple-string-p simple-bit-vector-p arrayp
	  functionp compiled-function-p commonp eq eql equal equalp not
	  type-of
	  ;; Names of types...
	  array atom bignum bit bit-vector character common
	  compiled-function complex cons double-float
	  fixnum float function integer keyword list long-float nil
	  null number ratio rational sequence short-float signed-byte
	  simple-array simple-bit-vector simple-string simple-vector
	  single-float standard-char string string-char symbol t
	  unsigned-byte vector structure satisfies))

(in-package "EXTENSIONS")
(export '(structurep fixnump bignump bitp ratiop))
(in-package "LISP")


;;; Data type predicates.

;;; Translation from type keywords to specific predicates.  Assumes that
;;; the following are named structures and need no special type hackery:
;;; PATHNAME, STREAM, READTABLE, PACKAGE, HASHTABLE, RANDOM-STATE.

(defparameter type-pred-alist
  '((keyword . keywordp)
    (common . commonp)
    (null . null)
    (cons . consp)
    (list . listp)
    (symbol . symbolp)
    (array . arrayp)
    (vector . vectorp)
    (bit-vector . bit-vector-p)
    (string . stringp)
    (sequence . sequencep)
    (simple-array . simple-array-p)
    (c::structure-vector . simple-vector-p)
    (simple-vector . simple-vector-p)
    (simple-string . simple-string-p)
    (simple-bit-vector . simple-bit-vector-p)
    (function . functionp)
    (compiled-function . compiled-function-p)
    (character . characterp)
    (number . numberp)
    (rational . rationalp)
    (float . floatp)
    (string-char . %string-char-p)
    (integer . integerp)
    (ratio . ratiop)
    (short-float . short-float-p)
    (standard-char . %standard-char-p)
    (fixnum . fixnump)
    (complex . complexp)
;    (single-float . single-float-p)
    (single-float . short-float-p)
    (bignum . bignump)
    (double-float . double-float-p)
    (bit . bitp)
    (long-float . long-float-p)
    (structure . structurep)
    (atom . atom)))


;;;; TYPE-OF and auxiliary functions.

(defun type-of (object)
  "Returns the type of OBJECT as a type-specifier.
  Since objects may be of more than one type, the choice is somewhat
  arbitrary and may be implementation-dependent."
  (if (null object) 'symbol
      (case (%primitive get-type object)
	(#.%+-fixnum-type 'fixnum)
	(#.%bignum-type 'bignum)
	(#.%ratio-type 'ratio)
	((#.%short-+-float-type #.%short---float-type) 'short-float)
	(#.%long-float-type 'long-float)
	(#.%complex-type 'complex)
	(#.%string-type `(simple-string ,(%primitive vector-length object)))
	(#.%bit-vector-type
	 `(simple-bit-vector ,(%primitive vector-length object)))
	(#.%integer-vector-type (type-of-i-vector object))
	(#.%general-vector-type (type-of-g-vector object))
	(#.%array-type (type-of-array object))
	(#.%function-type 'function)
	(#.%symbol-type 'symbol)
	(#.%list-type 'cons)
	(#.%string-char-type 'string-char)
	(#.%bitsy-char-type 'character)
	(#.%--fixnum-type 'fixnum)
	(t 'random))))

;;; %String-Char-P is called by typep when the type specification
;;; is string-char.  The CL string-char-p does not do the right thing.
(defun %string-char-p (x)
  (and (characterp x)
       (< (the fixnum (char-int x)) char-code-limit)))

;;; Create the list-style description of a G-vector.

(defun type-of-g-vector (object)
  (cond ((structurep object) (svref object 0))
	(t `(simple-vector ,(%primitive vector-length object)))))

;;; I-Vector-Element-Type  --  Internal
;;;
;;;    Return a type specifier for the element type of an I-Vector.
;;;
(defun i-vector-element-type (object)
  (let ((ac (%primitive get-vector-access-code object)))
    (if (< 0 ac 6)
	(svref '#((mod 2) (mod 4) (mod 16) (mod 256) (mod 65536)
		  (mod 4294967296))
	       ac)
	(error "Invalid I-Vector access code: ~S" ac))))
	
;;; Create the list-style description of an I-vector.
	
(defun type-of-i-vector (object)
  `(simple-array ,(i-vector-element-type object)
		 ,(%primitive vector-length object)))


;;; Create the list-style description of an array.

(defun type-of-array (object)
  (with-array-data ((data-vector object) (start) (end))
    (declare (ignore start end))
    (let ((rank (- (the fixnum (%primitive header-length object))
		   %array-first-dim-slot))
	  (length (%primitive header-ref object %array-length-slot)))
      (declare (fixnum rank length))
      (if (= rank 1)
	  (typecase data-vector
	    (simple-bit-vector `(bit-vector ,length))
	    (simple-string `(string ,length))
	    (simple-vector `(vector t ,length))
	    (t `(vector ,(i-vector-element-type data-vector) ,length)))
	  `(array
	    ,(typecase data-vector
	       (simple-bit-vector '(mod 2))
	       (simple-string 'string-char)
	       (simple-vector 't)
	       (t (i-vector-element-type data-vector)))
	    ,(array-dimensions object))))))

;;;; TYPEP and auxiliary functions.

(defun %typep (object type)
  (let ((type (type-expand type))
	temp)
    (cond ((symbolp type)
	   (cond ((or (eq type t) (eq type '*)) t)
		 ((eq type 'nil) nil)
		 ((setq temp (assq type type-pred-alist))
		  (funcall (cdr temp) object))
		 (t (structure-typep object type))))
	  ((listp type) 
	   ;; This handles list-style type specifiers.
	   (case (car type)
	     (vector (and (vectorp object)
			  (vector-eltype object (cadr type))
			  (test-length object (caddr type))))
	     (simple-vector (and (simple-vector-p object)
				 (test-length object (cadr type))))
	     (string (and (stringp object)
			  (test-length object (cadr type))))
	     (simple-string (and (simple-string-p object)
				 (test-length object (cadr type))))
	     (bit-vector (and (bit-vector-p object)
			      (test-length object (cadr type))))
	     (simple-bit-vector (and (simple-bit-vector-p object)
				     (test-length object (cadr type))))
	     (array (array-typep object type))
	     (simple-array (and (not (array-header-p object))
				(array-typep object type)))
	     (satisfies (funcall (cadr type) object))
	     (member (member object (cdr type)))
	     (not (not (typep object (cadr type))))
	     (or (dolist (x (cdr type) nil)
		   (if (typep object x) (return t))))
	     (and (dolist (x (cdr type) t)
		    (if (not (typep object x)) (return nil))))
	     (integer (and (integerp object) (test-limits object type)))
	     (rational (and (rationalp object) (test-limits object type)))
	     (float (and (floatp object) (test-limits object type)))
	     (short-float (and (short-float-p object)
			       (test-limits object type)))
	     (single-float (and (single-float-p object)
				(test-limits object type)))
	     (double-float (and (double-float-p object)
				(test-limits object type)))
	     (long-float (and (long-float-p object)
			      (test-limits object type)))
	     (mod (and (integerp object)
		       (>= object 0)
		       (< object (cadr type))))
	     (signed-byte
	      (and (integerp object)
		   (let ((n (cadr type)))
		     (or (not n) (eq n '*)
			 (> n (integer-length object))))))
	     (unsigned-byte
	      (and (integerp object)
		   (not (minusp object))
		   (let ((n (cadr type)))
		     (or (not n) (eq n '*)
			 (>= n (integer-length object))))))
	     (complex (and (numberp object)
			   (or (not (cdr type))
			       (typep (realpart object) (cadr type)))))
	     (t (error "~S -- Illegal type specifier to TYPEP."  type))))
	  (t (error "~S -- Illegal type specifier to TYPEP."  type)))))

(defun typep (obj type)
  "Returns T if OBJECT is of the specified TYPE, otherwise NIL."
  (declare (notinline %typep))
  (%typep obj type))


;;; Given that the object is a vector of some sort, and that we've already
;;; verified that it matches CAR of TYPE, see if the rest of the type
;;; specifier wins.  Mild hack: Eltype Nil means either type not supplied
;;; or was Nil.  Any vector can hold objects of type Nil, since there aren't
;;; any, so (vector nil) is the same as (vector *).
;;;
(defun vector-eltype (object eltype)
  (let ((data (if (array-header-p object)
		  (with-array-data ((data object) (start) (end))
		    (declare (ignore start end))
		    data)
		  object))
	(eltype (type-expand eltype)))
    (case eltype
      ((t) (simple-vector-p data))
      (string-char (simple-string-p data))
      (bit (simple-bit-vector-p data))
      ((* nil) t)
      (t
       (subtypep eltype
		 (cond ((simple-vector-p data) t)
		       ((simple-string-p data) 'string-char)
		       ((simple-bit-vector-p data) 'bit)
		       (t
			(i-vector-element-type data))))))))


;;; Test sequence for specified length.

(defun test-length (object length)
  (or (null length)
      (eq length '*)
      (= length (length object))))


;;; See if object satisfies the specifier for an array.

(defun array-typep (object type)
  (and (arrayp object)
       (vector-eltype object (cadr type))
       (if (cddr type)
	   (let ((dims (third type)))
	     (cond ((eq dims '*) t)
		   ((numberp dims)
		    (and (vectorp object)
			 (= (the fixnum (length (the vector object)))
			    (the fixnum dims))))
		   (t
		    (dotimes (i (array-rank object) (null dims))
		      (when (null dims) (return nil))
		      (let ((dim (pop dims)))
			(unless (or (eq dim '*)
				    (= dim (array-dimension object i)))
			  (return nil)))))))
	   t)))


;;; Test whether a number falls within the specified limits.

(defun test-limits (object type)
  (let ((low (cadr type))
	(high (caddr type)))
    (and (cond ((null low) t)
	       ((eq low '*) t)
	       ((numberp low) (>= object low))
	       ((and (consp low) (numberp (car low)))
		(> object (car low)))
	       (t nil))
	 (cond ((null high) t)
	       ((eq high '*) t)
	       ((numberp high) (<= object high))
	       ((and (consp high) (numberp (car high)))
		(< object (car high)))
	       (t nil)))))


;;; Structure-Typep  --  Internal
;;;
;;; This is called by Typep if the type-specifier is a symbol and is not one of
;;; the built-in Lisp types.  If it's a structure, see if it's that type, or if
;;; it includes that type.
;;;
(defun structure-typep (object type)
  (declare (optimize speed))
  (let ((type (type-expand type)))
    (if (symbolp type)
	(let ((info (info type defined-structure-info type)))
	  (if info
	      (and (structurep object)
		   (let ((obj-name (%primitive header-ref object 0)))
		     (or (eq obj-name type)
			 (if (memq obj-name (c::dd-included-by info))
			     t nil))))
	      (error "~S is an unknown type specifier." type)))
	(error "~S is an unknown type specifier." type))))


;;;; Assorted mumble-P type predicates.

(defun commonp (object)
  "Returns T if object is a legal Common-Lisp type, NIL if object is any
  sort of implementation-dependent or internal type."
  (or (structurep object)
      (let ((type-spec (type-of object)))
	(if (listp type-spec) (setq type-spec (car type-spec)))
	(when (memq type-spec
		    '(character fixnum short-float single-float double-float
				long-float vector string simple-vector
				simple-string bignum ratio complex
				compiled-function array symbol cons))
	  T))))

(defun bit-vector-p (object)
  "Returns T if the object is a bit vector, else returns NIL."
  (bit-vector-p object))

;;; The following definitions are trivial because the compiler open-codes
;;; all of these.

(defun null (object)
  "Returns T if the object is NIL, else returns NIL."
  (null object))

(defun not (object)
  "Returns T if the object is NIL, else returns NIL."
  (null object))

(defun symbolp (object)
  "Returns T if the object is a symbol, else returns NIL."
  (symbolp object))

(defun atom (object)
  "Returns T if the object is not a cons, else returns NIL.
  Note that (ATOM NIL) => T."
  (atom object))

(defun consp (object)
  "Returns T if the object is a cons cell, else returns NIL.
  Note that (CONSP NIL) => NIL."
  (consp object))

(defun listp (object)
  "Returns T if the object is a cons cell or NIL, else returns NIL."
  (listp object))

(defun numberp (object)
  "Returns T if the object is any kind of number."
  (numberp object))

(defun integerp (object)
  "Returns T if the object is an integer (fixnum or bignum), else 
  returns NIL."
  (integerp object))

(defun rationalp (object)
  "Returns T if the object is an integer or a ratio, else returns NIL."
  (rationalp object))

(defun floatp (object)
  "Returns T if the object is a floating-point number, else returns NIL."
  (floatp object))

(defun complexp (object)
  "Returns T if the object is a complex number, else returns NIL."
  (complexp object))

(defun %standard-char-p (x)
  (and (characterp x) (standard-char-p x)))

(defun characterp (object)
  "Returns T if the object is a character, else returns NIL."
  (characterp object))

(defun stringp (object)
  "Returns T if the object is a string, else returns NIL."
  (stringp object))

(defun simple-string-p (object)
  "Returns T if the object is a simple string, else returns NIL."
  (simple-string-p object))

(defun vectorp (object)
  "Returns T if the object is any kind of vector, else returns NIL."
  (vectorp object))

(defun simple-array-p (object)
  "Returns T if the object is a simple array, else returns NIL."
  (and (arrayp object) (not (array-header-p object))))

(defun simple-vector-p (object)
  "Returns T if the object is a simple vector, else returns NIL."
  (simple-vector-p object))

(defun simple-bit-vector-p (object)
  "Returns T if the object is a simple bit vector, else returns NIL."
  (simple-bit-vector-p object))

(defun arrayp (object)
  "Returns T if the argument is any kind of array, else returns NIL."
  (arrayp object))

(defun functionp (object)
  "Returns T if the object is a function, suitable for use by FUNCALL
  or APPLY, else returns NIL."
  (functionp object))

(defun compiled-function-p (object)
  "Returns T if the object is a compiled function object, else returns NIL."
  (compiled-function-p object))

;;; ### Dummy definition until we figure out what to really do...
(defun clos::funcallable-instance-p (object)
  (declare (ignore object))
  nil)

(defun sequencep (object)
  "Returns T if object is a sequence, NIL otherwise."
  (typep object 'sequence))


;;; The following are not defined at user level, but are necessary for
;;; internal use by TYPEP.

(defun structurep (object)
  (structurep object))

(defun fixnump (object)
  (fixnump object))

(defun bignump (object)
  (bignump object))

(defun bitp (object)
  (typep object 'bit))

(defun short-float-p (object)
  (typep object 'short-float))

(defun single-float-p (object)
  (typep object 'single-float))

(defun double-float-p (object)
  (typep object 'double-float))

(defun long-float-p (object)
  (typep object 'long-float))

(defun ratiop (object)
  (ratiop object))

;;; Some silly internal things for tenser array hacking:

(defun array-header-p (object)
  (array-header-p object))

;;;; Equality Predicates.

(defun eq (x y)
  "Returns T if X and Y are the same object, else returns NIL."
  (eq x y))

(defun eql (x y)
  "Returns T if X and Y are EQ, or if they are numbers of the same
  type and precisely equal value, or if they are characters and
  are CHAR=, else returns NIL."
  (eql x y))

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
	 (and (pathnamep y)
	      (do* ((i 1 (1+ i))
		    (len (length x)))
		   ((>= i len) t)
		(declare (fixnum i len))
		(let ((x-el (svref x i))
		      (y-el (svref y i)))
		  (if (and (simple-vector-p x-el)
			   (simple-vector-p y-el))
		      (let ((lx (length x-el))
			    (ly (length y-el)))
			(declare (fixnum lx ly))
			(if (/= lx ly) (return nil))
			(do ((i 0 (1+ i)))
			    ((>= i lx))
			  (declare (fixnum i))
			  (if (not (equal (svref x-el i) (svref y-el i)))
			      (return-from equal nil))))
		      (unless (or (eql x-el y-el)
				  (equal x-el y-el))
			(return nil)))))))
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


(defun equalp (x y)
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion.  Characters may differ in alphabetic case.  Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction."
  (cond ((eql x y) t)
	((characterp x) (char-equal x y))
	((numberp x) (and (numberp y) (= x y)))
	((consp x)
	 (and (consp y)
	      (equalp (car x) (car y))
	      (equalp (cdr x) (cdr y))))
	((vectorp x)
	 (let ((length (length x)))
	   (declare (fixnum length))
	   (and (vectorp y)
		(= length (the fixnum (length y)))
		(dotimes (i length t)
		  (let ((x-el (aref x i))
			(y-el (aref y i)))
		    (unless (or (eql x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((arrayp x)
	 (let ((rank (array-rank x))
	       (len (%primitive header-ref x %array-length-slot)))
	   (declare (fixnum rank len))
	   (and (arrayp y)
		(= (the fixnum (array-rank y)) rank)
		(dotimes (i rank t)
		  (unless (= (the fixnum (array-dimension x i))
			     (the fixnum (array-dimension y i)))
		    (return nil)))
		(with-array-data ((x-vec x) (x-start) (end))
		  (declare (ignore end))
		  (with-array-data ((y-vec y) (y-start) (end))
		    (declare (ignore end))
		    (do ((i x-start (1+ i))
			 (j y-start (1+ j))
			 (count len (1- count)))
			((zerop count) t)
		      (declare (fixnum i j count))
		      (let ((x-el (aref x-vec i))
			    (y-el (aref y-vec j)))
			(unless (or (eql x-el y-el)
				    (equalp x-el y-el))
			  (return nil)))))))))
	(t nil)))
