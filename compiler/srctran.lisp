;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/srctran.lisp,v 1.16 1990/10/03 17:34:40 ram Exp $
;;;
;;;    This file contains macro-like source transformations which convert
;;; uses of certain functions into the canonical form desired within the
;;; compiler.  ### and other IR1 transforms and stuff.  Some code adapted from
;;; CLC, written by Wholey and Fahlman.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

;;; Source transform for Not, Null  --  Internal
;;;
;;;    Convert into an IF so that IF optimizations will eliminate redundant
;;; negations.
;;;
(def-source-transform not (x) `(if ,x nil t))
(def-source-transform null (x) `(if ,x nil t))

;;; Source transform for Endp  --  Internal
;;;
;;;    Endp is just NULL with a List assertion.
;;;
(def-source-transform endp (x) `(null (the list ,x)))

;;; We turn Identity into Prog1 so that it is obvious that it just returns the
;;; first value of its argument.  Ditto for Values with one arg.
(def-source-transform identity (x) `(prog1 ,x))
(def-source-transform values (x) `(prog1 ,x))


;;;; List hackery:

;;;
;;; Translate CxxR into car/cdr combos.
(def-source-transform caar (x) `(car (car ,x)))
(def-source-transform cadr (x) `(car (cdr ,x)))
(def-source-transform cdar (x) `(cdr (car ,x)))
(def-source-transform cddr (x) `(cdr (cdr ,x)))
(def-source-transform caaar (x) `(car (car (car ,x))))
(def-source-transform caadr (x) `(car (car (cdr ,x))))
(def-source-transform cadar (x) `(car (cdr (car ,x))))
(def-source-transform caddr (x) `(car (cdr (cdr ,x))))
(def-source-transform cdaar (x) `(cdr (car (car ,x))))
(def-source-transform cdadr (x) `(cdr (car (cdr ,x))))
(def-source-transform cddar (x) `(cdr (cdr (car ,x))))
(def-source-transform cdddr (x) `(cdr (cdr (cdr ,x))))
(def-source-transform caaaar (x) `(car (car (car (car ,x)))))
(def-source-transform caaadr (x) `(car (car (car (cdr ,x)))))
(def-source-transform caadar (x) `(car (car (cdr (car ,x)))))
(def-source-transform caaddr (x) `(car (car (cdr (cdr ,x)))))
(def-source-transform cadaar (x) `(car (cdr (car (car ,x)))))
(def-source-transform cadadr (x) `(car (cdr (car (cdr ,x)))))
(def-source-transform caddar (x) `(car (cdr (cdr (car ,x)))))
(def-source-transform cadddr (x) `(car (cdr (cdr (cdr ,x)))))
(def-source-transform cdaaar (x) `(cdr (car (car (car ,x)))))
(def-source-transform cdaadr (x) `(cdr (car (car (cdr ,x)))))
(def-source-transform cdadar (x) `(cdr (car (cdr (car ,x)))))
(def-source-transform cdaddr (x) `(cdr (car (cdr (cdr ,x)))))
(def-source-transform cddaar (x) `(cdr (cdr (car (car ,x)))))
(def-source-transform cddadr (x) `(cdr (cdr (car (cdr ,x)))))
(def-source-transform cdddar (x) `(cdr (cdr (cdr (car ,x)))))
(def-source-transform cddddr (x) `(cdr (cdr (cdr (cdr ,x)))))

;;;
;;; Turn First..Fourth and Rest into the obvious synonym, assuming whatever is
;;; right for them is right for us.  Fifth..Tenth turn into Nth, which can be
;;; expanded into a car/cdr later on if policy favors it.
(def-source-transform first (x) `(car ,x))
(def-source-transform rest (x) `(cdr ,x))
(def-source-transform second (x) `(cadr ,x))
(def-source-transform third (x) `(caddr ,x))
(def-source-transform fourth (x) `(cadddr ,x))
(def-source-transform fifth (x) `(nth 4 ,x))
(def-source-transform sixth (x) `(nth 5 ,x))
(def-source-transform seventh (x) `(nth 6 ,x))
(def-source-transform eighth (x) `(nth 7 ,x))
(def-source-transform ninth (x) `(nth 8 ,x))
(def-source-transform tenth (x) `(nth 9 ,x))


;;;
;;; Translate RPLACx to LET and SETF.
(def-source-transform rplaca (x y)
  (once-only ((n-x x))
    `(progn
       (setf (car ,n-x) ,y)
       ,n-x)))
;;;
(def-source-transform rplacd (x y)
  (once-only ((n-x x))
    `(progn
       (setf (cdr ,n-x) ,y)
       ,n-x)))


(def-source-transform nth (n l) `(car (nthcdr ,n ,l)))
  
(defvar *default-nthcdr-open-code-limit* 6)
(defvar *extreme-nthcdr-open-code-limit* 20)

(deftransform nthcdr ((n l) (unsigned-byte t) * :node node)
  (unless (constant-continuation-p n) (give-up))
  (let ((n (continuation-value n)))
    (when (> n
	     (if (policy node (= speed 3) (= space 0))
		 *extreme-nthcdr-open-code-limit*
		 *default-nthcdr-open-code-limit*))
      (give-up))

    (labels ((frob (n)
	       (if (zerop n)
		   'l
		   `(cdr ,(frob (1- n))))))
      (frob n))))


;;;; ARITHMETIC and NUMEROLOGY.

(def-source-transform plusp (x) `(> ,x 0))
(def-source-transform minusp (x) `(< ,x 0))
(def-source-transform zerop (x) `(= ,x 0))

(def-source-transform 1+ (x) `(+ ,x 1))
(def-source-transform 1- (x) `(- ,x 1))

(def-source-transform oddp (x) `(not (zerop (logand ,x 1))))
(def-source-transform evenp (x) `(zerop (logand ,x 1)))

;;; Note that all the integer division functions are available for inline
;;; expansion.

(def-source-transform truncate (x &optional y)
  (if y
      (values nil t)
      `(truncate ,x 1)))

(def-source-transform lognand (x y) `(lognot (logand ,x ,y)))
(def-source-transform lognor (x y) `(lognot (logior ,x ,y)))
(def-source-transform logandc1 (x y) `(logand (lognot ,x) ,y))
(def-source-transform logandc2 (x y) `(logand ,x (lognot ,y)))
(def-source-transform logorc1 (x y) `(logior (lognot ,x) ,y))
(def-source-transform logorc2 (x y) `(logior ,x (lognot ,y)))
(def-source-transform logtest (x y) `(not (zerop (logand ,x ,y))))
(def-source-transform logbitp (index integer)
  `(not (zerop (logand (ash 1 ,index) ,integer))))
(def-source-transform byte (size position) `(cons ,size ,position))
(def-source-transform byte-size (spec) `(car ,spec))
(def-source-transform byte-position (spec) `(cdr ,spec))
(def-source-transform ldb-test (bytespec integer)
  `(not (zerop (ldb ,bytespec ,integer))))


;;; With the ratio and complex accessors, we pick off the "identity" case, and
;;; use a primitive to handle the cell access case.
;;;
(def-source-transform numerator (num)
  (once-only ((n-num `(the rational ,num)))
    `(if (ratiop ,n-num)
	 (%primitive numerator ,n-num)
	 ,n-num)))
;;;
(def-source-transform denominator (num)
  (once-only ((n-num `(the rational ,num)))
    `(if (ratiop ,n-num)
	 (%primitive denominator ,n-num)
	 1)))
;;;
(def-source-transform realpart (num)
  (once-only ((n-num num))
    `(if (complexp ,n-num)
	 (%primitive realpart ,n-num)
	 ,n-num)))
;;;
(def-source-transform imagpart (num)
  (once-only ((n-num num))
    `(cond ((complexp ,n-num)
	    (%primitive imagpart ,n-num))
	   ((floatp ,n-num)
	    (float 0 ,n-num))
	   (t
	    0))))


;;;; Numeric Derive-Type methods:

;;; Derive-Integer-Type  --  Internal
;;;
;;;    Utility for defining derive-type methods of integer operations.  If the
;;; types of both X and Y are integer types, then we compute a new integer type
;;; with bounds determined Fun when applied to X and Y.  Otherwise, we use
;;; Numeric-Contagion.
;;;
(defun derive-integer-type (x y fun)
  (declare (type continuation x y) (type function fun))
  (let ((x (continuation-type x))
	(y (continuation-type y)))
    (if (and (numeric-type-p x) (numeric-type-p y)
	     (eq (numeric-type-class x) 'integer)
	     (eq (numeric-type-class y) 'integer)
	     (eq (numeric-type-complexp x) :real)
	     (eq (numeric-type-complexp y) :real))
	(multiple-value-bind (low high)
			     (funcall fun x y)
	  (make-numeric-type :class 'integer  :complexp :real
			     :low low  :high high))
	(numeric-contagion x y))))


(defoptimizer (+ derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (flet ((frob (x y)
		(if (and x y)
		    (+ x y)
		    nil)))
	 (values (frob (numeric-type-low x) (numeric-type-low y))
		 (frob (numeric-type-high x) (numeric-type-high y)))))))

(defoptimizer (- derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (flet ((frob (x y)
		(if (and x y)
		    (- x y)
		    nil)))
	 (values (frob (numeric-type-low x) (numeric-type-high y))
		 (frob (numeric-type-high x) (numeric-type-low y)))))))

(defoptimizer (* derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (let ((x-low (numeric-type-low x))
	     (x-high (numeric-type-high x))
	     (y-low (numeric-type-low y))
	     (y-high (numeric-type-high y)))
	 (cond ((not (and x-low y-low))
		(values nil nil))
	       ((or (minusp x-low) (minusp y-low))
		(if (and x-high y-high)
		    (let ((max (* (max (abs x-low) (abs x-high))
				  (max (abs y-low) (abs y-high)))))
		      (values (- max) max))
		    (values nil nil)))
	       (t
		(values (* x-low y-low)
			(if (and x-high y-high)
			    (* x-high y-high)
			    nil))))))))

(defoptimizer (/ derive-type) ((x y))
  (numeric-contagion (continuation-type x) (continuation-type y)))


(defoptimizer (ash derive-type) ((n shift))
  (or (let ((n-type (continuation-type n)))
	(when (numeric-type-p n-type)
	  (let ((n-low (numeric-type-low n-type))
		(n-high (numeric-type-high n-type)))
	    (if (constant-continuation-p shift)
		(let ((shift (continuation-value shift)))
		  (make-numeric-type :class 'integer  :complexp :real
				     :low (when n-low
					    #+new-compiler
					    (ash n-low shift)
					    ;; ### fuckin' bignum bug.
					    #-new-compiler
					    (* n-low (ash 1 shift)))
				     :high (when n-high (ash n-high shift))))
		(let ((s-type (continuation-type shift)))
		  (when (numeric-type-p s-type)
		    (let ((s-low (numeric-type-low s-type))
			  (s-high (numeric-type-high s-type)))
		      (if (and s-low s-high (<= s-low 32) (<= s-high 32))
			  (make-numeric-type :class 'integer  :complexp :real
					     :low (when n-low
						    (min (ash n-low s-high)
							 (ash n-low s-low)))
					     :high (when n-high
						     (max (ash n-high s-high)
							  (ash n-high s-low))))
			  (make-numeric-type :class 'integer
					     :complexp :real)))))))))
      *universal-type*))


;;; Negative-Integer-P  --  Internal
;;;
;;;    Return true if Type is a integer type that includes negative numbers.
;;;
(defun negative-integer-p (type)
  (declare (type numeric-type type))
  (let ((low (numeric-type-low type)))
    (or (not low) (minusp low))))

(defoptimizer (logand derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (let* ((x-high (numeric-type-high x))
	      (y-high (numeric-type-high y))
	      (both-neg (and (negative-integer-p x)
			     (negative-integer-p y)))
	      (min (cond ((not x-high) y-high)
			 ((not y-high) x-high)
			 (t
			  (min x-high y-high)))))
	 (if min
	     (let ((mag (ldb (byte (integer-length min) 0) -1)))
	       (values (if both-neg (lognot mag) 0) mag))
	     (values (if both-neg nil 0) nil))))))


(defoptimizer (logior derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (let* ((x-high (numeric-type-high x))
	      (y-high (numeric-type-high y))
	      (one-neg (or (negative-integer-p x)
			   (negative-integer-p y)))
	      (max (cond ((not x-high) nil)
			 ((not y-high) nil)
			 (t
			  (max x-high y-high)))))
	 (if max
	     (let ((mag (ldb (byte (integer-length max) 0) -1)))
	       (values (if one-neg (lognot mag) 0) mag))
	     (values (if one-neg nil 0) nil))))))

;;; All we attempt to do is determine the maximum integer length that the
;;; result can take on, as that is all that is interesting.

(defoptimizer (logxor derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (let* ((x-high (numeric-type-high x))
	      (x-pos (plusp (or x-high 1)))
	      (y-high (numeric-type-high y))
	      (y-pos (plusp (or y-high 1)))
	      (x-low (numeric-type-low x))
	      (x-neg (minusp (or x-low -1)))
	      (y-low (numeric-type-low y))
	      (y-neg (minusp (or y-low -1)))
	      (signed (or (and x-pos y-neg) (and x-neg y-pos))))
	 (if (and x-high y-high x-low y-low)
	     (let ((max (max (integer-length x-high)
			     (integer-length x-low)
			     (integer-length y-high)
			     (integer-length y-low))))
	       (values (if signed (ash -1 max) 0)
		       (1- (ash 1 max))))
	     (values (if signed nil 0) nil))))))

(macrolet ((frob (fun)
	     `#'(lambda (type type2)
		  (declare (ignore type2))
		  (let ((lo (numeric-type-low type))
			(hi (numeric-type-high type)))
		    (values (if hi (,fun hi) nil) (if lo (,fun lo) nil))))))

  (defoptimizer (%negate derive-type) ((num))
    (derive-integer-type num num (frob -)))

  (defoptimizer (lognot derive-type) ((int))
    (derive-integer-type int int (frob lognot))))


(defoptimizer (abs derive-type) ((num))
  (let ((type (continuation-type num)))
    (if (and (numeric-type-p type)
	     (eq (numeric-type-class type) 'integer)
	     (eq (numeric-type-complexp type) :real))
	(let ((lo (numeric-type-low type))
	      (hi (numeric-type-high type)))
	  (make-numeric-type :class 'integer :complexp :real
			     :low (cond ((and hi (minusp hi))
					 (abs hi))
					(lo
					 (max 0 lo))
					(t
					 0))
			     :high (if (and hi lo)
				       (max (abs hi) (abs lo))
				       nil)))
	(numeric-contagion type type))))


(defoptimizer (truncate derive-type) ((number divisor))
  (let ((number-type (continuation-type number))
	(divisor-type (continuation-type divisor))
	(integer-type (specifier-type 'integer)))
    (if (and (numeric-type-p number-type)
	     (csubtypep number-type integer-type)
	     (numeric-type-p divisor-type)
	     (csubtypep divisor-type integer-type))
	(let ((number-low (numeric-type-low number-type))
	      (number-high (numeric-type-high number-type))
	      (divisor-low (numeric-type-low divisor-type))
	      (divisor-high (numeric-type-high divisor-type)))
	  (values-specifier-type
	   `(values ,(integer-truncate-derive-type number-low number-high
						   divisor-low divisor-high)
		    ,(integer-rem-derive-type number-low number-high
					      divisor-low divisor-high))))
	*universal-type*)))

;;; NUMERIC-RANGE-INFO  --  internal.
;;;
;;; Derive useful information about the range.  Returns three values:
;;; - '+ if its positive, '- negative, or nil if it overlaps 0.
;;; - The abs of the minimal value (i.e. closest to 0) in the range.
;;; - The abs of the maximal value if there is one, or nil if it is unbounded.
;;; 
(defun numeric-range-info (low high)
  (cond ((and low (not (minusp low)))
	 (values '+ low high))
	((and high (not (plusp high)))
	 (values '- (- high) (if low (- low) nil)))
	(t
	 (values nil 0 (and low high (max (- low) high))))))

;;; INTEGER-TRUNCATE-DERIVE-TYPE -- internal
;;; 
(defun integer-truncate-derive-type
       (number-low number-high divisor-low divisor-high)
  ;; The result cannot be larger in magnitude than the number, but the sign
  ;; might change.  If we can determine the sign of either the number or
  ;; the divisor, we can eliminate some of the cases.
  (multiple-value-bind
      (number-sign number-min number-max)
      (numeric-range-info number-low number-high)
    (multiple-value-bind
	(divisor-sign divisor-min divisor-max)
	(numeric-range-info divisor-low divisor-high)
      (when (and divisor-max (zerop divisor-max))
	;; We've got a problem: guarenteed division by zero.
	(return-from integer-truncate-derive-type t))
      (when (zerop divisor-min)
	;; We'll assume that they arn't going to divide by zero.
	(incf divisor-min))
      (cond ((and number-sign divisor-sign)
	     ;; We know the sign of both.
	     (if (eq number-sign divisor-sign)
		 ;; Same sign, so the result will be positive.
		 `(integer ,(if divisor-max
				(truncate number-min divisor-max)
				0)
			   ,(if number-max
				(truncate number-max divisor-min)
				'*))
		 ;; Different signs, the result will be negative.
		 `(integer ,(if number-max
				(- (truncate number-max divisor-min))
				'*)
			   ,(if divisor-max
				(- (truncate number-min divisor-max))
				0))))
	    ((eq divisor-sign '+)
	     ;; The divisor is positive.  Therefore, the number will just
	     ;; become closer to zero.
	     `(integer ,(if number-low
			    (truncate number-low divisor-min)
			    '*)
		       ,(if number-high
			    (truncate number-high divisor-min)
			    '*)))
	    ((eq divisor-sign '-)
	     ;; The divisor is negative.  Therefore, the absolute value of
	     ;; the number will become closer to zero, but the sign will also
	     ;; change.
	     `(integer ,(if number-high
			    (- (truncate number-high divisor-min))
			    '*)
		       ,(if number-low
			    (- (truncate number-low divisor-min))
			    '*)))
	    ;; The divisor could be either positive or negative.
	    (number-max
	     ;; The number we are dividing has a bound.  Divide that by the
	     ;; smallest posible divisor.
	     (let ((bound (truncate number-max divisor-min)))
	       `(integer ,(- bound) ,bound)))
	    (t
	     ;; The number we are dividing is unbounded, so we can't tell
	     ;; anything about the result.
	     'integer)))))
	  
(defun integer-rem-derive-type
       (number-low number-high divisor-low divisor-high)
  (if (and divisor-low divisor-high)
      ;; We know the range of the divisor, and the remainder must be smaller
      ;; than the divisor.  We can tell the sign of the remainer if we know
      ;; the sign of the number.
      (let ((divisor-max (1- (max (abs divisor-low) (abs divisor-high)))))
	`(integer ,(if (or (null number-low)
			   (minusp number-low))
		       (- divisor-max)
		       0)
		  ,(if (or (null number-high)
			   (plusp number-high))
		       divisor-max
		       0)))
      ;; The divisor is potentially either very positive or very negative.
      ;; Therefore, the remainer is unbounded, but we might be able to tell
      ;; something about the sign from the number.
      `(integer ,(if (and number-low (not (minusp number-low)))
		     ;; The number we are dividing is positive.  Therefore,
		     ;; the remainder must be positive.
		     0
		     '*)
		,(if (and number-high (not (plusp number-high)))
		     ;; The number we are dividing is negative.  Therefore,
		     ;; the remainder must be negative.
		     0
		     '*))))


;;;; Array derive-type optimizers:

(defoptimizer (array-in-bounds-p derive-type) ((array &rest indices))
  (assert-continuation-type
   array
   (specifier-type `(array * ,(make-list (length indices)
					 :initial-element '*))))
  *universal-type*)

(defoptimizer (aref derive-type) ((array &rest indices))
  (assert-continuation-type
   array
   (specifier-type `(array * ,(make-list (length indices)
					 :initial-element '*))))
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (array-type-element-type type))))

(defoptimizer (data-vector-ref derive-type) ((array index))
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (array-type-element-type type))))

(defoptimizer (%aset derive-type) ((array &rest stuff))
  (assert-continuation-type
   array
   (specifier-type `(array * ,(make-list (1- (length stuff))
					 :initial-element '*))))
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (let ((val (car (last stuff)))
	    (eltype (array-type-element-type type)))
	(assert-continuation-type val eltype)
	(continuation-type val)))))

(defoptimizer (data-vector-set derive-type) ((array index new-value))
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (let ((eltype (array-type-element-type type)))
	(assert-continuation-type new-value eltype)
	(continuation-type new-value)))))

(defoptimizer (array-row-major-index derive-type) ((array &rest indices))
  (assert-continuation-type
   array
   (specifier-type `(array * ,(make-list (length indices)
					 :initial-element '*))))
  *universal-type*)

(defoptimizer (row-major-aref derive-type) ((array index))
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (array-type-element-type type))))
  
(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (assert-continuation-type new-value (array-type-element-type type))
      (continuation-type new-value))))


;;; Unsupplied-Or-NIL  --  Internal
;;;
;;;    Return true if Arg is NIL, or is a constant-continuation whose value is
;;; NIL, false otherwise.
;;;
(defun unsupplied-or-nil (arg)
  (declare (type (or continuation null) arg))
  (or (not arg)
      (and (constant-continuation-p arg)
	   (not (continuation-value arg)))))

(defoptimizer (make-array derive-type) ((dims &key initial-element
					      element-type initial-contents
					      adjustable fill-pointer
					      displaced-index-offset
					      displaced-to))
  (specifier-type
   `(,(if (and (unsupplied-or-nil adjustable)
	       (unsupplied-or-nil displaced-to)
	       (unsupplied-or-nil fill-pointer))
	  'simple-array
	  'array)
     ,(cond ((not element-type) 't)
	    ((constant-continuation-p element-type)
	     (continuation-value element-type))
	    (t
	     '*))
     ,(cond ((constant-continuation-p dims)
	     (let ((val (continuation-value dims)))
	       (if (listp val) val (list val))))
	    ((csubtypep (continuation-type dims)
			(specifier-type 'integer))
	     '(*))
	    (t
	     '*)))))


;;;; Miscellaneous derive-type methods:


(defoptimizer (code-char derive-type) ((code))
  (specifier-type 'base-character))


(defoptimizer (values derive-type) ((&rest values))
  (values-specifier-type
   `(values ,@(mapcar #'(lambda (x)
			  (type-specifier (continuation-type x)))
		      values))))



;;;; Byte operations:
;;;
;;;    We try to turn byte operations into simple logical operations.  First,
;;; we convert byte specifiers into separate size and position arguments passed
;;; to internal %FOO functions.  We then attempt to transform the %FOO
;;; functions into boolean operations when the size and position are constant
;;; and the operands are fixnums.


;;; With-Byte-Specifier  --  Internal
;;;
;;;    Evaluate body with Size-Var and Pos-Var bound to expressions that
;;; evaluate to the Size and Position of the byte-specifier form Spec.  We may
;;; wrap a let around the result of the body to bind some variables.
;;;
;;;    If the spec is a Byte form, then bind the vars to the subforms.
;;; otherwise, evaluate Spec and use the Byte-Size and Byte-Position.  The goal
;;; of this transformation is to avoid consing up byte specifiers and then
;;; immediately throwing them away.
;;;
(defmacro with-byte-specifier ((size-var pos-var spec) &body body)
  (once-only ((spec `(macroexpand ,spec))
	      (temp '(gensym)))
    `(if (and (consp ,spec)
	      (eq (car ,spec) 'byte)
	      (= (length ,spec) 3))
	 (let ((,size-var (second ,spec))
	       (,pos-var (third ,spec)))
	   ,@body)
	 (let ((,size-var `(byte-size ,,temp))
	       (,pos-var `(byte-position ,,temp)))
	   `(let ((,,temp ,,spec))
	      ,,@body)))))

(def-source-transform ldb (spec int)
  (with-byte-specifier (size pos spec)
    `(%ldb ,size ,pos ,int)))

(def-source-transform dpb (newbyte spec int)
  (with-byte-specifier (size pos spec)
    `(%dpb ,newbyte ,size ,pos ,int)))

(def-source-transform mask-field (spec int)
  (with-byte-specifier (size pos spec)
    `(%mask-field ,size ,pos ,int)))

(def-source-transform deposit-field (newbyte spec int)
  (with-byte-specifier (size pos spec)
    `(%deposit-field ,newbyte ,size ,pos ,int)))


(defoptimizer (%ldb derive-type) ((size posn num))
  (let ((size (continuation-type size)))
    (if (and (numeric-type-p size)
	     (csubtypep size (specifier-type 'integer)))
	(let ((size-high (numeric-type-high size)))
	  (if (and size-high (<= size-high vm:word-bits))
	      (specifier-type `(unsigned-byte ,size-high))
	      (specifier-type 'unsigned-byte)))
	*universal-type*)))

(defoptimizer (%mask-field derive-type) ((size posn num))
  (let ((size (continuation-type size))
	(posn (continuation-type posn)))
    (if (and (numeric-type-p size)
	     (csubtypep size (specifier-type 'integer))
	     (numeric-type-p posn)
	     (csubtypep posn (specifier-type 'integer)))
	(let ((size-high (numeric-type-high size))
	      (posn-high (numeric-type-high posn)))
	  (if (and size-high posn-high
		   (<= (+ size-high posn-high) vm:word-bits))
	      (specifier-type `(unsigned-byte ,(+ size-high posn-high)))
	      (specifier-type 'unsigned-byte)))
	*universal-type*)))

(defoptimizer (%dpb derive-type) ((newbyte size posn int))
  (let ((size (continuation-type size))
	(posn (continuation-type posn))
	(int (continuation-type int)))
    (if (and (numeric-type-p size)
	     (csubtypep size (specifier-type 'integer))
	     (numeric-type-p posn)
	     (csubtypep posn (specifier-type 'integer))
	     (numeric-type-p int)
	     (csubtypep int (specifier-type 'integer)))
	(let ((size-high (numeric-type-high size))
	      (posn-high (numeric-type-high posn))
	      (high (numeric-type-high int))
	      (low (numeric-type-low int)))
	  (if (and size-high posn-high high low
		   (<= (+ size-high posn-high) vm:word-bits))
	      (specifier-type
	       (list (if (minusp low) 'signed-byte 'unsigned-byte)
		     (max (integer-length high)
			  (integer-length low)
			  (+ size-high posn-high))))
	      *universal-type*))
	*universal-type*)))

(defoptimizer (%deposit-field derive-type) ((newbyte size posn int))
  (let ((size (continuation-type size))
	(posn (continuation-type posn))
	(int (continuation-type int)))
    (if (and (numeric-type-p size)
	     (csubtypep size (specifier-type 'integer))
	     (numeric-type-p posn)
	     (csubtypep posn (specifier-type 'integer))
	     (numeric-type-p int)
	     (csubtypep int (specifier-type 'integer)))
	(let ((size-high (numeric-type-high size))
	      (posn-high (numeric-type-high posn))
	      (high (numeric-type-high int))
	      (low (numeric-type-low int)))
	  (if (and size-high posn-high high low
		   (<= (+ size-high posn-high) vm:word-bits))
	      (specifier-type
	       (list (if (minusp low) 'signed-byte 'unsigned-byte)
		     (max (integer-length high)
			  (integer-length low)
			  (+ size-high posn-high))))
	      *universal-type*))
	*universal-type*)))



(deftransform %ldb ((size posn int)
		    (fixnum fixnum integer)
		    (unsigned-byte #.vm:word-bits))
  `(logand (ash int (- posn))
	   (ash ,(1- (ash 1 vm:word-bits))
		(- size ,vm:word-bits))))

(deftransform %mask-field ((size posn int)
			   (fixnum fixnum integer)
			   (unsigned-byte #.vm:word-bits))
  `(logand int
	   (ash (ash ,(1- (ash 1 vm:word-bits))
		     (- size ,vm:word-bits))
		posn)))

;;; Note: for %dpb and %deposit-field, we can't use (or (signed-byte n)
;;; (unsigned-byte n)) as the result type, as that would allow result types
;;; that cover the range -2^(n-1) .. 1-2^n, instead of allowing result types
;;; of (unsigned-byte n) and result types of (signed-byte n).

(deftransform %dpb ((new size posn int)
		    *
		    (unsigned-byte #.vm:word-bits))
  `(let ((mask (ldb (byte size 0) -1)))
     (logior (ash (logand new mask) posn)
	     (logand int (lognot (ash mask posn))))))

(deftransform %dpb ((new size posn int)
		    *
		    (signed-byte #.vm:word-bits))
  `(let ((mask (ldb (byte size 0) -1)))
     (logior (ash (logand new mask) posn)
	     (logand int (lognot (ash mask posn))))))

(deftransform %deposit-field ((new size posn int)
			      *
			      (unsigned-byte #.vm:word-bits))
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
	     (logand int (lognot mask)))))

(deftransform %deposit-field ((new size posn int)
			      *
			      (signed-byte #.vm:word-bits))
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
	     (logand int (lognot mask)))))


;;;; Funny function stubs:
;;;
;;;    These functions are the result of compiler transformations.  We never
;;; actually compile a call to these functions, but we need to have a
;;; definition to allow constant folding.
;;;

#-new-compiler
(progn

(defun %negate (x) (%primitive negate x))
(defun %ldb (s p i) (%primitive ldb s p i))
(defun %dpb (n s p i) (%primitive dpb n s p i))
(defun %mask-field (s p i) (%primitive mask-field s p i))
(defun %deposit-field (n s p i) (%primitive deposit-field n s p i))

); #-new-compiler progn


;;; Miscellanous numeric transforms:


;;; COMMUTATIVE-ARG-SWAP  --  Internal
;;;
;;;    If a constant appears as the first arg, swap the args.
;;;
(deftransform commutative-arg-swap ((x y) * * :defun-only t :node node)
  (if (and (constant-continuation-p x)
	   (not (constant-continuation-p y)))
      `(,(continuation-function-name (basic-combination-fun node)) y x)
      (give-up)))

(dolist (x '(= char= + * logior logand logxor))
  (%deftransform x '(function * *) #'commutative-arg-swap))


;;; Handle the case of a constant boole-code.
;;;
(deftransform boole ((op x y))
  (unless (constant-continuation-p op)
    (give-up "BOOLE code is not a constant."))
  (let ((control (continuation-value op)))
    (case control
      (#.boole-clr 0)
      (#.boole-set -1)
      (#.boole-1 'x)
      (#.boole-2 'y)
      (#.boole-c1 '(lognot x))
      (#.boole-c2 '(lognot y))
      (#.boole-and '(logand x y))
      (#.boole-ior '(logior x y))
      (#.boole-xor '(logxor x y))
      (#.boole-eqv '(logeqv x y))
      (#.boole-nand '(lognand x y))
      (#.boole-nor '(lognor x y))
      (#.boole-andc1 '(logandc1 x y))
      (#.boole-andc2 '(logandc2 x y))
      (#.boole-orc1 '(logorc1 x y))
      (#.boole-orc2 '(logorc2 x y))
      (t
       (abort-transform "~S illegal control arg to BOOLE." control)))))


;;; If arg is a constant power of two, turn * into a shift.
;;;
(deftransform * ((x y) (integer integer))
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (= y-abs (ash 1 len)) (give-up))
    (if (minusp y)
	`(- (ash x ,len))
	`(ash x ,len))))

;;; If arg is a constant power of two, turn floor into a shift and mask.
;;; If ceiling, add in (1- (abs y)) and then do floor.
;;;
(flet ((frob (y ceil-p)
	 (unless (constant-continuation-p y) (give-up))
	 (let* ((y (continuation-value y))
		(y-abs (abs y))
		(len (1- (integer-length y-abs))))
	   (unless (= y-abs (ash 1 len)) (give-up))
	   (let ((shift (- len))
		 (mask (1- y-abs)))
	     `(let ,(when ceil-p `((x (+ x ,(1- y-abs)))))
		,(if (minusp y)
		     `(values (ash (- x) ,shift)
			      (- (logand (- x) ,mask)))
		     `(values (ash x ,shift)
			      (logand x ,mask))))))))
  (deftransform floor ((x y) (integer integer))
    (frob y nil))
  (deftransform ceiling ((x y) (integer integer))
    (frob y t)))


;;; Do the same for mod.
;;;
(deftransform mod ((x y) (integer integer))
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (= y-abs (ash 1 len)) (give-up))
    (let ((mask (1- y-abs)))
      (if (minusp y)
	  `(- (logand (- x) ,mask))
	  `(logand x ,mask)))))


;;; If arg is a constant power of two, turn truncate into a shift and mask.
;;;
(deftransform truncate ((x y) (integer integer))
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (= y-abs (ash 1 len)) (give-up))
    (let* ((shift (- len))
	   (mask (1- y-abs)))
      `(if (minusp x)
	   (values ,(if (minusp y)
			`(ash (- x) ,shift)
			`(- (ash (- x) ,shift)))
		   (- (logand (- x) ,mask)))
	   (values ,(if (minusp y)
			`(- (ash (- x) ,shift))
			`(ash x ,shift))
		   (logand x ,mask))))))

;;; And the same for rem.
;;;
(deftransform rem ((x y) (integer integer))
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (= y-abs (ash 1 len)) (give-up))
    (let ((mask (1- y-abs)))
      `(if (minusp x)
	   (- (logand (- x) ,mask))
	   (logand x ,mask)))))


;;; Flush calls to random arith functions that convert to the identity
;;; function.
;;; 
(deftransform ash ((x y) (* (constant-argument (member 0))))
  'x)


;;;; Character operations:

(deftransform char-equal ((a b) (base-character base-character))
  '(let* ((ac (char-code a))
	  (bc (char-code b))
	  (sum (logxor ac bc)))
     (or (zerop sum)
	 (when (eql sum #x20)
	   (let ((sum (+ ac bc)))
	     (and (> sum 161) (< sum 213)))))))

(deftransform char-upcase ((x) (base-character))
  '(let ((n-code (char-code x)))
     (if (and (> n-code #o140)	; Octal 141 is #\a.
	      (< n-code #o173))	; Octal 172 is #\z.
	 (code-char (logxor #x20 n-code))
	 x)))

(deftransform char-downcase ((x) (base-character))
  '(let ((n-code (char-code x)))
     (if (and (> n-code 64)	; 65 is #\A.
	      (< n-code 91))	; 90 is #\Z.
	 (code-char (logxor #x20 n-code))
	 x)))


;;;; Equality predicate transforms:


;;; SAME-LEAF-REF-P  --  Internal
;;;
;;;    Return true if X and Y are continuations whose only use is a reference
;;; to the same leaf.
;;;
(defun same-leaf-ref-p (x y)
  (declare (type continuation x y))
  (let ((x-use (continuation-use x))
	(y-use (continuation-use y)))
    (and (ref-p x-use)
	 (ref-p y-use)
	 (eq (ref-leaf x-use) (ref-leaf y-use)))))


;;; SIMPLE-EQUALITY-TRANSFORM  --  Internal
;;;
;;;    If X and Y are the same leaf, then the result is true.  Otherwise, if
;;; there is no intersection between the types of the arguments, then the
;;; result is definitely false.
;;;
(deftransform simple-equality-transform ((x y) * * :defun-only t)
  (cond ((same-leaf-ref-p x y)
	 't)
	((not (types-intersect (continuation-type x) (continuation-type y)))
	 'nil)
	(t
	 (give-up))))

(dolist (x '(eq char= equal))
  (%deftransform x '(function * *) #'simple-equality-transform))


;;; EQL IR1 Transform  --  Internal
;;;
;;;    Similar to SIMPLE-EQUALITY-PREDICATE, except that we also try to convert
;;; to a type-specific predicate or EQ:
;;; -- If both args are characters, convert to CHAR=.  This is better than just
;;;    converting to EQ, since CHAR= may have special compilation strategies
;;;    for non-standard representations, etc.
;;; -- If either arg is definitely not a number, then we can compare with EQ.
;;; -- Otherwise, we try to put the arg we know more about second.  If X is
;;;    constant then we put it second.  If X is a subtype of Y, we put it
;;;    second.  These rules make it easier for the back end to match these
;;;    interesting cases.
;;; -- If Y is a fixnum, then we quietly pass because the back end can handle
;;;    that case, otherwise give an efficency note.
;;;
(deftransform eql ((x y))
  (let ((x-type (continuation-type x))
	(y-type (continuation-type y))
	(char-type (specifier-type 'character))
	(number-type (specifier-type 'number)))
    (cond ((same-leaf-ref-p x y)
	   't)
	  ((not (types-intersect x-type y-type))
	   'nil)
	  ((and (csubtypep x-type char-type)
		(csubtypep y-type char-type))
	   '(char= x y))
	  ((or (not (types-intersect x-type number-type))
	       (not (types-intersect y-type number-type)))
	   '(eq x y))
	  ((and (not (constant-continuation-p y))
		(or (constant-continuation-p x)
		    (and (csubtypep x-type y-type)
			 (not (csubtypep y-type x-type)))))
	   '(eql y x))
	  ((csubtypep y-type (specifier-type 'fixnum))
	   (give-up))
	  (t
	   (give-up "Not enough type information to open-code.")))))


;;; = IR1 Transform  --  Internal
;;;
;;;    Convert to EQL if both args are the "same" numeric type.  This allows
;;; all of EQL's type-specific expertise to come into play.  "Same" means
;;; either both rational or both floats of the same format.  Complexp must also
;;; be specified and identical.
;;; 
(deftransform = ((x y))
  (let ((x-type (continuation-type x))
	(y-type (continuation-type y)))
    (if (and (numeric-type-p x-type) (numeric-type-p y-type)
	     (let ((x-class (numeric-type-class x-type))
		   (y-class (numeric-type-class y-type))
		   (x-format (numeric-type-format x-type)))
	       (or (and (eq x-class 'float) (eq y-class 'float)
			x-format
			(eq x-format (numeric-type-format y-type)))
		   (and (member x-class '(rational integer))
			(member y-class '(rational integer)))))
	     (let ((x-complexp (numeric-type-complexp x-type)))
	       (and x-complexp
		    (eq x-complexp (numeric-type-complexp y-type)))))
	'(eql x y)
	(give-up "Operands might not be the same type, so can't open code."))))


;;; Numeric-Type-Or-Lose  --  Interface
;;;
;;;    If Cont's type is a numeric type, then return the type, otherwise
;;; GIVE-UP.
;;;
(defun numeric-type-or-lose (cont)
  (declare (type continuation cont))
  (let ((res (continuation-type cont)))
    (unless (numeric-type-p res) (give-up))
    res))


;;; IR1-TRANSFORM-<  --  Internal
;;;
;;;    See if we can statically determine (< X Y) using type information.  If
;;; X's high bound is < Y's low, then X < Y.  Similarly, if X's low is >= to
;;; Y's high, the X >= Y (so return NIL).  If not, at least make sure any
;;; constant arg is second.
;;;
(defun ir1-transform-< (x y first second inverse)
  (if (same-leaf-ref-p x y)
      'nil
      (let* ((x-type (numeric-type-or-lose x))
	     (x-lo (numeric-type-low x-type))
	     (x-hi (numeric-type-high x-type))
	     (y-type (numeric-type-or-lose y))
	     (y-lo (numeric-type-low y-type))
	     (y-hi (numeric-type-high y-type)))
	(cond ((and x-hi y-lo (< x-hi y-lo))
	       't)
	      ((and y-hi x-lo (>= x-lo y-hi))
	       'nil)
	      ((and (constant-continuation-p first)
		    (not (constant-continuation-p second)))
	       `(,inverse y x))
	      (t
	       (give-up))))))
	      

(deftransform < ((x y) (integer integer))
  (ir1-transform-< x y x y '>))

(deftransform > ((x y) (integer integer))
  (ir1-transform-< y x x y '<))


;;;; Converting N-arg comparisons:
;;;
;;;    We convert calls to N-arg comparison functions such as < into two-arg
;;; calls.  This transformation is enabled for all such comparisons in this
;;; file.  If any of these predicates are not open-coded, then the
;;; transformation should be removed at some point to avoid pessimization.

;;; Multi-Compare  --  Internal
;;;
;;;    This function is used for source transformation of N-arg comparison
;;; functions other than inequality.  We deal both with converting to two-arg
;;; calls and inverting the sense of the test, if necessary.  If the call has
;;; two args, then we pass or return a negated test as appropriate.  If it is a
;;; degenerate one-arg call, then we transform to code that returns true.
;;; Otherwise, we bind all the arguments and expand into a bunch of IFs.
;;;
(proclaim '(function multi-compare (symbol list boolean)))
(defun multi-compare (predicate args not-p)
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
	  ((= nargs 1) `(progn ,@args t))
	  ((= nargs 2)
	   (if not-p
	       `(if (,predicate ,(first args) ,(second args)) nil t)
	       (values nil t)))
	  (t
	   (do* ((i (1- nargs) (1- i))
		 (last nil current)
		 (current (gensym) (gensym))
		 (vars (list current) (cons current vars))
		 (result 't (if not-p
				`(if (,predicate ,current ,last)
				     nil ,result)
				`(if (,predicate ,current ,last)
				     ,result nil))))
	       ((zerop i)
		`((lambda ,vars ,result) . ,args)))))))


(def-source-transform = (&rest args) (multi-compare '= args nil))
(def-source-transform < (&rest args) (multi-compare '< args nil))
(def-source-transform > (&rest args) (multi-compare '> args nil))
(def-source-transform <= (&rest args) (multi-compare '> args t))
(def-source-transform >= (&rest args) (multi-compare '< args t))

(def-source-transform char= (&rest args) (multi-compare 'char= args nil))
(def-source-transform char< (&rest args) (multi-compare 'char< args nil))
(def-source-transform char> (&rest args) (multi-compare 'char> args nil))
(def-source-transform char<= (&rest args) (multi-compare 'char> args t))
(def-source-transform char>= (&rest args) (multi-compare 'char< args t))

(def-source-transform char-equal (&rest args) (multi-compare 'char-equal args nil))
(def-source-transform char-lessp (&rest args) (multi-compare 'char-lessp args nil))
(def-source-transform char-greaterp (&rest args) (multi-compare 'char-greaterp args nil))
(def-source-transform char-not-greaterp (&rest args) (multi-compare 'char-greaterp args t))
(def-source-transform char-not-lessp (&rest args) (multi-compare 'char-lessp args t))


;;; Multi-Not-Equal  --  Internal
;;;
;;;    This function does source transformation of N-arg inequality functions
;;; such as /=.  This is similar to Multi-Compare in the <3 arg cases.  If
;;; there are more than two args, then we expand into the appropriate n^2
;;; comparisons only when speed is important.
;;;
(proclaim '(function multi-not-equal (symbol list)))
(defun multi-not-equal (predicate args)
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
	  ((= nargs 1) `(progn ,@args t))
	  ((= nargs 2)
	   `(if (,predicate ,(first args) ,(second args)) nil t))
	  ((not (policy nil (>= speed space) (>= speed cspeed)))
	   (values nil t))
	  (t
	   (collect ((vars))
	     (dotimes (i nargs) (vars (gensym)))
	     (do ((var (vars) next)
		  (next (cdr (vars)) (cdr next))
		  (result 't))
		 ((null next)
		  `((lambda ,(vars) ,result) . ,args))
	       (let ((v1 (first var)))
		 (dolist (v2 next)
		   (setq result `(if (,predicate ,v1 ,v2) nil ,result))))))))))

(def-source-transform /= (&rest args) (multi-not-equal '= args))
(def-source-transform char/= (&rest args) (multi-not-equal 'char= args))
(def-source-transform char-not-equal (&rest args) (multi-not-equal 'char-equal args))


;;; Expand Max and Min into the obvious comparisons.
(def-source-transform max (arg &rest more-args)
  (if (null more-args)
      `(values ,arg)
      (once-only ((arg1 arg)
		  (arg2 `(max ,@more-args)))
	`(if (> ,arg1 ,arg2)
	     ,arg1 ,arg2))))
;;;
(def-source-transform min (arg &rest more-args)
  (if (null more-args)
      `(values ,arg)
      (once-only ((arg1 arg)
		  (arg2 `(min ,@more-args)))
	`(if (< ,arg1 ,arg2)
	     ,arg1 ,arg2))))


;;;; Converting N-arg arithmetic functions:
;;;
;;;    N-arg arithmetic and logic functions are associated into two-arg
;;; versions, and degenerate cases are flushed.

;;; Associate-Arguments  --  Internal
;;;
;;;    Left-associate First-Arg and More-Args using Function.
;;;
(proclaim '(function associate-arguments (symbol t list) list))
(defun associate-arguments (function first-arg more-args)
  (let ((next (rest more-args))
	(arg (first more-args)))
    (if (null next)
	`(,function ,first-arg ,arg)
	(associate-arguments function `(,function ,first-arg ,arg) next))))

;;; Source-Transform-Transitive  --  Internal
;;;
;;;    Do source transformations for transitive functions such as +.  One-arg
;;; cases are replaced with the arg and zero arg cases with the identity.  If
;;; Leaf-Fun is true, then replace two-arg calls with a call to that function. 
;;;
(proclaim '(function source-transform-transitive
		     (symbol list (or symbol null))
		     void))
(defun source-transform-transitive (fun args identity &optional leaf-fun)
  (case (length args)
    (0 identity)
    (1 `(values ,(first args)))
    (2 (if leaf-fun
	   `(,leaf-fun ,(first args) ,(second args))
	   (values nil t)))
    (t
     (associate-arguments fun (first args) (rest args)))))

(def-source-transform + (&rest args) (source-transform-transitive '+ args 0))
(def-source-transform * (&rest args) (source-transform-transitive '* args 1))
(def-source-transform logior (&rest args) (source-transform-transitive 'logior args 0))
(def-source-transform logxor (&rest args) (source-transform-transitive 'logxor args 0))
(def-source-transform logand (&rest args) (source-transform-transitive 'logand args -1))

(def-source-transform logeqv (&rest args)
  (if (evenp (length args))
      `(lognot (logxor ,@args))
      `(logxor ,@args)))

;;; Note: we can't use source-transform-transitive for GCD and LCM because when
;;; they are given one argument, they return it's absolute value.

(def-source-transform gcd (&rest args)
  (case (length args)
    (0 0)
    (1 `(abs (the integer ,(first args))))
    (2 (values nil t))
    (t (associate-arguments 'gcd (first args) (rest args)))))

(def-source-transform lcm (&rest args)
  (case (length args)
    (0 1)
    (1 `(abs (the integer ,(first args))))
    (2 (values nil t))
    (t (associate-arguments 'lcm (first args) (rest args)))))


;;; Source-Transform-Intransitive  --  Internal
;;;
;;;    Do source transformations for intransitive n-arg functions such as /.
;;; With one arg, we form the inverse using the indentity, with two args we
;;; pass, otherwise we associate into two-arg calls.
;;;
(proclaim '(function source-transform-intransitive (symbol list t) list))
(defun source-transform-intransitive (function args identity)
  (case (length args)
    ((0 2) (values nil t))
    (1 `(,function ,identity ,(first args)))
    (t
     (associate-arguments function (first args) (rest args)))))

(def-source-transform - (&rest args) (source-transform-intransitive '- args 0))
(def-source-transform / (&rest args) (source-transform-intransitive '/ args 1))

(deftransform - ((x y))
  (unless (and (constant-continuation-p x) (zerop (continuation-value x)))
    (give-up))
  '(%negate y))


;;;; Array accessors:
;;;
;;;    We convert all array accessors into aref and %aset.
;;;

(def-source-transform svref (a i) `(aref (the simple-vector ,a) ,i))
(def-source-transform %svset (a i v) `(%aset (the simple-vector ,a) ,i ,v))

(def-source-transform schar (a i) `(aref (the simple-string ,a) ,i))
(def-source-transform %scharset (a i v) `(%aset (the simple-string ,a) ,i ,v))
(def-source-transform char (a i) `(aref (the string ,a) ,i))
(def-source-transform %charset (a i v) `(%aset (the string ,a) ,i ,v))

(def-source-transform sbit (a &rest i) `(aref (the (simple-array bit) ,a) ,@i))
(def-source-transform %sbitset (a &rest i) `(%aset (the (simple-array bit) ,a) ,@i))
(def-source-transform bit (a &rest i) `(aref (the (array bit) ,a) ,@i))
(def-source-transform %bitset (a &rest i) `(%aset (the (array bit) ,a) ,@i))


(def-source-transform vector (&rest elements)
  (let ((len (length elements))
	(n -1))
    (once-only ((n-vec `(make-array ,len)))
      `(progn 
	 ,@(mapcar #'(lambda (el)
		       (once-only ((n-val el))
			 `(locally (declare (optimize (safety 0)))
			    (setf (svref ,n-vec ,(incf n)) ,n-val))))
		   elements)
	 ,n-vec))))


(deftransform make-array ((length &key initial-element element-type)
			  (integer &rest *))
  (let* ((eltype (cond ((not element-type) t)
		       ((not (constant-continuation-p element-type))
			(give-up "Element-Type is not constant."))
		       (t
			(continuation-value element-type))))
	 (len (if (constant-continuation-p length)
		  (continuation-value length)
		  '*))
	 (spec `(simple-array ,eltype (,len)))
	 (type (specifier-type spec)))
    (cond ((csubtypep type (specifier-type 'simple-string))
	   (when initial-element
	     (give-up "Can't hack initial elements in strings."))
	   `(truly-the ,spec (%primitive alloc-string (the index length))))
	  ((csubtypep type (specifier-type 'simple-bit-vector))
	   (unless (or (not initial-element)
		       (and (constant-continuation-p initial-element)
			    (eql (continuation-value initial-element) 0)))
	     (give-up "Can't hack non-zero initial-elements in bit-vectors."))
	   `(truly-the ,spec
		       (%primitive allocate-vector
				   vm:simple-bit-vector-type
				   (the index length)
				   (the index
					(ceiling length vm:word-bits)))))
	  ((csubtypep type (specifier-type 'simple-vector))
	   `(truly-the ,spec
		       (%primitive alloc-g-vector
				   (the index length)
				   initial-element)))
	  (t
	   (give-up "Can't open-code creation of ~S."
		    (type-specifier type))))))


;;; ### Should pass though any :INITIAL-ELEMENT to MAKE-ARRAY, but this would
;;; be a pessimization until the compiler can transform MAKE-ARRAY of strings
;;; with initial elements.  Until then, it is faster to call MAKE-STRING than
;;; MAKE-ARRAY.
;;;
(def-source-transform make-string (length)
  `(make-array ,length :element-type 'base-character))


;;; Transforms for various random array properties.  If the property is know
;;; at compile time because of a type spec, use that constant value.

(deftransform array-rank ((array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (if (not (listp dims))
	  (give-up)
	  (length dims)))))

(deftransform array-dimension ((array axis)
			       (array index))
  (unless (constant-continuation-p axis)
    (give-up "Axis not constant."))
  (let ((array-type (continuation-type array))
	(axis (continuation-value axis)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (unless (listp dims)
	(give-up
	 "Array dimensions unknown, must call array-dimension at runtime."))
      (unless (> (length dims) axis)
	(abort-transform "Array has dimensions ~S, ~D is too large."
			 dims axis))
      (let ((dim (nth axis dims)))
	(cond ((integerp dim)
	       dim)
	      ((= (length dims) 1)
	       (ecase (array-type-complexp array-type)
		 ((t)
		  '(%array-dimension array 0))
		 ((nil)
		  '(length array))
		 (*
		  (give-up "Can't tell if array is simple."))))
	      (t
	       '(%array-dimension array axis)))))))

(deftransform length ((vector)
		      ((simple-array * (*))))
  (let ((type (continuation-type vector)))
    (unless (array-type-p type)
      (give-up))
    (let ((dims (array-type-dimensions type)))
      (unless (and (listp dims) (integerp (car dims)))
	(give-up "Vector length unknown, must call length at runtime."))
      (car dims))))

(deftransform length ((vector) (vector))
  '(vector-length vector))

(deftransform array-total-size ((array)
				(array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (unless (listp dims)
	(give-up)
	(if (member '* dims)
	    (do ((form 1 `(truly-the index
				     (* (truly-the index
						   (array-dimension array ,i))
					,form)))
		 (i 0 (1+ i)))
		((= i (length dims)) form))
	    (reduce #'* dims))))))

(deftransform array-has-fill-pointer-p ((array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (if (and (listp dims) (not (= (length dims) 1)))
	  nil
	  (ecase (array-type-complexp array-type)
	    ((t)
	     t)
	    ((nil)
	     nil)
	    (*
	     (give-up "Array type ambiguous; must call ~
	              array-has-fill-pointer-p at runtime.")))))))

;;; Primitive used to verify indicies into arrays.  If we can tell at
;;; compile-time or we are generating unsafe code, don't bother with the VOP.

(deftransform %check-bound ((array dimension index))
  (unless (constant-continuation-p dimension)
    (give-up))
  (let ((dim (continuation-value dimension)))
    `(the (integer 0 ,dim) index)))

(deftransform %check-bound ((array dimension index) * *
			    :policy (and (> speed safety) (= safety 0)))
  'index)

;;; Array accessor/setter transforms.

(defmacro with-row-major-index ((array indices index &optional new-value)
				&rest body)
  `(let (n-indices dims)
     (dotimes (i (length ,indices))
       (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
       (push (make-symbol (format nil "DIM-~D" i)) dims))
     (setf n-indices (nreverse n-indices))
     (setf dims (nreverse dims))
     `(lambda (,',array ,@n-indices ,@',(when new-value (list new-value)))
	(let* (,@(let ((,index -1))
		   (mapcar #'(lambda (name)
			       `(,name (array-dimension ,',array
							,(incf ,index))))
			   dims))
	       (,',index
		,(if (null dims)
		     0
		     (do* ((dims dims (cdr dims))
			   (indices n-indices (cdr indices))
			   (last-dim nil (car dims))
			   (form `(%check-bound ,',array
						,(car dims)
						,(car indices))
				 `(truly-the index
					     (+ (truly-the index
							   (* ,form
							      ,last-dim))
						(%check-bound
						 ,',array
						 ,(car dims)
						 ,(car indices))))))
			  ((null (cdr dims)) form)))))
	  ,',@body))))

(deftransform array-row-major-index ((array &rest indices))
  (with-row-major-index (array indices index)
    index))

(deftransform aref ((array &rest indices))
  (with-row-major-index (array indices index)
    (data-vector-ref array index)))

(deftransform %aset ((array &rest stuff))
  (let ((indices (butlast stuff)))
    (with-row-major-index (array indices index new-value)
      (data-vector-set array index new-value))))

(deftransform row-major-aref ((array index))
  `(data-vector-ref array (%check-bound array (array-total-size array) index)))

(deftransform %set-row-major-aref ((array index new-value))
  `(data-vector-set array
		    (%check-bound array (array-total-size array) index)
		    new-value))



;;;; Apply:
;;;
;;;    We convert Apply into Multiple-Value-Call so that the compiler only
;;; needs to understand one kind of variable-argument call.  It is more
;;; efficient to convert Apply to MV-Call than MV-Call to Apply.

(def-source-transform apply (fun arg &rest more-args)
  (let ((args (cons arg more-args)))
    `(multiple-value-call ,fun
       ,@(mapcar #'(lambda (x)
		     `(values ,x))
		 (butlast args))
       (values-list ,(car (last args))))))


;;;; FORMAT transform:

;;; A transform for FORMAT, based on the original (courtesy of Skef.)
;;;
(deftransform format ((stream control &rest args)
		      ((or (member t) stream) simple-string &rest t))
  (unless (constant-continuation-p control)
    (give-up "Control string is not a constant."))
  (let* ((control (continuation-value control))
	 (end (length control))
	 (penultimus (1- end))
	 (stream-form (if (csubtypep (continuation-type stream)
				     (specifier-type 'stream))
			  `(stream)
			  ()))
	 (arg-vars (mapcar #'(lambda (x)
			       (declare (ignore x))
			       (gensym))
			   args))
	 (args arg-vars)
	 (index 0))
    (declare (simple-string control))
    (collect ((forms))
      (loop
	(let ((command-index (position #\~ control :start index)))
	  (unless command-index
	    ;; Write out the final part of the string.
	    (forms `(write-string ,(subseq control index end)
				  ,@stream-form))
	    (return `(lambda (stream control ,@arg-vars)
		       (declare (ignorable stream control))
		       ,@(forms)
		       nil)))

	  (when (= command-index penultimus)
	    (abort-transform "FORMAT control string ends in a ~~: ~S"
			     control))

	  ;; Non-command stuff gets write-string'ed out.
	  (when (/= index command-index)
	    (forms `(write-string
		     ,(subseq control index command-index)
		     ,@stream-form)))
	  
	  ;; Get the format directive.
	  (forms
	   (case (schar control (1+ command-index))
	     ((#\b #\B) `(let ((*print-base* 2))
			   (princ ,(pop args) ,@stream-form)))
	     ((#\o #\O) `(let ((*print-base* 8))
			   (princ ,(pop args) ,@stream-form)))
	     ((#\d #\D) `(let ((*print-base* 10))
			   (princ ,(pop args) ,@stream-form)))
	     ((#\x #\X) `(let ((*print-base* 16))
			   (princ ,(pop args) ,@stream-form)))
	     ((#\a #\A) `(princ ,(pop args) ,@stream-form))
	     ((#\s #\S) `(prin1 ,(pop args) ,@stream-form))
	     (#\% `(terpri ,@stream-form))
	     (#\& `(fresh-line ,@stream-form))
	     (#\| `(write-char #\form ,@stream-form))
	     (#\~ `(write-char #\~ ,@stream-form))
	     (#\newline
	      (let ((new-pos (position-if-not
			      #'lisp::whitespace-char-p
			      control
			      :start (+ command-index 2))))
		(if new-pos
		    (setq command-index (- new-pos 2)))))
	     (t
	      (give-up))))

	  (setq index (+ command-index 2)))))))
