;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/float-tran.lisp,v 1.38 1997/10/08 19:41:16 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating-point specific transforms, and may be somewhat
;;; implementation dependent in its assumptions of what the formats are.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "C")


;;;; Coercions:

(defknown %single-float (real) single-float (movable foldable flushable))
(defknown %double-float (real) double-float (movable foldable flushable))

(deftransform float ((n &optional f) (* &optional single-float) *
		     :when :both)
		     
  '(%single-float n))

(deftransform float ((n f) (* double-float) * :when :both)
  '(%double-float n))

(deftransform %single-float ((n) (single-float) * :when :both)
  'n)

(deftransform %double-float ((n) (double-float) * :when :both)
  'n)

(deftransform coerce ((n type) (* *) * :when :both)
  (unless (constant-continuation-p type)
    (give-up))
  `(the ,(continuation-value type)
	,(let ( (tspec (specifier-type (continuation-value type))) )
	   (cond ((csubtypep tspec (specifier-type 'double-float))	
		  '(%double-float n))	
		 ((csubtypep tspec (specifier-type 'float))
		  '(%single-float n))
		 (t
		  (give-up))))))

;;; Not strictly float functions, but primarily useful on floats:
;;;
(macrolet ((frob (fun ufun)
	     `(progn
		(defknown ,ufun (real) integer (movable foldable flushable))
		(deftransform ,fun ((x &optional by)
				    (* &optional
				       (constant-argument (member 1))))
		  '(let ((res (,ufun x)))
		     (values res (- x res)))))))
  (frob truncate %unary-truncate)
  (frob round %unary-round))

;;; Random:
;;;
(macrolet ((frob (fun type)
	     `(deftransform random ((num &optional state)
				    (,type &optional *) *
				    :when :both)
		"use inline float operations"
		'(,fun num (or state *random-state*)))))
  (frob %random-single-float single-float)
  (frob %random-double-float double-float))

#-new-random
(deftransform random ((num &optional state)
		      ((integer 1 #.random-fixnum-max) &optional *))
  "use inline fixnum operations"
  '(rem (random-chunk (or state *random-state*)) num))

;;; With the latest propagate-float-type code the compiler can inline
;;; truncate (signed-byte 32) allowing 31 bits, and (unsigned-byte 32)
;;; 32 bits on the x86. When not using the propagate-float-type
;;; feature the best size that can be inlined is 29 bits.  The choice
;;; shouldn't cause bootstrap problems just slow code.
#+new-random
(deftransform random ((num &optional state)
		      ((integer 1
				#+(and propagate-float-type x86) #xffffffff
				#+(and propagate-float-type (not x86)) #x7fffffff
				#-propagate-float-type #.most-positive-fixnum)
		       &optional *))
  #+x86 "use inline (unsigned-byte 32) operations"
  #-x86 "use inline (signed-byte 32) operations"
  '(values (truncate (%random-double-float (coerce num 'double-float)
		      (or state *random-state*)))))

;;;; Float accessors:

(defknown make-single-float ((signed-byte 32)) single-float
  (movable foldable flushable))

(defknown make-double-float ((signed-byte 32) (unsigned-byte 32)) double-float
  (movable foldable flushable))

(defknown single-float-bits (single-float) (signed-byte 32)
  (movable foldable flushable))

(defknown double-float-high-bits (double-float) (signed-byte 32)
  (movable foldable flushable))

(defknown double-float-low-bits (double-float) (unsigned-byte 32)
  (movable foldable flushable))

(def-source-transform float-sign (float1 &optional (float2 nil f2-p))
  (cond ((byte-compiling) (values nil t))
	(f2-p `(* (float-sign ,float1) (abs ,float2)))
	(t
	 (let ((n-f1 (gensym)))
	   `(let ((,n-f1 ,float1))
	      (declare (float ,n-f1))
	      (if (minusp (if (typep ,n-f1 'single-float)
			      (single-float-bits ,n-f1)
			      (double-float-high-bits ,n-f1)))
		  (float -1 ,n-f1)
		  (float 1 ,n-f1)))))))


;;;; DECODE-FLOAT, INTEGER-DECODE-FLOAT, SCALE-FLOAT:
;;;
;;;    Convert these operations to format specific versions when the format is
;;; known.
;;;

(deftype single-float-exponent ()
  `(integer ,(- vm:single-float-normal-exponent-min vm:single-float-bias
		vm:single-float-digits)
	    ,(- vm:single-float-normal-exponent-max vm:single-float-bias)))

(deftype double-float-exponent ()
  `(integer ,(- vm:double-float-normal-exponent-min vm:double-float-bias
		vm:double-float-digits)
	    ,(- vm:double-float-normal-exponent-max vm:double-float-bias)))


(deftype single-float-int-exponent ()
  `(integer ,(- vm:single-float-normal-exponent-min vm:single-float-bias
		(* vm:single-float-digits 2))
	    ,(- vm:single-float-normal-exponent-max vm:single-float-bias
		vm:single-float-digits)))

(deftype double-float-int-exponent ()
  `(integer ,(- vm:double-float-normal-exponent-min vm:double-float-bias
		(* vm:double-float-digits 2))
	    ,(- vm:double-float-normal-exponent-max vm:double-float-bias
		vm:double-float-digits)))

(deftype single-float-significand ()
  `(integer 0 (,(ash 1 vm:single-float-digits))))

(deftype double-float-significand ()
  `(integer 0 (,(ash 1 vm:double-float-digits))))

(defknown decode-single-float (single-float)
  (values single-float single-float-exponent (single-float -1f0 1f0))
  (movable foldable flushable))

(defknown decode-double-float (double-float)
  (values double-float double-float-exponent (double-float -1d0 1d0))
  (movable foldable flushable))

(defknown integer-decode-single-float (single-float)
  (values single-float-significand single-float-int-exponent (integer -1 1))
  (movable foldable flushable))

(defknown integer-decode-double-float (double-float)
  (values double-float-significand double-float-int-exponent (integer -1 1))
  (movable foldable flushable))

(defknown scale-single-float (single-float fixnum) single-float
  (movable foldable flushable))

(defknown scale-double-float (double-float fixnum) double-float
  (movable foldable flushable))

(deftransform decode-float ((x) (single-float) * :when :both)
  '(decode-single-float x))

(deftransform decode-float ((x) (double-float) * :when :both)
  '(decode-double-float x))

(deftransform integer-decode-float ((x) (single-float) * :when :both)
  '(integer-decode-single-float x))

(deftransform integer-decode-float ((x) (double-float) * :when :both)
  '(integer-decode-double-float x))

(deftransform scale-float ((f ex) (single-float *) * :when :both)
  (if (and (backend-featurep :x86)
	   (csubtypep (continuation-type ex)
		      (specifier-type '(signed-byte 32)))
	   (not (byte-compiling)))
      '(coerce (%scalbn (coerce f 'double-float) ex) 'single-float)
      '(scale-single-float f ex)))

(deftransform scale-float ((f ex) (double-float *) * :when :both)
  (if (and (backend-featurep :x86)
	   (csubtypep (continuation-type ex)
		      (specifier-type '(signed-byte 32))))
      '(%scalbn f ex)
      '(scale-double-float f ex)))

;;; toy@rtp.ericsson.se:
;;;
;;; Optimizers for scale-float.  If the float has bounds, new bounds
;;; are computed for the result, if possible.

#+propagate-float-type
(progn
(macrolet
    ((frob (name)
       `(defoptimizer (,name derive-type) ((f ex))
	 (flet ((scale-bound (x n)
		  ;; We need to be a bit careful here and catch any
		  ;; overflows that might occur.  We can ignore
		  ;; underflows which become zeros.
		  (set-bound
		   (handler-case
		       (scale-float (bound-value x) n)
		     (floating-point-overflow ()
		       nil))
		   (consp x))))
	   (let ((f-type (continuation-type f))
		 (ex-type (continuation-type ex)))
	     (when (and (numeric-type-p f-type)
			(numeric-type-p ex-type))
	       (let ((f-lo (numeric-type-low f-type))
		     (f-hi (numeric-type-high f-type))
		     (ex-lo (numeric-type-low ex-type))
		     (ex-hi (numeric-type-high ex-type))
		     (new-lo nil)
		     (new-hi nil))
		 (when (and f-hi ex-hi)
		   (setf new-hi (scale-bound f-hi ex-hi)))
		 (when (and f-lo ex-lo)
		   (setf new-lo (scale-bound f-lo ex-lo)))
		 (make-numeric-type :class (numeric-type-class f-type)
				    :format (numeric-type-format f-type)
				    :complexp :real
				    :low new-lo
				    :high new-hi))))))))
  (frob scale-single-float)
  (frob scale-double-float))
	     
;;; toy@rtp.ericsson.se:
;;;
;;; Defoptimizers for %single-float and %double-float.  This makes the
;;; FLOAT function return the correct ranges if the input has some
;;; defined range.  Quite useful if we want to convert some type of
;;; bounded integer into a float.

(macrolet ((frob (fun type)
	     `(defoptimizer (,fun derive-type) ((num))
	       (let ((num-type (continuation-type num)))
		 (if (or (numeric-type-p num-type)
			   (union-type-p num-type))
		     (elfun-derive-type-union
		      num-type
		      (constantly t)
		      #'(lambda (lo hi)
			  ;; When converting a number to a float, the
			  ;; limits on the resulting float are obviously
			  ;; the same as the original number.
			  (values lo hi ',type))
		      (specifier-type ',type))
		     *universal-type*)))))
  (frob %single-float single-float)
  (frob %double-float double-float))

) ; end progn  


;;;; Float contagion:

;;; FLOAT-CONTAGION-ARG1, ARG2  --  Internal
;;;
;;;    Do some stuff to recognize when the luser is doing mixed float and
;;; rational arithmetic, or different float types, and fix it up.  If we don't,
;;; he won't even get so much as an efficency note.
;;;
(deftransform float-contagion-arg1 ((x y) * * :defun-only t :node node)
  `(,(continuation-function-name (basic-combination-fun node))
    (float x y) y))
;;;
(deftransform float-contagion-arg2 ((x y) * * :defun-only t :node node)
  `(,(continuation-function-name (basic-combination-fun node))
    x (float y x)))

(dolist (x '(+ * / -))
  (%deftransform x '(function (rational float) *) #'float-contagion-arg1)
  (%deftransform x '(function (float rational) *) #'float-contagion-arg2))

(dolist (x '(= < > + * / -))
  (%deftransform x '(function (single-float double-float) *)
		 #'float-contagion-arg1)
  (%deftransform x '(function (double-float single-float) *)
		 #'float-contagion-arg2))


;;; Prevent zerop, plusp, minusp from losing horribly.  We can't in general
;;; float rational args to comparison, since Common Lisp semantics says we are
;;; supposed to compare as rationals, but we can do it for any rational that
;;; has a precise representation as a float (such as 0).
;;;
(macrolet ((frob (op)
	     `(deftransform ,op ((x y) (float rational) * :when :both)
		(unless (constant-continuation-p y)
		  (give-up "Can't open-code float to rational comparison."))
		(let ((val (continuation-value y)))
		  (unless (eql (rational (float val)) val)
		    (give-up "~S doesn't have a precise float representation."
			     val)))
		`(,',op x (float y x)))))
  (frob <)
  (frob >)
  (frob =))


;;;; Irrational derive-type methods:

;;; Derive the result to be float for argument types in the appropriate domain.
;;;
#-propagate-fun-type
(dolist (stuff '((asin (real -1.0 1.0))
		 (acos (real -1.0 1.0))
		 (acosh (real 1.0))
		 (atanh (real -1.0 1.0))
		 (sqrt (real 0.0))))
  (destructuring-bind (name type) stuff
    (let ((type (specifier-type type)))
      (setf (function-info-derive-type (function-info-or-lose name))
	    #'(lambda (call)
		(declare (type combination call))
		(when (csubtypep (continuation-type
				  (first (combination-args call)))
				 type)
		  (specifier-type 'float)))))))

#-propagate-fun-type
(defoptimizer (log derive-type) ((x &optional y))
  (when (and (csubtypep (continuation-type x)
			(specifier-type '(real 0.0)))
	     (or (null y)
		 (csubtypep (continuation-type y)
			    (specifier-type '(real 0.0)))))
    (specifier-type 'float)))


;;;; Irrational transforms:

(defknown (%tan %sinh %asinh %atanh %log %logb %log10 %tan-quick)
	  (double-float) double-float
  (movable foldable flushable))

(defknown (%sin %cos %tanh %sin-quick %cos-quick)
    (double-float) (double-float -1.0d0 1.0d0)
    (movable foldable flushable))

(defknown (%asin %atan)
    (double-float) (double-float #.(- (/ pi 2)) #.(/ pi 2))
    (movable foldable flushable))
    
(defknown (%acos)
    (double-float) (double-float 0.0d0 #.pi)
    (movable foldable flushable))
    
(defknown (%cosh)
    (double-float) (double-float 1.0d0)
    (movable foldable flushable))

(defknown (%acosh %exp %sqrt)
    (double-float) (double-float 0.0d0)
    (movable foldable flushable))

(defknown (%hypot)
    (double-float double-float) (double-float 0d0)
  (movable foldable flushable))

(defknown (%pow)
    (double-float double-float) double-float
  (movable foldable flushable))

(defknown (%atan2)
    (double-float double-float) (double-float #.(- pi) #.pi)
  (movable foldable flushable))

(defknown (%scalb)
    (double-float double-float) double-float
  (movable foldable flushable))

(defknown (%scalbn)
    (double-float (signed-byte 32)) double-float
    (movable foldable flushable))

(defknown (%log1p-limited)
    (double-float) double-float
    (movable foldable flushable))

(dolist (stuff '((exp %exp *)
		 (log %log float)
		 (sqrt %sqrt float)
		 (asin %asin float)
		 (acos %acos float)
		 (atan %atan *)
		 (sinh %sinh *)
		 (cosh %cosh *)
		 (tanh %tanh *)
		 (asinh %asinh *)
		 (acosh %acosh float)
		 (atanh %atanh float)))
  (destructuring-bind (name prim rtype) stuff
    (deftransform name ((x) '(single-float) rtype :eval-name t)
      `(coerce (,prim (coerce x 'double-float)) 'single-float))
    (deftransform name ((x) '(double-float) rtype :eval-name t :when :both)
      `(,prim x))))

;;; The argument range is limited on the x86 FP trig. functions. A
;;; post-test can detect a failure (and load a suitable result), but
;;; this test is avoided if possible.
(dolist (stuff '((sin %sin %sin-quick)
		 (cos %cos %cos-quick)
		 (tan %tan %tan-quick)))
  (destructuring-bind (name prim prim-quick) stuff
    (deftransform name ((x) '(single-float) '* :eval-name t)
      (if (backend-featurep :x86)
	  (cond ((csubtypep (continuation-type x)
			    (specifier-type '(single-float
					      (#.(- (expt 2f0 64)))
					      (#.(expt 2f0 64)))))
		 `(coerce (,prim-quick (coerce x 'double-float))
		   'single-float))
		(t 
		 (compiler-note
		  "Unable to avoid inline argument range check~@
                      because the argument range (~s) was not within 2^64"
		  (type-specifier (continuation-type x)))
		 `(coerce (,prim (coerce x 'double-float)) 'single-float)))
	  `(coerce (,prim (coerce x 'double-float)) 'single-float)))
    (deftransform name ((x) '(double-float) '* :eval-name t :when :both)
      (if (backend-featurep :x86)
	  (cond ((csubtypep (continuation-type x)
			    (specifier-type '(double-float
					      (#.(- (expt 2d0 64)))
					      (#.(expt 2d0 64)))))
		 `(,prim-quick x))
		(t 
		 (compiler-note
		  "Unable to avoid inline argument range check~@
                   because the argument range (~s) was not within 2^64"
		  (type-specifier (continuation-type x)))
		 `(,prim x)))
	  `(,prim x)))))

(deftransform atan ((x y) (single-float single-float) *)
  `(coerce (%atan2 (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform atan ((x y) (double-float double-float) * :when :both)
  `(%atan2 x y))

(deftransform expt ((x y) ((single-float 0f0) single-float) *)
  "convert to inline float expt"
  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform expt ((x y) ((double-float 0d0) double-float) * :when :both)
  "convert to inline float expt"
  `(%pow x y))
(deftransform expt ((x y) ((single-float 0f0) (signed-byte 32)) *)
  "convert to inline float expt"
  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform expt ((x y) ((double-float 0d0) (signed-byte 32)) * :when :both)
  "convert to inline float expt"
  `(%pow x (coerce y 'double-float)))

;;; ANSI says log with base zero returns zero.
(deftransform log ((x y) (float float) float)
  '(if (zerop y) y (/ (log x) (log y))))


;;; Handle some simple transformations
  
(deftransform abs ((x) ((complex double-float)) double-float :when :both)
  '(%hypot (realpart x) (imagpart x)))

(deftransform abs ((x) ((complex single-float)) single-float)
  '(coerce (%hypot (coerce (the single-float (realpart x)) 'double-float)
		  (coerce (the single-float (imagpart x)) 'double-float))
	  'single-float))

(deftransform phase ((x) ((complex double-float)) double-float :when :both)
  '(%atan2 (imagpart x) (realpart x)))

(deftransform phase ((x) ((complex single-float)) single-float)
  '(coerce (%atan2 (coerce (the single-float (imagpart x)) 'double-float)
		  (coerce (the single-float (realpart x)) 'double-float))
	  'single-float))

(deftransform phase ((x) ((float)) float :when :both)
  '(if (minusp (float-sign x))
      (float pi x)
      (float 0 x)))



#+(or propagate-float-type propagate-fun-type)
(progn

;;; The number is of type REAL.
(proclaim '(inline numeric-type-real-p))
(defun numeric-type-real-p (type)
  (and (numeric-type-p type)
       (eq (numeric-type-complexp type) :real)))


;;; Functions to handle most cases of computing the bounds for a
;;; function.
;;;
;;; NUM is a numeric type representing the argument to the
;;; function.
;;;
;;; COND is an function that returns T when the number satisfies the
;;; desired condition.  It should take two arguments LO and HI which
;;; are the lower and upper bounds of the numeric-type.
;;;
;;; LIMIT-FUN is a function that returns the lower and upper
;;; bounds.after applying the desired function. Also, the limit
;;; function can return the preferred type of float, if
;;; necessary. This feature is used by the float optimizer to
;;; determine the desired result type.
;;;
;;; DEFAULT-TYPE is the specifier-type of the result if COND should
;;; return NIL.


(defun elfun-derive-type-1 (num cond limit-fun default-type)
  (declare (type numeric-type num))
  (cond ((and (numeric-type-real-p num)
	      (funcall cond (numeric-type-low num) (numeric-type-high num)))
	 (with-float-traps-masked (:underflow :overflow)
	   ;; The call to the limit-fun has (most) traps disabled.  It
	   ;; can naively compute the result and return infinity for
	   ;; the value.  We convert the infinity to nil as needed.
	   (multiple-value-bind (lo-lim hi-lim float-type)
	       (funcall limit-fun
			(numeric-type-low num)
			(numeric-type-high num))
	     (make-numeric-type :class 'float
				:format (or float-type
					    (elfun-float-format
					     (numeric-type-format num)))
				:complexp :real
				:low (if (and (floatp lo-lim)
					      (float-infinity-p lo-lim))
					 nil
					 lo-lim)
				:high (if (and (floatp hi-lim)
					       (float-infinity-p hi-lim))
					  nil
					  hi-lim)))))
	((eq (numeric-type-complexp num) :complex)
	 (copy-numeric-type num))
	(default-type
	    default-type)
	(t
	 (let ((f-type (or (numeric-type-format num) 'float)))
	   (specifier-type `(or ,f-type (complex ,f-type)))))))

;;; Same as ELFUN-DERIVE-TYPE-1 except we can handle simple
;;; NUMERIC-TYPEs and UNION-TYPEs.
(defun elfun-derive-type-union
    (type cond limit-fun
	  &optional default-type)
  (cond ((union-type-p type)
	 ;; For a UNION-TYPE, run down the list of unions and derive
	 ;; the resulting type of each union and make a UNION-TYPE of
	 ;; the results.
	 (let ((result '()))
	   (dolist (interval (union-type-types type))
	     (let ((derived-type (elfun-derive-type-1 interval cond limit-fun default-type)))
	       (if (union-type-p derived-type)
		   ;; Insert union types in to the result
		   (dolist (item (union-type-types derived-type))
		     (push item result))
		   (push derived-type result))))
	   (make-union-type (derive-merged-union-types result))))
	((numeric-type-p type)
	 (elfun-derive-type-1 type cond limit-fun default-type))
	(t
	 (specifier-type 'number))))
)  ; end progn

#+propagate-fun-type
(progn
;;;; Optimizers for elementary functions
;;;;
;;;; These optimizers compute the output range of the elementary
;;;; function, based on the domain of the input.
;;;;

;;; Handle these monotonic increasing functions whose domain is
;;; possibly part of the real line
(macrolet ((frob (name cond def-lo-bnd def-hi-bnd)
	     (let ((num (gensym))
		   (lo-bnd (gensym))
		   (hi-bnd (gensym)))
	       `(defoptimizer (,name derive-type) ((,num))
		 (elfun-derive-type-union
		  (continuation-type ,num)
		  ,cond
		  #'(lambda (,lo-bnd ,hi-bnd)
		      ;; Since the function is monotonic increasing, the
		      ;; lower bound and the upper bound are the values
		      ;; of the function at the bounds of the input
		      ;; range.
		      (values (or (bound-func #',name ,lo-bnd) ,def-lo-bnd)
			      (or (bound-func #',name ,hi-bnd) ,def-hi-bnd))))))))

  ;; These functions are easy because they are defined for the whole
  ;; real line.
  (frob exp (constantly t)
	0 nil)
  (frob sinh (constantly t)
	nil nil)
  (frob tanh (constantly t)
	-1 1)
  (frob asinh (constantly t)
	nil nil)

  ;; These functions are only defined for part of the real line.  The
  ;; condition selects the desired part of the line.  The default
  ;; return value of (OR FLOAT (COMPLEX FLOAT)) is ok as the default.
  (frob sqrt #'(lambda (lo hi)
		 (declare (ignore hi))
		 (and lo
		      (>= (bound-value lo) 0)))
	0 nil)
  (frob asin #'(lambda (lo hi)
		 (and lo hi
		      (>= (bound-value lo) -1)
		      (<= (bound-value hi) 1)))
	#.(- (/ pi 2)) #.(/ pi 2))
  (frob acosh #'(lambda (lo hi)
		  (declare (ignore hi))
		  (and lo (>= (bound-value lo) 1)))
	nil nil)
  (frob atanh #'(lambda (lo hi)
		  (and lo hi
		       (>= (bound-value lo) -1)
		       (<= (bound-value hi) 1)))
	-1 1))


;;; acos is monotonic decreasing, so we need to swap the function
;;; values at the lower and upper bounds of the input domain.
(defoptimizer (acos derive-type) ((num))
  (elfun-derive-type-union
   (continuation-type num)
   #'(lambda (lo hi)
       (and lo hi
	    (>= (bound-value lo) -1)
	    (<= (bound-value hi) 1)))
   #'(lambda (lo hi)
       (values (bound-func #'acos hi)
	       (bound-func #'acos lo)))))


;;; Optimizer for expt 
#+notyet
(progn
;;; Compute bounds for (expt x y).  This should be easy since (expt x
;;; y) = (exp (* y (log x))).  However, computations done this way
;;; have too much roundoff.  Thus we have to do it the hard way.
  
(defun safe-expt (x y)
  (handler-case
      (expt x y)
    (error ()
      nil)))

;;; Handle the case when x >= 1
(defun interval-expt-> (x y)
  (case (c::interval-range-info y)
    ('+
     ;; Y is positive and log X >= 0.  The range of exp(y * log(x)) is
     ;; obvious.  We just have to be careful for infinite bounds
     ;; (given by nil).
     (let ((lo (safe-expt (c::bound-value (c::interval-low x))
			  (c::bound-value (c::interval-low y))))
	   (hi (safe-expt (c::bound-value (c::interval-high x))
			  (c::bound-value (c::interval-high y)))))
       (c::make-interval :low lo :high hi)))
    ('-
     ;; Y is negative and log x >= 0.  The range of exp(y * log(x)) is
     ;; obvious.  However, underflow (nil) means 0 is the result
     (let ((lo (safe-expt (c::bound-value (c::interval-high x))
			  (c::bound-value (c::interval-low y))))
	   (hi (safe-expt (c::bound-value (c::interval-low x))
			  (c::bound-value (c::interval-high y)))))
       (c::make-interval :low (or lo 0) :high (or hi 0))))
    (t
     ;; Split the interval in half
     (destructuring-bind (y- y+)
	 (c::interval-split 0 y t)
       (list (interval-expt-> x y-)
	     (interval-expt-> x y+))))))

;;; Handle the case when 0<= x <= 1
(defun interval-expt-< (x y)
  (case (c::interval-range-info y)
    ('+
     ;; Y is positive and log X <= 0.  The range of exp(y * log(x)) is
     ;; obvious.  We just have to be careful for infinite bounds
     ;; (given by nil).
     (let ((lo (safe-expt (c::bound-value (c::interval-low x))
			  (c::bound-value (c::interval-high y))))
	   (hi (safe-expt (c::bound-value (c::interval-high x))
			  (c::bound-value (c::interval-low y)))))
       (c::make-interval :low lo :high hi)))
    ('-
     ;; Y is negative and log x <= 0.  The range of exp(y * log(x)) is
     ;; obvious.
     (let ((hi (safe-expt (c::bound-value (c::interval-low x))
			  (c::bound-value (c::interval-low y))))
	   (lo (safe-expt (c::bound-value (c::interval-high x))
			  (c::bound-value (c::interval-high y)))))
       (c::make-interval :low (or lo 0) :high (or hi 0))))
    (t
     ;; Split the interval in half
     (destructuring-bind (y- y+)
	 (c::interval-split 0 y t)
       (list (interval-expt-< x y-)
	     (interval-expt-< x y+))))))

;;; Compute bounds for (expt x y)
(defun interval-expt (x y)
  (cond
	((or (c::interval-< x (c::make-interval :low 1 :high 1))
	     (and (c::interval-high x)
		  (<= (c::bound-value (c::interval-high x)) 1)))
	 ;; X is definitely less than or equal 1
	 (interval-expt-< x y))
	((or (c::interval-< (c::make-interval :low 1 :high 1) x)
	     (and (c::interval-low x)
		  (>= (c::bound-value (c::interval-low x))) 1))
	 ;; X definitely greater than or equal to 1
	 (interval-expt-> x y))
	(
	 ;; Interval contains 1, so we need to break the problem into
	 ;; two pieces
	 (destructuring-bind (left right)
	     (c::interval-split 1 x t t)
	   (list (interval-expt left y)
		 (interval-expt right y))))))

;; Derive the type of (expt x-type y-type)
(defun expt-derive-type-aux (x-type y-type)
  (let ((x-int (numeric-type->interval x-type))
	(y-int (numeric-type->interval y-type)))
    (if (or (eq (numeric-type-complexp x-type) :complex)
	    (eq (numeric-type-complexp y-type) :complex))
	(numeric-contagion x-type y-type)
	;; Several cases to consider
	(cond ((>= (bound-value (interval-low x-int)) 0)
	       ;; A positive number to some power is fairly easy to handle.
	       (let ((bnd (interval-expt x-int y-int)))
		 (cond ((atom bnd)
			(fixup-interval-expt bnd x-int y-int x-type y-type))
		       ((listp bnd)
			(let ((union '()))
			  (dolist (type bnd (first (merge-types-aux union)))
			    (push (fixup-interval-expt type x-int y-int x-type y-type)
				  union))))
		       (t
			(error "Shouldn't happen!")))))
	      (t
	       ;; A number to some power.  We punt here.
	       (c::specifier-type '(or float (complex float))))))))
		       
  
(defun fixup-interval-expt (bnd x-int y-int x-type y-type)
  (let ((lo (bound-value (interval-low bnd)))
	(hi (bound-value (interval-high bnd))))
    ;; Figure out what the return type should be
    (multiple-value-bind (class format)
	(cond ((eq (numeric-type-class x-type) 'integer)
	       (case (numeric-type-class y-type)
		 (integer
		  ;; Positive integer to a integer power
		  (if (>= (bound-value (interval-low y-int)) 0)
		      (values 'integer nil)
		      (values 'rational nil)))
		 ((or rational float)
		  ;; Integer to rational or float power is a float.
		  (values 'float (or (numeric-type-format y-type) 'single-float)))))
	      (t
	       ;; Rational or float to a power is general numeric contagion
	       (values 'float (numeric-type-format (numeric-contagion x-type y-type)))))
      (when (member format '(single-float double-float))
	(setf lo (coerce lo format))
	(setf hi (coerce hi format)))
      (make-numeric-type
       :class class
       :format format
       :low lo
       :high hi))))
  
(defoptimizer (expt derive-type) ((x y))
  (let ((x-type (continuation-type x))
	(y-type (continuation-type y)))
    (derive-real-numeric-or-union-type x-type y-type #'expt-derive-type-aux)))



)  ; end progn


;;; Compute return type for EXPT.  No bounds are computed because
;;; that's pretty complicated in general.  We only return a lower
;;; bound of 0 if we know that 0 is a lower bound.  The general case
;;; will have to wait for another day.

(defoptimizer (expt derive-type) ((x y))
  (let ((x-type (continuation-type x))
	(y-type (continuation-type y)))
    (when (and (numeric-type-p x-type) (numeric-type-p y-type))
      (let ((x-lo (numeric-type-low x-type))
	    (y-lo (numeric-type-low y-type)))
	;; There are several cases to handle for Y.  We only handle
	;; the case where Y is an integer and Y is a float.  For Y
	;; rational, the result could be a rational or float.

	(cond ((eq (numeric-type-class y-type) 'integer)
	       (cond ((and y-lo (>= (bound-value y-lo) 0))
		      ;; When the power is a non-negative integer, the
		      ;; result is of the same class and format as the
		      ;; base.  If the base is known to be
		      ;; non-negative, the result is too.  We can
		      ;; easily compute the bound on the result in
		      ;; this case, but that's for another day.
		      (make-numeric-type
		       :class (numeric-type-class x-type)
		       :format (numeric-type-format x-type)
		       :complexp :real
		       :low (if (and x-lo (>= (bound-value x-lo) 0))
				0
				nil)
		       :high nil))
		     (t
		      ;; The power maybe positive or negative.
		      (let ((pos (and x-lo (>= (bound-value x-lo) 0))))
			;; If the base is positive, the result is
			;; positive.
			(cond ((member (numeric-type-class x-type)
				       '(integer rational))
			       ;; Rational raised to a non-negative
			       ;; integral power is rational.
			       (make-numeric-type
				:class 'rational
				:complexp :real
				:low (if pos 0 nil)
				:high nil))
			      (t
			       ;; Float raised to integral power is a
			       ;; float.
			       (make-numeric-type
				:class 'float
				:format (numeric-type-format x-type)
				:complexp :real
				:low (if pos 0 nil)
				:high nil)))))))
	      ((eq (numeric-type-class y-type) 'float)
	       ;; The power is a float.  The result is either a float
	       ;; or complex.  We punt if the result is complex.
	       (let ((pos (and x-lo (>= (bound-value x-lo) 0))))
		 ;; If both the base and the power are non-negative,
		 ;; the result is a non-negative real.  In any case
		 ;; the result is a float of some time.
		 (make-numeric-type
		  :class 'float
		  :format (if (or (eq (numeric-type-format x-type)
				      'double-float)
				  (eq (numeric-type-format y-type)
				      'double-float))
			      'double-float
			      'single-float)
		  :complexp :real
		  :low (if pos 0 nil)
		  :high nil))))))))

(defoptimizer (log derive-type) ((x &optional y))
  (flet ((derive-type (arg)
	   (elfun-derive-type-union
	    (continuation-type arg)
	    #'(lambda (lo hi)
		(declare (ignore hi))
		(and lo
		     (>= (bound-value lo) 0)))
	    #'(lambda (lo hi)
		(values
		 (if (zerop (bound-value lo))
		     nil
		     (set-bound (log (bound-value lo)) (consp lo)))
		 (if hi
		     (set-bound (log (bound-value hi)) (consp hi))
		     nil))))))
    (cond ((null y)
	   ;; The easy one arg case
	   (derive-type x))
	  (t
	   ;; The hard case with a base given.  Use the definition of
	   ;; (log x y) = (/ (log x) (log y)) to figure out what the
	   ;; answer should be.
	   (let ((log-x (derive-type x))
		 (log-y (derive-type y)))
	     (cond ((and (numeric-type-real-p log-x)
			 (numeric-type-real-p log-y))
		    ;; This stolen from the optimizer for /. 
		    (derive-real-numeric-or-union-type
		     log-x log-y
		     #'(lambda (x y)
			 (declare (type numeric-type x y))
			 (let ((result (interval-div (numeric-type->interval x)
						     (numeric-type->interval y)))
			       (result-type (numeric-contagion x y)))
			   ;; If the result type is a float, we need to be sure to
			   ;; coerce the bounds into the correct type.
			   (when (eq (numeric-type-class result-type) 'float)
			     (setf result (interval-func
					   #'(lambda (x)
					       (coerce x (or (numeric-type-format result-type)
							     'float)))
					   result)))
			   (values (interval-low result)
				   (interval-high result)
				   (numeric-type-class result-type)
				   (numeric-type-format result-type))))))
		   (t
		    ;; Should do a better job than this and specialize
		    ;; the float type if we can.
		    (specifier-type '(or float (complex float))))))))))

(defoptimizer (atan derive-type) ((y &optional x))
  (cond ((null x)
	 ;; Let's handle the easy one arg case
	 (elfun-derive-type-union
	  (continuation-type y)
	  #'(lambda (lo hi)
	      (declare (ignore lo hi))
	      t)
	  #'(lambda (lo hi)
	      (values (or (bound-func #'atan lo) #.(- (/ pi 2)))
		      (or (bound-func #'atan hi) #.(/ pi 2))))))
	(t
	 ;; Here is the hard case with two args.  However, we punt on
	 ;; it, and just return the max bounds.
	 (when (numeric-type-real-p (continuation-type x))
	   (make-numeric-type
	    :class 'float
	    :format (float-format-max
		     (numeric-type-format (continuation-type y))
		     (numeric-type-format (continuation-type x)))
	    :complexp :real
	    :low #.(- pi)
	    :high #.pi)))))
      

(defoptimizer (cosh derive-type) ((num))
  (elfun-derive-type-union
   (continuation-type num)
   #'(lambda (lo hi)
       (declare (ignore lo hi))
       t)
   #'(lambda (lo hi)
       ;; Note that cosh(x) = cosh(|x|), and that cosh is monotonic
       ;; increasing for the positive line.
       (let ((x (interval-abs (make-interval :low lo :high hi))))
	 (values (bound-func #'cosh (interval-low x))
		 (bound-func #'cosh (interval-high x)))))))


(defun phase-derive-type-aux (type)
  ;; Warning: This optimizer doesn't yet handle the case of -0.0.
  ;; It returns 0 for this case instead of pi.  Need to fix this.
  (cond ((numeric-type-real-p type)
	 (case (interval-range-info (numeric-type->interval type))
	   ('+
	    ;; The number is positive, so the phase is 0.
	    (make-numeric-type :class 'float
			       :format (elfun-float-format
					(numeric-type-format type))
			       :complexp :real
			       :low 0
			       :high 0))
	   ('-
	    ;; The number is always negative, so the phase is pi
	    (make-numeric-type :class 'float
			       :format (elfun-float-format
					(numeric-type-format type))
			       :complexp :real
			       :low pi
			       :high pi))
	   (t
	    ;; We can't tell.  The result is 0 or pi.  Use a union
	    ;; type for this
	    (list
	     (make-numeric-type :class 'float
				:format (elfun-float-format
					 (numeric-type-format type))
				:complexp :real
				:low 0
				:high 0)
	     (make-numeric-type :class 'float
				:format (elfun-float-format
					 (numeric-type-format type))
				:complexp :real
				:low pi
				:high pi)))))
	(t
	 ;; We have a complex number.  The answer is the range -pi
	 ;; to pi.  (-pi is included because we have -0.)
	 (make-numeric-type :class 'float
			    :format (elfun-float-format
				     (numeric-type-format type))
			    :complexp :real
			    :low #.(- pi)
			    :high pi))))

(defoptimizer (phase derive-type) ((num))
  (let ((type (continuation-type num)))
    (cond ((numeric-type-real-p type)
	   (let ((res (phase-derive-type-aux type)))
	     (if (listp res)
		 (make-union-type res)
		 res)))
	  ((union-type-p type)
	   ;; Run down the list and process each type
	   (let ((result '()))
	     (dolist (interval (union-type-types type))
	       (let ((res-1 (phase-derive-type-aux interval)))
		 (cond ((listp res-1)
			(push (first res-1) result)
			(push (second res-1) result))
		       (t
			(push res-1 result)))))
	     (make-union-type (derive-merged-union-types result)))))))
		 
) ;end progn for propagate-fun-type

;;; Make REALPART and IMAGPART return the appropriate types.  This
;;; helps a lot in optimized code.

;;; Doesn't work yet.
#+nil
(progn
(defknown (%realpart)
    (complex) real
    (flushable movable))

(defknown (%imagpart)
    (complex) real
    (flushable movable))

(defoptimizer (%realpart derive-type) ((num))
  (let ((type (continuation-type num)))
    (cond ((numeric-type-real-p type)
	   ;; The realpart of a real has the same type and range as the input.
	   (make-numeric-type :class (numeric-type-class type)
			      :format (numeric-type-format type)
			      :complexp :real
			      :low (numeric-type-low type)
			      :high (numeric-type-high type)))
	  (t
	   ;; We have a complex number.  The result has the same type
	   ;; as the real part, except that it's real, not complex,
	   ;; obviously.
	   (make-numeric-type :class (numeric-type-class type)
			      :format (numeric-type-format type)
			      :complexp :real
			      :low (numeric-type-low type)
			      :high (numeric-type-high type))))))

(defoptimizer (%imagpart derive-type) ((num))
  (let ((type (continuation-type num)))
    (cond ((numeric-type-real-p type)
	   ;; The imagpart of a real has the same type as the input,
	   ;; except that it's zero
	   (make-numeric-type :class (numeric-type-class type)
			      :format (numeric-type-format type)
			      :complexp :real
			      :low 0
			      :high 0))
	  (t
	   ;; We have a complex number.  The result has the same type
	   ;; as the imaginary part, except that it's real, not complex,
	   ;; obviously.
	   (make-numeric-type :class (numeric-type-class type)
			      :format (numeric-type-format type)
			      :complexp :real
			      :low (numeric-type-low type)
			      :high (numeric-type-high type))))))

  
) ;end progn
	   

;;;; Here are the optimizers for sin, cos, and tan.  While computing
;;;; the bounds for these functions is easy, I'm not sure about the
;;;; reliability of these functions.  Mainly, these functions do a
;;;; range reduction which may not be exactly the same as done in the
;;;; trig functions so that roundoff may signficantly affect the
;;;; limits returned.
;;;;
;;;; Thus, if x is (double-float 1d0 #.pi), the sin optimizer would
;;;; return (double-float 1.2246467991473534d-16
;;;; 0.8414709848078965d0), which is close to but not the same as
;;;; (double-float 0d0 0.8414709848078965d0), which is closer to the
;;;; truth.
;;;;
;;;; Proceed at your own risk here by adding propagate-trig-fun to
;;;; your *features*.

#+(and propagate-fun-type propagate-trig-fun)
(progn
(defoptimizer (tan derive-type) ((num))
  (let ((type (continuation-type num)))
    (when (numeric-type-real-p type)
      (let ((xl (numeric-type-low type))
	    (xh (numeric-type-high type)))
	(cond ((and xl xh
		    (<= (- (bound-value xh) (bound-value xl)) pi))
	       ;; We have a bounded input range and the range covers
	       ;; no more than one period.  We can compute bounds now.
	       ;; We reduce the argument range to a single period.
	       (multiple-value-bind (npi x-lo)
		   (truncate (bound-value xl) pi)
		 (let* ((x-hi (- (bound-value xh) (* npi pi))))
		   ;; If pi/2 is in the input range, the domain is the
		   ;; whole real line.
		   (format t "x-lo, x-hi = ~s ~s~%" x-lo x-hi)
		   (if (<= x-lo #.(* 0.5d0 pi) x-hi)
		       (make-numeric-type
			:class 'float
			:format (elfun-float-format (numeric-type-format type))
			:complexp :real
			:low nil
			:high nil)
		       (make-numeric-type
			:class 'float
			:format (elfun-float-format (numeric-type-format type))
			:complexp :real
			:low (set-bound (tan x-lo) (consp x-lo))
			:high (set-bound (tan x-hi) (consp x-hi)))))))
	      (t
	       ;; The range covers more than one period, so the answer
	       ;; is obvious.
	       (make-numeric-type
		:class 'float
		:format (elfun-float-format (numeric-type-format type))
		:complexp :real
		:low nil
		:high nil)))))))


(defun trig-limits (num fun bound-one bound-minus-one)
  (let ((type (continuation-type num)))
    (when (numeric-type-real-p type)
      (let ((two-pi #.(* 2 pi))
	    (xl (numeric-type-low type))
	    (xh (numeric-type-high type)))
	(cond ((and xl xh
		    (<= (- (bound-value xh) (bound-value xl)) #.(* 2 pi)))
	       ;; We have a bounded input range and the range covers
	       ;; no more than one period.  We can compute bounds now.
	       ;; We reduce the argument range to a single period.
	       (multiple-value-bind (nperiods x-lo)
		   (ftruncate (bound-value xl) two-pi)
		 (let* ((x-hi (- (bound-value xh) (* nperiods two-pi)))
			(bound-list (list (set-bound (funcall fun x-lo)
						     (consp xl))
					  (set-bound (funcall fun x-hi)
						     (consp xh)))))
		   ;; Add the upper and lower values for bounds on the
		   ;; function if the range covers the corresponding
		   ;; points.
		   (when (<= x-lo bound-one x-hi)
		     (push 1 bound-list))
		   (when (<= x-lo bound-minus-one x-hi)
		     (push -1 bound-list))
		   (make-numeric-type :class 'float
				      :format (elfun-float-format
					       (numeric-type-format type))
				      :complexp :real
				      :low (min-bound-list bound-list)
				      :high (max-bound-list bound-list)))))
	      (t
	       ;; The range covers more than one period,
	       ;; so the answer is obvious.
	       (make-numeric-type :class 'float
				  :format (elfun-float-format
					   (numeric-type-format type))
				  :complexp :real
				  :low nil
				  :high nil)))))))

(defoptimizer (sin derive-type) ((num))
  (trig-limits num #'sin #.(/ pi 2) #.(* 1.5d0 pi)))

(defoptimizer (cos derive-type) ((num))
  (trig-limits num #'cos 0 pi))
	     
) ; end progn



