;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/float-tran.lisp,v 1.58 1998/01/02 05:08:52 dtc Exp $")
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

#-(or new-random random-mt19937)
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

#+random-mt19937
(deftransform random ((num &optional state)
		      ((integer 1 #.(expt 2 32)) &optional *))
  "use inline (unsigned-byte 32) operations"
  (let ((num-high (numeric-type-high (continuation-type num))))
    (when (null num-high)
      (give-up))
    (cond ((constant-continuation-p num)
	   ;; Check the worst case sum abs error for the random number
	   ;; expectations.
	   (let ((rem (rem (expt 2 32) num-high)))
	     (unless (< (/ (* 2 rem (- num-high rem)) num-high (expt 2 32))
			(expt 2 (- kernel::random-integer-extra-bits)))
	       (give-up "The random number expectations are inaccurate."))
	     (if (= num-high (expt 2 32))
		 '(random-chunk (or state *random-state*))
		 #-x86 '(rem (random-chunk (or state *random-state*)) num)
		 #+x86
		 ;; Use multiplication which is faster.
		 '(values (bignum::%multiply 
			   (random-chunk (or state *random-state*))
			   num)))))
	  ((> num-high random-fixnum-max)
	   (give-up "The range is too large to assure an accurate result."))
	  #+x86
	  ((< num-high (expt 2 32))
	   '(values (bignum::%multiply (random-chunk (or state *random-state*))
		     num)))
	  (t
	   '(rem (random-chunk (or state *random-state*)) num)))))


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

(defun scale-float-derive-type-aux (f ex same-arg)
  (declare (ignore same-arg))
  (flet ((scale-bound (x n)
	   ;; We need to be a bit careful here and catch any overflows
	   ;; that might occur.  We can ignore underflows which become
	   ;; zeros.
	   (set-bound
	    (handler-case
	     (scale-float (bound-value x) n)
	     (floating-point-overflow ()
		nil))
	    (consp x))))
    (when (and (numeric-type-p f) (numeric-type-p ex))
      (let ((f-lo (numeric-type-low f))
	    (f-hi (numeric-type-high f))
	    (ex-lo (numeric-type-low ex))
	    (ex-hi (numeric-type-high ex))
	    (new-lo nil)
	    (new-hi nil))
	(when (and f-hi ex-hi)
	  (setf new-hi (scale-bound f-hi ex-hi)))
	(when (and f-lo ex-lo)
	  (setf new-lo (scale-bound f-lo ex-lo)))
	(make-numeric-type :class (numeric-type-class f)
			   :format (numeric-type-format f)
			   :complexp :real
			   :low new-lo
			   :high new-hi)))))
;;;
(defoptimizer (scale-single-float derive-type) ((f ex))
  (two-arg-derive-type f ex #'scale-float-derive-type-aux #'scale-single-float))
(defoptimizer (scale-double-float derive-type) ((f ex))
  (two-arg-derive-type f ex #'scale-float-derive-type-aux #'scale-double-float))
	     
;;; toy@rtp.ericsson.se:
;;;
;;; Defoptimizers for %single-float and %double-float.  This makes the
;;; FLOAT function return the correct ranges if the input has some
;;; defined range.  Quite useful if we want to convert some type of
;;; bounded integer into a float.

(macrolet
    ((frob (fun type)
       (let ((aux-name (symbolicate fun "-DERIVE-TYPE-AUX")))
	 `(progn
	   (defun ,aux-name (num)
	     ;; When converting a number to a float, the limits are
	     ;; the same.
	     (let* ((lo (bound-func #'(lambda (x)
					(coerce x ',type))
				    (numeric-type-low num)))
		    (hi (bound-func #'(lambda (x)
					(coerce x ',type))
				    (numeric-type-high num))))
	       (specifier-type `(,',type ,(or lo '*) ,(or hi '*)))))
	   
	   (defoptimizer (,fun derive-type) ((num))
	     (one-arg-derive-type num #',aux-name #',fun))))))
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
  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform expt ((x y) ((double-float 0d0) double-float) * :when :both)
  `(%pow x y))
(deftransform expt ((x y) ((single-float 0f0) (signed-byte 32)) *)
  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform expt ((x y) ((double-float 0d0) (signed-byte 32)) * :when :both)
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


;;; Compute a specifier like '(or float (complex float)), except float
;;; should be the right kind of float.  Allow bounds for the float
;;; part too.
(defun float-or-complex-type (num &optional lo hi)
  (declare (type numeric-type num))
  (let* ((f-type (or (numeric-type-format num) 'float))
	 (lo (and lo (coerce lo f-type)))
	 (hi (and hi (coerce hi f-type))))
    (specifier-type `(or (,f-type ,(or lo '*)
			          ,(or hi '*))
			 (complex ,f-type)))))

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

(defun elfun-derive-type-simple (arg fcn cond default-lo default-hi)
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (make-numeric-type :class (numeric-type-class arg)
			       :format (numeric-type-format arg)
			       :complexp :complex))
	   ((numeric-type-real-p arg)
	    (let ((lo (numeric-type-low arg))
		  (hi (numeric-type-high arg)))
	      (if (funcall cond lo hi)
		  (let ((f-type (or (numeric-type-format arg) 'single-float)))
		    (make-numeric-type
		     :class 'float
		     :format f-type
		     :low (or (bound-func fcn lo)
			      (and default-lo (coerce default-lo f-type)))
		     :high (or (bound-func fcn hi)
			       (and default-hi (coerce default-hi f-type)))))
		  (float-or-complex-type arg))))
	   (t
	    (float-or-complex-type arg default-lo default-hi))))))

(macrolet
    ((frob (name cond def-lo-bnd def-hi-bnd)
       (let ((num (gensym)))
	 `(defoptimizer (,name derive-type) ((,num))
	   (one-arg-derive-type
	    ,num
	    #'(lambda (arg)
		(elfun-derive-type-simple arg #',name
					  ,cond
					  ,def-lo-bnd ,def-hi-bnd))
	    #',name)))))
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


;;; Note must assume that a type including 0.0 may also include -0.0
;;; and thus the result may be complex.
;;;
(defun sqrt-derive-type-aux (arg)
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (make-numeric-type :class (numeric-type-class arg)
			       :format (numeric-type-format arg)
			       :complexp :complex))
	   ((numeric-type-real-p arg)
	    (let* ((lo (numeric-type-low arg))
		   (lo-val (and lo (bound-value lo)))
		   (hi (numeric-type-high arg)))
	      (if (typecase lo-val
		    (rational
		     (>= lo-val 0))
		    (float
		     (if (consp lo)
			 (>= lo-val 0)
			 (> lo-val 0))))
		  (let ((f-type (or (numeric-type-format arg) 'single-float)))
		    (make-numeric-type
		     :class 'float
		     :format f-type
		     :low (cond ((> lo-val 0) (bound-func #'sqrt lo))
				((consp lo) (list (coerce 0 f-type)))
				(t (coerce 0 f-type)))
		     :high (bound-func #'sqrt hi)))
		  (float-or-complex-type arg 0))))
	   (t
	    (float-or-complex-type arg 0))))))
;;;
(defoptimizer (sqrt derive-type) ((num))
  (one-arg-derive-type num #'sqrt-derive-type-aux #'sqrt))


;;; Acos is monotonic decreasing, so we need to swap the function
;;; values at the lower and upper bounds of the input domain.
;;;
(defun acos-derive-type-aux (arg)
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (make-numeric-type :class (numeric-type-class arg)
			       :format (numeric-type-format arg)
			       :complexp :complex))
	   ((numeric-type-real-p arg)
	    (let ((float-type (or (numeric-type-format arg) 'float))
		  (lo (numeric-type-low arg))
		  (hi (numeric-type-high arg)))
	      (cond
		((and lo hi
		      (>= (bound-value lo) -1)
		      (<= (bound-value hi) 1))
		 (setf lo (or (bound-func #'acos (numeric-type-high arg)) 0))
		 (setf hi (or (bound-func #'acos (numeric-type-low arg)) pi))
		 (specifier-type `(,float-type
				   ,(and lo (coerce lo float-type))
				   ,(and hi (coerce hi float-type)))))
		(t
		 (float-or-complex-type arg 0 pi)))))
	   (t
	    (float-or-complex-type arg 0 pi))))))
;;;
(defoptimizer (acos derive-type) ((num))
  (one-arg-derive-type num #'acos-derive-type-aux #'acos))


;;; Compute bounds for (expt x y).  This should be easy since (expt x
;;; y) = (exp (* y (log x))).  However, computations done this way
;;; have too much roundoff.  Thus we have to do it the hard way.
;;;  
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
     ;; obviously non-negative.  We just have to be careful for
     ;; infinite bounds (given by nil).
     (let ((lo (safe-expt (c::bound-value (c::interval-low x))
			  (c::bound-value (c::interval-low y))))
	   (hi (safe-expt (c::bound-value (c::interval-high x))
			  (c::bound-value (c::interval-high y)))))
       (list (c::make-interval :low (or lo 1) :high hi))))
    ('-
     ;; Y is negative and log x >= 0.  The range of exp(y * log(x)) is
     ;; obviously [0, 1].  However, underflow (nil) means 0 is the
     ;; result
     (let ((lo (safe-expt (c::bound-value (c::interval-high x))
			  (c::bound-value (c::interval-low y))))
	   (hi (safe-expt (c::bound-value (c::interval-low x))
			  (c::bound-value (c::interval-high y)))))
       (list (c::make-interval :low (or lo 0) :high (or hi 1)))))
    (t
     ;; Split the interval in half
     (destructuring-bind (y- y+)
	 (c::interval-split 0 y t)
       (list (interval-expt-> x y-)
	     (interval-expt-> x y+))))))

;;; Handle the case when x <= 1
(defun interval-expt-< (x y)
  (case (c::interval-range-info x 0)
    ('+
     ;; The case of 0 <= x <= 1 is easy
     (case (c::interval-range-info y)
       ('+
	;; Y is positive and log X <= 0.  The range of exp(y * log(x)) is
	;; obviously [0, 1].  We just have to be careful for infinite bounds
	;; (given by nil).
	(let ((lo (safe-expt (c::bound-value (c::interval-low x))
			     (c::bound-value (c::interval-high y))))
	      (hi (safe-expt (c::bound-value (c::interval-high x))
			     (c::bound-value (c::interval-low y)))))
	  (list (c::make-interval :low (or lo 0) :high (or hi 1)))))
       ('-
	;; Y is negative and log x <= 0.  The range of exp(y * log(x)) is
	;; obviously [1, inf].
	(let ((hi (safe-expt (c::bound-value (c::interval-low x))
			     (c::bound-value (c::interval-low y))))
	      (lo (safe-expt (c::bound-value (c::interval-high x))
			     (c::bound-value (c::interval-high y)))))
	  (list (c::make-interval :low (or lo 1) :high hi))))
       (t
	;; Split the interval in half
	(destructuring-bind (y- y+)
	    (c::interval-split 0 y t)
	  (list (interval-expt-< x y-)
		(interval-expt-< x y+))))))
    ('-
     ;; The case where x <= 0.  Y MUST be an INTEGER for this to
     ;; work!  The calling function must insure this! For now we'll
     ;; just return the appropriate unbounded float type.
     (list (c::make-interval :low nil :high nil)))
    (t
     (destructuring-bind (neg pos)
	 (interval-split 0 x t t)
       (list (interval-expt-< neg y)
	     (interval-expt-< pos y))))))

;;; Compute bounds for (expt x y)

(defun interval-expt (x y)
  (case (interval-range-info x 1)
    ('+
     ;; X >= 1
	 (interval-expt-> x y))
    ('-
     ;; X <= 1
     (interval-expt-< x y))
    (t
     (destructuring-bind (left right)
	 (interval-split 1 x t t)
       (list (interval-expt left y)
	     (interval-expt right y))))))

(defun fixup-interval-expt (bnd x-int y-int x-type y-type)
  (declare (ignore x-int))
  ;; Figure out what the return type should be, given the argument
  ;; types and bounds and the result type and bounds.
  (cond ((csubtypep x-type (specifier-type 'integer))
	 ;; An integer to some power.  Cases to consider:
	 (case (numeric-type-class y-type)
	   (integer
	    ;; Positive integer to an integer power is either an
	    ;; integer or a rational.
	    (let ((lo (or (interval-low bnd) '*))
		  (hi (or (interval-high bnd) '*)))
	      (if (and (interval-low y-int)
		       (>= (bound-value (interval-low y-int)) 0))
		  (specifier-type `(integer ,lo ,hi))
		  (specifier-type `(rational ,lo ,hi)))))
	   (rational
	    ;; Positive integer to rational power is either a rational
	    ;; or a single-float.
	    (let* ((lo (interval-low bnd))
		   (hi (interval-high bnd))
		   (int-lo (if lo
			       (floor (bound-value lo))
			       '*))
		   (int-hi (if hi
			       (ceiling (bound-value hi))
			       '*))
		   (f-lo (if lo
			     (bound-func #'float lo)
			     '*))
		   (f-hi (if hi
			     (bound-func #'float hi)
			     '*)))
	      (specifier-type `(or (rational ,int-lo ,int-hi)
				(single-float ,f-lo, f-hi)))))
	   (float
	    ;; Positive integer to a float power is a float
	    (let ((res (copy-numeric-type y-type)))
	      (setf (numeric-type-low res) (interval-low bnd))
	      (setf (numeric-type-high res) (interval-high bnd))
	      res))
	   (t
	    ;; Positive integer to a number is a number (for now)
	    (specifier-type 'number)))
	 )
	((csubtypep x-type (specifier-type 'rational))
	 ;; A rational to some power
	 (case (numeric-type-class y-type)
	   (integer
	    ;; Positive rational to an integer power is always a rational
	    (specifier-type `(rational ,(or (interval-low bnd) '*)
			      ,(or (interval-high bnd) '*))))
	   (rational
	    ;; Positive rational to rational power is either a rational
	    ;; or a single-float.
	    (let* ((lo (interval-low bnd))
		   (hi (interval-high bnd))
		   (int-lo (if lo
			       (floor (bound-value lo))
			       '*))
		   (int-hi (if hi
			       (ceiling (bound-value hi))
			       '*))
		   (f-lo (if lo
			     (bound-func #'float lo)
			     '*))
		   (f-hi (if hi
			     (bound-func #'float hi)
			     '*)))
	      (specifier-type `(or (rational ,int-lo ,int-hi)
				(single-float ,f-lo, f-hi)))))
	   (float
	    ;; Positive rational to a float power is a float
	    (let ((res (copy-numeric-type y-type)))
	      (setf (numeric-type-low res) (interval-low bnd))
	      (setf (numeric-type-high res) (interval-high bnd))
	      res))
	   (t
	    ;; Positive rational to a number is a number (for now)
	    (specifier-type 'number)))
	 )
	((csubtypep x-type (specifier-type 'float))
	 ;; A float to some power
	 (case (numeric-type-class y-type)
	   ((or integer rational)
	    ;; Positive float to an integer or rational power is always a float
	    (make-numeric-type
	     :class 'float
	     :format (numeric-type-format x-type)
	     :low (interval-low bnd)
	     :high (interval-high bnd)))
	   (float
	    ;; Positive float to a float power is a float of the higher type
	    (make-numeric-type
	     :class 'float
	     :format (float-format-max (numeric-type-format x-type)
				       (numeric-type-format y-type))
	     :low (interval-low bnd)
	     :high (interval-high bnd)))
	   (t
	    ;; Positive float to a number is a number (for now)
	    (specifier-type 'number))))
	(t
	 ;; A number to some power is a number.
	 (specifier-type 'number))))
  
(defun merged-interval-expt (x y)
  (let* ((x-int (numeric-type->interval x))
	 (y-int (numeric-type->interval y)))
    (mapcar #'(lambda (type)
		(fixup-interval-expt type x-int y-int x y))
	    (flatten-list (interval-expt x-int y-int)))))

(defun expt-derive-type-aux (x y same-arg)
  (declare (ignore same-arg))
  (cond ((or (not (numeric-type-real-p x))
	     (not (numeric-type-real-p y)))
	 ;; Use numeric contagion if either is not real
	 (numeric-contagion x y))
	((csubtypep y (specifier-type 'integer))
	 ;; A real raised to an integer power is well-defined
	 (merged-interval-expt x y))
	(t
	 ;; A real raised to a non-integral power can be a float or a
	 ;; complex number.
	 (cond ((or (csubtypep x (specifier-type '(rational 0)))
		    (csubtypep x (specifier-type '(float (0d0)))))
		;; But a positive real to any power is well-defined.
		(merged-interval-expt x y))
	       (t
		;; A real to some power.  The result could be a real
		;; or a complex.
		(float-or-complex-type (numeric-contagion x y)))))))

(defoptimizer (expt derive-type) ((x y))
  (two-arg-derive-type x y #'expt-derive-type-aux #'expt))


;;; Note must assume that a type including 0.0 may also include -0.0
;;; and thus the result may be complex -infinity + i*pi.
;;;
(defun log-derive-type-aux-1 (arg)
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (make-numeric-type :class (numeric-type-class arg)
			       :format (numeric-type-format arg)
			       :complexp :complex))
	   ((numeric-type-real-p arg)
	    (let* ((lo (numeric-type-low arg))
		   (lo-val (and lo (bound-value lo)))
		   (hi (numeric-type-high arg)))
	      (if (typecase lo-val
		    (rational
		     (>= lo-val 0))
		    (float
		     (if (consp lo)
			 (>= lo-val 0)
			 (> lo-val 0))))
		  (let ((f-type (or (numeric-type-format arg) 'single-float)))
		    (make-numeric-type
		     :class 'float
		     :format f-type
		     :low (and (> lo-val 0) (bound-func #'log lo))
		     :high (bound-func #'log hi)))
		  (float-or-complex-type arg))))
	   (t
	    (float-or-complex-type arg))))))

(defun log-derive-type-aux-2 (x y same-arg)
  (let ((log-x (log-derive-type-aux-1 x))
	(log-y (log-derive-type-aux-1 y))
	(result '()))
    ;; log-x or log-y might be union types.  We need to run through
    ;; the union types ourselves because /-derive-type-aux doesn't.
    (dolist (x-type (prepare-arg-for-derive-type log-x))
      (dolist (y-type (prepare-arg-for-derive-type log-y))
	(push (/-derive-type-aux x-type y-type same-arg) result)))
    (setf result (flatten-list result))
    (if (rest result)
	(make-union-type result)
	(first result))))

(defoptimizer (log derive-type) ((x &optional y))
  (if y
      (two-arg-derive-type x y #'log-derive-type-aux-2 #'log)
      (one-arg-derive-type x #'log-derive-type-aux-1 #'log)))


(defun atan-derive-type-aux-1 (y)
  (elfun-derive-type-simple
   y #'atan (constantly t) #.(- (/ pi 2)) #.(/ pi 2)))

(defun atan-derive-type-aux-2 (y x same-arg)
  (declare (ignore same-arg))
  ;; The hard case with two args.  We just return the max bounds.
  (cond ((and (numeric-type-real-p x)
	      (numeric-type-real-p y))
	 (make-numeric-type
	  :class 'float
	  :format (float-format-max
		   (numeric-type-format y)
		   (numeric-type-format x))
	  :complexp :real
	  :low #.(- pi)
	  :high #.pi))
	(t
	 ;; The result is a float or a complex number
	 (float-or-complex-type (numeric-contagion x y)))))

(defoptimizer (atan derive-type) ((y &optional x))
  (cond ((null x)
	 (one-arg-derive-type y #'atan-derive-type-aux-1 #'atan))
	(t
	 (two-arg-derive-type y x #'atan-derive-type-aux-2 #'atan))))


(defun cosh-derive-type-aux (x)
  (elfun-derive-type-simple
   (if (numeric-type-real-p x)
       (abs-derive-type-aux x)
       x)
   #'cosh (constantly t) 0 nil))

(defoptimizer (cosh derive-type) ((num))
  (one-arg-derive-type num #'cosh-derive-type-aux #'cosh))


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
  (one-arg-derive-type num #'phase-derive-type-aux #'phase))

) ;end progn for propagate-fun-type

#+complex-float
(progn

(deftransform realpart ((x) ((complex rational)) *)
  '(kernel:%realpart x))
(deftransform imagpart ((x) ((complex rational)) *)
  '(kernel:%imagpart x))
) ;end progn complex-float

;;; Make REALPART and IMAGPART return the appropriate types.  This
;;; should help a lot in optimized code.

(defun realpart-derive-type-aux (type)
    (cond ((numeric-type-real-p type)
	   ;; The realpart of a real has the same type and range as
	   ;; the input.
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
			      :high (numeric-type-high type)))))

(defoptimizer (realpart derive-type) ((num))
  (one-arg-derive-type num #'realpart-derive-type-aux #'realpart))

(defun imagpart-derive-type-aux (type)
  (cond ((numeric-type-real-p type)
	 ;; The imagpart of a real has the same type as the input,
	 ;; except that it's zero
	 (make-numeric-type :class (numeric-type-class type)
			    :format (numeric-type-format type)
			    :complexp :real
			    :low 0
			    :high 0))
	(t
	 ;; We have a complex number.  The result has the same type as
	 ;; the imaginary part, except that it's real, not complex,
	 ;; obviously.
	 (make-numeric-type :class (numeric-type-class type)
			    :format (numeric-type-format type)
			    :complexp :real
			    :low (numeric-type-low type)
			    :high (numeric-type-high type)))))

(defoptimizer (imagpart derive-type) ((num))
  (one-arg-derive-type num #'imagpart-derive-type-aux #'imagpart))

(defun complex-derive-type-aux-1 (re-type)
  (if (numeric-type-p re-type)
      (make-numeric-type :class (numeric-type-class re-type)
			 :format (numeric-type-format re-type)
			 :complexp (if (csubtypep re-type
						  (specifier-type 'rational))
				       :real
				       :complex)
			 :low (numeric-type-low re-type)
			 :high (numeric-type-high re-type))
      (specifier-type 'complex)))

(defun complex-derive-type-aux-2 (re-type im-type same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-p re-type)
	   (numeric-type-p im-type))
      ;; Need to check to make sure numeric-contagion returns the
      ;; right type for what we want here.
      
      ;; Also, what about rational canonicalization, like (complex 5 0)
      ;; is 5?  So, if the result must be complex, we make it so.
      ;; If the result might be complex, which happens only if the
      ;; arguments are rational, we make it a union type of (or
      ;; rational (complex rational)).
      (let* ((element-type (numeric-contagion re-type im-type))
	     (rat-result-p (csubtypep element-type
				      (specifier-type 'rational))))
	(if rat-result-p
	    (make-union-type
	     (list element-type
		   (specifier-type 
		    `(complex ,(numeric-type-class element-type)))))
	    (make-numeric-type :class (numeric-type-class element-type)
			       :format (numeric-type-format element-type)
			       :complexp (if rat-result-p
					     :real
					     :complex))))
      (specifier-type 'complex)))

(defoptimizer (complex derive-type) ((re &optional im))
  (if im
      (two-arg-derive-type re im #'complex-derive-type-aux-2 #'complex)
      (one-arg-derive-type re #'complex-derive-type-aux-1 #'complex)))


;;; Define some transforms for complex operations.  We do this in lieu
;;; of complex operation VOPs.
;;;
(macrolet ((frob (type)
	     `(progn
	       ;; Complex addition and subtraction
	       (deftransform + ((w z) ((complex ,type) (complex ,type)) *)
		 '(complex (+ (realpart w) (realpart z))
			   (+ (imagpart w) (imagpart z))))
	       (deftransform - ((w z) ((complex ,type) (complex ,type)) *)
		 '(complex (- (realpart w) (realpart z))
			   (- (imagpart w) (imagpart z))))
	       ;; Add and subtract a complex and a float
	       (deftransform + ((w z) ((complex ,type) ,type) *)
		 '(complex (+ (realpart w) z) (imagpart w)))
	       (deftransform + ((z w) (,type (complex ,type)) *)
		 '(complex (+ (realpart w) z) (imagpart w)))
	       ;; Add and subtract a float and a complex number
	       (deftransform - ((w z) ((complex ,type) ,type) *)
		 '(complex (- (realpart w) z) (imagpart w)))
	       (deftransform - ((z w) (,type (complex ,type)) *)
		 '(complex (- z (realpart w)) (- (imagpart w))))
	       ;; Multiply and divide two complex numbers
	       (deftransform * ((x y) ((complex ,type) (complex ,type)) *)
		 '(let* ((rx (realpart x))
			 (ix (imagpart x))
			 (ry (realpart y))
			 (iy (imagpart y)))
		    (complex (- (* rx ry) (* ix iy))
			     (+ (* rx iy) (* ix ry)))))
	       (deftransform / ((x y) ((complex ,type) (complex ,type)) *)
		 '(let* ((rx (realpart x))
			 (ix (imagpart x))
			 (ry (realpart y))
			 (iy (imagpart y)))
		    (if (> (abs ry) (abs iy))
			(let* ((r (/ iy ry))
			       (dn (* ry (+ 1 (* r r)))))
			  (complex (/ (+ rx (* ix r)) dn)
				   (/ (- ix (* rx r)) dn)))
			(let* ((r (/ ry iy))
			       (dn (* iy (+ 1 (* r r)))))
			  (complex (/ (+ (* rx r) ix) dn)
				   (/ (- (* ix r) rx) dn))))))
	       ;; Multiply a complex by a float or vice versa
	       (deftransform * ((w z) ((complex ,type) ,type) *)
		 '(complex (* (realpart w) z) (* (imagpart w) z)))
	       (deftransform * ((z w) (,type (complex ,type)) *)
		 '(complex (* (realpart w) z) (* (imagpart w) z)))
	       ;; Divide a complex by a float
	       (deftransform / ((w z) ((complex ,type) ,type) *)
		 '(complex (/ (realpart w) z) (/ (imagpart w) z)))
	       ;; Conjugate of a float or complex number
	       (deftransform conjugate ((z) ((complex ,type)) *)
		 '(complex (realpart z) (- (imagpart z))))
	       ;; Cis.
	       (deftransform cis ((z) ((,type)) *)
		 '(complex (cos z) (sin z))))))

  (frob single-float)
  (frob double-float))


;;; Here are simple optimizers for sin, cos, and tan.  They do not
;;; produce a minimal range for the result; the result is the widest
;;; possible answer.  This gets around the problem of doing range
;;; reduction correctly but still provides useful results when the
;;; inputs are union types.

#+propagate-fun-type
(progn
(defun sincos-derive-type-aux (arg)
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (make-numeric-type :class (numeric-type-class arg)
			       :format (numeric-type-format arg)
			       :complexp :complex))
	   ((numeric-type-real-p arg)
	    (let ((float-type (or (numeric-type-format arg) 'float)))
	      (specifier-type `(,float-type
				,(coerce -1 float-type)
				,(coerce 1 float-type)))))
	   (t
	    (float-or-complex-type arg -1 1))))))

(defoptimizer (sin derive-type) ((num))
  (one-arg-derive-type num #'sincos-derive-type-aux #'sin))
       
(defoptimizer (cos derive-type) ((num))
  (one-arg-derive-type num #'sincos-derive-type-aux #'cos))


(defun tan-derive-type-aux (arg)
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (make-numeric-type :class (numeric-type-class arg)
			       :format (numeric-type-format arg)
			       :complexp :complex))
	   ((numeric-type-real-p arg)
	    (let ((float-type (or (numeric-type-format arg) 'float)))
	      (specifier-type float-type)))
	   (t
	    (float-or-complex-type arg))))))

(defoptimizer (tan derive-type) ((num))
  (one-arg-derive-type num #'tan-derive-type-aux #'tan))

;;; conjugate always returns the same type as the input type  
(defoptimizer (conjugate derive-type) ((num))
  (continuation-type num))

(defoptimizer (cis derive-type) ((num))
  (one-arg-derive-type num
     #'(lambda (arg)
	 (c::specifier-type
	  `(complex ,(or (numeric-type-format arg) 'single-float))))
     #'cis))

) ; end progn
