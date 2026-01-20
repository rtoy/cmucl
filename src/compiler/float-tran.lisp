;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/float-tran.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating-point specific transforms, and may be somewhat
;;; implementation dependent in its assumptions of what the formats are.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "C")
(intl:textdomain "cmucl")


;;;; Coercions:

(defknown %single-float (real) single-float (movable foldable flushable))
(defknown %double-float (real) double-float (movable foldable flushable))

(deftransform float ((n prototype) (* single-float) * :when :both)
  '(%single-float n))

(deftransform float ((n prototype) (* double-float) * :when :both)
  '(%double-float n))

(deftransform float ((n) *)
  `(if (floatp n) n (%single-float n)))

(deftransform %single-float ((n) (single-float) * :when :both)
  'n)

(deftransform %double-float ((n) (double-float) * :when :both)
  'n)

(defknown %complex-single-float (number) (complex single-float)
  (movable foldable flushable))
(defknown %complex-double-float (number) (complex double-float)
  (movable foldable flushable))
(defknown %complex-double-double-float (number) (complex double-double-float)
  (movable foldable flushable))

(macrolet
    ((frob (type)
       (let ((name (symbolicate "%COMPLEX-" type "-FLOAT"))
	     (convert (symbolicate "%" type "-FLOAT")))
	 `(progn
	    (defun ,name (n)
	      (declare (number n))
	      (etypecase n
		(real
		 (complex (,convert n)))
		(complex
		 (complex (,convert (realpart n))
			  (,convert (imagpart n))))))
	    (deftransform ,name ((n) ((complex ,(symbolicate type "-FLOAT"))) * :when :both)
	      'n)))))
  (frob single)
  (frob double)
  #+double-double
  (frob double-double))


(deftransform coerce ((n type) (* *) * :when :both)
  (unless (constant-continuation-p type)
    (give-up))
  `(the ,(continuation-value type)
	,(let ( (tspec (specifier-type (continuation-value type))) )
	   (cond #+double-double
		 ((csubtypep tspec (specifier-type 'double-double-float))
		  '(%double-double-float n))
		 ((csubtypep tspec (specifier-type 'double-float))	
		  '(%double-float n))	
		 ((csubtypep tspec (specifier-type 'single-float))
		  '(%single-float n))
		 #+double-double
		 ((csubtypep tspec (specifier-type '(complex double-double-float)))
		  '(%complex-double-double-float n))
		 ((csubtypep tspec (specifier-type '(complex double-float)))
		  '(%complex-double-float n))
		 ((csubtypep tspec (specifier-type '(complex single-float)))
		  '(%complex-single-float n))
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
  (frob round %unary-round))

(defknown %unary-truncate (real) integer
	  (movable foldable flushable))

;; Convert (truncate x y) to the obvious implementation.  We only want
;; this when under certain conditions and let the generic truncate
;; handle the rest.  (Note: if y = 1, the divide and multiply by y
;; should be removed by other deftransforms.)

(deftransform truncate ((x &optional y)
			(float &optional (or float integer)))
  '(let ((res (%unary-truncate (/ x y))))
     (values res (- x (* y res)))))

(deftransform floor ((number &optional divisor)
		     (float &optional (or integer float)))
  '(multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(values (1- tru) (+ rem divisor))
	(values tru rem))))

(deftransform ceiling ((number &optional divisor)
		       (float &optional (or integer float)))
  '(multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (minusp number)
		 (plusp number)))
	(values (1+ tru) (- rem divisor))
	(values tru rem))))

(defknown %unary-ftruncate/single-float (single-float) single-float
	  (movable foldable flushable))
(defknown %unary-ftruncate/double-float (double-float) double-float
	  (movable foldable flushable))

(defknown %unary-ftruncate (real) float
	  (movable foldable flushable))

;; Convert (ftruncate x y) to the obvious implementation.  We only
;; want this under certain conditions and let the generic ftruncate
;; handle the rest.  (Note: if y = 1, the divide and multiply by y
;; should be removed by other deftransforms.)

(deftransform ftruncate ((x &optional (y 1))
			 (float &optional (or float integer)))
  '(let ((res (%unary-ftruncate (/ x y))))
     (values res (- x (* y res)))))

#+(or sparc (and x86 sse2))
(defknown fast-unary-ftruncate ((or single-float double-float))
  (or single-float double-float)
  (movable foldable flushable))

#+(or sparc (and x86 sse2))
(defoptimizer (fast-unary-ftruncate derive-type) ((f))
  (one-arg-derive-type f
		       #'(lambda (n)
			   (ftruncate-derive-type-quot-aux n
							   (specifier-type '(integer 1 1))
							   nil))
		       #'ftruncate))

;; Convert %unary-ftruncate to unary-ftruncate/{single,double}-float
;; if x is known to be of the right type.  Also, if the result is
;; known to fit in the same range as a (signed-byte 32), convert this
;; to %unary-truncate, which might be a single instruction, and float
;; the result.  However, for sparc and x86, we have a vop to do this
;; so call that, and for Sparc V9, we can actually handle a 64-bit
;; integer range.
(macrolet ((frob (ftype func)
	     `(deftransform %unary-ftruncate ((x) (,ftype))
	       (let* ((x-type (continuation-type x))
		      (lo (bound-value (numeric-type-low x-type)))
		      (hi (bound-value (numeric-type-high x-type)))
		      (limit-lo (- (ash 1 #-sparc-v9 31 #+sparc-v9 63)))
		      (limit-hi (ash 1 #-sparc-v9 31 #+sparc-v9 63)))
		 (if (and (numberp lo) (numberp hi)
			  (< limit-lo lo)
			  (< hi limit-hi))
		     #-(or sparc (and x86 sse2))
		     '(let ((result (coerce (%unary-truncate x) ',ftype)))
		       ;; Multiply by x when result is 0 so that we
		       ;; get the correct signed zero to match what
		       ;; ftruncate in float.lisp would return.
		       (if (zerop result)
			   (* result x)
			   result))
		     #+(or sparc (and x86 sse2))
		     '(let ((result (fast-unary-ftruncate x)))
		       (if (zerop result)
			   (* result x)
			   result))
		     '(,func x))))))
  (frob single-float %unary-ftruncate/single-float)
  (frob double-float %unary-ftruncate/double-float))

;;; FROUND
#-x87
(progn
(deftransform fround ((x &optional (y 1))
		      ((or single-float double-float)
		       &optional (or single-float double-float integer)))
  '(let ((res (%unary-fround (/ x y))))
    (values res (- x (* y res)))))

(defknown %unary-fround (real) float
  (movable foldable flushable))

(defknown %unary-fround/single-float (single-float) single-float
  (movable foldable flushable))

(defknown %unary-fround/double-float (double-float) double-float
  (movable foldable flushable))

(deftransform %unary-fround ((x) (single-float))
  '(%unary-fround/single-float x))

(deftransform %unary-fround ((x) (double-float))
  '(%unary-fround/double-float x))

); not x87

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

#-(or new-random random-mt19937 random-xoroshiro)
(deftransform random ((num &optional state)
		      ((integer 1 #.random-fixnum-max) &optional *))
  _N"use inline fixnum operations"
  '(rem (random-chunk (or state *random-state*)) num))

;;; With the latest propagate-float-type code the compiler can inline
;;; truncate (signed-byte 32) allowing 31 bits, and (unsigned-byte 32)
;;; 32 bits on the x86. When not using the propagate-float-type
;;; feature the best size that can be inlined is 29 bits.  The choice
;;; shouldn't cause bootstrap problems just slow code.
#+new-random
(deftransform random ((num &optional state)
		      ((integer 1
				#+x86 #xffffffff
				#-x86 #x7fffffff
				)
		       &optional *))
  #+x86 (intl:gettext "use inline (unsigned-byte 32) operations")
  #-x86 (intl:gettext "use inline (signed-byte 32) operations")
  '(values (truncate (%random-double-float (coerce num 'double-float)
		      (or state *random-state*)))))

#+(or random-mt19937 random-xoroshiro)
(deftransform random ((num &optional state)
		      ((integer 1 #.(expt 2 32)) &optional *))
  _N"use inline (unsigned-byte 32) operations"
  (let* ((num-type (continuation-type num))
	 (num-high (cond ((numeric-type-p num-type)
			  (numeric-type-high num-type))
			 ((union-type-p num-type)
			  ;; Find the maximum of the union type.  We
			  ;; know this works because if we're in this
			  ;; routine, NUM must be a subtype of
			  ;; (INTEGER 1 2^32), so each member of the
			  ;; union must be a subtype too.
			  (reduce #'max (union-type-types num-type)
				  :key #'numeric-type-high))
			 (t
			  (give-up)))))
    ;; Rather than doing (rem (random-chunk) num-high), we do,
    ;; essentially, (rem (* num-high (random-chunk)) #x100000000).  I
    ;; (rtoy) believe this approach doesn't have the bias issue with
    ;; doing rem.  This method works by treating (random-chunk) as if
    ;; it were a 32-bit fraction between 0 and 1, exclusive.  Multiply
    ;; this by num-high to get a random number between 0 and num-high,
    ;; This should have no bias.
    (cond ((constant-continuation-p num)
	   (if (= num-high (expt 2 32))
	       '(random-chunk (or state *random-state*))
	       '(values (bignum::%multiply 
			 (random-chunk (or state *random-state*))
			 num))))
	  ((< num-high (expt 2 32))
	   '(values (bignum::%multiply (random-chunk (or state *random-state*))
		     num)))
	  ((= num-high (expt 2 32))
	   '(if (= num (expt 2 32))
		(random-chunk (or state *random-state*))
		(values (bignum::%multiply (random-chunk (or state *random-state*))
					   num))))
	  (t
	   (error (intl:gettext "Shouldn't happen"))))))


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

#+(or sparc ppc (and x86 sse2))
(defknown double-float-bits (double-float)
  (values (signed-byte 32) (unsigned-byte 32))
  (movable foldable flushable))

(deftransform float-sign ((float &optional float2)
			  (single-float &optional single-float) *)
  (if float2
      (let ((temp (gensym)))
	`(let ((,temp (abs float2)))
	  (if (minusp (single-float-bits float)) (- ,temp) ,temp)))
      '(if (minusp (single-float-bits float)) -1f0 1f0)))

(deftransform float-sign ((float &optional float2)
			  (double-float &optional double-float) *)
  (if float2
      (let ((temp (gensym)))
	`(let ((,temp (abs float2)))
	  (if (minusp (double-float-high-bits float)) (- ,temp) ,temp)))
      '(if (minusp (double-float-high-bits float)) -1d0 1d0)))


;;;; DECODE-FLOAT, INTEGER-DECODE-FLOAT, SCALE-FLOAT:
;;;
;;;    Convert these operations to format specific versions when the format is
;;; known.
;;;

(deftype single-float-exponent ()
  `(integer (,(- vm:single-float-normal-exponent-min vm:single-float-bias
		 vm:single-float-digits))
	    ,(- vm:single-float-normal-exponent-max vm:single-float-bias)))

(deftype double-float-exponent ()
  `(integer (,(- vm:double-float-normal-exponent-min vm:double-float-bias
		 vm:double-float-digits))
	    ,(- vm:double-float-normal-exponent-max vm:double-float-bias)))


(deftype single-float-int-exponent ()
  `(integer (,(- vm:single-float-normal-exponent-min vm:single-float-bias
		 (* vm:single-float-digits 2)))
	    ,(- vm:single-float-normal-exponent-max vm:single-float-bias
		vm:single-float-digits)))

(deftype double-float-int-exponent ()
  `(integer (,(- vm:double-float-normal-exponent-min vm:double-float-bias
		 (* vm:double-float-digits 2)))
	    ,(- vm:double-float-normal-exponent-max vm:double-float-bias
		vm:double-float-digits)))

(deftype single-float-significand ()
  `(integer 0 (,(ash 1 vm:single-float-digits))))

(deftype double-float-significand ()
  `(integer 0 (,(ash 1 vm:double-float-digits))))

(defknown decode-single-float (single-float)
  (values (single-float 0.5f0 (1f0))
	  single-float-exponent
	  (member -1f0 1f0))
  (movable foldable flushable))

(defknown decode-double-float (double-float)
  (values (double-float 0.5d0 (1d0))
	  double-float-exponent
	  (member -1d0 1d0))
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
  (cond ((and (backend-featurep :x86)
	      (not (backend-featurep :sse2))
	      (csubtypep (continuation-type ex)
			 (specifier-type '(signed-byte 32)))
	      (not (byte-compiling)))
	 '(coerce (%scalbn (coerce f 'double-float) ex) 'single-float))
	((csubtypep (continuation-type ex)
		    (specifier-type `(integer #.(- vm:single-float-normal-exponent-min
						   vm:single-float-bias
						   vm:single-float-digits)
					      #.(- vm:single-float-normal-exponent-max
						   vm:single-float-bias
						   1))))
	 ;; The exponent is such that 2^ex will fit in a single-float.
	 ;; Thus, scale-float can be done multiplying by a suitable
	 ;; constant.
	 `(* f (kernel:make-single-float (dpb (+ ex (1+ vm:single-float-bias))
					      vm:single-float-exponent-byte
					      (kernel:single-float-bits 1f0)))))
	(t
	 '(scale-single-float f ex))))

(deftransform scale-float ((f ex) (double-float *) * :when :both)
  (cond ((and (backend-featurep :x86)
	      (not (backend-featurep :sse2))
	      (csubtypep (continuation-type ex)
			 (specifier-type '(signed-byte 32))))
	 '(%scalbn f ex))
	((csubtypep (continuation-type ex)
		    (specifier-type `(integer #.(- vm:double-float-normal-exponent-min
						   vm:double-float-bias
						   vm:double-float-digits)
					      #.(- vm:double-float-normal-exponent-max
						   vm:double-float-bias
						   1))))
	 ;; The exponent is such that 2^ex will fit in a double-float.
	 ;; Thus, scale-float can be done multiplying by a suitable
	 ;; constant.
	 `(* f (kernel:make-double-float (dpb (+ ex (1+ vm:double-float-bias))
					      vm:double-float-exponent-byte
					      (kernel::double-float-bits 1d0))
					 0)))
	(t
	 '(scale-double-float f ex))))

;;; toy@rtp.ericsson.se:
;;;
;;; Optimizers for scale-float.  If the float has bounds, new bounds
;;; are computed for the result, if possible.

(defun scale-float-derive-type-aux (f ex same-arg)
  (declare (ignore same-arg))
  (flet ((scale-bound (x n)
	   ;; We need to be a bit careful here and catch any overflows
	   ;; that might occur.  We can ignore underflows which become
	   ;; zeros.
	   (set-bound
	    (let ((value (handler-case
			     (scale-float (bound-value x) n)
			   (floating-point-overflow ()
			     nil))))
	      ;; This check is necessary for ppc because the current
	      ;; implementation on ppc doesn't signal floating-point
	      ;; overflow.  (How many other places do we need to check
	      ;; for this?)
	      (if (and (floatp value) (float-infinity-p value))
		  nil
		  value))
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
	;; We're computing bounds for scale-float.  Assume the bounds
	;; on f are fl and fh, and the bounds on ex are nl and nh.
	;; The resulting bound should be fl*2^nl and fh*2^nh.
	;; However, if fh is negative, and we get an underflow, we
	;; might get bounds like 0 and fh*2^nh < 0.  Our bounds are
	;; backwards.  Thus, swap the bounds to get the correct
	;; bounds.
	(when (and new-lo new-hi (< (bound-value new-hi)
				    (bound-value new-lo)))
	  (rotatef new-lo new-hi))
	(make-numeric-type :class (numeric-type-class f)
			   :format (numeric-type-format f)
			   :complexp :real
			   :low new-lo
			   :high new-hi)))))
;;;
(defoptimizer (scale-float derive-type) ((f ex))
  (two-arg-derive-type f ex #'scale-float-derive-type-aux
		       #'scale-float))
	     
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
	     ;; the "same."  
	     (let* ((lo (bound-func #'(lambda (x)
					;; If we can't coerce it, we
					;; return a NIL for the bound.
					;; (Is IGNORE-ERRORS too
					;; heavy-handed?  Should we
					;; try to do something more
					;; fine-grained?)
					(ignore-errors (coerce x ',type)))
				    (numeric-type-low num)))
		    (hi (bound-func #'(lambda (x)
					(ignore-errors (coerce x ',type)))
				    (numeric-type-high num))))
	       (specifier-type `(,',type ,(or lo '*) ,(or hi '*)))))
	   
	   (defoptimizer (,fun derive-type) ((num))
	     (one-arg-derive-type num #',aux-name #',fun))))))
  (frob %single-float single-float)
  (frob %double-float double-float))


;;;; Float contagion:

;;; FLOAT-CONTAGION-ARG1, ARG2  --  Internal
;;;
;;;    Do some stuff to recognize when the loser is doing mixed float and
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
		  (give-up (intl:gettext "Can't open-code float to rational comparison.")))
		(let ((val (continuation-value y)))
		  (unless (eql (rational (float val)) val)
		    (give-up (intl:gettext "~S doesn't have a precise float representation.")
			     val)))
		`(,',op x (float y x)))))
  (frob <)
  (frob >)
  (frob =))

;; Convert (/ x n) to (* x (/ n)) when x is a float and n is a power
;; of two, because (/ n) can be reprsented exactly.
(deftransform / ((x y) (float float) * :when :both)
  (unless (constant-continuation-p y)
    (give-up))
  (let ((val (continuation-value y)))
    (unless (= (decode-float val) 0.5)
      (give-up))
    `(* x (float (/ ,val) x))))

;; Convert 2*x to x+x.
(deftransform * ((x y) (float real) * :when :both)
  (unless (constant-continuation-p y)
    (give-up))
  (let ((val (continuation-value y)))
    (unless (= val 2)
      (give-up))
    '(+ x x)))

	      

;;;; Irrational transforms:

(defknown (%tan %sinh %asinh %atanh %log %logb %log10 #+x87 %tan-quick)
	  (double-float) double-float
  (movable foldable flushable))

(defknown (%sin %cos %tanh #+x87 %sin-quick #+x87 %cos-quick)
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

(defknown %expm1
    (double-float) (double-float -1d0)
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

(defknown (%log1p)
    (double-float) double-float
    (movable foldable flushable))

(dolist (stuff '((exp %exp *)
		 (log %log float)
		 (sqrt %sqrt float)
		 (sin %sin float)
		 (cos %cos float)
		 (tan %tan float)
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

(defknown (%sincos)
    (double-float) (values double-float double-float)
    (movable foldable flushable))

(deftransform cis ((x) (single-float) * :when :both)
  `(multiple-value-bind (s c)
       (%sincos (coerce x 'double-float))
     (complex (coerce c 'single-float)
	      (coerce s 'single-float))))

(deftransform cis ((x) (double-float) * :when :both)
  `(multiple-value-bind (s c)
       (%sincos x)
     (complex c s)))

;;; The argument range is limited on the x86 FP trig. functions. A
;;; post-test can detect a failure (and load a suitable result), but
;;; this test is avoided if possible.
;;;
;;; Simple tests show that sin/cos produce numbers greater than 1 when
;;; the arg >= 2^63.  tan produces floating-point invalid exceptions
;;; for arg >= 2^62.  So limit these to that range.
#+x87
(dolist (stuff '((sin %sin %sin-quick 63)
		 (cos %cos %cos-quick 63)
		 (tan %tan %tan-quick 62)))
  (destructuring-bind (name prim prim-quick limit)
      stuff
    (deftransform name ((x) '(single-float) '* :eval-name t)
      (if (and (backend-featurep :x86)
	       (not (backend-featurep :sse2)))
	  (cond ((csubtypep (continuation-type x)
			    (specifier-type `(single-float
					      (,(- (expt 2f0 limit)))
					      (,(expt 2f0 limit)))))
		 `(coerce (,prim-quick (coerce x 'double-float))
		   'single-float))
		(t 
		 (compiler-note
		  _N"Unable to avoid inline argument range check~@
                      because the argument range (~s) was not within 2^~D"
		  (type-specifier (continuation-type x))
		  limit)
		 `(coerce (,prim (coerce x 'double-float)) 'single-float)))
	  `(coerce (,prim (coerce x 'double-float)) 'single-float)))
    (deftransform name ((x) '(double-float) '* :eval-name t :when :both)
      (if (and (backend-featurep :x86)
	       (not (backend-featurep :sse2)))
	  (cond ((csubtypep (continuation-type x)
			    (specifier-type `(double-float
					      (,(- (expt 2d0 limit)))
					      (,(expt 2d0 limit)))))
		 `(,prim-quick x))
		(t 
		 (compiler-note
		  _N"Unable to avoid inline argument range check~@
                   because the argument range (~s) was not within 2^~D"
		  (type-specifier (continuation-type x))
		  limit)
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

(deftransform log ((x y) ((or (member 0f0) (single-float (0f0)))
			  (constant-argument number))
		   single-float)
  ;; Transform (log x 2) and (log x 10) to something simpler.
  (let ((y-val (continuation-value y)))
    (unless (and (not-more-contagious y x)
		 (or (= y-val 2)
		     (= y-val 10)))
      (give-up))
    (cond ((= y-val 10)
	   `(coerce (kernel:%log10 (float x 1d0)) 'single-float))
	  ((= y-val 2)
	   `(coerce (kernel:%log2 (float x 1d0)) 'single-float)))))

(deftransform log ((x y) ((or (member 0d0) (double-float 0d0))
			  (constant-argument number))
		   double-float)
  ;; Transform (log x 2) and (log x 10) to something simpler.
  (let ((y-val (continuation-value y)))
    (unless (and (not-more-contagious y x)
		 (or (= y-val 2)
		     (= y-val 10)))
      (give-up))
    (cond ((= y-val 10)
	   `(kernel:%log10 (float x 1d0)))
	  ((= y-val 2)
	   `(kernel:%log2 (float x 1d0))))))

;;; Handle some simple transformations
  
(deftransform abs ((x) ((complex double-float)) double-float :when :both)
  '(%hypot (realpart x) (imagpart x)))

(deftransform abs ((x) ((complex single-float)) single-float)
  '(coerce (%hypot (coerce (realpart x) 'double-float)
		   (coerce (imagpart x) 'double-float))
	  'single-float))

(deftransform abs ((x) (real) real)
  (let ((x-type (continuation-type x)))
    ;; If the arg is known to non-negative, we can just return the
    ;; arg.  However, (abs -0.0) is 0.0, so this transform only works
    ;; on floats that are known not to include negative zero.
    (if (csubtypep x-type (specifier-type '(or (rational 0) (float (0d0)) (member 0f0 0d0))))
	'x
	(give-up))))

(deftransform phase ((x) ((complex double-float)) double-float :when :both)
  '(%atan2 (imagpart x) (realpart x)))

(deftransform phase ((x) ((complex single-float)) single-float)
  '(coerce (%atan2 (coerce (imagpart x) 'double-float)
		   (coerce (realpart x) 'double-float))
	  'single-float))

(deftransform phase ((x) ((float)) float :when :both)
  '(if (minusp (float-sign x))
       (float pi x)
       (float 0 x)))

;;; The number is of type REAL.
(declaim (inline numeric-type-real-p))
(defun numeric-type-real-p (type)
  (and (numeric-type-p type)
       (eq (numeric-type-complexp type) :real)))

;;; Coerce a numeric type bound to the given type while handling
;;; exclusive bounds.
(defun coerce-numeric-bound (bound type)
  (when bound
    (if (consp bound)
	(list (coerce (car bound) type))
	(coerce bound type))))


;;;; Optimizers for elementary functions
;;;;
;;;; These optimizers compute the output range of the elementary
;;;; function, based on the domain of the input.
;;;;

;;; Generate a specifier for a complex type specialized to the same
;;; type as the argument.
(defun complex-float-type (arg)
  (declare (type numeric-type arg))
  (let* ((format (case (numeric-type-class arg)
		   ((integer rational) 'single-float)
		   (t (numeric-type-format arg))))
	 (float-type (or format 'float)))
    (specifier-type `(complex ,float-type))))

;;; Compute a specifier like '(or float (complex float)), except float
;;; should be the right kind of float.  Allow bounds for the float
;;; part too.
(defun float-or-complex-float-type (arg &optional lo hi)
  (declare (type numeric-type arg))
  (let* ((format (case (numeric-type-class arg)
		   ((integer rational) 'single-float)
		   (t (numeric-type-format arg))))
	 (float-type (or format 'float))
	 (lo (coerce-numeric-bound lo float-type))
	 (hi (coerce-numeric-bound hi float-type)))
    (specifier-type `(or (,float-type ,(or lo '*) ,(or hi '*))
		         (complex ,float-type)))))

;;; Domain-Subtype
;;;
;;; Test if the numeric-type ARG is within in domain specified by
;;; DOMAIN-LOW and DOMAIN-HIGH, consider negative and positive zero to
;;; be distinct as for the :negative-zero-is-not-zero feature. Note
;;; that only inclusive and open domain limits are handled as these
;;; are the only types of limits currently used. With the
;;; :negative-zero-is-not-zero feature this could be handled by the
;;; numeric subtype code in type.lisp.
;;;
(defun domain-subtypep (arg domain-low domain-high)
  (declare (type numeric-type arg)
	   (type (or real null) domain-low domain-high))
  (let* ((arg-lo (numeric-type-low arg))
	 (arg-lo-val (bound-value arg-lo))
	 (arg-hi (numeric-type-high arg))
	 (arg-hi-val (bound-value arg-hi)))
    ;; Check that the ARG bounds are correctly canonicalised.
    (when (and arg-lo (floatp arg-lo-val) (zerop arg-lo-val) (consp arg-lo)
	       (minusp (float-sign arg-lo-val)))
      (compiler-note _N"Float zero bound ~s not correctly canonicalised?" arg-lo)
      (setq arg-lo 0l0 arg-lo-val 0l0))
    (when (and arg-hi (zerop arg-hi-val) (floatp arg-hi-val) (consp arg-hi)
	       (plusp (float-sign arg-hi-val)))
      (compiler-note _N"Float zero bound ~s not correctly canonicalised?" arg-hi)
      (setq arg-hi -0l0 arg-hi-val -0l0))
    (flet ((fp-neg-zero-p (f)	; Is F -0.0?
	     (and (floatp f) (zerop f) (minusp (float-sign f))))
	   (fp-pos-zero-p (f)	; Is F +0.0? 
	     (and (floatp f) (zerop f) (plusp (float-sign f)))))
      (and (or (null domain-low)
	       (and arg-lo (>= arg-lo-val domain-low)
		    (not (and (fp-pos-zero-p domain-low)
			      (fp-neg-zero-p arg-lo)))))
	   (or (null domain-high)
	       (and arg-hi (<= arg-hi-val domain-high)
		    (not (and (fp-neg-zero-p domain-high)
			      (fp-pos-zero-p arg-hi)))))))))

;;; Elfun-Derive-Type-Simple
;;; 
;;; Handle monotonic functions of a single variable whose domain is
;;; possibly part of the real line.  ARG is the variable, FCN is the
;;; function, and DOMAIN is a specifier that gives the (real) domain
;;; of the function.  If ARG is a subset of the DOMAIN, we compute the
;;; bounds directly.  Otherwise, we compute the bounds for the
;;; intersection between ARG and DOMAIN, and then append a complex
;;; result, which occurs for the parts of ARG not in the DOMAIN.
;;;
;;; Negative and positive zero are considered distinct within
;;; DOMAIN-LOW and DOMAIN-HIGH, as for the :negative-zero-is-not-zero
;;; feature.
;;;
;;; DEFAULT-LOW and DEFAULT-HIGH are the lower and upper bounds if we
;;; can't compute the bounds using FCN.
;;;
(defun elfun-derive-type-simple (arg fcn domain-low domain-high
				     default-low default-high
				     &optional (increasingp t))
  (declare (type (or null real) domain-low domain-high))
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (complex-float-type arg))
	   ((numeric-type-real-p arg)
	    ;; The argument is real, so let's find the intersection
	    ;; between the argument and the domain of the function.
	    ;; We compute the bounds on the intersection, and for
	    ;; everything else, we return a complex number of the
	    ;; appropriate type.
	    (multiple-value-bind (intersection difference)
		(interval-intersection/difference
		 (numeric-type->interval arg)
		 (make-interval :low domain-low :high domain-high))
	      (cond
		(intersection
		 ;; Process the intersection.
		 (let* ((low (interval-low intersection))
			(high (interval-high intersection))
			(res-lo (or (bound-func fcn (if increasingp low high))
				    default-low))
			(res-hi (or (bound-func fcn (if increasingp high low))
				    default-high))
			;; Result specifier type.
			(format (case (numeric-type-class arg)
				  ((integer rational) 'single-float)
				  (t (numeric-type-format arg))))
			(bound-type (or format 'float))
			(result-type 
			 (make-numeric-type
			  :class 'float
			  :format format
			  :low (coerce-numeric-bound res-lo bound-type)
			  :high (coerce-numeric-bound res-hi bound-type))))
		   ;; If the ARG is a subset of the domain, we don't
		   ;; have to worry about the difference, because that
		   ;; can't occur.
		   (if (or (null difference)
			   ;; Check if the arg is within the domain.
			   (domain-subtypep arg domain-low domain-high))
		       result-type
		       (list result-type
			     (specifier-type `(complex ,bound-type))))))
		(t
		 ;; No intersection so the result must be purely complex.
		 (complex-float-type arg)))))
	   (t
	    (float-or-complex-float-type arg default-low default-high))))))

(macrolet
    ((frob (name domain-low domain-high def-low-bnd def-high-bnd
		 &key (increasingp t))
       (let ((num (gensym)))
	 `(defoptimizer (,name derive-type) ((,num))
	   (one-arg-derive-type
	    ,num
	    #'(lambda (arg)
		(elfun-derive-type-simple arg #',name
					  ,domain-low ,domain-high
					  ,def-low-bnd ,def-high-bnd
					  ,increasingp))
	    #',name)))))
  ;; These functions are easy because they are defined for the whole
  ;; real line.
  (frob exp nil nil 0 nil)
  (frob sinh nil nil nil nil)
  (frob tanh nil nil -1 1)
  (frob asinh nil nil nil nil)

  ;; These functions are only defined for part of the real line.  The
  ;; condition selects the desired part of the line.  
  (frob asin -1d0 1d0 (- (/ pi 2)) (/ pi 2))
  ;; Acos is monotonic decreasing, so we need to swap the function
  ;; values at the lower and upper bounds of the input domain.
  (frob acos -1d0 1d0 0 pi :increasingp nil)
  (frob acosh 1d0 nil nil nil)
  (frob atanh -1d0 1d0 -1 1)
  ;; Kahan says that (sqrt -0.0) is -0.0, so use a specifier that
  ;; includes -0.0.
  (frob sqrt -0d0 nil 0 nil))
 
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
  (case (c::interval-range-info y 0d0)
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

;;; Handle the case when x < 0, and when y is known to be an integer.
;;; In this case, we can do something useful because the x^y is still
;;; a real number if x and y are.
(defun interval-expt-<-0 (x y)
  #+(or)
  (progn
    (format t "x = ~A~%" x)
    (format t "range-info y (~A) = ~A~%" y (interval-range-info y)))
  (flet ((handle-positive-power-0 (x y)
	   ;; -1 <= X <= 0 and Y is positive.  We need to consider if
	   ;; Y contains an odd integer or not.  Find the smallest
	   ;; even and odd integer (if possible) contained in Y.
	   (let* ((y-lo (bound-value (interval-low y)))
		  (min-odd (if (oddp y-lo)
			       y-lo
			       (let ((y-odd (1+ y-lo)))
				 (if (interval-contains-p y-odd y)
				     y-odd
				     nil))))
		  (min-even (if (evenp y-lo)
				y-lo
				(let ((y-even (1+ y-lo)))
				  (if (interval-contains-p y-even y)
				      y-even
				      nil)))))
	     (cond ((and min-odd min-even)
		    ;; The Y interval contains both even and odd
		    ;; integers.  Then the lower bound is (least
		    ;; x)^(least positive odd), because this
		    ;; creates the most negative value.  The upper
		    ;; is (most x)^(least positive even), because
		    ;; this is the most positive number.
		    ;;
		    ;; (Recall that if |x|<1, |x|^y gets smaller as y
		    ;; increases.)
		    (let ((lo (safe-expt (bound-value (interval-low x))
					 min-odd))
			  (hi (safe-expt (bound-value (interval-high x))
					 min-even)))
		      (list (make-interval :low lo :high hi))))
		   (min-odd
		    ;; Y consists of just one odd integer.
		    (assert (oddp min-odd))
		    (let ((lo (safe-expt (bound-value (interval-low x))
					 min-odd))
			  (hi (safe-expt (bound-value (interval-high x))
					 min-odd)))
		      (list (make-interval :low lo :high hi))))
		   (min-even
		    ;; Y consists of just one even integer.
		    (assert (evenp min-even))
		    (let ((lo (safe-expt (bound-value (interval-high x))
					 min-even))
			  (hi (safe-expt (bound-value (interval-low x))
					 min-even)))
		      (list (make-interval :low lo :high hi))))
		   (t
		    ;; No mininum even or odd integer, so Y has no
		    ;; lower bound
		    (list (make-interval :low nil :high nil))))))
	 (handle-positive-power-1 (x y)
	   ;; X <= -1, Y is a positive integer.  Find the largest even
	   ;; and odd integer contained in Y, if possible.
	   (let* ((y-hi (bound-value (interval-high y)))
		  (max-odd (if y-hi
			       (if (oddp y-hi)
				   y-hi
				   (let ((y-odd (1- y-hi)))
				     (if (interval-contains-p y-odd y)
					 y-odd
					 nil)))
			       nil))
		  (max-even (if y-hi
				(if (evenp y-hi)
				    y-hi
				    (let ((y-even (1- y-hi)))
				      (if (interval-contains-p y-even y)
					  y-even
					  nil)))
				nil)))
	     ;; At least one of max-odd and max-even must be non-NIL!
	     (cond ((and max-odd max-even)
		    ;; The Y interval contains both even and odd
		    ;; integers.  Then the lower bound is (least
		    ;; x)^(most positive odd), because this
		    ;; creates the most negative value.  The upper
		    ;; is (least x)^(most positive even), because
		    ;; this is the most positive number.
		    ;;
		    (let ((lo (safe-expt (bound-value (interval-low x))
					 max-odd))
			  (hi (safe-expt (bound-value (interval-low x))
					 max-even)))
		      (list (make-interval :low lo :high hi))))
		   (max-odd
		    ;; Y consists of just one odd integer.
		    (assert (oddp max-odd))
		    (let ((lo (safe-expt (bound-value (interval-low x))
					 max-odd))
			  (hi (safe-expt (bound-value (interval-high x))
					 max-odd)))
		      (list (make-interval :low lo :high hi))))
		   (max-even
		    ;; Y consists of just one even integer.
		    (assert (evenp max-even))
		    (let ((lo (safe-expt (bound-value (interval-high x))
					 max-even))
			  (hi (safe-expt (bound-value (interval-low x))
					 max-even)))
		      (list (make-interval :low lo :high hi))))
		   (t
		    ;; No maximum even or odd integer, which means y
		    ;; is no upper bound.
		    (list (make-interval :low nil :high nil)))))))
    ;; We need to split into x < -1 and -1 <= x <= 0, first.
    (case (interval-range-info x -1)
      ('+
       ;; -1 <= x <= 0
       #+(or)
       (format t "x range +~%")
       (case (interval-range-info y 0)
	 ('+
	  (handle-positive-power-0 x y))
	 ('-
	  ;; Y is negative.  We should do something better
	  ;; than this because there's an extra rounding which
	  ;; we shouldn't do.
	  #+(or)
	  (format t "Handle y neg~%")
	  (let ((unit (make-interval :low 1 :high 1))
		(result (handle-positive-power-0 x (interval-neg y))))
	    #+(or)
	    (format t "result = ~A~%" result)
	    (mapcar #'(lambda (r)
			(interval-div unit r))
		    result)))
	 (t
	  ;; Split the interval and try again.  Since we know y is an
	  ;; integer, we don't need interval-split.  Also we want to
	  ;; handle an exponent of 0 ourselves as a special case.
	  (multiple-value-bind (y- y+)
	      (values (make-interval :low (interval-low y)
				     :high -1)
		      (make-interval :low 1
				     :high (interval-high y)))
	    (append (list (make-interval :low 1 :high 1))
		    (interval-expt-<-0 x y-)
		    (interval-expt-<-0 x y+))))))
      ('-
       ;; x < -1
       (case (c::interval-range-info y)
	 ('+
	  ;; Y is positive.  We need to consider if Y contains an
	  ;; odd integer or not.
	  ;;
	  (handle-positive-power-1 x y))
	 ('-
	  ;; Y is negative.  Do this in a better way
	  (let ((unit (make-interval :low 1 :high 1))
		(result (handle-positive-power-1 x (interval-neg y))))
	    (mapcar #'(lambda (r)
			(interval-div unit r))
		    result)))
	 (t
	  ;; Split the interval and try again.
	  #+(or)
	  (format t "split y ~A~%" y)
	  (multiple-value-bind (y- y+)
	      (values (make-interval :low (interval-low y) :high -1)
		      (make-interval :low 1 :high (interval-high y)))
	    (append (list (make-interval :low 1 :high 1))
		    (interval-expt-<-0 x y-)
		    (interval-expt-<-0 x y+))))))
      (t
       #+(or)
       (format t "splitting x ~A~%" x)
       (destructuring-bind (neg pos)
	   (interval-split -1 x t t)
	 (append (interval-expt-<-0 neg y)
		 (interval-expt-<-0 pos y)))))))

;;; Handle the case when x <= 1
(defun interval-expt-< (x y &optional integer-power-p)
  (case (c::interval-range-info x 0d0)
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
	  ;; If the low bound, LO, is NIL, that means the we have
	  ;; +0.0^inf, which is +0.0, but NIL is returned by
	  ;; SAFE-EXPT.  That means the result is includes +0.0.  Make
	  ;; it so by returning a member type and an exclusive
	  ;; interval.
	  (if lo
	      (list (c::make-interval :low lo :high (or hi 1)))
	      (list (c::make-interval :low (list 0) :high (or hi 1))
		    (c::make-member-type :members (list 0))))))
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
	  (list (interval-expt-< x y- integer-power-p)
		(interval-expt-< x y+ integer-power-p))))))
    ('-
     ;; The case where x <= 0.
     (cond (integer-power-p
	    (interval-expt-<-0 x y))
	   (t
	    ;; Y is not an integer.  Just give up and return an
	    ;; unbounded interval.
	    (list (c::make-interval :low nil :high nil)))))
    (t
     (destructuring-bind (neg pos)
	 (interval-split 0 x t t)
       (append (interval-expt-< neg y integer-power-p)
	       (interval-expt-< pos y integer-power-p))))))

;;; Compute bounds for (expt x y)

(defun interval-expt (x y &optional integer-power-p)
  (case (interval-range-info x 1)
    ('+
     ;; X >= 1
	 (interval-expt-> x y))
    ('-
     ;; X <= 1
     (interval-expt-< x y integer-power-p))
    (t
     (destructuring-bind (left right)
	 (interval-split 1 x t t)
       (append (interval-expt left y integer-power-p)
	       (interval-expt right y integer-power-p))))))

(defun fixup-interval-expt (bnd x-int y-int x-type y-type)
  (declare (ignore x-int))
  ;; Figure out what the return type should be, given the argument
  ;; types and bounds and the result type and bounds.
  (flet ((low-bnd (b)
	   (etypecase b
	     (member-type
	      (reduce #'min (member-type-members b)))
	     (interval
	      (interval-low b))))
	 (hi-bnd (b)
	   (etypecase b
	     (member-type
	      (reduce #'max (member-type-members b)))
	     (interval
	      (interval-high b)))))
    (cond ((csubtypep x-type (specifier-type 'integer))
	   ;; An integer to some power.  Cases to consider:
	   (case (numeric-type-class y-type)
	     (integer
	      ;; Positive integer to an integer power is either an
	      ;; integer or a rational.
	      (let ((lo (or (low-bnd bnd) '*))
		    (hi (or (hi-bnd bnd) '*)))
		(if (and (interval-low y-int)
			 (>= (bound-value (interval-low y-int)) 0))
		    (specifier-type `(integer ,lo ,hi))
		    (specifier-type `(rational ,lo ,hi)))))
	     (rational
	      ;; Positive integer to rational power is either a rational
	      ;; or a single-float.
	      (let* ((lo (low-bnd bnd))
		     (hi (hi-bnd bnd))
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
	      ;; Positive integer to a float power is a float of the
	      ;; same type.
	      (let* ((res (copy-numeric-type y-type))
		     (lo (low-bnd bnd))
		     (hi (hi-bnd bnd))
		     (f-lo (if lo
			       (coerce lo (numeric-type-format res))
			       nil))
		     (f-hi (if hi
			       (coerce hi (numeric-type-format res))
			       nil)))
		(setf (numeric-type-low res) f-lo)
		(setf (numeric-type-high res) f-hi)
		res))
	     (t
	      ;; Positive integer to a number is a number (for now)
	      (specifier-type 'number))))
	  ((csubtypep x-type (specifier-type 'rational))
	   ;; A rational to some power
	   (case (numeric-type-class y-type)
	     (integer
	      ;; Positive rational to an integer power is always a rational
	      (specifier-type `(rational ,(or (low-bnd bnd) '*)
					 ,(or (hi-bnd bnd) '*))))
	     (rational
	      ;; Positive rational to rational power is either a rational
	      ;; or a single-float.
	      (let* ((lo (low-bnd bnd))
		     (hi (hi-bnd bnd))
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
		(setf (numeric-type-low res) (low-bnd bnd))
		(setf (numeric-type-high res) (hi-bnd bnd))
		res))
	     (t
	      ;; Positive rational to a number is a number (for now)
	      (specifier-type 'number))))
	  ((csubtypep x-type (specifier-type 'float))
	   ;; A float to some power
	   (flet ((make-result (type)
		    (let ((res-type (or type 'float)))
		      (etypecase bnd
			(member-type
			 ;; Coerce all elements to the appropriate float
			 ;; type.
			 (make-member-type :members (mapcar #'(lambda (x)
								(coerce x res-type))
							    (member-type-members bnd))))
			(interval
			 (make-numeric-type
			  :class 'float
			  :format type
			  :low (coerce-numeric-bound (low-bnd bnd) res-type)
			  :high (coerce-numeric-bound (hi-bnd bnd) res-type)))))))
	     (case (numeric-type-class y-type)
	       ((or integer rational)
		;; Positive float to an integer or rational power is always a float
		(make-result (numeric-type-format x-type)))
	       (float
		;; Positive float to a float power is a float of the higher type
		(make-result (float-format-max (numeric-type-format x-type)
					       (numeric-type-format y-type))))
	       (t
		;; Positive float to a number is a number (for now)
		(specifier-type 'number)))))
	  (t
	   ;; A number to some power is a number.
	   (specifier-type 'number)))))
  
(defun merged-interval-expt (x y &optional integer-power-p)
  (let* ((x-int (numeric-type->interval x))
	 (y-int (numeric-type->interval y)))
    (mapcar #'(lambda (type)
		(fixup-interval-expt type x-int y-int x y))
	    (flatten-list (interval-expt x-int y-int integer-power-p)))))

(defun expt-derive-type-aux (x y same-arg)
  (declare (ignore same-arg))
  (cond ((or (not (numeric-type-real-p x))
	     (not (numeric-type-real-p y)))
	 ;; Use numeric contagion if either is not real
	 (numeric-contagion x y))
	((csubtypep y (specifier-type 'integer))
	 ;; A real raised to an integer power is well-defined
	 (merged-interval-expt x y t))
	(t
	 ;; A real raised to a non-integral power is complicated....
	 (cond ((or (csubtypep x (specifier-type '(rational 0)))
		    (csubtypep x (specifier-type '(float (0d0)))))
		;; A positive real to any power is well-defined.
		(merged-interval-expt x y))
	       ((and (csubtypep x (specifier-type 'rational))
		     (csubtypep x (specifier-type 'rational)))
		;; A rational to a rational power can be a rational or
		;; a single-float or a complex single-float.
		(specifier-type '(or rational single-float (complex single-float))))
	       (t
		;; A real to some power.  The result could be a real
		;; or a complex.
		(float-or-complex-float-type (numeric-contagion x y)))))))

(defoptimizer (expt derive-type) ((x y))
  (two-arg-derive-type x y #'expt-derive-type-aux #'expt))


;;; Note must assume that a type including 0.0 may also include -0.0
;;; and thus the result may be complex -infinity + i*pi.
;;;
(defun log-derive-type-aux-1 (x)
  (elfun-derive-type-simple x #'log 0d0 nil nil nil))

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
  (elfun-derive-type-simple y #'atan nil nil (- (/ pi 2)) (/ pi 2)))

(defun atan-derive-type-aux-2 (y x same-arg)
  (declare (ignore same-arg))
  ;; The hard case with two args.  We just return the max bounds.
  (let ((result-type (numeric-contagion y x)))
    (cond ((and (numeric-type-real-p x)
		(numeric-type-real-p y))
	   (let* ((format (case (numeric-type-class result-type)
			    ((integer rational) 'single-float)
			    (t (numeric-type-format result-type))))
		  (bound-format (or format 'float)))
	     (make-numeric-type :class 'float
				:format format
				:complexp :real
				:low (coerce (- pi) bound-format)
				:high (coerce pi bound-format))))
	  (t
	   ;; The result is a float or a complex number
	   (float-or-complex-float-type result-type)))))

(defoptimizer (atan derive-type) ((y &optional x))
  (if x
      (two-arg-derive-type y x #'atan-derive-type-aux-2 #'atan)
      (one-arg-derive-type y #'atan-derive-type-aux-1 #'atan)))


(defun cosh-derive-type-aux (x)
  ;; We note that cosh x = cosh |x| for all real x.
  (elfun-derive-type-simple
   (if (numeric-type-real-p x)
       (abs-derive-type-aux x)
       x)
   #'cosh nil nil 0 nil))

(defoptimizer (cosh derive-type) ((num))
  (one-arg-derive-type num #'cosh-derive-type-aux #'cosh))


(defun phase-derive-type-aux (arg)
  (let* ((format (case (numeric-type-class arg)
		   ((integer rational) 'single-float)
		   (t (numeric-type-format arg))))
	 (bound-type (or format 'float)))
    (cond ((numeric-type-real-p arg)
	   (case (interval-range-info (numeric-type->interval arg) 0.0)
	     ('+
	      ;; The number is positive, so the phase is 0.
	      (make-numeric-type :class 'float
				 :format format
				 :complexp :real
				 :low (coerce 0 bound-type)
				 :high (coerce 0 bound-type)))
	     ('-
	      ;; The number is always negative, so the phase is pi
	      (make-numeric-type :class 'float
				 :format format
				 :complexp :real
				 :low (coerce pi bound-type)
				 :high (coerce pi bound-type)))
	     (t
	      ;; We can't tell.  The result is 0 or pi.  Use a union
	      ;; type for this
	      (list
	       (make-numeric-type :class 'float
				  :format format
				  :complexp :real
				  :low (coerce 0 bound-type)
				  :high (coerce 0 bound-type))
	       (make-numeric-type :class 'float
				  :format format
				  :complexp :real
				  :low (coerce pi bound-type)
				  :high (coerce pi bound-type))))))
	  (t
	   ;; We have a complex number.  The answer is the range -pi
	   ;; to pi.  (-pi is included because we have -0.)
	   (make-numeric-type :class 'float
			      :format format
			      :complexp :real
			      :low (coerce (- pi) bound-type)
			      :high (coerce pi bound-type))))))

(defoptimizer (phase derive-type) ((num))
  (one-arg-derive-type num #'phase-derive-type-aux #'phase))


(deftransform realpart ((x) ((complex rational)) *)
  '(kernel:%realpart x))
(deftransform imagpart ((x) ((complex rational)) *)
  '(kernel:%imagpart x))

;;; Make REALPART and IMAGPART return the appropriate types.  This
;;; should help a lot in optimized code.

(defun realpart-derive-type-aux (type)
  (let ((class (numeric-type-class type))
	(format (numeric-type-format type)))
    (cond ((numeric-type-real-p type)
	   ;; The realpart of a real has the same type and range as
	   ;; the input.
	   (make-numeric-type :class class
			      :format format
			      :complexp :real
			      :low (numeric-type-low type)
			      :high (numeric-type-high type)))
	  (t
	   ;; We have a complex number.  The result has the same type
	   ;; as the real part, except that it's real, not complex,
	   ;; obviously.
	   (make-numeric-type :class class
			      :format format
			      :complexp :real
			      :low (numeric-type-low type)
			      :high (numeric-type-high type))))))

(defoptimizer (realpart derive-type) ((num))
  (one-arg-derive-type num #'realpart-derive-type-aux #'realpart))

(defun imagpart-derive-type-aux (type)
  (let ((class (numeric-type-class type))
	(format (numeric-type-format type)))
    (cond ((numeric-type-real-p type)
	   ;; The imagpart of a real has the same type as the input,
	   ;; except that it's zero
	   (let ((bound-format (or format class 'real)))
	     (make-numeric-type :class class
				:format format
				:complexp :real
				:low (coerce 0 bound-format)
				:high (coerce 0 bound-format))))
	  (t
	   ;; We have a complex number.  The result has the same type as
	   ;; the imaginary part, except that it's real, not complex,
	   ;; obviously.
	   (make-numeric-type :class class
			      :format format
			      :complexp :real
			      :low (numeric-type-low type)
			      :high (numeric-type-high type))))))

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
;;; of complex operation VOPs.  Some architectures have vops, though.
;;;
(macrolet
    ((frob (type &optional (real-type type))
       ;; These are functions for which we normally would want to
       ;; write vops for.
       `(progn
	  (deftransform %negate ((z) ((complex ,type)) *)
	    ;; Negation
	    '(complex (%negate (realpart z)) (%negate (imagpart z))))
	  (deftransform + ((w z) ((complex ,type) (complex ,type)) *)
	    ;; Complex + complex
	    '(complex (+ (realpart w) (realpart z))
	              (+ (imagpart w) (imagpart z))))
	  (deftransform - ((w z) ((complex ,type) (complex ,type)) *)
	    ;; Complex - complex
	    '(complex (- (realpart w) (realpart z))
	              (- (imagpart w) (imagpart z))))
	  (deftransform + ((w z) ((complex ,type) ,real-type) *)
	    ;; Complex + real
	    '(complex (+ (realpart w) z) (+ 0 (imagpart w))))
	  (deftransform - ((w z) ((complex ,type) ,real-type) *)
	    ;; Complex - real
	    '(complex (- (realpart w) z) (- (imagpart w) 0)))
	  (deftransform * ((x y) ((complex ,type) (complex ,type)) *)
	    ;; Complex * complex
	    '(let* ((rx (realpart x))
		    (ix (imagpart x))
		    (ry (realpart y))
		    (iy (imagpart y)))
	       (complex (- (* rx ry) (* ix iy))
			(+ (* rx iy) (* ix ry)))))
	  ;; SSE2 can use a special transform
	  #-(and sse2 complex-fp-vops)
	  (deftransform / ((x y) ((complex ,type) (complex ,type)) *
			   :policy (> speed space))
	    ;; Complex / complex
	    '(let* ((rx (realpart x))
		    (ix (imagpart x))
		    (ry (realpart y))
		    (iy (imagpart y)))
	      (if (> (abs ry) (abs iy))
		  (let* ((r (/ iy ry))
			 (dn (+ ry (* r iy))))
		    (complex (/ (+ rx (* ix r)) dn)
			     (/ (- ix (* rx r)) dn)))
		  (let* ((r (/ ry iy))
			 (dn (+ iy (* r ry))))
		    (complex (/ (+ (* rx r) ix) dn)
			     (/ (- (* ix r) rx) dn))))))
	  (deftransform * ((w z) ((complex ,type) ,real-type) *)
	    ;; Complex*real
	    '(complex (* (realpart w) z) (* (imagpart w) z)))
	  (deftransform / ((w z) ((complex ,type) ,real-type) *)
	    ;; Complex/real
	    '(complex (/ (realpart w) z) (/ (imagpart w) z)))
	  )))

  #-complex-fp-vops
  (frob single-float)
  #-complex-fp-vops
  (frob double-float)
  #+double-double
  (frob double-double-float real))

(macrolet
    ((frob (type &optional (real-type type))
       ;; These are functions for which we probably wouldn't want to
       ;; write vops for.
       `(progn
	 #-complex-fp-vops
	 (deftransform conjugate ((z) ((complex ,type)) *)
	   ;; Conjugate of complex number
	   '(complex (realpart z) (- (imagpart z))))
	 #-complex-fp-vops
	 (deftransform - ((z w) (,real-type (complex ,type)) *)
	   ;; Real - complex.  The 0 for the imaginary part is
	   ;; needed so we get the correct signed zero.
	   '(- (complex z (coerce 0 ',real-type)) w))
	 #-complex-fp-vops
	 (deftransform + ((z w) (,real-type (complex ,type)) *)
	   ;; Real + complex.  The 0 for the imaginary part is
	   ;; needed so we get the correct signed zero.
	   '(+ (complex z (coerce 0 ',real-type)) w))
	 #-complex-fp-vops
	 (deftransform * ((z w) (,real-type (complex ,type)) *)
	   ;; Real * complex
	   '(complex (* z (realpart w)) (* z (imagpart w))))
	 (deftransform / ((rx y) (,real-type (complex ,type)) *)
	   ;; Real/complex
	   '(let* ((ry (realpart y))
		   (iy (imagpart y)))
	     (if (> (abs ry) (abs iy))
		 (let* ((r (/ iy ry))
			(dn (+ ry (* r iy))))
		   (complex (/ rx dn)
			    (/ (- (* rx r)) dn)))
		 (let* ((r (/ ry iy))
			(dn (+ iy (* r ry))))
		   (complex (/ (* rx r) dn)
			    (/ (- rx) dn))))))
	 ;; Comparison
	 (deftransform = ((w z) ((complex ,type) (complex ,type)) *)
	   '(and (= (realpart w) (realpart z))
	         (= (imagpart w) (imagpart z))))
	 (deftransform = ((w z) ((complex ,type) ,type) *)
	   '(and (= (realpart w) z) (zerop (imagpart w))))
	 (deftransform = ((w z) (,type (complex ,type)) *)
	   '(and (= (realpart z) w) (zerop (imagpart z))))
	 )))
  (frob single-float)
  (frob double-float)
  #+double-double
  (frob double-double-float))
  
(macrolet
    ((frob (type name)
       `(deftransform / ((x y) ((complex ,type) (complex ,type)) *)
		      '(,name x y))))
  (frob double-float kernel::cdiv-double-float)
  (frob single-float kernel::cdiv-single-float))


;;;; Complex contagion:

(progn
;;; COMPLEX-CONTAGION-ARG1, ARG2
;;;
;;;    Handles complex contagion of two complex numbers of different types.
(deftransform complex-contagion-arg1 ((x y) * * :defun-only t :node node)
  ;;(format t "complex-contagion arg1~%")
  `(,(continuation-function-name (basic-combination-fun node))
    (coerce x ',(type-specifier (continuation-type y))) y))
;;;
(deftransform complex-contagion-arg2 ((x y) * * :defun-only t :node node)
  ;;(format t "complex-contagion arg2~%")
  `(,(continuation-function-name (basic-combination-fun node))
    x (coerce y ',(type-specifier (continuation-type x)))))

(dolist (x '(= + * / -))
  (%deftransform x '(function ((complex single-float) (complex double-float)) *)
		 #'complex-contagion-arg1)
  (%deftransform x '(function ((complex double-float) (complex single-float)) *)
		 #'complex-contagion-arg2))

;;; COMPLEX-REAL-CONTAGION-ARG1, ARG2
;;;
;;;   Handles the case of mixed complex and real numbers.  We assume
;;; the real number doesn't cause complex number to increase in
;;; precision.
(deftransform complex-real-contagion-arg1 ((x y) * * :defun-only t :node node)
  ;;(format t "complex-real-contagion-arg1~%")
  `(,(continuation-function-name (basic-combination-fun node))
     (coerce x ',(numeric-type-format (continuation-type y)))
     y))
;;;
(deftransform complex-real-contagion-arg2 ((x y) * * :defun-only t :node node)
  ;;(format t "complex-real-contagion-arg2~%")
  `(,(continuation-function-name (basic-combination-fun node))
     x
     (coerce y ',(numeric-type-format (continuation-type x)))))


(dolist (x '(= + * / -))
  (%deftransform x '(function ((or rational single-float) (complex double-float)) *)
		 #'complex-real-contagion-arg1)
  (%deftransform x '(function ((complex double-float) (or rational single-float)) *)
		 #'complex-real-contagion-arg2)
  (%deftransform x '(function (rational (complex single-float)) *)
		 #'complex-real-contagion-arg1)
  (%deftransform x '(function ((complex single-float) rational) *)
		 #'complex-real-contagion-arg2))

;;; UPGRADED-COMPLEX-REAL-CONTAGION-ARG1, ARG2
;;;
;;;   Handles the case of mixed complex and real numbers.  We assume
;;; the real number is more precise than the complex, so that the
;;; complex number needs to be coerced to a more precise complex.
(deftransform upgraded-complex-real-contagion-arg1 ((x y) * * :defun-only t :node node)
  ;;(format t "upgraded-complex-real-contagion-arg1~%")
  `(,(continuation-function-name (basic-combination-fun node))
     (coerce x '(complex ,(numeric-type-format (continuation-type y))))
     y))
;;;
(deftransform upgraded-complex-real-contagion-arg2 ((x y) * * :defun-only t :node node)
  #+nil
  (format t "upgraded-complex-real-contagion-arg2: ~A ~A~%"
	  (continuation-type x) (continuation-type y))
  `(,(continuation-function-name (basic-combination-fun node))
     x
     (coerce y '(complex ,(numeric-type-format (continuation-type x))))))


(dolist (x '(= + * / -))
  (%deftransform x '(function ((or (complex single-float) (complex rational))
			       double-float) *)
		 #'upgraded-complex-real-contagion-arg1)
  (%deftransform x '(function (double-float
			       (or (complex single-float) (complex rational))) *)
		 #'upgraded-complex-real-contagion-arg2))
)

;;; Here are simple optimizers for sin, cos, and tan.  They do not
;;; produce a minimal range for the result; the result is the widest
;;; possible answer.  This gets around the problem of doing range
;;; reduction correctly but still provides useful results when the
;;; inputs are union types.

(defun trig-derive-type-aux (arg domain fcn
				 &optional def-lo def-hi (increasingp t))
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
	    (complex-float-type arg))
	   ((numeric-type-real-p arg)
	    (let* ((format (case (numeric-type-class arg)
			     ((integer rational) 'single-float)
			     (t (numeric-type-format arg))))
		   (bound-type (or format 'float)))
	      ;; If the argument is a subset of the "principal" domain
	      ;; of the function, we can compute the bounds because
	      ;; the function is monotonic.  We can't do this in
	      ;; general for these periodic functions because we can't
	      ;; (and don't want to) do the argument reduction in
	      ;; exactly the same way as the functions themselves do
	      ;; it.
	      (if (csubtypep arg domain)
		  (let ((res-lo (bound-func fcn (numeric-type-low arg)))
			(res-hi (bound-func fcn (numeric-type-high arg))))
		    (unless increasingp
		      (rotatef res-lo res-hi))
		    (make-numeric-type
		     :class 'float
		     :format format
		     :low (coerce-numeric-bound res-lo bound-type)
		     :high (coerce-numeric-bound res-hi bound-type)))
		  (make-numeric-type
		   :class 'float
		   :format format
		   :low (and def-lo (coerce def-lo bound-type))
		   :high (and def-hi (coerce def-hi bound-type))))))
	   (t
	    (float-or-complex-float-type arg def-lo def-hi))))))

(defoptimizer (sin derive-type) ((num))
  (one-arg-derive-type
   num
   #'(lambda (arg)
       ;; Derive the bounds if the arg is in [-pi/2, pi/2]
       (trig-derive-type-aux
	arg
	(specifier-type `(float ,(- (/ pi 2)) ,(/ pi 2)))
	#'sin
	-1 1))
   #'sin))
       
(defoptimizer (cos derive-type) ((num))
  (one-arg-derive-type
   num
   #'(lambda (arg)
       ;; Derive the bounds if the arg is in [0, pi]
       (trig-derive-type-aux arg
			     (specifier-type `(float 0d0 ,pi))
			     #'cos
			     -1 1
			     nil))
   #'cos))

(defoptimizer (tan derive-type) ((num))
  (one-arg-derive-type
   num
   #'(lambda (arg)
       ;; Derive the bounds if the arg is in [-pi/2, pi/2]
       (trig-derive-type-aux arg
			     (specifier-type `(float ,(- (/ pi 2)) ,(/ pi 2)))
			     #'tan
			     nil nil))
   #'tan))

;;; conjugate always returns the same type as the input type  
(defoptimizer (conjugate derive-type) ((num))
  (continuation-type num))

(defoptimizer (cis derive-type) ((num))
  (one-arg-derive-type num
     #'(lambda (arg)
	 (specifier-type `(complex ,(or (numeric-type-format arg) 'float))))
     #'cis))

;;; Derive the result types of DECODE-FLOAT

(defun decode-float-frac-derive-type-aux (arg)
  ;; The fraction part of DECODE-FLOAT is always a subset of the
  ;; interval [0.5, 1), even for subnormals.  While possible to derive
  ;; a tighter bound in some cases, we don't.  Just return that
  ;; interval of the approriate type when possible.  If not, just use
  ;; float.
  (if (numeric-type-format arg)
      (specifier-type `(,(numeric-type-format arg)
			 ,(coerce 1/2 (numeric-type-format arg))
			 (,(coerce 1 (numeric-type-format arg)))))
      (specifier-type '(float 0.5 (1.0)))))

(defun decode-float-exp-derive-type-aux (arg)
  ;; Derive the exponent part of the float.  It's always an integer
  ;; type.
  (labels
      ((calc-exp (x)
	 (when x
	   (bound-func #'(lambda (arg)
			   (nth-value 1 (decode-float arg)))
		       x)))
       (min-exp (interval)
	 ;; (decode-float 0d0) returns an exponent of -1022.  But
	 ;; (decode-float least-positive-double-float returns -1073.
	 ;; Hence, if the low value is less than this, we need to
	 ;; return the exponent of the least positive number.
	 (let ((least (if (eq 'single-float (numeric-type-format arg))
			  least-positive-single-float
			  least-positive-double-float)))
	   (if (or (interval-contains-p 0 interval)
		   (interval-contains-p least interval))
	     (calc-exp least)
	     (calc-exp (bound-value (interval-low interval))))))
       (max-exp (interval)
	 ;; Use decode-float on the most postive number of the
	 ;; appropriate type to find the max exponent.  If we don't
	 ;; know the actual number format, use double, which has the
	 ;; widest range (including double-double-float).
	 (or (calc-exp (bound-value (interval-high interval)))
	     (calc-exp (if (eq 'single-float (numeric-type-format arg))
			   most-positive-single-float
			   most-positive-double-float)))))
    (let* ((interval (interval-abs (numeric-type->interval arg)))
	   (lo (min-exp interval))
	   (hi (max-exp interval)))
      (specifier-type `(integer ,(or lo '*) ,(or hi '*))))))

(defun decode-float-sign-derive-type-aux (arg)
  ;; Derive the sign of the float.
  (if (numeric-type-format arg)
      (let ((arg-range (interval-range-info (numeric-type->interval arg))))
	(case arg-range
	  (+ (make-member-type :members (list (coerce 1 (numeric-type-format arg)))))
	  (- (make-member-type :members (list (coerce -1 (numeric-type-format arg)))))
	  (otherwise
	   (make-member-type :members (list (coerce 1 (numeric-type-format arg))
					    (coerce -1 (numeric-type-format arg)))))))
      (specifier-type '(or (member 1f0 -1f0
			    1d0 -1d0
			    #+double-double 1w0
			    #+double-double -1w0)))))
    
(defoptimizer (decode-float derive-type) ((num))
  (let ((f (one-arg-derive-type num
				#'(lambda (arg)
				    (decode-float-frac-derive-type-aux arg))
				#'(lambda (arg)
				    (nth-value 0 (decode-float arg)))))
	(e (one-arg-derive-type num
				#'(lambda (arg)
				    (decode-float-exp-derive-type-aux arg))
				#'(lambda (arg)
				    (nth-value 1 (decode-float arg)))))
	(s (one-arg-derive-type num
				#'(lambda (arg)
				    (decode-float-sign-derive-type-aux arg))
				#'(lambda (arg)
				    (nth-value 2 (decode-float arg))))))
    (make-values-type :required (list f
				      e
				      s))))
