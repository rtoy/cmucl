;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/float-tran.lisp,v 1.8 1990/12/02 15:59:07 wlott Exp $
;;;
;;; This file contains floating-point specific transforms, and may be somewhat
;;; implementation dependent in its assumptions of what the formats are.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "C")


;;;; Coercions:

#-new-compiler
(progn
  (defun %single-float (x) (coerce x 'single-float))
  (defun %double-float (x) (coerce x 'double-float)))

(defknown %single-float (real) single-float (movable foldable flushable))
(defknown %double-float (real) double-float (movable foldable flushable))

(deftransform float ((n &optional f) (* &optional single-float))
  '(%single-float n))

(deftransform float ((n f) (* double-float))
  '(%double-float n))

(deftransform %single-float ((n) (single-float))
  'n)

(deftransform %double-float ((n) (double-float))
  'n)

(deftransform coerce ((n type)
		      (* (constant-argument
			  (member float short-float single-float))))
  '(%single-float n))

(deftransform coerce ((n type)
		      (* (constant-argument
			  (member double-float long-float))))
  '(%double-float n))


(defknown %unary-truncate (real) integer (movable foldable flushable))

;;; Not strictly a float function, but primarily useful on floats:
;;;
(deftransform truncate ((x &optional by)
			(* &optional (constant-argument (member 1))))
  '(let ((res (%unary-truncate x)))
     (values res (- x res))))


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


(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))
(defun single-float-bits (x) (single-float-bits x))
(defun double-float-high-bits (x) (double-float-high-bits x))
(defun double-float-low-bits (x) (double-float-low-bits x))

(def-source-transform float-sign (float1 &optional (float2 nil f2-p))
  (let ((n-f1 (gensym)))
    (if f2-p
	`(* (float-sign ,float1) (abs ,float2))
	`(let ((,n-f1 ,float1))
	   (declare (float ,n-f1))
	   (if (minusp (if (typep ,n-f1 'single-float)
			   (single-float-bits ,n-f1)
			   (double-float-high-bits ,n-f1)))
	       (float -1 ,n-f1)
	       (float 1 ,n-f1))))))


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
  (movable foldable flushable)))

(defknown scale-single-float (single-float fixnum) single-float
  (movable foldable flushable))

(defknown scale-double-float (double-float fixnum) double-float
  (movable foldable flushable))

(deftransform decode-float ((x) (single-float))
  '(decode-single-float x))

(deftransform decode-float ((x) (double-float))
  '(decode-double-float x))

(deftransform integer-decode-float ((x) (single-float))
  '(integer-decode-single-float x))

(deftransform integer-decode-float ((x) (double-float))
  '(integer-decode-double-float x))

(deftransform scale-float ((f ex) (single-float *))
  '(scale-single-float f ex))

(deftransform scale-float ((f ex) (double-float *))
  '(scale-double-float f ex))


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
	     `(deftransform ,op ((x y) (float rational))
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


;;;; Irrational stuff:

(defknown (%sin %cos %tan %asin %acos %atan %sinh %cosh %tanh %asinh
		%acosh %atanh %exp %expm1 %log %log10 %log1p %cbrt %sqrt)
	  (double-float) double-float
  (movable foldable flushable))

(defknown (%atan2 %pow %hypot)
	  (double-float double-float) double-float
  (movable foldable flushable))


(defmacro def-irrat-transforms (name prim args &optional assert-result)
  (flet ((frob (type)
	   (mapcar #'(lambda (x)
		       (declare (ignore x))
		       type)
		   args)))
    (let ((rtype (when assert-result '(float))))
      `(progn
	 (deftransform ,name (,args ,(frob 'single-float) ,@rtype)
	   '(coerce (,prim ,@(mapcar #'(lambda (arg)
					 `(coerce ,arg 'double-float))
				     args))
		    'single-float))
	 (deftransform ,name (,args ,(frob 'double-float) ,@rtype)
	   '(,prim ,@args))))))

(def-irrat-transforms exp %exp (x))
(def-irrat-transforms expt %pow (x y) t)
(def-irrat-transforms log %log (x) t)

(deftransform log ((x y) (float float) float)
  '(/ (log x) (log y)))

(def-irrat-transforms sqrt %sqrt (x) t)
(def-irrat-transforms sin %sin (x))
(def-irrat-transforms cos %cos (x))
(def-irrat-transforms tan %tan (x))
(def-irrat-transforms asin %asin (x) t)
(def-irrat-transforms acos %acos (x) t)
(def-irrat-transforms atan %atan (x) t)
(def-irrat-transforms atan %atan2 (x y) t)
(def-irrat-transforms sinh %sinh (x))
(def-irrat-transforms cosh %cosh (x))
(def-irrat-transforms tanh %tanh (x))
(def-irrat-transforms asinh %asinh (x))
(def-irrat-transforms acosh %acosh (x) t)
(def-irrat-transforms atanh %atanh (x) t)
