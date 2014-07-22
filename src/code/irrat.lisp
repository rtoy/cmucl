;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/irrat.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains all the irrational functions.  Actually, most of the
;;; work is done by calling out to C...
;;;
;;; Author: William Lott.
;;; 

(in-package "KERNEL")
(intl:textdomain "cmucl")


;;;; Random constants, utility functions, and macros.

(defconstant pi 3.14159265358979323846264338327950288419716939937511L0)
;(defconstant e 2.71828182845904523536028747135266249775724709369996L0)

;;; Make these INLINE, since the call to C is at least as compact as a Lisp
;;; call, and saves number consing to boot.
;;;
(defmacro def-math-rtn (name num-args)
  (let ((function (intern (concatenate 'simple-string
				       "%"
				       (string-upcase name)))))
    `(progn
       (declaim (inline ,function))
       (export ',function)
       (alien:def-alien-routine (,name ,function) double-float
	 ,@(let ((results nil))
	     (dotimes (i num-args (nreverse results))
	       (push (list (intern (format nil "ARG-~D" i))
			   'double-float)
		     results)))))))

(eval-when (compile load eval)

(defun handle-reals (function var)
  `((((foreach fixnum single-float bignum ratio))
     (coerce (,function (coerce ,var 'double-float)) 'single-float))
    ((double-float)
     (,function ,var))
    #+double-double
    ((double-double-float)
     (,(symbolicate "DD-" function) ,var))))

); eval-when (compile load eval)


;;;; Stubs for the Unix math library.

;;; Please refer to the Unix man pages for details about these routines.

;;; Trigonometric.
#-(and x86 (not sse2))
(progn
  ;; For x86 (without sse2), we can use x87 instructions to implement
  ;; these.  With sse2, we don't currently support that, so these
  ;; should be disabled.
  (def-math-rtn "sin" 1)
  (def-math-rtn "cos" 1)
  (def-math-rtn "tan" 1)
  (def-math-rtn "atan" 1)
  (def-math-rtn "atan2" 2))
(def-math-rtn "asin" 1)
(def-math-rtn "acos" 1)
(def-math-rtn "sinh" 1)
(def-math-rtn "cosh" 1)
(def-math-rtn "tanh" 1)
(def-math-rtn "asinh" 1)
(def-math-rtn "acosh" 1)
(def-math-rtn "atanh" 1)

;;; Exponential and Logarithmic.
#-(and x86 (not sse2))
(progn
  (def-math-rtn "exp" 1)
  (def-math-rtn "log" 1)
  (def-math-rtn "log10" 1))

(def-math-rtn "pow" 2)
#-(or x86 sparc-v7 sparc-v8 sparc-v9)
(def-math-rtn "sqrt" 1)
(def-math-rtn "hypot" 2)

;; Don't want log1p to use the x87 instruction.
#-(or hpux (and x86 (not sse2)))
(def-math-rtn "log1p" 1)

;; These are needed for use by byte-compiled files.  But don't use
;; these with sse2 since we don't support using the x87 instructions
;; here.
#+(and x86 (not sse2))
(progn
  #+nil
  (defun %sin (x)
    (declare (double-float x)
	     (values double-float))
    (%sin x))
  (defun %sin-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%sin-quick x))
  #+nil
  (defun %cos (x)
    (declare (double-float x)
	     (values double-float))
    (%cos x))
  (defun %cos-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%cos-quick x))
  #+nil
  (defun %tan (x)
    (declare (double-float x)
	     (values double-float))
    (%tan x))
  (defun %tan-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%tan-quick x))
  (defun %atan (x)
    (declare (double-float x)
	     (values double-float))
    (%atan x))
  (defun %atan2 (x y)
    (declare (double-float x y)
	     (values double-float))
    (%atan2 x y))
  (defun %exp (x)
    (declare (double-float x)
	     (values double-float))
    (%exp x))
  (defun %log (x)
    (declare (double-float x)
	     (values double-float))
    (%log x))
  (defun %log10 (x)
    (declare (double-float x)
	     (values double-float))
    (%log10 x))
  #+nil ;; notyet
  (defun %pow (x y)
    (declare (type (double-float 0d0) x)
	     (double-float y)
	     (values (double-float 0d0)))
    (%pow x y))
  (defun %sqrt (x)
    (declare (double-float x)
	     (values double-float))
    (%sqrt x))
  (defun %scalbn (f ex)
    (declare (double-float f)
	     (type (signed-byte 32) ex)
	     (values double-float))
    (%scalbn f ex))
  (defun %scalb (f ex)
    (declare (double-float f ex)
	     (values double-float))
    (%scalb f ex))
  (defun %logb (x)
    (declare (double-float x)
	     (values double-float))
    (%logb x))
  (defun %log1p (x)
    (declare (double-float x)
	     (values double-float))
    (%log1p x))
  ) ; progn


;; As above for x86.  It also seems to be needed to handle
;; constant-folding in the compiler.
#+(or sparc (and x86 sse2))
(progn
  (defun %sqrt (x)
    (declare (double-float x)
	     (values double-float))
    (%sqrt x))
  )

;; Same as above, but instead of needing to pass an array in, the
;; output array is broken up into two output values instead.  This is
;; easier for the user, and we don't have to wrap calls with
;; without-gcing.
(declaim (inline %ieee754-rem-pi/2))
(alien:def-alien-routine ("ieee754_rem_pio2" %ieee754-rem-pi/2) c-call:int
  (x double-float)
  (y0 double-float :out)
  (y1 double-float :out))

(declaim (inline %%sincos))
(alien:def-alien-routine ("sincos" %%sincos) c-call:void
  (x double-float)
  (s double-float :out)
  (c double-float :out))

(declaim (inline %sincos))
(defun %sincos (x)
  (declare (double-float x))
  (multiple-value-bind (ign s c)
      (%%sincos x)
    (values s c)))

#||
;; Implement sin/cos/tan in Lisp.  These are based on the routines
;; from fdlibm.

;; Block compile so the trig routines don't cons their args when
;; calling the kernel trig routines.
(declaim (ext:start-block kernel-sin kernel-cos kernel-tan
			  %sin %cos %tan
			  %sincos))

;; kernel sin function on [-pi/4, pi/4], pi/4 ~ 0.7854
;; Input x is assumed to be bounded by ~pi/4 in magnitude.
;; Input y is the tail of x.
;; Input iy indicates whether y is 0. (if iy=0, y assume to be 0). 
;;
;; Algorithm
;;	1. Since sin(-x) = -sin(x), we need only to consider positive x. 
;;	2. if x < 2^-27 (hx<0x3e400000 0), return x with inexact if x!=0.
;;	3. sin(x) is approximated by a polynomial of degree 13 on
;;	   [0,pi/4]
;;		  	         3            13
;;	   	sin(x) ~ x + S1*x + ... + S6*x
;;	   where
;;	
;; 	|sin(x)         2     4     6     8     10     12  |     -58
;; 	|----- - (1+S1*x +S2*x +S3*x +S4*x +S5*x  +S6*x   )| <= 2
;; 	|  x 					           | 
;; 
;;	4. sin(x+y) = sin(x) + sin'(x')*y
;;		    ~ sin(x) + (1-x*x/2)*y
;;	   For better accuracy, let 
;;		     3      2      2      2      2
;;		r = x *(S2+x *(S3+x *(S4+x *(S5+x *S6))))
;;	   then                   3    2
;;		sin(x) = x + (S1*x + (x *(r-y/2)+y))

(declaim (ftype (function (double-float double-float fixnum)
			  double-float)
		kernel-sin))

(defun kernel-sin (x y iy)
  (declare (type (double-float -1d0 1d0) x y)
	   (fixnum iy)
	   (optimize (speed 3) (safety 0)))
  (let ((ix (ldb (byte 31 0) (kernel:double-float-high-bits x))))
    (when (< ix #x3e400000)
      ;; |x| < 2^-27
      ;; Signal inexact if x /= 0
      (if (zerop (truncate x))
	  (return-from kernel-sin x)
	  (return-from kernel-sin x)))
    (let* ((s1 -1.66666666666666324348d-01) ; #xBFC55555 #x55555549
	   (s2  8.33333333332248946124d-03) ; #x3F811111 #x1110F8A6
	   (s3 -1.98412698298579493134d-04) ; #xBF2A01A0 #x19C161D5
	   (s4  2.75573137070700676789d-06) ; #x3EC71DE3 #x57B1FE7D
	   (s5 -2.50507602534068634195d-08) ; #xBE5AE5E6 #x8A2B9CEB
	   (s6  1.58969099521155010221d-10) ; #x3DE5D93A #x5ACFD57C
	   (z (* x x))
	   (v (* z x))
	   (r (+ s2
		 (* z
		    (+ s3
		       (* z
			  (+ s4
			     (* z
				(+ s5
				   (* z s6))))))))))
      (if (zerop iy)
	  (+ x (* v (+ s1 (* z r))))
	  (- x (- (- (* z (- (* .5 y)
			     (* v r)))
		     y)
		  (* v s1)))))))

;; kernel cos function on [-pi/4, pi/4], pi/4 ~ 0.785398164
;; Input x is assumed to be bounded by ~pi/4 in magnitude.
;; Input y is the tail of x. 
;;
;; Algorithm
;;	1. Since cos(-x) = cos(x), we need only to consider positive x.
;;	2. if x < 2^-27 (hx<0x3e400000 0), return 1 with inexact if x!=0.
;;	3. cos(x) is approximated by a polynomial of degree 14 on
;;	   [0,pi/4]
;;		  	                 4            14
;;	   	cos(x) ~ 1 - x*x/2 + C1*x + ... + C6*x
;;	   where the remez error is
;;	
;; 	|              2     4     6     8     10    12     14 |     -58
;; 	|cos(x)-(1-.5*x +C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  )| <= 2
;; 	|    					               | 
;; 
;; 	               4     6     8     10    12     14 
;;	4. let r = C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  , then
;;	       cos(x) = 1 - x*x/2 + r
;;	   since cos(x+y) ~ cos(x) - sin(x)*y 
;;			  ~ cos(x) - x*y,
;;	   a correction term is necessary in cos(x) and hence
;;		cos(x+y) = 1 - (x*x/2 - (r - x*y))
;;	   For better accuracy when x > 0.3, let qx = |x|/4 with
;;	   the last 32 bits mask off, and if x > 0.78125, let qx = 0.28125.
;;	   Then
;;		cos(x+y) = (1-qx) - ((x*x/2-qx) - (r-x*y)).
;;	   Note that 1-qx and (x*x/2-qx) is EXACT here, and the
;;	   magnitude of the latter is at least a quarter of x*x/2,
;;	   thus, reducing the rounding error in the subtraction.
(declaim (ftype (function (double-float double-float)
			  double-float)
		kernel-cos))

(defun kernel-cos (x y)
  (declare (type (double-float -1d0 1d0) x y)
	   (optimize (speed 3) (safety 0)))
  ;; cos(-x) = cos(x), so we just compute cos(|x|).
  (let ((ix (ldb (byte 31 0) (kernel:double-float-high-bits x))))
    ;; cos(x) = 1 when |x| < 2^-27
    (when (< ix #x3e400000)
      ;; Signal inexact if x /= 0
      (if (zerop (truncate x))
	  (return-from kernel-cos 1d0)
	  (return-from kernel-cos 1d0)))
    (let* ((c1  4.16666666666666019037d-02)
	   (c2 -1.38888888888741095749d-03)
	   (c3  2.48015872894767294178d-05)
	   (c4 -2.75573143513906633035d-07)
	   (c5  2.08757232129817482790d-09)
	   (c6 -1.13596475577881948265d-11)
	   (z (* x x))
	   (r (* z
		 (+ c1
		    (* z
		       (+ c2
			  (* z
			     (+ c3
				(* z
				   (+ c4
				      (* z
					 (+ c5
					    (* z c6)))))))))))))
      (cond ((< ix #x3fd33333)
	     ;; \x| < 0.3
	     (- 1 (- (* .5 z)
		     (- (* z r)
			(* x y)))))
	    (t
	     ;; qx = 0.28125 if |x| > 0.78125, else x/4 dropping the
	     ;; least significant 32 bits.
	     (let* ((qx (if (> ix #x3fe90000)
			    0.28125d0
			    ;; x/4, exactly, and also dropping the
			    ;; least significant 32 bits of the
			    ;; fraction.
			    (make-double-float (- ix #x00200000)
					       0)))
		    (hz (- (* 0.5 z) qx))
		    (a (- 1 qx)))
	       (- a (- hz (- (* z r)
			     (* x y))))))))))

(declaim (type (simple-array double-float (*)) tan-coef))
(defconstant tan-coef
  (make-array 13 :element-type 'double-float
	      :initial-contents
	      '(3.33333333333334091986d-01
		1.33333333333201242699d-01
		5.39682539762260521377d-02
		2.18694882948595424599d-02
		8.86323982359930005737d-03
		3.59207910759131235356d-03
		1.45620945432529025516d-03
		5.88041240820264096874d-04
		2.46463134818469906812d-04
		7.81794442939557092300d-05
		7.14072491382608190305d-05
		-1.85586374855275456654d-05
		2.59073051863633712884d-05)))

;; kernel tan function on [-pi/4, pi/4], pi/4 ~ 0.7854
;; Input x is assumed to be bounded by ~pi/4 in magnitude.
;; Input y is the tail of x.
;; Input k indicates whether tan (if k = 1) or -1/tan (if k = -1) is returned.
;;
;; Algorithm
;;	1. Since tan(-x) = -tan(x), we need only to consider positive x.
;;	2. if x < 2^-28 (hx<0x3e300000 0), return x with inexact if x!=0.
;;	3. tan(x) is approximated by a odd polynomial of degree 27 on
;;	   [0,0.67434]
;;		  	         3             27
;;	   	tan(x) ~ x + T1*x + ... + T13*x
;;	   where
;;
;; 	        |tan(x)         2     4            26   |     -59.2
;; 	        |----- - (1+T1*x +T2*x +.... +T13*x    )| <= 2
;; 	        |  x 					|
;;
;;	   Note: tan(x+y) = tan(x) + tan'(x)*y
;;		          ~ tan(x) + (1+x*x)*y
;;	   Therefore, for better accuracy in computing tan(x+y), let
;;		     3      2      2       2       2
;;		r = x *(T2+x *(T3+x *(...+x *(T12+x *T13))))
;;	   then
;;		 		    3    2
;;		tan(x+y) = x + (T1*x + (x *(r+y)+y))
;;
;;      4. For x in [0.67434,pi/4],  let y = pi/4 - x, then
;;		tan(x) = tan(pi/4-y) = (1-tan(y))/(1+tan(y))
;;		       = 1 - 2*(tan(y) - (tan(y)^2)/(1+tan(y)))
(declaim (ftype (function (double-float double-float fixnum)
			  double-float)
		kernel-tan))

(defun kernel-tan (x y iy)
  (declare (type (double-float -1d0 1d0) x y)
	   (type (member -1 1) iy)
	   (optimize (speed 3) (safety 0)))
  (let* ((hx (kernel:double-float-high-bits x))
	 (ix (logand hx #x7fffffff))
	 (w 0d0)
	 (z 0d0)
	 (v 0d0)
	 (s 0d0)
	 (r 0d0))
    (declare (double-float w z v s r))
    (when (< ix #x3e300000)
      ;; |x| < 2^-28
      (when (zerop (truncate x))
	(cond ((zerop (logior (logior ix (kernel:double-float-low-bits x))
			      (+ iy 1)))
	       ;; x = 0 (because hi and low bits are 0) and iy = -1
	       ;; (cot)
	       (return-from kernel-tan (/ (abs x))))
	      ((= iy 1)
	       (return-from kernel-tan x))
	      (t
	       ;; x /= 0 and iy = -1 (cot)
	       ;; Compute -1/(x+y) carefully
	       (let ((a 0d0)
		     (tt 0d0))
		 (setf w (+ x y))
		 (setf z (make-double-float (double-float-high-bits w) 0))
		 (setf v (- y (- z x)))
		 (setf a (/ -1 w))
		 (setf tt (make-double-float (double-float-high-bits a) 0))
		 (setf s (+ 1 (* tt z)))
		 (return-from kernel-tan (+ tt
					    (* a (+ s (* tt v))))))))))
    (when (>= ix #x3FE59428)
      ;; |x| > .6744
      (when (minusp hx)
	(setf x (- x))
	(setf y (- y)))
      ;; The two constants below are such that pi/4 + pi/4_lo is pi/4
      ;; to twice the accuracy of a double float.
      ;;
      ;; z = pi/4-x
      (setf z (- (make-double-float #x3FE921FB #x54442D18) x))
      ;; w = pi/4_lo - y.
      (setf w (- (make-double-float #x3C81A626 #x33145C07) y))
      (setf x (+ z w))
      (setf y 0d0))
    (setf z (* x x))
    (setf w (* z z))
    ;; Break x^5*(T[1]+x^2*T[2]+...) into
    ;; x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
    ;; x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
    (setf r (+ (aref tan-coef 1)
	       (* w
		  (+ (aref tan-coef 3)
		     (* w
			(+ (aref tan-coef 5)
			   (* w
			      (+ (aref tan-coef 7)
				 (* w
				    (+ (aref tan-coef 9)
				       (* w (aref tan-coef 11))))))))))))
    (setf v (* z
	       (+ (aref tan-coef 2)
		  (* w
		     (+ (aref tan-coef 4)
			(* w
			   (+ (aref tan-coef 6)
			      (* w
				 (+ (aref tan-coef 8)
				    (* w
				       (+ (aref tan-coef 10)
					  (* w (aref tan-coef 12)))))))))))))
    (setf s (* z x))
    (setf r (+ y (* z (+ (* s (+ r v))
			 y))))
    (incf r (* s (aref tan-coef 0)))
    (setf w (+ x r))
    (when (>= ix #x3FE59428)
      (let ((v (float iy 1d0)))
	(return-from kernel-tan
	  (* (- 1 (logand 2 (ash hx -30)))
	     (- v
		(* 2
		   (- x (- (/ (* w w)
			      (+ w v))
			   r))))))))
    (when (= iy 1)
      (return-from kernel-tan w))
    ;; Compute 1/w=1/(x+r) carefully
    (let ((a 0d0)
	  (tt 0d0))
      (setf z (kernel:make-double-float (kernel:double-float-high-bits w) 0))
      (setf v (- r (- z x)))		; z + v = r + x
      (setf a (/ -1 w))
      (setf tt (kernel:make-double-float (kernel:double-float-high-bits a) 0))
      (setf s (+ 1 (* tt z)))
      (+ tt
	 (* a
	    (+ s (* tt v)))))))

;; Return sine function of x.
;;
;; kernel function:
;;	__kernel_sin		... sine function on [-pi/4,pi/4]
;;	__kernel_cos		... cose function on [-pi/4,pi/4]
;;	__ieee754_rem_pio2	... argument reduction routine
;;
;; Method.
;;      Let S,C and T denote the sin, cos and tan respectively on 
;;	[-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2 
;;	in [-pi/4 , +pi/4], and let n = k mod 4.
;;	We have
;;
;;          n        sin(x)      cos(x)        tan(x)
;;     ----------------------------------------------------------
;;	    0	       S	   C		 T
;;	    1	       C	  -S		-1/T
;;	    2	      -S	  -C		 T
;;	    3	      -C	   S		-1/T
;;     ----------------------------------------------------------
;;
;; Special cases:
;;      Let trig be any of sin, cos, or tan.
;;      trig(+-INF)  is NaN, with signals;
;;      trig(NaN)    is that NaN;
;;
;; Accuracy:
;;	TRIG(x) returns trig(x) nearly rounded 
(defun %sin (x)
  (declare (double-float x)
	   (optimize (speed 3)))
  (let ((ix (ldb (byte 31 0) (kernel:double-float-high-bits x))))
    (cond
      ((<= ix #x3fe921fb)
       ;; |x| < pi/4, approx
       (kernel-sin x 0d0 0))
      ((>= ix #x7ff00000)
       ;; sin(Inf or NaN) is NaN
       (- x x))
      (t
       ;; Argument reduction needed
       (multiple-value-bind (n y0 y1)
	   (%ieee754-rem-pi/2 x)
	 (case (logand n 3)
	   (0
	    (kernel-sin y0 y1 1))
	   (1
	    (kernel-cos y0 y1))
	   (2
	    (- (kernel-sin y0 y1 1)))
	   (3
	    (- (kernel-cos y0 y1)))))))))

(defun %cos (x)
  (declare (double-float x)
	   (optimize (speed 3)))
  (let ((ix (ldb (byte 31 0) (kernel:double-float-high-bits x))))
    (cond
      ((< ix #x3fe921fb)
       ;;|x| < pi/4, approx
       (kernel-cos x 0d0))
      ((>= ix #x7ff00000)
       ;; cos(Inf or NaN) is NaN
       (- x x))
      (t
       ;; Argument reduction needed
       (multiple-value-bind (n y0 y1)
	   (%ieee754-rem-pi/2 x)
	 (ecase (logand n 3)
	   (0
	    (kernel-cos y0 y1))
	   (1
	    (- (kernel-sin y0 y1 1)))
	   (2
	    (- (kernel-cos y0 y1)))
	   (3
	    (kernel-sin y0 y1 1))))))))

(defun %tan (x)
  (declare (double-float x)
	   (optimize (speed 3)))
  (let ((ix (logand #x7fffffff (kernel:double-float-high-bits x))))
    (cond ((<= ix #x3fe921fb)
	   ;; |x| < pi/4
	   (kernel-tan x 0d0 1))
	  ((>= ix #x7ff00000)
	   ;; tan(Inf or Nan) is NaN
	   (- x x))
	  (t
	   (multiple-value-bind (n y0 y1)
	       (%ieee754-rem-pi/2 x)
	     (let ((flag (- 1 (ash (logand n 1) 1))))
	       ;; flag = 1 if n even, -1 if n odd
	       (kernel-tan y0 y1 flag)))))))
;; Compute sin and cos of x, simultaneously.
(defun %sincos (x)
  (declare (double-float x)
	   (optimize (speed 3)))
  (cond ((<= (abs x) (/ pi 4))
	 (values (kernel-sin x 0d0 0)
		 (kernel-cos x 0d0)))
	(t
	 ;; Argument reduction needed
	 (multiple-value-bind (n y0 y1)
	     (%ieee754-rem-pi/2 x)
	   (case (logand n 3)
	     (0
	      (values (kernel-sin y0 y1 1)
		      (kernel-cos y0 y1)))
	     (1
	      (values (kernel-cos y0 y1)
		      (- (kernel-sin y0 y1 1))))
	     (2
	      (values (- (kernel-sin y0 y1 1))
		      (- (kernel-cos y0 y1))))
	     (3
	      (values (- (kernel-cos y0 y1))
		      (kernel-sin y0 y1 1))))))))
;;(declaim (ext:end-block))
||#


;;;; Power functions.

(defun exp (number)
  "Return e raised to the power NUMBER."
  (number-dispatch ((number number))
    (handle-reals %exp number)
    ((complex)
     (* (exp (realpart number))
	(cis (imagpart number))))))

;;; INTEXP -- Handle the rational base, integer power case.

(defparameter *intexp-maximum-exponent* 10000)

(define-condition intexp-limit-error (error)
  ((base :initarg :base :reader intexp-base)
   (power :initarg :power :reader intexp-power))
  (:report (lambda (condition stream)
	     (format stream (intl:gettext "The absolute value of ~S exceeds limit ~S.")
		     (intexp-power condition)
		     *intexp-maximum-exponent*))))

;;; This function precisely calculates base raised to an integral power.  It
;;; separates the cases by the sign of power, for efficiency reasons, as powers
;;; can be calculated more efficiently if power is a positive integer.  Values
;;; of power are calculated as positive integers, and inverted if negative.
;;;
(defun intexp (base power)
  ;; Handle the special case of 1^power and (-1)^power.  Maxima
  ;; sometimes does this, and there's no need to cause a continuable
  ;; error in this case.
  (when (eql base 1)
    (return-from intexp base))
  (when (eql base -1)
    (return-from intexp (if (oddp power) -1 1)))
  
  (when (> (abs power) *intexp-maximum-exponent*)
    ;; Allow user the option to continue with calculation, possibly
    ;; increasing the limit to the given power.
    (restart-case
	(error 'intexp-limit-error
	       :base base
	       :power power)
      (continue ()
	:report (lambda (stream)
		  (write-string (intl:gettext "Continue with calculation") stream)))
      (new-limit ()
	:report (lambda (stream)
		  (write-string (intl:gettext "Continue with calculation, update limit") stream))
	(setq *intexp-maximum-exponent* (abs power)))))
  (cond ((minusp power)
	 (/ (intexp base (- power))))
	((eql base 2)
	 (ash 1 power))
	(t
	 (do ((nextn (ash power -1) (ash power -1))
	      (total (if (oddp power) base 1)
		     (if (oddp power) (* base total) total)))
	     ((zerop nextn) total)
	   (setq base (* base base))
	   (setq power nextn)))))


;;; EXPT  --  Public
;;;
;;;    If an integer power of a rational, use INTEXP above.  Otherwise, do
;;; floating point stuff.  If both args are real, we try %POW right off,
;;; assuming it will return 0 if the result may be complex.  If so, we call
;;; COMPLEX-POW which directly computes the complex result.  We also separate
;;; the complex-real and real-complex cases from the general complex case.
;;;
(defun expt (base power)
  "Returns BASE raised to the POWER."
  (if (zerop power)
      ;; CLHS says that if the power is 0, the result is 1, subject to
      ;; numeric contagion.  But what happens if base is infinity or
      ;; NaN?  Do we silently return 1?  For now, I think we should
      ;; signal an error if the FP modes say so.
      (let ((result (1+ (* base power))))
	;; If we get an NaN here, that means base*power above didn't
	;; produce 0 and FP traps were disabled, so we handle that
	;; here.  Should this be a continuable restart?
	(if (and (floatp result) (float-nan-p result))
	    (float 1 result)
	    result))
    (labels (;; determine if the double float is an integer.
	     ;;  0 - not an integer
	     ;;  1 - an odd int
	     ;;  2 - an even int
	     (isint (ihi lo)
	       (declare (type (unsigned-byte 31) ihi)
			(type (unsigned-byte 32) lo)
			(optimize (speed 3) (safety 0)))
	       (let ((isint 0))
		 (declare (type fixnum isint))
		 (cond ((>= ihi #x43400000)	; exponent >= 53
			(setq isint 2))
		       ((>= ihi #x3ff00000)
			(let ((k (- (ash ihi -20) #x3ff)))	; exponent
			  (declare (type (mod 53) k))
			  (cond ((> k 20)
				 (let* ((shift (- 52 k))
					(j (logand (ash lo (- shift))))
					(j2 (ash j shift)))
				   (declare (type (mod 32) shift)
					    (type (unsigned-byte 32) j j2))
				   (when (= j2 lo)
				     (setq isint (- 2 (logand j 1))))))
				((= lo 0)
				 (let* ((shift (- 20 k))
					(j (ash ihi (- shift)))
					(j2 (ash j shift)))
				   (declare (type (mod 32) shift)
					    (type (unsigned-byte 31) j j2))
				   (when (= j2 ihi)
				     (setq isint (- 2 (logand j 1))))))))))
		 isint))
	     (real-expt (x y rtype)
	       (let ((x (coerce x 'double-float))
		     (y (coerce y 'double-float)))
		 (declare (double-float x y))
		 (let* ((x-hi (kernel:double-float-high-bits x))
			(x-lo (kernel:double-float-low-bits x))
			(x-ihi (logand x-hi #x7fffffff))
			(y-hi (kernel:double-float-high-bits y))
			(y-lo (kernel:double-float-low-bits y))
			(y-ihi (logand y-hi #x7fffffff)))
		   (declare (type (signed-byte 32) x-hi y-hi)
			    (type (unsigned-byte 31) x-ihi y-ihi)
			    (type (unsigned-byte 32) x-lo y-lo))
		   ;; y==zero: x**0 = 1
		   (when (zerop (logior y-ihi y-lo))
		     (return-from real-expt (coerce 1d0 rtype)))
		   ;; +-NaN return x+y
		   (when (or (> x-ihi #x7ff00000)
			     (and (= x-ihi #x7ff00000) (/= x-lo 0))
			     (> y-ihi #x7ff00000)
			     (and (= y-ihi #x7ff00000) (/= y-lo 0)))
		     (return-from real-expt (coerce (+ x y) rtype)))
		   (let ((yisint (if (< x-hi 0) (isint y-ihi y-lo) 0)))
		     (declare (type fixnum yisint))
		     ;; special value of y
		     (when (and (zerop y-lo) (= y-ihi #x7ff00000))
		       ;; y is +-inf
		       (return-from real-expt
			 (cond ((and (= x-ihi #x3ff00000) (zerop x-lo))
				;; +-1**inf is NaN
				(coerce (- y y) rtype))
			       ((>= x-ihi #x3ff00000)
				;; (|x|>1)**+-inf = inf,0
				(if (>= y-hi 0)
				    (coerce y rtype)
				    (coerce 0 rtype)))
			       (t
				;; (|x|<1)**-,+inf = inf,0
				(if (< y-hi 0)
				    (coerce (- y) rtype)
				    (coerce 0 rtype))))))

		     (let ((abs-x (abs x)))
		       (declare (double-float abs-x))
		       ;; special value of x
		       (when (and (zerop x-lo)
				  (or (= x-ihi #x7ff00000) (zerop x-ihi)
				      (= x-ihi #x3ff00000)))
			 ;; x is +-0,+-inf,+-1
			 (let ((z (if (< y-hi 0)
				      (/ 1 abs-x)	; z = (1/|x|)
				      abs-x)))
			   (declare (double-float z))
			   (when (< x-hi 0)
			     (cond ((and (= x-ihi #x3ff00000) (zerop yisint))
				    ;; (-1)**non-int
				    (let ((y*pi (* y pi)))
				      (declare (double-float y*pi))
				      (return-from real-expt
				        (complex
					 (coerce (%cos y*pi) rtype)
					 (coerce (%sin y*pi) rtype)))))
				   ((= yisint 1)
				    ;; (x<0)**odd = -(|x|**odd)
				    (setq z (- z)))))
			   (return-from real-expt (coerce z rtype))))
		       
		       (if (>= x-hi 0)
			   ;; x>0
			   (coerce (kernel::%pow x y) rtype)
			   ;; x<0
			   (let ((pow (kernel::%pow abs-x y)))
			     (declare (double-float pow))
			     (case yisint
			       (1 ; Odd
				(coerce (* -1d0 pow) rtype))
			       (2 ; Even
				(coerce pow rtype))
			       (t ; Non-integer
				(let ((y*pi (* y pi)))
				  (declare (double-float y*pi))
				  (complex
				   (coerce (* pow (%cos y*pi)) rtype)
				   (coerce (* pow (%sin y*pi)) rtype))))))))))))
	     (expt-xfrm (b p)
	       ;; Apply the same transformation as in the deftransform
	       ;; for expt in compiler/srctran.lisp.  Only call this
	       ;; if B is more contagious than P.  Otherwise, the type
	       ;; of the result will be wrong which will confuse the
	       ;; compiler!  Return NIL if the transform can't be
	       ;; applied.
	       (cond
		 ((= p 2) (* b b))
		 ((= p -2) (/ (* b b)))
		 ((= p 3) (* b b b))
		 ((= p -3) (/ (* b b b)))
		 ((= p 1/2) (sqrt b))
		 ((= p -1/2) (/ (sqrt b)))
		 (t nil))))
      ;; This is really messy and should be cleaned up.  The easiest
      ;; way to see if we're doing what we should is the macroexpand
      ;; the number-dispatch and check each branch.
      ;;
      ;; We try to apply the rule of float precision contagion (CLHS
      ;; 12.1.4.4): the result has the same precision has the most
      ;; precise argument.
      (number-dispatch ((base number) (power number))
        (((foreach fixnum (or bignum ratio) (complex rational))
	  integer)
	 (intexp base power))
	(((foreach single-float double-float)
	  rational)
	 (or (expt-xfrm base power)
	     (real-expt base power '(dispatch-type base))))
	(((foreach fixnum (or bignum ratio) single-float)
	  (foreach ratio single-float))
	 (or (expt-xfrm (coerce base 'single-float) power)
	     (real-expt base power 'single-float)))
	(((foreach fixnum (or bignum ratio) single-float double-float)
	  double-float)
	 (or (expt-xfrm (coerce base 'double-float) power)
	     (real-expt base power 'double-float)))
	((double-float single-float)
	 (or (expt-xfrm (coerce base 'double-float) power)
	     (real-expt base power 'double-float)))
	#+double-double
	(((foreach fixnum (or bignum ratio) single-float double-float
		   double-double-float)
	  double-double-float)
	 (or (expt-xfrm (coerce base 'double-double-float) power)
	     (dd-%pow (coerce base 'double-double-float) power)))
	#+double-double
	((double-double-float
	  (foreach fixnum (or bignum ratio) single-float double-float))
	 (or (expt-xfrm base power)
	     (dd-%pow base (coerce power 'double-double-float))))
	(((foreach (complex rational) (complex single-float) (complex double-float)
		   #+double-double (complex double-double-float))
	  rational)
	 (or (expt-xfrm base power)
	     (* (expt (abs base) power)
		(cis (* power (phase base))))))
	#+double-double
	((double-double-float
	  complex)
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (* (log2 base 1w0) (log 2w0))))))
	(((foreach fixnum (or bignum ratio) single-float double-float)
	  (foreach (complex double-float)))
	 ;; Result should have double-float accuracy.  Use log2 in
	 ;; case the base won't fit in a double-float.
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (* (log2 base) (log 2d0))))))
	((double-float
	  (foreach (complex rational) (complex single-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (log base)))))
	#+double-double
	(((foreach fixnum (or bignum ratio) single-float double-float)
	  (foreach (complex double-double-float)))
	 ;; Result should have double-double-float accuracy.  Use log2
	 ;; in case the base won't fit in a double-float.
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (* (log2 base 1w0) (log 2w0))))))
	(((foreach fixnum (or bignum ratio) single-float)
	  (foreach (complex single-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (log base)))))
	(((foreach (complex rational) (complex single-float))
	  (foreach single-float (complex single-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (or (expt-xfrm (coerce base '(complex single-float)) power)
		 (exp (* power (log base))))))
	(((foreach (complex rational) (complex single-float))
	  (foreach double-float (complex double-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (or (expt-xfrm (coerce base '(complex double-float))
			    power)
		 (exp (* power (log (coerce base '(complex double-float))))))))
	#+double-double
	(((foreach (complex rational) (complex single-float))
	  (foreach double-double-float (complex double-double-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (or (expt-xfrm (coerce base '(complex double-double-float))
			    power)
		 (exp (* power (log (coerce base '(complex double-double-float))))))))
	(((foreach (complex double-float))
	  (foreach single-float double-float
		   (complex single-float) (complex double-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (or (expt-xfrm base power)
		 (exp (* power (log base))))))
	#+double-double
	(((foreach (complex double-float))
	  (foreach double-double-float (complex double-double-float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (or (expt-xfrm (coerce base '(complex double-double-float))
			    power)
		 (exp (* power (log (coerce base '(complex double-double-float))))))))
	#+double-double
	(((foreach (complex double-double-float))
	  (foreach float (complex float)))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (or (expt-xfrm base power)
		 (exp (* power (log base))))))))))

;; Log base 2 of a real number.  The result is a either a double-float
;; or double-double-float number (real or complex, as appropriate),
;; depending on the type of FLOAT-TYPE.
(defun log2 (x &optional (float-type 1d0))
  (labels ((log-of-2 (f)
	     ;; log(2), with the precision specified by the type of F
	     (number-dispatch ((f real))
	       ((double-float)
		#.(log 2d0))
	       #+double-double
	       ((double-double-float)
		#.(log 2w0))))
	   (log-2-pi (f)
	     ;; log(pi), with the precision specified by the type of F
	     (number-dispatch ((f real))
	       ((double-float)
		#.(/ pi (log 2d0)))
	       #+double-double
	       ((double-double-float)
		#.(/ dd-pi (log 2w0)))))
	   (log1p (x)
	     ;; log(1+x), with the precision specified by the type of
	     ;; X
	     (number-dispatch ((x real))
	       (((foreach single-float double-float))
		(%log1p (float x 1d0)))
	       #+double-double
	       ((double-double-float)
		(dd-%log1p x))))
	   (log2-bignum (bignum)
	     ;; Write x = 2^n*f where 1/2 < f <= 1.  Then log2(x) = n
	     ;; + log2(f).
	     ;;
	     ;; So we grab the top few bits of x and scale that
	     ;; appropriately, take the log of it and add it to n.
	     ;;
	     ;; Return n and log2(f) separately.
	     (if (minusp bignum)
		 (multiple-value-bind (n frac)
		     (log2-bignum (abs bignum))
		   (values n (complex frac (log-2-pi float-type))))
		 (let ((n (integer-length bignum))
		       (float-bits (float-digits float-type)))
		   (if (< n float-bits)
		       (values 0 (log (float bignum float-type)
				      (float 2 float-type)))
		       (let ((exp (min float-bits n))
			     (f (ldb (byte float-bits
					   (max 0 (- n float-bits)))
				     bignum)))
			 (values n (log (scale-float (float f float-type) (- exp))
					(float 2 float-type)))))))))
    (etypecase x
      (float
       (/ (log (float x float-type)) (log-of-2 float-type)))
      (ratio
       (let ((top (numerator x))
	     (bot (denominator x)))
	 ;; If the number of bits in the numerator and
	 ;; denominator are different, just use the fact
	 ;; log(x/y) = log(x) - log(y).  But to preserve
	 ;; accuracy, we actually do
	 ;; (log2(x)-log2(y))/log2(e)).
	 ;;
	 ;; However, if the numerator and denominator have the
	 ;; same number of bits, implying the quotient is near
	 ;; one, we use log1p(x) = log(1+x). Since the number is
	 ;; rational, we don't lose precision subtracting 1 from
	 ;; it, and converting it to double-float is accurate.
	 (if (= (integer-length top)
		(integer-length bot))
	     (/ (log1p (float (- x 1) float-type))
		(log-of-2 float-type))
	     (multiple-value-bind (top-n top-frac)
		 (log2-bignum top)
	       (multiple-value-bind (bot-n bot-frac)
		   (log2-bignum bot)
		 (+ (- top-n bot-n)
		    (- top-frac bot-frac)))))))
      (integer
       (multiple-value-bind (n frac)
	   (log2-bignum x)
	 (+ n frac))))))

(defun log (number &optional (base nil base-p))
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (if base-p
      (cond ((zerop base)
	     ;; ANSI spec
	     base)
	    ((and (realp number) (realp base))
	     ;; CLHS 12.1.4.1 says
	     ;;
	     ;;   When rationals and floats are combined by a
	     ;;   numerical function, the rational is first converted
	     ;;   to a float of the same format.
	     ;;
	     ;; So assume this applies to floats as well convert all
	     ;; numbers to the largest float format before computing
	     ;; the log.
	     ;;
	     ;; This makes (log 17 10.0) = (log 17.0 10) and so on.
	     (number-dispatch ((number real) (base real))
	       ((double-float
		 (foreach double-float single-float))
		(/ (log2 number) (log2 base)))
	       (((foreach fixnum bignum ratio)
		 (foreach fixnum bignum ratio single-float))
		(let* ((result (/ (log2 number) (log2 base))))
		  ;; Figure out the right result type
		  (if (realp result)
		      (coerce result 'single-float)
		      (coerce result '(complex single-float)))))
	       (((foreach fixnum bignum ratio)
		 double-float)
		(/ (log2 number) (log2 base)))
	       ((single-float
		 (foreach fixnum bignum ratio))
		(let* ((result (/ (log2 number) (log2 base))))
		  ;; Figure out the right result type
		  (if (realp result)
		      (coerce result 'single-float)
		      (coerce result '(complex single-float)))))
	       ((double-float
		 (foreach fixnum bignum ratio))
		(/ (log2 number) (log2 base)))
	       ((single-float double-float)
		(/ (log (coerce number 'double-float)) (log base)))
	       #+double-double
	       ((double-double-float
		 (foreach fixnum bignum ratio))
		(/ (log2 number 1w0) (log2 base 1w0)))
	       #+double-double
	       ((double-double-float
		 (foreach double-double-float double-float single-float))
		(/ (log number) (log (coerce base 'double-double-float))))
	       #+double-double
	       (((foreach fixnum bignum ratio)
		 double-double-float)
		(/ (log2 number 1w0) (log2 base 1w0)))
	       #+double-double
	       (((foreach double-float single-float)
		 double-double-float)
		(/ (log (coerce number 'double-double-float)) (log base)))
	       (((foreach single-float)
		 (foreach single-float))
		;; Converting everything to double-float helps the
		;; cases like (log 17 10) = (/ (log 17) (log 10)).
		;; This is usually handled above, but if we compute (/
		;; (log 17) (log 10)), we get a slightly different
		;; answer due to roundoff.  This makes it a bit more
		;; consistent.
		;;
		;; FIXME: This probably needs more work.
		(let ((result (/ (log (float number 1d0))
				 (log (float base 1d0)))))
		  (if (realp result)
		      (coerce result 'single-float)
		      (coerce result '(complex single-float)))))))
	    (t
	     ;; FIXME:  This probably needs some work as well.
	     (/ (log number) (log base))))
      (number-dispatch ((number number))
	(((foreach fixnum bignum))
	 (if (minusp number)
	     (complex (coerce (log (- number)) 'single-float)
		      (coerce pi 'single-float))
	     (coerce (/ (log2 number) #.(log (exp 1d0) 2d0)) 'single-float)))
	((ratio)
	 (if (minusp number)
	     (complex (coerce (log (- number)) 'single-float)
		      (coerce pi 'single-float))
	     ;; What happens when the ratio is close to 1?  We need to
	     ;; be careful to preserve accuracy.
	     (let ((top (numerator number))
		   (bot (denominator number)))
	       ;; If the number of bits in the numerator and
	       ;; denominator are different, just use the fact
	       ;; log(x/y) = log(x) - log(y).  But to preserve
	       ;; accuracy, we actually do
	       ;; (log2(x)-log2(y))/log2(e)).
	       ;;
	       ;; However, if the numerator and denominator have the
	       ;; same number of bits, implying the quotient is near
	       ;; one, we use log1p(x) = log(1+x). Since the number is
	       ;; rational, we don't lose precision subtracting 1 from
	       ;; it, and converting it to double-float is accurate.
	       (if (= (integer-length top)
		      (integer-length bot))
		   (coerce (%log1p (coerce (- number 1) 'double-float))
			   'single-float)
		   (coerce (/ (- (log2 top) (log2 bot))
			      #.(log (exp 1d0) 2d0))
			   'single-float)))))
	(((foreach single-float double-float))
	 ;; Is (log -0) -infinity (libm.a) or -infinity + i*pi (Kahan)?
	 ;; Since this doesn't seem to be an implementation issue
	 ;; I (pw) take the Kahan result.
	 (if (< (float-sign number)
		(coerce 0 '(dispatch-type number)))
	     (complex (log (- number)) (coerce pi '(dispatch-type number)))
	     (coerce (%log (coerce number 'double-float))
		     '(dispatch-type number))))
	#+double-double
	((double-double-float)
	 (let ((hi (kernel:double-double-hi number)))
	   (if (< (float-sign hi) 0d0)
	       (complex (dd-%log (- number)) dd-pi)
	       (dd-%log number))))
	((complex)
	 (complex-log number)))))

(defun sqrt (number)
  "Return the square root of NUMBER."
  (number-dispatch ((number number))
    (((foreach fixnum bignum ratio))
     (if (minusp number)
	 (complex-sqrt number)
	 (coerce (%sqrt (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (minusp number)
	 (complex-sqrt number)
	 (coerce (%sqrt (coerce number 'double-float))
		 '(dispatch-type number))))
    #+double-double
    ((double-double-float)
     (if (minusp number)
	 (dd-complex-sqrt number)
	 (multiple-value-bind (hi lo)
	     (c::sqrt-dd (kernel:double-double-hi number) (kernel:double-double-lo number))
	   (kernel:%make-double-double-float hi lo))))
    ((complex)
     (complex-sqrt number))))


;;;; Trigonometic and Related Functions

(defun abs (number)
  "Returns the absolute value of the number."
  (number-dispatch ((number number))
    (((foreach single-float double-float fixnum rational
	       #+double-double double-double-float))
     (abs number))
    ((complex)
     (let ((rx (realpart number))
	   (ix (imagpart number)))
       (etypecase rx
	 (rational
	  (sqrt (+ (* rx rx) (* ix ix))))
	 (single-float
	  (coerce (%hypot (coerce rx 'double-float)
			  (coerce ix 'double-float))
		  'single-float))
	 (double-float
	  (%hypot rx ix))
	 #+double-double
	 (double-double-float
	  (multiple-value-bind (abs^2 scale)
	      (dd-cssqs number)
	    (scale-float (sqrt abs^2) scale))))))))

(defun phase (number)
  "Returns the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0.  For non-complex negative
  numbers this is PI."
  (etypecase number
    (rational
     (if (minusp number)
	 (coerce pi 'single-float)
	 0.0f0))
    (single-float
     (if (minusp (float-sign number))
	 (coerce pi 'single-float)
	 0.0f0))
    (double-float
     (if (minusp (float-sign number))
	 (coerce pi 'double-float)
	 0.0d0))
    #+double-double
    (double-double-float
     (if (minusp (float-sign number))
	 dd-pi
	 0w0))
    (complex
     (atan (imagpart number) (realpart number)))))


(defun sin (number)  
  "Return the sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sin number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sin x) (cosh y))
		(* (cos x) (sinh y)))))))

(defun cos (number)
  "Return the cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cos number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cos x) (cosh y))
		(- (* (sin x) (sinh y))))))))

(defun tan (number)
  "Return the tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tan number)
    ((complex)
     (complex-tan number))))

(defun cis (theta)
  "Return cos(Theta) + i sin(Theta), AKA exp(i Theta)."
  (if (complexp theta)
      (error (intl:gettext "Argument to CIS is complex: ~S") theta)
      (number-dispatch ((theta real))
	((rational)
	 (let ((arg (coerce theta 'double-float)))
	   (multiple-value-bind (s c)
	       (%sincos arg)
	     (complex (coerce c 'single-float)
		      (coerce s 'single-float)))))
	(((foreach single-float double-float))
	 (multiple-value-bind (s c)
	     (%sincos (coerce theta 'double-float))
	   (complex (coerce c '(dispatch-type theta))
		    (coerce s '(dispatch-type theta)))))
	#+double-double
	((double-double-float)
	 (multiple-value-bind (s c)
	     (dd-%sincos theta)
	   (complex c s))))))

(defun asin (number)
  "Return the arc sine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-asin number)
	 (coerce (%asin (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (float-nan-p number)
	     (and (<= number (coerce 1 '(dispatch-type number)))
		  (>= number (coerce -1 '(dispatch-type number)))))
	 (coerce (%asin (coerce number 'double-float))
		 '(dispatch-type number))
	 (complex-asin number)))
    #+double-double
    ((double-double-float)
     (if (or (float-nan-p number)
	     (and (<= number 1w0)
		  (>= number -1w0)))
	 (dd-%asin number)
	 (dd-complex-asin number)))
    ((complex)
     (complex-asin number))))

(defun acos (number)
  "Return the arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-acos number)
	 (coerce (%acos (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (float-nan-p number)
	     (and (<= number (coerce 1 '(dispatch-type number)))
		  (>= number (coerce -1 '(dispatch-type number)))))
	 (coerce (%acos (coerce number 'double-float))
		 '(dispatch-type number))
	 (complex-acos number)))
    #+double-double
    ((double-double-float)
     (if (or (float-nan-p number)
	     (and (<= number 1w0)
		  (>= number -1w0)))
	 (dd-%acos number)
	 (complex-acos number)))
    ((complex)
     (complex-acos number))))


(defun atan (y &optional (x nil xp))
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (if xp
      (flet ((atan2 (y x)
	       (declare (type double-float y x)
			(values double-float))
	       (if (zerop x)
		   (if (zerop y)
		       (if (plusp (float-sign x))
			   y
			   (float-sign y pi))
		       (float-sign y (/ pi 2)))
		   (%atan2 y x))))
	;; If X is given, both X and Y must be real numbers.
	(number-dispatch ((y real) (x real))
	  ((double-float
	    (foreach double-float single-float fixnum bignum ratio))
	   (atan2 y (coerce x 'double-float)))
	  (((foreach single-float fixnum bignum ratio)
	    double-float)
	   (atan2 (coerce y 'double-float) x))
	  (((foreach single-float fixnum bignum ratio)
	    (foreach single-float fixnum bignum ratio))
	   (coerce (atan2 (coerce y 'double-float) (coerce x 'double-float))
		   'single-float))
	  #+double-double
	  ((double-double-float
	    (foreach double-double-float double-float single-float fixnum bignum ratio))
	   (dd-%atan2 y (coerce x 'double-double-float)))
	  #+double-double
	  (((foreach double-float single-float fixnum bignum ratio)
	    double-double-float)
	   (dd-%atan2 (coerce y 'double-double-float) x))))
      (number-dispatch ((y number))
	(handle-reals %atan y)
	((complex)
	 (complex-atan y)))))

(defun sinh (number)
  "Return the hyperbolic sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sinh number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sinh x) (cos y))
		(* (cosh x) (sin y)))))))

(defun cosh (number)
  "Return the hyperbolic cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cosh number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cosh x) (cos y))
		(* (sinh x) (sin y)))))))

(defun tanh (number)
  "Return the hyperbolic tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tanh number)
    ((complex)
     (complex-tanh number))))

(defun asinh (number)
  "Return the hyperbolic arc sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %asinh number)
    ((complex)
     (complex-asinh number))))

(defun acosh (number)
  "Return the hyperbolic arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     ;; acosh is complex if number < 1
     (if (< number 1)
	 (complex-acosh number)
	 (coerce (%acosh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (< number (coerce 1 '(dispatch-type number)))
	 (complex-acosh number)
	 (coerce (%acosh (coerce number 'double-float))
		 '(dispatch-type number))))
    #+double-double
    ((double-double-float)
     (if (< number 1w0)
	 (complex-acosh number)
	 (dd-%acosh number)))
    ((complex)
     (complex-acosh number))))

(defun atanh (number)
  "Return the hyperbolic arc tangent of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     ;; atanh is complex if |number| > 1
     (if (or (> number 1) (< number -1))
	 (complex-atanh number)
	 (coerce (%atanh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-atanh number)
	 (coerce (%atanh (coerce number 'double-float))
		 '(dispatch-type number))))
    #+double-double
    ((double-double-float)
     (if (or (> number 1w0)
	     (< number -1w0))
	 (complex-atanh number)
	 (dd-%atanh (coerce number 'double-double-float))))
    ((complex)
     (complex-atanh number))))

;;; HP-UX does not supply a C version of log1p, so use the definition.
;;; We really need to fix this.  The definition really loses big-time
;;; in roundoff as x gets small.

#+hpux
(declaim (inline %log1p))
#+hpux
(defun %log1p (number)
  (declare (double-float number)
	   (optimize (speed 3) (safety 0)))
  (the double-float (log (the (double-float 0d0) (+ number 1d0)))))


;;;;
;;;; This is a set of routines that implement many elementary
;;;; transcendental functions as specified by ANSI Common Lisp.  The
;;;; implementation is based on Kahan's paper.
;;;;
;;;; I believe I have accurately implemented the routines and are
;;;; correct, but you may want to check for your self.
;;;;
;;;; These functions are written for CMU Lisp and take advantage of
;;;; some of the features available there.  It may be possible,
;;;; however, to port this to other Lisps.
;;;;
;;;; Some functions are significantly more accurate than the original
;;;; definitions in CMU Lisp.  In fact, some functions in CMU Lisp
;;;; give the wrong answer like (acos #c(-2.0 0.0)), where the true
;;;; answer is pi + i*log(2-sqrt(3)).
;;;;
;;;; All of the implemented functions will take any number for an
;;;; input, but the result will always be a either a complex
;;;; single-float or a complex double-float.
;;;;
;;;; General functions
;;;;   complex-sqrt
;;;;   complex-log
;;;;   complex-atanh
;;;;   complex-tanh
;;;;   complex-acos
;;;;   complex-acosh
;;;;   complex-asin
;;;;   complex-asinh
;;;;   complex-atan
;;;;   complex-tan
;;;;
;;;; Utility functions:
;;;;   scalb logb
;;;;
;;;; Internal functions:
;;;;    square coerce-to-complex-type cssqs complex-log-scaled
;;;;
;;;;
;;;; Please send any bug reports, comments, or improvements to Raymond
;;;; Toy at toy@rtp.ericsson.se.
;;;;
;;;; References
;;;;
;;;; Kahan, W. "Branch Cuts for Complex Elementary Functions, or Much
;;;; Ado About Nothing's Sign Bit" in Iserles and Powell (eds.) "The
;;;; State of the Art in Numerical Analysis", pp. 165-211, Clarendon
;;;; Press, 1987
;;;;

(declaim (inline square))
(defun square (x)
  (declare (float x))
  (* x x))

;; If you have these functions in libm, perhaps they should be used
;; instead of these Lisp versions.  These versions are probably good
;; enough, especially since they are portable.

(declaim (inline scalb))
(defun scalb (x n)
  "Compute 2^N * X without compute 2^N first (use properties of the
underlying floating-point format"
  (declare (type float x)
	   (type double-float-exponent n))
  (scale-float x n))

(declaim (inline logb-finite))
(defun logb-finite (x)
  "Same as logb but X is not infinity and non-zero and not a NaN, so
that we can always return an integer"
  (declare (type float x))
  (multiple-value-bind (signif expon sign)
      (decode-float x)
    (declare (ignore signif sign))
    ;; decode-float is almost right, except that the exponent
    ;; is off by one
    (1- expon)))
      
(defun logb (x)
  "Compute an integer N such that 1 <= |2^(-N) * x| < 2.
For the special cases, the following values are used:

    x             logb
   NaN            NaN
   +/- infinity   +infinity
   0              -infinity
"
  (declare (type float x))
  (cond ((float-nan-p x)
	 x)
	((float-infinity-p x)
	 #.ext:double-float-positive-infinity)
	((zerop x)
	 ;; The answer is negative infinity, but we are supposed to
	 ;; signal divide-by-zero, so do the actual division
	 (/ -1 x)
	 )
	(t
	 (logb-finite x))))

  

;; This function is used to create a complex number of the appropriate
;; type.

(declaim (inline coerce-to-complex-type))
(defun coerce-to-complex-type (x y z)
  "Create complex number with real part X and imaginary part Y such that
it has the same type as Z.  If Z has type (complex rational), the X
and Y are coerced to single-float."
  (declare (double-float x y)
	   (number z)
	   (optimize (extensions:inhibit-warnings 3)))
  (if (typep (realpart z) 'double-float)
      (complex x y)
      ;; Convert anything that's not a double-float to a single-float.
      (complex (float x 1f0)
	       (float y 1f0))))

(defun cssqs (z)
  ;; Compute |(x+i*y)/2^k|^2 scaled to avoid over/underflow. The
  ;; result is r + i*k, where k is an integer.
  
  ;; Save all FP flags
  (let ((x (float (realpart z) 1d0))
	(y (float (imagpart z) 1d0)))
    ;; Would this be better handled using an exception handler to
    ;; catch the overflow or underflow signal?  For now, we turn all
    ;; traps off and look at the accrued exceptions to see if any
    ;; signal would have been raised.
    (with-float-traps-masked (:underflow :overflow)
      (let ((rho (+ (square x) (square y))))
	(declare (optimize (speed 3) (space 0)))
	(cond ((and (or (float-nan-p rho)
			(float-infinity-p rho))
		    (or (float-infinity-p (abs x))
			(float-infinity-p (abs y))))
	       (values ext:double-float-positive-infinity 0))
	      ((let ((threshold #.(/ least-positive-double-float
				     double-float-epsilon))
		     (traps (ldb vm::float-sticky-bits
				 (vm:floating-point-modes))))
		 ;; Overflow raised or (underflow raised and rho <
		 ;; lambda/eps)
		 (or (not (zerop (logand vm:float-overflow-trap-bit traps)))
		     (and (not (zerop (logand vm:float-underflow-trap-bit traps)))
			  (< rho threshold))))
	       ;; If we're here, neither x nor y are infinity and at
	       ;; least one is non-zero.. Thus logb returns a nice
	       ;; integer.
	       (let ((k (- (logb-finite (max (abs x) (abs y))))))
		 (values (+ (square (scalb x k))
			    (square (scalb y k)))
			 (- k))))
	      (t
	       (values rho 0)))))))

(defun complex-sqrt (z)
  "Principle square root of Z

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-sqrt (dd-complex-sqrt z)))
  (multiple-value-bind (rho k)
      (cssqs z)
    (declare (type (or (member 0d0) (double-float 0d0)) rho)
	     (type fixnum k))
    (let ((x (float (realpart z) 1.0d0))
	  (y (float (imagpart z) 1.0d0))
	  (eta 0d0)
	  (nu 0d0))
      (declare (double-float x y eta nu))

      (locally
	  ;; space 0 to get maybe-inline functions inlined.
	  (declare (optimize (speed 3) (space 0)))

	(if (not (locally (declare (optimize (inhibit-warnings 3)))
		   (float-nan-p x)))
	    (setf rho (+ (scalb (abs x) (- k)) (sqrt rho))))

	(cond ((oddp k)
	       (setf k (ash k -1)))
	      (t
	       (setf k (1- (ash k -1)))
	       (setf rho (+ rho rho))))

	(setf rho (scalb (sqrt rho) k))

	(setf eta rho)
	(setf nu y)

	(when (/= rho 0d0)
	  (when (not (float-infinity-p (abs nu)))
	    (setf nu (/ (/ nu rho) 2d0)))
	  (when (< x 0d0)
	    (setf eta (abs nu))
	    (setf nu (float-sign y rho))))
	(coerce-to-complex-type eta nu z)))))

(defun complex-log-scaled (z j)
  "Compute log(2^j*z).

This is for use with J /= 0 only when |z| is huge."
  (declare (number z)
	   (fixnum j))
  ;; The constants t0, t1, t2 should be evaluated to machine
  ;; precision.  In addition, Kahan says the accuracy of log1p
  ;; influences the choices of these constants but doesn't say how to
  ;; choose them.  We'll just assume his choices matches our
  ;; implementation of log1p.
  (let ((t0 #.(/ 1 (sqrt 2.0d0)))
	(t1 1.2d0)
	(t2 3d0)
	(ln2 #.(log 2d0))
	(x (float (realpart z) 1.0d0))
	(y (float (imagpart z) 1.0d0)))
    (multiple-value-bind (rho k)
	(cssqs z)
      (declare (optimize (speed 3)))
      (let ((beta (max (abs x) (abs y)))
	    (theta (min (abs x) (abs y))))
	(coerce-to-complex-type (if (and (zerop k)
					 (< t0 beta)
					 (or (<= beta t1)
					     (< rho t2)))
				    (/ (%log1p (+ (* (- beta 1.0d0)
						     (+ beta 1.0d0))
						  (* theta theta)))
				       2d0)
				    (+ (/ (log rho) 2d0)
				       (* (+ k j) ln2)))
				(atan y x)
				z)))))

(defun complex-log (z)
  "Log of Z = log |Z| + i * arg Z

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-log (dd-complex-log-scaled z 0)))
  (complex-log-scaled z 0))
	       
;; Let us note the following "strange" behavior.  atanh 1.0d0 is
;; +infinity, but the following code returns approx 176 + i*pi/4. The
;; reason for the imaginary part is caused by the fact that arg i*y is
;; never 0 since we have positive and negative zeroes.

(defun complex-atanh (z)
  "Compute atanh z = (log(1+z) - log(1-z))/2"
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-atanh (dd-complex-atanh z)))

  ;; When z is purely real and x > 1, we have
  ;;
  ;; atanh(z) = 1/2*(log(1+z) - log(1-z))
  ;;          = 1/2*(log(1+x) - (log(x-1)+i*pi))
  ;;          = 1/2*(log(1+x) - log(x-1) - i*pi)
  ;;          = 1/2(log(1+x)-log(x-1)) - i*pi/2
  ;;
  ;; Thus, the imaginary part is negative. Thus, for z > 1, atanh is
  ;; continuous with quadrant IV.  To preserve the identity atanh(-z)
  ;; = -atanh(z), we must have atanh be continuous with Quadrant II
  ;; for z < -1.  (The identity is obviously true from the
  ;; definition.)
  ;;
  ;; NOTE: this differs from what the CLHS says for the continuity.
  ;; Instead of the text in the CLHS, we choose to use the definition
  ;; to derive the correct values.
  (if (realp z)
      (complex-atanh (complex (float z) (- (* 0 (float z)))))
      (let* ( ;; Constants
	     (theta (/ (sqrt most-positive-double-float) 4.0d0))
	     (rho (/ 4.0d0 (sqrt most-positive-double-float)))
	     (half-pi (/ pi 2.0d0))
	     (rp (float (realpart z) 1.0d0))
	     (beta (float-sign rp 1.0d0))
	     (x (* beta rp))
	     (y (* beta (- (float (imagpart z) 1.0d0))))
	     (eta 0.0d0)
	     (nu 0.0d0))
	;; Shouldn't need this declare.
	(declare (double-float x y))
	(locally
	    (declare (optimize (speed 3)))
	  (cond ((or (> x theta)
		     (> (abs y) theta))
		 ;; To avoid overflow...
		 (setf nu (float-sign y half-pi))
		 ;; eta is real part of 1/(x + iy).  This is x/(x^2+y^2),
		 ;; which can cause overflow.  Arrange this computation so
		 ;; that it won't overflow.
		 (setf eta (let* ((x-bigger (> x (abs y)))
				  (r (if x-bigger (/ y x) (/ x y)))
				  (d (+ 1.0d0 (* r r))))
			     (if x-bigger
				 (/ (/ x) d)
				 (/ (/ r y) d)))))
		((= x 1.0d0)
		 ;; Should this be changed so that if y is zero, eta is set
		 ;; to +infinity instead of approx 176?  In any case
		 ;; tanh(176) is 1.0d0 within working precision.
		 (let ((t1 (+ 4d0 (square y)))
		       (t2 (+ (abs y) rho)))
		   (setf eta (log (/ (sqrt (sqrt t1))
				     (sqrt t2))))
		   (setf nu (* 0.5d0
			       (float-sign y
					   (+ half-pi (atan (* 0.5d0 t2))))))))
		(t
		 (let ((t1 (+ (abs y) rho)))
		   ;; Normal case using log1p(x) = log(1 + x)
		   (setf eta (* 0.25d0
				(%log1p (/ (* 4.0d0 x)
					   (+ (square (- 1.0d0 x))
					      (square t1))))))
		   (setf nu (* 0.5d0
			       (atan (* 2.0d0 y)
				     (- (* (- 1.0d0 x)
					   (+ 1.0d0 x))
					(square t1))))))))
	  (coerce-to-complex-type (* beta eta)
				  (- (* beta nu))
				  z)))))

(defun complex-tanh (z)
  "Compute tanh z = sinh z / cosh z"
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-tanh (dd-complex-tanh z)))
  
  (let ((x (float (realpart z) 1.0d0))
	(y (float (imagpart z) 1.0d0)))
    (locally
	;; space 0 to get maybe-inline functions inlined
	(declare (optimize (speed 3) (space 0)))
      (cond ((> (abs x)
		#-(or linux hpux) #.(/ (%asinh most-positive-double-float) 4d0)
		;; This is more accurate under linux.
		#+(or linux hpux) #.(/ (+ (%log 2.0d0)
					  (%log most-positive-double-float)) 4d0))
	     (coerce-to-complex-type (float-sign x)
				     (float-sign y) z))
	    (t
	     (let* ((tv (%tan y))
		    (beta (+ 1.0d0 (* tv tv)))
		    (s (sinh x))
		    (rho (sqrt (+ 1.0d0 (* s s)))))
	       (if (float-infinity-p (abs tv))
		   (coerce-to-complex-type (/ rho s)
					   (/ tv)
					   z)
		   (let ((den (+ 1.0d0 (* beta s s))))
		     (coerce-to-complex-type (/ (* beta rho s)
						den)
					     (/ tv den)
					     z)))))))))

;; Kahan says we should only compute the parts needed.  Thus, the
;; realpart's below should only compute the real part, not the whole
;; complex expression.  Doing this can be important because we may get
;; spurious signals that occur in the part that we are not using.
;;
;; However, we take a pragmatic approach and just use the whole
;; expression.

;; NOTE: The formula given by Kahan is somewhat ambiguous in whether
;; it's the conjugate of the square root or the square root of the
;; conjugate.  This needs to be checked.

;; I checked.  It doesn't matter because (conjugate (sqrt z)) is the
;; same as (sqrt (conjugate z)) for all z.  This follows because
;;
;; (conjugate (sqrt z)) = exp(0.5*log |z|)*exp(-0.5*j*arg z).
;;
;; (sqrt (conjugate z)) = exp(0.5*log|z|)*exp(0.5*j*arg conj z)
;;
;; and these two expressions are equal if and only if arg conj z =
;; -arg z, which is clearly true for all z.

;; NOTE: The rules of Common Lisp says that if you mix a real with a
;; complex, the real is converted to a complex before performing the
;; operation.  However, Kahan says in this paper (pg 176):
;;
;; (iii) Careless handling can turn infinity or the sign of zero into
;;       misinformation that subsequently disappears leaving behind
;;       only a plausible but incorrect result.  That is why compilers
;;       must not transform z-1 into z-(1+i*0), as we have seen above,
;;       nor -(-x-x^2) into (x+x^2), as we shall see below, lest a
;;       subsequent logarithm or square root produce a non-zero
;;       imaginary part whose sign is opposite to what was intended.
;;
;; The interesting examples are too long and complicated to reproduce
;; here.  We refer the reader to his paper.
;;
;; The functions below are intended to handle the cases where a real
;; is mixed with a complex and we don't want CL complex contagion to
;; occur..

(declaim (inline 1+z 1-z z-1 z+1))
(defun 1+z (z)
  (complex (+ 1 (realpart z)) (imagpart z)))
(defun 1-z (z)
  (complex (- 1 (realpart z)) (- (imagpart z))))
(defun z-1 (z)
  (complex (- (realpart z) 1) (imagpart z)))
(defun z+1 (z)
  (complex (+ (realpart z) 1) (imagpart z)))

(defun complex-acos (z)
  "Compute acos z = pi/2 - asin z

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-acos (dd-complex-acos z)))
  (if (and (realp z) (> z 1))
      ;; acos is continuous in quadrant IV in this case.
      (complex-acos (complex z -0f0))
      (let ((sqrt-1+z (complex-sqrt (1+z z)))
	    (sqrt-1-z (complex-sqrt (1-z z))))
	(with-float-traps-masked (:divide-by-zero)
	  (complex (* 2 (atan (/ (realpart sqrt-1-z)
				 (realpart sqrt-1+z))))
		   (asinh (imagpart (* (conjugate sqrt-1+z)
				       sqrt-1-z))))))))

(defun complex-acosh (z)
  "Compute acosh z = 2 * log(sqrt((z+1)/2) + sqrt((z-1)/2))

Z may be any number, but the result is always a complex."
  (declare (number z))
  (let ((sqrt-z-1 (complex-sqrt (z-1 z)))
	(sqrt-z+1 (complex-sqrt (z+1 z))))
    (with-float-traps-masked (:divide-by-zero)
      (complex (asinh (realpart (* (conjugate sqrt-z-1)
				   sqrt-z+1)))
	       (* 2 (atan (/ (imagpart sqrt-z-1)
			     (realpart sqrt-z+1))))))))


;; What is the value of asin(2)?  Here is a derivation.
;;
;; asin(2) = -i*log(i*2+sqrt(-3))
;;         = -i*log(2*i+sqrt(3)*i)
;;         = -i*(log(2+sqrt(3)) + i*pi/2)
;;         = pi/2 - i*log(2+sqrt(3))
;;
;; Note that this differs from asin(2+0.0*i) because we support signed
;; zeroes.
;;
;; asin(2+0*i) = -i*log(i*(2+0*i) + sqrt(1-(4+0*i)))
;;             = -i*log((2*i - 0) + sqrt(-3-0*i))
;;             = -i*log(-0 + 2*i - sqrt(3)*i)
;;             = -i*log(-0 + i*(2-sqrt(3)))
;;             = -i*(log(2-sqrt(3)) + i*pi/2)
;;             = pi/2 - i*log(2-sqrt(3))
;;             = pi/2 + i*log(2+sqrt(3))
;;
;; The last equation follows because (2-sqrt(3)) = 1/(2+sqrt(3)).
;; Hence asin(2) /= asin(2+0*i).
;;
;; Also
;;
;; asin(2-0*i) = -i*log(i*(2-0*i) + sqrt(1-(4-0*i)))
;;             = -i*log((2*i + 0) + sqrt(-3+0*i))
;;             = -i*log(0 + 2*i + sqrt(3)*i)
;;             = -i*log(0 + i*(2+sqrt(3)))
;;             = -i*(log(2+sqrt(3)) + i*pi/2)
;;             = pi/2 + i*log(2+sqrt(3))
;;
;; Hence asin(2) = asin(2-0.0*i).
;;
;; Similar derivations will show that asin(-2) = asin(-2 + 0.0*i) and
;; asin(-2+0.0*i) is different from asin(-2-0.0*i) because of the
;; branch cut, of course.
(defun complex-asin (z)
  "Compute asin z = -i*log(i*z + sqrt(1-z^2))

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-asin (dd-complex-asin z)))
  (if (and (realp z) (> z 1))
      ;; asin is continuous in quadrant IV in this case.
      (complex-asin (complex z -0f0))
      (let ((sqrt-1-z (complex-sqrt (1-z z)))
	    (sqrt-1+z (complex-sqrt (1+z z))))
	(with-float-traps-masked (:divide-by-zero)
	  (complex (atan (/ (realpart z)
			    (realpart (* sqrt-1-z sqrt-1+z))))
		   (asinh (imagpart (* (conjugate sqrt-1-z)
				       sqrt-1+z))))))))

(defun complex-asinh (z)
  "Compute asinh z = log(z + sqrt(1 + z*z))

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; asinh z = -i * asin (i*z)
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-asinh (dd-complex-asinh z)))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-asin iz)))
    (complex (imagpart result)
	     (- (realpart result)))))
	 
(defun complex-atan (z)
  "Compute atan z = (log(1+i*z) - log(1-i*z))/(2*i)

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; atan z = -i * atanh (i*z)
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-atan (dd-complex-atan z)))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-atanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

(defun complex-tan (z)
  "Compute tan z = -i * tanh(i * z)

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; tan z = -i * tanh(i*z)
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-tan (dd-complex-tan z)))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-tanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

