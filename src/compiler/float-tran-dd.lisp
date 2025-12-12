;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/float-tran-dd.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating-point specific transforms for
;;; double-doubles.
;;;
;;; The algorithms contained herein are based on the code written by
;;; Yozo Hida.  See http://www.cs.berkeley.edu/~yozo/ for more
;;; information.

(in-package "C")
(intl:textdomain "cmucl")

#+double-double
(progn
(defknown %double-double-float (real)
  double-double-float
  (movable foldable flushable))

(deftransform float ((n prototype) (* double-double-float) * :when :both)
  '(%double-double-float n))

(deftransform %double-float ((n) (double-double-float) * :when :both)
  '(double-double-hi n))

(deftransform %single-float ((n) (double-double-float) * :when :both)
  '(float (double-double-hi n) 1f0))

(deftransform %double-double-float ((n) (double-double-float) * :when :both)
  'n)

#+nil
(defun %double-double-float (n)
  (make-double-double-float (float n 1d0) 0d0))

;; Moved to code/float.lisp, because we need this relatively early in
;; the build process to handle float and real types.
#+nil
(defun %double-double-float (n)
  (typecase n
    (fixnum
     (%make-double-double-float (float n 1d0) 0d0))
    (single-float
     (%make-double-double-float (float n 1d0) 0d0))
    (double-float
     (%make-double-double-float (float n 1d0) 0d0))
    (double-double-float
     n)
    (bignum
     (bignum:bignum-to-float n 'double-double-float))
    (ratio
     (kernel::float-ratio n 'double-double-float))))

(defknown double-double-float-p (t)
  boolean
  (movable foldable flushable))

(defknown %make-double-double-float (double-float double-float)
  double-double-float
  (movable foldable flushable))


(defknown double-double-hi (double-double-float)
  double-float
  (movable foldable flushable))

(defknown double-double-lo (double-double-float)
  double-float
  (movable foldable flushable))

(deftransform float-sign ((float &optional float2)
			  (double-double-float &optional double-double-float) *)
  (if float2
      (let ((temp (gensym)))
	`(let ((,temp (abs float2)))
	   (if (minusp (float-sign (double-double-hi float)))
	       (- ,temp)
	       ,temp)))
      '(if (minusp (float-sign (double-double-hi float))) -1w0 1w0)))

(deftransform cis ((x) (double-double-float) *)
  `(multiple-value-bind (s c)
       (kernel::dd-%sincos x)
     (complex c s)))



(declaim (inline quick-two-sum))
(defun quick-two-sum (a b)
  "Computes fl(a+b) and err(a+b), assuming |a| >= |b|"
  (declare (double-float a b))
  (let* ((s (+ a b))
	 (e (- b (- s a))))
    (values s e)))

(declaim (inline two-sum))
(defun two-sum (a b)
  "Computes fl(a+b) and err(a+b)"
  (declare (double-float a b))
  (let* ((s (+ a b))
	 (v (- s a))
	 (e (+ (- a (- s v))
	       (- b v))))
    (locally
	(declare (optimize (inhibit-warnings 3)))
      (values s e))))

(declaim (maybe-inline add-dd))
(defun add-dd (a0 a1 b0 b1)
  "Add the double-double A0,A1 to the double-double B0,B1"
  (declare (double-float a0 a1 b0 b1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (multiple-value-bind (s1 s2)
      (two-sum a0 b0)
    (declare (double-float s1 s2))
    (when (float-infinity-p s1)
      (return-from add-dd (values s1 0d0)))
    (multiple-value-bind (t1 t2)
	(two-sum a1 b1)
      (declare (double-float t1 t2))
      (incf s2 t1)
      (multiple-value-bind (s1 s2)
	  (quick-two-sum s1 s2)
	(declare (double-float s1 s2))
	(incf s2 t2)
	(multiple-value-bind (r1 r2)
	    (quick-two-sum s1 s2)
	  (if (and (zerop a0) (zerop b0))
	      ;; Handle sum of signed zeroes here.
	      (values (float-sign (+ a0 b0) 0d0)
		      0d0)
	      (values r1 r2)))))))

(deftransform + ((a b) (vm::double-double-float vm::double-double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
       (add-dd (kernel:double-double-hi a) (kernel:double-double-lo a)
	       (kernel:double-double-hi b) (kernel:double-double-lo b))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(declaim (inline quick-two-diff))
(defun quick-two-diff (a b)
  "Compute fl(a-b) and err(a-b), assuming |a| >= |b|"
  (declare (double-float a b))
  (let ((s (- a b)))
    (values s (- (- a s) b))))

(declaim (inline two-diff))
(defun two-diff (a b)
  "Compute fl(a-b) and err(a-b)"
  (declare (double-float a b))
  (let* ((s (- a b))
	 (v (- s a))
	 (e (- (- a (- s v))
	       (+ b v))))
    (locally
	(declare (optimize (inhibit-warnings 3)))
      (values s e))))

(declaim (maybe-inline sub-dd))
(defun sub-dd (a0 a1 b0 b1)
  "Subtract the double-double B0,B1 from A0,A1"
  (declare (double-float a0 a1 b0 b1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (multiple-value-bind (s1 s2)
      (two-diff a0 b0)
    (declare (double-float s2))
    (when (float-infinity-p s1)
      (return-from sub-dd (values s1 0d0)))
    (multiple-value-bind (t1 t2)
	(two-diff a1 b1)
      (incf s2 t1)
      (multiple-value-bind (s1 s2)
	  (quick-two-sum s1 s2)
	(declare (double-float s2))
	(incf s2 t2)
	(multiple-value-bind (r1 r2)
	    (quick-two-sum s1 s2)
	  (if (and (zerop a0) (zerop b0))
	      (values (float-sign (- a0 b0) 0d0)
		      0d0)
	      (values r1 r2)))))))

(declaim (maybe-inline sub-d-dd))
(defun sub-d-dd (a b0 b1)
  "Compute double-double = double - double-double"
  (declare (double-float a b0 b1)
	   (optimize (speed 3) (safety 0)
		     (inhibit-warnings 3)))
  (multiple-value-bind (s1 s2)
      (two-diff a b0)
    (declare (double-float s2))
    (when (float-infinity-p s1)
      (return-from sub-d-dd (values s1 0d0)))
    (decf s2 b1)
    (multiple-value-bind (r1 r2)
	(quick-two-sum s1 s2)
      (if (and (zerop a) (zerop b0))
	(values (float-sign (- a b0) 0d0) 0d0)
	(values r1 r2)))))

(declaim (maybe-inline sub-dd-d))
(defun sub-dd-d (a0 a1 b)
  "Subtract the double B from the double-double A0,A1"
  (declare (double-float a0 a1 b)
	   (optimize (speed 3) (safety 0)
		     (inhibit-warnings 3)))
  (multiple-value-bind (s1 s2)
      (two-diff a0 b)
    (declare (double-float s2))
    (when (float-infinity-p s1)
      (return-from sub-dd-d (values s1 0d0)))
    (incf s2 a1)
    (multiple-value-bind (r1 r2)
	(quick-two-sum s1 s2)
      (if (and (zerop a0) (zerop b))
	(values (float-sign (- a0 b) 0d0) 0d0)
	(values r1 r2)))))

(deftransform - ((a b) (vm::double-double-float vm::double-double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
      (sub-dd (kernel:double-double-hi a) (kernel:double-double-lo a)
	      (kernel:double-double-hi b) (kernel:double-double-lo b))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform - ((a b) (double-float vm::double-double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
       (sub-d-dd a
		 (kernel:double-double-hi b) (kernel:double-double-lo b))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform - ((a b) (vm::double-double-float double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
       (sub-dd-d (kernel:double-double-hi a) (kernel:double-double-lo a)
		 b)
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(declaim (maybe-inline split))
;; See Listing 2.6: Mul12 in "CR-LIBM: A library of correctly rounded
;; elementary functions in double-precision".  Also known as Dekker's
;; algorithm.
(defun split (a)
  "Split the double-float number a into a-hi and a-lo such that a =
  a-hi + a-lo and a-hi contains the upper 26 significant bits of a and
  a-lo contains the lower 26 bits."
  (declare (double-float a))
  (let* ((tmp (* a (+ 1 (expt 2 27))))
	 (a-hi (- tmp (- tmp a)))
	 (a-lo (- a a-hi)))
    (values a-hi a-lo)))

;; Values used for scaling in two-prod.  These are used to determine
;; if SPLIT might overflow so the value (and result) can be scaled to
;; prevent overflow.
(defconstant +two970+
  (scale-float 1d0 970))

(defconstant +two53+
  (scale-float 1d0 53))

(defconstant +two-53+
  (scale-float 1d0 -53))

(declaim (inline two-prod))

;; This is essentially the algorithm given by Listing 2.7 Mul12Cond
;; given in "CR-LIBM: A library of correctly rounded elementary
;; functions in double-precision".
#-ppc
(defun two-prod (a b)
  _N"Compute fl(a*b) and err(a*b)"
  (declare (double-float a b)
	   (optimize (speed 3)))
  ;; If the numbers are too big, scale them done so SPLIT doesn't overflow.
  (multiple-value-bind (aa bb)
      (values (if (> a +two970+)
		  (* a +two-53+)
		  a)
	      (if (> b +two970+)
		  (* b +two-53+)
		  b))
    (let ((p (* aa bb)))
      (declare (double-float p)
	       (inline split))
      (multiple-value-bind (aa-hi aa-lo)
	  (split aa)
	;;(format t "aa-hi, aa-lo = ~S ~S~%" aa-hi aa-lo)
	(multiple-value-bind (bb-hi bb-lo)
	    (split bb)
	  ;;(format t "bb-hi, bb-lo = ~S ~S~%" bb-hi bb-lo)
	  (let ((e (+ (+ (- (* aa-hi bb-hi) p)
			 (* aa-hi bb-lo)
			 (* aa-lo bb-hi))
		      (* aa-lo bb-lo))))
	    (declare (double-float e))
	    (locally 
		(declare (optimize (inhibit-warnings 3)))
	      ;; If the numbers was scaled down, we need to scale the
	      ;; result back up.
	      (when (> a +two970+)
		(setf p (* p +two53+)
		      e (* e +two53+)))
	      (when (> b +two970+)
		(setf p (* p +two53+)
		      e (* e +two53+)))
	      (values p e))))))))

#+ppc
(defun two-prod (a b)
  _N"Compute fl(a*b) and err(a*b)"
  (declare (double-float a b))
  ;; PPC has a fused multiply-subtract instruction that can be used
  ;; here, so use it.
  (let* ((p (* a b))
	 (err (vm::fused-multiply-subtract a b p)))
    (values p err)))

(declaim (inline two-sqr))
#-ppc
(defun two-sqr (a)
  _N"Compute fl(a*a) and err(a*b).  This is a more efficient
  implementation of two-prod"
  (declare (double-float a))
  (let ((q (* a a)))
    (multiple-value-bind (a-hi a-lo)
	(split a)
      (locally
	  (declare (optimize (inhibit-warnings 3)))
	(values q (+ (+ (- (* a-hi a-hi) q)
			(* 2 a-hi a-lo))
		     (* a-lo a-lo)))))))
(defun two-sqr (a)
  _N"Compute fl(a*a) and err(a*b).  This is a more efficient
  implementation of two-prod"
  (declare (double-float a))
  (let ((aa (if (> a +two970+)
		(* a +two-53+)
		a)))
    (let ((q (* aa aa)))
      (declare (double-float q)
	       (inline split))
      (multiple-value-bind (a-hi a-lo)
	  (split aa)
	(locally
	    (declare (optimize (inhibit-warnings 3)))
	  (let ((e (+ (+ (- (* a-hi a-hi) q)
			 (* 2 a-hi a-lo))
		      (* a-lo a-lo))))
	    (if (> a +two970+)
	      (values (* q +two53+)
		      (* e +two53+))
	      (values q e))))))))

#+ppc
(defun two-sqr (a)
  _N"Compute fl(a*a) and err(a*b).  This is a more efficient
  implementation of two-prod"
  (declare (double-float a))
  (let ((q (* a a)))
    (values q (vm::fused-multiply-subtract a a q))))

(declaim (maybe-inline mul-dd-d))
(defun mul-dd-d (a0 a1 b)
  (declare (double-float a0 a1 b)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (multiple-value-bind (p1 p2)
      (two-prod a0 b)
    (declare (double-float p2))
    (when (float-infinity-p p1)
      (return-from mul-dd-d (values p1 0d0)))
    ;;(format t "mul-dd-d p1,p2 = ~A ~A~%" p1 p2)
    (incf p2 (* a1 b))
    ;;(format t "mul-dd-d p2 = ~A~%" p2)
    (multiple-value-bind (r1 r2)
	(quick-two-sum p1 p2)
      (when (zerop r1)
	(setf r1 (float-sign p1 0d0))
	(setf r2 p1))
      (values r1 r2))))

(declaim (maybe-inline mul-dd))
(defun mul-dd (a0 a1 b0 b1)
  "Multiply the double-double A0,A1 with B0,B1"
  (declare (double-float a0 a1 b0 b1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (multiple-value-bind (p1 p2)
      (two-prod a0 b0)
    (declare (double-float p1 p2))
    (when (float-infinity-p p1)
      (return-from mul-dd (values p1 0d0)))
    (incf p2 (* a0 b1))
    (incf p2 (* a1 b0))
    (multiple-value-bind (r1 r2)
	(quick-two-sum p1 p2)
      (if (zerop r1)
	(values (float-sign p1 0d0) 0d0)
	(values r1 r2)))))

(declaim (maybe-inline add-dd-d))
(defun add-dd-d (a0 a1 b)
  "Add the double-double A0,A1 to the double B"
  (declare (double-float a0 a1 b)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (multiple-value-bind (s1 s2)
      (two-sum a0 b)
    (declare (double-float s1 s2))
    (when (float-infinity-p s1)
      (return-from add-dd-d (values s1 0d0)))
    (incf s2 a1)
    (multiple-value-bind (r1 r2)
	(quick-two-sum s1 s2)
      (if (and (zerop a0) (zerop b))
	(values (float-sign (+ a0 b) 0d0) 0d0)
	(values r1 r2)))))

(declaim (maybe-inline sqr-dd))
(defun sqr-dd (a0 a1)
  (declare (double-float a0 a1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (multiple-value-bind (p1 p2)
      (two-sqr a0)
    (declare (double-float p1 p2))
    (incf p2 (* 2 a0 a1))
    ;; Hida's version of sqr (qd-2.1.210) has the following line for
    ;; the sqr function.  But if you compare this with mul-dd, this
    ;; doesn't exist there, and if you leave it in, it produces
    ;; results that are different from using mul-dd to square a value.
    #+nil
    (incf p2 (* a1 a1))
    (quick-two-sum p1 p2)))

(deftransform + ((a b) (vm::double-double-float (or integer single-float double-float))
		 * :node node)
  `(multiple-value-bind (hi lo)
       (add-dd-d (kernel:double-double-hi a) (kernel:double-double-lo a)
		 (float b 1d0))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform + ((a b) ((or integer single-float double-float) vm::double-double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
      (add-dd-d (kernel:double-double-hi b) (kernel:double-double-lo b)
		(float a 1d0))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform * ((a b) (vm::double-double-float vm::double-double-float)
		 * :node node)
  ;; non-const-same-leaf-ref-p is stolen from two-arg-derive-type.
  (flet ((non-const-same-leaf-ref-p (x y)
	   ;; Just like same-leaf-ref-p, but we don't care if the
	   ;; value of the leaf is constant or not.
	   (declare (type continuation x y))
	   (let ((x-use (continuation-use x))
		 (y-use (continuation-use y)))
	     (and (ref-p x-use)
		  (ref-p y-use)
		  (eq (ref-leaf x-use) (ref-leaf y-use))))))
    (destructuring-bind (arg1 arg2)
	(combination-args node)
      ;; If the two args to * are the same, we square the number
      ;; instead of multiply.  Squaring is simpler than a full
      ;; multiply.
      (if (non-const-same-leaf-ref-p arg1 arg2)
	  `(multiple-value-bind (hi lo)
	       (sqr-dd (kernel:double-double-hi a) (kernel:double-double-lo a))
	     (truly-the ,(type-specifier (node-derived-type node))
			(kernel:%make-double-double-float hi lo)))
	  `(multiple-value-bind (hi lo)
	       (mul-dd (kernel:double-double-hi a) (kernel:double-double-lo a)
		       (kernel:double-double-hi b) (kernel:double-double-lo b))
	     (truly-the ,(type-specifier (node-derived-type node))
			(kernel:%make-double-double-float hi lo)))))))

(deftransform * ((a b) (vm::double-double-float (or integer single-float double-float))
		 * :node node)
  `(multiple-value-bind (hi lo)
       (mul-dd-d (kernel:double-double-hi a) (kernel:double-double-lo a)
		 (float b 1d0))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform * ((a b) ((or integer single-float double-float) vm::double-double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
       (mul-dd-d (kernel:double-double-hi b) (kernel:double-double-lo b)
		 (float a 1d0))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(declaim (maybe-inline div-dd))
(defun div-dd (a0 a1 b0 b1)
  "Divide the double-double A0,A1 by B0,B1"
  (declare (double-float a0 a1 b0 b1)
	   (optimize (speed 3)
		     (inhibit-warnings 3))
	   (inline sub-dd))
  (let ((q1 (/ a0 b0)))
    (when (float-infinity-p q1)
      (return-from div-dd (values q1 0d0)))
    ;; (q1b0, q1b1) = q1*(b0,b1)
    ;;(format t "q1 = ~A~%" q1)
    (multiple-value-bind (q1b0 q1b1)
	(mul-dd-d b0 b1 q1)
      ;;(format t "q1*b = ~A ~A~%" q1b0 q1b1)
      (multiple-value-bind (r0 r1)
	  ;; r = a - q1 * b
	  (sub-dd a0 a1 q1b0 q1b1)
	;;(format t "r = ~A ~A~%" r0 r1)
	(let ((q2 (/ r0 b0)))
	  (multiple-value-bind (q2b0 q2b1)
	      (mul-dd-d b0 b1 q2)
	    (multiple-value-bind (r0 r1)
		;; r = r - (q2*b)
		(sub-dd r0 r1 q2b0 q2b1)
	      (declare (ignore r1))
	      (let ((q3 (/ r0 b0)))
		(multiple-value-bind (q1 q2)
		    (quick-two-sum q1 q2)
		  (add-dd-d q1 q2 q3))))))))))

(declaim (maybe-inline div-dd-d))
(defun div-dd-d (a0 a1 b)
  (declare (double-float a0 a1 b)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (let ((q1 (/ a0 b)))
    ;; q1 = approx quotient
    ;; Now compute a - q1 * b
    (multiple-value-bind (p1 p2)
	(two-prod q1 b)
      (multiple-value-bind (s e)
	  (two-diff a0 p1)
	(declare (double-float e))
	(incf e a1)
	(decf e p2)
	;; Next approx
	(let ((q2 (/ (+ s e) b)))
	  (quick-two-sum q1 q2))))))

(deftransform / ((a b) (vm::double-double-float vm::double-double-float)
		 * :node node)
  `(multiple-value-bind (hi lo)
      (div-dd (kernel:double-double-hi a) (kernel:double-double-lo a)
	      (kernel:double-double-hi b) (kernel:double-double-lo b))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform / ((a b) (vm::double-double-float (or integer single-float double-float))
		 * :node node)
  `(multiple-value-bind (hi lo)
       (div-dd-d (kernel:double-double-hi a) (kernel:double-double-lo a)
		 (float b 1d0))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform / ((a b) ((complex vm::double-double-float) (complex vm::double-double-float))
		 *)
  `(kernel::cdiv-double-double-float a b))	      
  	      
(declaim (inline sqr-d))
(defun sqr-d (a)
  "Square"
  (declare (double-float a)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (two-sqr a))

(declaim (inline mul-d-d))
(defun mul-d-d (a b)
  (two-prod a b))

(declaim (maybe-inline sqrt-dd))
(defun sqrt-dd (a0 a1)
  (declare (type (double-float 0d0) a0)
	   (double-float a1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  ;; Strategy: Use Karp's trick: if x is an approximation to sqrt(a),
  ;; then
  ;;
  ;; y = a*x + (a-(a*x)^2)*x/2
  ;;
  ;; is an approximation that is accurate to twice the accuracy of x.
  ;; Also, the multiplication (a*x) and [-]*x can be done with only
  ;; half the precision.
  (if (and (zerop a0) (zerop a1))
      (values a0 a1)
      (let* ((x (/ (sqrt a0)))
	     (ax (* a0 x)))
	(multiple-value-bind (s0 s1)
	    (sqr-d ax)
	  (multiple-value-bind (s2)
	      (sub-dd a0 a1 s0 s1)
	    (multiple-value-bind (p0 p1)
		(mul-d-d s2 (* x 0.5d0))
	      (add-dd-d p0 p1 ax)))))))

(deftransform sqrt ((a) ((vm::double-double-float 0w0))
		    * :node node)
  `(multiple-value-bind (hi lo)
       (sqrt-dd (kernel:double-double-hi a) (kernel:double-double-lo a))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(declaim (inline neg-dd))
(defun neg-dd (a0 a1)
  (declare (double-float a0 a1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (values (- a0) (- a1)))

(declaim (inline abs-dd))
(defun abs-dd (a0 a1)
  (declare (double-float a0 a1)
	   (optimize (speed 3)
		     (inhibit-warnings 3)))
  (if (minusp a0)
      (neg-dd a0 a1)
      (values a0 a1)))

(deftransform abs ((a) (vm::double-double-float)
		   * :node node)
  `(multiple-value-bind (hi lo)
       (abs-dd (kernel:double-double-hi a) (kernel:double-double-lo a))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(deftransform %negate ((a) (vm::double-double-float)
		       * :node node)
  `(multiple-value-bind (hi lo)
       (neg-dd (kernel:double-double-hi a) (kernel:double-double-lo a))
     (truly-the ,(type-specifier (node-derived-type node))
		(kernel:%make-double-double-float hi lo))))

(declaim (inline dd=))
(defun dd= (a0 a1 b0 b1)
  (and (= a0 b0)
       (= a1 b1)))
  
(declaim (inline dd<))
(defun dd< (a0 a1 b0 b1)
  (or (< a0 b0)
       (and (= a0 b0)
	    (< a1 b1))))

(declaim (inline dd>))
(defun dd> (a0 a1 b0 b1)
  (or (> a0 b0)
       (and (= a0 b0)
	    (> a1 b1))))
  
(deftransform = ((a b) (vm::double-double-float vm::double-double-float) *)
  `(dd= (kernel:double-double-hi a)
	(kernel:double-double-lo a)
	(kernel:double-double-hi b)
	(kernel:double-double-lo b)))


(deftransform < ((a b) (vm::double-double-float vm::double-double-float) *)
  `(dd< (kernel:double-double-hi a)
	(kernel:double-double-lo a)
	(kernel:double-double-hi b)
	(kernel:double-double-lo b)))


(deftransform > ((a b) (vm::double-double-float vm::double-double-float) *)
  `(dd> (kernel:double-double-hi a)
	(kernel:double-double-lo a)
	(kernel:double-double-hi b)
	(kernel:double-double-lo b)))
) ; end progn
