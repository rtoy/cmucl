;; Test that fdlibm routines signals exceptions as expected.

(defpackage :fdlibm-tests
  (:use :cl :lisp-unit))

(in-package "FDLIBM-TESTS")

(defparameter *qnan*
  (ext:with-float-traps-masked (:invalid)
    (* 0 ext:double-float-positive-infinity))
  "Some random double-float quiet NaN value")

(defparameter *sf-qnan*
  (ext:with-float-traps-masked (:invalid)
    (* 0 ext:single-float-positive-infinity))
  "Some random single-float quiet NaN value")

(defparameter *snan*
  (kernel:make-double-float #x7ff00000 1)
  "A random double-float signaling NaN value")

(defparameter *sf-snan*
  (kernel:make-single-float #x7f800001)
  "A random single-float signaling NaN value")

;;; A test body is written once, using the markers below, and
;;; DEF-FLOAT-EXCEPTIONS-TESTS substitutes the value appropriate to each
;;; float format to produce one test per format:
;;;
;;;   %fun			The routine under test
;;;   +snan+ +qnan+		A signaling and a quiet NaN
;;;   +infinity+		Positive and negative infinity
;;;   +negative-infinity+
;;;   +most-positive-float+	MOST-POSITIVE-<format>-FLOAT
;;;   +most-negative-float+	MOST-NEGATIVE-<format>-FLOAT
;;;   (%float number)		NUMBER in the format under test; NUMBER must
;;;                               be a rational or double-float
;;;
;;; Nothing else is touched, so anything that is not a marker means what
;;; it says.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun single-float-routine-name (name)
  "Return the name of the single-float version of the routine NAME.
  The C99 convention, which CMUCL follows, is to append an \"f\" to the
  name of the double-float version."
  (intern (string (ext:symbolicate name "F"))
	  (symbol-package name)))

(defun subst-float-markers (form markers format)
  "Copy FORM, replacing each marker symbol that is a key of the alist
  MARKERS by the corresponding value, and each form (%FLOAT NUMBER) by
  NUMBER coerced to the float format FORMAT.  Everything else is copied
  unchanged."
  (cond ((symbolp form)
	 (let ((marker (assoc form markers :test #'eq)))
	   (if marker
	       (cdr marker)
	       form)))
	((atom form)
	 form)
	((eq (car form) '%float)
	 (let ((number (second form)))
	   ;; A single-float has already lost the precision the
	   ;; double-float test needs.
	   (unless (or (rationalp number)
		       (typep number 'double-float))
	     (error "The argument of ~S must be a rational or a double-float, not ~S"
		    '%float number))
	   (coerce number format)))
	(t
	 (cons (subst-float-markers (car form) markers format)
	       (subst-float-markers (cdr form) markers format)))))

) ; eval-when

(defmacro def-float-exceptions-tests (routine options &body body)
  "Define a pair of tests named ROUTINE.EXCEPTIONS and
  ROUTINEf.EXCEPTIONS for the double-float and single-float versions,
  respectively, of ROUTINE.  OPTIONS is handed to DEFINE-TEST
  unchanged.  BODY is the test body, written using the markers above."
  (let ((single (single-float-routine-name routine)))
    `(progn
       (define-test ,(ext:symbolicate routine ".EXCEPTIONS")
	   ,options
	 ,@(subst-float-markers
	    body
	    `((%fun . ,routine)
	      (+snan+ . *snan*)
	      (+qnan+ . *qnan*)
	      (+infinity+ . ext:double-float-positive-infinity)
	      (+negative-infinity+ . ext:double-float-negative-infinity)
	      (+most-positive-float+ . most-positive-double-float)
	      (+most-negative-float+ . most-negative-double-float))
	    'double-float))
       (define-test ,(ext:symbolicate single ".EXCEPTIONS")
	   ,options
	 ,@(subst-float-markers
	    body
	    `((%fun . ,single)
	      (+snan+ . *sf-snan*)
	      (+qnan+ . *sf-qnan*)
	      (+infinity+ . ext:single-float-positive-infinity)
	      (+negative-infinity+ . ext:single-float-negative-infinity)
	      (+most-positive-float+ . most-positive-single-float)
	      (+most-negative-float+ . most-negative-single-float))
	    'single-float)))))

(def-float-exceptions-tests kernel:%cosh
    (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(%fun (%float 1000)))
  (assert-error 'floating-point-overflow
		(%fun (%float -1000)))
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-error 'floating-point-overflow
		(%fun +infinity+))
  (assert-error 'floating-point-overflow
		(%fun +negative-infinity+))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  ;; Same, but with overflow's masked
  (ext:with-float-traps-masked (:overflow)
    (assert-equal +infinity+
		  (%fun (%float 1000)))
    (assert-equal +infinity+
		  (%fun (%float -1000)))
    (assert-equal +infinity+
		  (%fun +infinity+))
    (assert-equal +infinity+
		  (%fun +negative-infinity+)))
  ;; Test NaN
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+)))))

(def-float-exceptions-tests kernel:%sinh
    (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(%fun (%float 1000)))
  (assert-error 'floating-point-overflow
		(%fun (%float -1000)))
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-error 'floating-point-overflow
		(%fun +infinity+))
  (assert-error 'floating-point-overflow
		(%fun +negative-infinity+))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  ;; Same, but with overflow's masked
  (ext:with-float-traps-masked (:overflow)
    (assert-equal +infinity+
		  (%fun (%float 1000)))
    (assert-equal +negative-infinity+
		  (%fun (%float -1000)))
    (assert-equal +infinity+
		  (%fun +infinity+))
    (assert-equal +negative-infinity+
		  (%fun +negative-infinity+)))
  ;; Test NaN
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+))))
  ;; sinh(x) = x for |x| < 2^-28.  Should signal inexact unless x = 0.
  (let ((x (scale-float (%float 1) -29))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%tanh
    (:tag :fdlibm)
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+))))
  ;; tanh(x) = +/- 1 for |x| > 22, raising inexact, always.
  (let ((x (%float 22.1d0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%acosh
    (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(%fun +infinity+))
  (assert-error 'floating-point-invalid-operation
		(%fun (%float 0)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal +infinity+
		  (%fun +infinity+)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun (%float 0))))))

(def-float-exceptions-tests kernel:%asinh
    (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-error 'floating-point-overflow
		(%fun +infinity+))
  (assert-error 'floating-point-overflow
		(%fun +negative-infinity+))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal +infinity+
		  (%fun +infinity+))
    (assert-equal +negative-infinity+
		  (%fun +negative-infinity+)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+))))
  (let ((x (scale-float (%float 1) -29))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (asinh x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (asinh x)))))

(def-float-exceptions-tests kernel:%atanh
    (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(%fun (%float 2)))
  (assert-error 'floating-point-invalid-operation
		(%fun (%float -2)))
  (assert-error 'division-by-zero
		(%fun (%float 1)))
  (assert-error 'division-by-zero
		(%fun (%float -1)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun (%float 2))))
    (assert-true (ext:float-nan-p (%fun (%float -2)))))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal +infinity+
		  (%fun (%float 1)))
    (assert-equal +negative-infinity+
		  (%fun (%float -1)))))

(def-float-exceptions-tests kernel:%expm1
    (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(%fun (%float 709.8d0)))
  (assert-equal +infinity+
		(%fun +infinity+))
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal +infinity+
		  (%fun (%float 709.8d0))))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+))))
  ;; expm1(x) = -1 for x < -56*log(2), signaling inexact
  #-core-math
  (let ((x (* -57 (log (%float 2)))))
    (ext:with-float-traps-enabled (:inexact)
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%log1p
    (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(%fun (%float -2)))
  (assert-error #-core-math 'floating-point-overflow
		#+core-math 'division-by-zero
		(%fun (%float -1)))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (ext:with-float-traps-masked (#-core-math :overflow
				#+core-math :divide-by-zero)
    (assert-equal +negative-infinity+
		  (%fun (%float -1))))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+))))
  ;; log1p(x) = x for |x| < 2^-54, signaling inexact except for x = 0.
  (let ((x (scale-float (%float 1) -55))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%exp
    (:tag :fdlibm)
  (assert-error 'floating-point-overflow
		(%fun (%float 710)))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-equal +infinity+
		(%fun +infinity+))
  (assert-equal (%float 0)
		(%fun (%float -1000)))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal +infinity+
		  (%fun (%float 710))))
  (ext:with-float-traps-enabled (:underflow)
    (assert-error 'floating-point-underflow
		  (%fun (%float -1000))))
  (let ((x (scale-float (%float 1) -29))
	(x0 (%float 0)))
    ;; exp(x) = x, |x| < 2^-28, with inexact exception unless x = 0
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 1) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%log
    (:tag :fdlibm)
  (assert-error 'division-by-zero
		(%fun (%float 0)))
  (assert-error 'division-by-zero
		(%fun (%float -0d0)))
  (assert-error 'floating-point-invalid-operation
		(%fun (%float -1)))
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal +negative-infinity+
		  (%fun (%float 0)))
    (assert-equal +negative-infinity+
		  (%fun (%float -0d0))))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun (%float -1))))
    (assert-true (ext:float-nan-p (%fun +snan+)))))

(def-float-exceptions-tests kernel:%acos
    (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(%fun (%float 2)))
  (assert-error 'floating-point-invalid-operation
		(%fun (%float -2)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun (%float 2))))
    (assert-true (ext:float-nan-p (%fun (%float -2))))))

(def-float-exceptions-tests kernel:%asin
    (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(%fun (%float 2)))
  (assert-error 'floating-point-invalid-operation
		(%fun (%float -2)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun (%float 2))))
    (assert-true (ext:float-nan-p (%fun (%float -2)))))
  ;; asin(x) = x for |x| < 2^-27.  Signal inexact unless x = 0.
  (let ((x (scale-float (%float 1) -28))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%atan
    (:tag :fdlibm)
  (assert-error 'floating-point-invalid-operation
		(%fun +snan+))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun +snan+))))
  ;; atan(x) = x for |x| < 2^-29, signaling inexact.
  (let ((x (scale-float (%float 1) -30))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(define-test %log10-basic-tests
    (:tag :fdlibm)
  ;; %log10(10^k) = k
  (dotimes (k 23)
    (assert-equalp k
		   (kernel:%log10 (float (expt 10 k) 1d0)))))

(def-float-exceptions-tests kernel:%log10
    (:tag :fdlibm)
  (assert-error 'division-by-zero
		(%fun (%float 0)))
  (assert-error 'floating-point-invalid-operation
		(%fun (%float -1)))
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  (assert-equal +infinity+
		(%fun +infinity+))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal +negative-infinity+
		  (%fun (%float 0)))
    (assert-equal +negative-infinity+
		  (%fun (%float -0d0))))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p (%fun (%float -1))))))

(define-test %scalbn.exceptions
  (:tag :fdlibm)
  (let ((modes (ext:get-floating-point-modes)))
    (unwind-protect
	 (progn
	   (ext:set-floating-point-modes :traps '(:underflow))
	   (assert-error 'floating-point-underflow
			 (kernel:%scalbn 1d0 -51000)))
      (apply #'ext:set-floating-point-modes modes)))
  (assert-true 0d0
	       (kernel:%scalbn 1d0 -51000))
  (assert-true -0d0
	       (kernel:%scalbn -1d0 -51000))
  (assert-error 'floating-point-overflow
		(kernel:%scalbn ext:double-float-positive-infinity 1))
  (assert-error 'floating-point-invalid-operation
		(kernel:%scalbn *snan* 1))
  (assert-error 'floating-point-overflow
		(kernel:%scalbn most-positive-double-float 2))
  (assert-error 'floating-point-overflow
		(kernel:%scalbn most-negative-double-float 2))
  (ext:with-float-traps-masked (:overflow)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%scalbn ext:double-float-positive-infinity 1))
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%scalbn most-positive-double-float 2))
    (assert-equal ext:double-float-negative-infinity
		  (kernel:%scalbn most-negative-double-float 2))))

;;; These tests taken from github.com/rtoy/fdlibm-js
(define-test acosh-basic-tests
    (:tag :fdlibm)
  ;; acosh(1) = 0
  (assert-eql 0d0 (acosh 1d0))
  ;; acosh(1.5) = log((sqrt(5)+3)/2, case 1 < x < 2
  (assert-eql 0.9624236501192069d0 (acosh 1.5d0))
  ;; acosh(4) = log(sqrt(15)+4), case 2 < x < 2^28
  (assert-eql #-core-math 2.0634370688955608d0
	      #+core-math 2.0634370688955603d0
	      (acosh 4d0))
  ;; acosh(2^50), case 2^28 < x
  (assert-eql 35.35050620855721d0 (acosh (scale-float 1d0 50)))
  ;; No overflow for most positive
  (assert-eql #-core-math 710.4758600739439d0
	      #+core-math 710.475860073944d0
	      (acosh most-positive-double-float)))

(define-test asinh-basic-tests
    (:tag :fdlibm)
  (assert-eql -0d0 (asinh -0d0))
  (assert-eql 0d0 (asinh 0d0))
  (let ((x (scale-float 1d0 -29))
	(x0 0d0))
    ;; asinh(x) = x for x < 2^-28
    (assert-eql x (asinh x))
    (assert-eql (- x) (asinh (- x))))
  (let ((x (scale-float 1d0 -28)))
    ;; Case 2 > |x| >= 2^-28
    (assert-eql 3.725290298461914d-9 (asinh x))
    (assert-eql -3.725290298461914d-9 (asinh (- x))))
  (let ((x 1d0))
    ;; Case 2 > |x| >= 2^-28
    (assert-eql 0.881373587019543d0 (asinh x))
    (assert-eql -0.881373587019543d0 (asinh (- x))))
  (let ((x 5d0))
    ;; Case 2^28 > |x| > 2
    (assert-eql 2.3124383412727525d0 (asinh x))
    (assert-eql -2.3124383412727525d0 (asinh (- x))))
  (let ((x (scale-float 1d0 28)))
    ;; Case 2^28 > |x|
    (assert-eql 20.101268236238415d0 (asinh x))
    (assert-eql -20.101268236238415d0 (asinh (- x))))
  (let ((x most-positive-double-float))
    ;; No overflow for most-positive-double-float
    (assert-eql #-core-math 710.4758600739439d0
		#+core-math 710.475860073944d0
		(asinh x))
    (assert-eql #-core-math -710.4758600739439d0
		#+core-math -710.475860073944d0
		(asinh (- x)))))
  
(define-test atanh-basic-tests
    (:tag :fdlibm)
  (assert-eql +0d0 (atanh +0d0))
  (assert-eql -0d0 (atanh -0d0))
  ;; atanh(x) = x, |x| < 2^-28
  (let ((x (scale-float 1d0 -29)))
    (assert-eql x (atanh x))
    (assert-eql (- x) (atanh (- x))))
  ;; atanh(0.25) = log(5/3)/2, |x| < 0.5
  (let ((x 0.25d0))
    (assert-eql 0.25541281188299536d0 (atanh x))
    (assert-eql -0.25541281188299536d0 (atanh (- x)))
    ;; There's no guarantee that atanh(1/4) = log(5/3)2 in floating
    ;; point, but it's true in this case with fdlibm
    (assert-eql (/ (log (float 5/3 1d0)) 2) (atanh x)))
  ;; atanh(0.75) = log(7)/2, 0.5 < |x| < 1
  (let ((x 0.75d0))
    (assert-eql 0.9729550745276566d0 (atanh x))
    (assert-eql -0.9729550745276566d0 (atanh (- x)))
    ;; There's no guarantee that atanh(3/4) = log(7)2 in floating
    ;; point, but it's true in this case with fdlibm
    (assert-eql (/ (log 7d0) 2) (atanh x))))

(define-test cosh-basic-tests
    (:tag :fdlibm)
  ;; cosh(2^-55) = 1, tiny x case
  (let ((x (scale-float 1d0 -55)))
    (assert-eql 1d0 (cosh x))
    (assert-eql 1d0 (cosh (- x))))
  ;; cosh(2^-55) = 1, tiny x case
  (let ((x (scale-float 1d0 -56)))
    (assert-eql 1d0 (cosh x))
    (assert-eql 1d0 (cosh (- x))))
  ;; cosh(log(2)/4) = (sqrt(2) + 1)/2^(5/4), case |x| < log(2)/2
  (let ((x (/ (log 2d0) 4)))
    ;; This depends on (/ (log 2d0) 4) producing the value we really
    ;; want as the arg.
    (assert-eql 1.0150517651282178d0 (cosh x))
    (assert-eql 1.0150517651282178d0 (cosh (- x))))
  ;; cosh(10*log(2)) = 1048577/2048, case log(2)/2 < |x| < 22
  (let ((x (* 10 (log 2d0)))
	(y (float 1048577/2048 1d0)))
    (assert-eql y (cosh x))
    (assert-eql y (cosh (- x))))
  ;; cosh(32*log(2)), case 22 <= |x| < log(maxdouble)
  (let ((x (* 32 (log 2d0))))
    (assert-eql 2.1474836479999983d9 (cosh x))
    (assert-eql 2.1474836479999983d9 (cosh (- x))))
  ;; cosh(710.4758600739439), case log(maxdouble) <= |x| <= overflowthreshold
  (let ((x 710.4758600739439d0))
    (assert-eql #+core-math 1.7976931348621744d308
		#-core-math 1.7976931348621746d308
		(cosh x))
    (assert-eql #+core-math 1.7976931348621744d308
		#-core-math 1.7976931348621746d308
		(cosh (- x)))))

(define-test exp-basic-tests
    (:tag :fdlibm)
  ;; No overflow and no underflow
  (let ((x 709.7822265625d0))
    (assert-eql 1.7968190737295725d308 (exp x))
    (assert-eql 5.565390609552841d-309 (exp (- x))))
  ;; exp(7.09782712893383973096e+02), no overflow
  (assert-eql 1.7976931348622732d308 (exp 7.09782712893383973096d+02))
  ;; exp(-7.45133219101941108420e+02), no underflow
  (assert-eql 4.9406564584124654d-324 (exp -7.45133219101941108420d+02))
  ;; Overflow
  (assert-error 'floating-point-overflow (exp 709.7827128933841d0))
  ;; Case |x| < 2^-28
  (let ((x (scale-float 1d0 -29)))
    (assert-eql (+ 1 x) (exp x))
    (assert-eql (- 1 x) (exp (- x))))
  ;; exp(0.5), case log(2)/2 < |x| < 3/2*log(2)
  (let ((x 0.5d0))
    (assert-eql 1.6487212707001282d0 (exp x))
    (assert-eql 0.6065306597126334d0 (exp (- x))))
  ;; exp(2), case |x| > 3/2*log(2)
  (let ((x 2d0))
    (assert-eql 7.38905609893065d0 (exp x))
    (assert-eql 0.1353352832366127d0 (exp (- x))))
  ;; exp(2^-1022), case k < -1021
  (assert-eql 1d0 (exp (scale-float 1d0 -1022)))
  ;; exp(2^-1021), case k >= -1021
  (assert-eql 1d0 (exp (scale-float 1d0 -1021)))
  ;; exp(7.09782712893383973096e+02), no overflow
  (assert-eql 1.7976931348622732d308 (exp 7.09782712893383973096d+02))
  ;; overflow
  (assert-error 'floating-point-overflow (exp 709.7827128933841d0))
  ;; exp(-7.45133219101941108420e+02), no underflow
  (assert-eql 4.9406564584124654d-324 (exp -745.1332191019411d0))
  ;; exp(-745.1332191019412), underflows
  (assert-eql 0d0 (exp -745.1332191019412d0))
  ;; exp(1000) overflow
  (assert-error 'floating-point-overflow (exp 1000d0))
  ;; exp(-1000) underflow
  (assert-eql 0d0 (exp -1000d0)))

(define-test log-basic-tests
    (:tag :fdlibm)
  (assert-eql 0d0 (log 1d0))
  (assert-eql 1d0 (log (exp 1d0)))
  (assert-eql -1d0 (log (exp -1d0)))
  (assert-eql 0.5d0 (log (sqrt (exp 1d0))))
  (assert-eql -0.5d0 (log (sqrt (exp -1d0))))
  ;; Test a denormal arg
  (assert-eql -709.08956571282410d0 (log (scale-float 1d0 -1023)))
  ;; Largest double value
  (assert-eql 709.7827128933840d0 (log most-positive-double-float))
  ;; Tests case 0 < f < 2^-20, k = 0
  ;; log(1+2^-21)
  (assert-eql 4.7683704451632344d-7 (log (+ 1 (scale-float 1d0 -21))))
  ;; Tests case 0 < f < 2^-20, k = 1
  ;; log(2 + 2^-20)
  (assert-eql 0.6931476573969898d0 (log (+ 2(scale-float 1d0 -20))))
  (assert-eql 1.3862943611198906d0 (log 4d0))
  ;; Tests main path, i > 0, k = 0
  (assert-eql 0.3220828910287846d0
	      (log (kernel:make-double-float (+ #x3ff00000 #x6147a) 0)))
  ;; Tests main path, i > 0, k = 1
  (assert-eql 0.35065625373947773d0
	      (log (kernel:make-double-float (+ #x3ff00000 #x6b851) 0)))
  ;; Tests main path, i > 0, k = -1
  (assert-eql -0.3710642895311607d0
	      (log (kernel:make-double-float (+ #x3fe00000 #x6147a) 0)))
  ;; Tests main path, i < 0, k = 0
  (assert-eql 0.3220821999597803d0
	      (log (kernel:make-double-float (+ #x3ff00000 #x61479) 0)))
  ;; Tests main path, i < 0, k = 1
  (assert-eql 1.0152293805197257d0
	      (log (kernel:make-double-float (+ #x40000000 #x61479) 0)))
  ;; Tests main path, i < 0, k = -1
  (assert-eql -0.37106498060016496d0
	      (log (kernel:make-double-float (+ #x3fe00000 #x61479) 0))))

(define-test log-consistency
    (:tag :fdlibm)
  ;; |log(x) + log(1/x)| < 1.77635684e-15, x = 1.2^k, 0 <= k < 2000
  ;; The threshold is experimentally determined
  (let ((x 1d0)
	(max-value -1d0)
	(worst-x 0d0))
    (declare (double-float max-value)
	     (type (double-float 1d0) x))
    (dotimes (k 2000)
      (let ((y (abs (+ (log x) (log (/ x))))))
	(when (> y max-value)
	  (setf worst-x x
		max-value y))
	(setf x (* x 1.4d0))))
    (assert-true (< max-value
		    #-core-math 1.77635684d-15
		    #+core-math 1.42108548d-14)
		 max-value
		 worst-x))
  ;; |exp(log(x)) - x|/x < 5.6766649d-14, x = 1.4^k, 0 <= k < 2000
  (let ((x 1d0)
	(max-error 0d0)
	(worst-x 0d0))
    (declare (double-float max-error worst-x worst-y)
	     (type (double-float 1d0) x))
    (dotimes (k 2000)
      (let ((y (abs (/ (- (exp (log x)) x) x))))
	(when (> y max-error)
	  (setf worst-x x
		max-error y))
	(setf x (* x 1.4d0))))
    (assert-true (< max-error 5.6766649d-14)
		 max-error
		 worst-x
		 worst-y))
  ;; |exp(log(x)) - x|/x < 5.68410245d-14, x = 1.4^(-k), 0 <= k < 2000
  (let ((x 1d0)
	(max-error 0d0)
	(worst-x 0d0))
    (declare (double-float max-error worst-x worst-y)
	     (type (double-float (0d0)) x))
    (dotimes (k 2000)
      (let ((y (abs (/ (- (exp (log x)) x) x))))
	(when (> y max-error)
	  (setf worst-x x
		max-error y))
	(setf x (/ x 1.4d0))))
    (assert-true (< max-error 5.68410245d-14)
		 max-error
		 worst-x)))

(define-test sinh-basic-tests
    (:tag :fdlibm)
  (assert-eql +0d0 (sinh 0d0))
  (assert-eql -0d0 (sinh -0d0))
  ;; sinh(x) = x, |x| < 2^-28
  (let ((x (scale-float 1d0 -29)))
    (assert-eql x (sinh x))
    (assert-eql (- x) (sinh (- x))))
  ;; case |x| < 1
  (assert-eql 0.5210953054937474d0 (sinh 0.5d0))
  (assert-eql -0.5210953054937474d0 (sinh -0.5d0))
  ;; sinh(10*log(2)) = 1048575/2048, case |x| < 22
  (let ((x (* 10 (log 2d0)))
	(y (float 1048575/2048 1d0)))
    (assert-eql y (sinh x))
    (assert-eql (- y) (sinh (- x))))
  ;; sinh(10), case |x| < 22
  (let ((y 11013.232874703393d0))
    (assert-eql y (sinh 10d0))
    (assert-eql (- y) (sinh -10d0)))
  ;; sinh(32*log(2)), case |x| in [22, log(maxdouble)]
  (let ((x (* 32 (log 2d0)))
	(y 2.1474836479999983d9))
    (assert-eql y (sinh x))
    (assert-eql (- y) (sinh (- x))))
  ;; sinh(100), case |x| in [22, log(maxdouble)]
  (let ((y 1.3440585709080678d43))
    (assert-eql y (sinh 100d0))
    (assert-eql (- y) (sinh -100d0)))
  ;; sinh(710....), no overflow, case |x| in [log(maxdouble), overflowthreshold]
  (let ((x 710.4758600739439d0)
	(y #+core-math 1.7976931348621744d308
	   #-core-math 1.7976931348621746d308))
    (assert-eql y (sinh x))
    (assert-eql (- y) (sinh (- x))))
  ;; sinh(710.475860073944), overflow, case |x| > ovfthreshold]
  (let ((x 710.475860073944d0))
    (assert-error 'floating-point-overflow (sinh x))
    (assert-error 'floating-point-overflow (sinh (- x))))
  (assert-error 'floating-point-overflow (sinh 1000d0))
  (assert-error 'floating-point-overflow (sinh -1000d0)))

(define-test tanh-basic-tests
    (:tag :fdlibm)
  ;; case |x| < 2^-55
  (let ((x (scale-float 1d0 -56)))
    (assert-eql x (tanh x))
    (assert-eql (- x) (tanh (- x))))
  ;; tanh(log(2)) = 3/5, case |x| < 1
  (let ((x (log 2d0))
	(y (float 3/5 1d0)))
    (assert-eql y (tanh x))
    (assert-eql (- y) (tanh (- x))))
  ;; tanh(2*log(2)) = 15/17, case |x| < 22
  (let ((x (* 2 (log 2d0)))
	(y (float 15/17 1d0)))
    (assert-eql y (tanh x))
    (assert-eql (- y) (tanh (- x))))
  ;; tanh(100) = 1, case |x| > 22
  (assert-eql 1d0 (tanh 100d0))
  (assert-eql -1d0 (tanh -100d0))
  ;; tanh(1d300), no overflow
  (assert-eql 1d0 (tanh most-positive-double-float))
  (assert-eql -1d0 (tanh (- most-positive-double-float))))

(define-test %asin-basic-tests
    (:tag :fdlibm)
  (let ((x (scale-float 1d0 -28)))
    ;; asin(x) = x for |x| < 2^-27
    (assert-eql x (kernel:%asin x))
    (assert-eql (- x) (kernel:%asin (- x)))))

(def-float-exceptions-tests kernel:%cos
    (:tag :fdlibm)
  ;; cos(inf) signals invalid operation
  (assert-error 'floating-point-invalid-operation
		(%fun +infinity+))
  (assert-error 'floating-point-invalid-operation
		(%fun +negative-infinity+))
  ;; cos(nan) is NaN
  (assert-true (ext:float-nan-p (%fun +qnan+)))
  
  ;; cos(x) = 1 for |x| < 2^-27.  Signal inexact unless x = 0
  (let ((x (scale-float (%float 1) -28))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 1) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%sin
    (:tag :fdlibm)
  ;; sin(inf) signals invalid operation
  (assert-error 'floating-point-invalid-operation
		(%fun +infinity+))
  (assert-error 'floating-point-invalid-operation
		(%fun +negative-infinity+))
  ;; sin(nan) is NaN
  (assert-true (ext:float-nan-p (%fun +qnan+)))

  ;; sin(x) = x for |x| < 2^-27.  Signal inexact unless x = 0
  (let ((x (scale-float (%float 1) -28))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

(def-float-exceptions-tests kernel:%tan
    (:tag :fdlibm)
  ;; tan(inf) signals invalid operation
  (assert-error 'floating-point-invalid-operation
		(%fun +infinity+))
  (assert-error 'floating-point-invalid-operation
		(%fun +negative-infinity+))
  ;; tan(nan) is NaN
  (assert-true (ext:float-nan-p (%fun +qnan+)))

  ;; tan(x) = x for |x| < 2^-28.  Signal inexact unless x = 0
  (let ((x (scale-float (%float 1) -29))
	(x0 (%float 0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must not throw an inexact exception because the result
	;; is exact when the arg is 0.
	(assert-eql (%float 0) (%fun x0)))
    (ext:with-float-traps-enabled (:inexact)
	;; This must throw an inexact exception for non-zero x even
	;; though the result is exactly x.
	(assert-error 'floating-point-inexact
		      (%fun x)))))

;; Test cases from e_pow.c for fdlibm.
(define-test %pow.case.1
    (:tag :fdlibm)
  ;; anything ^ 0 is 1
  (assert-equal 1d0
		(kernel:%pow ext:double-float-positive-infinity 0d0))
  (assert-equal 1d0
		(kernel:%pow ext:double-float-negative-infinity 0d0)))

(define-test %pow.case.2
    (:tag :fdlibm)
  ;; anything ^ 1 is itself
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow ext:double-float-positive-infinity 1d0))
  (assert-equal ext:double-float-negative-infinity
		(kernel:%pow ext:double-float-negative-infinity 1d0)))

(define-test %pow.case.3
    (:tag :fdlibm)
  ;; anything ^ NaN is NaN
  (assert-true (ext:float-nan-p
		(kernel:%pow pi *qnan*)))
  (assert-true (ext:float-nan-p
		(kernel:%pow ext:double-float-positive-infinity *qnan*))))

(define-test %pow.case.4
    (:tag :fdlibm)
  ;; NaN ^ non-zero is NaN
  (assert-true (ext:float-nan-p
		(kernel:%pow *qnan* pi)))
  (assert-true (ext:float-nan-p
		(kernel:%pow *qnan* ext:double-float-positive-infinity))))

(define-test %pow.case.5
    (:tag :fdlibm)
  ;; (|x| > 1) ^ +inf is +inf
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow pi ext:double-float-positive-infinity))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow (- pi) ext:double-float-positive-infinity)))

(define-test %pow.case.6
    (:tag :fdlibm)
  ;; (|x| > 1) ^ -inf is +0
  (assert-equal +0d0
		(kernel:%pow pi ext:double-float-negative-infinity))
  (assert-equal +0d0
		(kernel:%pow (- pi) ext:double-float-negative-infinity)))

(define-test %pow.case.7
    (:tag :fdlibm)
  ;; (|x| < 1) ^ +inf is +0
  (assert-equal +0d0
		(kernel:%pow 0.5d0 ext:double-float-positive-infinity))
  (assert-equal +0d0
		(kernel:%pow -0.5d0 ext:double-float-positive-infinity)))

(define-test %pow.case.8
    (:tag :fdlibm)
  ;; (|x| < 1) ^ -inf is +inf
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow 0.5d0 ext:double-float-negative-infinity))
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow -0.5d0 ext:double-float-negative-infinity)))

(define-test %pow.case.9
    (:tag :fdlibm)
  ;; std::pow says 1^exp is 1 for any exp, including NaN.  (-1)^(+/-inf)
  ;; is 1.  No errors signaled.
  #+core-math
  (progn
    (assert-equal 1d0
		  (kernel:%pow 1d0 ext:double-float-positive-infinity))
    (assert-equal 1d0
		  (kernel:%pow 1d0 ext:double-float-negative-infinity))
    (assert-equal 1d0
		  (kernel:%pow 1d0 *qnan*))
    (assert-equal 1d0
		  (kernel:%pow -1d0 ext:double-float-positive-infinity))
    (assert-equal 1d0
		  (kernel:%pow -1d0 ext:double-float-negative-infinity)))
  #-core-math
  ;; +-1 ^ +-inf is NaN.
  ;;
  ;; But the implementation signals invalid operation, so we need to
  ;; check for that.
  ;;
  (progn
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow 1d0 ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow 1d0 ext:double-float-negative-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow -1d0 ext:double-float-positive-infinity))
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow -1d0 ext:double-float-negative-infinity))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p
		  (kernel:%pow 1d0 ext:double-float-positive-infinity)))
    (assert-true (ext:float-nan-p
		  (kernel:%pow 1d0 ext:double-float-negative-infinity)))
    (assert-true (ext:float-nan-p
		  (kernel:%pow -1d0 ext:double-float-positive-infinity)))
    (assert-true (ext:float-nan-p
		  (kernel:%pow -1d0 ext:double-float-negative-infinity))))))

(define-test %pow.case.10
    (:tag :fdlibm)
  ;; +0 ^ (+anything except 0, Nan) is +0
  (assert-equal +0d0
		(kernel:%pow +0d0 10d0))
  (assert-equal +0d0
		(kernel:%pow +0d0 ext:double-float-positive-infinity)))

(define-test %pow.case.11
    (:tag :fdlibm)
  ;; +0 ^ (+anything except 0, Nan, odd integer) is +0
  (assert-equal +0d0
		(kernel:%pow -0d0 10d0))
  (assert-equal +0d0
		(kernel:%pow -0d0 ext:double-float-positive-infinity)))

(define-test %pow.case.12
    (:tag :fdlibm)
  ;; +0 ^ (-anything except 0, Nan) is +inf
  ;;
  ;; But fdlibm signals error for (+0)^(-10) instead of returning inf.  Check this.
  (assert-error 'division-by-zero
		(kernel:%pow +0d0 -10d0))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%pow +0d0 -10d0)))
  ;; No signals here.
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow +0d0 ext:double-float-negative-infinity)))

(define-test %pow.case.13
    (:tag :fdlibm)
  ;; -0 ^ (-anything except 0, Nan, odd integer) is +inf
  ;;
  ;; But (-0)^(-10) signals division by zero
  (assert-error 'division-by-zero
		(kernel:%pow -0d0 -10d0))
  (ext:with-float-traps-masked (:divide-by-zero)
    (assert-equal ext:double-float-positive-infinity
		  (kernel:%pow -0d0 -10d0)))
  ;; But no error here.
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow +0d0 ext:double-float-negative-infinity)))

(define-test %pow.case.14
    (:tag :fdlibm)
  ;; -0 ^ (odd integer) = -( +0 ^ (odd integer))
  (assert-equal (- (kernel:%pow +0d0 5d0))
		(kernel:%pow -0d0 5d0)))

(define-test %pow.case.15
    (:tag :fdlibm)
  ;; +inf ^ (+anything except 0, NaN) is +inf
  (assert-equal ext:double-float-positive-infinity
		(kernel:%pow ext:double-float-positive-infinity pi)))

(define-test %pow.case.16
    (:tag :fdlibm)
  ;; +inf ^ (-anything except 0, NaN) is +0
  (assert-equal +0d0
		(kernel:%pow ext:double-float-positive-infinity (- pi))))

(define-test %pow.case.17
    (:tag :fdlibm)
  ;; -inf ^ (anything) = -0 ^ (-anything)
  (assert-equal (ext:with-float-traps-masked (:divide-by-zero)
		  ;; This produces a divide-by-zero error so mask it
		  ;; to get a value.
		  (kernel:%pow -0d0 (- pi)))
		(kernel:%pow ext:double-float-negative-infinity pi))
  (assert-equal (kernel:%pow -0d0 pi)
		(kernel:%pow ext:double-float-negative-infinity (- pi))))

(define-test %pow.case.18
    (:tag :fdlibm)
  ;; (-anything) ^ integer is (-1)^integer * (+anything ^ integer)
  (dolist (base '(-2d0 -10d0))
    (dolist (power '(5 -5))
      (assert-equal (* (expt -1 power)
		       (kernel:%pow (- base) (coerce power 'double-float)))
		    (kernel:%pow base (coerce power 'double-float))
		    base power))))

(define-test %pow.case.19
    (:tag :fdlibm)
  ;; (-anything except 0 and inf) ^ non-integer is NaN
  ;;
  ;; But this signals invalid, so check for that too.
  (assert-error 'floating-point-invalid-operation
		(kernel:%pow -2d0 1.5d0))
  (ext:with-float-traps-masked (:invalid)
    (assert-true (ext:float-nan-p
		  (kernel:%pow -2d0 1.5d0)))))
