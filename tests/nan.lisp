;;; Tests for NaN comparisons.
(defpackage :nan-tests
  (:use :cl :lisp-unit))

(in-package :nan-tests)

(defparameter *single-float-nan*
  (ext:with-float-traps-masked (:invalid :divide-by-zero)
    (/ 0f0 0f0)))

(defparameter *double-float-nan*
  (ext:with-float-traps-masked (:invalid :divide-by-zero)
    (/ 0d0 0d0)))


;; Define functions to test 2 and 3 arg comparisons of single and
;; double-float numbers.  The 2-arg functions are named "[ds]tst-[op]"
;; and the 3-arg functions are "[ds]tst-[op]3".  "op" is <, >, =, <=,
;; >=.  Thus stst-< compares 2 single-float numbers using < and
;; >stst-<3 compare 3 single-float numbers using <.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet
      ((frob (ntype op)
	 (let* ((name (ext:symbolicate (if (eq ntype 'single-float)
					   "S"
					   "D")
				       "TST-" op))
		(name3 (ext:symbolicate name "3")))

	   `(progn
	      (defun ,name (x y)
		(declare (,ntype x y))
		(,op x y))
	      (defun ,name3 (x y z)
		(declare (,ntype x y z))
		(,op x y z))))))
    (frob single-float <)
    (frob single-float >)
    (frob double-float <)
    (frob double-float >)
    (frob single-float =)
    (frob double-float =)
    (frob single-float <=)
    (frob single-float >=)
    (frob double-float <=)
    (frob double-float >=)))

;; Define a test Name, tagged :nan, for one of the comparison
;; functions defined above.  Fn is the function to test and Args is a
;; list of two or three float literals arranged so that (Fn . Args) is
;; true.  The Sanity forms are assertions on ordinary numbers and are
;; run first.  Then Fn is called with every combination of Args in
;; which at least one argument has been replaced by a NaN of the same
;; float format, and each such call is asserted to be false: a NaN is
;; unordered with respect to everything, including itself, so every
;; comparison involving a NaN is false.
;;
;; Before issue #156 was fixed, <= and >= were compiled as the
;; negation of > and <, which is exactly wrong for NaN, and NaN in the
;; last position of a 3-arg comparison could not be tested.
(defmacro define-nan-test (name fn args &body sanity)
  (let ((nan (etypecase (first args)
	       (single-float '*single-float-nan*)
	       (double-float '*double-float-nan*)))
	(n (length args)))
    `(define-test ,name
	 (:tag :nan)
       ,@sanity
       (ext:with-float-traps-masked (:invalid)
	 ;; The one bits in the mask determine where NaN shows up in
	 ;; the comparison operation so we have NaN in all possible
	 ;; places.
	 ,@(loop for mask from 1 below (ash 1 n)
		 collect
		 `(assert-false
		   (,fn ,@(loop for i from 0 below n
				for arg in args
				collect (if (logbitp i mask) nan arg)))))))))

(define-nan-test nan-single.< stst-< (1f0 2f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-< 1f0 2f0))
  (assert-false (stst-< 1f0 1f0))
  (assert-false (stst-< 1f0 0f0)))

(define-nan-test nan-single.<3 stst-<3 (1f0 2f0 3f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-<3 1f0 2f0 3f0))
  (assert-false (stst-<3 1f0 2f0 2f0))
  (assert-false (stst-<3 1f0 1f0 2f0))
  (assert-false (stst-<3 1f0 0f0 2f0)))

(define-nan-test nan-double.< dtst-< (1d0 2d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-< 1d0 2d0))
  (assert-false (dtst-< 1d0 1d0))
  (assert-false (dtst-< 1d0 0d0)))

(define-nan-test nan-double.<3 dtst-<3 (1d0 2d0 3d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-<3 1d0 2d0 3d0))
  (assert-false (dtst-<3 1d0 2d0 2d0))
  (assert-false (dtst-<3 1d0 1d0 2d0))
  (assert-false (dtst-<3 1d0 0d0 2d0)))

(define-nan-test nan-single.> stst-> (2f0 1f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-> 2f0 1f0))
  (assert-false (stst-> 1f0 1f0))
  (assert-false (stst-> 0f0 1f0)))

(define-nan-test nan-single.>3 stst->3 (3f0 2f0 1f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst->3 3f0 2f0 1f0))
  (assert-false (stst->3 3f0 1f0 1f0))
  (assert-false (stst->3 2f0 2f0 1f0))
  (assert-false (stst->3 0f0 2f0 1f0)))

(define-nan-test nan-double.> dtst-> (2d0 1d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-> 2d0 1d0))
  (assert-false (dtst-> 1d0 1d0))
  (assert-false (dtst-> 0d0 1d0)))

(define-nan-test nan-double.>3 dtst->3 (3d0 2d0 1d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst->3 3d0 2d0 1d0))
  (assert-false (dtst->3 3d0 1d0 1d0))
  (assert-false (dtst->3 2d0 2d0 1d0))
  (assert-false (dtst->3 0d0 2d0 1d0)))

(define-nan-test nan-single.= stst-= (1f0 1f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-= 1f0 1f0))
  (assert-false (stst-= 2f0 1f0))
  (assert-false (stst-= 0f0 1f0)))

(define-nan-test nan-single.=3 stst-=3 (1f0 1f0 1f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-=3 1f0 1f0 1f0))
  (assert-false (stst-=3 1f0 1f0 0f0))
  (assert-false (stst-=3 0f0 1f0 1f0)))

(define-nan-test nan-double.= dtst-= (1d0 1d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-= 1d0 1d0))
  (assert-false (dtst-= 2d0 1d0))
  (assert-false (dtst-= 0d0 1d0)))

(define-nan-test nan-double.=3 dtst-=3 (1d0 1d0 1d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-=3 1d0 1d0 1d0))
  (assert-false (dtst-=3 1d0 1d0 0d0))
  (assert-false (dtst-=3 0d0 1d0 1d0)))

(define-nan-test nan-single.<= stst-<= (1f0 2f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-<= 1f0 2f0))
  (assert-true (stst-<= 1f0 1f0))
  (assert-false (stst-<= 1f0 0f0)))

(define-nan-test nan-single.<=3 stst-<=3 (1f0 2f0 3f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst-<=3 1f0 2f0 3f0))
  (assert-true (stst-<=3 1f0 2f0 2f0))
  (assert-true (stst-<=3 1f0 1f0 2f0))
  (assert-false (stst-<=3 1f0 0f0 2f0)))

(define-nan-test nan-double.<= dtst-<= (1d0 2d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-<= 1d0 2d0))
  (assert-true (dtst-<= 1d0 1d0))
  (assert-false (dtst-<= 1d0 0d0)))

(define-nan-test nan-double.<=3 dtst-<=3 (1d0 2d0 3d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst-<=3 1d0 2d0 3d0))
  (assert-true (dtst-<=3 1d0 2d0 2d0))
  (assert-true (dtst-<=3 1d0 1d0 2d0))
  (assert-false (dtst-<=3 1d0 0d0 2d0)))

(define-nan-test nan-single.>= stst->= (2f0 1f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst->= 2f0 1f0))
  (assert-true (stst->= 1f0 1f0))
  (assert-false (stst->= 0f0 1f0)))

(define-nan-test nan-single.>=3 stst->=3 (3f0 2f0 1f0)
  ;; Make sure it works with ordinary single-floats.
  (assert-true (stst->=3 3f0 2f0 1f0))
  (assert-true (stst->=3 3f0 1f0 1f0))
  (assert-true (stst->=3 2f0 2f0 1f0))
  (assert-false (stst->=3 0f0 2f0 1f0)))

(define-nan-test nan-double.>= dtst->= (2d0 1d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst->= 2d0 1d0))
  (assert-true (dtst->= 1d0 1d0))
  (assert-false (dtst->= 0d0 1d0)))

(define-nan-test nan-double.>=3 dtst->=3 (3d0 2d0 1d0)
  ;; Make sure it works with ordinary double-floats.
  (assert-true (dtst->=3 3d0 2d0 1d0))
  (assert-true (dtst->=3 3d0 1d0 1d0))
  (assert-true (dtst->=3 2d0 2d0 1d0))
  (assert-false (dtst->=3 0d0 2d0 1d0)))

(define-test nan.<=->=.full-call
    (:tag :nan)
  ;; Exercise the full-call path through #'<= and #'>=, which the
  ;; declared functions above never reach.
  (ext:with-float-traps-masked (:invalid)
    (dolist (nan (list *single-float-nan* *double-float-nan*))
      (assert-false (funcall #'<= nan nan))
      (assert-false (funcall #'>= nan nan))
      (assert-false (funcall #'<= nan 1d0))
      (assert-false (funcall #'>= nan 1d0))
      (assert-false (funcall #'<= 1d0 nan))
      (assert-false (funcall #'>= 1d0 nan)))))

(define-test nan.<=->=.rational
    (:tag :nan)
  ;; Comparison of a NaN against the rational 0 takes the zero
  ;; shortcut in BASIC-COMPARE and compares against a float zero, so
  ;; it must be false.  Comparison against any other rational must
  ;; convert the NaN with RATIONAL, which signals.
  (ext:with-float-traps-masked (:invalid)
    (dolist (nan (list *single-float-nan* *double-float-nan*))
      (assert-false (funcall #'<= nan 0))
      (assert-false (funcall #'>= nan 0))
      (assert-false (funcall #'<= 0 nan))
      (assert-false (funcall #'>= 0 nan))
      (assert-error 'error (funcall #'<= nan 1))
      (assert-error 'error (funcall #'>= nan 1))
      (assert-error 'error (funcall #'<= 1 nan))
      (assert-error 'error (funcall #'>= 1 nan)))))

(define-test nan.derived-type.<=
    (:tag :nan)
  ;; The interval arithmetic used to derive types assumes no NaN, so
  ;; (abs x) has a derived type of (double-float 0d0) even though
  ;; (abs NaN) is a NaN.  Make sure <= and >= do not use the derived
  ;; bound to justify compiling into the negation of a strict
  ;; comparison, which is wrong for NaN.
  (macrolet ((frob (op)
	       `(let ((f (compile nil '(lambda (x y)
					 (declare (double-float x y))
					 (,op (abs x) y)))))
		  (ext:with-float-traps-masked (:invalid)
		    (assert-false (funcall f *double-float-nan* 1d0))
		    (assert-false (funcall f 1d0 *double-float-nan*))))))
    (frob <=)
    (frob >=)))

(define-test nan.typep-bounded-float
    (:tag :nan)
  ;; A NaN satisfies no bound, so it is not a member of any bounded
  ;; float type.  The compiled bound test used to be the negation of a
  ;; strict comparison, so NaN incorrectly passed it.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (typep *double-float-nan* '(double-float 0d0)))
    (assert-false (typep *double-float-nan* '(double-float * 0d0)))
    (assert-false (typep *double-float-nan* '(double-float -1d0 1d0)))
    (assert-false (typep *single-float-nan* '(single-float 0f0)))
    (assert-false (typep *single-float-nan* '(single-float * 0f0)))
    (assert-false (typep *single-float-nan* '(single-float -1f0 1f0)))
    ;; And the compiled version, so the TYPEP source transform is
    ;; exercised, not just %TYPEP.
    (let ((f (compile nil '(lambda (x)
			     (typep x '(double-float 0d0))))))
      (assert-false (funcall f *double-float-nan*)))))
