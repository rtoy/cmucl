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

(define-test nan-single.<
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst-< 1f0 2f0))
  (assert-false (stst-< 1f0 1f0))
  (assert-false (stst-< 1f0 0f0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-< *single-float-nan* 1f0))
    (assert-false (stst-< 1f0 *single-float-nan*))
    (assert-false (stst-< *single-float-nan* *single-float-nan*))))

(define-test nan-double.<
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (dtst-< 1d0 2d0))
  (assert-false (dtst-< 1d0 1d0))
  (assert-false (dtst-< 1d0 0d0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-< *double-float-nan* 1d0))
    (assert-false (dtst-< 1d0 *double-float-nan*))
    (assert-false (dtst-< *double-float-nan* *double-float-nan*))))

(define-test nan-single.>
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst-> 2f0 1f0))
  (assert-false (stst-> 1f0 1f0))
  (assert-false (stst-> 0f0 1f0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-> *single-float-nan* 1f0))
    (assert-false (stst-> 1f0 *single-float-nan*))
    (assert-false (stst-> *single-float-nan* *single-float-nan*))))

(define-test nan-double.>
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (dtst-> 2d0 1d0))
  (assert-false (dtst-> 1d0 1d0))
  (assert-false (dtst-> 0d0 1d0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-> *double-float-nan* 1d0))
    (assert-false (dtst-> 1d0 *double-float-nan*))
    (assert-false (dtst-> *double-float-nan* *double-float-nan*))))

(define-test nan-single.<3
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst-<3 1f0 2f0 3f0))
  (assert-false (stst-<3 1f0 2f0 2f0))
  (assert-false (stst-<3 1f0 1f0 2f0))
  (assert-false (stst-<3 1f0 0f0 2f0))
  ;; Now try NaN.  Currently we can only test if there's NaN in the
  ;; first two args.  When NaN is the last arg, we return the
  ;; incorrect value because of how multi-compare converts multiple
  ;; args into paris of comparisons.
  ;;
  ;; When that is fixed, we can add additional tests.  Nevertheless,
  ;; this is useful because it tests the not-p case of the vops.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-<3 *single-float-nan* 2f0 3f0))
    (assert-false (stst-<3 1f0 *single-float-nan* 3f0))
    (assert-false (stst-<3 *single-float-nan* *single-float-nan* 3f0))))
  
(define-test nan-double.<3
    (:tag :nan)
  ;; First just make sure it works with regular double-floats
  (assert-true (dtst-<3 1d0 2d0 3d0))
  (assert-false (dtst-<3 1d0 2d0 2d0))
  (assert-false (dtst-<3 1d0 1d0 2d0))
  (assert-false (dtst-<3 1d0 0d0 2d0))
  ;; Now try NaN.  Currently we can only test if there's NaN in the
  ;; first two args.  When NaN is the last arg, we return the
  ;; incorrect value because of how multi-compare converts multiple
  ;; args into paris of comparisons.
  ;;
  ;; When that is fixed, we can add additional tests.  Nevertheless,
  ;; this is useful because it tests the not-p case of the vops.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-<3 *double-float-nan* 2d0 3d0))
    (assert-false (dtst-<3 1d0 *double-float-nan* 3d0))
    (assert-false (dtst-<3 *double-float-nan* *double-float-nan* 3d0))))
  
(define-test nan-single.>3
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst->3 3f0 2f0 1f0))
  (assert-false (stst->3 3f0 1f0 1f0))
  (assert-false (stst->3 2f0 2f0 1f0))
  (assert-false (stst->3 0f0 2f0 1f0))
  ;; Now try NaN.  Currently we can only test if there's NaN in the
  ;; first two args.  When NaN is the last arg, we return the
  ;; incorrect value because of how multi-compare converts multiple
  ;; args into paris of comparisons.
  ;;
  ;; When that is fixed, we can add additional tests.  Nevertheless,
  ;; this is useful because it tests the not-p case of the vops.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst->3 *single-float-nan* 2f0 3f0))
    (assert-false (stst->3 1f0 *single-float-nan* 3f0))
    (assert-false (stst->3 *single-float-nan* *single-float-nan* 3f0))))
  
(define-test nan-double.>3
    (:tag :nan)
  ;; First just make sure it works with regular double-floats
  (assert-true (dtst->3 3d0 2d0 1d0))
  (assert-false (dtst->3 3d0 1d0 1d0))
  (assert-false (dtst->3 2d0 2d0 1d0))
  (assert-false (dtst->3 0d0 2d0 1d0))
  ;; Now try NaN.  Currently we can only test if there's NaN in the
  ;; first two args.  When NaN is the last arg, we return the
  ;; incorrect value because of how multi-compare converts multiple
  ;; args into paris of comparisons.
  ;;
  ;; When that is fixed, we can add additional tests.  Nevertheless,
  ;; this is useful because it tests the not-p case of the vops.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst->3 *double-float-nan* 2d0 3d0))
    (assert-false (dtst->3 1d0 *double-float-nan* 3d0))
    (assert-false (dtst->3 *double-float-nan* *double-float-nan* 3d0))))
  
(define-test nan-single.=
    (:tag :nan)
  ;; Basic tests with regular numbers.
  (assert-true (stst-= 1f0 1f0))
  (assert-false (stst-= 2f0 1f0))
  (assert-false (stst-= 0f0 1f0))
  ;; Tests with NaN, where = should fail.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-= *single-float-nan* 1f0))
    (assert-false (stst-= 1f0 *single-float-nan*))
    (assert-false (stst-= *single-float-nan* *single-float-nan*))))

(define-test nan-double.=
    (:tag :nan)
  ;; Basic tests with regular numbers.
  (assert-true (dtst-= 1d0 1d0))
  (assert-false (dtst-= 2d0 1d0))
  (assert-false (dtst-= 0d0 1d0))
  ;; Tests with NaN, where = should fail.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-= *double-float-nan* 1d0))
    (assert-false (dtst-= 1d0 *double-float-nan*))
    (assert-false (dtst-= *double-float-nan* *double-float-nan*))))
  
(define-test nan-single.=3
    (:tag :nan)
  ;; Basic tests with regular numbers.
  (assert-true (stst-=3 1f0 1f0 1f0))
  (assert-false (stst-=3 1f0 1f0 0f0))
  (assert-false (stst-=3 0f0 1f0 1f0))
  ;; Tests with NaN, where = should fail.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-=3 *single-float-nan* 1f0 1f0))
    (assert-false (stst-=3 1f0 *single-float-nan* 1f0))
    (assert-false (stst-=3 1f0 1f0 *single-float-nan*))))

(define-test nan-double.=3
    (:tag :nan)
  ;; Basic tests with regular numbers.
  (assert-true (dtst-=3 1d0 1d0 1d0))
  (assert-false (dtst-=3 1d0 1d0 0d0))
  (assert-false (dtst-=3 0d0 1d0 1d0))
  ;; Tests with NaN, where = should fail.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-=3 *double-float-nan* 1d0 1d0))
    (assert-false (dtst-=3 1d0 *double-float-nan* 1d0))
    (assert-false (dtst-=3 1d0 1d0 *double-float-nan*))))

;;; Tests for <= and >=, for issue #156.  These used to be compiled as
;;; the negation of > and <, which gets the wrong answer for NaN: a
;;; NaN is unordered with respect to everything, so <, >, <=, and >=
;;; must all be false.

(define-test nan-single.<=
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst-<= 1f0 2f0))
  (assert-true (stst-<= 1f0 1f0))
  (assert-false (stst-<= 1f0 0f0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-<= *single-float-nan* 1f0))
    (assert-false (stst-<= 1f0 *single-float-nan*))
    (assert-false (stst-<= *single-float-nan* *single-float-nan*))))

(define-test nan-double.<=
    (:tag :nan)
  ;; First just make sure it works with regular double-floats
  (assert-true (dtst-<= 1d0 2d0))
  (assert-true (dtst-<= 1d0 1d0))
  (assert-false (dtst-<= 1d0 0d0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-<= *double-float-nan* 1d0))
    (assert-false (dtst-<= 1d0 *double-float-nan*))
    (assert-false (dtst-<= *double-float-nan* *double-float-nan*))))

(define-test nan-single.>=
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst->= 2f0 1f0))
  (assert-true (stst->= 1f0 1f0))
  (assert-false (stst->= 0f0 1f0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst->= *single-float-nan* 1f0))
    (assert-false (stst->= 1f0 *single-float-nan*))
    (assert-false (stst->= *single-float-nan* *single-float-nan*))))

(define-test nan-double.>=
    (:tag :nan)
  ;; First just make sure it works with regular double-floats
  (assert-true (dtst->= 2d0 1d0))
  (assert-true (dtst->= 1d0 1d0))
  (assert-false (dtst->= 0d0 1d0))
  ;; Now try NaN.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst->= *double-float-nan* 1d0))
    (assert-false (dtst->= 1d0 *double-float-nan*))
    (assert-false (dtst->= *double-float-nan* *double-float-nan*))))

(define-test nan-single.<=3
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst-<=3 1f0 2f0 3f0))
  (assert-true (stst-<=3 1f0 2f0 2f0))
  (assert-true (stst-<=3 1f0 1f0 2f0))
  (assert-false (stst-<=3 1f0 0f0 2f0))
  ;; Now try NaN in each position.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst-<=3 *single-float-nan* 2f0 3f0))
    (assert-false (stst-<=3 1f0 *single-float-nan* 3f0))
    (assert-false (stst-<=3 1f0 2f0 *single-float-nan*))
    (assert-false
     (stst-<=3 *single-float-nan* *single-float-nan* *single-float-nan*))))

(define-test nan-double.<=3
    (:tag :nan)
  ;; First just make sure it works with regular double-floats
  (assert-true (dtst-<=3 1d0 2d0 3d0))
  (assert-true (dtst-<=3 1d0 2d0 2d0))
  (assert-true (dtst-<=3 1d0 1d0 2d0))
  (assert-false (dtst-<=3 1d0 0d0 2d0))
  ;; Now try NaN in each position.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst-<=3 *double-float-nan* 2d0 3d0))
    (assert-false (dtst-<=3 1d0 *double-float-nan* 3d0))
    (assert-false (dtst-<=3 1d0 2d0 *double-float-nan*))
    (assert-false
     (dtst-<=3 *double-float-nan* *double-float-nan* *double-float-nan*))))

(define-test nan-single.>=3
    (:tag :nan)
  ;; First just make sure it works with regular single-floats
  (assert-true (stst->=3 3f0 2f0 1f0))
  (assert-true (stst->=3 3f0 1f0 1f0))
  (assert-true (stst->=3 2f0 2f0 1f0))
  (assert-false (stst->=3 0f0 2f0 1f0))
  ;; Now try NaN in each position.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (stst->=3 *single-float-nan* 2f0 1f0))
    (assert-false (stst->=3 3f0 *single-float-nan* 1f0))
    (assert-false (stst->=3 3f0 2f0 *single-float-nan*))
    (assert-false
     (stst->=3 *single-float-nan* *single-float-nan* *single-float-nan*))))

(define-test nan-double.>=3
    (:tag :nan)
  ;; First just make sure it works with regular double-floats
  (assert-true (dtst->=3 3d0 2d0 1d0))
  (assert-true (dtst->=3 3d0 1d0 1d0))
  (assert-true (dtst->=3 2d0 2d0 1d0))
  (assert-false (dtst->=3 0d0 2d0 1d0))
  ;; Now try NaN in each position.  All comparisons should be false.
  (ext:with-float-traps-masked (:invalid)
    (assert-false (dtst->=3 *double-float-nan* 2d0 1d0))
    (assert-false (dtst->=3 3d0 *double-float-nan* 1d0))
    (assert-false (dtst->=3 3d0 2d0 *double-float-nan*))
    (assert-false
     (dtst->=3 *double-float-nan* *double-float-nan* *double-float-nan*))))

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
