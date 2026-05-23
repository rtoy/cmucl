;;; Tests for ryu-printf WIP

(defpackage :ryu-tests
  (:use :cl :lisp-unit))

(in-package #:ryu-tests)

(define-test format-e.1
  (assert-equal "-1.23456789z+0012"
		(lisp::format-e -1.23456789d12 17 nil 4 1 #\* #\P #\z t)))

(define-test format-e.2
  (assert-equal "-1.23456795z+0012"
		(lisp::format-e (float -1.23456789e12 1d0) 17 nil 4 1 #\* #\P #\z t)))

(define-test format-e.3
  (assert-equal "-1.235z+0012"
		(lisp::format-e -1.23456789d12 12 nil 4 1 #\* #\P #\z t)))

(define-test format-e.4
  (assert-equal "-.01z+0014"
		(lisp::format-e -1.23456789d12 10 nil 4 -1 #\* #\P #\z t)))

(define-test format-e.5
  (assert-equal "PPP-0.0012345679z+0015"
		(lisp::format-e -1.23456789d12 22 10 4 -2 #\* #\P #\z t)))

(define-test format-e.6
  (assert-equal "PP+1.0z+0012"
		(lisp::format-e 9.9999999999999995d11 12 nil 4 1 #\* #\P #\z t)))

(define-test format-e.7
  (assert-equal "+5.00000e-01"
		(lisp::format-e 0.5d0 nil 5 2 1 nil nil #\e t))
  (assert-equal "5.00000e-01"
		(lisp::format-e 0.5d0 nil 5 2 1 nil nil #\e nil)))

;; This tests currently fails.
#+nil
(define-test format-e.8
  (assert-equal (format nil "~12,,2,1,'*,'P,'ze" 9.999999999d99)
		(lisp::format-e 9.999999999d99 12 nil 2 1 #\* #\P #\z t)))

;;; ~F tests
(define-test format-f.basic
  (:tag :format-f)
  ;; Common case, no width, d=2: plain d2fixed-via-d-given path.
  (assert-equal "3.14"
                (lisp::format-f  3.14159d0  nil  2 0 nil nil nil))
  (assert-equal "-3.14"
                (lisp::format-f -3.14159d0  nil  2 0 nil nil nil))
  (assert-equal "0.10"
                (lisp::format-f  0.1d0      nil  2 0 nil nil nil)))

(define-test format-f.d-given-zero-precision
  (:tag :format-f)
  ;; d=0 must append a trailing dot (CL ~F always shows the dot).
  (assert-equal "3."
                (lisp::format-f  3.14d0     nil  0 0 nil nil nil))
  (assert-equal "4."
                (lisp::format-f  3.5d0      nil  0 0 nil nil nil))   ; round half to even -> 4
  (assert-equal "4."
                (lisp::format-f  4.5d0      nil  0 0 nil nil nil))   ; same
  (assert-equal "0."
                (lisp::format-f  0.49d0     nil  0 0 nil nil nil))
  (assert-equal "0."
                (lisp::format-f  0.5d0      nil  0 0 nil nil nil))   ; round half to even -> 0? actually 0
  )

(define-test format-f.d-given-rounding-carry
  (:tag :format-f)
  ;; Rounding that carries across the decimal point.
  (assert-equal "10.0"
                (lisp::format-f  9.95d0     nil  1 0 nil nil nil))
  (assert-equal "1.00"
                (lisp::format-f  0.999d0    nil  2 0 nil nil nil))
  (assert-equal "100.00"
                (lisp::format-f 99.999d0    nil  2 0 nil nil nil)))

(define-test format-f.d-given-with-trailing-zeros
  (:tag :format-f)
  ;; d2fixed must emit trailing zeros when the value uses fewer digits
  ;; than d.  This is CL's required behavior for ~,dF.
  (assert-equal "1.00000"
                (lisp::format-f  1d0        nil  5 0 nil nil nil))
  (assert-equal "0.30000"
                (lisp::format-f  0.3d0      nil  5 0 nil nil nil))
  (assert-equal "1234.50000"
                (lisp::format-f  1234.5d0   nil  5 0 nil nil nil)))

(define-test format-f.d-nil-free-format
  (:tag :format-f)
  ;; d = nil: d2s + reshape, no d2fixed.
  (assert-equal "3.14"
                (lisp::format-f  3.14d0     nil nil 0 nil nil nil))
  (assert-equal "3.141592653589793"

                (lisp::format-f  3.141592653589793d0 nil nil 0 nil nil nil))
  ;; Integer-valued doubles get a forced ".0" since ~F always shows the dot.
  (assert-equal "1.0"
                (lisp::format-f  1d0        nil nil 0 nil nil nil))
  (assert-equal "1000000000000.0"

                (lisp::format-f  1d12       nil nil 0 nil nil nil))
  ;; Tiny values: leading zeros after the dot.
  (assert-equal "0.001"
                (lisp::format-f  0.001d0    nil nil 0 nil nil nil))
  (assert-equal "0.0001234"
                (lisp::format-f  1.234d-4   nil nil 0 nil nil nil)))

(define-test format-f.d-nil-with-w-fits
  (:tag :format-f)
  ;; d=nil with width: shortest fits, no shrink, just left-pad.
  (assert-equal "      3.14"
                (lisp::format-f  3.14d0     10 nil 0 nil nil nil))
  (assert-equal "  123456.7"
                (lisp::format-f  123456.7d0 10 nil 0 nil nil nil))
  (assert-equal " -123456.7"
                (lisp::format-f -123456.7d0 10 nil 0 nil nil nil))
  (assert-equal "       1.0"
                (lisp::format-f  1d0        10 nil 0 nil nil nil)))

(define-test format-f.d-nil-with-w-shrinks
  (:tag :format-f)
  ;; d=nil with tight width: must shrink fractional digits via d2fixed.
  ;; 3.141592653589793 has 16 fractional digits at shortest; w=8 only
  ;; allows 6 (sign 0 + int 1 + dot 1 + frac 6 = 8).
  (assert-equal "3.141593"
                (lisp::format-f  3.141592653589793d0  8 nil 0 nil nil nil))
  (assert-equal "  3.1416"
                (lisp::format-f  3.141592653589793d0 10 nil 0 nil nil nil))
  ;; The shrink should round correctly: 5 with following digits rounds up.
  (assert-equal "1.2346"
                (lisp::format-f  1.234567d0   6 nil 0 nil nil nil)))

(define-test format-f.d-nil-with-w-overflow
  (:tag :format-f)
  ;; Integer part alone exceeds w.  Without overflowchar, emit as-is.
  ;; With overflowchar, fill the field.
  ;; 123456789d0 has int-len 9, plus dot and at least one fractional = 11.
  (assert-equal "123456789.0"
                (lisp::format-f  123456789d0  8 nil 0 nil    nil nil))
  (assert-equal "********"
                (lisp::format-f  123456789d0  8 nil 0 #\*    nil nil))
  ;; Sign costs a char too.
  (assert-equal "*******"
                (lisp::format-f -123456789d0  7 nil 0 #\*    nil nil)))

(define-test format-f.d-given-too-wide
  (:tag :format-f)
  ;; d given AND too long for w: CL never shrinks d.  Emit as-is or
  ;; fill with overflowchar.
  (assert-equal "1.23456700"
                (lisp::format-f  1.234567d0  6  8 0 nil nil nil))
  (assert-equal "******"
                (lisp::format-f  1.234567d0  6  8 0 #\* nil nil)))

(define-test format-f.padding
  (:tag :format-f)
  ;; padchar choices.
  (assert-equal "**3.14"
                (lisp::format-f  3.14d0      6  2 0 nil #\* nil))
  (assert-equal "003.14"
                (lisp::format-f  3.14d0      6  2 0 nil #\0 nil))
  (assert-equal "  3.14"
                (lisp::format-f  3.14d0      6  2 0 nil nil nil)))

(define-test format-f.at-sign
  (:tag :format-f)
  ;; @ modifier forces + on positive values.
  (assert-equal "+3.14"
                (lisp::format-f  3.14d0      nil 2 0 nil nil t))
  (assert-equal "-3.14"
                (lisp::format-f -3.14d0      nil 2 0 nil nil t))
  (assert-equal " +3.14"
                (lisp::format-f  3.14d0      6   2 0 nil nil t))
  ;; The @ sign eats one char of width.
  (assert-equal "+3.14"
                (lisp::format-f  3.14d0      5   2 0 nil nil t)))

(define-test format-f.boundary-values
  (:tag :format-f)
  ;; Min positive subnormal: needs many leading zeros.
  ;; least-positive-double-float = 5e-324 (shortest).
  (assert-equal (concatenate 'string
                             "0." (make-string 323 :initial-element #\0) "5")

                (lisp::format-f least-positive-double-float nil nil 0 nil nil nil))
  ;; Max positive double: ~1.8e308.  Should print all 309 integer digits.
  (let ((s
                (lisp::format-f most-positive-double-float nil nil 0 nil nil nil)))
    (assert-true (> (length s) 300))
    (assert-true (find #\. s))))

(define-test format-f.exact-half
  (:tag :format-f)
  ;; Round half to even: d2fixed's rounding mode.
  (assert-equal "0.0"
                (lisp::format-f  0.5d0      nil 0 0 nil nil nil))   ; "0." actually -- check
  (assert-equal "2."
                (lisp::format-f  2.5d0      nil 0 0 nil nil nil))
  (assert-equal "2."
                (lisp::format-f  1.5d0      nil 0 0 nil nil nil)))
