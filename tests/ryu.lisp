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

;;; ~E tests, extended coverage.

(define-test format-e.basic
  (:tag :format-e)
  ;; No width, default scale (k=1), default everything.  Mantissa form
  ;; is one digit before the dot.
  (assert-equal "3.14159265358979d+0"
                (lisp::format-e 3.14159265358979d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "-3.14159265358979d+0"
                (lisp::format-e -3.14159265358979d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "1.0d+0"
                (lisp::format-e 1d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "1.0d-1"
                (lisp::format-e 0.1d0 nil nil nil 1 nil nil #\d nil)))

(define-test format-e.d-given-trailing-zeros
  (:tag :format-e)
  ;; When d is given, trailing zeros are emitted to reach exactly d
  ;; fractional digits, since CL ~E asks for exactly d+1 significant
  ;; digits with k=1.
  (assert-equal "5.00000e-1"
                (lisp::format-e 0.5d0 nil 5 nil 1 nil nil #\e nil))
  (assert-equal "1.000d+0"
                (lisp::format-e 1d0 nil 3 nil 1 nil nil #\d nil))
  (assert-equal "3.00000d+0"
                (lisp::format-e 3d0 nil 5 nil 1 nil nil #\d nil)))

(define-test format-e.zero-d
  (:tag :format-e)
  ;; d=0.  CLHS seems to say no digits should follow the decimal
  ;; point, but cmucl has always produced one digit.  Let's keep it
  ;; that way.
  (assert-equal "3.0d+0"
                (lisp::format-e 3.14d0 nil 0 nil 1 nil nil #\d nil))
  (assert-equal "1.0d+1"
                (lisp::format-e 9.9d0 nil 0 nil 1 nil nil #\d nil)))   ; rounds up

(define-test format-e.exponent-padding
  (:tag :format-e)
  ;; e parameter controls minimum exponent width.  Larger exponents
  ;; expand the field if e is too small.
  (assert-equal "3.14d+00"
                (lisp::format-e 3.14d0 nil 2 2 1 nil nil #\d nil))
  (assert-equal "3.14d+000"
                (lisp::format-e 3.14d0 nil 2 3 1 nil nil #\d nil))
  ;; Exponent doesn't fit in e -- emit at natural width (overflow).
  (assert-equal "3.14d+100"
                (lisp::format-e 3.14d100 nil 2 2 1 nil nil #\d nil)))

(define-test format-e.negative-exponent
  (:tag :format-e)
  (assert-equal "5.0d-2"
                (lisp::format-e 0.05d0 nil 1 nil 1 nil nil #\d nil))
  (assert-equal "1.5d-300"
                (lisp::format-e 1.5d-300 nil 1 nil 1 nil nil #\d nil)))

(define-test format-e.k-positive-large
  (:tag :format-e)
  ;; k > 1: more digits before the dot, fewer after.  CLHS: total
  ;; significant digits = d+1 when k > 0.
  (assert-equal "31.416d-1"
                (lisp::format-e 3.14159d0 nil 4 nil 2 nil nil #\d nil))
  (assert-equal "314.16d-2"
                (lisp::format-e 3.14159d0 nil 4 nil 3 nil nil #\d nil)))

(define-test format-e.k-zero
  (:tag :format-e)
  ;; k=0: form is 0.DDDD with d significant digits.
  (assert-equal "0.3142d+1"
                (lisp::format-e 3.14159d0 nil 4 nil 0 nil nil #\d nil))
  (assert-equal "0.5d+1"
                (lisp::format-e 5d0 nil 1 nil 0 nil nil #\d nil)))

(define-test format-e.k-negative
  (:tag :format-e)
  ;; k < 0: 0.0...0DDDD with |k| leading zeros after the dot.
  (assert-equal "0.0001d+5"
                (lisp::format-e 10d0 nil 4 nil -3 nil nil #\d nil))
  (assert-equal "0.0314d+2"
                (lisp::format-e 3.14d0 nil 4 nil -1 nil nil #\d nil)))

(define-test format-e.padding-and-at-sign
  (:tag :format-e)
  ;; Default padchar is space.  @ forces + on the mantissa.
  (assert-equal "    3.14d+0"
                (lisp::format-e 3.14d0 11 2 nil 1 nil nil #\d nil))
  (assert-equal "   +3.14d+0"
                (lisp::format-e 3.14d0 11 2 nil 1 nil nil #\d t))
  ;; Custom padchar.
  (assert-equal "****3.14d+0"
                (lisp::format-e 3.14d0 11 2 nil 1 nil #\* #\d nil))
  ;; Negative value always gets the sign.
  (assert-equal "   -3.14d+0"
                (lisp::format-e -3.14d0 11 2 nil 1 nil nil #\d nil)))

(define-test format-e.exponent-marker
  (:tag :format-e)
  ;; The marker character is whatever the caller passes.  We pass
  ;; explicit markers everywhere; CL chooses 'e' / 'f' / 'd' based on
  ;; *read-default-float-format*, but that's the dispatcher's job, not
  ;; format-e's.
  (assert-equal "3.14e+0"  (lisp::format-e 3.14d0 nil 2 nil 1 nil nil #\e nil))
  (assert-equal "3.14d+0"  (lisp::format-e 3.14d0 nil 2 nil 1 nil nil #\d nil))
  (assert-equal "3.14E+0"  (lisp::format-e 3.14d0 nil 2 nil 1 nil nil #\E nil)))

(define-test format-e.boundary-values
  (:tag :format-e)
  ;; Most positive and most negative doubles.
  (let ((big (lisp::format-e most-positive-double-float nil nil nil 1 nil nil #\d nil)))
    (assert-true (search "d+308" big) big))
  (let ((tiny (lisp::format-e least-positive-double-float nil nil nil 1 nil nil #\d nil)))
    (assert-true (search "d-324" tiny) tiny))
  ;; Smallest positive normal.
  (let ((norm (lisp::format-e least-positive-normalized-double-float
                              nil nil nil 1 nil nil #\d nil)))
    (assert-true (search "d-308" norm) norm)))

(define-test format-e.power-of-two-skew
  (:tag :format-e)
  ;; 2^89 is the article's worked example: shortest is 16 digits
  ;; ending in ...902, not the naive ...901 from fixed-width-17.
  ;; Free-format ~E must emit ...902.
  (assert-equal "6.189700196426902d+26"
                (lisp::format-e (scale-float 1d0 89)
                                nil nil nil 1 nil nil #\d nil)))

(define-test format-e.shrink-rounds-correctly
  (:tag :format-e)
  ;; d=nil with tight w forces shrinking via d2exp.  Verify rounding
  ;; is correct (not just truncation).
  ;; pi to 5 sig digits is "3.1416", not "3.1415".
  (assert-equal "3.1416d+0"
                (lisp::format-e 3.14159265358979d0 9 nil nil 1 nil nil #\d nil))
  ;; pi to 4 sig digits is "3.142".
  (assert-equal "3.142d+0"
                (lisp::format-e 3.14159265358979d0 8 nil nil 1 nil nil #\d nil))
  ;; 9.999... at low precision carries into a new binade.
  (assert-equal "1.00d+1"
                (lisp::format-e 9.999d0 7 2 nil 1 nil nil #\d nil)))

(define-test format-e.width-fits-without-shrink
  (:tag :format-e)
  ;; If w is generous, no shrink happens -- emit shortest.
  (assert-equal " 3.14159265358979d+0"
                (lisp::format-e 3.14159265358979d0 20 nil nil 1 nil nil #\d nil)))

(define-test format-e.overflow-with-overflowchar
  (:tag :format-e)
  ;; w too small AND overflowchar given -- fill the field.
  ;; 3.14d100 at full precision needs ~10 chars; w=3 with overflowchar
  ;; fills.
  (assert-equal "***"
                (lisp::format-e 3.14d100 3 nil nil 1 #\* nil #\d nil))
  ;; w too small even after shrink: same outcome.
  (assert-equal "*****"
                (lisp::format-e 3.14159265358979d0 5 nil nil 1 #\* nil #\d nil)))

(define-test format-e.overflow-no-overflowchar
  (:tag :format-e)
  ;; w too small AND no overflowchar -- emit the field at natural
  ;; width, exceeding w.  No truncation.
  (assert-equal "3.14d+100"
                (lisp::format-e 3.14d100 3 nil nil 1 nil nil #\d nil))
  ;; Free-format too long for w with no overflowchar.
  (assert-equal "3.14159265358979d+0"
                (lisp::format-e 3.14159265358979d0 5 nil nil 1 nil nil #\d nil)))

(define-test format-e.overflow-exponent-too-wide
  (:tag :format-e)
  ;; e is given, but the actual exponent has more digits than e.
  ;; CLHS says this triggers overflow: the field grows to fit the
  ;; exponent, and if overflowchar is given, the field is overflowed.
  (assert-equal "***"
                (lisp::format-e 3.14d100 3 2 2 1 #\* nil #\d nil))
  ;; Without overflowchar, field grows.
  (assert-equal "3.14d+100"
                (lisp::format-e 3.14d100 nil 2 2 1 nil nil #\d nil)))

(define-test format-e.overflow-bumped-exponent
  (:tag :format-e)
  ;; Rounding bumps the exponent into more digits than e allows.
  ;; 9.999d99 at d=2 rounds to "1.00e+100", which has 3 exponent
  ;; digits.  With e=2 given, this is overflow.
  (assert-equal "*******"
                (lisp::format-e 9.999d99 7 2 2 1 #\* nil #\d nil))
  ;; Without overflowchar: field grows by one to accommodate.
  (assert-equal "1.00d+100"
                (lisp::format-e 9.999d99 nil 2 2 1 nil nil #\d nil)))

(define-test format-e.overflow-shrink-exhausts
  (:tag :format-e)
  ;; w forces shrinking down to d_fit = 0, which still won't fit.
  ;; Should produce overflow fill.
  (assert-equal "****"
                (lisp::format-e 3.14159265358979d0 4 nil nil 1 #\* nil #\d nil))
  ;; Same but no overflowchar -- emit at natural shortest width.
  (assert-equal "3.14159265358979d+0"
                (lisp::format-e 3.14159265358979d0 4 nil nil 1 nil nil #\d nil)))

(define-test format-e.overflow-with-padchar
  (:tag :format-e)
  ;; padchar doesn't affect overflow detection -- it only matters when
  ;; the field fits.
  (assert-equal "PPPP3.14d+0"
                (lisp::format-e 3.14d0 11 2 nil 1 #\* #\P #\d nil))
  (assert-equal "***"
                (lisp::format-e 3.14d100 3 nil nil 1 #\* #\P #\d nil)))

(define-test format-e.large-exponent
  (:tag :format-e)
  ;; e is given but the actual exponent has more digits.  CL ~E
  ;; lets the field grow to accommodate (no truncation of the
  ;; exponent digits).
  (assert-equal "3.14d+250"
                (lisp::format-e 3.14d250 nil 2 2 1 nil nil #\d nil)))

(define-test format-e.tiny-d-and-shortest
  (:tag :format-e)
  ;; Values whose shortest representation is just 1 digit, with d=nil.
  ;; Must force ".0" since CL ~E always shows the dot.
  (assert-equal "1.0d+0"
                (lisp::format-e 1d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "2.0d+0"
                (lisp::format-e 2d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "5.0d-1"
                (lisp::format-e 0.5d0 nil nil nil 1 nil nil #\d nil)))

(define-test format-e.signed-zero
  (:tag :format-e)
  ;; -0.0 preserves its sign through ~E.  This is the IEEE-754 default
  ;; and what SBCL/CMUCL/CCL all do.
  (assert-equal "-0.0d+0"
                (lisp::format-e (- 0d0) nil nil nil 1 nil nil #\d nil))
  (assert-equal "-0.00d+0"
                (lisp::format-e (- 0d0) nil 2 nil 1 nil nil #\d nil))
  ;; @ modifier emits the existing minus, not a plus.
  (assert-equal "-0.0d+0"
                (lisp::format-e (- 0d0) nil nil nil 1 nil nil #\d t))
  ;; +0.0 with @ modifier emits an explicit "+".
  (assert-equal "+0.0d+0"
                (lisp::format-e 0d0 nil nil nil 1 nil nil #\d t))
  (assert-equal "+0.00d+0"
                (lisp::format-e 0d0 nil 2 nil 1 nil nil #\d t))
  ;; Without @, no sign on positive zero.
  (assert-equal "0.0d+0"
                (lisp::format-e 0d0 nil nil nil 1 nil nil #\d nil)))

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
  (assert-equal "9.9"
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
  (assert-equal "3.14159265"
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

;; CLHS doesn't say what rounding to do.  We leave these here as a
;; record of what we do with ~F using Ryu.
(define-test format-f.exact-half
  (:tag :format-f)
  ;; Round half to even: d2fixed's rounding mode.
  (assert-equal "0."
                (lisp::format-f  0.5d0      nil 0 0 nil nil nil))
  (assert-equal "2."
                (lisp::format-f  2.5d0      nil 0 0 nil nil nil))
  (assert-equal "2."
                (lisp::format-f  1.5d0      nil 0 0 nil nil nil)))


(define-test format-f.overflow-no-overflowchar
  (:tag :format-f)
  ;; When the field overflows w and no overflowchar is given, CLHS says
  ;; only "the number is printed using as many positions as necessary"
  ;; (CLHS 22.3.3.1) -- it does not specify whether digits should be
  ;; rounded to fit.
  ;;
  ;; The Burger & Dybvig implementation says:
  ;; (format nil "~8,f" 123456789d0) => "123456800."
  (assert-equal "123456789.0"
                (lisp::format-f 123456789d0  8 nil 0 nil nil nil))
  ;; The Burger & Dybvig implementation says:
  ;; (format nil "~8,f" -123456789d0) => "-123457000."
  (assert-equal "-123456789.0"
                (lisp::format-f -123456789d0 8 nil 0 nil nil nil)))

(define-test format-f.signed-zero
  (:tag :format-f)
  (assert-equal "-0.0"
                (lisp::format-f (- 0d0) nil nil 0 nil nil nil))
  (assert-equal "-0.00"
                (lisp::format-f (- 0d0) nil 2 0 nil nil nil))
  (assert-equal "-0."
                (lisp::format-f (- 0d0) nil 0 0 nil nil nil))
  ;; @ doesn't override the existing minus.
  (assert-equal "-0.0"
                (lisp::format-f (- 0d0) nil nil 0 nil nil t))
  (assert-equal "+0.0"
                (lisp::format-f 0d0 nil nil 0 nil nil t))
  (assert-equal "+0.00"
                (lisp::format-f 0d0 nil 2 0 nil nil t))
  (assert-equal "+0."
                (lisp::format-f 0d0 nil 0 0 nil nil t))
  ;; Without @, no sign on positive zero.
  (assert-equal "0.0"
                (lisp::format-f 0d0 nil nil 0 nil nil nil)))

