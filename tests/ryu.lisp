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
  ;; w too small AND no overflowchar -- emit the no-fractional-digit
  ;; form "d.e+exp" (overflowing W), per CLHS 22.3.3.2 "single zero
  ;; digit ... if the width w permits" (here it does not).
  (assert-equal "3.d+100"
                (lisp::format-e 3.14d100 3 nil nil 1 nil nil #\d nil))
  ;; Free-format too long for w with no overflowchar -- same rule:
  ;; drop to no-fractional-digit form rather than emitting the full
  ;; multi-digit shortest representation.
  (assert-equal "3.d+0"
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
  ;; w forces shrinking down to d_fit = nil (not even d=0 fits).
  ;; With an overflow char, the field is filled.
  (assert-equal "****"
                (lisp::format-e 3.14159265358979d0 4 nil nil 1 #\* nil #\d nil))
  ;; Without overflowchar, emit the no-fractional-digit form
  ;; "d.e+exp" (overflowing W) rather than the full multi-digit
  ;; shortest representation.
  (assert-equal "3.d+0"
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

(define-test format-e.tight-width-no-fraction
  (:tag :format-e)
  ;; ANSI FORMAT.E.4/.5/.7/.8/.9: ~Ne with N too small for the
  ;; "d.0e+0" shortest form drops the fractional digit instead of
  ;; forcing the ".0", giving "d.e+0" (overflowing W when even that
  ;; form is wider).  CLHS 22.3.3.2 "single zero digit ... if the
  ;; width w permits" -- here it does not.
  (assert-equal "1.e+0"
                (lisp::format-e 1.0f0 5 nil nil 1 nil nil #\e nil))
  (assert-equal "1.e+0"
                (lisp::format-e 1.0f0 4 nil nil 1 nil nil #\e nil))
  (assert-equal "+1.e+0"
                (lisp::format-e 1.0f0 6 nil nil 1 nil nil #\e t))
  (assert-equal "+1.e+0"
                (lisp::format-e 1.0f0 5 nil nil 1 nil nil #\e t))
  (assert-equal "-1.e+0"
                (lisp::format-e -1.0f0 6 nil nil 1 nil nil #\e nil)))

(define-test format-e.d-given-k-consumes-fraction
  (:tag :format-e)
  ;; ANSI FORMAT.E.19: with D specified, the number of fractional
  ;; digits is exactly d-(k-1).  When k >= d+1 (and k >= 2) that count
  ;; is 0, and the output is "[int].e[exp]" with no forced ".0".
  ;; d=2, k=3, value 0.05 -> "5.00E-2" rounded by d2exp -> "500.e-4"
  ;; after the k-shift consumes the fractional digits.
  (assert-equal "500.e-4"
                (lisp::format-e 0.05f0 nil 2 nil 3 nil nil #\e nil))
  (assert-equal "500.e-4"
                (lisp::format-e 0.05d0 nil 2 nil 3 nil nil #\e nil))
  ;; Negated: leading "-" sign included.
  (assert-equal "-500.e-4"
                (lisp::format-e -0.05d0 nil 2 nil 3 nil nil #\e nil))
  ;; At-sign: explicit "+".
  (assert-equal "+500.e-4"
                (lisp::format-e 0.05d0 nil 2 nil 3 nil nil #\e t)))

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
  ;; d=nil with a tight width: CL rounds the value to the largest
  ;; number of fractional digits that fits in W (matching the B&D
  ;; reference output).  3.141592653589793 shrinks to fit; 1.234567
  ;; with w=6 rounds to "1.2346".
  (assert-equal "1.2346"
                (lisp::format-f  1.234567d0   6 nil 0 nil nil nil))
  (assert-equal "3.14159265"
                (lisp::format-f  3.141592653589793d0 10 nil 0 nil nil nil))
  (assert-equal "3.141593"
                (lisp::format-f  3.141592653589793d0  8 nil 0 nil nil nil)))

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

;;; ~G tests

(define-test format-g.basic
  (:tag :format-g)
  ;; Values that fit the ~F-like range (n in [1, 7]).  Should produce
  ;; ~F-form output with trailing spaces.  Default ee = 4.
  (assert-equal "3.14    "
                (lisp::format-g 3.14d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "10.    "
                (lisp::format-g 10d0 nil nil nil 1 nil nil #\d nil))
  (assert-equal "1000.    "
                (lisp::format-g 1000d0 nil nil nil 1 nil nil #\d nil)))

(define-test format-g.zero
  (:tag :format-g)
  ;; Zero falls into the ~F path with the d-from-d2s shortcut.
  ;; CMUCL emits "0." (d=0 form), then 4 trailing spaces.
  (assert-equal "0.    "
                (lisp::format-g 0d0 nil nil nil 1 nil nil #\d nil))
  ;; Negative zero preserves the sign.
  (assert-equal "-0.    "
                (lisp::format-g (- 0d0) nil nil nil 1 nil nil #\d nil)))

(define-test format-g.large-uses-e
  (:tag :format-g)
  ;; Values with n > 7 should fall into ~E form (no trailing spaces).
  ;; Per CLHS 22.3.3.3, when D is unspecified ~G's ~E call also has
  ;; D unspecified, so ~E uses free-format (shortest) output rather
  ;; than fixed precision.
  (assert-equal "1.0d+10"
                (lisp::format-g 1d10 nil nil nil 1 nil nil #\d nil))
  (assert-equal "1.0d+100"
                (lisp::format-g 1d100 nil nil nil 1 nil nil #\d nil)))

(define-test format-g.small-uses-e
  (:tag :format-g)
  ;; Very small values: n is negative or zero.  effective-d = max(q, n)
  ;; with n <= 0 means effective-d = q, dd = q - n which is > q for
  ;; negative n.  Falls into ~E.
  (assert-equal "1.0d-5"
                (lisp::format-g 1d-5 nil nil nil 1 nil nil #\d nil))
  (assert-equal "1.0d-300"
                (lisp::format-g 1d-300 nil nil nil 1 nil nil #\d nil)))

(define-test format-g.d-given-f-form
  (:tag :format-g)
  ;; d given, value in ~F range.
  ;; pi to d=5 sig digits: ~F form with dd = 5-1 = 4 fractional.
  (assert-equal "3.1416    "
                (lisp::format-g 3.14159265d0 nil 5 nil 1 nil nil #\d nil))
  ;; d=2 for 3.14: dd = 2-1 = 1 -> "3.1" + 4 spaces.
  (assert-equal "3.1    "
                (lisp::format-g 3.14d0 nil 2 nil 1 nil nil #\d nil)))

(define-test format-g.d-given-e-form
  (:tag :format-g)
  ;; d given, value in ~E range.
  ;; 1d10 with d=4: dd = 4-11 = -7 -> ~E.
  (assert-equal "1.0000d+10"
                (lisp::format-g 1d10 nil 4 nil 1 nil nil #\d nil)))

(define-test format-g.width-fits
  (:tag :format-g)
  ;; w is generous enough that no shrinking is needed.
  ;; ~F path: format-f gets ww = w - ee.
  (assert-equal "      3.14    "
                (lisp::format-g 3.14d0 14 nil nil 1 nil nil #\d nil)))

(define-test format-g.e-parameter
  (:tag :format-g)
  ;; e parameter controls exponent width in the ~E branch and ee in
  ;; the ~F branch (ee = e + 2).
  ;; 1d10 with e=3: ~E with 3-digit exponent.  D is unspecified so
  ;; ~E uses free-format, giving "1.0d+010".
  (assert-equal "1.0d+010"
                (lisp::format-g 1d10 nil nil 3 1 nil nil #\d nil))
  ;; 3.14 with e=3: ~F branch, ee = 5 trailing spaces.
  (assert-equal "3.14     "
                (lisp::format-g 3.14d0 nil nil 3 1 nil nil #\d nil)))

(define-test format-g.at-sign
  (:tag :format-g)
  ;; @ modifier forces + on positive values, passed through to ~F or ~E.
  (assert-equal "+3.14    "
                (lisp::format-g 3.14d0 nil nil nil 1 nil nil #\d t))
  (assert-equal "-3.14    "
                (lisp::format-g -3.14d0 nil nil nil 1 nil nil #\d t))
  (assert-equal "+1.0d+10"
                (lisp::format-g 1d10 nil nil nil 1 nil nil #\d t)))

(define-test format-g.boundary-n-7
  (:tag :format-g)
  ;; At n=7 the value is still in the ~F range.
  ;; 1d6 has n=7, q=1, effective-d = max(1, 7) = 7, dd = 7-7 = 0.
  ;; dd = 0 is in [0, d] so ~F path with d=0.  Output: "1000000." + 4 sp.
  (assert-equal "1000000.    "
                (lisp::format-g 1d6 nil nil nil 1 nil nil #\d nil))
  ;; 1d7 has n=8, dd = 7-8 = -1, falls into ~E.  D is unspecified so
  ;; ~E uses free-format.
  (assert-equal "1.0d+7"
                (lisp::format-g 1d7 nil nil nil 1 nil nil #\d nil)))

(define-test format-g.d-nil-routes-to-e-with-d-nil
  (:tag :format-g)
  ;; CLHS 22.3.3.3 specifies that when ~G falls through to the ~E
  ;; branch, the ~E directive is called with the user's original D
  ;; (per "~w,d,e,k,...,E"), NOT the EFFECTIVE-D that ~G computed for
  ;; the ~F-vs-~E decision.  This makes a visible difference when D
  ;; was originally NIL: ~E then produces its free-format (shortest)
  ;; output rather than the longer fixed-precision form.
  ;; 1.5d-10 has n=-9, q=2, effective-d=2, dd=11 -> ~E with D=nil.
  (assert-equal "1.5d-10"
                (lisp::format-g 1.5d-10 nil nil nil 1 nil nil #\d nil))
  ;; If we passed effective-d=2 to ~E we'd get "1.50d-10" instead.
  (assert-false (equal "1.50d-10"
                       (lisp::format-g 1.5d-10 nil nil nil 1 nil nil #\d nil))))

(define-test format-g.padchar
  (:tag :format-g)
  ;; padchar passed through to whichever sub-formatter fires.
  (assert-equal "PPPPPP3.14    "
                (lisp::format-g 3.14d0 14 nil nil 1 nil #\P #\d nil)))

(define-test format-g.overflow-tight-w
  (:tag :format-g)
  ;; CLHS-strict reading: ~F sub-form emits at natural width when ww
  ;; would be negative; trailing ee chars are spaces, not overflowchar.
  ;; CMUCL extends this with overflowchar in the trailing chars; we
  ;; follow CLHS literally.
  (assert-equal "3.14    "
                (lisp::format-g 3.14d0 3 nil nil 1 #\* nil #\d nil)))


;;;; Single-float tests for format-e, format-f, format-g.
;;;;
;;;; The shortest-form (d-nil) paths use f2s and may produce different
;;;; digits than d2s would for the corresponding double-float value,
;;;; since the shortest round-trip representation depends on float
;;;; precision.  The d-given paths widen to double and use d2fixed/
;;;; d2exp; results match the double-float case at that precision.

(define-test format-e.single-basic
  (:tag :format-e :single-float)
  ;; Default ~E exponent marker for single-floats is 'f' (matches
  ;; CL's single-float-format default).
  (assert-equal "3.1415927f+0"
                (lisp::format-e 3.1415927f0 nil nil nil 1 nil nil #\f nil))
  (assert-equal "-3.1415927f+0"
                (lisp::format-e -3.1415927f0 nil nil nil 1 nil nil #\f nil))
  (assert-equal "1.0f+0"
                (lisp::format-e 1f0 nil nil nil 1 nil nil #\f nil))
  (assert-equal "1.0f-1"
                (lisp::format-e 0.1f0 nil nil nil 1 nil nil #\f nil)))

(define-test format-e.single-d-given
  (:tag :format-e :single-float)
  ;; d-given path: widens to double, calls d2exp.  At precision 5,
  ;; matches what d2exp produces for the exact double representation
  ;; of the single value.
  (assert-equal "3.14159f+0"
                (lisp::format-e 3.1415927f0 nil 5 nil 1 nil nil #\f nil))
  (assert-equal "5.00000f-1"
                (lisp::format-e 0.5f0 nil 5 nil 1 nil nil #\f nil)))

(define-test format-e.single-shortest-differs-from-double
  (:tag :format-e :single-float)
  ;; The single-float path uses f2s, which gives the shortest form
  ;; for the single-precision value.  Widening to double first would
  ;; produce a different (longer) shortest form because the widened
  ;; value 0.10000000149011612d0 is not the same as 0.1d0.
  (assert-equal "1.0f-1"
                (lisp::format-e 0.1f0 nil nil nil 1 nil nil #\f nil))
  ;; Confirm: if you widened first, d2s on the result would give:
  (assert-equal "1.0000000149011612f-1"
                (lisp::format-e 0.10000000149011612d0 nil nil nil 1 nil nil #\f nil)))


(define-test format-e.single-boundary
  (:tag :format-e :single-float)
  ;; Single-float bounds.
  (let ((maxs (lisp::format-e most-positive-single-float
                              nil nil nil 1 nil nil #\f nil)))
    (assert-true (search "f+38" maxs) maxs))
  (let ((mins (lisp::format-e least-positive-single-float
                              nil nil nil 1 nil nil #\f nil)))
    (assert-true (search "f-45" mins) mins)))

(define-test format-e.single-negative-zero
  (:tag :format-e :single-float)
  (assert-equal "-0.0f+0"
                (lisp::format-e (- 0f0) nil nil nil 1 nil nil #\f nil))
  (assert-equal "+0.0f+0"
                (lisp::format-e 0f0 nil nil nil 1 nil nil #\f t)))

;;; ----------------------------------------------------------------------
;;; ~F single-float

(define-test format-f.single-basic
  (:tag :format-f :single-float)
  (assert-equal "3.14"
                (lisp::format-f 3.14159f0 nil 2 0 nil nil nil))
  (assert-equal "-3.14"
                (lisp::format-f -3.14159f0 nil 2 0 nil nil nil))
  (assert-equal "1.00000"
                (lisp::format-f 1f0 nil 5 0 nil nil nil)))

(define-test format-f.single-d-nil
  (:tag :format-f :single-float)
  ;; Shortest path: f2s output for single-floats is shorter than
  ;; d2s would be for the widened value.
  (assert-equal "3.1415927"
                (lisp::format-f 3.1415927f0 nil nil 0 nil nil nil))
  (assert-equal "1.0"
                (lisp::format-f 1f0 nil nil 0 nil nil nil))
  (assert-equal "0.1"
                (lisp::format-f 0.1f0 nil nil 0 nil nil nil))
  (assert-equal "0.5"
                (lisp::format-f 0.5f0 nil nil 0 nil nil nil)))

(define-test format-f.single-d-nil-vs-double
  (:tag :format-f :single-float)
  ;; Confirm that the single-float path uses f2s, not d2s on a
  ;; widened value.  For 0.1f0, f2s gives "1" at exp -1, so output
  ;; is "0.1".  If we widened first, d2s on the resulting double
  ;; would give a longer shortest form.
  (assert-equal "0.1"
                (lisp::format-f 0.1f0 nil nil 0 nil nil nil))
  ;; Same numeric value, but as a double-float literal -- d2s gives
  ;; the long shortest form that round-trips through double.
  (assert-equal "0.10000000149011612"
                (lisp::format-f 0.10000000149011612d0 nil nil 0 nil nil nil))
  ;; And 0.1d0 (the double literal nearest to 0.1) has its own
  ;; shortest form, which IS "0.1" since 0.1d0 is the canonical
  ;; double for that decimal.
  (assert-equal "0.1"
                (lisp::format-f 0.1d0 nil nil 0 nil nil nil)))


(define-test format-f.single-leading-zeros
  (:tag :format-f :single-float)
  ;; Small singles still need leading-zero handling.
  (assert-equal "0.001"
                (lisp::format-f 0.001f0 nil nil 0 nil nil nil))
  (assert-equal "0.00001"
                (lisp::format-f 0.00001f0 nil nil 0 nil nil nil)))

(define-test format-f.single-integer-valued
  (:tag :format-f :single-float)
  ;; Integer-valued singles get ".0" forced.
  (assert-equal "1.0"
                (lisp::format-f 1f0 nil nil 0 nil nil nil))
  (assert-equal "100.0"
                (lisp::format-f 100f0 nil nil 0 nil nil nil))
  (assert-equal "1000000.0"
                (lisp::format-f 1000000f0 nil nil 0 nil nil nil)))

(define-test format-f.single-width-fits
  (:tag :format-f :single-float)
  (assert-equal "      3.14"
                (lisp::format-f 3.14f0 10 nil 0 nil nil nil)))

(define-test format-f.single-narrow-width-no-overflow-char
  (:tag :format-f :single-float)
  ;; ~F with unspecified d and a width too small for the shortest form:
  ;; the value is rounded to the fractional digits that fit, but at
  ;; least one fractional digit is always shown (CLHS 22.3.3.1).  With
  ;; no overflow char the field expands when even that minimum
  ;; doesn't fit.  1.0 -> "1.0".
  (assert-equal "1.0"
                (lisp::format-f 1.0f0 2 nil 0 nil nil nil))
  (assert-equal "1.0"
                (lisp::format-f 1.0f0 1 nil 0 nil nil nil))
  ;; Same for double-floats.
  (assert-equal "1.0"
                (lisp::format-f 1.0d0 2 nil 0 nil nil nil)))

(define-test format-f.d-nil-rounds-to-width-with-forced-zero
  (:tag :format-f)
  ;; ANSI FORMAT.F.45/F.46/F.47: ~F with unspecified d rounds the
  ;; value to the fractional digits that fit in W, always shows at
  ;; least one fractional digit (a single "0" if the fraction rounds
  ;; away), and drops the leading "0." -> "." only when keeping it
  ;; would overflow W.
  ;; ~2f of 1.1 and 1.9 round to integers, forced ".0", overflow to 3
  ;; (FORMAT.F.45).
  (assert-equal "1.0" (lisp::format-f 1.1f0 2 nil 0 nil nil nil))
  (assert-equal "2.0" (lisp::format-f 1.9f0 2 nil 0 nil nil nil))
  ;; ~3f of 1e-6: "0.0" fits w=3 exactly, leading zero kept
  ;; (FORMAT.F.47).
  (assert-equal "0.0" (lisp::format-f 1.0f-6 3 nil 0 nil nil nil))
  ;; ~4f of 1e-6: "0.00" fits w=4 exactly.
  (assert-equal "0.00" (lisp::format-f 1.0f-6 4 nil 0 nil nil nil))
  ;; ~2f, ~1f, ~0f of 0.01: leading zero dropped to ".0" (FORMAT.F.46).
  (assert-equal ".0" (lisp::format-f 0.01f0 2 nil 0 nil nil nil))
  (assert-equal ".0" (lisp::format-f 0.01f0 1 nil 0 nil nil nil))
  (assert-equal ".0" (lisp::format-f 0.01f0 0 nil 0 nil nil nil)))

(define-test format-f.single-narrow-width-with-overflow-char
  (:tag :format-f :single-float)
  ;; With overflowchar supplied, the field is filled with the overflow
  ;; character when the shortest form does not fit.
  (assert-equal "**"
                (lisp::format-f 1.0f0 2 nil 0 #\* nil nil))
  (assert-equal "***"
                (lisp::format-f 1234.5f0 3 nil 0 #\* nil nil)))

(define-test format-f.optional-leading-zero
  (:tag :format-f :single-float :double-float)
  ;; CLHS 22.3.3.1: "a zero is printed before the decimal point if
  ;; there is room".  When the field is too narrow to fit the leading
  ;; zero, it is dropped, and the output starts with the decimal point.
  ;; ANSI tests FORMAT.F.13-16.
  ;;
  ;; ~3,2F of 0.5 -> ".50"
  (assert-equal ".50" (lisp::format-f 0.5f0 3 2 0 nil nil nil))
  (assert-equal ".50" (lisp::format-f 0.5d0 3 2 0 nil nil nil))
  ;; ~2,1F of 0.5 -> ".5"
  (assert-equal ".5"  (lisp::format-f 0.5f0 2 1 0 nil nil nil))
  (assert-equal ".5"  (lisp::format-f 0.5d0 2 1 0 nil nil nil))
  ;; ~4,2@F of 0.5 -> "+.50"
  (assert-equal "+.50" (lisp::format-f 0.5f0 4 2 0 nil nil t))
  (assert-equal "+.50" (lisp::format-f 0.5d0 4 2 0 nil nil t))
  ;; ~2,2F of 0.5 -> ".50" (field expands beyond w=2 since no overflowchar)
  (assert-equal ".50" (lisp::format-f 0.5f0 2 2 0 nil nil nil))
  (assert-equal ".50" (lisp::format-f 0.5d0 2 2 0 nil nil nil))
  ;; Negative values: ~3,2F of -0.5 -> "-.50" (field expands past w=3).
  (assert-equal "-.50" (lisp::format-f -0.5d0 3 2 0 nil nil nil))
  ;; -0.5 in width 4: drops the zero, fits exactly.
  (assert-equal "-.50" (lisp::format-f -0.5d0 4 2 0 nil nil nil)))

(define-test format-f.leading-zero-kept-when-room
  (:tag :format-f :single-float :double-float)
  ;; When there is room for the leading zero, it must be printed.
  (assert-equal " 0.50" (lisp::format-f 0.5d0 5 2 0 nil nil nil))
  (assert-equal "0.50"  (lisp::format-f 0.5d0 4 2 0 nil nil nil))
  (assert-equal "0.50"  (lisp::format-f 0.5d0 nil 2 0 nil nil nil)))

(define-test format-f.leading-zero-required-for-exact-zero
  (:tag :format-f :single-float :double-float)
  ;; CLHS 22.3.3.1: "If the magnitude is exactly zero, a single zero
  ;; digit is printed before the decimal point."  Don't drop it even
  ;; if the field would overflow.
  (assert-equal "0.00" (lisp::format-f 0.0d0 3 2 0 nil nil nil))
  (assert-equal "0.00" (lisp::format-f 0.0f0 3 2 0 nil nil nil)))

(define-test format-f.single-negative-zero
  (:tag :format-f :single-float)
  (assert-equal "-0.0"
                (lisp::format-f (- 0f0) nil nil 0 nil nil nil))
  (assert-equal "+0.0"
                (lisp::format-f 0f0 nil nil 0 nil nil t)))

;;; ----------------------------------------------------------------------
;;; ~G single-float

(define-test format-g.single-basic
  (:tag :format-g :single-float)
  ;; ~F-form path for value in normal range.
  (assert-equal "3.1415927    "
                (lisp::format-g 3.1415927f0 nil nil nil 1 nil nil #\f nil))
  (assert-equal "0.    "
                (lisp::format-g 0f0 nil nil nil 1 nil nil #\f nil))
  (assert-equal "-0.    "
                (lisp::format-g (- 0f0) nil nil nil 1 nil nil #\f nil)))

(define-test format-g.single-large-uses-e
  (:tag :format-g :single-float)
  ;; Large values fall to ~E form.  effective-d for f2s shortest
  ;; depends on the digit count, but for 1e10f0 it's small.
  ;; 1e10f0 has n=11, q small, dd negative -> ~E form.
  (let ((s (lisp::format-g 1f10 nil nil nil 1 nil nil #\f nil)))
    (assert-true (search "f+10" s) s)))

(define-test format-g.single-small-uses-e
  (:tag :format-g :single-float)
  (let ((s (lisp::format-g 1f-5 nil nil nil 1 nil nil #\f nil)))
    (assert-true (search "f-5" s) s)))

(define-test format-g.single-d-given
  (:tag :format-g :single-float)
  ;; d-given uses the double path internally.
  (assert-equal "3.1416    "
                (lisp::format-g 3.1415927f0 nil 5 nil 1 nil nil #\f nil)))

;;; ----------------------------------------------------------------------
;;; Mixed-type sanity checks

(define-test format-e.mixed-types
  (:tag :format-e :single-float)
  ;; Same nominal value, different float types -- should produce
  ;; different shortest forms.
  (let ((single-out (lisp::format-e 3.14f0 nil nil nil 1 nil nil #\f nil))
        (double-out (lisp::format-e 3.14d0 nil nil nil 1 nil nil #\d nil)))
    (assert-true (search "f" single-out) single-out)
    (assert-true (search "d" double-out) double-out)))

;;; ----------------------------------------------------------------------
;;; Tests that ~E dispatch routes through Ryu when *use-ryu-printer* is
;;; set.  When it is NIL we should get the Burger and Dybvig result;
;;; when it is T we should get the result that LISP::FORMAT-E produces
;;; directly.

(setf lisp::*use-ryu-printer* nil)

(define-test format-e.dispatch-via-use-ryu-printer.t
  (:tag :format-e :dispatch)
  ;; With *use-ryu-printer* T, FORMAT "~E" must agree with LISP::FORMAT-E.
  (let ((lisp::*use-ryu-printer* t))
    (assert-equal (lisp::format-e 3.14d0 nil nil nil 1 nil nil #\d nil)
                  (format nil "~E" 3.14d0))
    (assert-equal (lisp::format-e -1.23456789d12 17 nil 4 1 #\* #\P #\z t)
                  (format nil "~17,,4,1,'*,'P,'zE" -1.23456789d12))))

(define-test format-e.dispatch-via-use-ryu-printer.nil
  (:tag :format-e :dispatch)
  ;; With *use-ryu-printer* NIL, FORMAT "~E" goes through the original
  ;; Burger and Dybvig code path.  Just check that some reasonable
  ;; output is produced (we don't assert against B&D output specifically
  ;; because that is the existing, well-tested code path).
  (let ((lisp::*use-ryu-printer* nil))
    (assert-true (stringp (format nil "~E" 3.14d0)))
    (assert-true (stringp (format nil "~17,,4,1,'*,'P,'zE" -1.23456789d12)))))

(define-test format-e.dispatch-falls-back-for-double-double
  (:tag :format-e :dispatch)
  ;; Double-double values must go through the B&D code even when
  ;; *use-ryu-printer* is T, because the Ryu routines only handle
  ;; single- and double-float.
  #+double-double
  (let ((lisp::*use-ryu-printer* t))
    ;; Should not error and should produce a string.
    (assert-true (stringp (format nil "~E" 1w0)))))

(define-test format-e.dispatch-falls-back-for-special-values
  (:tag :format-e :dispatch)
  ;; Infinity and NaN must go through the B&D path because the Ryu
  ;; routines do not handle them.  Just confirm no error and that we get
  ;; a string back.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~E"
                                  ext:double-float-positive-infinity)))))
(define-test format-e.dispatch-non-float
  (:tag :format-e :dispatch)
  ;; Non-float arguments (rationals, integers) must continue to work.
  ;; Rationals are coerced to single-float by FORMAT-EXPONENTIAL before
  ;; reaching FORMAT-EXP-AUX, so the Ryu path will see a single-float.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~E" 1/2)))
    (assert-true (stringp (format nil "~E" 1)))))

;;; ----------------------------------------------------------------------
;;; Tests that ~E dispatch routes through Ryu when *use-ryu-printer* is
;;; set.  When it is NIL we should get the Burger and Dybvig result;
;;; when it is T we should get the result that LISP::FORMAT-E produces
;;; directly.

(define-test format-e.dispatch-via-use-ryu-printer.t
  (:tag :format-e :dispatch)
  ;; With *use-ryu-printer* T, FORMAT "~E" must agree with LISP::FORMAT-E.
  (let ((lisp::*use-ryu-printer* t))
    (assert-equal (lisp::format-e 3.14d0 nil nil nil 1 nil nil #\d nil)
                  (format nil "~E" 3.14d0))
    (assert-equal (lisp::format-e -1.23456789d12 17 nil 4 1 #\* #\P #\z t)
                  (format nil "~17,,4,1,'*,'P,'zE" -1.23456789d12))))

(define-test format-e.dispatch-via-use-ryu-printer.nil
  (:tag :format-e :dispatch)
  ;; With *use-ryu-printer* NIL, FORMAT "~E" goes through the original
  ;; Burger and Dybvig code path.  Just check that some reasonable
  ;; output is produced (we don't assert against B&D output specifically
  ;; because that is the existing, well-tested code path).
  (let ((lisp::*use-ryu-printer* nil))
    (assert-true (stringp (format nil "~E" 3.14d0)))
    (assert-true (stringp (format nil "~17,,4,1,'*,'P,'zE" -1.23456789d12)))))

(define-test format-e.dispatch-falls-back-for-double-double
  (:tag :format-e :dispatch)
  ;; Double-double values must go through the B&D code even when
  ;; *use-ryu-printer* is T, because the Ryu routines only handle
  ;; single- and double-float.
  #+double-double
  (let ((lisp::*use-ryu-printer* t))
    ;; Should not error and should produce a string.
    (assert-true (stringp (format nil "~E" 1w0)))))

(define-test format-e.dispatch-special-values
  (:tag :format-e :dispatch)
  ;; Infinity and NaN are handled directly inside FORMAT-EXP-RYU (by
  ;; PRIN1, matching the B&D behavior); they no longer require falling
  ;; back to the B&D path.  Just confirm no error and that we get a
  ;; string back.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~E"
                                  ext:double-float-positive-infinity)))))

(define-test format-e.dispatch-non-float
  (:tag :format-e :dispatch)
  ;; Non-float arguments (rationals, integers) must continue to work.
  ;; Rationals are coerced to single-float by FORMAT-EXPONENTIAL before
  ;; reaching FORMAT-EXP-AUX, so the Ryu path will see a single-float.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~E" 1/2)))
    (assert-true (stringp (format nil "~E" 1)))))

;;; ----------------------------------------------------------------------
;;; Tests that ~F dispatch routes through Ryu when *use-ryu-printer* is
;;; set.

(define-test format-f.dispatch-via-use-ryu-printer.t
  (:tag :format-f :dispatch)
  ;; With *use-ryu-printer* T, FORMAT "~F" must agree with LISP::FORMAT-F.
  (let ((lisp::*use-ryu-printer* t))
    ;; Default ~F: no width, no fdigits, k = 0.
    (assert-equal (lisp::format-f 3.14d0 nil nil 0 nil nil nil)
                  (format nil "~F" 3.14d0))
    ;; With width and fdigits.
    (assert-equal (lisp::format-f 3.14d0 10 4 0 nil #\space nil)
                  (format nil "~10,4F" 3.14d0))))

(define-test format-f.dispatch-via-use-ryu-printer.nil
  (:tag :format-f :dispatch)
  (let ((lisp::*use-ryu-printer* nil))
    (assert-true (stringp (format nil "~F" 3.14d0)))
    (assert-true (stringp (format nil "~10,4F" 3.14d0)))))

(define-test format-f.dispatch-non-zero-k
  (:tag :format-f :dispatch)
  ;; Non-zero scale factor K stays correct: LISP::FORMAT-F itself falls
  ;; back internally to FORMAT::FORMAT-FIXED-AUX-BD for that case.
  ;; Result should be a string, and should match the B&D result.
  (let ((with-ryu
	  (let ((lisp::*use-ryu-printer* t))
	    (format nil "~10,4,2F" 3.14d0)))
	(without-ryu
	  (let ((lisp::*use-ryu-printer* nil))
	    (format nil "~10,4,2F" 3.14d0))))
    (assert-equal without-ryu with-ryu)))

(define-test format-f.dispatch-falls-back-for-double-double
  (:tag :format-f :dispatch)
  #+double-double
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~F" 1w0)))))

(define-test format-f.dispatch-special-values
  (:tag :format-f :dispatch)
  ;; Infinity and NaN handled by FORMAT-FIXED-RYU.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~F"
                                  ext:double-float-positive-infinity)))))

(define-test format-f.dispatch-non-float
  (:tag :format-f :dispatch)
  ;; Rationals are coerced to single-float by FORMAT-FIXED before
  ;; reaching FORMAT-FIXED-AUX.  Integers exercise the integer branch
  ;; of FORMAT-FIXED (via DECIMAL-STRING) and do not touch the ryu
  ;; path at all -- they should still work.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~F" 1/2)))
    (assert-true (stringp (format nil "~F" 1)))))

;;; ----------------------------------------------------------------------
;;; Tests that ~G dispatch routes through Ryu when *use-ryu-printer* is
;;; set.

(define-test format-g.dispatch-via-use-ryu-printer.t
  (:tag :format-g :dispatch)
  ;; With *use-ryu-printer* T, FORMAT "~G" must agree with LISP::FORMAT-G.
  (let ((lisp::*use-ryu-printer* t))
    (assert-equal (lisp::format-g 3.14d0 nil nil nil 1 nil nil #\d nil)
                  (format nil "~G" 3.14d0))
    (assert-equal (lisp::format-g 12345.6789d0 12 4 nil 1 nil #\space #\d nil)
                  (format nil "~12,4,,1G" 12345.6789d0))))

(define-test format-g.dispatch-via-use-ryu-printer.nil
  (:tag :format-g :dispatch)
  (let ((lisp::*use-ryu-printer* nil))
    (assert-true (stringp (format nil "~G" 3.14d0)))
    (assert-true (stringp (format nil "~12,4,,1G" 12345.6789d0)))))

(define-test format-g.dispatch-falls-back-for-double-double
  (:tag :format-g :dispatch)
  #+double-double
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~G" 1w0)))))

(define-test format-g.dispatch-special-values
  (:tag :format-g :dispatch)
  ;; Infinity and NaN handled by FORMAT-GENERAL-RYU.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~G"
                                  ext:double-float-positive-infinity)))))

(define-test format-g.dispatch-non-float
  (:tag :format-g :dispatch)
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (format nil "~G" 1/2)))
    (assert-true (stringp (format nil "~G" 1)))))

;;; ----------------------------------------------------------------------
;;; Tests that the float printer used by PRIN1/PRINC/PRINT/WRITE
;;; (i.e. OUTPUT-FLOAT-AUX) dispatches through Ryu when
;;; *use-ryu-printer* is set.  Both code paths should produce
;;; round-trippable strings.  We don't assert character equality
;;; between the two paths because the shortest round-tripping digit
;;; sequence is allowed to differ in rounding of the tie case; we do
;;; assert each path round-trips.

(define-test output-float.dispatch.round-trip
  (:tag :output-float :dispatch)
  (let ((values '(3.14d0
		  -3.14d0
		  0.1d0
		  1.0d100
		  1.0d-100
		  1.0d0
		  3.14f0
		  -3.14f0
		  0.1f0)))
    (dolist (v values)
      (let ((with-ryu
	      (let ((lisp::*use-ryu-printer* t))
		(prin1-to-string v)))
	    (without-ryu
	      (let ((lisp::*use-ryu-printer* nil))
		(prin1-to-string v))))
	(assert-equal v (read-from-string with-ryu) v with-ryu)
	(assert-equal v (read-from-string without-ryu) v without-ryu)))))

(define-test output-float.dispatch.princ-matches-prin1
  (:tag :output-float :dispatch)
  ;; For floats, PRIN1 and PRINC produce the same string (floats don't
  ;; have escape characters).  This should hold under both code paths.
  (let ((lisp::*use-ryu-printer* t))
    (assert-equal (prin1-to-string 3.14d0)
                  (princ-to-string 3.14d0))
    (assert-equal (prin1-to-string 3.14f0)
                  (princ-to-string 3.14f0))))

(define-test output-float.dispatch.zero
  (:tag :output-float :dispatch)
  ;; Zero is handled by OUTPUT-FLOAT itself (not OUTPUT-FLOAT-AUX), so
  ;; the dispatch is never reached.  Still, make sure both settings
  ;; produce identical output.
  (let ((with-ryu
	  (let ((lisp::*use-ryu-printer* t))
	    (prin1-to-string 0.0d0)))
	(without-ryu
	  (let ((lisp::*use-ryu-printer* nil))
	    (prin1-to-string 0.0d0))))
    (assert-equal without-ryu with-ryu)))

(define-test output-float.dispatch.special-values
  (:tag :output-float :dispatch)
  ;; Infinity and NaN are handled in OUTPUT-FLOAT itself, before the
  ;; OUTPUT-FLOAT-AUX dispatch, so the Ryu path is not exercised.
  ;; Verify no error.
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (prin1-to-string
                            ext:double-float-positive-infinity)))))

(define-test output-float.dispatch.double-double
  (:tag :output-float :dispatch)
  ;; DOUBLE-DOUBLE-FLOAT is handled in OUTPUT-FLOAT itself when the
  ;; feature is enabled (currently behind #+(and nil double-double),
  ;; i.e. inactive), so it actually reaches OUTPUT-FLOAT-AUX.  Confirm
  ;; the type-guard in the dispatch wrapper sends it to the B&D path.
  #+double-double
  (let ((lisp::*use-ryu-printer* t))
    (assert-true (stringp (prin1-to-string 1w0)))))

