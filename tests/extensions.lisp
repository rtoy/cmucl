;; Test extensions 
(defpackage :extensions-tests
  (:use :cl :lisp-unit))

(in-package "EXTENSIONS-TESTS")

;;; ---- write-hex-float / float-to-hex-string tests -------------------------

(define-test write-double-zero
  (assert-equal "0x0p+0"  (ext:float-to-hex-string 0.0d0))
  (assert-equal "-0x0p+0" (ext:float-to-hex-string -0.0d0)))

(define-test write-double-one
  (assert-equal "0x1p+0"  (ext:float-to-hex-string 1.0d0))
  (assert-equal "-0x1p+0" (ext:float-to-hex-string -1.0d0)))

(define-test write-double-powers-of-two
  (assert-equal "0x1p+1"    (ext:float-to-hex-string (scale-float 1.0d0 1)))
  (assert-equal "0x1p-1"    (ext:float-to-hex-string (scale-float 1.0d0 -1)))
  (assert-equal "0x1p+52"   (ext:float-to-hex-string (scale-float 1.0d0 52)))
  (assert-equal "0x1p-52"   (ext:float-to-hex-string (scale-float 1.0d0 -52)))
  (assert-equal "0x1p+1023" (ext:float-to-hex-string (scale-float 1.0d0 1023)))
  (assert-equal "0x1p-1022" (ext:float-to-hex-string (scale-float 1.0d0 -1022))))

(define-test write-double-fractions
  (assert-equal "0x1.8p+1"             (ext:float-to-hex-string 3.0d0))
  (assert-equal "-0x1.8p+1"            (ext:float-to-hex-string -3.0d0))
  (assert-equal "0x1.5555555555555p-2" (ext:float-to-hex-string (/ 1.0d0 3.0d0)))
  (assert-equal "0x1.921fb54442d18p+1" (ext:float-to-hex-string pi)))

(define-test write-double-denormals
  (assert-equal "0x0.8p-1022"             (ext:float-to-hex-string (scale-float 1.0d0 -1023)))
  (assert-equal "0x0.0000000000001p-1022" (ext:float-to-hex-string (scale-float 1.0d0 -1074))))

(define-test write-double-special
  (assert-equal "0x1.0p+inf"
                (ext:float-to-hex-string ext:double-float-positive-infinity))
  (assert-equal "-0x1.0p+inf"
                (ext:float-to-hex-string ext:double-float-negative-infinity))
  (assert-equal "0x0.0p+nan"
                (ext:float-to-hex-string
                  (ext:with-float-traps-masked (:invalid)
                    (- ext:double-float-positive-infinity
                       ext:double-float-positive-infinity)))))

(define-test write-single-zero
  (assert-equal "0x0p+0f"  (ext:float-to-hex-string 0.0f0))
  (assert-equal "-0x0p+0f" (ext:float-to-hex-string -0.0f0)))

(define-test write-single-values
  (assert-equal "0x1p+0f"          (ext:float-to-hex-string 1.0f0))
  (assert-equal "-0x1p+0f"         (ext:float-to-hex-string -1.0f0))
  (assert-equal "0x1p+1f"          (ext:float-to-hex-string 2.0f0))
  (assert-equal "0x1.8p+1f"        (ext:float-to-hex-string 3.0f0))
  (assert-equal "0x1.555556p-2f"   (ext:float-to-hex-string (/ 1.0f0 3.0f0)))
  (assert-equal "0x1.fffffep+127f" (ext:float-to-hex-string most-positive-single-float))
  (assert-equal "0x1p-126f"        (ext:float-to-hex-string (scale-float 1.0f0 -126))))

(define-test write-single-denormals
  (assert-equal "0x0.000002p-126f" (ext:float-to-hex-string (scale-float 1.0f0 -149))))

(define-test write-single-special
  (assert-equal "0x1.0p+inff"
                (ext:float-to-hex-string ext:single-float-positive-infinity))
  (assert-equal "-0x1.0p+inff"
                (ext:float-to-hex-string ext:single-float-negative-infinity))
  (assert-equal "0x0.0p+nanf"
                (ext:float-to-hex-string
                  (ext:with-float-traps-masked (:invalid)
                    (- ext:single-float-positive-infinity
                       ext:single-float-positive-infinity)))))

(define-test write-double-double-zero
  (assert-equal "0x0p+0w"  (ext:float-to-hex-string 0.0w0))
  (assert-equal "-0x0p+0w" (ext:float-to-hex-string -0.0w0)))

(define-test write-double-double-values
  (assert-equal "0x1p+0w"                (ext:float-to-hex-string 1.0w0))
  (assert-equal "-0x1p+0w"               (ext:float-to-hex-string -1.0w0))
  (assert-equal "0x1.8p+1w"              (ext:float-to-hex-string 3.0w0))
  (assert-equal "0x1p+64w"               (ext:float-to-hex-string (scale-float 1.0w0 64)))
  (assert-equal "0x1.921fb54442d18p+1w"
                (ext:float-to-hex-string (coerce pi 'ext:double-double-float)))
  (assert-equal "0x1.fffffffffffff8p-1w"
                (ext:float-to-hex-string (- 1.0w0 (scale-float 1.0w0 -54)))))


;;; ---- read-hex-float tests ------------------------------------------------

(define-test read-double-zero
  (assert-eql 0.0d0  (ext:read-hex-float "0x0p+0"))
  (assert-eql -0.0d0 (ext:read-hex-float "-0x0p+0")))

(define-test read-double-values
  (assert-eql 1.0d0  (ext:read-hex-float "0x1p+0"))
  (assert-eql -1.0d0 (ext:read-hex-float "-0x1p+0"))
  (assert-eql 2.0d0  (ext:read-hex-float "0x1p+1"))
  (assert-eql 0.5d0  (ext:read-hex-float "0x1p-1"))
  (assert-eql 3.0d0  (ext:read-hex-float "0x1.8p+1"))
  (assert-eql -3.0d0 (ext:read-hex-float "-0x1.8p+1"))
  (assert-eql pi     (ext:read-hex-float "0x1.921fb54442d18p+1")))

(define-test read-double-denormals
  (assert-eql (scale-float 1.0d0 -1023)
              (ext:read-hex-float "0x0.8p-1022"))
  (assert-eql (scale-float 1.0d0 -1074)
              (ext:read-hex-float "0x0.0000000000001p-1022")))

(define-test read-double-case-insensitive
  (assert-eql 3.0d0 (ext:read-hex-float "0X1.8P+1"))
  (assert-eql 0.5d0 (ext:read-hex-float "0X1P-1")))

(define-test read-single-zero
  (assert-eql 0.0f0  (ext:read-hex-float "0x0p+0f"))
  (assert-eql -0.0f0 (ext:read-hex-float "-0x0p+0f")))

(define-test read-single-values
  (assert-eql 1.0f0  (ext:read-hex-float "0x1p+0f"))
  (assert-eql -1.0f0 (ext:read-hex-float "-0x1p+0f"))
  (assert-eql 2.0f0  (ext:read-hex-float "0x1p+1f"))
  (assert-eql 3.0f0  (ext:read-hex-float "0x1.8p+1f"))
  (assert-eql (/ 1.0f0 3.0f0)
              (ext:read-hex-float "0x1.555556p-2f"))
  (assert-eql most-positive-single-float
              (ext:read-hex-float "0x1.fffffep+127f"))
  (assert-eql (scale-float 1.0f0 -149)
              (ext:read-hex-float "0x0.000002p-126f")))

(define-test read-single-case-insensitive
  (assert-eql 3.0f0 (ext:read-hex-float "0x1.8p+1F")))

(define-test read-double-double-zero
  (assert-eql 0.0w0  (ext:read-hex-float "0x0p+0w"))
  (assert-eql -0.0w0 (ext:read-hex-float "-0x0p+0w")))

(define-test read-double-double-values
  (assert-eql 1.0w0  (ext:read-hex-float "0x1p+0w"))
  (assert-eql -1.0w0 (ext:read-hex-float "-0x1p+0w"))
  (assert-eql 3.0w0  (ext:read-hex-float "0x1.8p+1w"))
  (assert-eql (scale-float 1.0w0 64)
              (ext:read-hex-float "0x1p+64w"))
  (assert-eql (coerce pi 'ext:double-double-float)
              (ext:read-hex-float "0x1.921fb54442d18p+1w")))

(define-test read-double-double-case-insensitive
  (assert-eql 3.0w0 (ext:read-hex-float "0x1.8p+1W")))


;;; ---- round-trip tests ----------------------------------------------------

(define-test round-trip-double
  (dolist (x (list 0.0d0 -0.0d0 1.0d0 -1.0d0 pi
                   most-positive-double-float
                   least-positive-double-float
                   (scale-float 1.0d0 -1022)
                   (scale-float 1.0d0 -1074)
                   (/ 1.0d0 3.0d0)))
    (assert-eql x (ext:read-hex-float (ext:float-to-hex-string x)))))

(define-test round-trip-single
  (dolist (x (list 0.0f0 -0.0f0 1.0f0 -1.0f0
                   most-positive-single-float
                   least-positive-single-float
                   (scale-float 1.0f0 -126)
                   (scale-float 1.0f0 -149)
                   (/ 1.0f0 3.0f0)))
    (assert-eql x (ext:read-hex-float (ext:float-to-hex-string x)))))

(define-test round-trip-double-double
  (dolist (x (list 0.0w0 -0.0w0 1.0w0 -1.0w0
                   (coerce pi 'ext:double-double-float)
                   (scale-float 1.0w0 64)
                   (+ 1.0w0 1.0w-100)
                   (- 1.0w0 (scale-float 1.0w0 -54))
                   ext:most-positive-double-double-float
                   ext:least-positive-double-double-float))
    (assert-eql x (ext:read-hex-float (ext:float-to-hex-string x)))))


;;; ---- read-hex-float-from-string tests ------------------------------------

(define-test read-from-string-positions
  (multiple-value-bind (val pos)
      (ext:read-hex-float "0x1.8p+1")
    (assert-eql 3.0d0 val)
    (assert-equal 8 pos))
  (multiple-value-bind (val pos)
      (ext:read-hex-float "0x1.8p+1f")
    (assert-eql 3.0f0 val)
    (assert-equal 9 pos))
  (multiple-value-bind (val pos)
      (ext:read-hex-float "xxx0x1.8p+1" :start 3)
    (assert-eql 3.0d0 val)
    (assert-equal 11 pos))
  (multiple-value-bind (val pos)
      (ext:read-hex-float "0x1.8p+1 etc")
    (assert-eql 3.0d0 val)
    (assert-equal 8 pos)))


;;; ---- format-hex-float tests ----------------------------------------------

(define-test format-hex-float-basic
  (assert-equal "0x1.8p+1"
                (format nil "~/ext:format-hex-float/" 3.0d0))
  (assert-equal "0x1.8p+1f"
                (format nil "~/ext:format-hex-float/" 3.0f0))
  (assert-equal "0x1.8p+1w"
                (format nil "~/ext:format-hex-float/" 3.0w0)))

(define-test format-hex-float-atsign
  (assert-equal "+0x1.8p+1"
                (format nil "~@/ext:format-hex-float/" 3.0d0))
  (assert-equal "+0x1p+0f"
                (format nil "~@/ext:format-hex-float/" 1.0f0))
  (assert-equal "-0x1.8p+1"
                (format nil "~@/ext:format-hex-float/" -3.0d0))
  (assert-equal "0x0.0p+nan"
                (format nil "~@/ext:format-hex-float/"
                        (ext:with-float-traps-masked (:invalid)
                          (- ext:double-float-positive-infinity
                             ext:double-float-positive-infinity)))))


;;; ---- parse error tests ---------------------------------------------------

(define-test read-hex-float-parse-errors
  (assert-true (subtypep 'ext:hex-float-parse-error 'parse-error))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float ""))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "1.0p+0"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "00.0p+0"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "0x1.0"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "0x1.0p"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "0x1.0p+"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "0x.p+0"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "0x1p+0q"))
  (assert-error 'ext:hex-float-parse-error
                (ext:read-hex-float "-")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-double-bits (val)
  (multiple-value-bind (hi lo) (kernel:double-float-bits val)
    (logior (ash (ldb (byte 32 0) hi) 32) (ldb (byte 32 0) lo))))

(defun get-single-bits (val)
  (ldb (byte 32 0) (kernel:single-float-bits val)))

(define-test test-hex-syntax
  (:tag :validation)
  (assert-error 'ext:hex-float-parse-error (ext:read-hex-float "inf"))
  (assert-error 'ext:hex-float-parse-error (ext:read-hex-float "0x.p+0"))
  (assert-error 'ext:hex-float-parse-error (ext:read-hex-float "0x1.0p")))

(define-test test-cliff-boundaries
  (:tag :precision)
  ;; Double Precision (-1022 Cliff)
  
  (assert-equal #x0010000000000000
		(get-double-bits (ext:read-hex-float "0x1.0000000000000p-1022")))
  (assert-equal #x000fffffffffffff
		(get-double-bits (ext:read-hex-float "0x0.fffffffffffffp-1022")))
  (assert-equal #x001f0195cb356b8f
		(get-double-bits (ext:read-hex-float "0x1.f0195cb356b8fp-1022")))
  
  ;; Single Precision (-126 Cliff)
  
  (assert-equal #x00800000
		(get-single-bits (ext:read-hex-float "0x1.000000p-126f")))
  (assert-equal #x00400000
		(get-single-bits (ext:read-hex-float "0x0.800000p-126f")))
  (assert-equal #x7f7fffff
		(get-single-bits (ext:read-hex-float "0x1.fffffep+127f"))))

(define-test test-negative-zero
  (:tag :edge-cases)
  (assert-equal #x8000000000000000
		(get-double-bits (ext:read-hex-float "-0x0.0p+0")))
  (assert-equal #x80000000
		(get-single-bits (ext:read-hex-float "-0x0.0p+0f")))
  (assert-true (typep (ext:read-hex-float "-0x0.0p+0f")
		      'single-float)))

(define-test test-subnormal-boundaries
  (:tag :edge)
  ;; Test smallest single-float subnormal
  (let* ((val (kernel:make-single-float 1))
         (str (ext:float-to-hex-string val))
         (parsed (ext:read-hex-float str)))
    (assert-equal (get-single-bits val) (get-single-bits parsed)
		  val str parsed))
  ;; Test smallest double-float subnormal
  (let* ((val (kernel:make-double-float 0 1))
         (str (ext:float-to-hex-string val))
         (parsed (ext:read-hex-float str)))
    (assert-equal (get-double-bits val) (get-double-bits parsed)
		  val str parsed)))

(define-test test-double-roundtrip
  (:tag :stress)
  (loop repeat 10000 do
    (let* ((hi-bits (random #x100000000))
           (hi (if (logbitp 31 hi-bits) (- hi-bits #x100000000) hi-bits))
           (lo (random #x100000000))
           (val (kernel:make-double-float hi lo)))
      (unless (or (ext:float-nan-p val) (ext:float-infinity-p val))
        (let* ((str (ext:float-to-hex-string val))
               (parsed (ext:read-hex-float str)))
          (assert-equal (get-double-bits val)
			(get-double-bits parsed)
			val str parsed))))))

(define-test test-single-roundtrip
  (:tag :stress)
  (loop repeat 10000 do
    (let* ((bits-raw (random #x100000000))
           (bits (if (logbitp 31 bits-raw) (- bits-raw #x100000000) bits-raw))
           (val (kernel:make-single-float bits)))
      (unless (or (ext:float-nan-p val) (ext:float-infinity-p val))
        (let* ((str (concatenate 'string (ext:float-to-hex-string val) "f"))
               (parsed (ext:read-hex-float str)))
          (assert-equal (get-single-bits val)
			(get-single-bits parsed)
			val str parsed))))))

