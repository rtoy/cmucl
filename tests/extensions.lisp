;; Test extensions 
(defpackage :extensions-tests
  (:use :cl :lisp-unit))

(in-package "EXTENSIONS-TESTS")

(define-test float-to-hex-string
  (assert-equal "0x1.8p+1"   (ext:float-to-hex-string 3.0d0))
  (assert-equal "0x1.8p+1f"  (ext:float-to-hex-string 3.0f0))
  (assert-equal "0x1.8p+1w"  (ext:float-to-hex-string 3.0w0))
  (assert-equal "-0x1.8p+1"  (ext:float-to-hex-string -3.0d0)))

(define-test write-double-zero
  (assert-equal "0x0p+0"  (ext:float-to-hex-string 0.0d0))
  (assert-equal "-0x0p+0" (ext:float-to-hex-string -0.0d0)))

(define-test write-double-one
  (assert-equal "0x1p+0"  (ext:float-to-hex-string 1.0d0))
  (assert-equal "-0x1p+0" (ext:float-to-hex-string -1.0d0)))

(define-test write-double-powers-of-two
  (assert-equal "0x1p+1"    (ext:float-to-hex-string 2.0d0))
  (assert-equal "0x1p-1"    (ext:float-to-hex-string 0.5d0))
  (assert-equal "0x1p+52"   (ext:float-to-hex-string (expt 2.0d0 52)))
  (assert-equal "0x1p-52"   (ext:float-to-hex-string (expt 2.0d0 -52)))
  (assert-equal "0x1p+1023" (ext:float-to-hex-string (expt 2.0d0 1023)))
  (assert-equal "0x1p-1022" (ext:float-to-hex-string (expt 2.0d0 -1022))))

(define-test write-double-fractions
  (assert-equal "0x1.8p+1"             (ext:float-to-hex-string 3.0d0))
  (assert-equal "-0x1.8p+1"            (ext:float-to-hex-string -3.0d0))
  (assert-equal "0x1.5555555555555p-2" (ext:float-to-hex-string (/ 1.0d0 3.0d0)))
  (assert-equal "0x1.921fb54442d18p+1" (ext:float-to-hex-string pi)))

(define-test write-double-denormals
  (assert-equal "0x0.8p-1022"             (ext:float-to-hex-string (expt 2.0d0 -1023)))
  (assert-equal "0x0.0000000000001p-1022" (ext:float-to-hex-string (expt 2.0d0 -1074))))

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
  (assert-equal "0x1p-126f"        (ext:float-to-hex-string (expt 2.0f0 -126))))

(define-test write-single-denormals
  (assert-equal "0x0.000002p-126f" (ext:float-to-hex-string (expt 2.0f0 -149))))

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
  (assert-equal "0x1p+64w"               (ext:float-to-hex-string (expt 2.0w0 64)))
  (assert-equal "0x1.921fb54442d18p+1w"
                (ext:float-to-hex-string (coerce pi 'ext:double-double-float)))
  (assert-equal "0x1.fffffffffffff8p-1w"
                (ext:float-to-hex-string (- 1.0w0 (expt 2.0w0 -54)))))

(defun get-double-bits (val)
  (multiple-value-bind (hi lo) (kernel:double-float-bits val)
    (logior (ash (ldb (byte 32 0) hi) 32) (ldb (byte 32 0) lo))))

(defun get-single-bits (val)
  (ldb (byte 32 0) (kernel:single-float-bits val)))

(define-test test-hex-syntax
  (:tag :validation)
  (assert-error 'ext:hex-parse-error (ext:parse-hex-float "inf"))
  (assert-error 'ext:hex-parse-error (ext:parse-hex-float "0x.p+0"))
  (assert-error 'ext:hex-parse-error (ext:parse-hex-float "0x1.0p")))

(define-test test-cliff-boundaries
  (:tag :precision)
  ;; Double Precision (-1022 Cliff)
  
  (assert-equal #x0010000000000000
		(get-double-bits (ext:parse-hex-float "0x1.0000000000000p-1022")))
  (assert-equal #x000fffffffffffff
		(get-double-bits (ext:parse-hex-float "0x0.fffffffffffffp-1022")))
  (assert-equal #x001f0195cb356b8f
		(get-double-bits (ext:parse-hex-float "0x1.f0195cb356b8fp-1022")))
  
  ;; Single Precision (-126 Cliff)
  
  (assert-equal #x00800000
		(get-single-bits (ext:parse-hex-float "0x1.000000p-126f")))
  (assert-equal #x00400000
		(get-single-bits (ext:parse-hex-float "0x0.800000p-126f")))
  (assert-equal #x7f7fffff
		(get-single-bits (ext:parse-hex-float "0x1.fffffep+127f"))))

(define-test test-negative-zero
  (:tag :edge-cases)
  (assert-equal #x8000000000000000
		(get-double-bits (ext:parse-hex-float "-0x0.0p+0")))
  (assert-equal #x80000000
		(get-single-bits (ext:parse-hex-float "-0x0.0p+0f")))
  (assert-true (typep (ext:parse-hex-float "-0x0.0p+0f")
		      'single-float)))

(define-test test-subnormal-boundaries
  (:tag :edge)
  ;; Test smallest single-float subnormal
  (let* ((val (kernel:make-single-float 1))
         (str (ext:float-to-hex-string val))
         (parsed (ext:parse-hex-float str)))
    (assert-equal (get-single-bits val) (get-single-bits parsed)
		  val str parsed))
  ;; Test smallest double-float subnormal
  (let* ((val (kernel:make-double-float 0 1))
         (str (ext:float-to-hex-string val))
         (parsed (ext:parse-hex-float str)))
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
               (parsed (ext:parse-hex-float str)))
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
               (parsed (ext:parse-hex-float str)))
          (assert-equal (get-single-bits val)
			(get-single-bits parsed)
			val str parsed))))))
