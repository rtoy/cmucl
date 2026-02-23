;; Test extensions 
(defpackage :extensions-tests
  (:use :cl :lisp-unit))

(in-package "EXTENSIONS-TESTS")

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
  
  (assert-equal #x0010000000000000 (get-double-bits (ext:parse-hex-float "0x1.0000000000000p-1022")))
  (assert-equal #x000fffffffffffff (get-double-bits (ext:parse-hex-float "0x0.fffffffffffffp-1022")))
  (assert-equal #x001f0195cb356b8f (get-double-bits (ext:parse-hex-float "0x1.f0195cb356b8fp-1022")))
  
  ;; Single Precision (-126 Cliff)
  
  (assert-equal #x00800000 (get-single-bits (ext:parse-hex-float "0x1.000000p-126f")))
  (assert-equal #x00400000 (get-single-bits (ext:parse-hex-float "0x0.800000p-126f")))
  (assert-equal #x7f7fffff (get-single-bits (ext:parse-hex-float "0x1.fffffep+127f"))))

(define-test test-negative-zero
  (:tag :edge-cases)
  (assert-equal #x8000000000000000 (get-double-bits (ext:parse-hex-float "-0x0.0p+0")))
  (assert-equal #x80000000         (get-single-bits (ext:parse-hex-float "-0x0.0p+0f")))
  (assert-true (typep (ext:parse-hex-float "-0x0.0p+0f") 'single-float)))

(define-test test-double-roundtrip
  (:tag :stress)
  (loop repeat 10000 do
    (let* ((hi-bits (random #x100000000))
           (hi (if (logbitp 31 hi-bits) (- hi-bits #x100000000) hi-bits))
           (lo (random #x100000000))
           (val (kernel:make-double-float hi lo)))
      (unless (or (ext:float-nan-p val) (ext:float-infinity-p val))
        (let* ((str (ext::print-hex-double-float val))
               (parsed (ext:parse-hex-float str)))
          (assert-equal (get-double-bits val) (get-double-bits parsed)))))))

(define-test test-single-roundtrip
  (:tag :stress)
  (loop repeat 10000 do
    (let* ((bits-raw (random #x100000000))
           (bits (if (logbitp 31 bits-raw) (- bits-raw #x100000000) bits-raw))
           (val (kernel:make-single-float bits)))
      (unless (or (ext:float-nan-p val) (ext:float-infinity-p val))
        (let* ((str (concatenate 'string (ext::print-hex-single-float val) "f"))
               (parsed (ext:parse-hex-float str)))
          (assert-equal (get-single-bits val) (get-single-bits parsed)))))))
