;; Test extensions 
(defpackage :extensions-tests
  (:use :cl :lisp-unit))

(in-package "EXTENSIONS-TESTS")

#+nil
(defun test-invalid-strings ()
  (format t "Testing invalid strings...~%")
  (let ((invalid-cases '("" "1.0" "0x1.0" "0x1.0p" "0x1.zp+0" "0x.p+0" "0x1 .0p+0")))
    (dolist (case invalid-cases)
      (handler-case
          (progn (parse-hex-float case) (error "Failed to trap ~S" case))
        (hex-parse-error () (format t "  Caught expected error for: ~S~%" case)))))
  (format t "Invalid string tests passed.~%"))

(define-test parse-hex.invalid-strings
  (dolist (case '("" "1.0" "0x1.0" "0x1.0p" "0x1.zp+0" "0x.p+0" "0x1 .0p+0"))
    (assert-error 'ext:hex-parse-error
		  (ext:parse-hex-float case)
		  case)))

#+nil
(defun run-hex-float-tests (&key (iterations 20000))
  "Validates bit-consistency for double floats."
  (format t "Testing ~D random bit patterns (Double Precision)...~%" iterations)
  (loop repeat iterations do
    (let* ((hi-bits (random (expt 2 32)))
           (hi (if (logbitp 31 hi-bits)
                   (- hi-bits (expt 2 32))
                   hi-bits))
           (lo (random (expt 2 32)))
           (d-float (kernel:make-double-float hi lo))
           (d-str (print-hex-double-float d-float))
           (d-parsed (parse-hex-float d-str)))
      (cond
        ((eq d-parsed :nan)
         (assert (float-nan-p d-float)))
        (t
         (multiple-value-bind (n-hi n-lo)
             (kernel:double-float-bits d-parsed)
           (assert (and (= (ldb (byte 32 0) hi)
                           (ldb (byte 32 0) n-hi)) 
                        (= lo n-lo))))))))
  (format t "Bit verification passed.~%"))

(define-test hex-parse-print-consistency
  (loop repeat 20000 do
        (let* ((hi-bits (random (expt 2 32)))
           (hi (if (logbitp 31 hi-bits)
                   (- hi-bits (expt 2 32))
                   hi-bits))
           (lo (random (expt 2 32)))
           (d-float (kernel:make-double-float hi lo))
           (d-str (ext:print-hex-float d-float))
           (d-parsed (ext:parse-hex-float d-str)))
      (cond
        ((eq d-parsed :nan)
         (assert-true (ext:float-nan-p d-float)
		      d-float d-parsed))
        (t
         (multiple-value-bind (n-hi n-lo)
             (kernel:double-float-bits d-parsed)
           (assert-true (= (ldb (byte 32 0) hi)
                           (ldb (byte 32 0) n-hi))
			hi n-hi)
	   (assert-true (= lo n-lo)
			lo n-lo)))))))
	

#+nil
(defun run-subnormal-stress-test ()
  (format t "Running subnormal stress tests...~%")
  (let* ((s-str "0x0.10534ec00dae8p-1022")
         (parsed (parse-hex-float s-str)))
    ;; Using assumed builtin float-denormalized-p
    (assert (float-denormalized-p parsed))
    (multiple-value-bind (hi lo) (kernel:double-float-bits parsed)
      (assert (= (logior (ash (ldb (byte 20 0) hi) 32) lo) #x10534ec00dae8))))
  (loop repeat 5000 do
    (let* ((lo (random (expt 2 32)))
           (hi (random (expt 2 20))) ; biased exponent is 0
           (val (kernel:make-double-float hi lo))
           (str (ext::print-hex-double-float val))
           (parsed (parse-hex-float str)))
      (unless (zerop val)
        (multiple-value-bind (new-hi new-lo) (kernel:double-float-bits parsed)
          (assert (and (= hi new-hi) (= lo new-lo)))))))
  (format t "Subnormal stress test passed.~%"))

(define-test hex-parse-denormals.1
  (let* ((s-str "0x0.10534ec00dae8p-1022")
         (parsed (ext:parse-hex-float s-str)))
    (assert-true (ext:float-denormalized-p parsed))
    (multiple-value-bind (hi lo)
	(kernel:double-float-bits parsed)
      (assert-true (= (logior (ash (ldb (byte 20 0) hi) 32) lo)
		      #x10534ec00dae8)))))
  
(define-test hex-parse-denormals.random
  (loop repeat 5000 do
    (let* ((lo (random (expt 2 32)))
           (hi (random (expt 2 20))) ; biased exponent is 0
           (val (kernel:make-double-float hi lo))
           (str (ext::print-hex-double-float val))
           (parsed (ext:parse-hex-float str)))
      (unless (zerop val)
        (multiple-value-bind (new-hi new-lo)
	    (kernel:double-float-bits parsed)
          (assert-true (and (= hi new-hi) (= lo new-lo))))))))

#+nil
(defun run-cliff-tests ()
  "Tests precision around the smallest normalized and largest subnormal boundary."
  (format t "Running boundary (cliff) tests...~%")
  (let ((cases '(;; Smallest normalized number (2^-1022)
                 ("0x1.0000000000000p-1022" #x0010000000000000)
                 ;; Smallest normalized + 1 ULP
                 ("0x1.0000000000001p-1022" #x0010000000000001)
                 ;; Smallest normalized - 1 ULP (Largest subnormal)
                 ("0x0.fffffffffffffp-1022" #x000fffffffffffff)
                 ;; The user reported failing case
                 ("0x1.f0195cb356b8fp-1022" #x001f0195cb356b8f))))
    (dolist (test cases)
      (destructuring-bind (str expected-bits) test
        (let* ((parsed (parse-hex-float str))
               (actual-bits (multiple-value-bind (hi lo) (kernel:double-float-bits parsed)
                              (logior (ash (ldb (byte 32 0) hi) 32) lo))))
          (format t "  Testing ~A...~%" str)
          (unless (= actual-bits expected-bits)
            (error "Cliff Mismatch!~%Str: ~A~%Expected: ~16,'0X~%Actual:   ~16,'0X" 
                   str expected-bits actual-bits))))))
  (format t "Cliff tests passed.~%"))

;; Test precision around the smallest normalized and larges denormal boundary.
(define-test hex-parse-denormal-boundary
  (let ((cases '(;; Smallest normalized number (2^-1022)
                 ("0x1.0000000000000p-1022" #x0010000000000000)
                 ;; Smallest normalized + 1 ULP
                 ("0x1.0000000000001p-1022" #x0010000000000001)
                 ;; Smallest normalized - 1 ULP (Largest subnormal)
                 ("0x0.fffffffffffffp-1022" #x000fffffffffffff)
                 ;; The user reported failing case
                 ("0x1.f0195cb356b8fp-1022" #x001f0195cb356b8f)
		 ;; Failing case 1: 0x0.10534ec00dae8p-1022
                 ("0x0.10534ec00dae8p-1022" #x00010534ec00dae8)
                 ;; Failing case 2: 0x0.49df16729d954p-1022
                 ("0x0.49df16729d954p-1022" #x00049df16729d954))))
    (dolist (test cases)
      (destructuring-bind (str expected-bits) test
        (let* ((parsed (ext:parse-hex-float str))
               (actual-bits (multiple-value-bind (hi lo)
				(kernel:double-float-bits parsed)
                              (logior (ash (ldb (byte 32 0) hi) 32) lo))))
	  (assert-equal expected-bits actual-bits
			str))))))
