;; Tests of float functions

(defpackage :float-x86-tests
  (:use :cl :lisp-unit))

(in-package "FLOAT-X86-TESTS")

(define-test set-floating-point-modes
  (let ((old-x87-modes (x86::x87-floating-point-modes))
	(old-sse2-modes (x86::sse2-floating-point-modes))
	x87-modes sse2-modes)
    (unwind-protect
	 (progn
	   ;; Set some new traps and rounding mode.
	   (ext:set-floating-point-modes :traps '(:underflow
						  :overflow
						  :invalid
						  :divide-by-zero)
					 :rounding-mode :zero)
	   ;; Save these new FP modes
	   (setf x87-modes (x86::x87-floating-point-modes))
	   (setf sse2-modes (x86::sse2-floating-point-modes)))

      (setf (x86::x87-floating-point-modes) old-x87-modes)
      (setf (x86::sse2-floating-point-modes) old-sse2-modes))

    (let* ((x87-exceptions-enabled (ldb x86::x87-float-traps-byte x87-modes))
	   (x87-rc (ldb x86::x87-float-rounding-mode x87-modes))
	   (sse2-exceptions-enabled (ldb x86::float-traps-byte sse2-modes))
	   (sse2-rc (ldb x86::float-rounding-mode sse2-modes)))
      (format t "*X87 FP mode words:~%")
      (x86::print-x87-fp-modes x87-modes)
      (format t "~%*SSE2 FP mode words:~%")
      (x86::print-sse2-fp-modes sse2-modes)
      
      ;; Verify that we set the enabled exceptions
      ;; correctly. First for sse2, then for x87.
      (assert-false (logbitp 5 sse2-exceptions-enabled)) ; precision
      (assert-true (logbitp 4 sse2-exceptions-enabled))	 ; underflow
      (assert-true (logbitp 3 sse2-exceptions-enabled))	 ; overflow
      (assert-true (logbitp 2 sse2-exceptions-enabled))	; divide-by-zero
      (assert-false (logbitp 1 sse2-exceptions-enabled)) ; denormal
      (assert-true (logbitp 0 sse2-exceptions-enabled))	 ; invalid
	   
      (assert-false (logbitp 5 x87-exceptions-enabled))	; precision
      (assert-true (logbitp 4 x87-exceptions-enabled))	; underflow
      (assert-true (logbitp 3 x87-exceptions-enabled))	; overflow
      (assert-true (logbitp 2 x87-exceptions-enabled)) ; divide-by-zero
      (assert-false (logbitp 1 x87-exceptions-enabled))	; denormal
      (assert-true (logbitp 0 x87-exceptions-enabled))	; invalid

      ;; Verify the rounding mode is set to zero
      (assert-eql :zero (car (rassoc sse2-rc x86::rounding-mode-alist)))
      (assert-eql :zero (car (rassoc x87-rc x86::rounding-mode-alist)))

      ;; Verify precision for x87
      (assert-eql :64-bits
		  (car (rassoc (ldb x86::x87-float-precision-control-byte x87-modes)
			       x86::x87-float-precision-control-alist))))))
