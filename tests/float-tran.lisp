;; Tests for various float transformations.

(defpackage :float-tran-tests
  (:use :cl :lisp-unit))

(in-package "FLOAT-TRAN-TESTS")

(define-test decode-float-sign
  "Test type derivation of the sign from decode-float"
  (assert-equalp (c::make-member-type :members (list 1f0 -1f0))
		 (c::decode-float-sign-derive-type-aux
		  (c::specifier-type 'single-float)))
  (assert-equalp (c::make-member-type :members (list 1d0 -1d0))
		 (c::decode-float-sign-derive-type-aux
		  (c::specifier-type 'double-float)))
  (assert-equalp (c::make-member-type :members (list 1f0))
		 (c::decode-float-sign-derive-type-aux
		  (c::specifier-type '(single-float (0f0))))))

(define-test decode-float-exp
  "Test type derivation of the exponent from decode-float"
  (assert-equalp (c::specifier-type '(integer -148 128))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type 'single-float)))
  (assert-equalp (c::specifier-type '(integer -1073 1024))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type 'double-float)))
  #+double-double
  (assert-equalp (c::specifier-type '(integer -1073 1024))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type 'kernel:double-double-float)))
  (assert-equalp (c::specifier-type '(integer 2 8))
		 (c::decode-float-exp-derive-type-aux
		  (c::specifier-type '(double-float 2d0 128d0)))))
				    
(define-test log2-single-transform
  "Test tranform of (log x 2) to (kernel::log2 x)"
  (let ((test-fun
	  (compile nil
		   (lambda (x)
		     (declare (type (single-float (0f0)) x))
		     (log x 2)))))
    ;; test-fun should have transformed (log x 2) to kernel::log2
    (assert-true (search "log2" (with-output-to-string (*standard-output*)
				  (disassemble test-fun)))))
  (let ((test-fun
	  (compile nil
		   (lambda (x)
		     (declare (type (single-float 0f0) x))
		     (log x 2)))))
    ;; test-fun should not have transformed (log x 2) to kernel::log2
    ;; because x can be -0 for which log should return a complex
    ;; result.
    (assert-false (search "log2" (with-output-to-string (*standard-output*)
				   (disassemble test-fun)))))
  (let ((test-fun
	  (compile nil
		   (lambda (x)
		     (declare (type (single-float 0f0) x))
		     (log x 2d0)))))
    ;; test-fun should not have transformed (log x 2) to kernel::log2
    ;; because the result should be a double due to floating-point
    ;; contagion.
    (assert-false (search "log2" (with-output-to-string (*standard-output*)
				   (disassemble test-fun))))))

(define-test log2-double-transform
  "Test tranform of (log x 2) to (kernel::log2 x)"
  (let ((test-fun-good
	  (compile nil
		   (lambda (x)
		     (declare (type (double-float (0d0)) x))
		     (log x 2)))))
    ;; test-fun should have transformed (log x 2) to kernel::log2
    (assert-true (search "log2" (with-output-to-string (*standard-output*)
				  (disassemble test-fun-good)))))
  (let ((test-fun-bad
	  (compile nil
		   (lambda (x)
		     (declare (type (double-float 0d0) x))
		     (log x 2)))))
    ;; test-fun should not have transformed (log x 2) to kernel::log2
    ;; because x can be -0 for which log should return a complex
    ;; result.
    (assert-false (search "log2" (with-output-to-string (*standard-output*)
				   (disassemble test-fun-bad)))))
  (let ((test-fun-good-2
	  (compile nil
		   (lambda (x)
		     (declare (type (double-float (0d0)) x))
		     (log x 2f0)))))
    ;; test-fun should have transformed (log x 2) to kernel::log2
    (assert-true (search "log2" (with-output-to-string (*standard-output*)
				  (disassemble test-fun-good-2))))))

(define-test log10-single-transform
  "Test tranform of (log x 10) to (kernel::log2 x)"
  (let ((test-fun-good
	  (compile nil
		   (lambda (x)
		     (declare (type (single-float (0f0)) x))
		     (log x 10)))))
    ;; test-fun should have transformed (log x 2) to kernel:%log10
    (assert-true (search "log10" (with-output-to-string (*standard-output*)
				  (disassemble test-fun-good)))))
  (let ((test-fun-bad
	  (compile nil
		   (lambda (x)
		     (declare (type (single-float 0f0) x))
		     (log x 10)))))
    ;; test-fun should not have transformed (log x 2) to kernel:%log10
    ;; because x can be -0 for which log should return a complex
    ;; result.
    (assert-false (search "log10" (with-output-to-string (*standard-output*)
				   (disassemble test-fun-bad)))))
  (let ((test-fun-bad-2
	  (compile nil
		   (lambda (x)
		     (declare (type (single-float (0f0)) x))
		     (log x 10d0)))))
    ;; test-fun should not have transformed (log x 2) to kernel:%log10
    ;; because the result should be a double due to floating-point
    ;; contagion.
    (assert-false (search "log10" (with-output-to-string (*standard-output*)
				   (disassemble test-fun-bad-2))))))

(define-test log10-double-transform
  "Test tranform of (log x 10) to (kernel:%log10 x)"
  (let ((test-fun-good
	  (compile nil
		   (lambda (x)
		     (declare (type (double-float (0d0)) x))
		     (log x 10)))))
    ;; test-fun should have transformed (log x 10) to kernel:%log10
    (assert-true (search "log10" (with-output-to-string (*standard-output*)
				  (disassemble test-fun-good)))))
  (let ((test-fun-bad
	  (compile nil
		   (lambda (x)
		     (declare (type (double-float 0d0) x))
		     (log x 10)))))
    ;; test-fun should not have transformed (log x 10) to kernel:%log10
    ;; because x can be -0 for which log should return a complex
    ;; result.
    (assert-false (search "log10" (with-output-to-string (*standard-output*)
				   (disassemble test-fun-bad)))))
  (let ((test-fun-good-2
	  (compile nil
		   (lambda (x)
		     (declare (type (double-float (0d0)) x))
		     (log x 10f0)))))
    ;; test-fun should have transformed (log x 10) to kernel:%log10
    (assert-true (search "log10" (with-output-to-string (*standard-output*)
				   (disassemble test-fun-good-2))))))

(define-test scale-float-transform.single
  (let ((xfrm-scale
	  (compile nil
		   (lambda (x n)
		     (declare (single-float x)
			      (type (integer -149 127) n))
		     (scale-float x n))))
	(scale
	  (compile nil
		   (lambda (x n)
		     (declare (single-float x)
			      (type (signed-byte 32) n))
		     (scale-float x n)))))
    ;; If the deftransform for scale-float was applied, (scale-float
    ;; most-positive-single-float 2) is done as a multiplication which
    ;; will overflow.  The operation will be '*.  If the deftransform
    ;; was not applied, the overflow will still be signaled, but the
    ;; operation will be 'scale-float.
    (assert-eql '*
		(handler-case 
		    (funcall xfrm-scale most-positive-single-float 2)
		  (arithmetic-error (c)
	            (arithmetic-error-operation c))))
    (assert-eql 'scale-float
		(handler-case
		    (funcall scale most-positive-single-float 2)
		  (arithmetic-error (c)
		    (arithmetic-error-operation c))))))

(define-test scale-float-transform.double
  (let ((xfrm-scale
	  (compile nil
		   (lambda (x n)
		     (declare (double-float x)
			      (type (integer -1074 1023) n))
		     (scale-float x n))))
	(scale
	  (compile nil
		   (lambda (x n)
		     (declare (double-float x)
			      (type (signed-byte 32) n))
		     (scale-float x n)))))
    ;; If the deftransform for scale-float was applied, (scale-float
    ;; most-positive-double-float 2) is done as a multiplication which
    ;; will overflow.  The operation will be '*.  If the deftransform
    ;; was not applied, the overflow will still be signaled, but the
    ;; operation will be 'scale-float.
    (assert-eql '*
		(handler-case 
		    (funcall xfrm-scale most-positive-double-float 2)
		  (arithmetic-error (c)
	            (arithmetic-error-operation c))))
    (assert-eql 'scale-float
		(handler-case
		    (funcall scale most-positive-double-float 2)
		  (arithmetic-error (c)
		    (arithmetic-error-operation c))))))
