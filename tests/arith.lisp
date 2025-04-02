;; Test x86 multiplication by small constants

(defpackage :arith-tests
  (:use :cl :lisp-unit))

(in-package "ARITH-TESTS")

#+x86
(macrolet
    ((frob (c arg expected)
       (let ((test-name-fixnum (intern (format nil "X86-TEST-CONST-MUL-FIXNUM-~D" c))))
	 `(define-test ,test-name-fixnum
	      (:tag :x86-arith)
	    (assert-eql ,expected
			(funcall (compile nil #'(lambda (x)
						  (declare (type (signed-byte 24) x))
						  (* x ,c)))
				 ,arg))))))
  ;; This is a test of #397.  Test that the multiplication by small
  ;; constants is correct for each of the constants defined for the
  ;; fixnum-*-c/fixnum=>fixnum vop.

  (frob 2 10 20)
  (frob 2 -10 -20)
  (frob 3 10 30)
  (frob 3 -10 -30)
  (frob 4 10 40)
  (frob 4 -10 -40)
  (frob 5 10 50)
  (frob 5 -10 -50)
  (frob 6 10 60)
  (frob 6 -10 -60)
  (frob 7 10 70)
  (frob 7 -10 -70)
  (frob 8 10 80)
  (frob 8 -10 -80)
  (frob 9 10 90)
  (frob 9 -10 -90)
  (frob 10 10 100)
  (frob 10 -10 -100)
  (frob 11 10 110)
  (frob 11 -10 -110)
  (frob 12 10 120)
  (frob 12 -10 -120)
  (frob 13 10 130)
  (frob 13 -10 -130))
