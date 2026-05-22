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
