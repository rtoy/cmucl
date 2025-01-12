;;; Tests for external formats

(defpackage :external-formats-tests
  (:use :cl :lisp-unit))

(in-package "EXTERNAL-FORMATS-TESTS")

(defparameter *test-iso8859-1*
  (let ((rs (kernel::make-random-object :state (kernel::init-random-state 27182828))))
    (lisp::codepoints-string
     (loop for k from 0 below 1000
	   collect (random 256))))
  "Random test string with ISO8859-1 characters")

(defparameter *test-unicode*
  (let ((rs (kernel::make-random-object :state (kernel::init-random-state 27182828))))
    (lisp::codepoints-string
     (loop for k from 0 below 1000
	   collect (random 20000))))
  "Random test string with codepoints below 20000")


(defmacro test-octet-count (string format)
  `(assert-equal (length (stream:string-to-octets ,string :external-format ,format))
		 (stream::string-octet-count ,string :external-format ,format)))

(define-test octet-count.iso8859-1
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-1))

(define-test octet-count.ascii
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :ascii))

(define-test octet-count.utf-8
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-8))

#+nil
(define-test octet-count.utf-16
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-16))

(define-test octet-count.utf-16-be
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-16-be))

(define-test octet-count.utf-16-le
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-16-le))

#+nil
(define-test octet-count.utf-32
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-32))

(define-test octet-count.utf-32-le
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-32-le))

(define-test octet-count.utf-32-le
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-32-le))





