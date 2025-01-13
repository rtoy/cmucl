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
  "Test that STRING-OCTET-COUNT returns the correct number of octets"
  ;; We expect STRING-OCTET-COUNT returns the same number of octets
  ;; that are produced by STRING-TO-OCTETS.
  `(multiple-value-bind (octets count converted)
       (stream:string-to-octets ,string :external-format ,format)
     ;; While we're at it, make sure that the length of the octet
     ;; buffer matches returned count.  And make sure we converted all
     ;; the characters in the string.
     (assert-equal (length octets) count)
     (assert-equal (length ,string) converted)
     ;; Finally, make sure that STRING-OCTET-COUNT returns the same
     ;; number of octets from STRING-TO-OCTETS.
     (assert-equal (length octets)
		   (stream::string-octet-count ,string :external-format ,format))))

(define-test octet-count.iso8859-1
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-1))

(define-test octet-count.ascii
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :ascii))

(define-test octet-count.ascii.error
    (:tag :octet-count)
  (assert-error 'simple-error
		(stream::string-octet-count *test-iso8859-1*
					    :external-format :ascii
					    :error 'error)))

(define-test octet-count.utf-8
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-8))

(define-test octet-count.utf-16
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-16))

(define-test octet-count.utf-16-be
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-16-be))

(define-test octet-count.utf-16-le
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-16-le))

(define-test octet-count.utf-32
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-32))

(define-test octet-count.utf-32-le
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-32-le))

(define-test octet-count.utf-32-le
    (:tag :octet-count)
  (test-octet-count *test-unicode* :utf-32-le))

(define-test octet-count.euc-kr
    (:tag :octet-count)
  (test-octet-count *test-unicode* :euc-kr))

(define-test octet-count.iso8859-2
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-2))

(define-test octet-count.iso8859-3
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-3))

(define-test octet-count.iso8859-4
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-4))

(define-test octet-count.iso8859-5
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-5))

(define-test octet-count.iso8859-6
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-6))

(define-test octet-count.iso8859-7
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-7))

(define-test octet-count.iso8859-8
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-8))

(define-test octet-count.iso8859-10
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-10))

(define-test octet-count.iso8859-13
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-13))

(define-test octet-count.iso8859-14
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-14))

(define-test octet-count.iso8859-15
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :iso8859-15))

(define-test octet-count.mac-roman
    (:tag :octet-count)
  (test-octet-count *test-iso8859-1* :mac-roman))
  

