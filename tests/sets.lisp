;; Tests for sets

(defpackage :sets-tests
  (:use :cl :lisp-unit))

(in-package "SETS-TESTS")

(define-test set-diff.hash-eql
    (:tag :issues)
  ;; For set-difference to use hashtables by making the threshold
  ;; small.
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '(2 2 1)
		  (set-difference '(1 2 2 3) '(3 4)))
    (assert-equal '(2 2 1)
		  (set-difference '(1 2 2 3) '(3 4 5 6 7 8)))
    (assert-equal '(2 2 1)
		  (set-difference '(1 2 2 3) '(3 4)
				  :test #'eql))
    (assert-equal '(2 2 1)
		  (set-difference '(1 2 2 3) '(3 4 5 6 7 8)
				  :test #'eql))))

(define-test set-diff.hash-eq
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '(b b a)
		  (set-difference '(a b b c) '(c d e) :test 'eq))
    (assert-equal '(b b a)
		  (set-difference '(a b b c) '(c d e f g h) :test 'eq))
    (assert-equal '(b b a)
		  (set-difference '(a b b c) '(c d e) :test #'eq))
    (assert-equal '(b b a)
		  (set-difference '(a b b c) '(c d e f g h) :test #'eq))))

(define-test set-diff.hash-equal
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "c")
				  '("c" "d" "e")
				  :test 'equal))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "c")
				  '("c" "d" "e" "f" "g" "h")
				  :test 'equal))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "c")
				  '("c" "d" "e")
				  :test #'equal))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "c")
				  '("c" "d" "e" "f" "g" "h")
				  :test #'equal))))

(define-test set-diff.hash-equalp
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "c")
				  '("C" "d" "e")
				  :test 'equalp))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "C")
				  '("c" "D" "e" "f" "g" "h")
				  :test 'equalp))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "c")
				  '("C" "d" "e")
				  :test #'equalp))
    (assert-equal '("b" "b" "a")
		  (set-difference '("a" "b" "b" "C")
				  '("c" "D" "e" "f" "g" "h")
				  :test #'equalp))))

;; Simple test that we handle a key correctly
(define-test set-diff.hash-eql-with-key
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '((3 "b") (2 "b"))
		  (set-difference '((1 "a") (2 "b") (3 "b"))
				  '((1 "a") (4 "c") (5 "d"))
				  :key #'first))))

(define-test set-diff.test-and-test-not
  (assert-error 'simple-error
		(set-difference '(1 2)
				'(3 4)
				:test 'eql
				:test-not 'eql)))

   
