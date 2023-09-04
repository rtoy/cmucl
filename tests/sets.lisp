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


(define-test nset-diff.1
    (:tag :issues)
  ;; From CLHS
  (flet 
      ((test1 (min-length-limit)
         (let ((lisp::*min-list-length-for-hashtable* min-length-limit)
               (lst1 (list "A" "b" "C" "d"))
               (lst2 (list "a" "B" "C" "d")))
           (assert-equal '("b" "A")
                         (nset-difference lst1 lst2 :test 'equal))
           ;; This isn't specified by the CLHS, but it is what we do.
           (assert-equal '("A") lst1))))
    (test1 100)
    (test1 1)))

(define-test nset-diff.key
    (:tag :issues)
  (flet
      ((test (min-length-limit)
         ;; From CLHS
         (let ((lisp::*min-list-length-for-hashtable* min-length-limit)
               (lst1 (list '("a" . "b") '("c" . "d") '("e" . "f")))
               (lst2 (list '("c" . "a") '("e" . "b") '("d" . "a"))))
           (assert-equal '(("e" . "f") ("c" . "d"))
                         (nset-difference lst1 lst2 :test 'equal :key #'cdr))
           ;; This isn't specified by the CLHS, but it is what we do.
           (assert-equal '(("a" . "b") ("c" . "d")) lst1))))
    (test 100)
    (test 1)))
  
(define-test union.hash-eql
    (:tag :issues)
  ;; For union to use hashtables by making the threshold
  ;; small.
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '(2 2 1 3 4)
		  (union '(1 2 2 3) '(3 4)))
    (assert-equal '(2 2 1 3 4 5 6 7 8)
		  (union '(1 2 2 3) '(3 4 5 6 7 8)))
    (assert-equal '(2 2 1 3 4)
		  (union '(1 2 2 3) '(3 4)
			 :test #'eql))
    (assert-equal '(2 2 1 3 4 5 6 7 8)
		  (union '(1 2 2 3) '(3 4 5 6 7 8)
			 :test #'eql))))

(define-test union.hash-eq
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '(b b a c d e)
		  (union '(a b b c) '(c d e) :test 'eq))
    (assert-equal '(b b a c d e f g h)
		  (union '(a b b c) '(c d e f g h) :test 'eq))
    (assert-equal '(b b a c d e)
		  (union '(a b b c) '(c d e) :test #'eq))
    (assert-equal '(b b a c d e f g h)
		  (union '(a b b c) '(c d e f g h) :test #'eq))))

(define-test union.hash-equal
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '("b" "b" "a" "c" "d" "e")
		  (union '("a" "b" "b" "c")
			 '("c" "d" "e")
			 :test 'equal))
    (assert-equal '("b" "b" "a" "c" "d" "e" "f" "g" "h")
		  (union '("a" "b" "b" "c")
			 '("c" "d" "e" "f" "g" "h")
			 :test 'equal))
    (assert-equal '("b" "b" "a" "c" "d" "e")
		  (union '("a" "b" "b" "c")
			 '("c" "d" "e")
			 :test #'equal))
    (assert-equal '("b" "b" "a" "c" "d" "e" "f" "g" "h")
		  (union '("a" "b" "b" "c")
			 '("c" "d" "e" "f" "g" "h")
			 :test #'equal))))

(define-test union.hash-equalp
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '("b" "b" "a" "C" "d" "e")
		  (union '("a" "b" "b" "c")
			 '("C" "d" "e")
			 :test 'equalp))
    (assert-equal '("b" "b" "a" "c" "D" "e" "f" "g" "h")
		  (union '("a" "b" "b" "C")
			 '("c" "D" "e" "f" "g" "h")
			 :test 'equalp))
    (assert-equal '("b" "b" "a" "C" "d" "e")
		  (union '("a" "b" "b" "c")
			 '("C" "d" "e")
			 :test #'equalp))
    (assert-equal '("b" "b" "a" "c" "D" "e" "f" "g" "h")
		  (union '("a" "b" "b" "C")
			 '("c" "D" "e" "f" "g" "h")
			 :test #'equalp))))

;; Simple test that we handle a key correctly
(define-test union.hash-eql-with-key
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-equal '((3 "b") (2 "b") (1 "a") (4 "c") (5 "d"))
		  (union '((1 "a") (2 "b") (3 "b"))
			 '((1 "a") (4 "c") (5 "d"))
			 :key #'first))))

(define-test union.test-and-test-not
  (assert-error 'simple-error
		(union '(1 2)
		       '(3 4)
		       :test 'eql
		       :test-not 'eql)))

(define-test nunion.1
    (:tag :issues)
  (flet
      ((test (min-list-length)
         (let ((lisp::*min-list-length-for-hashtable* min-list-length)
               (lst1 (list 1 2 '(1 2) "a" "b"))
               (lst2 (list 2 3 '(2 3) "B" "C")))
           (assert-equal '("b" "a" (1 2) 1 2 3 (2 3) "B" "C")
                         (nunion lst1 lst2))
           (assert-equal '(1 2 3 (2 3) "B" "C")
                         lst1))))
    (test 100)
    (test 1)))

(define-test nintersection.1
    (:tag :issues)
  (flet
      ((test (min-list-length)
         (let ((lisp::*min-list-length-for-hashtable* min-list-length)
               (lst1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
               (lst2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
           (assert-equal '(c b 4 1 1)
                         (nintersection lst1 lst2))
           (assert-equal '(1) lst1))))
    (test 100)
    (test 1)))


(define-test subsetp.hash-eq
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-true
     (subsetp '(a b c a) '(a a d d c b) :test 'eq))
    (assert-true
     (subsetp '(a b c a b c a b c) '(a a d d c b) :test 'eq))
    (assert-false
     (subsetp '(a b c a z) '(a a d d c b) :test 'eq))
    (assert-false
     (subsetp '(a b c a b cz) '(a a d d c b) :test 'eq))))

(define-test subsetp.hash-eql
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-true
     (subsetp '(a b c a) '(a a d d c b) :test 'eql))
    (assert-false
     (subsetp '(a b c a z) '(a a d d c b) :test 'eql))))

(define-test subsetp.hash-equal
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-true
     (subsetp '("a" "b" "c" "a") '("a" "a" "d" "d" "c" "b") :test 'equal))
    (assert-false
     (subsetp '("a" "b" "c" "a" "z") '("a" "a" "d" "d" "c" "b") :test 'equal))))

(define-test subsetp.hash-equalp
    (:tag :issues)
  (let ((lisp::*min-list-length-for-hashtable* 2))
    (assert-true
     (subsetp '("a" "b" "C" "A") '("a" "a" "d" "d" "c" "b") :test 'equalp))
    (assert-false
     (subsetp '("a" "b" "C" "A" "z") '("a" "a" "d" "d" "c" "b") :test 'equalp))))

(define-test subsetp.hash-eql-with-key
    (:tag :issues)
  (assert-true (subsetp '((1 "a") (2 "b") (3 "c"))
                        '((3 "c") (3 "c") (2 "b") (1 "a"))
                        :test 'eql
                        :key #'first)))

(define-test subsetp.test-and-test-not
  (assert-error 'simple-error
                (subsetp '(1 2)
                         '(3 4)
                         :test 'eql
                         :test-not 'equal)))

(define-test set-exclusive-or.1
  (:tag :issues)
  (flet
      ((test (min-length)
         ;; From CLHS
         (let ((lisp::*min-list-length-for-hashtable* min-length))
           (assert-equal '("b" "A" "b" "a")
                         (set-exclusive-or '(1 "a" "b")
                                           '(1 "A" "b")))
           (assert-equal '("A" "a")
                         (set-exclusive-or '(1 "a" "b")
                                           '(1 "A" "b")
                                           :test #'equal))
           (assert-equal nil
                         (set-exclusive-or '(1 "a" "b")
                                           '(1 "A" "b")
                                           :test #'equalp)))))
    ;; Test the list impl by making the min length large.  Then test
    ;; the hashtable impl with a very short min length
    (test 100)
    (test 2)))
  
