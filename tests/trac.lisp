;;; Tests from trac tickets

(defpackage :trac-tests
  (:use :cl :lisp-unit))

(in-package "TRAC-TESTS")

(define-test trac.1
  (:tag :trac)
  (assert-false
   (let (failures)
     (dotimes (k 1000)
       (let ((x (random 1d-3)))
	 (let ((s (prin1-to-string x))
	       (f (format nil "~E" x)))
	 (unless (string-equal s f)
	   (push (list x s f)
		 failures)))))
     failures)))

(define-test trac.8
  (:tag :trac)
  (assert-false
   (let (failures)
     (dolist (base (list nil 2 2.0 2d0
			 (ash 1 99) (ash 1 3000)
			 8/7 (/ (ash 1 3000) 7)))
       (dolist (number (list 100 100.0 100d0
			     (ash 1 100) (ash 1 3500)
			     50/7 (/ (ash 1 3500) 7)))
	 (multiple-value-bind (result cond)
	     (ignore-errors (if base
				(log number base)
				(log number)))
	   (unless result
	     (push (list number base cond)
		   failures)))))
     failures)))

(define-test trac.10
  (:tag :trac)
  (assert-equal '(536870912 0.5d0)
		(multiple-value-list (round (+ 536870911 1.5d0)))))

(define-test trac.11
  (:tag :trac)
  (assert-true (eql 0w0 0w0)))

(define-test trac.12
  (:tag :trac)
  (assert-equal "   0.1D-05"
		 (format nil "~10,1,2,0,'*,,'DE" 1d-6)))

(define-test trac.13
  (:tag :trac)
  (assert-equal "  0.100E+01"
		(format nil "~11,3,2,0,'*,,'EE" .9999)))

(define-test trac.50
  (:tag :trac)
  (assert-equal "#P(:DIRECTORY (:ABSOLUTE \"tmp\" \"\" \"a\" \"\" \"b\"))"
		(princ (make-pathname :directory '(:absolute "tmp" "" "a" "" "b")))))

(defparameter *trac.70* (make-string 40 :initial-element #\A))

(compile 'trac.70-test
	 (lambda (workspace s)
	   (declare (simple-string workspace s))
	   (replace workspace s :start1 1 :end1 5 :start2 1 :end2 5)))

(define-test trac.76
  (:tag :trac)
  (assert-equal "A1234AAAA"
		(subseq (trac.70-test *trac.70* "a12345") 0 9)))

(define-test trac.80
  (:tag :trac)
  ;; The following formats should not signal an error.
  (assert-true (ignore-errors (format nil "~ve" 21 5d-234)))
  (assert-true (ignore-errors (format nil "~ve" 100 5d-234))))



  