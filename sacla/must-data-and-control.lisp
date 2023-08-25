(in-package #:sacla-lisp-unit)
(define-test sacla-must-data-and-control.1 (:tag :sacla)
 (assert-true
  (let (a b c)
    (and (null (psetq a 1 b 2 c 3)) (eql a 1) (eql b 2) (eql c 3)))))
(define-test sacla-must-data-and-control.2 (:tag :sacla)
 (assert-true
  (let ((a 1) (b 2) (c 3))
    (and (null (psetq a (1+ b) b (1+ a) c (+ a b)))
         (eql a 3)
         (eql b 2)
         (eql c 3)))))
(define-test sacla-must-data-and-control.3 (:tag :sacla)
 (assert-true
  (let ((x (list 10 20 30)))
    (symbol-macrolet ((y (car x)) (z (cadr x)))
      (psetq y (1+ z) z (1+ y))
      (equal (list x y z) '((21 11 30) 21 11))))))
(define-test sacla-must-data-and-control.4 (:tag :sacla)
 (assert-true
  (let ((a 1) (b 2))
    (and (null (psetq a b b a)) (eql a 2) (eql b 1)))))
(define-test sacla-must-data-and-control.5 (:tag :sacla)
 (assert-true (null (psetq))))
(define-test sacla-must-data-and-control.6 (:tag :sacla)
 (assert-true
  (let ((a nil))
    (and (null (psetq a t)) (eq a t)))))
(define-test sacla-must-data-and-control.7 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and (null (psetq a b b a)) (eq a 1) (eq b 0)))))
(define-test sacla-must-data-and-control.8 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1) (c 2))
    (and (null (psetq a b b c c a)) (eq a 1) (eq b 2) (eq c 0)))))
(define-test sacla-must-data-and-control.9 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1) (c 2) (d 3))
    (and (null (psetq a b b c c d d a)) (eq a 1) (eq b 2) (eq c 3) (eq d 0)))))
(define-test sacla-must-data-and-control.10 (:tag :sacla)
 (assert-true (null (block nil (return) 1))))
(define-test sacla-must-data-and-control.11 (:tag :sacla)
 (assert-true (eql (block nil (return 1) 2) 1)))
(define-test sacla-must-data-and-control.12 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (block nil (return (values 1 2)) 3)) '(1 2))))
(define-test sacla-must-data-and-control.13 (:tag :sacla)
 (assert-true (eql (block nil (block alpha (return 1) 2)) 1)))
(define-test sacla-must-data-and-control.14 (:tag :sacla)
 (assert-true (eql (block alpha (block nil (return 1)) 2) 2)))
(define-test sacla-must-data-and-control.15 (:tag :sacla)
 (assert-true (eql (block nil (block nil (return 1) 2)) 1)))
(define-test sacla-must-data-and-control.16 (:tag :sacla)
 (assert-true (eq (dotimes (i 10 nil) (return t)) t)))
(define-test sacla-must-data-and-control.17 (:tag :sacla)
 (assert-true
  (eq
   (dolist (elt (list 0 1 2 3) nil)
     (when (numberp elt)
       (return t)))
   t)))
(define-test sacla-must-data-and-control.18 (:tag :sacla)
 (assert-true (not nil)))
(define-test sacla-must-data-and-control.19 (:tag :sacla)
 (assert-true (not 'nil)))
(define-test sacla-must-data-and-control.20 (:tag :sacla)
 (assert-true (not (integerp 'sss))))
(define-test sacla-must-data-and-control.21 (:tag :sacla)
 (assert-true (null (not (integerp 1)))))
(define-test sacla-must-data-and-control.22 (:tag :sacla)
 (assert-true (null (not 3.7))))
(define-test sacla-must-data-and-control.23 (:tag :sacla)
 (assert-true (null (not 'apple))))
(define-test sacla-must-data-and-control.24 (:tag :sacla)
 (assert-true (not nil)))
(define-test sacla-must-data-and-control.25 (:tag :sacla)
 (assert-true (null (not t))))
(define-test sacla-must-data-and-control.26 (:tag :sacla)
 (assert-true (not (cdr '(a)))))
(define-test sacla-must-data-and-control.27 (:tag :sacla)
 (assert-true (equal 'a 'a)))
(define-test sacla-must-data-and-control.28 (:tag :sacla)
 (assert-true (not (equal 'a 'b))))
(define-test sacla-must-data-and-control.29 (:tag :sacla)
 (assert-true (equal 'abc 'abc)))
(define-test sacla-must-data-and-control.30 (:tag :sacla)
 (assert-true (equal 1 1)))
(define-test sacla-must-data-and-control.31 (:tag :sacla)
 (assert-true (equal 2 2)))
(define-test sacla-must-data-and-control.32 (:tag :sacla)
 (assert-true (equal 0.1 0.1)))
(define-test sacla-must-data-and-control.33 (:tag :sacla)
 (assert-true (equal 1/3 1/3)))
(define-test sacla-must-data-and-control.34 (:tag :sacla)
 (assert-true (not (equal 0 1))))
(define-test sacla-must-data-and-control.35 (:tag :sacla)
 (assert-true (not (equal 1 1.0))))
(define-test sacla-must-data-and-control.36 (:tag :sacla)
 (assert-true (not (equal 1/3 1/4))))
(define-test sacla-must-data-and-control.37 (:tag :sacla)
 (assert-true (equal #\a #\a)))
(define-test sacla-must-data-and-control.38 (:tag :sacla)
 (assert-true (equal #\b #\b)))
(define-test sacla-must-data-and-control.39 (:tag :sacla)
 (assert-true (not (equal #\b #\B))))
(define-test sacla-must-data-and-control.40 (:tag :sacla)
 (assert-true (not (equal #\C #\c))))
(define-test sacla-must-data-and-control.41 (:tag :sacla)
 (assert-true (equal '(0) '(0))))
(define-test sacla-must-data-and-control.42 (:tag :sacla)
 (assert-true (equal '(0 #\a) '(0 #\a))))
(define-test sacla-must-data-and-control.43 (:tag :sacla)
 (assert-true (equal '(0 #\a x) '(0 #\a x))))
(define-test sacla-must-data-and-control.44 (:tag :sacla)
 (assert-true (equal '(0 #\a x (0)) '(0 #\a x (0)))))
(define-test sacla-must-data-and-control.45 (:tag :sacla)
 (assert-true
  (equal '(0 #\a x (0 (#\a (x "abc" #*0101))))
         '(0 #\a x (0 (#\a (x "abc" #*0101)))))))
(define-test sacla-must-data-and-control.46 (:tag :sacla)
 (assert-true
  (not
   (equal (make-array '(2 2) :initial-contents '((a b) (c d)))
          (make-array '(2 2) :initial-contents '((a b) (c d)))))))
(define-test sacla-must-data-and-control.47 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 2) :initial-contents '((a b) (c d)))))
    (equal array array))))
(define-test sacla-must-data-and-control.48 (:tag :sacla)
 (assert-true (eql (identity 101) 101)))
(define-test sacla-must-data-and-control.49 (:tag :sacla)
 (assert-true
  (equal (mapcan #'identity (list (list 1 2 3) '(4 5 6))) '(1 2 3 4 5 6))))
(define-test sacla-must-data-and-control.50 (:tag :sacla)
 (assert-true (eq (identity 'x) 'x)))
(define-test sacla-must-data-and-control.51 (:tag :sacla)
 (assert-true (funcall (complement #'zerop) 1)))
(define-test sacla-must-data-and-control.52 (:tag :sacla)
 (assert-true (not (funcall (complement #'characterp) #\A))))
(define-test sacla-must-data-and-control.53 (:tag :sacla)
 (assert-true (not (funcall (complement #'member) 'a '(a b c)))))
(define-test sacla-must-data-and-control.54 (:tag :sacla)
 (assert-true (funcall (complement #'member) 'd '(a b c))))
(define-test sacla-must-data-and-control.55 (:tag :sacla)
 (assert-true (equal (mapcar (constantly 3) '(a b c d)) '(3 3 3 3))))
(define-test sacla-must-data-and-control.56 (:tag :sacla)
 (assert-true
  (let ((const-func (constantly 'xyz)))
    (every #'(lambda (arg) (eq arg 'xyz))
           (list (funcall const-func)
                 (funcall const-func 'a)
                 (funcall const-func 'a 'b)
                 (funcall const-func 'a 'b 'c)
                 (funcall const-func 'a 'b 'c 'd))))))
(define-test sacla-must-data-and-control.57 (:tag :sacla)
 (assert-true
  (let ((temp1 1) (temp2 1) (temp3 1))
    (and (eql (and (incf temp1) (incf temp2) (incf temp3)) 2)
         (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3))
         (eql (decf temp3) 1)
         (null (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3)))
         (and (eql temp1 temp2) (eql temp2 temp3))
         (and)))))
(define-test sacla-must-data-and-control.58 (:tag :sacla)
 (assert-true (eq (and) t)))
(define-test sacla-must-data-and-control.59 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (and 't 't 't (values 'a 'b 'c))) '(a b c))))
(define-test sacla-must-data-and-control.60 (:tag :sacla)
 (assert-true (null (and 't 't (cdr '(a)) (error "error")))))
(define-test sacla-must-data-and-control.61 (:tag :sacla)
 (assert-true
  (let ((temp0 nil) (temp1 10) (temp2 20) (temp3 30))
    (and (eql (or temp0 temp1 (setq temp2 37)) 10)
         (eql temp2 20)
         (eql (or (incf temp1) (incf temp2) (incf temp3)) 11)
         (eql temp1 11)
         (eql temp2 20)
         (eql temp3 30)
         (equal (multiple-value-list (or (values) temp1)) '(11))
         (equal (multiple-value-list (or (values temp1 temp2) temp3)) '(11))
         (equal (multiple-value-list (or temp0 (values temp1 temp2))) '(11 20))
         (equal
          (multiple-value-list (or (values temp0 temp1) (values temp2 temp3)))
          '(20 30))))))
(define-test sacla-must-data-and-control.62 (:tag :sacla)
 (assert-true (zerop (or '0 '1 '2))))
(define-test sacla-must-data-and-control.63 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and (eql (or (incf a) (incf a) (incf a)) 1) (eql a 1)))))
(define-test sacla-must-data-and-control.64 (:tag :sacla)
 (assert-true (equal (multiple-value-list (or (values) 1)) '(1))))
(define-test sacla-must-data-and-control.65 (:tag :sacla)
 (assert-true (equal (multiple-value-list (or (values 1 2) 3)) '(1))))
(define-test sacla-must-data-and-control.66 (:tag :sacla)
 (assert-true (null (or))))
(define-test sacla-must-data-and-control.67 (:tag :sacla)
 (assert-true (equal (multiple-value-list (or (values 0 1 2))) '(0 1 2))))
(define-test sacla-must-data-and-control.68 (:tag :sacla)
 (assert-true (equal (multiple-value-list (or nil (values 0 1 2))) '(0 1 2))))
(define-test sacla-must-data-and-control.69 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (or nil nil (values 0 1 2))) '(0 1 2))))
(define-test sacla-must-data-and-control.70 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (or nil nil nil (values 0 1 2))) '(0 1 2))))
(define-test sacla-must-data-and-control.71 (:tag :sacla)
 (assert-true
  (let ((a nil))
    (flet ((select-options ()
             (cond
               ((= a 1)
                (setq a 2))
               ((= a 2)
                (setq a 3))
               ((and (= a 3) (floor a 2)))
               (t
                (floor a 3)))))
      (and (eql (setq a 1) 1)
           (eql (select-options) 2)
           (eql a 2)
           (eql (select-options) 3)
           (eql a 3)
           (eql (select-options) 1)
           (setq a 5)
           (equal (multiple-value-list (select-options)) '(1 2)))))))
(define-test sacla-must-data-and-control.72 (:tag :sacla)
 (assert-true (null (cond))))
(define-test sacla-must-data-and-control.73 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (cond
      ((values 1 2 3))))
   '(1))))
(define-test sacla-must-data-and-control.74 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (cond
      (t
       (values 1 2 3))))
   '(1 2 3))))
(define-test sacla-must-data-and-control.75 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (cond
      (t
       (values 1)
       (values 1 2)
       (values 1 2 3))))
   '(1 2 3))))
(define-test sacla-must-data-and-control.76 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and
     (eql
      (cond
        ((incf a))
        ((incf a))
        ((incf a)))
      1)
     (eql a 1)))))
(define-test sacla-must-data-and-control.77 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and
     (eql
      (cond
        ((incf a)
         (incf a)
         (incf a))
        ((incf a)
         (incf a)
         (incf a))
        ((incf a)
         (incf a)
         (incf a)))
      3)
     (eql a 3)))))
(define-test sacla-must-data-and-control.78 (:tag :sacla)
 (assert-true
  (eq
   (when t
     'hello)
   'hello)))
(define-test sacla-must-data-and-control.79 (:tag :sacla)
 (assert-true
  (null
   (unless t
     'hello))))
(define-test sacla-must-data-and-control.80 (:tag :sacla)
 (assert-true
  (null
   (when nil
     'hello))))
(define-test sacla-must-data-and-control.81 (:tag :sacla)
 (assert-true
  (eq
   (unless nil
     'hello)
   'hello)))
(define-test sacla-must-data-and-control.82 (:tag :sacla)
 (assert-true
  (null
   (when t
    ))))
(define-test sacla-must-data-and-control.83 (:tag :sacla)
 (assert-true
  (null
   (unless nil
    ))))
(define-test sacla-must-data-and-control.84 (:tag :sacla)
 (assert-true
  (let ((x 3))
    (equal
     (list
      (when (oddp x)
        (incf x)
        (list x))
      (when (oddp x)
        (incf x)
        (list x))
      (unless (oddp x)
        (incf x)
        (list x))
      (unless (oddp x)
        (incf x)
        (list x))
      (if (oddp x) (incf x) (list x))
      (if (oddp x) (incf x) (list x))
      (if (not (oddp x)) (incf x) (list x))
      (if (not (oddp x)) (incf x) (list x)))
     '((4) nil (5) nil 6 (6) 7 (7))))))
(define-test sacla-must-data-and-control.85 (:tag :sacla)
 (assert-true
  (equal
   (let ((list nil))
     (dolist (k '(1 2 3 :four #\v nil t 'other))
       (push
        (case k
          ((1 2) 'clause1)
          (3 'clause2)
          (() 'no-keys-so-never-seen)
          ((nil) 'nilslot)
          ((:four #\v) 'clause4)
          ((t) 'tslot)
          (otherwise 'others))
        list))
     list)
   '(others tslot nilslot clause4 clause4 clause2 clause1 clause1))))
(define-test sacla-must-data-and-control.86 (:tag :sacla)
 (assert-true (macro-function 'case)))
(define-test sacla-must-data-and-control.87 (:tag :sacla)
 (assert-true (macro-function 'ccase)))
(define-test sacla-must-data-and-control.88 (:tag :sacla)
 (assert-true (macro-function 'ecase)))
(define-test sacla-must-data-and-control.89 (:tag :sacla)
 (assert-true (eql (case 'a ((a b c) 0) (x 1) (y 2) (z 3)) 0)))
(define-test sacla-must-data-and-control.90 (:tag :sacla)
 (assert-true (eql (case 'j ((a b c) 0) (x 1) (y 2) (z 3) (t 9)) 9)))
(define-test sacla-must-data-and-control.91 (:tag :sacla)
 (assert-true (eql (case 'j ((a b c) 0) (x 1) (y 2) (z 3) (otherwise 9)) 9)))
(define-test sacla-must-data-and-control.92 (:tag :sacla)
 (assert-true (eql (case 'j ((a b c) 0) (x 1) (y 2) (z 3)) nil)))
(define-test sacla-must-data-and-control.93 (:tag :sacla)
 (assert-true (null (case 'x))))
(define-test sacla-must-data-and-control.94 (:tag :sacla)
 (assert-true
  (let ((x #\a))
    (equal (case x ((#\x #\y #\z) "xyz") (#\a "a") (t "-")) "a"))))
(define-test sacla-must-data-and-control.95 (:tag :sacla)
 (assert-true
  (let ((x #\A))
    (equal (case x ((#\x #\y #\z) "xyz") (#\a "a") (t "-")) "-"))))
(define-test sacla-must-data-and-control.96 (:tag :sacla)
 (assert-true
  (let ((x t))
    (eql (case x ((t) 0) (t 1)) 0))))
(define-test sacla-must-data-and-control.97 (:tag :sacla)
 (assert-true
  (let ((x nil))
    (eql (case x ((t) 0) (t 1)) 1))))
(define-test sacla-must-data-and-control.98 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (eql (case x ((t) 0)) nil))))
(define-test sacla-must-data-and-control.99 (:tag :sacla)
 (assert-true
  (let ((x 'otherwise))
    (eql (case x ((otherwise) 0) (otherwise 1)) 0))))
(define-test sacla-must-data-and-control.100 (:tag :sacla)
 (assert-true
  (let ((x nil))
    (eql (case x ((otherwise) 0) (otherwise 1)) 1))))
(define-test sacla-must-data-and-control.101 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (eql (case x ((otherwise) 0)) nil))))
(define-test sacla-must-data-and-control.102 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (and (eql (case x ((a b c) (setq x 0) 'a) ((x y z) (setq x 1) 'x)) 'a)
         (eql x 0)))))
(define-test sacla-must-data-and-control.103 (:tag :sacla)
 (assert-true
  (let ((x 'x))
    (and (eql (case x ((a b c) (setq x 0) 'a) ((x y z) (setq x 1) 'x)) 'x)
         (eql x 1)))))
(define-test sacla-must-data-and-control.104 (:tag :sacla)
 (assert-true
  (equal
   (mapcar #'(lambda (x) (case x (a 0) (b 1) (c 2) (d 3) (e 4)))
           '(a b c d e f))
   '(0 1 2 3 4 nil))))
(define-test sacla-must-data-and-control.105 (:tag :sacla)
 (assert-true (case 'a (otherwise t))))
(define-test sacla-must-data-and-control.106 (:tag :sacla)
 (assert-true (eql (case 'a (otherwise 10)) 10)))
(define-test sacla-must-data-and-control.107 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and
     (eq
      (case
          (progn
            (incf a)
            (incf b))
        (0 'a)
        (1 'b)
        (2 'c))
      'c)
     (eql a 1)
     (eql b 2)))))
(define-test sacla-must-data-and-control.108 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and
     (eq
      (case
          (progn
            (incf a)
            (incf b))
        (0 'a)
        (1 'b)
        (2 (incf a) (incf b) 'c))
      'c)
     (eql a 2)
     (eql b 3)))))
(define-test sacla-must-data-and-control.109 (:tag :sacla)
 (assert-true
  (let ((a (list 0 1 2 3)))
    (eq (case (caddr a) (0 'x) (1 'y) (2 'z) (3 t)) 'z))))
(define-test sacla-must-data-and-control.110 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (case 2
      (0 (values 0 'x))
      (1 (values 1 'y))
      (2 (values 2 'z))
      (3 (values 3 't))))
   '(2 z))))
(define-test sacla-must-data-and-control.111 (:tag :sacla)
 (assert-true
  (let ((a 'c))
    (eql (ccase a ((a b c) 0) (x 1) (y 2) (z 3)) 0))))
(define-test sacla-must-data-and-control.112 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a 'j))
          (ccase a ((a b c) 0) (x 1) (y 2) (z 3))))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.113 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a nil))
          (ccase a)))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.114 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a #\a))
          (ccase a ((#\A #\B #\C) 0) ((#\X #\Y #\Z) 1))))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.115 (:tag :sacla)
 (assert-true
  (let ((a (list 0 1 2 3)))
    (eq (ccase (caddr a) (0 'x) (1 'y) (2 'z) (3 t)) 'z))))
(define-test sacla-must-data-and-control.116 (:tag :sacla)
 (assert-true
  (let ((x #\a))
    (equal (ccase x ((#\x #\y #\z) "xyz") (#\a "a")) "a"))))
(define-test sacla-must-data-and-control.117 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (and (eql (ccase x ((a b c) (setq x 0) 'a) ((x y z) (setq x 1) 'x)) 'a)
         (eql x 0)))))
(define-test sacla-must-data-and-control.118 (:tag :sacla)
 (assert-true
  (let ((x 'x))
    (and (eql (ccase x ((a b c) (setq x 0) 'a) ((x y z) (setq x 1) 'x)) 'x)
         (eql x 1)))))
(define-test sacla-must-data-and-control.119 (:tag :sacla)
 (assert-true
  (equal
   (mapcar #'(lambda (x) (ccase x (a 0) (b 1) (c 2) (d 3) (e 4))) '(a b c d e))
   '(0 1 2 3 4))))
(define-test sacla-must-data-and-control.120 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (let ((a 2))
      (ccase a
        (0 (values 0 'x))
        (1 (values 1 'y))
        (2 (values 2 'z))
        (3 (values 3 't)))))
   '(2 z))))
(define-test sacla-must-data-and-control.121 (:tag :sacla)
 (assert-true
  (let ((a 'c))
    (eql (ecase a ((a b c) 0) (x 1) (y 2) (z 3)) 0))))
(define-test sacla-must-data-and-control.122 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a 'j))
          (ecase a ((a b c) 0) (x 1) (y 2) (z 3))))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.123 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a nil))
          (ecase a)))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.124 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a #\a))
          (ecase a ((#\A #\B #\C) 0) ((#\X #\Y #\Z) 1))))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.125 (:tag :sacla)
 (assert-true
  (let ((a (list 0 1 2 3)))
    (eq (ecase (caddr a) (0 'x) (1 'y) (2 'z) (3 t)) 'z))))
(define-test sacla-must-data-and-control.126 (:tag :sacla)
 (assert-true
  (let ((x #\a))
    (equal (ecase x ((#\x #\y #\z) "xyz") (#\a "a")) "a"))))
(define-test sacla-must-data-and-control.127 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (and (eql (ecase x ((a b c) (setq x 0) 'a) ((x y z) (setq x 1) 'x)) 'a)
         (eql x 0)))))
(define-test sacla-must-data-and-control.128 (:tag :sacla)
 (assert-true
  (let ((x 'x))
    (and (eql (ecase x ((a b c) (setq x 0) 'a) ((x y z) (setq x 1) 'x)) 'x)
         (eql x 1)))))
(define-test sacla-must-data-and-control.129 (:tag :sacla)
 (assert-true
  (equal
   (mapcar #'(lambda (x) (ecase x (a 0) (b 1) (c 2) (d 3) (e 4))) '(a b c d e))
   '(0 1 2 3 4))))
(define-test sacla-must-data-and-control.130 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (let ((a 2))
      (ecase a
        (0 (values 0 'x))
        (1 (values 1 'y))
        (2 (values 2 'z))
        (3 (values 3 't)))))
   '(2 z))))
(define-test sacla-must-data-and-control.131 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (equal
     (typecase x
       (cons "cons")
       (symbol "symbol")
       (number "number")
       (otherwise "unknown"))
     "symbol"))))
(define-test sacla-must-data-and-control.132 (:tag :sacla)
 (assert-true
  (let ((x (list 'a)))
    (equal
     (typecase x
       (cons "cons")
       (symbol "symbol")
       (number "number")
       (otherwise "unknown"))
     "cons"))))
(define-test sacla-must-data-and-control.133 (:tag :sacla)
 (assert-true
  (let ((x 0))
    (equal
     (typecase x
       (cons "cons")
       (symbol "symbol")
       (number "number")
       (otherwise "unknown"))
     "number"))))
(define-test sacla-must-data-and-control.134 (:tag :sacla)
 (assert-true
  (let ((x (make-array '(3 3))))
    (equal
     (typecase x
       (cons "cons")
       (symbol "symbol")
       (number "number")
       (otherwise "unknown"))
     "unknown"))))
(define-test sacla-must-data-and-control.135 (:tag :sacla)
 (assert-true (null (typecase 'a))))
(define-test sacla-must-data-and-control.136 (:tag :sacla)
 (assert-true (typecase 'a (otherwise t))))
(define-test sacla-must-data-and-control.137 (:tag :sacla)
 (assert-true (typecase 'a (t t))))
(define-test sacla-must-data-and-control.138 (:tag :sacla)
 (assert-true
  (let ((x (make-array '(3 3))))
    (equal (typecase x (cons "cons") (symbol "symbol") (number "number"))
           nil))))
(define-test sacla-must-data-and-control.139 (:tag :sacla)
 (assert-true
  (let ((x ""))
    (equal (typecase x (t "anything") (otherwise nil)) "anything"))))
(define-test sacla-must-data-and-control.140 (:tag :sacla)
 (assert-true
  (let ((x ""))
    (and
     (eql
      (typecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2)
        (t (setq x 't) 9))
      0)
     (eq x 'string)))))
(define-test sacla-must-data-and-control.141 (:tag :sacla)
 (assert-true
  (let ((x (list nil)))
    (and
     (eql
      (typecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2)
        (t (setq x 't) 9))
      1)
     (eq x 'cons)))))
(define-test sacla-must-data-and-control.142 (:tag :sacla)
 (assert-true
  (let ((x #*01))
    (and
     (eql
      (typecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2)
        (t (setq x 't) 9))
      2)
     (eq x 'array)))))
(define-test sacla-must-data-and-control.143 (:tag :sacla)
 (assert-true
  (let ((x #\a))
    (and
     (eql
      (typecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2)
        (t (setq x 't) 9))
      9)
     (eq x 't)))))
(define-test sacla-must-data-and-control.144 (:tag :sacla)
 (assert-true
  (let ((x #*01))
    (and
     (equal
      (multiple-value-list
       (typecase x
         (string (setq x 'string) (values 'string 0))
         (cons (setq x 'cons) (values 'cons 1))
         (array (setq x 'array) (values 'array 2))
         (t (setq x 't) (values 't 9))))
      '(array 2))
     (eq x 'array)))))
(define-test sacla-must-data-and-control.145 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (equal (ctypecase x (cons "cons") (symbol "symbol") (number "number"))
           "symbol"))))
(define-test sacla-must-data-and-control.146 (:tag :sacla)
 (assert-true
  (let ((x (list 'a)))
    (equal (ctypecase x (cons "cons") (symbol "symbol") (number "number"))
           "cons"))))
(define-test sacla-must-data-and-control.147 (:tag :sacla)
 (assert-true
  (let ((x 0))
    (equal (ctypecase x (cons "cons") (symbol "symbol") (number "number"))
           "number"))))
(define-test sacla-must-data-and-control.148 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((x (make-array '(3 3))))
        (ctypecase x (cons "cons") (symbol "symbol") (number "number")))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.149 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((a nil))
        (ctypecase a))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.150 (:tag :sacla)
 (assert-true
  (let ((x ""))
    (and
     (eql
      (ctypecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2))
      0)
     (eq x 'string)))))
(define-test sacla-must-data-and-control.151 (:tag :sacla)
 (assert-true
  (let ((x (list nil)))
    (and
     (eql
      (ctypecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2))
      1)
     (eq x 'cons)))))
(define-test sacla-must-data-and-control.152 (:tag :sacla)
 (assert-true
  (let ((x #*01))
    (and
     (eql
      (ctypecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2))
      2)
     (eq x 'array)))))
(define-test sacla-must-data-and-control.153 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((x #\a))
        (ctypecase x
          (string (setq x 'string) 0)
          (cons (setq x 'cons) 1)
          (array (setq x 'array) 2)))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.154 (:tag :sacla)
 (assert-true
  (let ((x #*01))
    (and
     (equal
      (multiple-value-list
       (ctypecase x
         (string (setq x 'string) (values 'string 0))
         (cons (setq x 'cons) (values 'cons 1))
         (array (setq x 'array) (values 'array 2))))
      '(array 2))
     (eq x 'array)))))
(define-test sacla-must-data-and-control.155 (:tag :sacla)
 (assert-true
  (let ((x 'a))
    (equal (etypecase x (cons "cons") (symbol "symbol") (number "number"))
           "symbol"))))
(define-test sacla-must-data-and-control.156 (:tag :sacla)
 (assert-true
  (let ((x (list 'a)))
    (equal (etypecase x (cons "cons") (symbol "symbol") (number "number"))
           "cons"))))
(define-test sacla-must-data-and-control.157 (:tag :sacla)
 (assert-true
  (let ((x 0))
    (equal (etypecase x (cons "cons") (symbol "symbol") (number "number"))
           "number"))))
(define-test sacla-must-data-and-control.158 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((x (make-array '(3 3))))
          (etypecase x (cons "cons") (symbol "symbol") (number "number"))))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.159 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((a nil))
          (etypecase a)))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.160 (:tag :sacla)
 (assert-true
  (let ((x ""))
    (and
     (eql
      (etypecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2))
      0)
     (eq x 'string)))))
(define-test sacla-must-data-and-control.161 (:tag :sacla)
 (assert-true
  (let ((x (list nil)))
    (and
     (eql
      (etypecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2))
      1)
     (eq x 'cons)))))
(define-test sacla-must-data-and-control.162 (:tag :sacla)
 (assert-true
  (let ((x #*01))
    (and
     (eql
      (etypecase x
        (string (setq x 'string) 0)
        (cons (setq x 'cons) 1)
        (array (setq x 'array) 2))
      2)
     (eq x 'array)))))
(define-test sacla-must-data-and-control.163 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (let ((x #\a))
          (etypecase x
            (string (setq x 'string) 0)
            (cons (setq x 'cons) 1)
            (array (setq x 'array) 2))))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-data-and-control.164 (:tag :sacla)
 (assert-true
  (let ((x #*01))
    (and
     (equal
      (multiple-value-list
       (etypecase x
         (string (setq x 'string) (values 'string 0))
         (cons (setq x 'cons) (values 'cons 1))
         (array (setq x 'array) (values 'array 2))))
      '(array 2))
     (eq x 'array)))))
(define-test sacla-must-data-and-control.165 (:tag :sacla)
 (assert-true (macro-function 'multiple-value-bind)))
(define-test sacla-must-data-and-control.166 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-bind (f r)
       (floor 130 11)
     (list f r))
   '(11 9))))
(define-test sacla-must-data-and-control.167 (:tag :sacla)
 (assert-true
  (multiple-value-bind (a b c d)
      (values 0 1 2 3 4 5)
    (and (eql a 0) (eql b 1) (eql c 2) (eql d 3)))))
(define-test sacla-must-data-and-control.168 (:tag :sacla)
 (assert-true
  (multiple-value-bind (a b c d)
      (values 0 1)
    (and (eql a 0) (eql b 1) (eql c nil) (eql d nil)))))
(define-test sacla-must-data-and-control.169 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (multiple-value-bind (a b)
        (values 0 1)
      (values a b 2 3)))
   '(0 1 2 3))))
(define-test sacla-must-data-and-control.170 (:tag :sacla)
 (assert-true
  (multiple-value-bind nil
      (values 0 1 2)
    t)))
(define-test sacla-must-data-and-control.171 (:tag :sacla)
 (assert-true
  (null
   (multiple-value-bind nil
       nil))))
(define-test sacla-must-data-and-control.172 (:tag :sacla)
 (assert-true
  (eql
   (multiple-value-bind (a)
       (floor 130 11)
     (+ a 10))
   21)))
(define-test sacla-must-data-and-control.173 (:tag :sacla)
 (assert-true
  (eql
   (multiple-value-bind (a)
       (floor 130 11)
     (+ a 10)
     (incf a 100)
     (+ a 10))
   121)))
(define-test sacla-must-data-and-control.174 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
   '(1 / 2 3 / / 2 0.5))))
(define-test sacla-must-data-and-control.175 (:tag :sacla)
 (assert-true (eql (+ (floor 5 3) (floor 19 4)) (+ 1 4))))
(define-test sacla-must-data-and-control.176 (:tag :sacla)
 (assert-true
  (eql (multiple-value-call #'+ (floor 5 3) (floor 19 4)) (+ 1 2 4 3))))
(define-test sacla-must-data-and-control.177 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (multiple-value-call
          (progn
            (push 'function list)
            #'+)
        (progn
          (push 0 list)
          0)
        (progn
          (push 1 list)
          (values 1 2))
        (progn
          (push 2 list)
          (values 3 4 5))
        (progn
          (push 3 list)
          (values 6 7 8 9)))
      45)
     (equal (reverse list) '(function 0 1 2 3))))))
(define-test sacla-must-data-and-control.178 (:tag :sacla)
 (assert-true (eql (multiple-value-call #'+ 0 1 2 3 4) 10)))
(define-test sacla-must-data-and-control.179 (:tag :sacla)
 (assert-true (eql (multiple-value-call #'+) 0)))
(define-test sacla-must-data-and-control.180 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (multiple-value-call #'values 0 1 (values 2) (values 3 4) (values 5 6 7)))
   '(0 1 2 3 4 5 6 7))))
(define-test sacla-must-data-and-control.181 (:tag :sacla)
 (assert-true (special-operator-p 'multiple-value-call)))
(define-test sacla-must-data-and-control.182 (:tag :sacla)
 (assert-true (macro-function 'multiple-value-list)))
(define-test sacla-must-data-and-control.183 (:tag :sacla)
 (assert-true (equal (multiple-value-list (floor -3 4)) '(-1 1))))
(define-test sacla-must-data-and-control.184 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (progn
      (values 'a 'b)
      0))
   '(0))))
(define-test sacla-must-data-and-control.185 (:tag :sacla)
 (assert-true (equal (multiple-value-list (prog1 (values 'a 'b) 0)) '(a))))
(define-test sacla-must-data-and-control.186 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (multiple-value-prog1 (values 'a 'b) 0)) '(a b))))
(define-test sacla-must-data-and-control.187 (:tag :sacla)
 (assert-true (special-operator-p 'multiple-value-prog1)))
(define-test sacla-must-data-and-control.188 (:tag :sacla)
 (assert-true (eql (multiple-value-prog1 1 2 3) 1)))
(define-test sacla-must-data-and-control.189 (:tag :sacla)
 (assert-true (eql (multiple-value-prog1 1 2 3) 1)))
(define-test sacla-must-data-and-control.190 (:tag :sacla)
 (assert-true
  (let ((temp '(1 2 3)))
    (multiple-value-bind (a b c)
        (multiple-value-prog1 (values-list temp)
          (setq temp nil)
          (values-list temp))
      (and (eql a 1) (eql b 2) (eql c 3))))))
(define-test sacla-must-data-and-control.191 (:tag :sacla)
 (assert-true (zerop (multiple-value-prog1 0 (values 0 1) (values 0 1 2)))))
(define-test sacla-must-data-and-control.192 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (multiple-value-prog1
        (progn
          0
          (values 0 1)
          (values 0 1 2))))
   '(0 1 2))))
(define-test sacla-must-data-and-control.193 (:tag :sacla)
 (assert-true
  (let (quotient remainder)
    (and (eql (multiple-value-setq (quotient remainder) (truncate 3.2 2)) 1)
         (eql quotient 1)
         (eql remainder 1.2)))))
(define-test sacla-must-data-and-control.194 (:tag :sacla)
 (assert-true
  (let ((a 7) (b 8) (c 9))
    (and (eql (multiple-value-setq (a b c) (values 1 2)) 1)
         (eql a 1)
         (eql b 2)
         (eql c nil)))))
(define-test sacla-must-data-and-control.195 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and (eql (multiple-value-setq (a b) (values 4 5 6)) 4)
         (eql a 4)
         (eql b 5)))))
(define-test sacla-must-data-and-control.196 (:tag :sacla)
 (assert-true (null (multiple-value-list (values-list nil)))))
(define-test sacla-must-data-and-control.197 (:tag :sacla)
 (assert-true (equal (multiple-value-list (values-list '(1))) '(1))))
(define-test sacla-must-data-and-control.198 (:tag :sacla)
 (assert-true (equal (multiple-value-list (values-list '(1 2))) '(1 2))))
(define-test sacla-must-data-and-control.199 (:tag :sacla)
 (assert-true (equal (multiple-value-list (values-list '(1 2 3))) '(1 2 3))))
(define-test sacla-must-data-and-control.200 (:tag :sacla)
 (assert-true
  (every
   #'(lambda (list) (equal (multiple-value-list (values-list list)) list))
   'nil
   '(a)
   '(a b)
   '(a b c)
   '(a b c d)
   '(a b c d e)
   '(a b c d e f)
   '(a b c d e f g)
   '(a b c d e f g h))))
(define-test sacla-must-data-and-control.201 (:tag :sacla)
 (assert-true (macro-function 'nth-value)))
(define-test sacla-must-data-and-control.202 (:tag :sacla)
 (assert-true (eql (nth-value 0 (values 'a 'b)) 'a)))
(define-test sacla-must-data-and-control.203 (:tag :sacla)
 (assert-true (eql (nth-value 1 (values 'a 'b)) 'b)))
(define-test sacla-must-data-and-control.204 (:tag :sacla)
 (assert-true (null (nth-value 2 (values 'a 'b)))))
(define-test sacla-must-data-and-control.205 (:tag :sacla)
 (assert-true
  (multiple-value-bind (a b eq?)
      (let* ((x 83927472397238947423879243432432432)
             (y 32423489732)
             (a (nth-value 1 (floor x y)))
             (b (mod x y)))
        (values a b (= a b)))
    (and (eql a 3332987528) (eql b 3332987528) eq?))))
(define-test sacla-must-data-and-control.206 (:tag :sacla)
 (assert-true (null (nth-value 0 (values)))))
(define-test sacla-must-data-and-control.207 (:tag :sacla)
 (assert-true (eql (nth-value 0 1) 1)))
(define-test sacla-must-data-and-control.208 (:tag :sacla)
 (assert-true (null (nth-value 1 1))))
(define-test sacla-must-data-and-control.209 (:tag :sacla)
 (assert-true (eql (nth-value 0 (values 0 1 2)) 0)))
(define-test sacla-must-data-and-control.210 (:tag :sacla)
 (assert-true (eql (nth-value 1 (values 0 1 2)) 1)))
(define-test sacla-must-data-and-control.211 (:tag :sacla)
 (assert-true (eql (nth-value 2 (values 0 1 2)) 2)))
(define-test sacla-must-data-and-control.212 (:tag :sacla)
 (assert-true (eql (nth-value 3 (values 0 1 2)) nil)))
(define-test sacla-must-data-and-control.213 (:tag :sacla)
 (assert-true (eql (nth-value 4 (values 0 1 2)) nil)))
(define-test sacla-must-data-and-control.214 (:tag :sacla)
 (assert-true (eql (nth-value 5 (values 0 1 2)) nil)))
(define-test sacla-must-data-and-control.215 (:tag :sacla)
 (assert-true
  (let ((z (list 0 1 2 3)))
    (eql (prog* ((y z) (x (car y))) (return x)) (car z)))))
(define-test sacla-must-data-and-control.216 (:tag :sacla)
 (assert-true (macro-function 'prog)))
(define-test sacla-must-data-and-control.217 (:tag :sacla)
 (assert-true (macro-function 'prog*)))
(define-test sacla-must-data-and-control.218 (:tag :sacla)
 (assert-true
  (let ((a 1))
    (eq (prog ((a 2) (b a)) (return (if (= a b) '= '/=))) '/=))))
(define-test sacla-must-data-and-control.219 (:tag :sacla)
 (assert-true (eq (prog* ((a 2) (b a)) (return (if (= a b) '= '/=))) '=)))
(define-test sacla-must-data-and-control.220 (:tag :sacla)
 (assert-true (null (prog () 'no-return-value))))
(define-test sacla-must-data-and-control.221 (:tag :sacla)
 (assert-true
  (flet ((king-of-confusion (w)
           "Take a cons of two lists and make a list of conses.
Think of this function as being like a zipper."
           (prog (x y z)
             (setq y (car w) z (cdr w))
            loop
             (cond
               ((null y)
                (return x))
               ((null z)
                (go err)))
            rejoin
             (setq x (cons (cons (car y) (car z)) x))
             (setq y (cdr y) z (cdr z))
             (go loop)
            err
             (cerror "Will self-pair extraneous items"
                     "Mismatch - gleep!  ~S"
                     y)
             (setq z y)
             (go rejoin))))
    (and
     (equal (king-of-confusion '((0 1 2) a b c)) '((2 . c) (1 . b) (0 . a)))
     (equal (king-of-confusion '((0 1 2 3 4 5) a b c d e f))
            '((5 . f) (4 . e) (3 . d) (2 . c) (1 . b) (0 . a)))))))
(define-test sacla-must-data-and-control.222 (:tag :sacla)
 (assert-true (null (prog () t))))
(define-test sacla-must-data-and-control.223 (:tag :sacla)
 (assert-true (null (prog ()))))
(define-test sacla-must-data-and-control.224 (:tag :sacla)
 (assert-true
  (eql
   (let ((a 0) (b 0))
     (prog ((a 10) (b 100)) (return (+ a b))))
   110)))
(define-test sacla-must-data-and-control.225 (:tag :sacla)
 (assert-true
  (prog (a (b 1) (c 2)) (return (and (null a) (eql b 1) (eql c 2))))))
(define-test sacla-must-data-and-control.226 (:tag :sacla)
 (assert-true
  (prog ((a 0) b (c 2)) (return (and (eql a 0) (null b) (eql c 2))))))
(define-test sacla-must-data-and-control.227 (:tag :sacla)
 (assert-true
  (prog ((a 0) (b 1) c) (return (and (eql a 0) (eql b 1) (null c))))))
(define-test sacla-must-data-and-control.228 (:tag :sacla)
 (assert-true (prog (a b c) (return (every #'null (list a b c))))))
(define-test sacla-must-data-and-control.229 (:tag :sacla)
 (assert-true
  (eql
   (let ((a 0))
     (declare (special a))
     (flet ((ref-a ()
              a))
       (prog ((a 10)) (declare (special a)) (return (ref-a)))))
   10)))
(define-test sacla-must-data-and-control.230 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (declare (special a))
    (and
     (eql
      (flet ((ref-a ()
               a))
        (prog ((a 10) b (c 100))
          (declare (special a))
          (setq b 1)
          (return (+ (ref-a) b c))))
      111)
     (eql a 0)))))
(define-test sacla-must-data-and-control.231 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (declare (special a))
    (and
     (equal
      (multiple-value-list
       (flet ((ref-a ()
                a))
         (prog ((a 10) b (c 100))
           (declare (special a))
           (setq b 1)
           (return (values (ref-a) b c)))))
      '(10 1 100))
     (eql a 0)))))
(define-test sacla-must-data-and-control.232 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and (eql (prog () (return a)) 0) (eql a 0)))))
(define-test sacla-must-data-and-control.233 (:tag :sacla)
 (assert-true
  (flet ((rev (list)
           (prog ((x list) (result nil))
            top
             (when (null x)
               (return result))
             (psetq x (cdr x) result (cons (car x) result))
             (go top))))
    (and (equal (rev '(0 1 2 3)) '(3 2 1 0))
         (equal (rev nil) nil)
         (equal (rev '(0)) '(0))))))
(define-test sacla-must-data-and-control.234 (:tag :sacla)
 (assert-true
  (eql
   (prog (val)
     (setq val 1)
     (go point-a)
     (incf val 16)
    point-c
     (incf val 4)
     (go point-b)
     (incf val 32)
    point-a
     (incf val 2)
     (go point-c)
     (incf val 64)
    point-b
     (incf val 8)
     (return val))
   15)))
(define-test sacla-must-data-and-control.235 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and
     (equal
      (multiple-value-list
       (prog ((a 100) (b a) (c 1)) (return (values a b c))))
      '(100 0 1))
     (eql a 0)))))
(define-test sacla-must-data-and-control.236 (:tag :sacla)
 (assert-true (null (prog* () 'no-return-value))))
(define-test sacla-must-data-and-control.237 (:tag :sacla)
 (assert-true
  (flet ((king-of-confusion (w)
           "Take a cons of two lists and make a list of conses.
Think of this function as being like a zipper."
           (prog* (x y z)
             (setq y (car w) z (cdr w))
            loop
             (cond
               ((null y)
                (return x))
               ((null z)
                (go err)))
            rejoin
             (setq x (cons (cons (car y) (car z)) x))
             (setq y (cdr y) z (cdr z))
             (go loop)
            err
             (cerror "Will self-pair extraneous items"
                     "Mismatch - gleep!  ~S"
                     y)
             (setq z y)
             (go rejoin))))
    (and
     (equal (king-of-confusion '((0 1 2) a b c)) '((2 . c) (1 . b) (0 . a)))
     (equal (king-of-confusion '((0 1 2 3 4 5) a b c d e f))
            '((5 . f) (4 . e) (3 . d) (2 . c) (1 . b) (0 . a)))))))
(define-test sacla-must-data-and-control.238 (:tag :sacla)
 (assert-true (null (prog* () t))))
(define-test sacla-must-data-and-control.239 (:tag :sacla)
 (assert-true (null (prog* ()))))
(define-test sacla-must-data-and-control.240 (:tag :sacla)
 (assert-true
  (eql
   (let ((a 0) (b 0))
     (prog* ((a 10) (b 100)) (return (+ a b))))
   110)))
(define-test sacla-must-data-and-control.241 (:tag :sacla)
 (assert-true
  (prog* (a (b 1) (c 2)) (return (and (null a) (eql b 1) (eql c 2))))))
(define-test sacla-must-data-and-control.242 (:tag :sacla)
 (assert-true
  (prog* ((a 0) b (c 2)) (return (and (eql a 0) (null b) (eql c 2))))))
(define-test sacla-must-data-and-control.243 (:tag :sacla)
 (assert-true
  (prog* ((a 0) (b 1) c) (return (and (eql a 0) (eql b 1) (null c))))))
(define-test sacla-must-data-and-control.244 (:tag :sacla)
 (assert-true (prog* (a b c) (return (every #'null (list a b c))))))
(define-test sacla-must-data-and-control.245 (:tag :sacla)
 (assert-true
  (eql
   (let ((a 0))
     (declare (special a))
     (flet ((ref-a ()
              a))
       (prog* ((a 10)) (declare (special a)) (return (ref-a)))))
   10)))
(define-test sacla-must-data-and-control.246 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (declare (special a))
    (and
     (eql
      (flet ((ref-a ()
               a))
        (prog* ((a 10) b (c 100))
          (declare (special a))
          (setq b 1)
          (return (+ (ref-a) b c))))
      111)
     (eql a 0)))))
(define-test sacla-must-data-and-control.247 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (declare (special a))
    (and
     (equal
      (multiple-value-list
       (flet ((ref-a ()
                a))
         (prog* ((a 10) b (c 100))
           (declare (special a))
           (setq b 1)
           (return (values (ref-a) b c)))))
      '(10 1 100))
     (eql a 0)))))
(define-test sacla-must-data-and-control.248 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and (eql (prog* () (return a)) 0) (eql a 0)))))
(define-test sacla-must-data-and-control.249 (:tag :sacla)
 (assert-true
  (flet ((rev (list)
           (prog* ((x list) (result nil))
            top
             (when (null x)
               (return result))
             (psetq x (cdr x) result (cons (car x) result))
             (go top))))
    (and (equal (rev '(0 1 2 3)) '(3 2 1 0))
         (equal (rev nil) nil)
         (equal (rev '(0)) '(0))))))
(define-test sacla-must-data-and-control.250 (:tag :sacla)
 (assert-true
  (eql
   (prog* (val)
     (setq val 1)
     (go point-a)
     (incf val 16)
    point-c
     (incf val 4)
     (go point-b)
     (incf val 32)
    point-a
     (incf val 2)
     (go point-c)
     (incf val 64)
    point-b
     (incf val 8)
     (return val))
   15)))
(define-test sacla-must-data-and-control.251 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and
     (equal
      (multiple-value-list
       (prog* ((a 100) (b a) (c 1)) (return (values a b c))))
      '(100 100 1))
     (eql a 0)))))
(define-test sacla-must-data-and-control.252 (:tag :sacla)
 (assert-true (macro-function 'prog1)))
(define-test sacla-must-data-and-control.253 (:tag :sacla)
 (assert-true (macro-function 'prog2)))
(define-test sacla-must-data-and-control.254 (:tag :sacla)
 (assert-true
  (eql
   (let ((temp 1))
     (prog1 temp (incf temp) temp))
   1)))
(define-test sacla-must-data-and-control.255 (:tag :sacla)
 (assert-true
  (let ((temp t))
    (and (eq (prog1 temp (setq temp nil)) 't) (null temp)))))
(define-test sacla-must-data-and-control.256 (:tag :sacla)
 (assert-true (equal (multiple-value-list (prog1 (values 1 2 3) 4)) '(1))))
(define-test sacla-must-data-and-control.257 (:tag :sacla)
 (assert-true
  (let ((temp (list 'a 'b 'c)))
    (and (eq (prog1 (car temp) (setf (car temp) 'alpha)) 'a)
         (equal temp '(alpha b c))))))
(define-test sacla-must-data-and-control.258 (:tag :sacla)
 (assert-true
  (equal
   (flet ((swap-symbol-values (x y)
            (setf (symbol-value x)
                    (prog1 (symbol-value y)
                      (setf (symbol-value y) (symbol-value x))))))
     (let ((*foo* 1) (*bar* 2))
       (declare (special *foo* *bar*))
       (swap-symbol-values '*foo* '*bar*)
       (list *foo* *bar*)))
   '(2 1))))
(define-test sacla-must-data-and-control.259 (:tag :sacla)
 (assert-true
  (let ((temp 1))
    (and (eql (prog2 (incf temp) (incf temp) (incf temp)) 3) (eql temp 4)))))
(define-test sacla-must-data-and-control.260 (:tag :sacla)
 (assert-true (equal (multiple-value-list (prog2 1 (values 2 3 4) 5)) '(2))))
(define-test sacla-must-data-and-control.261 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (prog2 1 (values 2 3 4) 5 (values 6 7))) '(2))))
(define-test sacla-must-data-and-control.262 (:tag :sacla)
 (assert-true (eql (prog1 1) 1)))
(define-test sacla-must-data-and-control.263 (:tag :sacla)
 (assert-true (eql (prog1 1 2) 1)))
(define-test sacla-must-data-and-control.264 (:tag :sacla)
 (assert-true (eql (prog1 1 2 3) 1)))
(define-test sacla-must-data-and-control.265 (:tag :sacla)
 (assert-true (equal (multiple-value-list (prog1 (values 1 2 3))) '(1))))
(define-test sacla-must-data-and-control.266 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list (prog1 (values 1 2 3) (values 4 5 6) (values 7 8 9)))
   '(1))))
(define-test sacla-must-data-and-control.267 (:tag :sacla)
 (assert-true (eql (prog2 1 2) 2)))
(define-test sacla-must-data-and-control.268 (:tag :sacla)
 (assert-true (eql (prog2 1 2 3) 2)))
(define-test sacla-must-data-and-control.269 (:tag :sacla)
 (assert-true (eql (prog2 1 2 3 4) 2)))
(define-test sacla-must-data-and-control.270 (:tag :sacla)
 (assert-true
  (let ((x 0))
    (and (eql (prog2 (incf x) (incf x) (incf x) (incf x)) 2) (eql x 4)))))
(define-test sacla-must-data-and-control.271 (:tag :sacla)
 (assert-true
  (let ((x (cons 'a 'b)) (y (list 1 2 3)))
    (and (equal (setf (car x) 'x (cadr y) (car x) (cdr x) y) '(1 x 3))
         (equal x '(x 1 x 3))
         (equal y '(1 x 3))))))
(define-test sacla-must-data-and-control.272 (:tag :sacla)
 (assert-true
  (let ((x (cons 'a 'b)) (y (list 1 2 3)))
    (and (null (psetf (car x) 'x (cadr y) (car x) (cdr x) y))
         (equal x '(x 1 a 3))
         (equal y '(1 a 3))))))
(define-test sacla-must-data-and-control.273 (:tag :sacla)
 (assert-true (null (setf))))
(define-test sacla-must-data-and-control.274 (:tag :sacla)
 (assert-true (null (psetf))))
(define-test sacla-must-data-and-control.275 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and (eql (setf a 10) 10) (eql a 10)))))
(define-test sacla-must-data-and-control.276 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and (eql (setf a 10 b 20) 20) (eql a 10) (eql b 20)))))
(define-test sacla-must-data-and-control.277 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1) (c 2))
    (and (eql (setf a 10 b (+ a 10) c (+ b 10)) 30)
         (eql a 10)
         (eql b 20)
         (eql c 30)))))
(define-test sacla-must-data-and-control.278 (:tag :sacla)
 (assert-true
  (let ((x (list 0 1 2)))
    (and (eq (setf (car x) 'a) 'a)
         (eq (setf (cadr x) 'b) 'b)
         (eq (setf (caddr x) 'c) 'c)
         (equal x '(a b c))))))
(define-test sacla-must-data-and-control.279 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and (null (psetf a 10)) (eql a 10)))))
(define-test sacla-must-data-and-control.280 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and (null (psetf a 10 b 20)) (eql a 10) (eql b 20)))))
(define-test sacla-must-data-and-control.281 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1) (c 2))
    (and (null (psetf a 10 b (+ a 10) c (+ b 10)))
         (eql a 10)
         (eql b 10)
         (eql c 11)))))
(define-test sacla-must-data-and-control.282 (:tag :sacla)
 (assert-true
  (let ((x (list 0 1 2)))
    (and (null (psetf (car x) 'a))
         (null (psetf (cadr x) 'b))
         (null (psetf (caddr x) 'c))
         (equal x '(a b c))))))
(define-test sacla-must-data-and-control.283 (:tag :sacla)
 (assert-true
  (let ((x (make-array '(2 3) :initial-contents '((a b c) (x y z)))))
    (and (eql (setf (aref x 0 0) 0.0) 0.0)
         (eql (setf (aref x 0 1) 0.1) 0.1)
         (eql (setf (aref x 0 2) 0.2) 0.2)
         (eql (setf (aref x 1 0) 1.0) 1.0)
         (eql (setf (aref x 1 1) 1.1) 1.1)
         (eql (setf (aref x 1 2) 1.2) 1.2)
         (equalp x #2A((0.0 0.1 0.2) (1.0 1.1 1.2)))))))
(define-test sacla-must-data-and-control.284 (:tag :sacla)
 (assert-true
  (let ((x (make-array 4 :element-type 'bit :initial-element 0)))
    (and (equalp x #*0000)
         (eql (setf (bit x 0) 1) 1)
         (eql (setf (bit x 2) 1) 1)
         (equal x #*1010)))))
(define-test sacla-must-data-and-control.285 (:tag :sacla)
 (assert-true
  (let ((x (copy-seq "dog")))
    (and (eql (setf (char x 0) #\c) #\c)
         (eql (setf (char x 1) #\a) #\a)
         (eql (setf (char x 2) #\t) #\t)
         (equal x "cat")))))
(define-test sacla-must-data-and-control.286 (:tag :sacla)
 (assert-true
  (let ((x (copy-seq "dog")))
    (and (eql (setf (schar x 0) #\c) #\c)
         (eql (setf (schar x 1) #\a) #\a)
         (eql (setf (schar x 2) #\t) #\t)
         (equal x "cat")))))
(define-test sacla-must-data-and-control.287 (:tag :sacla)
 (assert-true
  (let ((x (copy-seq "dog")))
    (and (eql (setf (elt x 0) #\c) #\c)
         (eql (setf (elt x 1) #\a) #\a)
         (eql (setf (elt x 2) #\t) #\t)
         (equal x "cat")))))
(define-test sacla-must-data-and-control.288 (:tag :sacla)
 (assert-true
  (let ((x (list 0 1 2)))
    (and (eql (setf (elt x 0) #\c) #\c)
         (eql (setf (elt x 1) #\a) #\a)
         (eql (setf (elt x 2) #\t) #\t)
         (equal x '(#\c #\a #\t))))))
(define-test sacla-must-data-and-control.289 (:tag :sacla)
 (assert-true
  (let ((x #'(lambda (a) (+ a 10)))
        (saved
         (when (fboundp 'test-fn)
           (fdefinition 'test-fn))))
    (unwind-protect
        (and (eq (setf (fdefinition 'test-fn) x) x) (eql (test-fn 10) 20))
      (when saved
        (setf (fdefinition 'test-fn) saved))))))
(define-test sacla-must-data-and-control.290 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)))
    (and (equal (multiple-value-list (gethash 1 table)) '(nil nil))
         (equal (multiple-value-list (gethash 1 table 2)) '(2 nil))
         (equal (setf (gethash 1 table) "one") "one")
         (equal (setf (gethash 2 table "two") "two") "two")
         (multiple-value-bind (value present-p)
             (gethash 1 table)
           (and (equal value "one") present-p))
         (multiple-value-bind (value present-p)
             (gethash 2 table)
           (and (equal value "two") present-p))))))
(define-test sacla-must-data-and-control.291 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)))
    (and (equal (multiple-value-list (gethash nil table)) '(nil nil))
         (null (setf (gethash nil table) nil))
         (multiple-value-bind (value present-p)
             (gethash nil table)
           (and (equal value nil) present-p))))))
(define-test sacla-must-data-and-control.292 (:tag :sacla)
 (assert-true
  (let ((x (copy-seq #*0101)))
    (and (eql (setf (sbit x 0) 1) 1)
         (eql (setf (sbit x 2) 1) 1)
         (equal x #*1111)))))
(define-test sacla-must-data-and-control.293 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1))
    (and
     (equal (multiple-value-list (setf (values a b) (values 'x 'y 'z))) '(x y))
     (eq a 'x)
     (eq b 'y)))))
(define-test sacla-must-data-and-control.294 (:tag :sacla)
 (assert-true
  (let ((x (list 0 1 2)) (order nil))
    (and
     (equal
      (multiple-value-list
       (setf (values (car (prog1 x (push 0 order)))
                     (cadr (prog1 x (push 1 order)))
                     (caddr (prog1 x (push 2 order))))
               (values 'a 'b)))
      '(a b nil))
     (equal x '(a b nil))
     (equal order '(2 1 0))))))
(define-test sacla-must-data-and-control.295 (:tag :sacla)
 (assert-true
  (let ((a 'a) (b 'b) (c 'c))
    (and
     (equal
      (multiple-value-list
       (setf (values (values a) (values b c)) (values 0 1 2 3 4)))
      '(0 1))
     (eql a 0)
     (eql b 1)
     (null c)))))
(define-test sacla-must-data-and-control.296 (:tag :sacla)
 (assert-true
  (let ((a 'a) (b 'b) (c 'c) (d 'd))
    (and
     (equal
      (multiple-value-list
       (setf (values (values a b) (values c d)) (values 0 1 2 3 4)))
      '(0 1))
     (eql a 0)
     (null b)
     (eql c 1)
     (null d)))))
(define-test sacla-must-data-and-control.297 (:tag :sacla)
 (assert-true
  (let ((a 'a) (b 'b) (c 'c) (d 'd))
    (and
     (equal
      (multiple-value-list
       (setf (values (values a b) (values c d)) (values 0)))
      '(0 nil))
     (eql a 0)
     (null b)
     (null c)
     (null d)))))
(define-test sacla-must-data-and-control.298 (:tag :sacla)
 (assert-true
  (let ((a 'a) (b 'b) (c 'c))
    (and (equal (multiple-value-list (setf (values a) (values 0 1 2))) '(0))
         (eql a 0)
         (eq b 'b)
         (eq c 'c)))))
(define-test sacla-must-data-and-control.299 (:tag :sacla)
 (assert-true
  (let ((x (list 1 2 3)) (y 'trash))
    (and (eq (shiftf y x (cdr x) '(hi there)) 'trash)
         (equal x '(2 3))
         (equal y '(1 hi there))))))
(define-test sacla-must-data-and-control.300 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eq (shiftf (cadr x) 'z) 'b)
         (equal x '(a z c))
         (eq (shiftf (cadr x) (cddr x) 'q) 'z)
         (equal x '(a (c) . q))))))
(define-test sacla-must-data-and-control.301 (:tag :sacla)
 (assert-true
  (let ((n 0) (x (list 'a 'b 'c 'd)))
    (and (eq (shiftf (nth (setq n (+ n 1)) x) 'z) 'b) (equal x '(a z c d))))))
(define-test sacla-must-data-and-control.302 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1) (c 2) (d 3))
    (and
     (equal
      (multiple-value-list (shiftf (values a b) (values c d) (values 4 5)))
      '(0 1))
     (eql a 2)
     (eql b 3)
     (eql c 4)
     (eql d 5)))))
(define-test sacla-must-data-and-control.303 (:tag :sacla)
 (assert-true
  (let ((n 0) (x (list 'a 'b 'c 'd 'e 'f 'g)))
    (and (null (rotatef (nth (incf n) x) (nth (incf n) x) (nth (incf n) x)))
         (equal x '(a c d b e f g))))))
(define-test sacla-must-data-and-control.304 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (null (rotatef (first x) (second x) (third x))) (equal x '(b c a))))))
(define-test sacla-must-data-and-control.305 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e 'f)))
    (and (null (rotatef (second x) (third x) (fourth x) (fifth x)))
         (equal x '(a c d e b f))))))
(define-test sacla-must-data-and-control.306 (:tag :sacla)
 (assert-true (null (rotatef))))
(define-test sacla-must-data-and-control.307 (:tag :sacla)
 (assert-true
  (let ((a 0))
    (and (null (rotatef a)) (zerop a)))))
(define-test sacla-must-data-and-control.308 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)) (order nil))
    (and
     (null
      (rotatef
       (first
        (progn
          (push 1 order)
          x))
       (second
        (progn
          (push 2 order)
          x))
       (third
        (progn
          (push 3 order)
          x))))
     (equal x '(b c a))
     (equal order '(3 2 1))))))
(define-test sacla-must-data-and-control.309 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)) (order nil))
    (and
     (null
      (psetf (first
              (progn
                (push 1 order)
                x))
               (second
                (progn
                  (push 2 order)
                  x))
             (second
              (progn
                (push 2 order)
                x))
               (third
                (progn
                  (push 3 order)
                  x))
             (third
              (progn
                (push 3 order)
                x))
               (first
                (progn
                  (push 1 order)
                  x))))
     (equal x '(b c a))
     (equal order '(1 3 3 2 2 1))))))
(define-test sacla-must-data-and-control.310 (:tag :sacla)
 (assert-true
  (let ((a 0) (b 1) (c 2) (d 3))
    (and (null (rotatef (values a b) (values c d)))
         (eql a 2)
         (eql b 3)
         (eql c 0)
         (eql d 1)))))

