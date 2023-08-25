(in-package #:sacla-lisp-unit)
(define-test sacla-must-cons.1 (:tag :sacla) (assert-true (consp (cons 'a 'b))))
(define-test sacla-must-cons.2 (:tag :sacla) (assert-true (consp '(1 . 2))))
(define-test sacla-must-cons.3 (:tag :sacla) (assert-true (consp (list nil))))
(define-test sacla-must-cons.4 (:tag :sacla) (assert-true (not (consp 'a))))
(define-test sacla-must-cons.5 (:tag :sacla) (assert-true (not (consp nil))))
(define-test sacla-must-cons.6 (:tag :sacla) (assert-true (not (consp 1))))
(define-test sacla-must-cons.7 (:tag :sacla) (assert-true (not (consp #\a))))
(define-test sacla-must-cons.8 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (and (eql (car a) 1) (eql (cdr a) 2)))))
(define-test sacla-must-cons.9 (:tag :sacla)
 (assert-true (equal (cons 1 nil) '(1))))
(define-test sacla-must-cons.10 (:tag :sacla)
 (assert-true (equal (cons nil nil) '(nil))))
(define-test sacla-must-cons.11 (:tag :sacla)
 (assert-true (equal (cons 'a (cons 'b (cons 'c 'nil))) '(a b c))))
(define-test sacla-must-cons.12 (:tag :sacla) (assert-true (atom 'a)))
(define-test sacla-must-cons.13 (:tag :sacla) (assert-true (atom nil)))
(define-test sacla-must-cons.14 (:tag :sacla) (assert-true (atom 1)))
(define-test sacla-must-cons.15 (:tag :sacla) (assert-true (atom #\a)))
(define-test sacla-must-cons.16 (:tag :sacla)
 (assert-true (not (atom (cons 1 2)))))
(define-test sacla-must-cons.17 (:tag :sacla)
 (assert-true (not (atom '(a . b)))))
(define-test sacla-must-cons.18 (:tag :sacla)
 (assert-true (not (atom (list nil)))))
(define-test sacla-must-cons.19 (:tag :sacla) (assert-true (listp nil)))
(define-test sacla-must-cons.20 (:tag :sacla) (assert-true (listp '(a b c))))
(define-test sacla-must-cons.21 (:tag :sacla) (assert-true (listp '(a . b))))
(define-test sacla-must-cons.22 (:tag :sacla)
 (assert-true (listp (cons 'a 'b))))
(define-test sacla-must-cons.23 (:tag :sacla)
 (assert-true (listp '#1=(1 2 . #1#))))
(define-test sacla-must-cons.24 (:tag :sacla) (assert-true (not (listp 1))))
(define-test sacla-must-cons.25 (:tag :sacla) (assert-true (not (listp 't))))
(define-test sacla-must-cons.26 (:tag :sacla) (assert-true (null 'nil)))
(define-test sacla-must-cons.27 (:tag :sacla) (assert-true (null 'nil)))
(define-test sacla-must-cons.28 (:tag :sacla) (assert-true (null nil)))
(define-test sacla-must-cons.29 (:tag :sacla) (assert-true (not (null t))))
(define-test sacla-must-cons.30 (:tag :sacla) (assert-true (null (cdr '(a)))))
(define-test sacla-must-cons.31 (:tag :sacla)
 (assert-true (not (null (cdr '(1 . 2))))))
(define-test sacla-must-cons.32 (:tag :sacla) (assert-true (not (null 'a))))
(define-test sacla-must-cons.33 (:tag :sacla) (assert-true (endp 'nil)))
(define-test sacla-must-cons.34 (:tag :sacla) (assert-true (not (endp '(1)))))
(define-test sacla-must-cons.35 (:tag :sacla) (assert-true (not (endp '(1 2)))))
(define-test sacla-must-cons.36 (:tag :sacla)
 (assert-true (not (endp '(1 2 3)))))
(define-test sacla-must-cons.37 (:tag :sacla)
 (assert-true (not (endp (cons 1 2)))))
(define-test sacla-must-cons.38 (:tag :sacla)
 (assert-true (endp (cddr '(1 2)))))
(define-test sacla-must-cons.39 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (and (eq (rplaca a 0) a) (equal a '(0 . 2))))))
(define-test sacla-must-cons.40 (:tag :sacla)
 (assert-true
  (let ((a (list 1 2 3)))
    (and (eq (rplaca a 0) a) (equal a '(0 2 3))))))
(define-test sacla-must-cons.41 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (and (eq (rplacd a 0) a) (equal a '(1 . 0))))))
(define-test sacla-must-cons.42 (:tag :sacla)
 (assert-true
  (let ((a (list 1 2 3)))
    (and (eq (rplacd a 0) a) (equal a '(1 . 0))))))
(define-test sacla-must-cons.43 (:tag :sacla)
 (assert-true (eq (car '(a . b)) 'a)))
(define-test sacla-must-cons.44 (:tag :sacla) (assert-true (null (car nil))))
(define-test sacla-must-cons.45 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (eq (car (list a)) a))))
(define-test sacla-must-cons.46 (:tag :sacla)
 (assert-true (eq (car '#1=(a . #1#)) 'a)))
(define-test sacla-must-cons.47 (:tag :sacla)
 (assert-true (eq (cdr '(a . b)) 'b)))
(define-test sacla-must-cons.48 (:tag :sacla)
 (assert-true (eq (rest '(a . b)) 'b)))
(define-test sacla-must-cons.49 (:tag :sacla) (assert-true (null (cdr nil))))
(define-test sacla-must-cons.50 (:tag :sacla) (assert-true (null (rest nil))))
(define-test sacla-must-cons.51 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (eq (cdr (cons 1 a)) a))))
(define-test sacla-must-cons.52 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (eq (rest (cons 1 a)) a))))
(define-test sacla-must-cons.53 (:tag :sacla)
 (assert-true
  (let ((x '#1=(a . #1#)))
    (eq (cdr x) x))))
(define-test sacla-must-cons.54 (:tag :sacla)
 (assert-true
  (let ((x '#1=(a . #1#)))
    (eq (rest x) x))))
(define-test sacla-must-cons.55 (:tag :sacla)
 (assert-true (eq (caar '((a) b c)) 'a)))
(define-test sacla-must-cons.56 (:tag :sacla)
 (assert-true (eq (cadr '(a b c)) 'b)))
(define-test sacla-must-cons.57 (:tag :sacla)
 (assert-true (eq (cdar '((a . aa) b c)) 'aa)))
(define-test sacla-must-cons.58 (:tag :sacla)
 (assert-true (eq (cddr '(a b . c)) 'c)))
(define-test sacla-must-cons.59 (:tag :sacla)
 (assert-true (eq (caaar '(((a)) b c)) 'a)))
(define-test sacla-must-cons.60 (:tag :sacla)
 (assert-true (eq (caadr '(a (b) c)) 'b)))
(define-test sacla-must-cons.61 (:tag :sacla)
 (assert-true (eq (cadar '((a aa) b c)) 'aa)))
(define-test sacla-must-cons.62 (:tag :sacla)
 (assert-true (eq (caddr '(a b c)) 'c)))
(define-test sacla-must-cons.63 (:tag :sacla)
 (assert-true (eq (cdaar '(((a . aa)) b c)) 'aa)))
(define-test sacla-must-cons.64 (:tag :sacla)
 (assert-true (eq (cdadr '(a (b . bb) c)) 'bb)))
(define-test sacla-must-cons.65 (:tag :sacla)
 (assert-true (eq (cddar '((a aa . aaa) b c)) 'aaa)))
(define-test sacla-must-cons.66 (:tag :sacla)
 (assert-true (eq (cdddr '(a b c . d)) 'd)))
(define-test sacla-must-cons.67 (:tag :sacla)
 (assert-true (eq (caaaar '((((a))) b c)) 'a)))
(define-test sacla-must-cons.68 (:tag :sacla)
 (assert-true (eq (caaadr '(a ((b)) c)) 'b)))
(define-test sacla-must-cons.69 (:tag :sacla)
 (assert-true (eq (caadar '((a (aa)) b c)) 'aa)))
(define-test sacla-must-cons.70 (:tag :sacla)
 (assert-true (eq (caaddr '(a b (c))) 'c)))
(define-test sacla-must-cons.71 (:tag :sacla)
 (assert-true (eq (cadaar '(((a aa)) b c)) 'aa)))
(define-test sacla-must-cons.72 (:tag :sacla)
 (assert-true (eq (cadadr '(a (b bb) c)) 'bb)))
(define-test sacla-must-cons.73 (:tag :sacla)
 (assert-true (eq (caddar '((a aa aaa) b c)) 'aaa)))
(define-test sacla-must-cons.74 (:tag :sacla)
 (assert-true (eq (cadddr '(a b c d)) 'd)))
(define-test sacla-must-cons.75 (:tag :sacla)
 (assert-true (eq (cdaaar '((((a . aa))) b c)) 'aa)))
(define-test sacla-must-cons.76 (:tag :sacla)
 (assert-true (eq (cdaadr '(a ((b . bb)) c)) 'bb)))
(define-test sacla-must-cons.77 (:tag :sacla)
 (assert-true (eq (cdadar '((a (aa . aaa)) b c)) 'aaa)))
(define-test sacla-must-cons.78 (:tag :sacla)
 (assert-true (eq (cdaddr '(a b (c . cc))) 'cc)))
(define-test sacla-must-cons.79 (:tag :sacla)
 (assert-true (eq (cddaar '(((a aa . aaa)) b c)) 'aaa)))
(define-test sacla-must-cons.80 (:tag :sacla)
 (assert-true (eq (cddadr '(a (b bb . bbb) c)) 'bbb)))
(define-test sacla-must-cons.81 (:tag :sacla)
 (assert-true (eq (cdddar '((a aa aaa . aaaa) b c)) 'aaaa)))
(define-test sacla-must-cons.82 (:tag :sacla)
 (assert-true (eq (cddddr '(a b c d . e)) 'e)))
(define-test sacla-must-cons.83 (:tag :sacla)
 (assert-true
  (let ((x (cons 1 2)))
    (and (eql (setf (car x) 0) 0) (equal x '(0 . 2))))))
(define-test sacla-must-cons.84 (:tag :sacla)
 (assert-true
  (let ((x (cons 1 2)))
    (and (eql (setf (cdr x) 0) 0) (equal x '(1 . 0))))))
(define-test sacla-must-cons.85 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a) b c))))
    (and (eql (setf (caar x) 0) 0) (equal x '((0) b c))))))
(define-test sacla-must-cons.86 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eql (setf (cadr x) 0) 0) (equal x '(a 0 c))))))
(define-test sacla-must-cons.87 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a . aa) b c))))
    (and (eql (setf (cdar x) 0) 0) (equal x '((a . 0) b c))))))
(define-test sacla-must-cons.88 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a b . c))))
    (and (eql (setf (cddr x) 0) 0) (equal x '(a b . 0))))))
(define-test sacla-must-cons.89 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(((a)) b c))))
    (and (eql (setf (caaar x) 0) 0) (equal x '(((0)) b c))))))
(define-test sacla-must-cons.90 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a (b) c))))
    (and (eql (setf (caadr x) 0) 0) (equal x '(a (0) c))))))
(define-test sacla-must-cons.91 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a aa) b c))))
    (and (eql (setf (cadar x) 0) 0) (equal x '((a 0) b c))))))
(define-test sacla-must-cons.92 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eql (setf (caddr x) 0) 0) (equal x '(a b 0))))))
(define-test sacla-must-cons.93 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(((a . aa)) b c))))
    (and (eql (setf (cdaar x) 0) 0) (equal x '(((a . 0)) b c))))))
(define-test sacla-must-cons.94 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a (b . bb) c))))
    (and (eql (setf (cdadr x) 0) 0) (equal x '(a (b . 0) c))))))
(define-test sacla-must-cons.95 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a aa . aaa) b c))))
    (and (eql (setf (cddar x) 0) 0) (equal x '((a aa . 0) b c))))))
(define-test sacla-must-cons.96 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a b c . d))))
    (and (eql (setf (cdddr x) 0) 0) (equal x '(a b c . 0))))))
(define-test sacla-must-cons.97 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((((a))) b c))))
    (and (eql (setf (caaaar x) 0) 0) (equal x '((((0))) b c))))))
(define-test sacla-must-cons.98 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a ((b)) c))))
    (and (eql (setf (caaadr x) 0) 0) (equal x '(a ((0)) c))))))
(define-test sacla-must-cons.99 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a (aa)) b c))))
    (and (eql (setf (caadar x) 0) 0) (equal x '((a (0)) b c))))))
(define-test sacla-must-cons.100 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a b (c)))))
    (and (eql (setf (caaddr x) 0) 0) (equal x '(a b (0)))))))
(define-test sacla-must-cons.101 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(((a aa)) b c))))
    (and (eql (setf (cadaar x) 0) 0) (equal x '(((a 0)) b c))))))
(define-test sacla-must-cons.102 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a (b bb) c))))
    (and (eql (setf (cadadr x) 0) 0) (equal x '(a (b 0) c))))))
(define-test sacla-must-cons.103 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a aa aaa) b c))))
    (and (eql (setf (caddar x) 0) 0) (equal x '((a aa 0) b c))))))
(define-test sacla-must-cons.104 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd)))
    (and (eql (setf (cadddr x) 0) 0) (equal x '(a b c 0))))))
(define-test sacla-must-cons.105 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((((a . aa))) b c))))
    (and (eql (setf (cdaaar x) 0) 0) (equal x '((((a . 0))) b c))))))
(define-test sacla-must-cons.106 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a ((b . bb)) c))))
    (and (eql (setf (cdaadr x) 0) 0) (equal x '(a ((b . 0)) c))))))
(define-test sacla-must-cons.107 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a (aa . aaa)) b c))))
    (and (eql (setf (cdadar x) 0) 0) (equal x '((a (aa . 0)) b c))))))
(define-test sacla-must-cons.108 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a b (c . cc)))))
    (and (eql (setf (cdaddr x) 0) 0) (equal x '(a b (c . 0)))))))
(define-test sacla-must-cons.109 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(((a aa . aaa)) b c))))
    (and (eql (setf (cddaar x) 0) 0) (equal x '(((a aa . 0)) b c))))))
(define-test sacla-must-cons.110 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a (b bb . bbb) c))))
    (and (eql (setf (cddadr x) 0) 0) (equal x '(a (b bb . 0) c))))))
(define-test sacla-must-cons.111 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '((a aa aaa . aaaa) b c))))
    (and (eql (setf (cdddar x) 0) 0) (equal x '((a aa aaa . 0) b c))))))
(define-test sacla-must-cons.112 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a b c d . e))))
    (and (eql (setf (cddddr x) 0) 0) (equal x '(a b c d . 0))))))
(define-test sacla-must-cons.113 (:tag :sacla)
 (assert-true (eq (copy-tree 'a) 'a)))
(define-test sacla-must-cons.114 (:tag :sacla)
 (assert-true (eq (copy-tree nil) nil)))
(define-test sacla-must-cons.115 (:tag :sacla)
 (assert-true
  (let* ((a (list 'a))
         (b (list 'b))
         (c (list 'c))
         (x3 (cons c nil))
         (x2 (cons b x3))
         (x (cons a x2))
         (y (copy-tree x)))
    (and (not (eq x y))
         (not (eq (car x) (car y)))
         (not (eq (cdr x) (cdr y)))
         (not (eq (cadr x) (cadr y)))
         (not (eq (cddr x) (cddr y)))
         (not (eq (caddr x) (caddr y)))
         (eq (cdddr x) (cdddr y))
         (equal x y)
         (eq (car x) a)
         (eq (car a) 'a)
         (eq (cdr a) nil)
         (eq (cdr x) x2)
         (eq (car x2) b)
         (eq (car b) 'b)
         (eq (cdr b) nil)
         (eq (cdr x2) x3)
         (eq (car x3) c)
         (eq (car c) 'c)
         (eq (cdr c) nil)
         (eq (cdr x3) nil)))))
(define-test sacla-must-cons.116 (:tag :sacla)
 (assert-true
  (let* ((x (list (list 'a 1) (list 'b 2) (list 'c 3))) (y (copy-tree x)))
    (and (not (eq (car x) (car y)))
         (not (eq (cadr x) (cadr y)))
         (not (eq (caddr x) (caddr y)))))))
(define-test sacla-must-cons.117 (:tag :sacla)
 (assert-true
  (let* ((x (list (list (list 1)))) (y (copy-tree x)))
    (and (not (eq x y))
         (not (eq (car x) (car y)))
         (not (eq (caar x) (caar y)))))))
(define-test sacla-must-cons.118 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd)))
    (and (equal (sublis '((a . 1) (b . 2) (c . 3)) x) '(1 2 3 d))
         (equal x '(a b c d))))))
(define-test sacla-must-cons.119 (:tag :sacla)
 (assert-true
  (let* ((n (cons 'n nil))
         (m (cons 'm n))
         (l (cons 'l m))
         (x (sublis '((a . 1) (b . 2) (c . 3)) l)))
    (and (eq x l)
         (eq (car l) 'l)
         (eq (cdr l) m)
         (eq (car m) 'm)
         (eq (cdr m) n)
         (eq (car n) 'n)
         (eq (cdr n) nil)))))
(define-test sacla-must-cons.120 (:tag :sacla)
 (assert-true (eq (sublis 'nil 'nil) 'nil)))
(define-test sacla-must-cons.121 (:tag :sacla)
 (assert-true (equal (sublis 'nil '(1 2 3)) '(1 2 3))))
(define-test sacla-must-cons.122 (:tag :sacla)
 (assert-true (eq (sublis '((a . 1) (b . 2)) 'nil) nil)))
(define-test sacla-must-cons.123 (:tag :sacla)
 (assert-true
  (equal (sublis '((a b c) (b c d) (c d e)) '(a b c)) '((b c) (c d) (d e)))))
(define-test sacla-must-cons.124 (:tag :sacla)
 (assert-true
  (equal (sublis '((a . 1) (b . 2) (c . 3)) '(((a)) (b) c)) '(((1)) (2) 3))))
(define-test sacla-must-cons.125 (:tag :sacla)
 (assert-true
  (equal (sublis '(((a) . 1) ((b) . 2) ((c) . 3)) '((((a))) ((b)) (c)))
         '((((a))) ((b)) (c)))))
(define-test sacla-must-cons.126 (:tag :sacla)
 (assert-true
  (equal
   (sublis '(((a) . 1) ((b) . 2) ((c) . 3)) '((((a))) ((b)) (c)) :test #'equal)
   '(((1)) (2) 3))))
(define-test sacla-must-cons.127 (:tag :sacla)
 (assert-true
  (equal
   (sublis '(((a) . 1) ((b) . 2) ((c) . 3))
           '((((a))) ((b)) (c))
           :test-not (complement #'equal))
   '(((1)) (2) 3))))
(define-test sacla-must-cons.128 (:tag :sacla)
 (assert-true
  (equal (sublis '((a . 1) (b . 2) (c . 3)) '((((a))) ((b)) (c)) :key #'car)
         '(((1)) (2) 3))))
(define-test sacla-must-cons.129 (:tag :sacla)
 (assert-true
  (equal
   (sublis '(((a) . 1) ((b) . 2) ((c) . 3))
           '((((a))) ((b)) (c))
           :key #'car
           :test #'equal)
   '((1) 2 . 3))))
(define-test sacla-must-cons.130 (:tag :sacla)
 (assert-true
  (equal (nsublis '((a . 1) (b . 2) (c . 3)) (list 'a 'b 'c 'd)) '(1 2 3 d))))
(define-test sacla-must-cons.131 (:tag :sacla)
 (assert-true
  (let* ((x (list 'a 'b 'c 'd)) (y (nsublis '((a . 1) (b . 2) (c . 3)) x)))
    (and (eq x y) (equal x '(1 2 3 d))))))
(define-test sacla-must-cons.132 (:tag :sacla)
 (assert-true
  (let ((x (list 'l 'm 'n)))
    (and (eq (nsublis '((a . 1) (b . 2) (c . 3)) x) x) (equal x '(l m n))))))
(define-test sacla-must-cons.133 (:tag :sacla)
 (assert-true
  (let* ((n (cons 'n nil))
         (m (cons 'm n))
         (l (cons 'l m))
         (x (nsublis '((a . 1) (b . 2) (c . 3)) l)))
    (and (eq x l)
         (eq (car l) 'l)
         (eq (cdr l) m)
         (eq (car m) 'm)
         (eq (cdr m) n)
         (eq (car n) 'n)
         (eq (cdr n) nil)))))
(define-test sacla-must-cons.134 (:tag :sacla)
 (assert-true (eq (nsublis 'nil 'nil) 'nil)))
(define-test sacla-must-cons.135 (:tag :sacla)
 (assert-true (equal (nsublis 'nil '(1 2 3)) '(1 2 3))))
(define-test sacla-must-cons.136 (:tag :sacla)
 (assert-true (eq (nsublis '((a . 1) (b . 2)) 'nil) nil)))
(define-test sacla-must-cons.137 (:tag :sacla)
 (assert-true
  (equal (nsublis '((a b c) (b c d) (c d e)) (list 'a 'b 'c))
         '((b c) (c d) (d e)))))
(define-test sacla-must-cons.138 (:tag :sacla)
 (assert-true
  (equal (nsublis '((a . 1) (b . 2) (c . 3)) (copy-tree '(((a)) (b) c)))
         '(((1)) (2) 3))))
(define-test sacla-must-cons.139 (:tag :sacla)
 (assert-true
  (equal
   (nsublis '(((a) . 1) ((b) . 2) ((c) . 3)) (copy-tree '((((a))) ((b)) (c))))
   '((((a))) ((b)) (c)))))
(define-test sacla-must-cons.140 (:tag :sacla)
 (assert-true
  (equal
   (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
            (copy-tree '((((a))) ((b)) (c)))
            :test #'equal)
   '(((1)) (2) 3))))
(define-test sacla-must-cons.141 (:tag :sacla)
 (assert-true
  (equal
   (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
            (copy-tree '((((a))) ((b)) (c)))
            :test-not (complement #'equal))
   '(((1)) (2) 3))))
(define-test sacla-must-cons.142 (:tag :sacla)
 (assert-true
  (equal
   (nsublis '((a . 1) (b . 2) (c . 3))
            (copy-tree '((((a))) ((b)) (c)))
            :key #'car)
   '(((1)) (2) 3))))
(define-test sacla-must-cons.143 (:tag :sacla)
 (assert-true
  (equal
   (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
            (copy-tree '((((a))) ((b)) (c)))
            :key 'car
            :test #'equal)
   '((1) 2 . 3))))
(define-test sacla-must-cons.144 (:tag :sacla)
 (assert-true
  (let ((tree '(old (old) ((old)))))
    (equal (subst 'new 'old tree) '(new (new) ((new)))))))
(define-test sacla-must-cons.145 (:tag :sacla)
 (assert-true (eq (subst 'new 'old 'old) 'new)))
(define-test sacla-must-cons.146 (:tag :sacla)
 (assert-true (eq (subst 'new 'old 'not-old) 'not-old)))
(define-test sacla-must-cons.147 (:tag :sacla)
 (assert-true (equal (subst 'new '(b) '(a ((b))) :test #'equal) '(a (new)))))
(define-test sacla-must-cons.148 (:tag :sacla)
 (assert-true
  (equal (subst 'new '(b) '(a ((b))) :test-not (complement #'equal))
         '(a (new)))))
(define-test sacla-must-cons.149 (:tag :sacla)
 (assert-true
  (equal
   (subst 'x
          3
          '(1 (1 2) (1 2 3) (1 2 3 4))
          :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.150 (:tag :sacla)
 (assert-true
  (equal
   (subst 'x
          "D"
          '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))
          :test #'equalp
          :key #'(lambda (y) (and (listp y) (fourth y))))
   '("a" ("a" "b") ("a" "b" "c") x))))
(define-test sacla-must-cons.151 (:tag :sacla)
 (assert-true
  (equal (subst-if 'new #'(lambda (x) (eq x 'old)) '(old old)) '(new new))))
(define-test sacla-must-cons.152 (:tag :sacla)
 (assert-true (eq (subst-if 'new #'(lambda (x) (eq x 'old)) 'old) 'new)))
(define-test sacla-must-cons.153 (:tag :sacla)
 (assert-true
  (equal
   (subst-if 'x
             #'(lambda (x) (eql x 3))
             '(1 (1 2) (1 2 3) (1 2 3 4))
             :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.154 (:tag :sacla)
 (assert-true
  (let ((tree '(old (old) ((old)))))
    (equal (subst-if 'new #'(lambda (x) (eq x 'old)) tree)
           '(new (new) ((new)))))))
(define-test sacla-must-cons.155 (:tag :sacla)
 (assert-true (eq (subst-if 'new #'(lambda (x) (eq x 'old)) 'old) 'new)))
(define-test sacla-must-cons.156 (:tag :sacla)
 (assert-true
  (eq (subst-if 'new #'(lambda (x) (eq x 'old)) 'not-old) 'not-old)))
(define-test sacla-must-cons.157 (:tag :sacla)
 (assert-true
  (equal (subst-if 'new #'(lambda (x) (equal x '(b))) '(a ((b)))) '(a (new)))))
(define-test sacla-must-cons.158 (:tag :sacla)
 (assert-true
  (equal
   (subst-if 'x
             #'(lambda (x) (eql x 3))
             '(1 (1 2) (1 2 3) (1 2 3 4))
             :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.159 (:tag :sacla)
 (assert-true
  (equal
   (subst-if 'x
             #'(lambda (x) (equalp x "D"))
             '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))
             :key #'(lambda (y) (and (listp y) (fourth y))))
   '("a" ("a" "b") ("a" "b" "c") x))))
(define-test sacla-must-cons.160 (:tag :sacla)
 (assert-true
  (equal (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) '(old old))
         '(new new))))
(define-test sacla-must-cons.161 (:tag :sacla)
 (assert-true
  (eq (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old) 'new)))
(define-test sacla-must-cons.162 (:tag :sacla)
 (assert-true
  (equal
   (subst-if-not 'x
                 #'(lambda (x) (not (eql x 3)))
                 '(1 (1 2) (1 2 3) (1 2 3 4))
                 :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.163 (:tag :sacla)
 (assert-true
  (let ((tree '(old (old) ((old)))))
    (equal (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) tree)
           '(new (new) ((new)))))))
(define-test sacla-must-cons.164 (:tag :sacla)
 (assert-true
  (eq (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old) 'new)))
(define-test sacla-must-cons.165 (:tag :sacla)
 (assert-true
  (eq (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'not-old) 'not-old)))
(define-test sacla-must-cons.166 (:tag :sacla)
 (assert-true
  (equal (subst-if-not 'new #'(lambda (x) (not (equal x '(b)))) '(a ((b))))
         '(a (new)))))
(define-test sacla-must-cons.167 (:tag :sacla)
 (assert-true
  (equal
   (subst-if-not 'x
                 #'(lambda (x) (not (eql x 3)))
                 '(1 (1 2) (1 2 3) (1 2 3 4))
                 :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.168 (:tag :sacla)
 (assert-true
  (equal
   (subst-if-not 'x
                 #'(lambda (x) (not (equalp x "D")))
                 '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))
                 :key #'(lambda (y) (and (listp y) (fourth y))))
   '("a" ("a" "b") ("a" "b" "c") x))))
(define-test sacla-must-cons.169 (:tag :sacla)
 (assert-true
  (let ((tree '(old (old) ((old)))))
    (equal (nsubst 'new 'old (copy-tree tree)) '(new (new) ((new)))))))
(define-test sacla-must-cons.170 (:tag :sacla)
 (assert-true
  (let* ((tree (copy-tree '(old (old) ((old)))))
         (new-tree (nsubst 'new 'old tree)))
    (and (eq tree new-tree) (equal tree '(new (new) ((new))))))))
(define-test sacla-must-cons.171 (:tag :sacla)
 (assert-true (eq (nsubst 'new 'old 'old) 'new)))
(define-test sacla-must-cons.172 (:tag :sacla)
 (assert-true (eq (nsubst 'new 'old 'not-old) 'not-old)))
(define-test sacla-must-cons.173 (:tag :sacla)
 (assert-true
  (equal (nsubst 'new '(b) (copy-tree '(a ((b)))) :test #'equal) '(a (new)))))
(define-test sacla-must-cons.174 (:tag :sacla)
 (assert-true
  (equal
   (nsubst 'new '(b) (copy-tree '(a ((b)))) :test-not (complement #'equal))
   '(a (new)))))
(define-test sacla-must-cons.175 (:tag :sacla)
 (assert-true
  (equal
   (nsubst 'x
           3
           (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
           :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.176 (:tag :sacla)
 (assert-true
  (equal
   (nsubst 'x
           "D"
           (copy-tree '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d")))
           :test #'equalp
           :key #'(lambda (y) (and (listp y) (fourth y))))
   '("a" ("a" "b") ("a" "b" "c") x))))
(define-test sacla-must-cons.177 (:tag :sacla)
 (assert-true
  (equal (nsubst-if 'new #'(lambda (x) (eq x 'old)) (list 'old 'old))
         '(new new))))
(define-test sacla-must-cons.178 (:tag :sacla)
 (assert-true (eq (nsubst-if 'new #'(lambda (x) (eq x 'old)) 'old) 'new)))
(define-test sacla-must-cons.179 (:tag :sacla)
 (assert-true
  (let* ((x (copy-tree '(old (old) ((old)) (old) old)))
         (y (nsubst-if 'new #'(lambda (x) (eq x 'old)) x)))
    (and (eq x y) (equal x '(new (new) ((new)) (new) new))))))
(define-test sacla-must-cons.180 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if 'x
              #'(lambda (x) (eql x 3))
              (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
              :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.181 (:tag :sacla)
 (assert-true
  (let ((tree '(old (old) ((old)))))
    (equal (nsubst-if 'new #'(lambda (x) (eq x 'old)) (copy-tree tree))
           '(new (new) ((new)))))))
(define-test sacla-must-cons.182 (:tag :sacla)
 (assert-true (eq (nsubst-if 'new #'(lambda (x) (eq x 'old)) 'old) 'new)))
(define-test sacla-must-cons.183 (:tag :sacla)
 (assert-true
  (eq (nsubst-if 'new #'(lambda (x) (eq x 'old)) 'not-old) 'not-old)))
(define-test sacla-must-cons.184 (:tag :sacla)
 (assert-true
  (equal (nsubst-if 'new #'(lambda (x) (equal x '(b))) (copy-tree '(a ((b)))))
         '(a (new)))))
(define-test sacla-must-cons.185 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if 'x
              #'(lambda (x) (eql x 3))
              (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
              :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.186 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if 'x
              #'(lambda (x) (equalp x "D"))
              (copy-tree '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d")))
              :key #'(lambda (y) (and (listp y) (fourth y))))
   '("a" ("a" "b") ("a" "b" "c") x))))
(define-test sacla-must-cons.187 (:tag :sacla)
 (assert-true
  (equal (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) (list 'old 'old))
         '(new new))))
(define-test sacla-must-cons.188 (:tag :sacla)
 (assert-true
  (eq (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old) 'new)))
(define-test sacla-must-cons.189 (:tag :sacla)
 (assert-true
  (let* ((x (copy-tree '(old (old) ((old)) (old) old)))
         (y (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) x)))
    (and (eq x y) (equal x '(new (new) ((new)) (new) new))))))
(define-test sacla-must-cons.190 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if-not 'x
                  #'(lambda (x) (not (eql x 3)))
                  (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
                  :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.191 (:tag :sacla)
 (assert-true
  (let ((tree '(old (old) ((old)))))
    (equal
     (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) (copy-tree tree))
     '(new (new) ((new)))))))
(define-test sacla-must-cons.192 (:tag :sacla)
 (assert-true
  (eq (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old) 'new)))
(define-test sacla-must-cons.193 (:tag :sacla)
 (assert-true
  (eq (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'not-old) 'not-old)))
(define-test sacla-must-cons.194 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if-not 'new
                  #'(lambda (x) (not (equal x '(b))))
                  (copy-tree '(a ((b)))))
   '(a (new)))))
(define-test sacla-must-cons.195 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if-not 'x
                  #'(lambda (x) (not (eql x 3)))
                  (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
                  :key #'(lambda (y) (and (listp y) (third y))))
   '(1 (1 2) x x))))
(define-test sacla-must-cons.196 (:tag :sacla)
 (assert-true
  (equal
   (nsubst-if-not 'x
                  #'(lambda (x) (not (equalp x "D")))
                  (copy-tree '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d")))
                  :key #'(lambda (y) (and (listp y) (fourth y))))
   '("a" ("a" "b") ("a" "b" "c") x))))
(define-test sacla-must-cons.197 (:tag :sacla) (assert-true (tree-equal 'a 'a)))
(define-test sacla-must-cons.198 (:tag :sacla)
 (assert-true (not (tree-equal 'a 'b))))
(define-test sacla-must-cons.199 (:tag :sacla)
 (assert-true (tree-equal '(a (b (c))) '(a (b (c))))))
(define-test sacla-must-cons.200 (:tag :sacla)
 (assert-true (tree-equal '(a (b (c))) '(a (b (c))) :test #'eq)))
(define-test sacla-must-cons.201 (:tag :sacla)
 (assert-true
  (tree-equal '(a (b (c))) '(a (b (c))) :test-not (complement #'eq))))
(define-test sacla-must-cons.202 (:tag :sacla)
 (assert-true (not (tree-equal '("a" ("b" ("c"))) '("a" ("b" ("c")))))))
(define-test sacla-must-cons.203 (:tag :sacla)
 (assert-true (tree-equal '("a" ("b" ("c"))) '("a" ("b" ("c"))) :test #'equal)))
(define-test sacla-must-cons.204 (:tag :sacla)
 (assert-true
  (tree-equal '("a" ("b" ("c")))
              '("a" ("b" ("c")))
              :test-not (complement #'equal))))
(define-test sacla-must-cons.205 (:tag :sacla)
 (assert-true (not (tree-equal '(a b) '(a (b))))))
(define-test sacla-must-cons.206 (:tag :sacla)
 (assert-true (eq (copy-list 'nil) 'nil)))
(define-test sacla-must-cons.207 (:tag :sacla)
 (assert-true (equal (copy-list '(a b c)) '(a b c))))
(define-test sacla-must-cons.208 (:tag :sacla)
 (assert-true (equal (copy-list '(a . b)) '(a . b))))
(define-test sacla-must-cons.209 (:tag :sacla)
 (assert-true
  (let* ((x '(a b c)) (y (copy-list x)))
    (and (equal x y) (not (eq x y))))))
(define-test sacla-must-cons.210 (:tag :sacla)
 (assert-true
  (let* ((a (list 'a))
         (b (list 'b))
         (c (list 'c))
         (x (list a b c))
         (y (copy-list x)))
    (and (equal x y)
         (not (eq x y))
         (eq (car x) (car y))
         (eq (cadr x) (cadr y))
         (eq (caddr x) (caddr y))
         (eq (caar x) 'a)
         (eq (caadr x) 'b)
         (eq (caaddr x) 'c)))))
(define-test sacla-must-cons.211 (:tag :sacla) (assert-true (null (list))))
(define-test sacla-must-cons.212 (:tag :sacla)
 (assert-true (equal (list 1) '(1))))
(define-test sacla-must-cons.213 (:tag :sacla)
 (assert-true (equal (list 1 2 3) '(1 2 3))))
(define-test sacla-must-cons.214 (:tag :sacla)
 (assert-true (equal (list* 1 2 '(3)) '(1 2 3))))
(define-test sacla-must-cons.215 (:tag :sacla)
 (assert-true (equal (list* 1 2 'x) '(1 2 . x))))
(define-test sacla-must-cons.216 (:tag :sacla)
 (assert-true (equal (list* 1 2 '(3 4)) '(1 2 3 4))))
(define-test sacla-must-cons.217 (:tag :sacla) (assert-true (eq (list* 'x) 'x)))
(define-test sacla-must-cons.218 (:tag :sacla)
 (assert-true (eql (list-length 'nil) 0)))
(define-test sacla-must-cons.219 (:tag :sacla)
 (assert-true (eql (list-length '(1)) 1)))
(define-test sacla-must-cons.220 (:tag :sacla)
 (assert-true (eql (list-length '(1 2)) 2)))
(define-test sacla-must-cons.221 (:tag :sacla)
 (assert-true (null (list-length '#1=(1 2 3 4 . #1#)))))
(define-test sacla-must-cons.222 (:tag :sacla)
 (assert-true (equal (make-list 5) '(nil nil nil nil nil))))
(define-test sacla-must-cons.223 (:tag :sacla)
 (assert-true (equal (make-list 3 :initial-element 'rah) '(rah rah rah))))
(define-test sacla-must-cons.224 (:tag :sacla)
 (assert-true
  (equal (make-list 2 :initial-element '(1 2 3)) '((1 2 3) (1 2 3)))))
(define-test sacla-must-cons.225 (:tag :sacla)
 (assert-true (null (make-list 0))))
(define-test sacla-must-cons.226 (:tag :sacla)
 (assert-true (null (make-list 0 :initial-element 'new-element))))
(define-test sacla-must-cons.227 (:tag :sacla)
 (assert-true
  (let ((place nil))
    (and (equal (push 0 place) '(0)) (equal place '(0))))))
(define-test sacla-must-cons.228 (:tag :sacla)
 (assert-true
  (let ((place (list 1 2 3)))
    (and (equal (push 0 place) '(0 1 2 3)) (equal place '(0 1 2 3))))))
(define-test sacla-must-cons.229 (:tag :sacla)
 (assert-true
  (let ((a (list (list 1 2 3) 9)))
    (and (equal (push 0 (car a)) '(0 1 2 3)) (equal a '((0 1 2 3) 9))))))
(define-test sacla-must-cons.230 (:tag :sacla)
 (assert-true
  (let ((x (copy-tree '(a (b c) d))))
    (and (equal (push 'aa (cadr x)) '(aa b c)) (equal x '(a (aa b c) d))))))
(define-test sacla-must-cons.231 (:tag :sacla)
 (assert-true
  (let ((place (list 1 2 3)))
    (and (eql (pop place) 1) (equal place '(2 3))))))
(define-test sacla-must-cons.232 (:tag :sacla)
 (assert-true
  (let ((place 'nil))
    (and (eql (pop place) nil) (equal place 'nil)))))
(define-test sacla-must-cons.233 (:tag :sacla)
 (assert-true
  (let ((a (list (list 1 2 3) 9)))
    (and (eql (pop (car a)) 1) (equal a '((2 3) 9))))))
(define-test sacla-must-cons.234 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eq (pop (cdr x)) 'b) (equal x '(a c))))))
(define-test sacla-must-cons.235 (:tag :sacla)
 (assert-true (eq (first '(a . b)) 'a)))
(define-test sacla-must-cons.236 (:tag :sacla) (assert-true (null (first nil))))
(define-test sacla-must-cons.237 (:tag :sacla)
 (assert-true
  (let ((a (cons 1 2)))
    (eq (first (list a)) a))))
(define-test sacla-must-cons.238 (:tag :sacla)
 (assert-true (eq (first '#1=(a . #1#)) 'a)))
(define-test sacla-must-cons.239 (:tag :sacla)
 (assert-true (eql (first '(1 2 3)) '1)))
(define-test sacla-must-cons.240 (:tag :sacla)
 (assert-true (eql (second '(1 2 3)) '2)))
(define-test sacla-must-cons.241 (:tag :sacla)
 (assert-true (eql (third '(1 2 3)) '3)))
(define-test sacla-must-cons.242 (:tag :sacla)
 (assert-true (eql (fourth '(1 2 3 4)) '4)))
(define-test sacla-must-cons.243 (:tag :sacla)
 (assert-true (eql (fifth '(1 2 3 4 5)) '5)))
(define-test sacla-must-cons.244 (:tag :sacla)
 (assert-true (eql (sixth '(1 2 3 4 5 6)) '6)))
(define-test sacla-must-cons.245 (:tag :sacla)
 (assert-true (eql (seventh '(1 2 3 4 5 6 7)) '7)))
(define-test sacla-must-cons.246 (:tag :sacla)
 (assert-true (eql (eighth '(1 2 3 4 5 6 7 8)) '8)))
(define-test sacla-must-cons.247 (:tag :sacla)
 (assert-true (eql (ninth '(1 2 3 4 5 6 7 8 9)) '9)))
(define-test sacla-must-cons.248 (:tag :sacla)
 (assert-true (eql (tenth '(1 2 3 4 5 6 7 8 9 10)) '10)))
(define-test sacla-must-cons.249 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eql (setf (first x) 0) 0) (equal x '(0 b c))))))
(define-test sacla-must-cons.250 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eql (setf (second x) 0) 0) (equal x '(a 0 c))))))
(define-test sacla-must-cons.251 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eql (setf (third x) 0) 0) (equal x '(a b 0))))))
(define-test sacla-must-cons.252 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd)))
    (and (eql (setf (fourth x) 0) 0) (equal x '(a b c 0))))))
(define-test sacla-must-cons.253 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e)))
    (and (eql (setf (fifth x) 0) 0) (equal x '(a b c d 0))))))
(define-test sacla-must-cons.254 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e 'f)))
    (and (eql (setf (sixth x) 0) 0) (equal x '(a b c d e 0))))))
(define-test sacla-must-cons.255 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e 'f 'g)))
    (and (eql (setf (seventh x) 0) 0) (equal x '(a b c d e f 0))))))
(define-test sacla-must-cons.256 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e 'f 'g 'h)))
    (and (eql (setf (eighth x) 0) 0) (equal x '(a b c d e f g 0))))))
(define-test sacla-must-cons.257 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e 'f 'g 'h 'i)))
    (and (eql (setf (ninth x) 0) 0) (equal x '(a b c d e f g h 0))))))
(define-test sacla-must-cons.258 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
    (and (eql (setf (tenth x) 0) 0) (equal x '(a b c d e f g h i 0))))))
(define-test sacla-must-cons.259 (:tag :sacla)
 (assert-true
  (let ((x '(a b c)))
    (eq (nthcdr 0 x) x))))
(define-test sacla-must-cons.260 (:tag :sacla)
 (assert-true
  (let ((x '(a b c)))
    (eq (nthcdr 1 x) (cdr x)))))
(define-test sacla-must-cons.261 (:tag :sacla)
 (assert-true
  (let ((x '(a b c)))
    (eq (nthcdr 2 x) (cddr x)))))
(define-test sacla-must-cons.262 (:tag :sacla)
 (assert-true
  (let ((x '(a b c)))
    (eq (nthcdr 2 x) (cddr x)))))
(define-test sacla-must-cons.263 (:tag :sacla)
 (assert-true
  (let ((x '(a b c)))
    (eq (nthcdr 3 x) (cdddr x)))))
(define-test sacla-must-cons.264 (:tag :sacla)
 (assert-true (equal (nthcdr 0 '(0 1 2)) '(0 1 2))))
(define-test sacla-must-cons.265 (:tag :sacla)
 (assert-true (equal (nthcdr 1 '(0 1 2)) '(1 2))))
(define-test sacla-must-cons.266 (:tag :sacla)
 (assert-true (equal (nthcdr 2 '(0 1 2)) '(2))))
(define-test sacla-must-cons.267 (:tag :sacla)
 (assert-true (equal (nthcdr 3 '(0 1 2)) 'nil)))
(define-test sacla-must-cons.268 (:tag :sacla)
 (assert-true (eql (nthcdr 1 '(0 . 1)) 1)))
(define-test sacla-must-cons.269 (:tag :sacla)
 (assert-true (eql (nth 0 '(a b c)) 'a)))
(define-test sacla-must-cons.270 (:tag :sacla)
 (assert-true (eql (nth 1 '(a b c)) 'b)))
(define-test sacla-must-cons.271 (:tag :sacla)
 (assert-true (eql (nth 2 '(a b c)) 'c)))
(define-test sacla-must-cons.272 (:tag :sacla)
 (assert-true (eql (nth 3 '(a b c)) 'nil)))
(define-test sacla-must-cons.273 (:tag :sacla)
 (assert-true (eql (nth 4 '(a b c)) 'nil)))
(define-test sacla-must-cons.274 (:tag :sacla)
 (assert-true (eql (nth 5 '(a b c)) 'nil)))
(define-test sacla-must-cons.275 (:tag :sacla)
 (assert-true (eql (nth 6 '(a b c)) 'nil)))
(define-test sacla-must-cons.276 (:tag :sacla)
 (assert-true (eq (nth 0 '(a . b)) 'a)))
(define-test sacla-must-cons.277 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eq (setf (nth 0 x) 'z) 'z) (equal x '(z b c))))))
(define-test sacla-must-cons.278 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eq (setf (nth 1 x) 'z) 'z) (equal x '(a z c))))))
(define-test sacla-must-cons.279 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (and (eq (setf (nth 2 x) 'z) 'z) (equal x '(a b z))))))
(define-test sacla-must-cons.280 (:tag :sacla)
 (assert-true
  (let ((0-to-3 (list 0 1 2 3)))
    (and (equal (setf (nth 2 0-to-3) "two") "two")
         (equal 0-to-3 '(0 1 "two" 3))))))
(define-test sacla-must-cons.281 (:tag :sacla) (assert-true (eq (nconc) 'nil)))
(define-test sacla-must-cons.282 (:tag :sacla)
 (assert-true
  (equal (nconc nil (list 'a 'b 'c) (list 'd 'e 'f)) '(a b c d e f))))
(define-test sacla-must-cons.283 (:tag :sacla)
 (assert-true
  (equal (nconc nil nil (list 'a 'b 'c) (list 'd 'e 'f)) '(a b c d e f))))
(define-test sacla-must-cons.284 (:tag :sacla)
 (assert-true
  (equal (nconc nil nil nil (list 'a 'b 'c) (list 'd 'e 'f)) '(a b c d e f))))
(define-test sacla-must-cons.285 (:tag :sacla)
 (assert-true
  (let* ((x (list 'a 'b 'c)))
    (eq (nconc x) x))))
(define-test sacla-must-cons.286 (:tag :sacla)
 (assert-true
  (let* ((x (list 'a 'b 'c)) (y (list 'd 'e 'f)) (list (nconc x y)))
    (and (eq list x) (eq (nthcdr 3 list) y) (equal list '(a b c d e f))))))
(define-test sacla-must-cons.287 (:tag :sacla)
 (assert-true
  (let* ((x (list 'a)) (y (list 'b)) (z (list 'c)) (list (nconc x y z)))
    (and (eq x list)
         (eq (first list) 'a)
         (eq y (cdr list))
         (eq (second list) 'b)
         (eq z (cddr list))
         (eq (third list) 'c)))))
(define-test sacla-must-cons.288 (:tag :sacla)
 (assert-true (equal (append '(a b) 'nil '(c d) '(e f)) '(a b c d e f))))
(define-test sacla-must-cons.289 (:tag :sacla) (assert-true (null (append))))
(define-test sacla-must-cons.290 (:tag :sacla)
 (assert-true (null (append 'nil))))
(define-test sacla-must-cons.291 (:tag :sacla)
 (assert-true (null (append 'nil 'nil))))
(define-test sacla-must-cons.292 (:tag :sacla)
 (assert-true (eq (append 'a) 'a)))
(define-test sacla-must-cons.293 (:tag :sacla)
 (assert-true (eq (append 'nil 'a) 'a)))
(define-test sacla-must-cons.294 (:tag :sacla)
 (assert-true (eq (append 'nil 'nil 'a) 'a)))
(define-test sacla-must-cons.295 (:tag :sacla)
 (assert-true (equal (append '(a b) 'c) '(a b . c))))
(define-test sacla-must-cons.296 (:tag :sacla)
 (assert-true
  (let* ((x '(a b c)) (y '(d e f)) (z (append x y)))
    (and (equal z '(a b c d e f)) (eq (nthcdr 3 z) y) (not (eq x z))))))
(define-test sacla-must-cons.297 (:tag :sacla)
 (assert-true (equal (revappend '(a b c) '(d e f)) '(c b a d e f))))
(define-test sacla-must-cons.298 (:tag :sacla)
 (assert-true
  (let* ((x '(a b c)) (y '(d e f)) (z (revappend x y)))
    (and (equal z '(c b a d e f)) (not (eq x z)) (eq (nthcdr 3 z) y)))))
(define-test sacla-must-cons.299 (:tag :sacla)
 (assert-true
  (let ((x '(a b c)))
    (eq (revappend 'nil x) x))))
(define-test sacla-must-cons.300 (:tag :sacla)
 (assert-true (null (revappend 'nil 'nil))))
(define-test sacla-must-cons.301 (:tag :sacla)
 (assert-true (eq (revappend 'nil 'a) 'a)))
(define-test sacla-must-cons.302 (:tag :sacla)
 (assert-true (equal (revappend '(a) 'b) '(a . b))))
(define-test sacla-must-cons.303 (:tag :sacla)
 (assert-true (equal (revappend '(a) 'nil) '(a))))
(define-test sacla-must-cons.304 (:tag :sacla)
 (assert-true (equal (revappend '(1 2 3) 'nil) '(3 2 1))))
(define-test sacla-must-cons.305 (:tag :sacla)
 (assert-true (equal (nreconc (list 'a 'b 'c) '(d e f)) '(c b a d e f))))
(define-test sacla-must-cons.306 (:tag :sacla)
 (assert-true
  (let* ((x (list 'a 'b 'c)) (y '(d e f)) (z (nreconc x y)))
    (and (equal z '(c b a d e f)) (eq (nthcdr 3 z) y)))))
(define-test sacla-must-cons.307 (:tag :sacla)
 (assert-true
  (let ((x (list 'a 'b 'c)))
    (eq (nreconc 'nil x) x))))
(define-test sacla-must-cons.308 (:tag :sacla)
 (assert-true (null (nreconc 'nil 'nil))))
(define-test sacla-must-cons.309 (:tag :sacla)
 (assert-true (eq (nreconc 'nil 'a) 'a)))
(define-test sacla-must-cons.310 (:tag :sacla)
 (assert-true (equal (nreconc (list 'a) 'b) '(a . b))))
(define-test sacla-must-cons.311 (:tag :sacla)
 (assert-true (equal (nreconc (list 'a) 'nil) '(a))))
(define-test sacla-must-cons.312 (:tag :sacla)
 (assert-true (equal (nreconc (list 1 2 3) 'nil) '(3 2 1))))
(define-test sacla-must-cons.313 (:tag :sacla)
 (assert-true (null (butlast nil))))
(define-test sacla-must-cons.314 (:tag :sacla)
 (assert-true (null (butlast nil 1))))
(define-test sacla-must-cons.315 (:tag :sacla)
 (assert-true (null (butlast nil 2))))
(define-test sacla-must-cons.316 (:tag :sacla)
 (assert-true (null (butlast nil 3))))
(define-test sacla-must-cons.317 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5)) '(1 2 3 4))))
(define-test sacla-must-cons.318 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 1) '(1 2 3 4))))
(define-test sacla-must-cons.319 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 2) '(1 2 3))))
(define-test sacla-must-cons.320 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 3) '(1 2))))
(define-test sacla-must-cons.321 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 4) '(1))))
(define-test sacla-must-cons.322 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 5) 'nil)))
(define-test sacla-must-cons.323 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 6) 'nil)))
(define-test sacla-must-cons.324 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5) 7) 'nil)))
(define-test sacla-must-cons.325 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6)) '(1 2 3 4))))
(define-test sacla-must-cons.326 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 1) '(1 2 3 4))))
(define-test sacla-must-cons.327 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 2) '(1 2 3))))
(define-test sacla-must-cons.328 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 3) '(1 2))))
(define-test sacla-must-cons.329 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 4) '(1))))
(define-test sacla-must-cons.330 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 5) 'nil)))
(define-test sacla-must-cons.331 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 6) 'nil)))
(define-test sacla-must-cons.332 (:tag :sacla)
 (assert-true (equal (butlast '(1 2 3 4 5 . 6) 7) 'nil)))
(define-test sacla-must-cons.333 (:tag :sacla)
 (assert-true
  (let ((a '(1 2 3 4 5)))
    (equal (butlast a 3) '(1 2))
    (equal a '(1 2 3 4 5)))))
(define-test sacla-must-cons.334 (:tag :sacla)
 (assert-true (null (nbutlast nil))))
(define-test sacla-must-cons.335 (:tag :sacla)
 (assert-true (null (nbutlast nil 1))))
(define-test sacla-must-cons.336 (:tag :sacla)
 (assert-true (null (nbutlast nil 2))))
(define-test sacla-must-cons.337 (:tag :sacla)
 (assert-true (null (nbutlast nil 3))))
(define-test sacla-must-cons.338 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5)) '(1 2 3 4))))
(define-test sacla-must-cons.339 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 1) '(1 2 3 4))))
(define-test sacla-must-cons.340 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 2) '(1 2 3))))
(define-test sacla-must-cons.341 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 3) '(1 2))))
(define-test sacla-must-cons.342 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 4) '(1))))
(define-test sacla-must-cons.343 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 5) 'nil)))
(define-test sacla-must-cons.344 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 6) 'nil)))
(define-test sacla-must-cons.345 (:tag :sacla)
 (assert-true (equal (nbutlast (list 1 2 3 4 5) 7) 'nil)))
(define-test sacla-must-cons.346 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6)) '(1 2 3 4))))
(define-test sacla-must-cons.347 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 1) '(1 2 3 4))))
(define-test sacla-must-cons.348 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 2) '(1 2 3))))
(define-test sacla-must-cons.349 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 3) '(1 2))))
(define-test sacla-must-cons.350 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 4) '(1))))
(define-test sacla-must-cons.351 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 5) 'nil)))
(define-test sacla-must-cons.352 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 6) 'nil)))
(define-test sacla-must-cons.353 (:tag :sacla)
 (assert-true (equal (nbutlast (list* 1 2 3 4 5 6) 7) 'nil)))
(define-test sacla-must-cons.354 (:tag :sacla)
 (assert-true
  (let* ((a '(1 2 3 4 5)) (b (nbutlast a 3)))
    (and (eq a b) (equal a '(1 2))))))
(define-test sacla-must-cons.355 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4 5 6 7 8 9)))
    (eq (last x) (nthcdr 9 x)))))
(define-test sacla-must-cons.356 (:tag :sacla) (assert-true (null (last nil))))
(define-test sacla-must-cons.357 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 . 2)))
    (eq (last x) (cdr x)))))
(define-test sacla-must-cons.358 (:tag :sacla)
 (assert-true (eql (last '(1 . 2) 0) 2)))
(define-test sacla-must-cons.359 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 0) nil))))
(define-test sacla-must-cons.360 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x) (nthcdr 4 x)))))
(define-test sacla-must-cons.361 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 1) (nthcdr 4 x)))))
(define-test sacla-must-cons.362 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 2) (cdddr x)))))
(define-test sacla-must-cons.363 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 3) (cddr x)))))
(define-test sacla-must-cons.364 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 4) (cdr x)))))
(define-test sacla-must-cons.365 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 5) x))))
(define-test sacla-must-cons.366 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 6) x))))
(define-test sacla-must-cons.367 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 7) x))))
(define-test sacla-must-cons.368 (:tag :sacla)
 (assert-true
  (let ((x '(0 1 2 3 4)))
    (eq (last x 8) x))))
(define-test sacla-must-cons.369 (:tag :sacla) (assert-true (tailp 'nil 'nil)))
(define-test sacla-must-cons.370 (:tag :sacla) (assert-true (tailp 'nil '(1))))
(define-test sacla-must-cons.371 (:tag :sacla)
 (assert-true (tailp 'nil '(1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-cons.372 (:tag :sacla)
 (assert-true
  (let ((x '(1 2 3)))
    (and (tailp x x)
         (tailp (cdr x) x)
         (tailp (cddr x) x)
         (tailp (cdddr x) x)))))
(define-test sacla-must-cons.373 (:tag :sacla)
 (assert-true
  (let ((x '(1 . 2)))
    (and (tailp x x) (tailp (cdr x) x)))))
(define-test sacla-must-cons.374 (:tag :sacla)
 (assert-true (not (tailp nil '(1 . 2)))))
(define-test sacla-must-cons.375 (:tag :sacla)
 (assert-true (not (tailp 'x '(1 2 3 4 5 6)))))
(define-test sacla-must-cons.376 (:tag :sacla)
 (assert-true (not (tailp (list 1 2 3) '(1 2 3)))))
(define-test sacla-must-cons.377 (:tag :sacla)
 (assert-true
  (let ((x '(1 2 3 4 5 . 6)))
    (tailp (last x) x))))
(define-test sacla-must-cons.378 (:tag :sacla)
 (assert-true
  (let ((x '(1 2 3 4 5 . 6)))
    (tailp (last x) x))))
(define-test sacla-must-cons.379 (:tag :sacla)
 (assert-true (null (ldiff 'nil 'nil))))
(define-test sacla-must-cons.380 (:tag :sacla)
 (assert-true (equal (ldiff '(1 . 2) 2) '(1))))
(define-test sacla-must-cons.381 (:tag :sacla)
 (assert-true (equal (ldiff '(1 2 3 4 5 6 7 8 9) 'nil) '(1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-cons.382 (:tag :sacla)
 (assert-true
  (let ((x '(1 2 3)))
    (and (null (ldiff x x))
         (equal (ldiff x (cdr x)) '(1))
         (equal (ldiff x (cddr x)) '(1 2))
         (equal (ldiff x (cdddr x)) '(1 2 3))))))
(define-test sacla-must-cons.383 (:tag :sacla)
 (assert-true
  (let* ((x '(1 2 3)) (y '(a b c)) (z (ldiff x y)))
    (and (not (eq x z)) (equal z '(1 2 3))))))
(define-test sacla-must-cons.384 (:tag :sacla)
 (assert-true (equal (member 'a '(a b c d)) '(a b c d))))
(define-test sacla-must-cons.385 (:tag :sacla)
 (assert-true (equal (member 'b '(a b c d)) '(b c d))))
(define-test sacla-must-cons.386 (:tag :sacla)
 (assert-true (equal (member 'c '(a b c d)) '(c d))))
(define-test sacla-must-cons.387 (:tag :sacla)
 (assert-true (equal (member 'd '(a b c d)) '(d))))
(define-test sacla-must-cons.388 (:tag :sacla)
 (assert-true (equal (member 'e '(a b c d)) 'nil)))
(define-test sacla-must-cons.389 (:tag :sacla)
 (assert-true (equal (member 'f '(a b c d)) 'nil)))
(define-test sacla-must-cons.390 (:tag :sacla)
 (assert-true
  (let ((x '(a b c d)))
    (eq (member 'a x) x)
    (eq (member 'b x) (cdr x))
    (eq (member 'c x) (cddr x))
    (eq (member 'd x) (cdddr x))
    (eq (member 'e x) nil))))
(define-test sacla-must-cons.391 (:tag :sacla)
 (assert-true (equal (member 'a '(a b c d) :test #'eq) '(a b c d))))
(define-test sacla-must-cons.392 (:tag :sacla)
 (assert-true (equal (member 'b '(a b c d) :test #'eq) '(b c d))))
(define-test sacla-must-cons.393 (:tag :sacla)
 (assert-true (equal (member 'c '(a b c d) :test #'eq) '(c d))))
(define-test sacla-must-cons.394 (:tag :sacla)
 (assert-true (equal (member 'd '(a b c d) :test #'eq) '(d))))
(define-test sacla-must-cons.395 (:tag :sacla)
 (assert-true (equal (member 'e '(a b c d) :test #'eq) 'nil)))
(define-test sacla-must-cons.396 (:tag :sacla)
 (assert-true (equal (member 'f '(a b c d) :test #'eq) 'nil)))
(define-test sacla-must-cons.397 (:tag :sacla)
 (assert-true (null (member 'a 'nil))))
(define-test sacla-must-cons.398 (:tag :sacla)
 (assert-true
  (let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
         (y (member 'd x :key #'cdr :test #'eq)))
    (and (equal y '((4 . d) (5 . e))) (eq y (nthcdr 3 x))))))
(define-test sacla-must-cons.399 (:tag :sacla)
 (assert-true
  (let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
         (y (member 'd x :key #'cdr)))
    (and (equal y '((4 . d) (5 . e))) (eq y (nthcdr 3 x))))))
(define-test sacla-must-cons.400 (:tag :sacla)
 (assert-true
  (let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
         (y (member 'd x :key #'cdr :test-not (complement #'eq))))
    (and (equal y '((4 . d) (5 . e))) (eq y (nthcdr 3 x))))))
(define-test sacla-must-cons.401 (:tag :sacla)
 (assert-true
  (let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
         (y (member 'd x :test-not (complement #'eq))))
    (eq y nil))))
(define-test sacla-must-cons.402 (:tag :sacla)
 (assert-true
  (equal (member 2 '((1 . 2) (3 . 4)) :test-not #'= :key #'cdr) '((3 . 4)))))
(define-test sacla-must-cons.403 (:tag :sacla)
 (assert-true
  (equal (member-if #'(lambda (x) (eql x 'a)) '(a b c d)) '(a b c d))))
(define-test sacla-must-cons.404 (:tag :sacla)
 (assert-true
  (equal (member-if #'(lambda (x) (eql x 'b)) '(a b c d)) '(b c d))))
(define-test sacla-must-cons.405 (:tag :sacla)
 (assert-true (equal (member-if #'(lambda (x) (eql x 'c)) '(a b c d)) '(c d))))
(define-test sacla-must-cons.406 (:tag :sacla)
 (assert-true (equal (member-if #'(lambda (x) (eql x 'd)) '(a b c d)) '(d))))
(define-test sacla-must-cons.407 (:tag :sacla)
 (assert-true (equal (member-if #'(lambda (x) (eql x 'e)) '(a b c d)) 'nil)))
(define-test sacla-must-cons.408 (:tag :sacla)
 (assert-true (equal (member-if #'(lambda (x) (eql x 'f)) '(a b c d)) 'nil)))
(define-test sacla-must-cons.409 (:tag :sacla)
 (assert-true (null (member-if #'(lambda (x) (eql x 'a)) 'nil))))
(define-test sacla-must-cons.410 (:tag :sacla)
 (assert-true
  (let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
         (y (member-if #'(lambda (p) (eq p 'd)) x :key #'cdr)))
    (and (equal y '((4 . d) (5 . e))) (eq y (nthcdr 3 x))))))
(define-test sacla-must-cons.411 (:tag :sacla)
 (assert-true
  (equal (member-if #'cdr '((1) (2 . 2) (3 3 . 3))) '((2 . 2) (3 3 . 3)))))
(define-test sacla-must-cons.412 (:tag :sacla)
 (assert-true (null (member-if #'zerop '(7 8 9)))))
(define-test sacla-must-cons.413 (:tag :sacla)
 (assert-true
  (equal (member-if-not #'(lambda (x) (not (eql x 'a))) '(a b c d))
         '(a b c d))))
(define-test sacla-must-cons.414 (:tag :sacla)
 (assert-true
  (equal (member-if-not #'(lambda (x) (not (eql x 'b))) '(a b c d)) '(b c d))))
(define-test sacla-must-cons.415 (:tag :sacla)
 (assert-true
  (equal (member-if-not #'(lambda (x) (not (eql x 'c))) '(a b c d)) '(c d))))
(define-test sacla-must-cons.416 (:tag :sacla)
 (assert-true
  (equal (member-if-not #'(lambda (x) (not (eql x 'd))) '(a b c d)) '(d))))
(define-test sacla-must-cons.417 (:tag :sacla)
 (assert-true
  (equal (member-if-not #'(lambda (x) (not (eql x 'e))) '(a b c d)) 'nil)))
(define-test sacla-must-cons.418 (:tag :sacla)
 (assert-true
  (equal (member-if-not #'(lambda (x) (not (eql x 'f))) '(a b c d)) 'nil)))
(define-test sacla-must-cons.419 (:tag :sacla)
 (assert-true (null (member-if-not #'(lambda (x) (not (eql x 'a))) 'nil))))
(define-test sacla-must-cons.420 (:tag :sacla)
 (assert-true
  (let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
         (y (member-if-not #'(lambda (p) (not (eq p 'd))) x :key #'cdr)))
    (and (equal y '((4 . d) (5 . e))) (eq y (nthcdr 3 x))))))
(define-test sacla-must-cons.421 (:tag :sacla)
 (assert-true
  (let ((x '((1 2) (2 3) (3 4) (4 5))) (y nil))
    (and (eq (mapc #'(lambda (a) (push (car a) y)) x) x)
         (equal y '(4 3 2 1))))))
(define-test sacla-must-cons.422 (:tag :sacla)
 (assert-true
  (let ((dummy nil) (list-1 '(1 2 3 4)))
    (and
     (eq
      (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
            list-1
            '(a b c d e)
            '(x y z))
      list-1)
     (equal dummy '(1 a x 2 b y 3 c z))))))
(define-test sacla-must-cons.423 (:tag :sacla)
 (assert-true
  (let* ((x '(0 1 2 3))
         (y nil)
         (z
          (mapc #'(lambda (a b c) (push (list a b c) y))
                x
                '(1 2 3 4)
                '(2 3 4 5))))
    (and (eq z x) (equal y '((3 4 5) (2 3 4) (1 2 3) (0 1 2)))))))
(define-test sacla-must-cons.424 (:tag :sacla)
 (assert-true
  (let* ((x '(0 1 2 3))
         (y nil)
         (z
          (mapc #'(lambda (a b c) (push (list a b c) y))
                nil
                x
                '(1 2 3 4)
                '(2 3 4 5))))
    (and (null z) (null y)))))
(define-test sacla-must-cons.425 (:tag :sacla)
 (assert-true
  (let ((sum 0))
    (mapc #'(lambda (&rest rest) (setq sum (+ sum (apply #'+ rest))))
          '(0 1 2)
          '(1 2 0)
          '(2 0 1))
    (eql sum 9))))
(define-test sacla-must-cons.426 (:tag :sacla)
 (assert-true
  (let ((result 'initial-value) (list-1 nil))
    (and
     (eq (mapc #'(lambda (a b) (setq result (cons (cons a b) result))) list-1)
         list-1)
     (eq result 'initial-value)))))
(define-test sacla-must-cons.427 (:tag :sacla)
 (assert-true
  (let ((result 'initial-value) (list-1 nil))
    (and
     (eq
      (mapc #'(lambda (a b) (setq result (cons (cons a b) result)))
            list-1
            '(1 2 3))
      list-1)
     (eq result 'initial-value)))))
(define-test sacla-must-cons.428 (:tag :sacla)
 (assert-true
  (let ((result 'initial-value) (list-1 '(1 2 3)))
    (and
     (eq
      (mapc #'(lambda (a b) (setq result (cons (cons a b) result)))
            list-1
            'nil)
      list-1)
     (eq result 'initial-value)))))
(define-test sacla-must-cons.429 (:tag :sacla)
 (assert-true (equal (mapcar #'car '((1 2) (2 3) (3 4) (4 5))) '(1 2 3 4))))
(define-test sacla-must-cons.430 (:tag :sacla)
 (assert-true (null (mapcar #'identity 'nil))))
(define-test sacla-must-cons.431 (:tag :sacla)
 (assert-true
  (equal (mapcar #'list '(0 1 2 3) '(a b c d) '(w x y z))
         '((0 a w) (1 b x) (2 c y) (3 d z)))))
(define-test sacla-must-cons.432 (:tag :sacla)
 (assert-true (null (mapcar #'list 'nil '(0 1 2 3) '(1 2 3 4) '(2 3 4 5)))))
(define-test sacla-must-cons.433 (:tag :sacla)
 (assert-true (null (mapcar #'list '(0 1 2 3) 'nil '(1 2 3 4) '(2 3 4 5)))))
(define-test sacla-must-cons.434 (:tag :sacla)
 (assert-true (null (mapcar #'list '(0 1 2 3) '(1 2 3 4) 'nil '(2 3 4 5)))))
(define-test sacla-must-cons.435 (:tag :sacla)
 (assert-true (null (mapcar #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) 'nil))))
(define-test sacla-must-cons.436 (:tag :sacla)
 (assert-true (equal (mapcar #'list '(0) '(a b) '(x y z)) '((0 a x)))))
(define-test sacla-must-cons.437 (:tag :sacla)
 (assert-true (equal (mapcar #'list '(a b) '(0) '(x y z)) '((a 0 x)))))
(define-test sacla-must-cons.438 (:tag :sacla)
 (assert-true (equal (mapcar #'list '(a b) '(x y z) '(0)) '((a x 0)))))
(define-test sacla-must-cons.439 (:tag :sacla)
 (assert-true
  (equal (mapcar #'cons '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))))
(define-test sacla-must-cons.440 (:tag :sacla)
 (assert-true
  (equal (mapcan #'cdr (copy-tree '((1 2) (2 3) (3 4) (4 5)))) '(2 3 4 5))))
(define-test sacla-must-cons.441 (:tag :sacla)
 (assert-true
  (equal
   (mapcan #'append
           '((1 2 3) (4 5 6) (7 8 9))
           '((a) (b c) (d e f))
           (list (list 'x 'y 'z) (list 'y 'z) (list 'z)))
   '(1 2 3 a x y z 4 5 6 b c y z 7 8 9 d e f z))))
(define-test sacla-must-cons.442 (:tag :sacla)
 (assert-true
  (null (mapcan #'append '((1 2 3) (4 5 6) (7 8 9)) '((a) (b c)) 'nil))))
(define-test sacla-must-cons.443 (:tag :sacla)
 (assert-true
  (null (mapcan #'append '((1 2 3) (4 5 6) (7 8 9)) 'nil '((a) (b c))))))
(define-test sacla-must-cons.444 (:tag :sacla)
 (assert-true
  (null (mapcan #'append 'nil '((1 2 3) (4 5 6) (7 8 9)) '((a) (b c))))))
(define-test sacla-must-cons.445 (:tag :sacla)
 (assert-true
  (equal
   (mapcan #'list
           (list 1 2 3 4 5)
           (list 2 3 4 5 6)
           (list 3 4 5 6 7)
           (list 4 5 6 7 8))
   '(1 2 3 4 2 3 4 5 3 4 5 6 4 5 6 7 5 6 7 8))))
(define-test sacla-must-cons.446 (:tag :sacla)
 (assert-true
  (equal
   (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
           '(nil nil nil d e)
           '(1 2 3 4 5 6))
   '(d 4 e 5))))
(define-test sacla-must-cons.447 (:tag :sacla)
 (assert-true
  (equal (mapcan #'(lambda (x) (and (numberp x) (list x))) '(a 1 b c 3 4 d 5))
         '(1 3 4 5))))
(define-test sacla-must-cons.448 (:tag :sacla)
 (assert-true
  (equal (maplist #'identity '(a b c d)) '((a b c d) (b c d) (c d) (d)))))
(define-test sacla-must-cons.449 (:tag :sacla)
 (assert-true
  (equal (maplist #'car '((1 2) (2 3) (3 4) (4 5)))
         '((1 2) (2 3) (3 4) (4 5)))))
(define-test sacla-must-cons.450 (:tag :sacla)
 (assert-true
  (equal (maplist #'list '(a b c) '(b c d) '(c d e))
         '(((a b c) (b c d) (c d e)) ((b c) (c d) (d e)) ((c) (d) (e))))))
(define-test sacla-must-cons.451 (:tag :sacla)
 (assert-true
  (equal (maplist #'append '(a b c) '(b c d) '(c d e))
         '((a b c b c d c d e) (b c c d d e) (c d e)))))
(define-test sacla-must-cons.452 (:tag :sacla)
 (assert-true (equal (maplist #'append '(a b c) '(b c) '(c)) '((a b c b c c)))))
(define-test sacla-must-cons.453 (:tag :sacla)
 (assert-true (null (maplist #'append 'nil '(a b c) '(b c) '(c)))))
(define-test sacla-must-cons.454 (:tag :sacla)
 (assert-true (null (maplist #'append '(a b c) 'nil '(b c) '(c)))))
(define-test sacla-must-cons.455 (:tag :sacla)
 (assert-true (null (maplist #'append '(a b c) '(b c) '(c) 'nil))))
(define-test sacla-must-cons.456 (:tag :sacla)
 (assert-true
  (let ((x '((1 2) (2 3) (3 4) (4 5))) (y nil))
    (and (eq (mapl #'(lambda (a) (push (car a) y)) x) x)
         (equal y '((4 5) (3 4) (2 3) (1 2)))))))
(define-test sacla-must-cons.457 (:tag :sacla)
 (assert-true
  (let ((x nil))
    (and (null (mapl #'(lambda (&rest rest) (push rest x)) 'nil '(0) '(0 1)))
         (null x)))))
(define-test sacla-must-cons.458 (:tag :sacla)
 (assert-true
  (let ((x nil))
    (and
     (equal (mapl #'(lambda (&rest rest) (push rest x)) '(0) 'nil '(0 1)) '(0))
     (null x)))))
(define-test sacla-must-cons.459 (:tag :sacla)
 (assert-true
  (let ((x nil))
    (and
     (equal (mapl #'(lambda (&rest rest) (push rest x)) '(0) '(0 1) 'nil) '(0))
     (null x)))))
(define-test sacla-must-cons.460 (:tag :sacla)
 (assert-true
  (equal (mapcon #'car (copy-tree '((1 2) (2 3) (3 4) (4 5))))
         '(1 2 2 3 3 4 4 5))))
(define-test sacla-must-cons.461 (:tag :sacla)
 (assert-true
  (equal (mapcon #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '(3 4 5 6))
         '((0 1 2 3) (1 2 3 4) (2 3 4 5) (3 4 5 6) (1 2 3) (2 3 4) (3 4 5)
           (4 5 6) (2 3) (3 4) (4 5) (5 6) (3) (4) (5) (6)))))
(define-test sacla-must-cons.462 (:tag :sacla)
 (assert-true
  (null (mapcon #'list 'nil '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '(3 4 5 6)))))
(define-test sacla-must-cons.463 (:tag :sacla)
 (assert-true
  (null (mapcon #'list '(0 1 2 3) 'nil '(1 2 3 4) '(2 3 4 5) '(3 4 5 6)))))
(define-test sacla-must-cons.464 (:tag :sacla)
 (assert-true
  (null (mapcon #'list '(0 1 2 3) '(1 2 3 4) 'nil '(2 3 4 5) '(3 4 5 6)))))
(define-test sacla-must-cons.465 (:tag :sacla)
 (assert-true
  (null (mapcon #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) 'nil '(3 4 5 6)))))
(define-test sacla-must-cons.466 (:tag :sacla)
 (assert-true
  (null (mapcon #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '(3 4 5 6) 'nil))))
(define-test sacla-must-cons.467 (:tag :sacla)
 (assert-true
  (let* ((x '((apple . 1) (orange . 2) (grapes . 3))) (y (acons 'plum 9 x)))
    (and (equal y '((plum . 9) (apple . 1) (orange . 2) (grapes . 3)))
         (eq x (cdr y))))))
(define-test sacla-must-cons.468 (:tag :sacla)
 (assert-true (equal (acons 'a '0 nil) '((a . 0)))))
(define-test sacla-must-cons.469 (:tag :sacla)
 (assert-true
  (equal (acons 'apple 1 (acons 'orange 2 (acons 'grapes '3 nil)))
         '((apple . 1) (orange . 2) (grapes . 3)))))
(define-test sacla-must-cons.470 (:tag :sacla)
 (assert-true (equal (acons nil nil nil) '((nil)))))
(define-test sacla-must-cons.471 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (assoc 'y alist) (cadr alist)))))
(define-test sacla-must-cons.472 (:tag :sacla)
 (assert-true (null (assoc 'no-such-key '((x . 100) (y . 200) (z . 50))))))
(define-test sacla-must-cons.473 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (assoc 'y alist :test #'eq) (cadr alist)))))
(define-test sacla-must-cons.474 (:tag :sacla)
 (assert-true (null (assoc 'key 'nil))))
(define-test sacla-must-cons.475 (:tag :sacla)
 (assert-true (null (assoc 'nil '(nil)))))
(define-test sacla-must-cons.476 (:tag :sacla)
 (assert-true (null (assoc 'nil '(nil nil)))))
(define-test sacla-must-cons.477 (:tag :sacla)
 (assert-true
  (let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
    (eq (assoc 'y alist) (car (cddddr alist))))))
(define-test sacla-must-cons.478 (:tag :sacla)
 (assert-true
  (let ((alist '((1 . a) nil (2 . b) (nil))))
    (eq (assoc 'nil alist) (cadddr alist)))))
(define-test sacla-must-cons.479 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (x . 100) (z . 50))))
    (eq (assoc 'y alist) (cadr alist)))))
(define-test sacla-must-cons.480 (:tag :sacla)
 (assert-true
  (let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
    (eq (assoc 'a alist :test-not (complement #'eq)) (car alist)))))
(define-test sacla-must-cons.481 (:tag :sacla)
 (assert-true
  (let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
    (null (assoc 'z alist :test-not (complement #'eq))))))
(define-test sacla-must-cons.482 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'aa alist :key #'cadr :test #'eq) (car alist)))))
(define-test sacla-must-cons.483 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'bb alist :key #'cadr :test #'eq) (cadr alist)))))
(define-test sacla-must-cons.484 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'cc alist :key #'cadr :test #'eq) (caddr alist)))))
(define-test sacla-must-cons.485 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'dd alist :key #'cadr :test #'eq) (cadddr alist)))))
(define-test sacla-must-cons.486 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (null (assoc 'ee alist :key #'cadr :test #'eq)))))
(define-test sacla-must-cons.487 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) nil ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'dd alist :key #'cadr :test #'eq) (car (cddddr alist))))))
(define-test sacla-must-cons.488 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) nil ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'dd alist :key #'cadr :test #'eq) (car (cddddr alist))))))
(define-test sacla-must-cons.489 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) nil ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc 'dd alist :key #'cadr :test #'eq) (car (cddddr alist))))))
(define-test sacla-must-cons.490 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)) nil)))
    (eq (assoc 'dd alist :key #'cadr :test #'eq) (cadddr alist)))))
(define-test sacla-must-cons.491 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (assoc-if #'(lambda (arg) (eq arg 'y)) alist) (cadr alist)))))
(define-test sacla-must-cons.492 (:tag :sacla)
 (assert-true (null (assoc-if #'consp '((x . 100) (y . 200) (z . 50))))))
(define-test sacla-must-cons.493 (:tag :sacla)
 (assert-true (null (assoc-if #'(lambda (x) (eq x 'key)) 'nil))))
(define-test sacla-must-cons.494 (:tag :sacla)
 (assert-true (null (assoc-if #'identity '(nil)))))
(define-test sacla-must-cons.495 (:tag :sacla)
 (assert-true (null (assoc-if #'identity '(nil nil)))))
(define-test sacla-must-cons.496 (:tag :sacla)
 (assert-true
  (let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
    (eq (assoc-if #'(lambda (arg) (eq arg 'y)) alist) (car (cddddr alist))))))
(define-test sacla-must-cons.497 (:tag :sacla)
 (assert-true
  (let ((alist '((1 . a) nil (2 . b) (nil))))
    (eq (assoc-if #'(lambda (arg) (null arg)) alist) (cadddr alist)))))
(define-test sacla-must-cons.498 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc-if #'(lambda (x) (eq x 'aa)) alist :key #'cadr) (car alist)))))
(define-test sacla-must-cons.499 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc-if #'(lambda (x) (eq x 'bb)) alist :key #'cadr) (cadr alist)))))
(define-test sacla-must-cons.500 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (null (assoc-if #'(lambda (x) (eq x 'ee)) alist :key #'cadr)))))
(define-test sacla-must-cons.501 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (assoc-if-not #'(lambda (arg) (not (eq arg 'y))) alist) (cadr alist)))))
(define-test sacla-must-cons.502 (:tag :sacla)
 (assert-true
  (null (assoc-if-not (complement #'consp) '((x . 100) (y . 200) (z . 50))))))
(define-test sacla-must-cons.503 (:tag :sacla)
 (assert-true (null (assoc-if-not #'(lambda (x) (not (eq x 'key))) 'nil))))
(define-test sacla-must-cons.504 (:tag :sacla)
 (assert-true (null (assoc-if-not #'identity '(nil)))))
(define-test sacla-must-cons.505 (:tag :sacla)
 (assert-true (null (assoc-if-not #'identity '(nil nil)))))
(define-test sacla-must-cons.506 (:tag :sacla)
 (assert-true
  (let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
    (eq (assoc-if-not #'(lambda (arg) (not (eq arg 'y))) alist)
        (car (cddddr alist))))))
(define-test sacla-must-cons.507 (:tag :sacla)
 (assert-true
  (let ((alist '((1 . a) nil (2 . b) (nil))))
    (eq (assoc-if-not #'identity alist) (cadddr alist)))))
(define-test sacla-must-cons.508 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc-if-not #'(lambda (x) (not (eq x 'aa))) alist :key #'cadr)
        (car alist)))))
(define-test sacla-must-cons.509 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (eq (assoc-if-not #'(lambda (x) (not (eq x 'bb))) alist :key #'cadr)
        (cadr alist)))))
(define-test sacla-must-cons.510 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
    (null (assoc-if-not #'(lambda (x) (not (eq x 'ee))) alist :key #'cadr)))))
(define-test sacla-must-cons.511 (:tag :sacla)
 (assert-true
  (equal (copy-alist '((a . 10) (b . 100) (c . 1000)))
         '((a . 10) (b . 100) (c . 1000)))))
(define-test sacla-must-cons.512 (:tag :sacla)
 (assert-true
  (let* ((alist '((a . 10) (b . 100) (c . 1000))) (copy (copy-alist alist)))
    (and (not (eq alist copy))
         (not (eq (cdr alist) (cdr copy)))
         (not (eq (cddr alist) (cddr copy)))
         (not (eq (car alist) (car copy)))
         (not (eq (cadr alist) (cadr copy)))
         (not (eq (caddr alist) (caddr copy)))))))
(define-test sacla-must-cons.513 (:tag :sacla)
 (assert-true
  (let* ((alist '((a 10 x) (b 100 y) (c 1000 z))) (copy (copy-alist alist)))
    (and (not (eq alist copy))
         (not (eq (cdr alist) (cdr copy)))
         (not (eq (cddr alist) (cddr copy)))
         (not (eq (car alist) (car copy)))
         (not (eq (cadr alist) (cadr copy)))
         (not (eq (caddr alist) (caddr copy)))
         (eq (cdar alist) (cdar copy))
         (eq (cdadr alist) (cdadr copy))
         (eq (cdaddr alist) (cdaddr copy))))))
(define-test sacla-must-cons.514 (:tag :sacla)
 (assert-true
  (let* ((alist (pairlis '(x y z) '(xx yy zz) '((a . aa) (b . bb)))))
    (and (equal (assoc 'x alist) '(x . xx))
         (equal (assoc 'y alist) '(y . yy))
         (equal (assoc 'z alist) '(z . zz))
         (equal (assoc 'a alist) '(a . aa))
         (equal (assoc 'b alist) '(b . bb))
         (null (assoc 'key alist))))))
(define-test sacla-must-cons.515 (:tag :sacla)
 (assert-true
  (let* ((alist (pairlis '(x y z) '(xx yy zz))))
    (and (equal (assoc 'x alist) '(x . xx))
         (equal (assoc 'y alist) '(y . yy))
         (equal (assoc 'z alist) '(z . zz))
         (null (assoc 'key alist))))))
(define-test sacla-must-cons.516 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (rassoc '200 alist) (cadr alist)))))
(define-test sacla-must-cons.517 (:tag :sacla)
 (assert-true (null (rassoc 'no-such-datum '((x . 100) (y . 200) (z . 50))))))
(define-test sacla-must-cons.518 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (rassoc '200 alist :test #'=) (cadr alist)))))
(define-test sacla-must-cons.519 (:tag :sacla)
 (assert-true (null (rassoc 'key 'nil))))
(define-test sacla-must-cons.520 (:tag :sacla)
 (assert-true (null (rassoc 'nil '(nil)))))
(define-test sacla-must-cons.521 (:tag :sacla)
 (assert-true (null (rassoc 'nil '(nil nil)))))
(define-test sacla-must-cons.522 (:tag :sacla)
 (assert-true
  (let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
    (eq (rassoc '200 alist) (car (cddddr alist))))))
(define-test sacla-must-cons.523 (:tag :sacla)
 (assert-true
  (let ((alist '((1 . a) nil (2 . b) (nil))))
    (eq (rassoc 'nil alist) (cadddr alist)))))
(define-test sacla-must-cons.524 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (x . 100) (z . 50))))
    (eq (rassoc '200 alist) (cadr alist)))))
(define-test sacla-must-cons.525 (:tag :sacla)
 (assert-true
  (let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
    (eq (rassoc '1 alist :test-not (complement #'=)) (car alist)))))
(define-test sacla-must-cons.526 (:tag :sacla)
 (assert-true
  (let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
    (null (rassoc '9 alist :test-not (complement #'=))))))
(define-test sacla-must-cons.527 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc 'aa alist :key #'car :test #'eq) (car alist)))))
(define-test sacla-must-cons.528 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (cadddr alist)))))
(define-test sacla-must-cons.529 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (null (rassoc 'eee alist :key #'cadr :test #'eq)))))
(define-test sacla-must-cons.530 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) nil (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (car (cddddr alist))))))
(define-test sacla-must-cons.531 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) nil (c cc ccc) (d dd ddd))))
    (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (car (cddddr alist))))))
(define-test sacla-must-cons.532 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd) nil)))
    (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (car (cdddr alist))))))
(define-test sacla-must-cons.533 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (rassoc-if #'(lambda (arg) (= arg 200)) alist) (cadr alist)))))
(define-test sacla-must-cons.534 (:tag :sacla)
 (assert-true (null (rassoc-if #'consp '((x . 100) (y . 200) (z . 50))))))
(define-test sacla-must-cons.535 (:tag :sacla)
 (assert-true (null (rassoc-if #'(lambda (x) (eq x 'key)) 'nil))))
(define-test sacla-must-cons.536 (:tag :sacla)
 (assert-true (null (rassoc-if #'identity '(nil)))))
(define-test sacla-must-cons.537 (:tag :sacla)
 (assert-true (null (rassoc-if #'identity '(nil nil)))))
(define-test sacla-must-cons.538 (:tag :sacla)
 (assert-true
  (let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
    (eq (rassoc-if #'(lambda (arg) (= arg 200)) alist) (car (cddddr alist))))))
(define-test sacla-must-cons.539 (:tag :sacla)
 (assert-true
  (let ((alist '((1 . a) nil (2 . b) (nil))))
    (eq (rassoc-if #'(lambda (arg) (null arg)) alist) (cadddr alist)))))
(define-test sacla-must-cons.540 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc-if #'(lambda (x) (eq x 'aaa)) alist :key #'cadr) (car alist)))))
(define-test sacla-must-cons.541 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc-if #'(lambda (x) (eq x 'bbb)) alist :key #'cadr)
        (cadr alist)))))
(define-test sacla-must-cons.542 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (null (rassoc-if #'(lambda (x) (eq x 'eee)) alist :key #'cadr)))))
(define-test sacla-must-cons.543 (:tag :sacla)
 (assert-true
  (let ((alist '((x . 100) (y . 200) (z . 50))))
    (eq (rassoc-if-not #'(lambda (arg) (not (= arg 200))) alist)
        (cadr alist)))))
(define-test sacla-must-cons.544 (:tag :sacla)
 (assert-true
  (null (rassoc-if-not (complement #'consp) '((x . 100) (y . 200) (z . 50))))))
(define-test sacla-must-cons.545 (:tag :sacla)
 (assert-true (null (rassoc-if-not #'(lambda (x) (not (eq x 'key))) 'nil))))
(define-test sacla-must-cons.546 (:tag :sacla)
 (assert-true (null (rassoc-if-not #'identity '(nil)))))
(define-test sacla-must-cons.547 (:tag :sacla)
 (assert-true (null (rassoc-if-not #'identity '(nil nil)))))
(define-test sacla-must-cons.548 (:tag :sacla)
 (assert-true
  (let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
    (eq (rassoc-if-not #'(lambda (arg) (not (= arg 200))) alist)
        (car (cddddr alist))))))
(define-test sacla-must-cons.549 (:tag :sacla)
 (assert-true
  (let ((alist '((1 . a) nil (2 . b) (nil))))
    (eq (assoc-if-not #'identity alist) (cadddr alist)))))
(define-test sacla-must-cons.550 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc-if-not #'(lambda (x) (not (eq x 'aaa))) alist :key #'cadr)
        (car alist)))))
(define-test sacla-must-cons.551 (:tag :sacla)
 (assert-true
  (let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
    (eq (rassoc-if-not #'(lambda (x) (not (eq x 'bbb))) alist :key #'cadr)
        (cadr alist)))))
(define-test sacla-must-cons.552 (:tag :sacla)
 (assert-true
  (let ((alist '(((a aa aaa) . 0) ((b bb bbb) . 1) ((c cc ccc) . 2))))
    (eq (rassoc-if-not #'(lambda (x) (not (= x '2))) alist :key #'1+)
        (cadr alist)))))
(define-test sacla-must-cons.553 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (multiple-value-bind (indicator value tail)
        (get-properties plist '(prop3 prop4 propx propy))
      (and (eq indicator 'prop3) (eql value 3) (eq tail (nthcdr 4 plist)))))))
(define-test sacla-must-cons.554 (:tag :sacla)
 (assert-true
  (multiple-value-bind (indicator value tail)
      (get-properties '(prop1 1 prop2 2 prop3 3 prop4 4) '(propx propy propz))
    (and (eq indicator nil) (eq value nil) (eq tail nil)))))
(define-test sacla-must-cons.555 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (multiple-value-bind (indicator value tail)
        (get-properties plist '(prop1))
      (and (eq indicator 'prop1) (eql value 1) (eq tail plist))))))
(define-test sacla-must-cons.556 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 nil nil prop2 2 prop3 3 prop4 4)))
    (multiple-value-bind (indicator value tail)
        (get-properties plist '(nil))
      (and (eq indicator nil) (eql value nil) (eq tail (cddr plist)))))))
(define-test sacla-must-cons.557 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (multiple-value-bind (indicator value tail)
        (get-properties plist '(prop3 prop4 propx propy prop1))
      (and (eq indicator 'prop1) (eql value 1) (eq tail plist))))))
(define-test sacla-must-cons.558 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (eql (getf plist 'prop1) 1))))
(define-test sacla-must-cons.559 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (eql (getf plist 'prop2) 2))))
(define-test sacla-must-cons.560 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (eql (getf plist 'prop3) 3))))
(define-test sacla-must-cons.561 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (eql (getf plist 'prop4) 4))))
(define-test sacla-must-cons.562 (:tag :sacla)
 (assert-true
  (let ((plist
         '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
    (eql (getf plist 'prop1) 1))))
(define-test sacla-must-cons.563 (:tag :sacla)
 (assert-true
  (let ((plist
         '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
    (eql (getf plist 'prop2) 2))))
(define-test sacla-must-cons.564 (:tag :sacla)
 (assert-true
  (let ((plist
         '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
    (eql (getf plist 'prop3) 3))))
(define-test sacla-must-cons.565 (:tag :sacla)
 (assert-true
  (let ((plist
         '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
    (eql (getf plist 'prop4) 4))))
(define-test sacla-must-cons.566 (:tag :sacla)
 (assert-true
  (let ((plist
         '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
    (null (getf plist 'propx)))))
(define-test sacla-must-cons.567 (:tag :sacla)
 (assert-true
  (let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
    (eq (getf plist 'weird-property 'not-found) 'not-found))))
(define-test sacla-must-cons.568 (:tag :sacla)
 (assert-true
  (let ((plist (copy-list '(prop1 1 prop2 2 prop3 3 prop4 4))))
    (and (eql (setf (getf plist 'prop1) 9) 9) (eql (getf plist 'prop1) 9)))))
(define-test sacla-must-cons.569 (:tag :sacla)
 (assert-true
  (let ((plist nil))
    (and (eql (setf (getf plist 'prop1) 9) 9) (eql (getf plist 'prop1) 9)))))
(define-test sacla-must-cons.570 (:tag :sacla)
 (assert-true
  (let ((plist 'nil))
    (incf (getf plist 'count 0))
    (eql (getf plist 'count) 1))))
(define-test sacla-must-cons.571 (:tag :sacla)
 (assert-true
  (let ((x (list nil)))
    (and (eql (setf (getf (car x) 'prop1) 9) 9)
         (eql (getf (car x) 'prop1) 9)))))
(define-test sacla-must-cons.572 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (remf plist 'p2) (eq (getf plist 'p2 'not-found) 'not-found)))))
(define-test sacla-must-cons.573 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (remf plist 'p3) (eq (getf plist 'p3 'not-found) 'not-found)))))
(define-test sacla-must-cons.574 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (remf plist 'p4) (eq (getf plist 'p4 'not-found) 'not-found)))))
(define-test sacla-must-cons.575 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (null (remf plist 'px)) (equal plist '(p1 1 p2 2 p3 3 p4 4))))))
(define-test sacla-must-cons.576 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (remf plist 'p4)
         (remf plist 'p2)
         (remf plist 'p3)
         (remf plist 'p1)
         (null (remf plist 'px))
         (null (remf plist 'p1))
         (null (remf plist 'p2))
         (null (remf plist 'p3))
         (null (remf plist 'p4))
         (null plist)))))
(define-test sacla-must-cons.577 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4 'p1 5 'p2 6 'p3 7 'p4 8)))
    (and (remf plist 'p4)
         (remf plist 'p2)
         (remf plist 'p3)
         (remf plist 'p1)
         (null (remf plist 'px))
         (eql (getf plist 'p1) 5)
         (eql (getf plist 'p2) 6)
         (eql (getf plist 'p3) 7)
         (eql (getf plist 'p4) 8)))))
(define-test sacla-must-cons.578 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 100 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (eql (getf plist 'p1) 100)
         (remf plist 'p1)
         (eql (getf plist 'p1) 1)
         (remf plist 'p1)
         (null (getf plist 'p1))))))
(define-test sacla-must-cons.579 (:tag :sacla)
 (assert-true
  (let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
    (and (remf plist 'p4) (null (getf plist 'p4))))))
(define-test sacla-must-cons.580 (:tag :sacla)
 (assert-true
  (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
        (list2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
    (null (set-exclusive-or (intersection list1 list2) '(c b 4 1 1)))
    (null
     (set-exclusive-or (intersection list1 list2 :test 'equal)
                       '("B" c b 4 1 1)
                       :test 'equal))
    (null
     (set-exclusive-or (intersection list1 list2 :test #'equalp)
                       '("d" "C" "B" "A" c b 4 1 1)
                       :test #'equalp)))))
(define-test sacla-must-cons.581 (:tag :sacla)
 (assert-true (null (intersection '(0 1 2) 'nil))))
(define-test sacla-must-cons.582 (:tag :sacla)
 (assert-true (null (intersection 'nil 'nil))))
(define-test sacla-must-cons.583 (:tag :sacla)
 (assert-true (null (intersection 'nil '(0 1 2)))))
(define-test sacla-must-cons.584 (:tag :sacla)
 (assert-true (equal (intersection '(0) '(0)) '(0))))
(define-test sacla-must-cons.585 (:tag :sacla)
 (assert-true (equal (intersection '(0 1 2 3) '(2)) '(2))))
(define-test sacla-must-cons.586 (:tag :sacla)
 (assert-true (member 0 (intersection '(0 0 0 0 0) '(0 1 2 3 4 5)))))
(define-test sacla-must-cons.587 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or (intersection '(0 1 2 3 4) '(4 3 2 1 0)) '(4 3 2 1 0)))))
(define-test sacla-must-cons.588 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or (intersection '(0 1 2 3 4) '(0 1 2 3 4)) '(0 1 2 3 4)))))
(define-test sacla-must-cons.589 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or (intersection '(0 1 2 3 4) '(4 3 2 1 0)) '(0 1 2 3 4)))))
(define-test sacla-must-cons.590 (:tag :sacla)
 (assert-true
  (let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
        (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
    (null
     (set-exclusive-or
      (intersection list1 list2 :test #'char= :key #'(lambda (x) (char x 0)))
      '("B" "F" "h")
      :test #'char=
      :key #'(lambda (x) (char x 0)))))))
(define-test sacla-must-cons.591 (:tag :sacla)
 (assert-true
  (let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
        (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
    (null
     (set-exclusive-or
      (intersection list1
                    list2
                    :test #'char-equal
                    :key #'(lambda (x) (char x 0)))
      '("A" "B" "C" "d" "e" "F" "G" "h")
      :test #'char-equal
      :key #'(lambda (x) (char x 0)))))))
(define-test sacla-must-cons.592 (:tag :sacla)
 (assert-true
  (let ((list1 (list "A" "B" "C" "d")) (list2 (list "D" "E" "F" "g" "h")))
    (null
     (set-exclusive-or
      (intersection list1
                    list2
                    :test #'char-equal
                    :key #'(lambda (x) (char x 0)))
      '("d")
      :test #'char-equal
      :key #'(lambda (x) (char x 0)))))))
(define-test sacla-must-cons.593 (:tag :sacla)
 (assert-true
  (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
        (list2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
    (null
     (set-exclusive-or (nintersection (copy-list list1) list2) '(c b 4 1 1)))
    (null
     (set-exclusive-or (nintersection (copy-list list1) list2 :test 'equal)
                       '("B" c b 4 1 1)
                       :test 'equal))
    (null
     (set-exclusive-or (nintersection (copy-list list1) list2 :test #'equalp)
                       '("d" "C" "B" "A" c b 4 1 1)
                       :test #'equalp)))))
(define-test sacla-must-cons.594 (:tag :sacla)
 (assert-true (null (nintersection (list 0 1 2) 'nil))))
(define-test sacla-must-cons.595 (:tag :sacla)
 (assert-true (null (nintersection 'nil 'nil))))
(define-test sacla-must-cons.596 (:tag :sacla)
 (assert-true (null (nintersection 'nil '(0 1 2)))))
(define-test sacla-must-cons.597 (:tag :sacla)
 (assert-true (equal (nintersection (list 0) '(0)) '(0))))
(define-test sacla-must-cons.598 (:tag :sacla)
 (assert-true (equal (nintersection (list 0 1 2 3) '(2)) '(2))))
(define-test sacla-must-cons.599 (:tag :sacla)
 (assert-true (member 0 (nintersection (list 0 0 0 0 0) '(0 1 2 3 4 5)))))
(define-test sacla-must-cons.600 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or (nintersection (list 0 1 2 3 4) '(4 3 2 1 0))
                     '(4 3 2 1 0)))))
(define-test sacla-must-cons.601 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or (nintersection (list 0 1 2 3 4) '(0 1 2 3 4))
                     '(0 1 2 3 4)))))
(define-test sacla-must-cons.602 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or (nintersection (list 0 1 2 3 4) '(4 3 2 1 0))
                     '(0 1 2 3 4)))))
(define-test sacla-must-cons.603 (:tag :sacla)
 (assert-true
  (let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
        (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
    (null
     (set-exclusive-or
      (nintersection list1 list2 :test #'char= :key #'(lambda (x) (char x 0)))
      '("B" "F" "h")
      :test #'char=
      :key #'(lambda (x) (char x 0)))))))
(define-test sacla-must-cons.604 (:tag :sacla)
 (assert-true
  (let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
        (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
    (null
     (set-exclusive-or
      (nintersection list1
                     list2
                     :test #'char-equal
                     :key #'(lambda (x) (char x 0)))
      '("A" "B" "C" "d" "e" "F" "G" "h")
      :test #'char-equal
      :key #'(lambda (x) (char x 0)))))))
(define-test sacla-must-cons.605 (:tag :sacla)
 (assert-true
  (let ((list1 (list "A" "B" "C" "d")) (list2 (list "D" "E" "F" "g" "h")))
    (null
     (set-exclusive-or
      (nintersection list1
                     list2
                     :test #'char-equal
                     :key #'(lambda (x) (char x 0)))
      '("d")
      :test #'char-equal
      :key #'(lambda (x) (char x 0)))))))
(define-test sacla-must-cons.606 (:tag :sacla)
 (assert-true
  (let ((set '(a b c)))
    (eq (adjoin 'a set) set))))
(define-test sacla-must-cons.607 (:tag :sacla)
 (assert-true
  (let* ((set '(a b c)) (new-set (adjoin 'x set)))
    (and (equal new-set '(x a b c)) (eq set (cdr new-set))))))
(define-test sacla-must-cons.608 (:tag :sacla)
 (assert-true (equal (adjoin 1 nil) '(1))))
(define-test sacla-must-cons.609 (:tag :sacla)
 (assert-true (equal (adjoin nil nil) '(nil))))
(define-test sacla-must-cons.610 (:tag :sacla)
 (assert-true (equal (adjoin nil '(nil)) '(nil))))
(define-test sacla-must-cons.611 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (equal (adjoin '(test-item 1) set) '((test-item 1) (test-item 1))))))
(define-test sacla-must-cons.612 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (equal (adjoin '(test-item 1) set) '((test-item 1) (test-item 1))))))
(define-test sacla-must-cons.613 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (eq (adjoin '(test-item 1) set :test #'equal) set))))
(define-test sacla-must-cons.614 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (eq (adjoin '(test-item) set :key #'car) set))))
(define-test sacla-must-cons.615 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (eq (adjoin '(test-item) set :key #'car :test #'eq) set))))
(define-test sacla-must-cons.616 (:tag :sacla)
 (assert-true
  (let ((set '(("test-item" 1))))
    (eq (adjoin '("test-item") set :key #'car :test #'equal) set))))
(define-test sacla-must-cons.617 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (eq (adjoin '(test-item 1) set :test-not (complement #'equal)) set))))
(define-test sacla-must-cons.618 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (eq (adjoin '(test-item) set :test-not (complement #'eql) :key #'car)
        set))))
(define-test sacla-must-cons.619 (:tag :sacla)
 (assert-true
  (let ((set '((test-item 1))))
    (eq (adjoin '(test-item) set :key #'car :test-not (complement #'eq)) set))))
(define-test sacla-must-cons.620 (:tag :sacla)
 (assert-true
  (let ((set '(("test-item" 1))))
    (eq (adjoin '("test-item") set :key #'car :test-not (complement #'equal))
        set))))
(define-test sacla-must-cons.621 (:tag :sacla)
 (assert-true
  (let ((place nil))
    (and (equal (pushnew 'a place) '(a)) (equal place '(a))))))
(define-test sacla-must-cons.622 (:tag :sacla)
 (assert-true
  (let ((place nil))
    (and (equal (pushnew 'a place) '(a)) (equal place '(a))))))
(define-test sacla-must-cons.623 (:tag :sacla)
 (assert-true
  (let ((place '((a . 1) (b . 2))))
    (and
     (equal (pushnew '(b . 2) place :test #'= :key #'cdr) '((a . 1) (b . 2)))
     (equal place '((a . 1) (b . 2)))))))
(define-test sacla-must-cons.624 (:tag :sacla)
 (assert-true
  (let ((place '((a . 1) (b . 2))))
    (and
     (equal (pushnew '(b . 2) place :test-not (complement #'=) :key #'cdr)
            '((a . 1) (b . 2)))
     (equal place '((a . 1) (b . 2)))))))
(define-test sacla-must-cons.625 (:tag :sacla)
 (assert-true
  (let ((place '((a . 1) (b . 2))))
    (and (eq (pushnew '(z . 2) place :test #'= :key #'cdr) place)
         (equal place '((a . 1) (b . 2)))))))
(define-test sacla-must-cons.626 (:tag :sacla)
 (assert-true
  (let ((place '((a . 1) (b . 2))))
    (and
     (eq (pushnew '(z . 2) place :test-not (complement #'=) :key #'cdr) place)
     (equal place '((a . 1) (b . 2)))))))
(define-test sacla-must-cons.627 (:tag :sacla)
 (assert-true
  (let ((place '("love" "peace")))
    (equal (pushnew "war" place :test #'equal) '("war" "love" "peace")))))
(define-test sacla-must-cons.628 (:tag :sacla)
 (assert-true
  (let ((place '("love" "peace")))
    (equal (pushnew "war" place :test-not (complement #'equal))
           '("war" "love" "peace")))))
(define-test sacla-must-cons.629 (:tag :sacla)
 (assert-true
  (let ((place '("love" "peace")))
    (and (eq (pushnew "peace" place :test #'equal) place)
         (equal place '("love" "peace"))))))
(define-test sacla-must-cons.630 (:tag :sacla)
 (assert-true
  (let ((place '("love" "peace")))
    (and (eq (pushnew "peace" place :test-not (complement #'equal)) place)
         (equal place '("love" "peace"))))))
(define-test sacla-must-cons.631 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (equal (pushnew '("war" . w) place :test #'equal :key #'car)
           '(("war" . w) ("love" . l) ("peace" . p))))))
(define-test sacla-must-cons.632 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (equal
     (pushnew '("war" . w) place :test-not (complement #'equal) :key #'car)
     '(("war" . w) ("love" . l) ("peace" . p))))))
(define-test sacla-must-cons.633 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (and (eq (pushnew '("love" . l) place :test #'equal :key #'car) place)
         (equal place '(("love" . l) ("peace" . p)))))))
(define-test sacla-must-cons.634 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (and
     (eq
      (pushnew '("love" . l) place :test-not (complement #'equal) :key #'car)
      place)
     (equal place '(("love" . l) ("peace" . p)))))))
(define-test sacla-must-cons.635 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (and (eq (pushnew '("LOVE" . l) place :test #'equalp :key #'car) place)
         (equal place '(("love" . l) ("peace" . p)))))))
(define-test sacla-must-cons.636 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (and
     (eq
      (pushnew '("LOVE" . l) place :test-not (complement #'equalp) :key #'car)
      place)
     (equal place '(("love" . l) ("peace" . p)))))))
(define-test sacla-must-cons.637 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (equal (pushnew '("LOVE" . l) place :test #'equal :key #'car)
           '(("LOVE" . l) ("love" . l) ("peace" . p))))))
(define-test sacla-must-cons.638 (:tag :sacla)
 (assert-true
  (let ((place '(("love" . l) ("peace" . p))))
    (equal
     (pushnew '("LOVE" . l) place :test-not (complement #'equal) :key #'car)
     '(("LOVE" . l) ("love" . l) ("peace" . p))))))
(define-test sacla-must-cons.639 (:tag :sacla)
 (assert-true
  (let ((list '((1) (1 2) (1 2 3))))
    (and (equal (pushnew '(1) list) '((1) (1) (1 2) (1 2 3)))
         (equal list '((1) (1) (1 2) (1 2 3)))))))
(define-test sacla-must-cons.640 (:tag :sacla)
 (assert-true
  (let* ((list '((1) (1 2) (1 2 3))) (original list))
    (and (equal (pushnew '(1) list :test #'equal) '((1) (1 2) (1 2 3)))
         (eq list original)))))
(define-test sacla-must-cons.641 (:tag :sacla)
 (assert-true
  (let* ((list '((1) (1 2) (1 2 3))) (original list))
    (and
     (equal (pushnew '(1) list :test #'equal :key nil) '((1) (1 2) (1 2 3)))
     (eq list original)))))
(define-test sacla-must-cons.642 (:tag :sacla)
 (assert-true
  (let ((list (copy-tree '(1 (2) 3 4))))
    (and (equal (pushnew 4 (cadr list)) '(4 2)) (equal list '(1 (4 2) 3 4))))))
(define-test sacla-must-cons.643 (:tag :sacla)
 (assert-true
  (let ((list (copy-tree '(1 (2) 3 4))))
    (and (equal (pushnew 4 (cadr list) :key nil) '(4 2))
         (equal list '(1 (4 2) 3 4))))))
(define-test sacla-must-cons.644 (:tag :sacla)
 (assert-true
  (null
   (set-difference (set-difference '(1 2 3 4 5 6 7 8 9) '(2 4 6 8))
                   '(1 3 5 7 9)))))
(define-test sacla-must-cons.645 (:tag :sacla)
 (assert-true
  (null
   (nset-difference (set-difference (list 1 2 3 4 5 6 7 8 9) '(2 4 6 8))
                    '(1 3 5 7 9)))))
(define-test sacla-must-cons.646 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (set-difference '("1" "2" "3" "4" "5" "6" "7" "8" "9")
                    '("2" "4" "6" "8")
                    :test #'equal)
    '("1" "3" "5" "7" "9")
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.647 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (set-difference '("1" "2" "3" "4" "5" "6" "7" "8" "9")
                    '("2" "4" "6" "8")
                    :test #'equal)
    '("1" "3" "5" "7" "9")
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.648 (:tag :sacla)
 (assert-true
  (null
   (nset-difference
    (nset-difference (list "1" "2" "3" "4" "5" "6" "7" "8" "9")
                     '("2" "4" "6" "8")
                     :test #'equal)
    '("1" "3" "5" "7" "9")
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.649 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (set-difference '(("love") ("hate") ("peace") ("war"))
                    '(("love") ("peace"))
                    :key #'car
                    :test #'equal)
    '(("hate") ("war"))
    :key #'car
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.650 (:tag :sacla)
 (assert-true
  (null
   (nset-difference
    (nset-difference (list '("love") '("hate") '("peace") '("war"))
                     '(("love") ("peace"))
                     :key #'car
                     :test #'equal)
    '(("hate") ("war"))
    :key #'car
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.651 (:tag :sacla)
 (assert-true (null (set-difference 'nil 'nil))))
(define-test sacla-must-cons.652 (:tag :sacla)
 (assert-true (null (set-difference 'nil 'nil :test #'equal :key 'identity))))
(define-test sacla-must-cons.653 (:tag :sacla)
 (assert-true (null (nset-difference 'nil 'nil))))
(define-test sacla-must-cons.654 (:tag :sacla)
 (assert-true (null (set-difference 'nil '(1 2 3)))))
(define-test sacla-must-cons.655 (:tag :sacla)
 (assert-true
  (null (set-difference 'nil '(1 2 3) :test #'equal :key 'identity))))
(define-test sacla-must-cons.656 (:tag :sacla)
 (assert-true (null (nset-difference 'nil '(1 2 3)))))
(define-test sacla-must-cons.657 (:tag :sacla)
 (assert-true (null (set-difference '(1 2 3 4) '(4 3 2 1)))))
(define-test sacla-must-cons.658 (:tag :sacla)
 (assert-true (null (nset-difference (list 1 2 3 4) '(4 3 2 1)))))
(define-test sacla-must-cons.659 (:tag :sacla)
 (assert-true (null (set-difference '(1 2 3 4) '(2 4 3 1)))))
(define-test sacla-must-cons.660 (:tag :sacla)
 (assert-true (null (nset-difference (list 1 2 3 4) '(2 4 3 1)))))
(define-test sacla-must-cons.661 (:tag :sacla)
 (assert-true (null (set-difference '(1 2 3 4) '(1 3 4 2)))))
(define-test sacla-must-cons.662 (:tag :sacla)
 (assert-true (null (nset-difference (list 1 2 3 4) '(1 3 4 2)))))
(define-test sacla-must-cons.663 (:tag :sacla)
 (assert-true (null (set-difference '(1 2 3 4) '(1 3 2 4)))))
(define-test sacla-must-cons.664 (:tag :sacla)
 (assert-true (null (nset-difference (list 1 2 3 4) '(1 3 2 4)))))
(define-test sacla-must-cons.665 (:tag :sacla)
 (assert-true
  (eq (set-difference (set-difference '(1 2 3) 'nil) '(1 2 3)) 'nil)))
(define-test sacla-must-cons.666 (:tag :sacla)
 (assert-true
  (eq (nset-difference (nset-difference (list 1 2 3) 'nil) '(1 2 3)) 'nil)))
(define-test sacla-must-cons.667 (:tag :sacla)
 (assert-true (eq (set-difference (set-difference '(1 2 3) '(1)) '(2 3)) 'nil)))
(define-test sacla-must-cons.668 (:tag :sacla)
 (assert-true
  (eq (nset-difference (nset-difference (list 1 2 3) '(1)) '(2 3)) 'nil)))
(define-test sacla-must-cons.669 (:tag :sacla)
 (assert-true (eq (set-difference (set-difference '(1 2 3) '(1 2)) '(3)) 'nil)))
(define-test sacla-must-cons.670 (:tag :sacla)
 (assert-true
  (eq (nset-difference (nset-difference (list 1 2 3) '(1 2)) '(3)) 'nil)))
(define-test sacla-must-cons.671 (:tag :sacla)
 (assert-true
  (null (set-exclusive-or (set-exclusive-or '(1 2 3) '(2 3 4)) '(1 4)))))
(define-test sacla-must-cons.672 (:tag :sacla)
 (assert-true
  (null (nset-exclusive-or (nset-exclusive-or (list 1 2 3) '(2 3 4)) '(1 4)))))
(define-test sacla-must-cons.673 (:tag :sacla)
 (assert-true
  (null (set-exclusive-or (set-exclusive-or '(1 2 3) '(1 3)) '(2)))))
(define-test sacla-must-cons.674 (:tag :sacla)
 (assert-true
  (null (nset-exclusive-or (nset-exclusive-or (list 1 2 3) '(1 3)) '(2)))))
(define-test sacla-must-cons.675 (:tag :sacla)
 (assert-true (null (set-exclusive-or 'nil 'nil))))
(define-test sacla-must-cons.676 (:tag :sacla)
 (assert-true (null (nset-exclusive-or 'nil 'nil))))
(define-test sacla-must-cons.677 (:tag :sacla)
 (assert-true (null (set-exclusive-or '(1 2 3) '(3 2 1)))))
(define-test sacla-must-cons.678 (:tag :sacla)
 (assert-true (null (nset-exclusive-or (list 1 2 3) '(3 2 1)))))
(define-test sacla-must-cons.679 (:tag :sacla)
 (assert-true (null (set-exclusive-or '(1 2 3) '(2 3 1)))))
(define-test sacla-must-cons.680 (:tag :sacla)
 (assert-true (null (nset-exclusive-or (list 1 2 3) '(2 3 1)))))
(define-test sacla-must-cons.681 (:tag :sacla)
 (assert-true (null (set-exclusive-or '(1 2 3) '(1 3 2)))))
(define-test sacla-must-cons.682 (:tag :sacla)
 (assert-true (null (nset-exclusive-or (list 1 2 3) '(1 3 2)))))
(define-test sacla-must-cons.683 (:tag :sacla)
 (assert-true
  (null (set-exclusive-or (set-exclusive-or '(1 2 3) 'nil) '(3 2 1)))))
(define-test sacla-must-cons.684 (:tag :sacla)
 (assert-true
  (null (nset-exclusive-or (nset-exclusive-or (list 1 2 3) 'nil) '(3 2 1)))))
(define-test sacla-must-cons.685 (:tag :sacla)
 (assert-true
  (null (set-exclusive-or (set-exclusive-or 'nil '(1 2 3)) '(2 1 3)))))
(define-test sacla-must-cons.686 (:tag :sacla)
 (assert-true
  (null (nset-exclusive-or (nset-exclusive-or 'nil '(1 2 3)) '(2 1 3)))))
(define-test sacla-must-cons.687 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or '("car" "ship" "airplane" "submarine")
                     '("car" "ship" "airplane" "submarine")
                     :test #'equal))))
(define-test sacla-must-cons.688 (:tag :sacla)
 (assert-true
  (null
   (nset-exclusive-or (copy-list '("car" "ship" "airplane" "submarine"))
                      '("car" "ship" "airplane" "submarine")
                      :test #'equal))))
(define-test sacla-must-cons.689 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or '("car" "ship" "airplane" "submarine")
                     '("CAR" "SHIP" "AIRPLANE" "SUBMARINE")
                     :test #'equalp))))
(define-test sacla-must-cons.690 (:tag :sacla)
 (assert-true
  (null
   (nset-exclusive-or (copy-list '("car" "ship" "airplane" "submarine"))
                      '("CAR" "SHIP" "AIRPLANE" "SUBMARINE")
                      :test #'equalp))))
(define-test sacla-must-cons.691 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or '("car" "ship" "airplane" "submarine")
                     '("ship" "airplane" "submarine" "car")
                     :test-not (complement #'equal)))))
(define-test sacla-must-cons.692 (:tag :sacla)
 (assert-true
  (null
   (nset-exclusive-or (copy-list '("car" "ship" "airplane" "submarine"))
                      '("ship" "airplane" "submarine" "car")
                      :test-not (complement #'equal)))))
(define-test sacla-must-cons.693 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or '(("car") ("ship") ("airplane") ("submarine"))
                     '(("car") ("ship") ("airplane") ("submarine"))
                     :test #'string=
                     :key #'car))))
(define-test sacla-must-cons.694 (:tag :sacla)
 (assert-true
  (null
   (nset-exclusive-or
    (copy-tree '(("car") ("ship") ("airplane") ("submarine")))
    '(("car") ("ship") ("airplane") ("submarine"))
    :test #'string=
    :key #'car))))
(define-test sacla-must-cons.695 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or '(("car") ("ship") ("airplane") ("submarine"))
                     '(("car") ("ship") ("airplane") ("submarine"))
                     :test-not (complement #'string=)
                     :key #'car))))
(define-test sacla-must-cons.696 (:tag :sacla)
 (assert-true
  (null
   (nset-exclusive-or
    (copy-tree '(("car") ("ship") ("airplane") ("submarine")))
    '(("car") ("ship") ("airplane") ("submarine"))
    :test-not (complement #'string=)
    :key #'car))))
(define-test sacla-must-cons.697 (:tag :sacla)
 (assert-true
  (null
   (set-exclusive-or
    (set-exclusive-or '("car" "ship" "airplane" "submarine")
                      '("car" "ship" "horse" "airplane" "submarine" "camel")
                      :test #'equal)
    '("camel" "horse")
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.698 (:tag :sacla)
 (assert-true
  (null
   (nset-exclusive-or
    (nset-exclusive-or (list "car" "ship" "airplane" "submarine")
                       '("car" "ship" "horse" "airplane" "submarine" "camel")
                       :test #'equal)
    '("camel" "horse")
    :test-not (complement #'equal)))))
(define-test sacla-must-cons.699 (:tag :sacla)
 (assert-true (subsetp '(1 2 3) '(1 2 3))))
(define-test sacla-must-cons.700 (:tag :sacla)
 (assert-true (subsetp '(1 2 3) '(3 2 1))))
(define-test sacla-must-cons.701 (:tag :sacla)
 (assert-true (subsetp '(1 2 3) '(2 1 3))))
(define-test sacla-must-cons.702 (:tag :sacla)
 (assert-true (null (subsetp '(1 2 3 4) '(2 1 3)))))
(define-test sacla-must-cons.703 (:tag :sacla)
 (assert-true (subsetp '(1) '(2 1 3))))
(define-test sacla-must-cons.704 (:tag :sacla)
 (assert-true (subsetp '(1 2) '(1 2 3 4 5 6 7 8))))
(define-test sacla-must-cons.705 (:tag :sacla)
 (assert-true (subsetp '(1 2 3 4 5) '(8 7 6 5 4 3 2 1))))
(define-test sacla-must-cons.706 (:tag :sacla)
 (assert-true
  (null
   (subsetp '("car" "ship" "airplane" "submarine")
            '("car" "ship" "horse" "airplane" "submarine" "camel")))))
(define-test sacla-must-cons.707 (:tag :sacla)
 (assert-true
  (subsetp '("car" "ship" "airplane" "submarine")
           '("car" "ship" "horse" "airplane" "submarine" "camel")
           :test #'equal)))
(define-test sacla-must-cons.708 (:tag :sacla)
 (assert-true
  (subsetp '("CAR" "SHIP" "AIRPLANE" "SUBMARINE")
           '("car" "ship" "horse" "airplane" "submarine" "camel")
           :test #'equalp)))
(define-test sacla-must-cons.709 (:tag :sacla)
 (assert-true
  (subsetp '(("car") ("ship") ("airplane") ("submarine"))
           '(("car") ("ship") ("horse") ("airplane") ("submarine") ("camel"))
           :test #'string=
           :key #'car)))
(define-test sacla-must-cons.710 (:tag :sacla)
 (assert-true (null (union 'nil 'nil))))
(define-test sacla-must-cons.711 (:tag :sacla)
 (assert-true (null (nunion 'nil 'nil))))
(define-test sacla-must-cons.712 (:tag :sacla)
 (assert-true (null (set-difference (union '(1 2 3) '(2 3 4)) '(1 2 3 4)))))
(define-test sacla-must-cons.713 (:tag :sacla)
 (assert-true
  (null (set-difference (nunion (list 1 2 3) (list 2 3 4)) '(1 2 3 4)))))
(define-test sacla-must-cons.714 (:tag :sacla)
 (assert-true (null (set-difference (union '(1 2 3) '(1 2 3)) '(1 2 3)))))
(define-test sacla-must-cons.715 (:tag :sacla)
 (assert-true
  (null (set-difference (nunion (list 1 2 3) (list 1 2 3)) '(1 2 3)))))
(define-test sacla-must-cons.716 (:tag :sacla)
 (assert-true (null (set-difference (union '(1) '(3 2 1)) '(1 2 3)))))
(define-test sacla-must-cons.717 (:tag :sacla)
 (assert-true (null (set-difference (nunion (list 1) (list 3 2 1)) '(1 2 3)))))
(define-test sacla-must-cons.718 (:tag :sacla)
 (assert-true (null (set-difference (union '(1 2 3) 'nil) '(1 2 3)))))
(define-test sacla-must-cons.719 (:tag :sacla)
 (assert-true (null (set-difference (nunion (list 1 2 3) 'nil) '(1 2 3)))))
(define-test sacla-must-cons.720 (:tag :sacla)
 (assert-true (null (set-difference (union 'nil '(1 2 3)) '(1 2 3)))))
(define-test sacla-must-cons.721 (:tag :sacla)
 (assert-true (null (set-difference (nunion 'nil (list 1 2 3)) '(1 2 3)))))
(define-test sacla-must-cons.722 (:tag :sacla)
 (assert-true (null (set-difference (union '(1 2 3) '(2)) '(1 2 3)))))
(define-test sacla-must-cons.723 (:tag :sacla)
 (assert-true (null (set-difference (nunion (list 1 2 3) (list 2)) '(1 2 3)))))
(define-test sacla-must-cons.724 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (union '("Alpha" "Bravo" "Charlie")
           '("Bravo" "Charlie" "Delta" "Echo")
           :test #'string=)
    '("Alpha" "Bravo" "Charlie" "Delta" "Echo")
    :test-not (complement #'string=)))))
(define-test sacla-must-cons.725 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (nunion (list "Alpha" "Bravo" "Charlie")
            (list "Bravo" "Charlie" "Delta" "Echo")
            :test #'string=)
    '("Alpha" "Bravo" "Charlie" "Delta" "Echo")
    :test-not (complement #'string=)))))
(define-test sacla-must-cons.726 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (union (copy-tree '(("Alpha") ("Bravo") ("Charlie")))
           (copy-tree '(("Bravo") ("Charlie") ("Delta") ("Echo")))
           :test #'string=
           :key #'car)
    '(("Alpha") ("Bravo") ("Charlie") ("Delta") ("Echo"))
    :test-not (complement #'string=)
    :key #'car))))
(define-test sacla-must-cons.727 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (nunion (copy-tree '(("Alpha") ("Bravo") ("Charlie")))
            (copy-tree '(("Bravo") ("Charlie") ("Delta") ("Echo")))
            :test #'string=
            :key #'car)
    '(("Alpha") ("Bravo") ("Charlie") ("Delta") ("Echo"))
    :test-not (complement #'string=)
    :key #'car))))
(define-test sacla-must-cons.728 (:tag :sacla)
 (assert-true
  (null
   (set-difference
    (union '("Alpha" "Bravo" "Charlie")
           '("BRAVO" "CHARLIE" "DELTA" "ECHO")
           :test #'string-equal)
    '("ALPHA" "BRAVO" "CHARLIE" "DELTA" "ECHO")
    :test-not (complement #'string-equal)))))

